{-# LANGUAGE OverloadedStrings #-}

module Publish (mkPublish, prettyPublish) where

import Types
import Utilities
import qualified Data.Vector.Unboxed as U
import qualified Data.Graph.Inductive as Gr
import qualified Data.Text as T
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Data
import Data.Validation
import Data.Maybe (fromJust)
import qualified Data.List as L


-- format a Validation [PubInvalid] (DMModel, CitationDictionary) for humans. 
prettyPublish :: Validation [PubInvalid] (DMModel, CitationDictionary)
              -> T.Text
prettyPublish (Success _)    = "Ready to Publish"
prettyPublish (Failure errs) = stripped
                             <> "\n\n"
                             <> preppedErrors
    where
        preppedErrors = T.intercalate "\n\n" $ sequenceA
            [ nTypePrep
            , lTypePrep
            , lEffectPrep
            , citePrep
            ] sErrs
        stripped = sPShowNoColor (filter (not. selecter) sErrs)
--         stripLELTNTCE = filter (not . isCiteError) stripLELTNT
--         stripLELTNT = filter (not . isUndefinedEffectType) stripLTNT
--         stripLTNT = filter (not . isUndefinedLinkType) stripNT
--         stripNT = filter (not . isUndefinedNodeType) sErrs
        selecter = or . sequenceA [ isUndefinedNodeType
                                 , isUndefinedLinkType
                                 , isUndefinedEffectType
                                 , isCiteError
                                 ]
        sErrs = L.sort errs


-- Extract the UndefinedNodeType elements from a [PubInvalid] and append the
-- current possible NodeTypes to the front, so we don't list them all for each
-- UndefinedNodeType. 
nTypePrep :: [PubInvalid] -> T.Text
nTypePrep errs = errorText <> (sPShowNoColor nodes)
    where
        nodes = (\(UndefinedNodeType x) -> x) <$> nTypeErrs
        errorText = " A NodeType must be one of:\n" <> typesList <> presentText
        presentText = if (length nTypeErrs) == 1 
            then "\n This Node's NodeType is undefined:\n"
            else "\n The following Nodes have undefined NodeTypes:\n"
        typesList = sPShowNoColor optionList
        optionList = (\(AlgRep xs) -> tail xs) algReps
        algReps = dataTypeRep (dataTypeOf (undefined :: NodeType))
        nTypeErrs = filter isUndefinedNodeType errs

isUndefinedNodeType :: PubInvalid -> Bool
isUndefinedNodeType (UndefinedNodeType _) = True
isUndefinedNodeType _                     = False

-- Extract the UndefinedLinkType elements from a [PubInvalid] and append the
-- current possible LinkTypes to the front, so we don't list them all for each
-- UndefinedLinkType. 
lTypePrep :: [PubInvalid] -> T.Text
lTypePrep errs = errorText <> (sPShowNoColor links)
    where
        links = (\(UndefinedLinkType x) -> x) <$> lTypeErrs
        errorText = "A LinkType must be one of:\n" <> typesList <> presentText
        presentText = if (length lTypeErrs) == 1 
          then "\n There is an InLink in this Node whose LinkType is undefined\
                    \:\n"
          else "\n There are Inlinks in the following Nodes whose LinkTypes \
                    \are undefined\n"
        typesList = sPShowNoColor $
            (\xs -> if xs == a then b else xs) <$> optionText
        b = "Process"
        a = "LinkProcess"
        optionText = sPShowNoColor <$> optionList
        optionList =  (\(AlgRep xs) -> tail xs) algReps
        algReps = dataTypeRep (dataTypeOf (undefined :: LinkType))
        lTypeErrs = filter isUndefinedLinkType errs

isUndefinedLinkType :: PubInvalid -> Bool
isUndefinedLinkType (UndefinedLinkType _) = True
isUndefinedLinkType _                     = False

-- Extract the UndefinedEffectType elements from a [PubInvalid] and append the
-- current possible LinkEffects to the front, so we don't list them all for each
-- UndefinedEffectType. 
lEffectPrep :: [PubInvalid] -> T.Text
lEffectPrep errs = errorText <> (sPShowNoColor effects)
    where
        effects = (\(UndefinedEffectType x) -> x) <$> lEffectErrs
        errorText = "A LinkEffect must be one of:\n"
                    <> typesList
                    <> presentText
        presentText = if (length lEffectErrs) == 1 
          then "\n There is an InLink in this Node whose LinkEffect is \
                    \undefined:\n"
          else "\n There are Inlinks in the following Nodes whose LinkEffects \
                    \are undefined\n"
        typesList = sPShowNoColor optionList
        optionList =  (\(AlgRep xs) -> drop 2 xs) algReps
        algReps = dataTypeRep (dataTypeOf (undefined :: LinkEffect))
        lEffectErrs = filter isUndefinedEffectType errs

isUndefinedEffectType :: PubInvalid -> Bool
isUndefinedEffectType (UndefinedEffectType _) = True
isUndefinedEffectType _                       = False

citePrep :: [PubInvalid] -> T.Text
citePrep errs = sPShowNoColor cites
    where
        cites = filter isCiteError errs

isCiteError :: PubInvalid -> Bool
isCiteError x = (isOrphanedModelCites x) || (isExcessDictCites x)

isExcessDictCites :: PubInvalid -> Bool
isExcessDictCites (ExcessDictCites _) = True
isExcessDictCites _                   = False

isOrphanedModelCites :: PubInvalid -> Bool
isOrphanedModelCites (OrphanedModelCites _) = True
isOrphanedModelCites _                      = False

-- Eventually, we would like to publish these models. when that happens, they
-- need to be described in supplementary materials. Everything that is optional
-- in a working model needs to be nailed down at that point. 
mkPublish :: (DMModel, CitationDictionary)
          -> Validation [PubInvalid] (DMModel, CitationDictionary)
mkPublish (dmM, cts) = (,) <$> (mkPublishModel dmM)
                           <*> (mkPublishCiteDict mKeys cts)
    where mKeys = modelCiteKeys dmM

-- Make sure that the BibTeXKeys in a DMModel actually exist in the associated
-- CitationDictionary, and vice versa. 
mkPublishCiteDict :: Set.HashSet BibTeXKey
                  -> CitationDictionary 
                  -> Validation [PubInvalid] CitationDictionary
mkPublishCiteDict mKeys cd
    | (mUniques == Set.empty) && (cdUniques == Set.empty) = Success cd
    | (mUniques /= Set.empty) && (cdUniques == Set.empty) = Failure [errM]
    | (mUniques == Set.empty) && (cdUniques /= Set.empty) = Failure [errCD]
    | (mUniques /= Set.empty) && (cdUniques /= Set.empty) = Failure [errM,errCD]
    where
        mUniques  = Set.difference mKeys cdKeys
        cdUniques = Set.difference cdKeys mKeys
        cdKeys    = Map.keysSet cd
        errM
            | Set.size mUniques == 1 =
              OrphanedModelCites (
                "The following citation key does not exist in\
                \ the CitationDictionary. Please insert it. "
                , Set.toList mUniques)
              
            | otherwise =
              OrphanedModelCites (
                "The following citation keys do not exist in\
                \ the CitationDictionary. Please insert them. "
                , Set.toList mUniques)
        errCD
            | Set.size cdUniques == 1 =
              ExcessDictCites (
                "The following citation key is not referenced \
                \in the model. Either reference it there or remove it from the\
                \ CitationDictionary. "
                , Set.toList cdUniques)
            | otherwise =
              ExcessDictCites (
                "The following citation keys are not referenced\
                \ in the model. Either reference them there or remove them from\
                \ the CitationDictionary. "
                , Set.toList cdUniques)

mkPublishModel :: DMModel -> Validation [PubInvalid] DMModel
mkPublishModel (Fine mL) = Fine <$> (mkLPublish mL)
mkPublishModel (LayerBinding mMap mL dmM) = LayerBinding
                                            <$> (pure mMap)
                                            <*> (mkLPublish mL)
                                            <*> (mkPublishModel dmM)

mkLPublish :: ModelLayer -> Validation [PubInvalid] ModelLayer
mkLPublish (ModelLayer mG mMeta) = ModelLayer 
                                    <$> (mkMGPublish mG)
                                    <*> (mkMMPublish mMeta)

mkMMPublish :: ModelMeta -> Validation [PubInvalid] ModelMeta
mkMMPublish mMeta =
    ModelMeta <$> (pure $ modelName mMeta)
              <*> (pure $ modelVersion mMeta)
              <*> (pure $ modelPaper mMeta)
              <*> (pure $ biasOrderFirst mMeta)
              <*> (pure $ biasOrderLast mMeta)
              <*> (mkDescPublish (ModelD $ modelName mMeta) (modelInfo mMeta))

-- Check if the Description part of a LitInfo is empty, which it should not be
-- for publication. If it is, error out with the name of whatever the LitInfo is
-- attached to. 
mkDescPublish :: MissingDescription -- The kind and name of the associated
              -- structure. Either the name of the DMNode, the name of the Node
              -- associated with the InLink, or the name of the DMModel. 
              -> LitInfo 
              -> Validation [PubInvalid] LitInfo
mkDescPublish md lInfo =  case (fst . fst) lInfo of
    "" -> Failure [PubMissingDesc md]
    _  -> Success $ lInfo

mkMGPublish :: ModelGraph -> Validation [PubInvalid] ModelGraph
mkMGPublish mG = Gr.mkGraph <$> checkedNodes <*> checkedEdges
    where
        checkedNodes = traverse mkPublishNode $ Gr.labNodes mG
        checkedEdges = traverse (mkPublishLink mG) $ Gr.labEdges mG

mkPublishNode :: Gr.LNode DMNode -> Validation [PubInvalid] (Gr.LNode DMNode)
mkPublishNode (n, (DMNode nM nG)) = (,) <$> (pure n) <*> pubNode
    where
        pubNode = DMNode <$> (mkPublishNodeMeta nM) <*> (pure nG)

mkPublishNodeMeta :: NodeMeta -> Validation [PubInvalid] NodeMeta
mkPublishNodeMeta nMD = let nName = nodeName nMD in
    NodeMeta <$> (pure nName)
             <*> (pure $ nodeGenes nMD)
             <*> (mkNodeTypePublish nName (nodeType nMD))
             <*> (mkNodeColorPublish nName (nodeColor nMD))
             <*> (mkNodeCoordPublish nName (nodeCoordinate nMD))
             <*> (mkDescPublish (NodeD nName) (nodeInfo nMD))

-- Use Data.Data to display the current valid NodeTypes. 
mkNodeTypePublish :: NodeName -> NodeType -> Validation [PubInvalid] NodeType
mkNodeTypePublish nN nT = case nT of
    Undefined_NT -> Failure [err]
    _            -> Success nT
    where
        err = UndefinedNodeType nN
    
mkNodeColorPublish :: NodeName
                   -> LocalColor
                   -> Validation [PubInvalid] LocalColor
mkNodeColorPublish nN nC
    | nC == defaultColor = Failure [err]
    | otherwise          = Success nC
    where
        err = UnspecifiedNodeColor $ "In Node " <> nN
            <> ". Choose an SVG color."
    

mkNodeCoordPublish :: NodeName
                   -> U.Vector Double
                   -> Validation [PubInvalid] (U.Vector Double)
mkNodeCoordPublish nN nV
    | l == 2    = Success nV
    | l == 0    = Failure [err0]
    | otherwise = Failure [errN]
        where
            l = U.length nV
            err0 = MissingCoord $ "Coord of Node " <> nN <>
                " is unspecified. Add a 2D coord. "
            errN = CoordWrongDimension $ "Coord of Node" <> nN <>
                " is not 2D. Project into 2D"

mkPublishLink :: ModelGraph
              -> Gr.LEdge DMLink 
              -> Validation [PubInvalid] (Gr.LEdge DMLink)
mkPublishLink mG e@(fromN, toN, dmL) = (,,) <$> (pure fromN) <*> (pure toN) <*>
    (
        DMLink <$> (mkLinkEffectPublish nName lEffect) 
               <*> (mkLinkTypePublish nName lType)
               <*> (mkDescPublish (InLinkD nName) lInfo)
    )
    where
        lEffect = linkEffect dmL
        lType = linkType dmL
        lInfo = linkInfo dmL
        nName = (nodeName . nodeMeta . thdOf4 . fromJust . fst) nDecomp
        nDecomp = Gr.match (sndOf3 e) mG

mkLinkEffectPublish :: NodeName
                    -> LinkEffect
                    -> Validation [PubInvalid] LinkEffect
mkLinkEffectPublish nN lE = case lE of
    Undefined_LE -> Failure [err]
    _            -> Success lE
    where
        err = UndefinedEffectType nN

mkLinkTypePublish :: NodeName -> LinkType -> Validation [PubInvalid] LinkType
mkLinkTypePublish nN lT = case lT of
    Undefined_LT -> Failure [err]
    _            -> Success lT
    where
        err = UndefinedLinkType nN
        