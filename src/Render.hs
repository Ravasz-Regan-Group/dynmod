{-# LANGUAGE OverloadedStrings #-}

module Render 
    ( renderGML
    , renderDMMS
    , renderSwitch
    , renderDMMSDiff
    , purgeTableRenderDMMS
    )
    where

import Types.GML
import Types.DMModel
import Constants
import Compare
import Utilities
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Versions as Ver
import qualified Data.Colour.SRGB as SC
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L
import Data.Function (on)

renderGML :: GML -> T.Text
renderGML = flip gmlListRender 0

gmlListRender :: [(GKey, GValue)] -> Int -> T.Text
gmlListRender kvPs tabDepth = T.intercalate "\n" $ pairRender <$> kvPs
  where
      tabber i = T.replicate i "\t"
      pairRender (k, v) = case v of
          GInt  i -> tabber tabDepth <> k <> "\t" <> (T.pack . show) i
          GReal r -> tabber tabDepth <> k <> "\t" <> (T.pack . show) r
          GStr  x -> tabber tabDepth <> k <> "\t" <> "\"" <> x <> "\""
          GList g ->   tabber tabDepth <> k <> "\n"
                    <> tabber tabDepth <> "[\n" 
                                       <> gmlListRender g (tabDepth + 1) <> "\n"
                    <> tabber tabDepth <> "]"

renderDMMS :: FileFormatVersion -> DMModel -> CitationDictionary -> T.Text
renderDMMS ver dmm cDict =
    T.intercalate "\n\n" [ "FormatVersion: " <> (Ver.prettySemVer ver)
                         , renderDMModel dmm
                         , renderCDict cDict
                         ]

renderDMModel :: DMModel -> T.Text
renderDMModel (Fine (ModelLayer mg mmd)) = dmmsWrap "Model" entries
    where
        entries = renderMMeta mmd <> "\n\n"
               <> renderMGraph mg
renderDMModel (LayerBinding mm (ModelLayer mg mmd) dmm) =
    dmmsWrap "Model" entries
        where
            entries = renderMMeta mmd <> "\n\n"
                   <> renderMMaping mm <> "\n\n"
                   <> renderMGraph mg  <> "\n\n"
                   <> renderDMModel dmm


renderMMeta :: ModelMeta -> T.Text
renderMMeta (ModelMeta mName mVer paper bOF bOL mInfo) =
    dmmsWrap "ModelMetaData" entries
        where
            entries = T.intercalate "\n"
                [rMName, rMVer, rPaper, rMDesc, rMNotes, rBOF, rBOL]
            rMName = "ModelName: " <> mName
            rMVer = "ModelVersion: " <> Ver.prettySemVer mVer
            rPaper = "ModelPaper: " <> T.intercalate "," paper
            rMDesc = "ModelDescription: " <> (fst . fst) mInfo
            rMNotes = "ModelNotes: " <> (fst . snd) mInfo
            rBOF = "BiasOrderFirst: " <> (T.intercalate ", " $ pairShow <$> bOF)
            rBOL = "BiasOrderLast: " <> (T.intercalate ", " $ pairShow <$> bOL)
            pairShow (n, i) = "(" <> n <> ", " <> (T.pack . show) i <> ")" 

renderMMaping :: ModelMapping -> T.Text
renderMMaping mm = dmmsWrap "ModelMapping" entries 
    where
        entries = T.intercalate "\n" $ renderSwitch <$> mm

renderSwitch :: (NodeName, [NodeName]) -> T.Text
renderSwitch (s, ns) = "Switch: " <> s <> " (" <> (T.intercalate ", " ns) <> ")"

renderMGraph :: ModelGraph -> T.Text
renderMGraph mg = dmmsWrap "ModelGraph" entries
    where
        entries = T.intercalate "\n\n" nodes
        nodes = renderDMMSNode ranges <$> dmmsNS
        ranges = Map.fromList $ (nodeRange . fst) <$> dmmsNS
        dmmsNS = dmmsNodes mg

renderDMMSNode :: LayerRange -> DMMSNode -> T.Text
renderDMMSNode r (n, ls) = dmmsWrap "Node" entries <> " //" <> nName
    where
        entries = T.intercalate "\n\n" $ [meta, gate] <> links
        meta = renderNMeta $ nodeMeta n
        gate = renderGate r n
        links = renderNamedLink <$> ls
        nName = (nodeName . nodeMeta) n

renderNMeta :: NodeMeta -> T.Text
renderNMeta nm = dmmsWrap "NodeMetaData" entries
    where
        entries = T.intercalate "\n"
            [rName, rGene, rType, rColor, rCoord, rDesc, rNotes]
        rName = "NodeName: " <> (nodeName nm)
        rGene = "NodeGenes: " <>
            (T.intercalate ", " $ (T.pack . show) <$> (nodeGenes nm))
        rType = "NodeType: " <> ((T.pack . show . nodeType) nm)
        rColor = "NodeColor: " <> (renderColor . nodeColor) nm
        rCoord = "NodeCoordinate: " <>
            (T.intercalate ", " $ (T.pack . show) <$>
                ((U.toList . nodeCoordinate) nm))
        rDesc = "NodeDescription: " <> (fst . fst) nInfo
        rNotes = "NodeNotes: " <> (fst . snd) nInfo
        nInfo = nodeInfo nm

renderColor :: LocalColor -> T.Text
renderColor c = case L.lookup c cPairs of
    Just svgText -> svgText
    Nothing   -> (T.pack . SC.sRGB24show) c
    where
        cPairs = zip svgLocalColors svgColors

renderGate :: LayerRange -> DMNode -> T.Text
renderGate lr n = dmmsWrap "NodeGate" entries
    where
        entries = case gateOrigin nGate of
            DMMSTruthTable -> renderTableG nGate
            LogicalExpression -> renderDiscreteG lr nGate
            Both -> renderDiscreteG lr nGate <> "\n" <> renderTableG nGate
        nGate = nodeGate n

renderDiscreteG :: LayerRange -> NodeGate -> T.Text
renderDiscreteG r ng = dmmsWrap "DiscreteLogic" entries
    where
        entries
            | length assigns == 1
                = "\t" <> nName <> " *= " <> (renderExpr boolNS $ last exprs)
            | otherwise = T.intercalate "\n" rExprs
        rExprs = ("\t" <>) <$> (zipWith ((<> ) . (<> " *= "))
                            gStates $ renderExpr boolNS <$> exprs)
        gStates = ((assignName <>) . T.pack . show . fst) <$> assigns
        exprs = snd <$> assigns
--      We never write out the zeroth state in a dmms, so only take the tail
--      of the gate assignments. 
        assigns = (tail . gateAssigns) ng
        assignName = nName <> ":"
        nName = gNodeName ng
        boolNS = Map.keysSet $ Map.filter (== 1) r

renderExpr :: Set.HashSet NodeName -> NodeExpr -> T.Text
renderExpr _ (GateLit b) = (T.pack . show) b
renderExpr s (GateConst n st) = case Set.member n s of
    True -> case st of
        0 -> "not " <> n
        _ -> n
    False -> n <> ":" <> (T.pack . show) st
renderExpr s (Not expr) = "not " <> parRenderExpr s expr
renderExpr s (Binary And expr1 expr2) =
    (parRenderExpr s expr1) <> " and " <> (parRenderExpr s expr2)
renderExpr s (Binary Or expr1 expr2) =
    (parRenderExpr s expr1) <> " or " <> (parRenderExpr s expr2)

parRenderExpr :: Set.HashSet NodeName -> NodeExpr -> T.Text
parRenderExpr s ex = case ex of
    (GateLit _)     -> renderExpr s ex
    (GateConst _ _) -> renderExpr s ex
    _               -> "(" <> renderExpr s ex <> ")"


-- For when we want to render a gate but do not have the LayerRange available.  
renderDiscreteGBlind :: NodeGate -> T.Text
renderDiscreteGBlind ng
    | length assigns == 1 = "\t" <> nName <> " *= " <> 
                                        (renderExprBlind $ last exprs)
    | otherwise = T.intercalate "\n" rExprs
        where
            rExprs = ("\t" <>) <$> (zipWith ((<> ) . (<> " *= ")) gStates $
                renderExprBlind <$> exprs)
            gStates = ((assignName <>) . T.pack . show . fst) <$> assigns
            exprs = snd <$> assigns
           -- We never write out the zeroth state in a dmms,
           -- so only take the tail of the gate assignments.
            assigns = (tail . gateAssigns) ng
            assignName = nName <> ":"
            nName = gNodeName ng

renderExprBlind :: NodeExpr -> T.Text
renderExprBlind (GateLit b) = (T.pack . show) b
renderExprBlind (GateConst n st) = n <> ":" <> (T.pack . show) st
renderExprBlind (Not expr) = "not " <> parRenderExprBlind expr
renderExprBlind (Binary And expr1 expr2) =
    (parRenderExprBlind expr1) <> " and " <> (parRenderExprBlind expr2)
renderExprBlind (Binary Or expr1 expr2) =
    (parRenderExprBlind expr1) <> " or " <> (parRenderExprBlind expr2)

parRenderExprBlind :: NodeExpr -> T.Text
parRenderExprBlind ex = case ex of
    (GateLit _)     -> renderExprBlind ex
    (GateConst _ _) -> renderExprBlind ex
    _               -> "(" <> renderExprBlind ex <> ")"

renderTableG :: NodeGate -> T.Text
renderTableG ng = dmmsWrap "TruthTable" entries
    where
        entries = topLine <> "\n" <> (T.intercalate "\n" rows)
        topLine = T.intercalate "\t" $ gateOrder ng <> [gNodeName ng]
        rows = L.sort $ zipWith (<>) inputRows tOutputs
        inputRows =
            (T.intercalate "\t" . ((T.pack . show) <$>) . U.toList) <$> vecs
        tOutputs = ((<>) "\t" . T.pack . show) <$> outputs
        (vecs, outputs) = (unzip . Map.toList . gateTable) ng

renderNamedLink :: (NodeName, DMLink) -> T.Text
renderNamedLink (inName, dmL) =
    dmmsWrap "InLink" (renderNamedLinkContent (dmL, inName)) <> " //" <> inName

renderNamedLinkContent :: (DMLink, NodeName) -> T.Text
renderNamedLinkContent (dmL, inName) = entries
    where
        entries = T.intercalate "\n"
            [rInNode, rEffect, rType, rDesc, rNotes]
        rInNode = "InputNode: " <> inName
        rEffect = "LinkEffect: " <> ((T.pack . show . linkEffect) dmL)
        rType = "LinkType: " <> ((T.pack . show . linkType) dmL)
        rDesc = "LinkDescription: " <> (fst . fst) lInfo
        rNotes = "LinkNotes: " <> (fst . snd) lInfo
        lInfo = linkInfo dmL

-- Consume a dmms keyword, the content for that keyword, and properly wrap the
-- second in the first
dmmsWrap :: T.Text -> T.Text -> T.Text
dmmsWrap keyword body = keyword <> "{\n" <> body <> "\n" <> keyword <> "}"

renderCDict :: CitationDictionary -> T.Text
renderCDict dict = dmmsWrap "CitationDictionary" entries
    where
        entries = T.intercalate "\n\n" $ renderCitation <$> (Map.toList dict)

renderCitation :: (BibTeXKey, BibTeXEntry) -> T.Text
renderCitation (_,(BibTeXEntry eKey eType fields)) = opener <> entries <> closer
    where
        opener = "@" <> eType <> "{" <> eKey <> ",\n  "
        closer = "\n}"
        entries = T.intercalate ",\n  " $ fieldRender <$> fields

fieldRender :: (BibTeXField, BibTeXRecord) -> T.Text
fieldRender (k, r) = k <> " = " <> r


-- Consumes the names, in order, of the  compared DMMS files, and their diff. 
renderDMMSDiff :: T.Text -> T.Text -> DMModelDiff -> T.Text
renderDMMSDiff dmms1 dmms2 dmMDiff =
    dmms1 <> " vs " <> dmms2 <> "\n\n" <> renderDMModelDiff dmMDiff

renderDMModelDiff :: DMModelDiff -> T.Text
renderDMModelDiff (CoarseDiff mmD ((lN1, lN2), lD) dmD) =
    "Layers " <> lN1 <> " and " <> lN2 <> ":\n" <>
    mmDRenderWrapped <>
    mlDRenderWrapped <>
    "\n****************************************\n" <>
    renderDMModelDiff dmD
    where
        mmDRenderWrapped
            | T.null mmDRender = ""
            | otherwise = "ModelMapping diff:\n" <> mmDRender <> "\n"
        mmDRender = renderMappingDiff mmD
        mlDRenderWrapped
            | T.null mmDRender = ""
            | otherwise = "ModelLayer diff:\n" <> mlDRender <> "\n"
        mlDRender = renderLayerDiff lD
renderDMModelDiff (FineDiff ((lN1, lN2), lD)) =
    "Layers " <> lN1 <> " and " <> lN2 <> ":\n" <>
    "ModelLayer diff:\n" <> renderLayerDiff lD <> "\n"

renderMappingDiff :: MappingDiff -> T.Text
renderMappingDiff (SD (LD ldSwitchNames)
                      (RD rdSwitchNames)
                      (FC (SD (LD ldSwitchContent)
                              (RD rdSwitchContent)
                              (FC fcNameContent)
                          )
                      )
                  ) = ldSN <> rdSN <> ldSC <> rdSC <> fcSC
    where
        ldSN | null ldSwitchNames = ""
             | otherwise = "Left unique Switch names:\n" <>
                T.intercalate "\n" (renderSwitch <$> ldSwitchNames) <> "\n"
        rdSN | null rdSwitchNames = ""
             | otherwise = "Right unique Switch names:\n" <>
                T.intercalate "\n" (renderSwitch <$> rdSwitchNames) <> "\n"
        ldSC | null ldSwitchContent = ""
             | otherwise = "Left shared Switch names but unique content:\n" <>
                 T.intercalate "\n" (renderSwitch <$> ldSwitchContent) <> "\n"
        rdSC | null ldSwitchContent = ""
             | otherwise = "Right shared Switch names but unique content:\n" <>
                 T.intercalate "\n" (renderSwitch <$> rdSwitchContent) <> "\n"
        fcSC | null fcNameContent = ""
             | otherwise = "Identical Switches:\n" <>
                 T.intercalate "\n" (renderSwitch <$> fcNameContent) <> "\n"

renderLayerDiff :: SplitDiff [NodeName] [NodeDiff] -> T.Text
renderLayerDiff (SD (LD lNNames) (RD rNNames) (FC nDiffs)) =
    lUNs <> rUNs <> sharedNodeDiffRenders
    where
        lUNs
            | null lNNames = ""
            | otherwise = "Left unique NodeNames:\n" <>
                T.intercalate ", " lNNames <> "\n"
        rUNs
            | null rNNames = ""
            | otherwise = "Right unique NodeNames:\n" <>
                T.intercalate ", " rNNames <> "\n"
        sharedNodeDiffRenders
            | and $ T.null <$> nDiffRenders = ""
            | otherwise = "Shared DMNode diffs:\n" <> T.concat nDiffRenders
                where
                    nDiffRenders = renderNodeDiff <$> nDiffs

renderNodeDiff :: NodeDiff -> T.Text
renderNodeDiff (NodeDiff nN nTD lDs tD)
    | (T.null typesMaybe) && (T.null rLinkDiffs) && (T.null tablesMaybe) = ""
    | otherwise =
        "Node: " <> nN <>
        typesMaybe <>
        rLinkDiffs <>
        tablesMaybe <>
            "\n--------------------------------------------------\n"
    where
    typesMaybe = case nTD of
        Nothing -> ""
        Just (lType, rType) -> "\n" <> "NodeTypes vary: " <>
            ((T.pack . show) lType) <> " vs " <> ((T.pack . show) rType) <> "\n"
    rLinkDiffs = renderLinkDiff lDs
    tablesMaybe = case tD of
        Left gates -> "\n" <> "Gate Inlinks differ, so comparing the gate\
                \ outputs is nonsensical. " <>
            "The gates follow, if their GateOrigins were LogicalExpressions. "
            <> maybeLeftGate <> maybeRightGate
                where
                    maybeLeftGate
                        | (gateOrigin . fst) gates == DMMSTruthTable = ""
                        | otherwise = "\n" <> (renderDiscreteGBlind $ fst gates)
                    maybeRightGate
                        | (gateOrigin . snd) gates == DMMSTruthTable = ""
                        | otherwise = "\n" <> (renderDiscreteGBlind $ snd gates)
        Right tDiff -> case renderedTD of
            ""  -> ""
            _ -> "\nGate diff:\n" <> renderedTD
            where renderedTD = renderTableDiff tDiff

renderLinkDiff :: LinkDiff -> T.Text
renderLinkDiff (SD (LD lLinks) (RD rLinks) (FC lteDiffs))
    | (T.null lU) && (T.null rU) && (T.null lteDiffRenders) = ""
    | otherwise = "\nInLink diffs:\n" <> lU <> rU <> lteDiffRenders
    where
        lU
            | null lLinks = ""
            | otherwise = "\nLeft unique InLinks: " <> 
                T.intercalate "," (snd <$> lLinks) <> "\n"
        rU
            | null rLinks = ""
            | otherwise = "\nRight unique InLinks: " <> 
                T.intercalate "," (snd <$> rLinks) <> "\n"
        lteDiffRenders
            | and $ nullLTEDiff <$> lteDiffs = ""
            | otherwise = "Shared InLink diffs:\n" <>
                T.intercalate "\n" (renderLTEDiff <$> lteDiffs)

nullLTEDiff :: LTEDiff -> Bool
nullLTEDiff (LTEDiff _ Nothing Nothing) = True
nullLTEDiff _ = False

renderLTEDiff :: LTEDiff -> T.Text
renderLTEDiff (LTEDiff nN lT lE) = case (lT, lE) of
    (Nothing, Nothing) -> "Inlink " <> nN <> " perfectly shared."
    (Just t, Nothing) -> "Inlink " <> nN <> " has differing NodeTypes: " <>
        ((T.pack . show . fst) t) <> " vs " <> ((T.pack . show . fst) t)
    (Nothing, Just e) -> "Inlink " <> nN <> " has differing NodeEffects: " <>
        ((T.pack . show . fst) e) <> " vs " <> ((T.pack . show . fst) e)
    (Just t, Just e) -> "Inlink " <> nN <>
        " has differing NodeTypes: " <>
        ((T.pack . show . fst) t) <> " vs " <> ((T.pack . show . fst) t) <>
        "\n" <> "and NodeEffects: " <>
        ((T.pack . show . fst) e) <> " vs " <> ((T.pack . show . fst) e)

renderTableDiff :: TableDiff -> T.Text
renderTableDiff (nodes, (SD (LD lTable) (RD rTable) _)) = case rows of
    []  -> ""
    mis -> topLine <> "\n" <> (T.intercalate "\n" mis)
    
    where
        rows = zipWith (<>) inputRows tOutputs
        topLine = T.intercalate "\t" $ fst orders <> [nN]
        orders = isoBimap (gateOrder . nodeGate) nodes
        nN = (nodeName . nodeMeta . fst) nodes
        inputRows =
            (T.intercalate "\t" . ((T.pack . show) <$>) . U.toList) <$> vecs
        tOutputs = ((<>) "\t") <$> outs
        (vecs, outs) = unzip $ L.sortBy (compare `on` fst) $ zipdLR
        zipdLR = ((<>) "\t" . T.pack . show) <<$>> (zipWith zipper lList rList)
        zipper (x, y) (_, b) = (x, (y, b))
        lList = L.sortBy (compare `on` fst) $ Map.toList lTable
        rList = L.sortBy (compare `on` fst) $ Map.toList rTable

-- Write out a DMModel to Text, but do not write out a TruthTable if the
-- GateOrigin is LogicalExpression OR Both. 
purgeTableRenderDMMS :: FileFormatVersion
                     -> DMModel
                     -> CitationDictionary
                     -> T.Text
purgeTableRenderDMMS ver dmm cDict =
    T.intercalate "\n\n" [ "FormatVersion: " <> (Ver.prettySemVer ver)
                         , purgeTableRenderDMModel dmm
                         , renderCDict cDict
                         ]

purgeTableRenderDMModel :: DMModel -> T.Text
purgeTableRenderDMModel (Fine (ModelLayer mg mmd)) = dmmsWrap "Model" entries
    where
        entries = renderMMeta mmd <> "\n\n"
               <> purgeTableRenderMGraph mg
purgeTableRenderDMModel (LayerBinding mm (ModelLayer mg mmd) dmm) =
    dmmsWrap "Model" entries
        where
            entries = renderMMeta mmd <> "\n\n"
                   <> renderMMaping mm <> "\n\n"
                   <> purgeTableRenderMGraph mg  <> "\n\n"
                   <> purgeTableRenderDMModel dmm

purgeTableRenderMGraph :: ModelGraph -> T.Text
purgeTableRenderMGraph mg = dmmsWrap "ModelGraph" entries
    where
        entries = T.intercalate "\n\n" nodes
        nodes = purgeTableRenderDMMSNode ranges <$> dmmsNS
        ranges = Map.fromList $ (nodeRange . fst) <$> dmmsNS
        dmmsNS = dmmsNodes mg

purgeTableRenderDMMSNode :: LayerRange -> DMMSNode -> T.Text
purgeTableRenderDMMSNode r (n, ls) = dmmsWrap "Node" entries <> " //" <> nName
    where
        entries = T.intercalate "\n\n" $ [meta, gate] <> links
        meta = renderNMeta $ nodeMeta n
        gate = purgeTableRenderGate r n
        links = renderNamedLink <$> ls
        nName = (nodeName . nodeMeta) n

purgeTableRenderGate :: LayerRange -> DMNode -> T.Text
purgeTableRenderGate lr n = dmmsWrap "NodeGate" entries
    where
        entries = case gateOrigin nGate of
            DMMSTruthTable -> renderTableG nGate
            _ -> renderDiscreteG lr nGate
        nGate = nodeGate n
