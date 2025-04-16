{-# LANGUAGE OverloadedStrings #-}

module Render 
    ( renderGML
    , renderDMMS
    , renderDMMSDiff
    , renderDMMSSwitch
    , purgeTableRenderDMMS
    , renderDMExpOutput
    , renderNBCData
    , renderPhBCData
    )
    where

import Types
import Constants
import Compare
import Utilities
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Bimap as BM
import qualified Data.Versions as Ver
import qualified Data.Colour.SRGB as SC
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import TextShow
import qualified Data.List as L
import Data.Function (on)

renderGML :: GML -> T.Text
renderGML = flip gmlListRender 0

gmlListRender :: [(GKey, GValue)] -> Int -> T.Text
gmlListRender kvPs tabDepth = T.intercalate "\n" $ pairRender <$> kvPs
  where
      tabber i = T.replicate i "\t"
      pairRender (k, v) = case v of
          GInt  i -> tabber tabDepth <> k <> "\t" <> showt i
          GReal r -> tabber tabDepth <> k <> "\t" <> showt r
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
renderDMModel (Fine (ModelLayer mg mmd)) = dmmsWrap "Model" entries Nothing
    where
        entries = renderMMeta mmd <> "\n\n"
               <> renderMGraph mg
renderDMModel (LayerBinding mm (ModelLayer mg mmd) dmm) =
    dmmsWrap "Model" entries Nothing
        where
            entries = renderMMeta mmd <> "\n\n"
                   <> renderMMaping mm <> "\n\n"
                   <> renderMGraph mg  <> "\n\n"
                   <> renderDMModel dmm


renderMMeta :: ModelMeta -> T.Text
renderMMeta (ModelMeta mName mVer paper bOF bOL mInfo) =
    dmmsWrap "ModelMetaData" entries Nothing
        where
            entries = T.intercalate "\n"
                [rMName, rMVer, rPaper, rMDesc, rMNotes, rBOF, rBOL]
            rMName = "ModelName: " <> mName
            rMVer = "ModelVersion: " <> Ver.prettySemVer mVer
            rPaper = "ModelPaper: " <> T.intercalate "," paper
            rMDesc = "ModelDescription: " <> desc mInfo
            rMNotes = "ModelNotes: " <> note mInfo
            rBOF = "BiasOrderFirst: " <> (T.intercalate ", " $ boShow <$> bOF)
            rBOL = "BiasOrderLast: " <> (T.intercalate ", " $ boShow <$> bOL)
            boShow (WholeNode n) = n
            boShow (SpecificState n i) = "(" <> n <> ", " <> showt i <> ")"

renderMMaping :: ModelMapping -> T.Text
renderMMaping mm = (dmmsWrap "ModelMapping" dmmsMMEntries Nothing) <>
    switchProfilesText
    where
        switchProfilesText
            | L.null switchProfiles = ""
            | otherwise = "\n\n" <>
                            (dmmsWrap "SwitchProfiles" spEntries Nothing)
        (dmmsMMaping, switchProfiles) = modelMappingSplit mm
        dmmsMMEntries = T.intercalate "\n" $ renderDMMSSwitch <$> dmmsMMaping
        spEntries = T.intercalate "\n" $ renderSwitchProfile <$> switchProfiles

renderDMMSSwitch :: (NodeName, [NodeName]) -> T.Text
renderDMMSSwitch (s, ns) =
    "Switch: " <> s <> " (" <> (T.intercalate ", " ns) <> ")"

renderSwitchProfile :: SwitchProfile -> T.Text
renderSwitchProfile (nN, phs) = dmmsWrap "SwitchPhenotypes" entry (Just nN)
    where
        entry = "SwitchName: " <> nN <> "\n" <> entries
        entries = T.intercalate "\n" $ renderPhenotype <$> phs

renderPhenotype :: Phenotype -> T.Text
renderPhenotype ph =
    "            " <>
    phenotypeName ph <>
    ":" <>
    ((showt . switchNodeState) ph) <>
    " *= " <>
    (T.intercalate " -> " $ renderSubSpace <$> (fingerprint ph))

renderSubSpace :: SubSpace -> T.Text
renderSubSpace sSp = "(" <> entries <> ")"
    where
        entries = T.intercalate ", " $ renderSSElem <$> sSp
        renderSSElem (nN, nS) = nN <> ":" <> showt nS

renderMGraph :: ModelGraph -> T.Text
renderMGraph mg = dmmsWrap "ModelGraph" entries Nothing
    where
        entries = T.intercalate "\n\n" nodes
        nodes = renderDMMSNode ranges <$> dmmsNS
        ranges = Map.fromList $ (nodeRange . fst) <$> dmmsNS
        dmmsNS = dmmsNodes mg

renderDMMSNode :: LayerRange -> DMMSNode -> T.Text
renderDMMSNode r (n, ls) = dmmsWrap "Node" entries (Just nName)
    where
        entries = T.intercalate "\n\n" $ [meta, gate] <> links
        meta = renderNMeta $ nodeMeta n
        gate = renderGate r n
        links = renderNamedLink <$> ls
        nName = (nodeName . nodeMeta) n

renderNMeta :: NodeMeta -> T.Text
renderNMeta nm = dmmsWrap "NodeMetaData" entries Nothing
    where
        entries = T.intercalate "\n"
            [rName, rGene, rType, rColor, rCoord, rDesc, rNotes]
        rName = "NodeName: " <> (nodeName nm)
        rGene = "NodeGenes: " <>
            (T.intercalate ", " $ showt <$> (nodeGenes nm))
        rType = "NodeType: " <> ((showt . nodeType) nm)
        rColor = "NodeColor: " <> (renderColor . nodeColor) nm
        rCoord = "NodeCoordinate: " <>
            (T.intercalate ", " $ showt <$> ((U.toList . nodeCoordinate) nm))
        rDesc = "NodeDescription: " <> desc nInfo
        rNotes = "NodeNotes: " <> note nInfo
        nInfo = nodeInfo nm

renderColor :: LocalColor -> T.Text
renderColor c = case L.lookup c cPairs of
    Just svgText -> svgText
    Nothing   -> (T.pack . SC.sRGB24show) c
    where
        cPairs = zip svgLocalColors svgColors

renderGate :: LayerRange -> DMNode -> T.Text
renderGate lr n = dmmsWrap "NodeGate" entries Nothing
    where
        entries = case gateOrigin nGate of
            DMMSTruthTable -> renderTableG nGate
            LogicalExpression -> renderDiscreteG lr nGate
            Both -> renderDiscreteG lr nGate <> "\n" <> renderTableG nGate
        nGate = nodeGate n

renderGate' :: LayerRange -> NodeGate -> T.Text
renderGate' lr nGate = dmmsWrap "NodeGate" entries Nothing
    where
        entries = case gateOrigin nGate of
            DMMSTruthTable -> renderTableG nGate
            LogicalExpression -> renderDiscreteG lr nGate
            Both -> renderDiscreteG lr nGate <> "\n" <> renderTableG nGate

renderDiscreteG :: LayerRange -> NodeGate -> T.Text
renderDiscreteG r ng = dmmsWrap "DiscreteLogic" entries Nothing
    where
        entries
            | length assigns == 1
                = "\t" <> nName <> " *= " <> (renderExpr boolNS $ last exprs)
            | otherwise = T.intercalate "\n" rExprs
        rExprs = ("\t" <>) <$> (zipWith ((<> ) . (<> " *= "))
                            gStates $ renderExpr boolNS <$> exprs)
        gStates = ((assignName <>) . showt . fst) <$> assigns
        exprs = snd <$> assigns
--      We never write out the zeroth state in a dmms, so only take the tail
--      of the gate assignments. 
        assigns = (tail . gateAssigns) ng
        assignName = nName <> ":"
        nName = gNodeName ng
        boolNS = Map.keysSet $ Map.filter (== 1) r

renderExpr :: Set.HashSet NodeName -> NodeExpr -> T.Text
renderExpr _ (GateLit b) = showt b
renderExpr s (GateConst n st) = case Set.member n s of
    True -> case st of
        0 -> "not " <> n
        _ -> n
    False -> n <> ":" <> showt st
renderExpr s (Not expr) = "not " <> renderExpr s expr
renderExpr s (Pars expr) = "(" <> renderExpr s expr <> ")"
renderExpr s (Binary And expr1 expr2) =
    (renderExpr s expr1) <> " and " <> (renderExpr s expr2)
renderExpr s (Binary Or expr1 expr2) =
    (renderExpr s expr1) <> " or " <> (renderExpr s expr2)


renderTableG :: NodeGate -> T.Text
renderTableG ng = dmmsWrap "TruthTable" entries Nothing
    where
        entries = topLine <> "\n" <> (T.intercalate "\n" rows)
        topLine = T.intercalate "\t" $ gateOrder ng <> [gNodeName ng]
        rows = L.sort $ zipWith (<>) inputRows tOutputs
        inputRows = (T.intercalate "\t" . fmap showt . U.toList) <$> vecs
        tOutputs = ((<>) "\t" . showt) <$> outputs
        (vecs, outputs) = (unzip . Map.toList . gateTable) ng

renderNamedLink :: (NodeName, DMLink) -> T.Text
renderNamedLink (inName, dmL) =
    dmmsWrap "InLink" (renderNamedLinkContent (dmL, inName)) Nothing <>
        " //" <> inName

renderNamedLinkContent :: (DMLink, NodeName) -> T.Text
renderNamedLinkContent (dmL, inName) = entries
    where
        entries = T.intercalate "\n"
            [rInNode, rEffect, rType, rDesc, rNotes]
        rInNode = "InputNode: " <> inName
        rEffect = "LinkEffect: " <> ((showt . linkEffect) dmL)
        rType = "LinkType: " <> ((showt . linkType) dmL)
        rDesc = "LinkDescription: " <> desc lInfo
        rNotes = "LinkNotes: " <> note lInfo
        lInfo = linkInfo dmL

-- Consume a dmms keyword, the content for that keyword, and properly wrap the
-- second in the first. Optionally add a commented note at the end. 
dmmsWrap :: T.Text -> T.Text -> Maybe T.Text -> T.Text
dmmsWrap keyword body Nothing =
    keyword <> "{\n" <> body <> "\n" <> keyword <> "}"
dmmsWrap keyword body (Just cNote) = 
    keyword <> "{\n" <> body <> "\n" <> keyword <> "}"  <> " // " <> cNote

renderCDict :: CitationDictionary -> T.Text
renderCDict dict = dmmsWrap "CitationDictionary" entries Nothing
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
renderDMModelDiff (CoarseDiff (mmD, spD) (((lN1, lR1), (lN2, lR2)), lD) dmD) =
    "Layers " <> lN1 <> " and " <> lN2 <> ":\n" <>
    dmmsMMDRenderWrapped <>
    spDRenderWrapped <>
    mlDRenderWrapped <>
    "\n****************************************\n" <>
    renderDMModelDiff dmD
    where
        dmmsMMDRenderWrapped
            | T.null dmmsMMDRender = ""
            | otherwise = "ModelMapping diff:\n" <> dmmsMMDRender <> "\n"
        dmmsMMDRender = renderDMMSMappingDiff mmD
        spDRenderWrapped
            | T.null spDRender = ""
            | otherwise = "SwitchProfiles diff:\n" <> spDRender <> "\n"
        spDRender = renderSPDiff spD
        mlDRenderWrapped
            | T.null mlDRender = ""
            | otherwise = "ModelLayer diff:\n" <> mlDRender <> "\n"
        mlDRender = renderLayerDiff (lR1, lR2) lD
renderDMModelDiff (FineDiff (((lN1, lR1), (lN2, lR2)), lD)) =
    "Layers " <> lN1 <> " and " <> lN2 <> ":\n" <>
    "ModelLayer diff:\n" <> renderLayerDiff (lR1, lR2) lD <> "\n"

renderDMMSMappingDiff :: DMMSMappingDiff -> T.Text
renderDMMSMappingDiff (SD (LD ldSwitchNames)
                      (RD rdSwitchNames)
                      (FC (SD (LD ldSwitchContent)
                              (RD rdSwitchContent)
                              _
                          )
                      )
                  ) = ldSN <> rdSN <> ldSC <> rdSC -- <> fcSC
    where
        ldSN | null ldSwitchNames = ""
             | otherwise = "Left unique Switch names:\n" <>
                T.intercalate "\n" (renderDMMSSwitch <$> ldSwitchNames) <> "\n"
        rdSN | null rdSwitchNames = ""
             | otherwise = "Right unique Switch names:\n" <>
                T.intercalate "\n" (renderDMMSSwitch <$> rdSwitchNames) <> "\n"
        ldSC | null ldSwitchContent = ""
             | otherwise = "Left shared Switch names but unique content:\n" <>
                 T.intercalate "\n" (renderDMMSSwitch <$> ldSwitchContent) <>
                    "\n"
        rdSC | null rdSwitchContent = ""
             | otherwise = "Right shared Switch names but unique content:\n" <>
                 T.intercalate "\n" (renderDMMSSwitch <$> rdSwitchContent) <>
                    "\n"
--         fcSC | null fcNameContent = ""
--              | otherwise = "Identical Switches:\n" <>
--                  T.intercalate "\n" (renderDMMSSwitch <$> fcNameContent) <>
--                     "\n"

renderSPDiff :: SwitchProfilesDiff -> T.Text
renderSPDiff (SD (LD lSProfiles) (RD rSProfiles) (FC switchDiffs)) =
    lSProfilesRender <> rSProfilesRender <> switchDiffsRender
    where
        lSProfilesRender
            | null lSProfiles = ""
            | otherwise = "Left unique SwitchNames:\n" <>
                T.intercalate "\n" (fst <$> lSProfiles) <> "\n"
        rSProfilesRender
            | null rSProfiles = ""
            | otherwise = "Right unique SwitchNames:\n" <>
                T.intercalate "\n" (fst <$> rSProfiles) <> "\n"
        switchDiffsRender
            | null switchDiffs = ""
            | T.null phenotypesRender = ""
            | otherwise = "Shared SwitchNames but differing Phenotypes:\n" <>
                phenotypesRender
                where
                    phenotypesRender = foldr renderSwitchDiff "" switchDiffs

renderSwitchDiff :: SwitchDiff -> T.Text -> T.Text
renderSwitchDiff (nN, (SD (LD phsL) (RD phsR) (FC pDiffs))) acc =
    phsLRender <> phsRRender <> pDiffsRender <> acc
    where
        phsLRender
            | null phsL = ""
            | otherwise = "Left unique Phenotype names for Switch " <>
                nN <> ":\n" <> T.intercalate "\n" (phenotypeName <$> phsL)
        phsRRender
            | null phsR = ""
            | otherwise = "Right unique Phenotype names for Switch " <>
                nN <> ":\n" <> T.intercalate "\n" (phenotypeName <$> phsR)
        pDiffsRender
            | null pDiffs = ""
            | T.null pDsRender = ""
            | otherwise = "Shared SwitchName(" <> nN <>
                "), and PhenotypeNames, but differing SwitchNodeStates or \
                \Fingerprint:\n" <> pDsRender
                where
                    pDsRender = foldr renderPDiff "" pDiffs

renderPDiff :: PDiff -> T.Text -> T.Text
renderPDiff (PDiff _ Nothing Nothing) acc = acc
renderPDiff (PDiff pDN (Just lSwitchSDiff) Nothing) acc =
    "Shared PhenotypeName(" <> pDN <> "), but differing SwitchNodeStates:" <>
        showt lSwitchSDiff <> "\n" <> acc
renderPDiff (PDiff pDN Nothing (Just _)) acc =
    "Shared PhenotypeName(" <> pDN <> "), but differing FingerPrints\n" <> acc
renderPDiff (PDiff pDN (Just lSwitchSDiff) (Just _)) acc =
    "Shared PhenotypeName(" <> pDN <>
        "), but differing SwitchNodeStates(" <> showt lSwitchSDiff <>
         ") and differing Fingerprints\n" <> acc

renderLayerDiff :: (LayerRange, LayerRange)
                -> SplitDiff [NodeName] [NodeDiff]
                -> T.Text
renderLayerDiff lRs (SD (LD lNNames) (RD rNNames) (FC nDiffs)) =
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
            | all T.null nDiffRenders = ""
            | otherwise = "Shared DMNode diffs:\n" <> T.concat nDiffRenders
                where
                    nDiffRenders = renderNodeDiff lRs <$> nDiffs

renderNodeDiff :: (LayerRange, LayerRange) -> NodeDiff -> T.Text
renderNodeDiff (lRangeL, lRangeR) (NodeDiff nN nTD lDs tD)
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
        Just (lType, rType) -> "\n" <> "NodeTypes vary: " <> showt lType <>
            " vs " <> showt rType <> "\n"
    rLinkDiffs = renderLinkDiff lDs
    tablesMaybe = case tD of
        Left gates -> "\n" <> "Gate Inlinks differ, so comparing the gate\
                \ outputs is nonsensical. " <>
            "The gates follow, if their GateOrigins were LogicalExpressions. "
            <> maybeLeftGate <> maybeRightGate
                where
                    maybeLeftGate
                        | (gateOrigin . fst) gates == DMMSTruthTable = ""
                        | otherwise = "\n" <> (renderGate' lRangeL (fst gates))
                    maybeRightGate
                        | (gateOrigin . snd) gates == DMMSTruthTable = ""
                        | otherwise = "\n" <> (renderGate' lRangeR (snd gates))
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
            | all nullLTEDiff lteDiffs = ""
            | otherwise = "Shared InLink diffs:\n" <>
                T.intercalate "\n" (renderLTEDiff <$> lteDiffs)

nullLTEDiff :: LTEDiff -> Bool
nullLTEDiff (LTEDiff _ Nothing Nothing) = True
nullLTEDiff _ = False

renderLTEDiff :: LTEDiff -> T.Text
renderLTEDiff (LTEDiff nN lT lE) = case (lT, lE) of
    (Nothing, Nothing) -> "Inlink " <> nN <> " perfectly shared."
    (Just t, Nothing) -> "Inlink " <> nN <> " has differing NodeTypes: " <>
        ((showt . fst) t) <> " vs " <> ((showt . fst) t)
    (Nothing, Just e) -> "Inlink " <> nN <> " has differing NodeEffects: " <>
        ((showt . fst) e) <> " vs " <> ((showt . fst) e)
    (Just t, Just e) -> "Inlink " <> nN <>
        " has differing NodeTypes: " <>
        ((showt . fst) t) <> " vs " <> ((showt . fst) t) <>
        "\n" <> "and NodeEffects: " <>
        ((showt . fst) e) <> " vs " <> ((showt . fst) e)

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
            (T.intercalate "\t" . fmap showt . U.toList) <$> vecs
        tOutputs = ((<>) "\t") <$> outs
        (vecs, outs) = unzip $ L.sortBy (compare `on` fst) $ zipdLR
        zipdLR = ((<>) "\t" . showt) <<$>> (zipWith zipper lList rList)
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
purgeTableRenderDMModel (Fine (ModelLayer mg mmd)) =
    dmmsWrap "Model" entries Nothing
    where
        entries = renderMMeta mmd <> "\n\n"
               <> purgeTableRenderMGraph mg
purgeTableRenderDMModel (LayerBinding mm (ModelLayer mg mmd) dmm) =
    dmmsWrap "Model" entries Nothing
        where
            entries = renderMMeta mmd <> "\n\n"
                   <> renderMMaping mm <> "\n\n"
                   <> purgeTableRenderMGraph mg  <> "\n\n"
                   <> purgeTableRenderDMModel dmm

purgeTableRenderMGraph :: ModelGraph -> T.Text
purgeTableRenderMGraph mg = dmmsWrap "ModelGraph" entries Nothing
    where
        entries = T.intercalate "\n\n" nodes
        nodes = purgeTableRenderDMMSNode ranges <$> dmmsNS
        ranges = Map.fromList $ (nodeRange . fst) <$> dmmsNS
        dmmsNS = dmmsNodes mg

purgeTableRenderDMMSNode :: LayerRange -> DMMSNode -> T.Text
purgeTableRenderDMMSNode r (n, ls) = dmmsWrap "Node" entries (Just nName)
    where
        entries = T.intercalate "\n\n" $ [meta, gate] <> links
        meta = renderNMeta $ nodeMeta n
        gate = purgeTableRenderGate r n
        links = renderNamedLink <$> ls
        nName = (nodeName . nodeMeta) n

purgeTableRenderGate :: LayerRange -> DMNode -> T.Text
purgeTableRenderGate lr n = dmmsWrap "NodeGate" entries Nothing
    where
        entries = case gateOrigin nGate of
            DMMSTruthTable -> renderTableG nGate
            _ -> renderDiscreteG lr nGate
        nGate = nodeGate n


----------------------------------------------------------------------------
-- Render DMExperiment output. Note the use of Data.Text.Lazy to try to clear
-- the result from memory as quickly as possible. 

renderDMExpOutput :: DMExpOutput -> TL.Text
renderDMExpOutput dmExpOP = layerGatesT <> "\n" <> lniBMapT <> "\n" <> expOPT
    where
        expOPT = (renderSingleExpOP mM lniBMap . dmExpOutput) dmExpOP
        layerGatesT = "Layer Gates: " <> TL.intercalate "\n" gateTs
        lRange = opLayerRange dmExpOP
        gateTs = renderDiscreteGL lRange <$> (layerGateSet dmExpOP)
        lniBMapT = "LayerNameIndexBimap: " <> TL.intercalate ", " lniBMapPairTs
        lniBMapPairTs = (fmap (TL.pack . show) . BM.toList) lniBMap
        lniBMap = layerNIBM dmExpOP
        mM = layerMM dmExpOP

renderDiscreteGL :: LayerRange -> NodeGate -> TL.Text
renderDiscreteGL r ng
    | length assigns == 1
                = "\t" <> nName <>" *= " <> (renderExprL boolNS $ last exprs)
    | otherwise = TL.intercalate "\n" rExprs
    where
        rExprs = ("\t" <>) <$> (zipWith ((<> ) . (<> " *= "))
                            gStates $ renderExprL boolNS <$> exprs)
        gStates = ((assignName <>) . showtl . fst) <$> assigns
        exprs = snd <$> assigns
--      We never write out the zeroth state in a dmms, so only take the tail
--      of the gate assignments. 
        assigns = (tail . gateAssigns) ng
        assignName = nName <> ":"
        nName = (TL.fromStrict . gNodeName) ng
        boolNS = (Map.keysSet . Map.filter (== 1)) r

renderExprL :: Set.HashSet NodeName -> NodeExpr -> TL.Text
renderExprL _ (GateLit b) = showtl b
renderExprL s (GateConst n st) = case Set.member n s of
    True -> case st of
        0 -> "not " <> tlN
        _ -> tlN
    False -> tlN <> ":" <> showtl st
    where
        tlN = TL.fromStrict n
renderExprL s (Not expr) = "not " <> renderExprL s expr
renderExprL s (Pars expr) = "(" <> renderExprL s expr <> ")"
renderExprL s (Binary And expr1 expr2) =
    (renderExprL s expr1) <> " and " <> (renderExprL s expr2)
renderExprL s (Binary Or expr1 expr2) =
    (renderExprL s expr1) <> " or " <> (renderExprL s expr2)

renderSingleExpOP :: ModelMapping -> LayerNameIndexBimap -> ExpOutput -> TL.Text
renderSingleExpOP mM lniBMap expOutPut = "Experiment: \n" <> vexExpT <>
    "\n" <> "ExperimentMark: " <> expMarkT <> "\n" <> "Output: \n" <> outputT
    where
        vexExpT = (renderVEXExperiment . opVexExp) expOutPut
        expMarkT = (showtl . expMark) expOutPut
        outputT = (renderExpOP mM lniBMap . expOP) expOutPut

renderVEXExperiment :: VEXExperiment -> TL.Text
renderVEXExperiment (VXTC vexTC) = renderTCVEXExperiment vexTC
renderVEXExperiment (TXSC vexScan) = renderScanVEXExperiment vexScan


-- Render TimeCourse experiments. 
renderTCVEXExperiment :: VEXTimeCourse -> TL.Text
renderTCVEXExperiment tcExp = case tcExp of
    GeneralTC exNm inEnv expStep vexPlss exReps fkds mPRMNGSeed ->
        vexWrap "GeneralExperiment" bLines $ Just $ TL.fromStrict exNm
        where
            bLines = [
                  "ExperimentName: " <> TL.fromStrict exNm
                , renderVexVar "SampleSize" showtl Nothing exReps
                , renderManualSeed mPRMNGSeed
                , renderExperimentStep expStep
                , renderInitialEnvironment inEnv
                , renderFigKinds fkds
                ] <> fmap renderVEXInputPulse vexPlss
    Pulse1 (t_0, t_end) inEnv dur (pName, pState) exReps fkds mPRMNGSeed ->
        vexWrap "Pulse1" bLines Nothing
        where
            bLines = [
                  renderUserDuration "t_0" t_0
                , renderInitialEnvironment inEnv
                , renderUserDuration "Duration" dur
                , "FlipTo: " <> TL.fromStrict pName <> ", " <> showtl pState
                , renderVexVar "SampleSize" showtl (Just 1) exReps
                , renderFigKinds fkds
                , renderManualSeed mPRMNGSeed
                , renderUserDuration "t_end" t_end
                ]
    KnockDOverE (t_0, t_end) inEnv dur nAlts exReps fkds mPRMNGSeed ->
        vexWrap "KDOE" bLines Nothing
        where
            bLines = [
                  renderUserDuration "t_0" t_0
                , renderInitialEnvironment inEnv
                , renderUserDuration "Duration" dur
                , renderNodeAlterations nAlts
                , renderVexVar "SampleSize" showtl (Just 1) exReps
                , renderFigKinds fkds
                , renderManualSeed mPRMNGSeed
                , renderUserDuration "t_end" t_end
                ]
    KDOEAtTransition (t_0, t_end) inEnv pDur (pN, pSt) nAlts exReps fkds
        mPRMNGSeed -> vexWrap "KDOEAtTransition" bLines Nothing
        where
            bLines =[
                  renderUserDuration "t_0" t_0
                , renderInitialEnvironment inEnv
                , renderUserDuration "Duration" pDur
                , "FlipTo: " <> TL.fromStrict pN <> ", " <> showtl pSt
                , renderVexVar "SampleSize" showtl (Just 1) exReps
                , renderNodeAlterations nAlts
                , renderFigKinds fkds
                , renderManualSeed mPRMNGSeed
                , renderUserDuration "t_end" t_end
                ]

-- Consume a vex keyword, the content for that keyword, and properly wrap the
-- second in the first. Optionally add a commented note at the end. 
vexWrap :: TL.Text -> [TL.Text] -> Maybe TL.Text -> TL.Text
vexWrap keyword bLines mComment =
    keyword <> "{\n" <> body <> "\n" <> keyword <> "}" <> commentT
    where
        body = (TL.intercalate "\n" . filter (not . TL.null)) bLines
        commentT = maybe "" (\cNote -> " // " <> cNote) mComment

-- Render a VEX file assignment that might have a default value that we
-- want rewritten. 
renderVexVar :: (Show a, Eq a)
              => TL.Text
              -> (a -> TL.Text)
              -> Maybe a
              -> a
              -> TL.Text
renderVexVar vName renderF mDefault vexV = case (vexV ==) <$> mDefault of
    Nothing -> vName <> ": " <> renderF vexV
    Just False -> vName <> ": " <> renderF vexV
    Just True -> ""

renderManualSeed :: ManualSeed -> TL.Text
renderManualSeed = maybe "" (\sd -> "ManualPRNGSeed: " <> showtl sd)

renderExperimentStep :: ExperimentStep -> TL.Text
renderExperimentStep = showtl

renderInitialEnvironment :: InitialEnvironment -> TL.Text
renderInitialEnvironment inEnv = vexWrap "StartingModelState" bLines Nothing
    where
        bLines = "InputCoordinate: " : ((fmap coordF . initCoord) inEnv)
            <> [(maybe "" renderBarcodeFilter . initFilters) inEnv]
        coordF (nName, nState) = TL.fromStrict nName <> ":" <> showtl nState

renderBarcodeFilter :: BarcodeFilter -> TL.Text
renderBarcodeFilter bcFilt = case bcFilt of 
    (OnlyBarCodesWithAny flts) -> "OnlyBarCodesWithAny: " <>
        (TL.intercalate ", " . fmap renderF) flts
    (OnlyBarCodesWithAll flts) -> "OnlyBarCodesWithAll: " <>
        (TL.intercalate ", " . fmap renderF) flts
    (ExcludeBarCodesWithAny flts) -> "ExcludeBarCodesWithAny: " <>
        (TL.intercalate ", " . fmap renderF) flts
    (ExcludeBarCodesWithAll flts) -> "ExcludeBarCodesWithAll: " <>
        (TL.intercalate ", " . fmap renderF) flts
    where
        renderF (nName, phName) = TL.fromStrict (nName <> ":" <> phName)


renderFigKinds :: FigKinds -> TL.Text
renderFigKinds fgKinds
    | L.null bLines = ""
    | otherwise = vexWrap "Figures" bLines Nothing
    where
        bLines = [nTCText, phTCText, nAvgNodeNames, phAvgSwitchNames]
        nTCText = (nodeTCF . nodeTimeCourse) fgKinds
        nodeTCF = renderVexVar "NodeTimeCourse" showtl (Just True)
        phTCText = (phTCF . phenotypeTimeCourse) fgKinds
        phTCF = renderVexVar "PhenotypeTimeCourse" showtl (Just False)
        nAvgNodeNames = (nodeAvgTCF . nodeAvgBars) fgKinds
        nodeAvgTCF = renderVexVar "AvgBarChartNodes" renderTList (Just [])
        phAvgSwitchNames = (phAvgTCF . phenotypeAvgBars) fgKinds
        phAvgTCF = renderVexVar "AvgBarChartSwitches" renderTList (Just [])

renderTList :: [T.Text] -> TL.Text
renderTList = TL.fromStrict . T.intercalate ", "

renderVEXInputPulse :: VEXInputPulse -> TL.Text
renderVEXInputPulse (VEXInPt vexRIC vexNodeAlts vexDur) =
    vexWrap "Pulse" [ipFixText, durText, nAltsWrapL] Nothing
    where
        ipFixText = renderVexVar "InputFix" renderPairsTL Nothing vexRIC
        durText = renderUserDuration "Duration" vexDur
        nAltsWrapL = renderNodeAlterations vexNodeAlts

renderPairsTL :: (TextShow a, TextShow b) => [(a, b)] -> TL.Text
renderPairsTL = TL.intercalate ", " . fmap pairTLF
    where pairTLF (x, y) = showtl x <> ":" <> showtl y


renderNodeAlterations :: [NodeAlteration] -> TL.Text
renderNodeAlterations [] = ""
renderNodeAlterations nAlts = vexWrap "NodeAlterations" nAltLines Nothing
    where
        nAltLines = fmap renderNAlts nAlts
        renderNAlts (NodeLock nName nState lockProb) = TL.fromStrict nName <>
            ": " <> showtl nState <> ", " <> showtl lockProb
        renderNAlts (GradientNudge nName NudgeUp nudgeProp) =
            TL.fromStrict nName <> ": Up, " <> showtl nudgeProp
        renderNAlts (GradientNudge nName NudgeDown nudgeProp) =
            TL.fromStrict nName <> ": Down, " <> showtl nudgeProp

-- When rendering a VEX file, we only ever want to write out a UserD Duration. 
renderUserDuration :: TL.Text -> Duration -> TL.Text
renderUserDuration _ (DefaultD _) = ""
renderUserDuration vexVarName (UserD i) = vexVarName <> ": " <> showtl i <> "\n"


-- Render Scans
renderScanVEXExperiment :: VEXScan -> TL.Text
renderScanVEXExperiment (VEXScan scKind inEnv mScNm nAlts iFix maxN relN stopPhs
    expStep (plottingSws, plottingNs)) = vexWrap "Scan" bLines Nothing
    where
        bLines = [
              renderInitialEnvironment inEnv
            , renderVexVar "Max_T" showtl Nothing maxN
            , renderVexVar "Relevant_T" showtl Nothing relN
            , maybe "" (renderVexVar "ScanName" showtl Nothing) mScNm
            , renderVexVar "InputFix" renderPairsTL (Just []) iFix
            , renderVexVar "StopPhenotypes" renderPairsTL (Just []) stopPhs
            , renderExperimentStep expStep
            , renderVexVar "ScanSwitches" renderTList (Just []) plottingSws
            , renderVexVar "ScanNodes" renderTList (Just []) plottingNs
            , renderScanKind scKind
            , renderNodeAlterations nAlts
            ]

renderScanKind :: ScanKind -> TL.Text
renderScanKind (EnvSc envScan) = renderEnvScan envScan
renderScanKind (KDOESc kdoeScan) = renderKDOEScan kdoeScan
renderScanKind (EnvKDOEScan envScan kdoeScan x_Axis) =
    vexWrap "EnvKDOEScan" bLines Nothing
    where
        bLines = [
              renderEnvScan envScan
            , renderKDOEScan kdoeScan
            , renderVexVar "X_Axis" showtl Nothing x_Axis
            ]
renderScanKind (TwoDEnvScan envScan1 envScan2 doOverLayVs mKDOeScan) =
    vexWrap "TwoDEnvScan" bLines Nothing
    where
        bLines = [
              renderEnvScan envScan1
            , renderEnvScan envScan2
            , renderVexVar "ValuesOnHeatMap" showtl (Just False) doOverLayVs
            , maybe "" renderKDOEScan mKDOeScan
            ]
renderScanKind (ThreeDEnvScan envScan1 envScan2 envScan3 doOverLayVs nAlts) =
    vexWrap "ThreeDEnvScan" bLines Nothing
    where
        bLines = [
              renderEnvScan envScan1
            , renderEnvScan envScan2
            , renderEnvScan envScan3
            , renderVexVar "ValuesOnHeatMap" showtl (Just False) doOverLayVs
            , renderNodeAlterations nAlts
            ]

renderEnvScan :: EnvScan -> TL.Text
renderEnvScan envScan = renderVexVar "EnvironmentalScan" envScF Nothing envScan
  where
    envScF (StepSpecESC nName stepLevels) =
      TL.fromStrict nName <> ": " <> showtl stepLevels
    envScF (RangeESC nName stState endState scSteps) = TL.fromStrict nName <>
      ":" <> showtl stState <> "," <> showtl endState <> "," <> showtl scSteps
    envScF (WholeESC nName scSteps) =
      TL.fromStrict nName <> ":" <> showtl scSteps


renderKDOEScan :: KDOEScan -> TL.Text
renderKDOEScan kdoeScan = renderVexVar "KDOEScan" kdoeScF Nothing kdoeScan
  where
    kdoeScF (StepSpecKDOESC kdoes lockProbs) =
      renderKDOEs kdoes <> ": " <> showtl lockProbs
    kdoeScF (RangeKDOESC kdoes stLockProb endLockProb scSteps) = 
      renderKDOEs kdoes <> ":" <> showtl stLockProb <> "," <> showtl endLockProb
      <> "," <> showtl scSteps
    kdoeScF (WholeKDOESC kdoes scSteps) =
      renderKDOEs kdoes <> ":" <> showtl scSteps
    renderKDOEs kds = "(" <> renderPairsTL kds <> ")"

-- Render experiment results
renderExpOP :: ModelMapping -> LayerNameIndexBimap -> ExpOP -> TL.Text
renderExpOP mM lniBMap (TCO ress) =
    (TL.intercalate "\n" . fmap (renderTCOutput mM lniBMap)) ress
renderExpOP mM lniBMap (SCO ress) = case ress of
    ScRe rawRess ->
        (TL.intercalate "\n" . fmap (renderRawScanResult mM lniBMap)) rawRess
    ScPr preppedRess ->
        (TL.intercalate "\n" . fmap renderScanResult) preppedRess
    

renderTCOutput :: ModelMapping
               -> LayerNameIndexBimap
               -> (Barcode, RepResults)
               -> TL.Text
renderTCOutput mM lniBMap (bc, rRess) = "Barcode, " <>
    (TL.intercalate ", " . fmap renderBar) bc <> "\n" <>
    "RepResults \n" <> renderRepResults mM lniBMap rRess

renderBar :: Bar -> TL.Text
renderBar br = TL.intercalate ", " [bKindT, aSizeT, sNameT, phNamesT]
    where
        bKindT = "BarKind:" <> (renderBarKind . barKind) br
        aSizeT = "AttSize:" <> (showtl . attractorSize) br
        sNameT = "SwitchName:" <> (TL.fromStrict . switchName) br
        phNamesT = "PhenotypeNames:" <> TL.intercalate "-" phNamesTs
        phNamesTs = (fmap TL.fromStrict . phenotypeNames) br

renderBarKind :: BarKind -> TL.Text
renderBarKind (FullMiss bHeight) = "FullMiss " <> showtl bHeight
renderBarKind (MatchBar slices lColor) =
    "MatchBar " <> slicesT <> " " <> lColorT
    where
        slicesT = TL.intercalate "-" $ showtl <$> slices
        lColorT = (TL.pack . SC.sRGB24show) lColor

-- Render bare results 
renderRepResults :: ModelMapping -> LayerNameIndexBimap -> RepResults -> TL.Text
renderRepResults mM lniBMap (timelinesss, _) = inCF3 timelineTsss
  where
    timelineTsss = (fmap . fmap . fmap) (renderTimeline mM lniBMap) timelinesss

renderTimeline :: ModelMapping -> LayerNameIndexBimap -> Timeline -> TL.Text
renderTimeline mM lniBMap tmln = nodeT <> "\n" <> phenotypeT
    where
        phenotypeT = TL.intercalate "\n" $ renderPhPrevalence <$> phList
        phList = concatMap snd switchList
        nodeT = TL.intercalate "\n" $ renderNodeHistory <$> nodeList
        (nodeList, switchList) = tpTimeline mM lniBMap tmln

renderNodeHistory :: NodeHistory -> TL.Text
renderNodeHistory (nName, tmln) = TL.fromStrict nName <> ", " <> tmlnT
    where
        tmlnT = TL.intercalate ", " $ tmlnF <$> tmln
        tmlnF (nSt, wFcd)
            | wFcd = showtl nSt <> ":T"
            | otherwise = showtl nSt <> ":F"

renderPhPrevalence :: PhenotypePrevalence -> TL.Text
renderPhPrevalence (phName, phPrevs) = TL.fromStrict phName <> ", " <> phPrevT
    where
        phPrevT = TL.intercalate ", " $ phPrevF <$> phPrevs
        phPrevF True = "T"
        phPrevF False = "F"

-- Timelines are vectors of whole network states. Often, we need vectors of
-- DMNode time sequences with accompanying Phenotype prevalences. 
tpTimeline :: ModelMapping -> LayerNameIndexBimap -> Timeline -> TPTimeLine
tpTimeline mM lniBMap tmln = (namedNodeFirstLists, phPrevalences)
    where
        phPrevalences = (fmap . fmap . fmap) annotatorF nonEmptySwPhNs
        annotatorF phName = (phName,(B.toList . B.map (elem phName)) presPhsVec)
        nonEmptySwPhNs :: [(SwitchName, [PhenotypeName])]
        nonEmptySwPhNs = (fmap . fmap . fmap) phenotypeName nonEmptySwPhs
        nonEmptySwPhs = snd <<$>> (nonEmptyPhenotypes mM)
        namedNodeFirstLists :: [(NodeName, [(NodeState, WasForced)])]
        namedNodeFirstLists = zipWith namerF [0..] nodeFirstLists
        namerF i x = (lniBMap BM.!> i, x)
        nodeFirstLists = L.transpose annoLists
        annoLists :: [[(NodeState, WasForced)]]
        annoLists = (B.toList . B.map U.toList) anolVecs
        (anolVecs, presPhsVec) = B.unzip tmln


-- Vectors of DMNode time sequences with accompanying Phenotype prevalences. 
type TPTimeLine = ([NodeHistory], [(SwitchName, [PhenotypePrevalence])])
type NodeHistory = (NodeName, [(NodeState, WasForced)])
type PhenotypePrevalence = (PhenotypeName, [WasPresent])

type WasPresent = Bool

-- Render the results of a DMScan, after being processed down to stop phenotype
-- distributions, phenotype prevelances, and node averages. 
renderScanResult :: (Barcode, ScanPrep) -> TL.Text
renderScanResult (bc, scPrep) = preface <> body
  where
    preface = "Barcode, " <> (TL.intercalate ", " . fmap renderBar) bc <> "\n"
      <> "ScanResults\n"
    body = case scPrep of
      SPREnv (stopDs, phDists, nodeStats) -> stopDsPreface <> "\n" <> stopDsBody
        <> "\n" <> phDistsPreface <> "/n" <> phDistsBody <> "/n" <>
        nodeStatsPreface <> "/n" <> nodeStatsBody
        where
          stopDsBody = inCF $ renderStopDistribution <$> stopDs
          phDistsBody = inCF $ renderPhDistribution <$> phDists
          nodeStatsBody = inCF $ renderScanNodeStats <$> nodeStats
      SPRKDOE (stopDs, phDists, nodeStats) -> stopDsPreface <> "\n" <>
        stopDsBody <> "\n" <> phDistsPreface <> "/n" <> phDistsBody <> "/n" <>
        nodeStatsPreface <> "/n" <> nodeStatsBody
        where
          stopDsBody = inCF $ renderStopDistribution <$> stopDs
          phDistsBody = inCF $ renderPhDistribution <$> phDists
          nodeStatsBody = inCF $ renderScanNodeStats <$> nodeStats
      SPREnvKDOE scanStatss -> stopDsPreface <> "\n" <> stopDssBody <> "\n" <>
        phDistsPreface <> "/n" <> phDistssBody <> "/n" <> nodeStatsPreface <>
        "/n" <> nodeStatssBody
        where
          stopDssBody = inCF2 $ renderStopDistribution <<$>> stopDss
          phDistssBody = inCF2 $ renderPhDistribution <<$>> phDistss
          nodeStatssBody = inCF2 $ renderScanNodeStats <<$>> nodeStatss
          (stopDss, phDistss, nodeStatss) = unzip3 scanStatss
      SPRTwoEnvWithoutKDOE scanStatss -> stopDsPreface <> "\n" <> stopDssBody <>
        "\n" <> phDistsPreface <> "/n" <> phDistssBody <> "/n" <>
        nodeStatsPreface <> "/n" <> nodeStatssBody
        where
          stopDssBody = inCF2 $ renderStopDistribution <<$>> stopDss
          phDistssBody = inCF2 $ renderPhDistribution <<$>> phDistss
          nodeStatssBody = inCF2 $ renderScanNodeStats <<$>> nodeStatss
          (stopDss, phDistss, nodeStatss) = unzip3 scanStatss
      SPRTwoEnvWithKDOE scanStatsss -> stopDsPreface <> "\n" <> stopDsssBody <>
        "\n" <> phDistsPreface <> "/n" <> phDistsssBody <> "/n" <>
        nodeStatsPreface <> "/n" <> nodeStatsssBody
        where
          stopDsssBody = inCF3Fmap renderStopDistribution stopDsss
          phDistsssBody = inCF3Fmap renderPhDistribution phDistsss
          nodeStatsssBody = inCF3Fmap renderScanNodeStats nodeStatsss
          (stopDsss, phDistsss, nodeStatsss) =(unzip3 . fmap unzip3) scanStatsss
      SPRThreeEnv scanStatsss mScanStatsss -> "Wildtype stats:\n" <>
        wildTypeBody <> "\nMutant stats:\n" <> mutantBody
        where
          wildTypeBody = stopDsPreface <> "\n" <> stopDsssBody <> "\n" <>
            phDistsPreface <> "/n" <> phDistsssBody <> "/n" <> nodeStatsPreface
            <> "/n" <> nodeStatsssBody
          stopDsssBody = inCF3Fmap renderStopDistribution stopDsss
          phDistsssBody = inCF3Fmap renderPhDistribution phDistsss
          nodeStatsssBody = inCF3Fmap renderScanNodeStats nodeStatsss
          (stopDsss, phDistsss, nodeStatsss) =(unzip3 . fmap unzip3) scanStatsss
          mutantBody = case mScanStatsss of 
            Nothing -> ""
            Just mutantScanStatsss -> stopDsPreface <> "\n"
              <> mStopDsssBody <> "\n" <> phDistsPreface <> "/n" <>
              mPhDistsssBody <> "/n" <> nodeStatsPreface <> "/n" <>
              mNodeStatsssBody
              where
                mStopDsssBody = inCF3Fmap renderStopDistribution mStopDsss
                mPhDistsssBody = inCF3Fmap renderPhDistribution mPhDistsss
                mNodeStatsssBody = inCF3Fmap renderScanNodeStats mNodeStatsss
                (mStopDsss, mPhDistsss, mNodeStatsss) =
                  (unzip3 . fmap unzip3) mutantScanStatsss
    stopDsPreface = "Stop Phenotype Occurrence Stats: "
    phDistsPreface = "Phenotype Prevalence Stats: "
    nodeStatsPreface = "DMNode State Stats: "
    inCF3Fmap x y = (inCF3 . (fmap . fmap . fmap) x) y

-- Useful recursive intercalation functions:
inCF :: [TL.Text] -> TL.Text
inCF = TL.intercalate "\n"

inCF2 :: [[TL.Text]] -> TL.Text
inCF2 = inCF . fmap inCF

inCF3 :: [[[TL.Text]]] -> TL.Text
inCF3 = inCF2 . (fmap . fmap) inCF

inCF4 :: [[[[TL.Text]]]] -> TL.Text
inCF4 = inCF3 . (fmap . fmap . fmap) inCF
 
renderStopDistribution :: StopDistribution -> TL.Text
renderStopDistribution (stopMap, noStopFrac) =
    "Fraction of runs not Phenotype stopped :" <> showtl noStopFrac <> "\n" <>
    "Run stop fractions by Phenotype:\n" <> mapTL
    where
        mapTL = (TL.intercalate ", " . fmap pairF . Map.toList) stopMap
        pairF (phN, phFrac) = showtl phN <> ":" <> showtl phFrac

renderPhDistribution :: PhDistribution -> TL.Text
renderPhDistribution phDistMap = "Run Phenotype prevalence:\n" <> mapTL
    where
        mapTL = (TL.intercalate ", " . fmap pairF . Map.toList) phDistMap
        pairF (phN, phFrac) = showtl phN <> ":" <> showtl phFrac

renderScanNodeStats :: ScanNodeStats -> TL.Text
renderScanNodeStats nodeStatMap =
    "Run DMNode average and standard deviations:\n" <> mapTL
    where
        mapTL = (TL.intercalate ", " . fmap pairF . Map.toList) nodeStatMap
        pairF (nName, (nAvg, nStdDev)) = showtl nName <> ": (" <> showtl nAvg <>
            ", " <> showtl nStdDev <> ")"

-- Render the unprocessed recults of a DMScan. 
renderRawScanResult :: ModelMapping
                    -> LayerNameIndexBimap
                    -> (Barcode, ScanResult)
                    -> TL.Text
renderRawScanResult mM lniBMap (bc, scRes) = preface <> body
    where
        preface = "Barcode, " <> (TL.intercalate ", " . fmap renderBar) bc <>
            "\n" <> "ScanResults\n"
        body = case scRes of
            (SKREnv tmlnss) -> inCF2 $ renderTimeline mM lniBMap <<$>> tmlnss
            (SKRKDOE tmlnss) -> inCF2 $ renderTimeline mM lniBMap <<$>> tmlnss
            (SKREnvKDOE tmlnsss) -> inCF3 $ (fmap . fmap . fmap)
                 (renderTimeline mM lniBMap) tmlnsss
            (SKRTwoEnvWithoutKDOE tmlnsss) -> inCF3 $ (fmap . fmap . fmap)
                (renderTimeline mM lniBMap) tmlnsss
            (SKRTwoEnvWithKDOE tmlnssss) -> inCF4 $ (fmap . fmap . fmap . fmap)
                (renderTimeline mM lniBMap) tmlnssss
            (SKRThreeEnv (tmlnssss, maybeTmlnssss)) -> case maybeTmlnssss of
                Nothing -> inCF4 $ (fmap . fmap . fmap . fmap)
                    (renderTimeline mM lniBMap) tmlnssss
                Just mutTmlnssss -> wildTypeF <> "/n" <> mutantTypeF
                    where
                        wildTypeF = "Wild type:\n" <>
                            (inCF4 . (fmap . fmap . fmap . fmap)
                            (renderTimeline mM lniBMap)) tmlnssss
                        mutantTypeF = "Mutants:\n" <>
                            (inCF4 . (fmap . fmap . fmap . fmap)
                            (renderTimeline mM lniBMap)) mutTmlnssss


-- Render out the data used to make node barcharts.
renderNBCData :: [[[(NodeName, U.Vector RealNodeState)]]] -> TL.Text
renderNBCData pairedVecsss = (TL.intercalate "\n" . fmap fDataF1) indexedPVecsss
  where
    indexer = showtl <$> [(0 :: Int)..]
    indexedPVecsss = zip indexer pairedVecsss
    fDataF1 (j, pairedVecss) = "Chart " <> j <> ":\n" <> body1
      where
        body1 = (TL.intercalate "\n" . fmap fDataF2) indexedPVecss
        indexedPVecss = zip indexer pairedVecss
        fDataF2 (k, pairedVecs) = "Pulse " <> k <> ":\n" <> body2
          where
            body2 = (TL.intercalate "\n" . fmap fDataF3) pairedVecs
            fDataF3 (nN, nVec) = (TL.fromStrict nN) <> ", " <>
              (TL.intercalate ", " . fmap showtl . U.toList) nVec

renderPhBCData :: [[(SwitchName, [[(PhenotypeName, U.Vector Double)]])]]
               -> TL.Text
renderPhBCData pairss = (TL.intercalate "\n" . fmap fDataF1) indexedPairss
  where
    indexer = showtl <$> [(0 :: Int)..]
    indexedPairss = zip indexer pairss
    fDataF1 (j, pairs) = "Chart " <> j <> ":\n" <> body1
      where
        body1 = (TL.intercalate "\n" . fmap fDataF2) indexedByPulsePairs
        indexedByPulsePairs = zip indexer (byPulseData pairs)
        fDataF2 (k, phD) = "Pulse " <> k <> ":\n" <> body2
          where
            body2 = (TL.intercalate "\n" . fmap fDataF3) phD
            fDataF3 (swN, phs) = (TL.fromStrict swN) <> ":\n" <> body3
              where
                body3 = (TL.intercalate "\n" . fmap fDataF4) phs
                fDataF4 (phN, avgs) = (TL.fromStrict phN) <> ", " <>
                  (TL.intercalate ", " ((fmap showtl . U.toList) avgs))

-- Rearrange Phenotype Barchart data so that it is ordered first by Pulse, 
-- rather than Switch. 
byPulseData :: [(NodeName, [[(PhenotypeName, U.Vector Double)]])]
            -> [[(NodeName, [(PhenotypeName, U.Vector Double)])]]
byPulseData pairs = zip swNames <$> phData
    where (swNames, phData) = (fmap L.transpose . unzip) pairs


