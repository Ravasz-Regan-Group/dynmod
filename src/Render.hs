{-# LANGUAGE OverloadedStrings #-}

module Render 
    ( renderGML
    , renderDMMS
    , renderDMMSDiff
    , renderDMMSSwitch
    , purgeTableRenderDMMS
    )
    where

import Types.GML
import Types.DMModel
import Types.DMInvestigation
import Types.Figures
import Types.Simulation
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
            boShow (WholeNode n) = "WholeNode: " <> n
            boShow (SpecificState n i) = "SpecificState: " <>
                n <> " " <> showt i

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
    ((T.pack . show . switchNodeState) ph) <>
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
        gStates = ((assignName <>) . T.pack . show . fst) <$> assigns
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
        tOutputs = ((<>) "\t" . T.pack . show) <$> outputs
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
        rEffect = "LinkEffect: " <> ((T.pack . show . linkEffect) dmL)
        rType = "LinkType: " <> ((T.pack . show . linkType) dmL)
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
            (T.intercalate "\t" . fmap showt . U.toList) <$> vecs
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
        expOPT = (renderSingleExpOP lniBMap . dmExpOutput) dmExpOP
        layerGatesT = "Layer Gates: " <> TL.intercalate ", " gateTs
        gateTs = renderLGate <$> (layerGateSet dmExpOP)
        lniBMapT = "LayerNameIndexBimap: " <> TL.intercalate ", " lniBMapPairTs
        lniBMapPairTs = (fmap (TL.pack . show) . BM.toList) lniBMap
        lniBMap = layerNIBM dmExpOP


renderLGate :: NodeGate -> TL.Text
renderLGate nGate = TL.intercalate ", " (tlNName:ttPairTs)
    where
        tlNName = (TL.fromStrict . gNodeName) nGate
        ttPairTs = ttPairF <$> tablePairs
        ttPairF (inputV, opState) = showtl inputV <> ":" <> showtl opState
        tablePairs = (Map.toList . gateTable) nGate

renderSingleExpOP :: LayerNameIndexBimap -> ExpOutput -> TL.Text
renderSingleExpOP lniBMap (TCO expOP) = "Experiment Parameters \n" <> paramsT <>
    "\n" <> "Experiment Output: \n" <> outputT
    where
        paramsT = renderTCParams lniBMap tcParams
        outputT = (TL.intercalate "\n" . fmap renderTCOutput) tcOP
        tcParams = tcOutputParams expOP
        tcOP = tcOutput expOP
renderSingleExpOP _ (SCO expOp) = "Experiment Parameters \n" <> paramsT <>
    "\n" <> "Experiment Output: \n" <> outputT
    where
        paramsT = renderSCParams scParams
        outputT = (TL.intercalate "\n" . fmap renderSCOutput) scOP
        scParams = scOutputParams expOp
        scOP = scOutput expOp

renderTCParams :: LayerNameIndexBimap -> TCOutputParameters -> TL.Text
renderTCParams lniBMap tcParams = (TL.intercalate "\n" . filter (not . TL.null))
    [nameT, detailsT, repsT, kindT, stepperT, seedT, pulseSpacingT]
    where
        pulseSpacingT = "PulseSpacings: \n" <> formatF pulseSpacingTss
        formatF = (TL.intercalate "\n" . fmap (TL.intercalate ", "))
        pulseSpacingTss = renderPulseSpacing lniBMap <<$>>
            (tcExpOPPulseSps tcParams)
        seedT = maybe "" (\x -> "Experimental Seed, " <> showtl x) mSeed
            where mSeed = tcExpOPMPRNGSeed tcParams
        stepperT = "Experiment Stepper, " <> (showtl . tcExpOPStepper) tcParams
        kindT = "Experiment Kind, " <> (showtl . tcExpOPKind) expMeta
        repsT = "Repetitions, " <> (showtl . tcExpOPReps) expMeta
        detailsT = "Details, " <> tcExpOPDetails expMeta
        nameT = "Name, " <> tcExpOPName expMeta
        expMeta = tcExpOPMeta tcParams

renderPulseSpacing :: LayerNameIndexBimap -> PulseSpacing -> TL.Text
renderPulseSpacing lniBMap (dur, ricVec, nAlts) = durT <> inputT <> nAltsT
    where
        durT = "Duration, " <> showtl durT <> "\n"
        inputT = "RealInputCoord, " <> renderInputCoord lniBMap ricVec <> "\n"
        nAltsT = "NodeAlterations, " <> TL.intercalate ", " nAltTs
        nAltTs = (TL.fromStrict . nAltTPrep) <$> nAlts

renderInputCoord :: LayerNameIndexBimap -> RealInputCoord -> TL.Text
renderInputCoord lniBMap = TL.intercalate ", " . fmap riF . U.toList
    where riF (nI, nS) = showtl nI <> ":" <> showtl nS

renderTCOutput :: (Barcode, RepResults) -> TL.Text
renderTCOutput (bc, rRess) = "Barcode, " <>
    (TL.intercalate ", " . fmap renderBar) bc <> "\n" <>
    "RepResults \n" <> renderRepResults rRess

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

-- We are not rendering the 
renderRepResults :: RepResults -> TL.Text
renderRepResults (timelinesss, _) = timelineF timelineTsss
    where
        timelineF = inCF . fmap inCF . (fmap . fmap) inCF
        inCF = TL.intercalate "\n"
        timelineTsss = (fmap . fmap . fmap) renderTimeline timelinesss

renderTimeline :: Timeline -> TL.Text
renderTimeline = undefined

renderSCParams :: SCOutputParameters -> TL.Text
renderSCParams = undefined

renderSCOutput :: (Barcode, ScanResult) -> TL.Text
renderSCOutput (bc, scRess) = "Barcode, " <>
    (TL.intercalate ", " . fmap renderBar) bc <> "\n" <> "ScanResults \n" <>
    renderScanResult scRess

renderScanResult :: ScanResult -> TL.Text
renderScanResult = undefined

