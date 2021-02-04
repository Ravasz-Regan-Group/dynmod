{-# LANGUAGE OverloadedStrings #-}

module Render 
    ( renderGML
    , renderDMMS
    )
    where

import Types.GML
import Types.DMModel
import Constants
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Versions as Ver
import qualified Data.Colour.SRGB as SC
import qualified Data.Vector.Unboxed as U
import Data.Maybe (fromJust)
import qualified Data.List as L

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
        entries = renderDiscreteG lr nGate <> "\n" <> renderTableG combos nGate
        nGate = nodeGate n
        combos = fromJust $ nodeCombinations lr n

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
        boolNS = Map.keysSet $ Map.filter (\v -> length v == 2) r

renderExpr :: Set.HashSet NodeName -> NodeExpr -> T.Text
renderExpr _ (GateLit b) = (T.pack . show) b
renderExpr s (GateConst n st)
    | Set.member n s = n
    | otherwise      = n <> ":" <> (T.pack . show) st
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

-- Rendering a table requires knowing the full range of all the nodes referenced
-- in all the NodeExprs of all the NodeStateAssigns, so those must be passed in
-- also. 
renderTableG :: [ExprInput] -> NodeGate -> T.Text
renderTableG cs ng = dmmsWrap "TruthTable" entries
    where
        entries = topLine <> "\n" <> rows
        topLine = T.intercalate "\t" $ gateOrder ng <> [gNodeName ng]
        rows = T.intercalate "\n" $ L.sort $ prettyGateEval order assigns <$> cs
        assigns = gateAssigns ng
        order = gateOrder ng

renderNamedLink :: (NodeName, DMLink) -> T.Text
renderNamedLink (inName, dmL) = dmmsWrap "InLink" entries <> " //" <> inName
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