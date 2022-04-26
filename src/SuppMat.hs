{-# LANGUAGE OverloadedStrings #-}

module SuppMat
    ( sM_LaTeX
    , mkBibFile
    , mkBooleanNet
    , BooleanNet
    ) 
    where

import Constants
import Types.DMModel
import Utilities
import Text.LaTeX.DynMod.Extra
import Text.LaTeX.Packages.Booktabs
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Texy
import Text.LaTeX.Base.Types
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.Fontenc
import Text.LaTeX.Packages.LongTable
import Text.LaTeX.Packages.Color
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Babel
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Graph.Inductive as Gr
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Text.Pretty.Simple as PS
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Foldable (fold)
import Control.Monad.Reader (Reader, runReader, ask)

displayStyle :: LaTeXC l => l
displayStyle = commS "displaystyle"

subColSep :: LaTeXC l => l
subColSep = fromLaTeX $ textwidth' <> "-2" <> tabcolsep

colDim :: LaTeXC l => Float -> l
colDim x = dimexpr <> " " <> texy x <> subColSep

-- TabelSpecs for the node rows
node1ASpec, node1BSpec :: LaTeXC l => l
node1ASpec = fromLaTeX $
    newCommand' "nodeoneaspec" 0 TeXEmpty (colDim 0.14)
node1BSpec = fromLaTeX $
    newCommand' "nodeonebspec" 0 TeXEmpty (colDim 0.86)

node1ASpecCall, node1BSpecCall :: LaTeXC l => l
node1ASpecCall = commS "nodeoneaspec"
node1BSpecCall = commS "nodeonebspec"

node2ASpec, node2BSpec, node2CSpec :: LaTeXC l => l
node2ASpec = fromLaTeX $
    newCommand' "nodetwoaspec" 0 TeXEmpty (colDim 0.14)
node2BSpec = fromLaTeX $
    newCommand' "nodetwobspec" 0 TeXEmpty (colDim 0.13)
node2CSpec = fromLaTeX $
    newCommand' "nodetwocspec" 0 TeXEmpty (colDim 0.73)

node2ASpecCall, node2BSpecCall, node2CSpecCall :: LaTeXC l => l
node2ASpecCall = commS "nodetwoaspec"
node2BSpecCall = commS "nodetwobspec"
node2CSpecCall = commS "nodetwocspec"
    
-- TabelSpecs for the link rows
linkASpec, linkBSpec, linkCSpec, linkDSpec :: LaTeXC l => l
linkASpec = fromLaTeX $
    newCommand' "linkaspec" 0 TeXEmpty (colDim 0.14)
linkBSpec = fromLaTeX $
    newCommand' "linkbspec" 0 TeXEmpty (colDim 0.13)
linkCSpec = fromLaTeX $
    newCommand' "linkcspec" 0 TeXEmpty (colDim 0.14)
linkDSpec = fromLaTeX $
    newCommand' "linkdspec" 0 TeXEmpty (colDim 0.59)

linkASpecCall, linkDSpecCall, linkCSpecCall, linkBSpecCall :: LaTeXC l => l
linkASpecCall = commS "linkaspec"
linkBSpecCall = commS "linkbspec"
linkCSpecCall = commS "linkcspec"
linkDSpecCall = commS "linkdspec"

-- Substitutes "\allowbreak\_{}" for "\_{}". Applied to places where a NodeName
-- is displayed, to get around long NodeNames while keeping lots of space for
-- descriptions in other columns. 
uScoreSub :: (LaTeXC l) => T.Text -> l
uScoreSub = fromLaTeX . TeXRaw . T.replace "_" "\\allowbreak\\_{}"

-- Half the separation between columns. 
tabcolsep :: LaTeXC l => l
tabcolsep = commS "tabcolsep"

preamble :: LaTeX
preamble = 
     documentclass [] article <> "\n"
  <> declareFontFamily "U" "FdSymbolC" "" <> "\n"
  <> declareFontShape "U" "FdSymbolC" "m" "n" "<-> s * FdSymbolC-Book" ""
    <> "\n"
  <> declareSymbolFont "fdarrows" "U" "FdSymbolC" "m" "n" <> "\n"
  <> declareMathSymbol (commS "leftfootline") (commS "mathrel")
        "fdarrows" "\"AC" <> "\n"
  <> declareMathSymbol (commS "longleftfootline") (commS "mathrel")
        "fdarrows" "\"C6" <> "\n"
  <> declareMathSymbol (commS "leftblackspoon") (commS "mathrel")
        "fdarrows" "\"6E" <> "\n"
  <> usepackage [utf8] inputenc <> "\n"
  <> useencoding [T1] <> "\n"
--   Insert your language here if you are recompiling this to work mainly in
--   a non-English context. Extended latin characters are supported regardless.
  <> usepackage ["english"] babel <>"\n"
  <> usepackage [] amsmath <> "\n"
  <> newCommand "andop" 0 TeXEmpty (mathbin $ mathrm "and") <> "\n"
  <> newCommand "orop" 0 TeXEmpty (mathbin $ mathrm "or") <> "\n"
  <> newCommand "notop" 0 TeXEmpty (mathop $ mathrm "not") <> "\n"
  <> usepackage [] longtablep <> "\n"
  <> setCounter "LTchunksize" 200 <> "\n"
  <> comm2 "setlength" (commS "LTcapwidth")
    ((texy (1.15 :: Float)) <> commS "textwidth") <> "\n"
  <> usepackage ["font=bf", "labelfont=bf"] captionp <> "\n"
  <> usepackage [] subfloatp <> "\n"
  <> usepackage [] booktabs <> "\n"
  <> usepackage [] makecellp <> "\n"
  <> usepackage [] microtypep <> "\n"
  <> usepackage [dvipsnames] pxcolor <> "\n"
  <> usepackage ["margin=1.0in"] geometry <> "\n"
  <> usepackage ["raggedright"] titlesecp <> "\n"
--   <> usepackage [] showframep <> "\n"
  <> renewCommand' "thepage" 0 TeXEmpty (TeXRaw "S" <>
    comm1 "arabic" "page")
  <> "\n"
  <> renewCommand' "thesection" 0 TeXEmpty (TeXRaw "S" <>
    comm1 "arabic" "section")
  <> "\n"
  <> renewCommand' "thetable" 0 TeXEmpty (TeXRaw "S" <>
    comm1 "arabic" "table")
  <> "\n"
  <> renewCommand' "thefigure" 0 TeXEmpty (TeXRaw "S" <>
    comm1 "arabic" "figure")
  <> "\n"
  <> renewCommand' "figurename" 0 TeXEmpty (TeXRaw
    "Supplemental Material, Figure")
  <> "\n"
  <> node1ASpec <> "\n"
  <> node1BSpec <> "\n"
  <> node2ASpec <> "\n"
  <> node2BSpec <> "\n"
  <> node2CSpec <> "\n"
  <> linkASpec <> "\n"
  <> linkBSpec <> "\n"
  <> linkCSpec <> "\n"
  <> linkDSpec <> "\n"

sM_LaTeX :: Reader DMModel LaTeX
sM_LaTeX = do 
    body' <- body
    return $ preamble <> (document body')


theTSpec :: [TableSpec]
theTSpec = [ Separator TeXEmpty
           , LeftColumn
           , LeftColumn
           , LeftColumn
           , LeftColumn
           , Separator TeXEmpty
           ]

sMTableUnits :: LaTeX
sMTableUnits = 
  (multicolumn 1 [ParColumnTop node1ASpecCall] "Target Node")
    & multicolumn 3 [ParColumnMid node1BSpecCall] "Node Gate" <> lnbk <> "\n" <>
  (multicolumn 1 [ParColumnMid node2ASpecCall] TeXEmpty)
    & multicolumn 1 [ParColumnMid node2BSpecCall] "Node Type"
    & multicolumn 2 [ParColumnMid node2CSpecCall] "Node Description"
    <> lnbk <> "\n" <>
  (multicolumn 1 [ParColumnMid linkASpecCall] TeXEmpty)
    & multicolumn 1 [ParColumnMid linkBSpecCall] "Link Type"
    & (multicolumn 1 [ParColumnMid linkCSpecCall] "Input Node")
    & (multicolumn 1 [ParColumnMid linkDSpecCall] "Link Description")
      <> lnbk <> "\n"

body :: Reader DMModel LaTeX
body = do
    tables <- mkSMSections
    let biblio = comm1 "bibliographystyle" "unsrt" <> "\n"
              <> comm1 "bibliography" "references.bib"
    return $ "\n" <> tables <> "\n" <> legend <> "\n" <> biblio <> "\n\n"

mkSMSections :: Reader DMModel LaTeX
mkSMSections = do
  dmM   <- ask
  info  <- mappingGrouper
  gmap <- modelLaTeXGate
  return $ case dmM of
    (Fine _) ->
      longtable (Just Center) theTSpec (
        caption "Regulatory logic representing the model." <> "\n" <>
        endhead <> lnbk <> "\n" <>
        toprule Nothing <> "\n" <>
        sMTableUnits    <> "\n" <>
        midrule Nothing <> "\n" <>
        fold ((mkSMSection' . fst) <$> insAdjs) <>
        bottomrule Nothing <> "\n"
      )
        where
          (_, insAdjs) = head info
          mkSMSection' (n, ls) =
            smNodePrep gmap n <> lnbk <> "\n"
                <> (addLineSpace $ Just $ CustomMeasure $ (dimexpr <> (texy
                (1.0 :: Float))) <> defaultAddSpace) <> "\n"
                <> (inLinkConcat (smLinkPrep <$> ls))
    (LayerBinding _ _ _) ->
      foldr (grouper gmap) TeXEmpty info
        where
          grouper gm (mName, adjPList) y =
            (section ("Description " <> commS "&" <> " experimental support for\
              \ the modules of " <> (texy mName) <> ". \n")) <> "\n" <>
            (subtables (mkSMSubtables gm adjPList)) <> "\n" <> y

mkSMSubtables :: Map.HashMap NodeName LaTeX -> [(InAdj, [InAdj])] -> LaTeX
mkSMSubtables gmap = foldr (grouper gmap) TeXEmpty where
  grouper gmp (inAdj, insAdjs) y = 
   longtable (Just Center) theTSpec
    (
      "\n" <>
      (caption (((texy . nodeName . nodeMeta . fst) inAdj) <>
         (texy (" module" :: T.Text))))
         <> "\n" <> endhead <> lnbk <> "\n" <>
      toprule Nothing <> "\n" <>
      sMTableUnits    <> "\n" <>
      midrule Nothing <> "\n" <>
      fold (mkSMSubtables' gmp <$> insAdjs) <>
      bottomrule Nothing <> "\n"
    ) <> "\n" <> y
      where
        mkSMSubtables' gm (n, ls) =
            smNodePrep gm n <> lnbk <> "\n"
                <> (addLineSpace $ Just $ CustomMeasure $ (dimexpr <> (texy
                (1.0 :: Float))) <> defaultAddSpace) <> "\n"
                <> (inLinkConcat (smLinkPrep <$> ls)) <> lnbk <> "\n"

-- Prep the LaTeX depicting the NodeGates of a DMModel. This is done all at
-- once, because the depiction of a GateConst NodeName NodeState depends on
-- whether or not that input is Boolean or integer valued (more than on or off).
-- We therefore need the entirety of the ModelLayer that it sits in. We process
-- the entirety of the DMModel because we are in a Reader DMModel LaTeX anyway.
-- This assumes that all NodeNames in an entire parsed dmms file ARE UNIQUE.
-- That is the spec, is checked for, but it bears repeating
modelLaTeXGate :: Reader DMModel (Map.HashMap NodeName LaTeX)
modelLaTeXGate = do
    dmM <- ask
    return $ case dmM of
        (Fine mL) -> modelLaTeXGate' mL
        (LayerBinding _ mL dmM') -> Map.union (modelLaTeXGate' mL)
            (runReader modelLaTeXGate dmM')
        where
            modelLaTeXGate' m = Map.fromList pairs
                where
                    pairs = zip nNames (mkLaTeXGate (boolNNodes m) <$> nodes)
                    nNames = (nodeName . nodeMeta) <$> nodes
                    nodes = ((snd <$>) . Gr.labNodes . modelGraph) m

-- Return the NodeNames from those DMNodes in a ModelLayer which are boolean. 
boolNNodes :: ModelLayer -> Set.HashSet NodeName
boolNNodes mL = Set.fromList bNames
    where
        bNames = (nodeName . nodeMeta) <$> filter isDMNodeBoolean nodes
        nodes = ((snd <$>) . Gr.labNodes . modelGraph) mL

-- Is a DMNode boolean
isDMNodeBoolean :: DMNode -> Bool
isDMNodeBoolean x = (length . gateAssigns . nodeGate) x == 2

-- Is a ModelLayer boolean
isModelLayerBoolean :: ModelLayer -> Bool
isModelLayerBoolean =
    and . ((isDMNodeBoolean . snd) <$>) . Gr.labNodes . modelGraph

-- The display of a gate in LaTeX will depend on whether it, or its inputs, are
-- boolean or integer valued. If integer-valued, a node, whether on the left 
-- hand side of an assignment or in the expression itself, will have a
-- subscript to indicate which of its states is referenced. If boolean, no
-- subscript is necessary. 
mkLaTeXGate :: Set.HashSet NodeName -> DMNode -> LaTeX
mkLaTeXGate s n
    | Set.member nName s =
        let texState :: LaTeX
            texState = mathbf $ texy nName
            texExpr = (exprTex s . snd . head) assigns
        in
        fromLaTeX $ math (displayStyle <> lLet <> lLeft <> relax <> lLet
                <> lRight <> relax <> " " <> texState <> "=" <> texExpr)
    | otherwise =
        let combine x l = (math (displayStyle <>
                lLet <> lLeft <> relax <> lLet <> lRight <> relax
                <> " " <> x)) <> "\n" <> l
            texAssigns = zipWith ((<>) . (<> "=")) texStates texExprs
            texStates = (mathbf . (((texy nName) !:) . texy . fst)) <$> assigns
            texExprs = (exprTex s . snd) <$> assigns
        in
        fromLaTeX $ foldr combine TeXEmpty texAssigns
    where
        assigns = (tail . gateAssigns . nodeGate) n
        nName = (gNodeName . nodeGate) n

-- Render a NodeExpr to LaTeX, paying attention to whether or not the GateConsts
-- are from boolean nodes or not. 
exprTex :: LaTeXC l => Set.HashSet NodeName -> NodeExpr -> l
exprTex _ (GateLit b) = mathbf $ texy b
exprTex s (GateConst n st)
    | Set.member n s = mathbf $ (texy n)
    | otherwise      = mathbf $ (texy n) !: (texy st)
exprTex s (Not expr) = (commS "notop") <> parTexy s expr
exprTex s (Binary And expr1 expr2) =
    (parTexy s expr1) <> (commS "andop") <> (parTexy s expr2)
exprTex s (Binary Or expr1 expr2) =
    (parTexy s expr1) <> (commS "orop") <> (parTexy s expr2)

-- Put parentheses around and call exprTex on a compound NodeExpr, but only call
-- exprTex on a simple NodeExpr
parTexy :: LaTeXC l => Set.HashSet NodeName -> NodeExpr -> l
parTexy s ex@(GateLit _) = exprTex s ex
parTexy s ex@(GateConst _ _) = exprTex s ex
parTexy s ex@(Not _) = (autoParens . exprTex s) ex
parTexy s ex@(Binary And _ _) = (autoParens . exprTex s) ex
parTexy s ex@(Binary Or _ _) = (autoParens . exprTex s) ex

-- Prep the 2 rows of a supplementary table that describe a node. 
smNodePrep :: Map.HashMap NodeName LaTeX -> DMNode -> LaTeX
smNodePrep gmap n = 
  let nName = nodeName nMeta
      gate = fromJust $ Map.lookup nName gmap
      nDesc = TeXRaw $ (desc . nodeInfo) nMeta
      nSymbol = (texy . nodeType) nMeta
      nMeta = nodeMeta n
  in
  (addLineSpace $ Just $ CustomMeasure $
            (dimexpr <> (texy (1.5 :: Float))) <> defaultAddSpace) <> "\n" <>
  (multicolumn 1 [ParColumnTop node1ASpecCall] (uScoreSub nName)
    & (multicolumn 3 [ParColumnMid node1BSpecCall] gate)) <> 
        lnbk <> "\n" <> (addLineSpace $ Just $ CustomMeasure $
        (dimexpr <> (texy (1.5 :: Float))) <> defaultAddSpace) <> "\n" <>
  ((multicolumn 1 [ParColumnMid node2ASpecCall] TeXEmpty)
    & (multicolumn 1 [ParColumnMid node2BSpecCall] nSymbol)
    & (multicolumn 2 [ParColumnMid node2CSpecCall] nDesc)
  ) <> "\n"

-- Prep the n rows of a supplementary table that describe a nodes InLinks.
smLinkPrep :: (DMLink, NodeName) -> LaTeX
smLinkPrep lk =
  let nName   = (uScoreSub . snd) lk
      lDesc   = (TeXRaw . desc . linkInfo . fst) lk
      lType   = (texy . linkType . fst) lk
      lEffect = (texy . linkEffect . fst) lk
  in
  (multicolumn 1 [ParColumnMid linkASpecCall] TeXEmpty) 
    & (multicolumn 1
      [ParColumnMid linkBSpecCall] (makecell (lEffect <> lnbk <> lType)))
    & (multicolumn 1 [ParColumnMid linkCSpecCall] nName)
    & (multicolumn 1 [ParColumnMid linkDSpecCall] lDesc)

-- Fold up a List of InLink rows. 
inLinkConcat :: (LaTeXC l) => [l] -> l
inLinkConcat = fold . L.intersperse spacer
    where
        spacer = lnbk <> "\n" <> (addLineSpace $ Just $ CustomMeasure $
            (dimexpr <> (texy (1.0 :: Float))) <> defaultAddSpace) <> "\n"

-- The PDF is structured by sections, which mention the modelName of the layer
-- below, then tables displaying one NodeName from a layer, then the nodes from
-- the layer below (and their Inlinks) which are the first node's switch.
-- mappingGrouper preps this arrangement for the entire DMModel. In the case
-- that the whole DMModel is a single (Fine ModelLayer), that modelName is used 
mappingGrouper :: Reader DMModel [(T.Text, [(InAdj, [InAdj])])]
mappingGrouper = let f x = (x, []) in do
    dmM <- ask
    return $ case dmM of
        (Fine mL) ->
            [((modelName . modelMeta) mL, f <$> (inAdjs (modelGraph mL)))]
        (LayerBinding x y z) -> go x y (coarseLayer z) : go' z
            where
                go' (Fine _) = []
                go' ((LayerBinding a b c)) =  go a b (coarseLayer c) : go' c
                go mM cML fML = (mName, zip coarseAdjs fineAdjs)
                    where
                        fineAdjs = fromJust <<$>> 
                          (sequenceA
                            (sequenceA <$> (findInAdj <<$>> fineNNames))
                            finePairs)
                        finePairs = inAdjs fineGr
                        coarseAdjs = fromJust <$> 
                          (sequenceA (findInAdj <$> coarseNNames) coarsePairs)
                        coarsePairs = inAdjs coarseGr
                        mName = (modelName . modelMeta) fML
                        (coarseGr, fineGr) = (modelGraph cML, modelGraph fML)
                        coarseNNames = fst <$> mM
                        fineNNames   = snd <$> mM

mkBibFile :: CitationDictionary -> T.Text
mkBibFile = T.concat . L.intersperse "\n\n" . ((mkBibText . snd) <$>)
                . Map.toList

mkBibText :: BibTeXEntry -> T.Text
mkBibText (BibTeXEntry{entryKey = key, entryType = tp, entryFields = fs}) =
    "@" <> tp <> "{" <> key <> ",\n" <> fields <> "\n}"
    where
        fields = (T.concat . L.intersperse ",\n" . (f <$>)) fs
        f (field, record) = field <> " = " <> record

-- Render a DMModel into a List of (ModelName, BooleanNet) pairs. A pair is only
-- created from a ModeLayer if every node in that ModeLayer is boolean. 
-- mkLEBNExpr and mkTTBNExpr are kept internal to ensure that they do not 
-- accidentally get used on a non-boolean NodeExpr. 
mkBooleanNet :: DMModel -> [(ModelName, BooleanNet)]
mkBooleanNet dmM = zip modelNs $ mkBooleanNet' <$> boolLs
    where
        mkBooleanNet' mL = T.unlines bNGates 
            where
                bNGates = zipWith (\n ex -> n <> " *= " <> ex) bNNames bNExpr
                bNNames = gNodeName <$> gates
                bNExpr = mkBNExpr <$> gates
                gates = (((nodeGate . snd) <$>) . Gr.labNodes . modelGraph) mL
        mkBNExpr :: NodeGate -> T.Text
        mkBNExpr nG = case gateOrigin nG of
            DMMSTruthTable ->  mkTTBNExpr nG
            _              -> (mkLEBNExpr . snd . last . gateAssigns) nG
        modelNs = (modelName . modelMeta) <$> boolLs
        boolLs = filter isModelLayerBoolean layers
        layers = modelLayers dmM

mkLEBNExpr :: NodeExpr -> T.Text
mkLEBNExpr (GateLit b) = (toStrict . PS.pShowNoColor) b
mkLEBNExpr (GateConst n i)
    | i /= 0 = n
    | otherwise = "not " <> n
mkLEBNExpr (Not expr) = "not " <> (exprPars mkLEBNExpr expr)
mkLEBNExpr (Binary And expr1 expr2) =
    (exprPars mkLEBNExpr expr1) <> " and " <> (exprPars mkLEBNExpr expr2)
mkLEBNExpr (Binary Or expr1 expr2) =
    (exprPars mkLEBNExpr expr1) <> " or " <> (exprPars mkLEBNExpr expr2)

mkTTBNExpr :: NodeGate -> T.Text
mkTTBNExpr nG = T.intercalate " or " ands
    where
        ands = (\x -> "(" <> x <> ")") <$> (ander nNames <$> orLists)
        ander nns nss = T.intercalate " and " $ zipWith onOff nns nss
        onOff :: NodeName -> NodeState -> T.Text
        onOff nN nS
            | nS == 0 = "(not " <> nN <> ")"
            |otherwise = nN
        orLists = L.sort $ U.toList <$> orVecs
        orVecs = (Map.keys . (Map.filter (\i -> i == 1)) . gateTable) nG
        nNames = gateOrder nG


type BooleanNet = T.Text

-- Assemble the three legend tables
legend :: LaTeX
legend = subtables $ "\n"
            <> nTypeLegend
            <> lTypeLegend
            <> lEffectLegend

legendTSpec :: [TableSpec]
legendTSpec = [ Separator TeXEmpty
              , LeftColumn
              , LeftColumn
              , ParColumnTop $ colDim 0.6
              , Separator TeXEmpty
              ]

-- Create a table to display the symbols that represent the various NodeTypes.
nTypeLegend :: LaTeX
nTypeLegend = legendScafold lCap legendRows
    where
        lCap = TeXRaw "Node Type" 
        legendRows = fold $ mkLRow <$> triples
        triples = zip3 symbolList typesList descList
        descList = nTDesc <$> optionList
        symbolList = texy <$> optionList
        typesList = sPShowNoColor <$> optionList
        optionList = tail [minBound :: NodeType ..]

-- Create a table to display the symbols that represent the various LinkTypes.
lTypeLegend :: LaTeX
lTypeLegend = legendScafold lCap legendRows
    where
        lCap = TeXRaw "Link Type"
        legendRows = fold $ mkLRow <$> triples
        triples = zip3 symbolList typesList descList
        descList = lTDesc <$> optionList
        symbolList = texy <$> optionList
        typesList = sPShowNoColor <$> optionList
        optionList = tail [minBound :: LinkType ..]

-- Create a table to display the symbols that represent the various LinkEffects.
lEffectLegend :: LaTeX
lEffectLegend = legendScafold lCap legendRows
    where
        lCap = TeXRaw "Link Effect"
        legendRows = fold $ mkLRow <$> triples
        triples = zip3 symbolList typesList descList
        descList = lEDesc <$> optionList
        symbolList = texy <$> optionList
        typesList = sPShowNoColor <$> optionList
        optionList = tail [minBound :: LinkEffect ..]


-- 
legendScafold :: LaTeX -> LaTeX -> LaTeX
legendScafold lCaption rows = "\n" <>
    ("\n" <> (longtable (Just Center) legendTSpec $ "\n" <>
    caption "Key to " <> lCaption <> " Symbols" <> "\n" <>
    endhead <> lnbk <> "\n" <>
    toprule Nothing <> "\n" <>
    "Symbol" & lCaption & "Description" <>lnbk <> "\n" <>
    midrule Nothing <> "\n" <>
    rows <>
    bottomrule Nothing <> "\n"
    ) <> "\n")

mkLRow :: (LaTeX, T.Text, T.Text) -> LaTeX
mkLRow (symbol, haskT, aDesc) =
    symbol & texy haskT & texy aDesc <> lnbk <>
        (addLineSpace $ Just $ CustomMeasure $
            (dimexpr <> (texy (0.75 :: Float))) <> defaultAddSpace) <> "\n"


