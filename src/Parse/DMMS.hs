{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parse.DMMS
    ( modelFileParse ) 
      where

import Types.DMModel
import qualified Types.Simulation as S
import Constants
import Utilities
import qualified Data.Text as T
import TextShow
import qualified Data.Versions as Ver
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as SC
import qualified Data.Vector.Unboxed as U
import qualified Data.Graph.Inductive as Gr
import qualified Data.Graph.Inductive.NodeMap as Grm
import qualified Data.HashMap.Strict as Map
import qualified Data.Bimap as BM
import qualified Data.HashSet as Set
import Control.Monad.Combinators.Expr   -- from parser-combinators
import Control.Applicative.Permutations -- from parser-combinators
import Data.Validation
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LE
import Data.Scientific
import qualified Data.List as L
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Bifunctor as B
import Data.Void
import Data.Char (toLower)
import Data.Word (Word8)
import Control.Monad (void)
import Numeric (readHex)
import Data.Ix (inRange)


type Parser = Parsec Void T.Text

sc :: Parser ()
sc = LE.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = LE.skipLineComment "//"
    blockCmnt = LE.skipBlockComment "/*" "*/"

hsc :: Parser ()
hsc = LE.space hspace1 lineCmnt blockCmnt
  where
    lineCmnt  = LE.skipLineComment "//"
    blockCmnt = LE.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = LE.lexeme sc

hlexeme :: Parser a -> Parser a
hlexeme = LE.lexeme hsc

symbol :: T.Text -> Parser T.Text
symbol = LE.symbol sc

hsymbol :: T.Text -> Parser T.Text
hsymbol = LE.symbol hsc

-- | 'braces' parses something between braces.

-- braces :: Parser a -> Parser a
-- braces = between (symbol "{") (symbol "}")

-- hbraces :: Parser a -> Parser a
-- hbraces = between (hsymbol "{") (hsymbol "}")

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- hparens :: Parser a -> Parser a
-- hparens = between (hsymbol "(") (hsymbol ")")

-- | 'number' parses an number in scientific format.

number :: Parser Scientific
number = lexeme LE.scientific

signedNumber :: Parser Scientific
signedNumber = LE.signed sc number

-- | 'integer' parses an Int. (Note that this does not parse signs!)

integer :: Parser Int
integer = lexeme LE.decimal


-- | 'colon' parses a colon.

colon :: Parser T.Text
colon = symbol ":"

hcolon :: Parser T.Text
hcolon = hsymbol ":"

hash :: Parser T.Text
hash = symbol "#"

-- | 'stateAssign' parses a node state assignment operator.

stateAssign :: Parser T.Text
stateAssign = symbol "*="

-- | 'comma' parses a comma.

comma :: Parser T.Text
comma = symbol ","

-- hcomma :: Parser T.Text
-- hcomma = hsymbol ","

-- | 'rword' generates a parser for a specified reserved word. 

rword :: T.Text -> Parser ()
rword w = (lexeme . try) (chunk w *> notFollowedBy alphaNumChar)

-- Parse an identifier
identifier :: T.Text -> Parser T.Text
identifier name = lexeme $ rword name >> colon >> (p >>= check)
  where
    p       = T.pack <$> ((:) <$> letterChar <*> 
        (many (alphaNumChar <|> char '_')))
    check x = if x `elem` rws
                then fail $ "Keyword " ++ show x ++ " cannot be a "
                    ++ (T.unpack name)
                else return x

-- Parse an assignment
variable :: Parser T.Text
variable = (lexeme . try) (p >>= check)
  where
    p       = T.pack <$> ((:) <$> letterChar <*> 
        (many (alphaNumChar <|> char '_')))
    check x = if x `elem` rws
                then fail $ "Keyword " ++ show x ++ " cannot be a variable. "
                else return x

-- Parse a dmms file. 
modelFileParse :: Parser (FileFormatVersion, (DMModel, CitationDictionary))
modelFileParse = sc >> (,) <$> dmmsVersion <*> modelCiteParse <* eof

-- Parse the dmms file format version. 
dmmsVersion :: Parser FileFormatVersion
dmmsVersion = versionParse "FormatVersion"

-- Parse a DMModel and CitationDictionary. 
modelCiteParse :: Parser (DMModel, CitationDictionary)
modelCiteParse = ((,) <$> modelParse <*> citeDictParse)
    >>= modelDupeCheck
    >>= nodeDupeCheck
    >>= nodeDifferentiate
    >>= phenotypeDupeCheck

modelParse :: Parser DMModel
modelParse = between (symbol "Model{") (symbol "Model}") 
    (
        (try 
            ((runPermutation $
                (,,,)
                    <$> toPermutation modelMappingParse
--            There might not be SwitchProfiles{}, so provide an empty default.
                    <*> toPermutationWithDefault [] switchProfilesParse
                    <*> (ModelLayer <$> toPermutation modelGraphParse
                                     <*> toPermutation modelConfigParse)
                    <*> toPermutation modelParse
             )
                >>= modelMappingCheck
            )
        )
        <|> (Fine <$> modelLayerParse)
    ) 

-- Return a DMModel whose Gr.Nodes are unique in all layers. This is useful when
-- doing anything related to the ModelMappings, since by definition they work
-- with Gr.LNodes from different Gr.Gr DMNode DMLink graphs. 
nodeDifferentiate :: (DMModel, CitationDictionary)
                  -> Parser (DMModel, CitationDictionary)
nodeDifferentiate (dMM, cd) = do
    let spreadDMM = nDifferentiate dMM 0
    return (spreadDMM, cd)


nDifferentiate :: DMModel -> Int -> DMModel
nDifferentiate (Fine mL) offSet = Fine shiftedLayer
    where
        -- Reassemble the shifted layer.
        shiftedLayer = ModelLayer shiftedGraph mMeta
        shiftedGraph = indexShift offSet mGraph
        (mGraph, mMeta) = (modelGraph mL, modelMeta mL)
nDifferentiate (LayerBinding mM mL dm) offSet =
    LayerBinding mM shiftedLayer (nDifferentiate dm newOffSet)
    where
        -- Calculate the new offset. 
        newOffSet =  1 + ((maximum . Gr.nodes) shiftedGraph)
        -- Reassemble the shifted layer.
        shiftedLayer = ModelLayer shiftedGraph mMeta
        shiftedGraph = indexShift offSet mGraph
        -- Dissasemble the layer to get at the graph.
        (mGraph, mMeta) = (modelGraph mL, modelMeta mL)


indexShift :: Int -> Gr.Gr a b -> Gr.Gr a b
indexShift offSet g = Gr.mkGraph sNodes sEdges -- Reassemble the graph. 
    where
        -- Shift the node IDs and the node references in the edges. 
        sEdges = ((\i (x, y, z) -> (x + i, y + i, z)) offSet) <$> edges
        sNodes = (B.first (+ offSet)) <$> nodes
        -- Dissasemble the graph to get at the nodes and edges.
        (edges, nodes)  = (Gr.labEdges g, Gr.labNodes g)


-- Check that ALL NodeNames in the entire parsed DMModel are unique. 
nodeDupeCheck :: (DMModel, CitationDictionary)
               -> Parser (DMModel, CitationDictionary)
nodeDupeCheck (dM, cd)
    | L.null repeats = return (dM, cd)
    | otherwise = fail $ show $ DuplicatedNodeNames repeats
    where
        repeats = repeated ns
        ns = (nodeName . nodeMeta) <$> (concatMap layerNodes layers)
        layers = modelLayers dM  

-- Make sure that Phenotype PhenotypeNames are globally unique. 
phenotypeDupeCheck :: (DMModel, CitationDictionary)
                   -> Parser (DMModel, CitationDictionary)
phenotypeDupeCheck (dM, cd)
    | L.null repeats = return (dM, cd)
    | otherwise = fail $ show $ DuplicatedPhenotypeNames repeats
    where
        repeats = repeated pNs
        pNs = phenotypeName <$> (concatMap (snd . snd) switches)
        switches = (concat . modelMappings) dM

-- Check that ALL ModelNames in the entire parsed DMModel are unique. 
modelDupeCheck :: (DMModel, CitationDictionary)
               -> Parser (DMModel, CitationDictionary)
modelDupeCheck (dM, cd)
    | L.null repeats = return (dM, cd)
    | otherwise = fail $ show $ DuplicatedModelNames repeats
    where
        repeats = repeated ms
        ms = (fmap (modelName . modelMeta) . modelLayers) dM

-- Assemble the pieces of the LayerBinding, with various consistency checks. 
modelMappingCheck :: (DMMSModelMapping, [SwitchProfile], ModelLayer, DMModel)
                  -> Parser DMModel
modelMappingCheck (dmmsMap, sProfiles, mLayer, mModel) =
    case mkLayerBinding dmmsMap sProfiles mLayer mModel of
        Success dmModel -> case mkModelLayer mLayer of
            Success _ -> return dmModel
            Failure errs -> fail $ show errs
        Failure errs    -> fail $ show errs

-- Parse the mapping from one layer of a model to the next. 
modelMappingParse :: Parser DMMSModelMapping
modelMappingParse = between (symbol "ModelMapping{") (symbol "ModelMapping}") $
    some ((,) <$> identifier "Switch" <*> parens 
           ((fmap T.pack) <$> sepBy1 (some $ alphaNumChar <|> char '_') comma))

-- Parse the phenotypes of the switches, so that we may usefully label the
-- attractors of the corresponding fine ModelLayer. 
switchProfilesParse :: Parser [SwitchProfile]
switchProfilesParse = between (symbol "SwitchProfiles{") 
                              (symbol "SwitchProfiles}") $
                                some switchPhenotypesParse

switchPhenotypesParse :: Parser SwitchProfile
switchPhenotypesParse = between (symbol "SwitchPhenotypes{")
                                (symbol "SwitchPhenotypes}") $
    runPermutation $
        (,) <$> toPermutation (identifier "SwitchName")
            <*> toPermutation (some (try phenotypeParse))

phenotypeParse :: Parser Phenotype
phenotypeParse =   barePhParse
              <|> (wrappedPhParse >>= errorPhcheck)

barePhParse :: Parser Phenotype
barePhParse = do
    phName <- variable
    void colon
    switchState <- integer
    void stateAssign
    phFingerprint <- fingerPrintParse
    return $ Phenotype phName switchState phFingerprint Nothing []

wrappedPhParse :: Parser Phenotype
wrappedPhParse = between (symbol "Phenotype{") (symbol "Phenotype}") $ do
    mainPh <- barePhParse
    errorPhs <- some errorPhParse
    void $ errorPhcheck (mainPh {phenotypeErrors = errorPhs})
    let phErrFpL = length . phErrorFingerprint
        sortedPhErrors = (reverse . L.sortOn phErrFpL) errorPhs
    return $ mainPh {phenotypeErrors = sortedPhErrors}

errorPhParse :: Parser PhenotypeError
errorPhParse = do
    phEName <- variable
    void colon
    void (symbol "E")
    phErrorIndex <- integer
    void stateAssign
    phErrFingerprint <- fingerPrintParse
    return $ PhError phEName phErrorIndex phErrFingerprint

-- Check that no PhenotypeErrors are isomorphic up to permutation to their
-- parent Phenotype, or each other. 
errorPhcheck :: Phenotype -> Parser Phenotype
errorPhcheck phs
    | (not . null) differentStartingSSs = fail $ show $
        PhErrorsStartDifferentlyFromPh (phName, fst <$> phErrSSStarts)
    | (not . null) dupofPhPhErrs = fail $ show $
        PhenotypeandPhErrorCyclicPermute (phName, fst <$> dupofPhPhErrs)
    | (not . null) (head <$> dupePhErrGroups) = fail $ show $ 
        CyclicPermutePhErrorSubSpaces $ phErrorName <<$>> dupePhErrGroups
    | otherwise  = return phs
    where
        dupePhErrGroups = (filter ((>1) . length) . slowGroupBySS) phErrs
        areSSIsomorphic phErr1 phErr2 = areCyclicPermutes fPrint1 fPrint2
            where
                fPrint1 = phErrorFingerprint phErr1
                fPrint2 = phErrorFingerprint phErr2
        dupofPhPhErrs = filter ((areCyclicPermutes phFPrint) . snd) phErrPairs
        differentStartingSSs = filter ((phSSStart ==) . snd) phErrSSStarts
        phErrSSStarts :: [(PhenotypeName, SubSpace)]
        phErrSSStarts = head <<$>> phErrPairs
        phSSStart = head phFPrint
        phErrPairs :: [(PhenotypeName, [SubSpace])]
        phErrPairs = phErrPairF <$> phErrs
        phErrPairF phErr = (phErrorName phErr, phErrorFingerprint phErr)
        phErrs = phenotypeErrors phs
        phName = phenotypeName phs
        phFPrint = fingerprint phs

-- We use this because I do not want to re-order the SubSpaces by "smallest"
-- element, and because the numberof PhenotypeErrors in any given phenotype is
-- unlikely to ever exceed 10, much less 100. Thus an O(n^2) check of cyclic
-- permutivity for each PhonotypeError against every other is not a deal-breaker
-- . -- Pete Regan, June 25, 2025
slowGroupBySS :: [PhenotypeError] -> [[PhenotypeError]] 
slowGroupBySS [] = []
slowGroupBySS (phErr:phErrs) = (phErr : cp) : slowGroupBySS notCp
    where
        (cp, notCp) = L.partition (checkSSCP phErr) phErrs
        checkSSCP phErrX phErrY = areCyclicPermutes fPrintX fPrintY
            where
                fPrintX = phErrorFingerprint phErrX
                fPrintY = phErrorFingerprint phErrY

-- Note that you may not write to an element of a SubSpace without a state, even
-- if the DMNode in question is binary; ie CyclinA:1, not CyclinA. 
fingerPrintParse :: Parser [SubSpace]
fingerPrintParse = sepBy1 (parens (fingerNodeParse `sepBy1` comma))
                          (symbol "->")

fingerNodeParse :: Parser (NodeName, NodeState)
fingerNodeParse = do
    fingerNode <- variable
    void colon
    fingerNodeState <- integer
    return (fingerNode, fingerNodeState)

modelLayerParse :: Parser ModelLayer
modelLayerParse = try
    (
        (runPermutation $
            ModelLayer <$> toPermutation modelGraphParse
                       <*> toPermutation modelConfigParse
    )
    >>= modelLayerCheck)


-- Parse the internal network of a particular model layer. 
modelGraphParse :: Parser ModelGraph
modelGraphParse = between (symbol "ModelGraph{") (symbol "ModelGraph}") $
    some nodeParse
    >>= nodeUniqueCheck
    >>= nodeStateCheck
    >>= nodeLinkCheck
    >>= (traverse linkEffectCheck)
    >>= coordDimensionCheck
    >>= modelGraphAssembler
    >>= nonBinaryMultiInputNodesCheck

nodeUniqueCheck :: [(DMNode, [(NodeName, DMLink)])]
               -> Parser [(DMNode, [(NodeName, DMLink)])] 
nodeUniqueCheck nodesWLinks = case repeated nodeNames of
    [] -> return nodesWLinks
    rns@(_:_) -> fail $ "NodeNamesRepeated\n" <> (T.unpack . T.unlines) rns
    where
        nodeNames = (nodeName . nodeMeta . fst) <$> nodesWLinks

-- Check that, for every node and state reference in every NodeExpr, that node
-- exists in the DMModel layer that we are in, and has a NodeStateAssign for
-- that state. Also check to make sure that every TruthTable that references a
-- node has rows for every state that that node can have, even if some of those
-- states are not referenced in the discrete logical expression which might also
-- exist for that NodeGate in the DMMS file. 
nodeStateCheck :: [(DMNode, [(NodeName, DMLink)])]
               -> Parser [(DMNode, [(NodeName, DMLink)])]
nodeStateCheck nodesWLinks =  case isSubset exNodes nodes of
    False -> fail $ show $ NodeRefdNodesMismatch (exNodes L.\\ nodes)
    True -> case filter (\(n, m) -> n /= m) $ zip exprRefs equivalentNodeRefs of
        zs@(_:_) -> fail $ show $ StatesRefdStatesMisMatch $
            (\(a, b) -> (fst a, (snd a, snd b))) <$> zs
        []  -> case sequenceA (tableSizeCheck nodeMaxes <$> dmNodes) of
            Failure errs -> fail $
                T.unpack $ T.unlines $ showt <$> errs
            Success ns -> return $ zip ns links
    where
        links = snd <$> nodesWLinks
        dmNodes = fst <$> nodesWLinks
        nodeGates = nodeGate <$> dmNodes
        exprRefs = L.sort $ refdNodesStatesNG $
            concatMap (fmap snd . gateAssigns) nodeGates
        exNodes = fst <$> exprRefs
        nodes = gNodeName <$> nodeGates
        assigns = gateAssigns <$> nodeGates
        nodeRefs = L.sort $ zip nodes $ (fst <$>) <$> assigns
        equivalentNodeRefs = filter (isExprNode exprRefs) nodeRefs
        isExprNode refs (n, _) = elem n $ fst <$> refs
        nodeMaxes :: [(NodeName, NodeState)]
        nodeMaxes = (\(x, ns) -> (x, maximum ns)) <$> nodeRefs

-- Given a List of the maxima of a Layer's DMNodes and a DMNode, check that the
-- maxima of each referenced node in the DMNode matches. If it does not, check
-- the GateOrigin: If it is from a LogicalExpression, then simply correct the 
-- NodeGate's TruthTable. Otherwise, return a GateInvalid and require the user
-- to correct the TruthTable in the DMMS file. 
tableSizeCheck :: [(NodeName, NodeState)]
               -> DMNode
               -> Validation [GateInvalid] DMNode
tableSizeCheck lMaxima n = case misses of
    [] -> Success n
    mss@(_:_) -> case (gOrigin == DMMSTruthTable) || (gOrigin == Both) of
        True  -> Failure $ [TruthTableIncomplete misses]
        False -> Success $ tableMaxAdjust (fst <$> mss) n
    where
        misses = compTRefsNodes lMaxima (nName, refNodeMxMp)
        refNodeMxMp = 
            Map.fromList $ (maximum <$>) <$> (refdNodesStatesTM order tTable)
        gOrigin = gateOrigin nGate
        order = gateOrder nGate
        tTable = gateTable nGate
        nName = nodeName nMeta
        nMeta = nodeMeta n
        nGate = nodeGate n

-- Check if the maximum value of the input nodes in a TruthTable match the
-- actual maximum of those nodes from their DMNodes. Returned is a List of:
-- (
--  ( a NodeName, Its maximum NodeState)
--  , (The NodeName where it is referenced, The maximum in that TruthTable.
--  )
-- )
-- The List only has those cases where these maxima are DIFFERENT.  
compTRefsNodes :: [(NodeName, NodeState)]
               -> (NodeName, Map.HashMap NodeName NodeState)
               -> [((NodeName, NodeState), (NodeName, NodeState))]
compTRefsNodes nMaxes refMap = filter noSames $ mapMaybe (go refMap) nMaxes
    where
        go (rfnm, rmxs) (nM, nMax) = 
                            (,) <$> ((,) <$> (pure nM) <*> (pure nMax))
                                <*> ((,) <$> pure rfnm <*> (Map.lookup nM rmxs))
        noSames ((_, n), (_, m)) = n /= m

-- A TruthTable which was derived from a logical expression may be missing
-- the whole range of some of its input nodes, as they may not mave been
-- referenced in the expression. Given the correct maximum states, this corrects
-- that TruthTable. 
tableMaxAdjust :: [(NodeName, NodeState)] -> DMNode -> DMNode
tableMaxAdjust maxima n = DMNode nMeta newGate
    where
        newGate = NodeGate gName order assigns newTable origin
        newTable = Map.fromList inputOutputPairs
        inputOutputPairs = zip inputs $ (fromJust . gateEval assigns) <$> combos
        inputs = U.fromList <$> ((snd <$>) <$> combosList)
        combos = Map.fromList <$> combosList
        combosList = (zip nNames) <$> (sequenceA nStates)
        (nNames, nStates) = unzip orderedStateRanges
        orderedStateRanges = sortWithOrderOn fst order stateRanges
        stateRanges = (\(nName, s) -> (nName, [0..s])) <$> (Map.toList newMap)
        newMap = foldr fixMax nMap maxima
        fixMax (nM, nS) m = Map.adjust (const nS) nM m
        nMap = foldr (Map.unionWith max) Map.empty (exprNodes <$> exprs)
        exprs = snd <$> assigns
        gName = nodeName nMeta
        order = gateOrder nGate
        assigns = gateAssigns nGate
        origin = gateOrigin nGate
        nGate = nodeGate n
        nMeta = nodeMeta n


-- Check that for the NodeName associated with each DMLink there exists an
-- actual DMNode with that NodeName
nodeLinkCheck :: [(DMNode, [(NodeName, DMLink)])]
              -> Parser [(DMNode, [(NodeName, DMLink)])]
nodeLinkCheck nodesWLinks
    | orphanLinkNamesSet == Set.empty = return nodesWLinks
    | otherwise = fail $ show $
        NodeInlinkMismatch (Set.toList orphanLinkNamesSet)
    where
      orphanLinkNamesSet = lSet `Set.difference` nSet
      lSet = (Set.fromList . fmap fst . concatMap snd) nodesWLinks
      nSet = Set.fromList $ (nodeName . nodeMeta . fst) <$> nodesWLinks

-- For each boolean DMNode, check that DMLinks of that DMNode whose LinkEffect
-- is Activation or Repression, that InLink is actually activating or repressing
-- . 
linkEffectCheck :: (DMNode, [(NodeName, DMLink)])
                -> Parser (DMNode, [(NodeName, DMLink)])
linkEffectCheck (dmNode, linksWNNs)
    | isNodeBoolean dmNode = return (dmNode, linksWNNs)
    | otherwise = (,) <$> pure dmNode <*> checkedInLs
        where
            isNodeBoolean = (>2) . length . gateAssigns . nodeGate
            nName = (nodeName . nodeMeta) dmNode
            gOrder = (gateOrder . nodeGate) dmNode
            tTable = (gateTable . nodeGate) dmNode
            checkedInLs = traverse lEffectChF linksWNNs
            lEffectChF :: (NodeName, DMLink) -> Parser (NodeName, DMLink)
            lEffectChF (nNm, dmLink)
                | linkMax > 1 = return (nNm, dmLink)
                | otherwise = case linkEffect dmLink of
                    Activation
                        | aligned > misAligned -> return (nNm, dmLink)
                        | aligned == misAligned -> fail $
                            errorF Activation Context_Dependent
                        | aligned < misAligned -> fail $
                            errorF Activation Repression
                    Repression
                        | aligned < misAligned -> return (nNm, dmLink)
                        | aligned == misAligned -> fail $
                            errorF Repression Context_Dependent
                        | aligned > misAligned -> fail $
                            errorF Repression Activation
                    _ -> return (nNm, dmLink)
                where
                    errorF incLE corLE = "The " <> T.unpack nNm <>
                        " InLink " <> "for the " <> T.unpack nName <> " Node has\
                        \ LinkEffect " <> show incLE <> ", but its actual\
                        \ behavior is " <> show corLE
                    misAligned = (length . filter (not . pairEq)) lInputPairs
                    aligned = (length . filter pairEq) lInputPairs
                    pairEq (i, j) = i == j
                    lInputPairs = (fmap . B.first) (U.! lIndex) tTPairs
                    tTPairs = Map.toList tTable
                    linkMax = (maximum . fmap (U.! lIndex) .  Map.keys) tTable
                    lIndex = (fromJust . L.elemIndex nNm) gOrder
            
            
            
            

-- Check that all of the NodeCoordinates in a given layer are of the same
-- dimension, or empty. If they are, fill the empty ones with appropriately
-- dimensioned origin points. 
coordDimensionCheck :: [(DMNode, [(NodeName, DMLink)])]
                    -> Parser [(DMNode, [(NodeName, DMLink)])]
coordDimensionCheck nodesWLinks = case L.nub nodeDims of
    [] -> fail "Expected some DMNodes"
    [n] -> case n < 2 of
        True  -> return $ zip (padNodeDim 2 <$> nodes) links
        False -> return $ nodesWLinks
    [0,n] -> case n < 2 of
        True  -> return $ zip (padNodeDim 2 <$> nodes) links
        False -> return $ zip (padNodeDim n <$> nodes) links
    (n:m:ms) -> fail $ show $ NodeDimensionsInconsistent (n:m:ms)
    where
        nodeDims = L.sort $ (U.length . nodeCoordinate . nodeMeta) <$> nodes 
        (nodes, links) = unzip nodesWLinks 

-- Extend the dimension of the coordinate a DMNode to the given Int, adding on
-- zeroes as necessary. 
padNodeDim :: Int -> DMNode -> DMNode
padNodeDim dim oldN@(DMNode nMeta nGate)
    | oldDim >= dim = oldN
    | otherwise = (DMNode (nMeta {nodeCoordinate = newCoord}) nGate)
    where
        newCoord = (oldCoord) U.++ padVec
        padVec = U.replicate (dim - oldDim) (0 :: Double)
        oldDim = U.length oldCoord
        oldCoord = nodeCoordinate nMeta

-- Assemble the DMNodes & DMLinks into an fgl graph. The fromJust is justified
-- because we have already ensured that every NodeName associated with a DMLink
-- exists as a DMNode in the model Layer. 
modelGraphAssembler :: [(DMNode, [(NodeName, DMLink)])] -> Parser ModelGraph
modelGraphAssembler ns = return $ Gr.mkGraph grNodes grEdges
    where
      (grNodes, grMap) = Grm.mkNodes Grm.new dmNodes
      grEdges = fromJust $ Grm.mkEdges grMap linkTriples
      linkTriples :: [(DMNode, DMNode, DMLink)]
      linkTriples = concatMap mkLinkTriple ns
      mkLinkTriple (n, ls) = (\y (x,z) -> (x,y,z)) n <$> (nSub <$> ls)
      nSub (nName, l) = (fromJust $ Map.lookup nName nMap, l)
      nMap = Map.fromList $ zip ((nodeName . nodeMeta) <$> dmNodes) dmNodes
      dmNodes = fst <$> ns

-- Make sure that, in the case that an environmental input is constructed of
-- multiple nodes, that all of those nodes are binary. Otherwise, why would we
-- bother with multi-node inputs. We assume they are wired such that, from first
-- to last in the list, for eg a 3-node input, 000, 001, 011, and 111 will be
-- the attractors of the input that represent the zeroth through third levels.
-- In this way, an n-level input will be represented by n-1 nodes. 
nonBinaryMultiInputNodesCheck :: ModelGraph -> Parser ModelGraph
nonBinaryMultiInputNodesCheck mG = do
    let multiNodeInputs = filter (\x -> length x >= 2) $ S.inputs mG
        enbyLists = filter (not . L.null) $ mkEnbyList <$> multiNodeInputs
    case enbyLists of
        [] -> return mG
        enbyLs -> do
            let enbyLsStr = L.intercalate "\n" (show <$> enbyLs)
            fail $ "Non-binary nodes in multi-node input: " ++ enbyLsStr ++
                (show enbyLs)

mkEnbyList :: [DMNode] -> [(NodeName, [NodeStateAssign])]
mkEnbyList ns = filter enbys nameAssigns
    where
        enbys xs = ((length . snd) xs) > 2
        nameAssigns = (\n -> (gNodeName n, gateAssigns n)) <$> gates
        gates = nodeGate <$> ns

-- Parse Model metadata
modelConfigParse :: Parser ModelMeta
modelConfigParse = between (symbol "ModelMetaData{") (symbol "ModelMetaData}")
   (runPermutation $ 
      ModelMeta <$> toPermutation (identifier "ModelName")
                <*> toPermutation (versionParse "ModelVersion")
                <*> toPermutation modelPaperParse
                <*> toPermutation (biasOrderParse "BiasOrderFirst")
                <*> toPermutation (biasOrderParse "BiasOrderLast")
                <*> (doubleUncurry LitInfo
                     <$> toPermutation (extractCitations "ModelDescription")
                     <*> toPermutation (extractCitations "ModelNotes")
                    )
   )

-- Parse a version
versionParse :: T.Text -> Parser Ver.SemVer
versionParse mVer = (lexeme . try) $ (rword mVer) >> colon >> Ver.semver'

-- Parse a list of paper references          
modelPaperParse :: Parser [T.Text]
modelPaperParse = do
    _ <- symbol "ModelPaper"
    _ <- hcolon
    paperKeys <- sepBy citeKeyParse comma
    _ <- eol
    return paperKeys

-- Parse a list of (NodeName, NodeState) for BiasOrder*
biasOrderParse :: T.Text -> Parser [BiasOrder]
biasOrderParse bias = lexeme
    (rword bias
        >> colon
        >> (sepBy ((WholeNode <$> variable) <|> specificStateParse) comma)
    ) >>= check
    where
        check xs = case repeated wss of
            [] -> case repeated sss of
                [] -> case L.intersect wss (fst <$> sss) of
                    [] -> return xs
                    oss -> fail $ show (DuplicatedBiasOrderNodeNames oss)
                ms -> fail $ show bias <> ": " <>
                    show (DuplicatedBiasOrderNodeStates ms)
            ns -> fail $ show bias <> ": " <>
                show (DuplicatedBiasOrderNodeNames ns)
        
            where
                sss = mapMaybe specificSs xs
                specificSs (WholeNode _) = Nothing
                specificSs (SpecificState nN nSt ) = Just (nN, nSt)
                wss = mapMaybe wholes xs
                wholes (WholeNode nN) = Just nN
                wholes (SpecificState _ _) = Nothing

specificStateParse :: Parser BiasOrder
specificStateParse = (lexeme . try) 
    (parens (SpecificState <$> variable <*> (comma >> integer))
    )

-- Parse a Colour. 
metaColor :: Parser LocalColor
metaColor = (lexeme . try) (rword "NodeColor" >> colon >>
    (try rgbColorParse <|> svgColor)
    )


-- Parse an RGB hex of the form: #123456
rgbColorParse :: Parser LocalColor
rgbColorParse = do
    _ <- hash
    red   <- (count 2 hexDigitChar >>= hexInt)
    green <- (count 2 hexDigitChar >>= hexInt)
    blue  <- (count 2 hexDigitChar >>= hexInt)
    return $ SC.sRGB24 red green blue

hexInt :: String -> Parser Word8
hexInt xs = case readHex xs of
    [(i, "")] -> return i
    _ -> fail $ xs <> " is not a hexadecimal literal"

svgColor :: Parser LocalColor
svgColor = some letterChar >>= colorCheck

colorCheck :: String -> Parser LocalColor
colorCheck ts
    | (T.pack lowered) `elem` svgColors = C.readColourName lowered
    | ts == "" = return defaultColor
    | otherwise = fail $ show $ ts <> " is not an SVG color. "
        where lowered = toLower <$> ts

modelLayerCheck :: ModelLayer -> Parser ModelLayer
modelLayerCheck mL = case mkModelLayer mL of
    Success theML -> return theML
    Failure errs    -> fail $ show errs

-- Parse a straight Text metadata item. 
metaItem :: T.Text -> Parser T.Text
metaItem aMeta = (lexeme . try) (rword aMeta >> hcolon >>
    (T.pack <$> manyTill (anySingle <?> "metaItem") eol))

-- Parser a node
nodeParse :: Parser (DMNode, [(NodeName, DMLink)])
nodeParse = (between (symbol "Node{") (symbol "Node}") (runPermutation $
             (,) <$> (DMNode <$> toPermutation nodeConfigParse
                             <*> toPermutation gateParse) 
                 <*> toPermutation (some dMLinkParse)))
              >>= linkDupeCheck
              >>= gateLinkCheck 
              >>= nodeNameCheck
              >>= inLinkOrderPrep

-- Make sure that no two InLinks in a Node have the same InputNode
linkDupeCheck :: (DMNode, [(NodeName, DMLink)])
              -> Parser (DMNode, [(NodeName, DMLink)])
linkDupeCheck p@(_, ls)
    | nDupes == [] = return p
    | otherwise = fail $ show $ DuplicateDMLinks nDupes
    where nDupes = repeated $ fst <$> ls

-- Make sure that the InLinks in a Node match up with the input nodes in the 
-- NodeGate. 
gateLinkCheck :: (DMNode, [(NodeName, DMLink)])
              -> Parser (DMNode, [(NodeName, DMLink)])
gateLinkCheck parsed@((DMNode _ pNode), links) = 
    let linkInputs = L.sort $ fst <$> links
        gateInputs = L.sort $ gateOrder pNode
    in
    case gateInputs == linkInputs of
        True  -> return parsed
        False -> fail $ show $ GateInLinkMismatch $ linkInputs \|\ gateInputs

-- Make sure that the NodeName from the parsed NodeGate is the same as the
-- NodeName from the parsed NodeMetaData
nodeNameCheck :: (DMNode, [(NodeName, DMLink)])
              -> Parser (DMNode, [(NodeName, DMLink)])
nodeNameCheck parsed@(pDMNode, _) = 
    let gName = (gNodeName . nodeGate) pDMNode
        mName = (nodeName . nodeMeta) pDMNode
    in
    case gName == mName of
        True  -> return parsed
        False -> fail $ show $ NodeMetaNameMismatch (gName, mName)

inLinkOrderPrep :: (DMNode, [(NodeName, DMLink)])
                -> Parser (DMNode, [(NodeName, DMLink)])
inLinkOrderPrep (dmN, ils) = return $
    (dmN {nodeMeta = (nodeMeta dmN) {inlinkOrder = fst <$> ils}}, ils)

-- Parse a Note or Description, along with any embedded BibTeX citations. 
extractCitations :: T.Text -> Parser (T.Text, [[T.Text]])
extractCitations aMeta = do
    lText <- lookAhead (metaItem aMeta)
    citations <- pullCitations aMeta
    return (lText, citations)

-- Return a (possibly empty) list of citation key lists. 
pullCitations :: T.Text -> Parser [[T.Text]]
pullCitations aMeta = (hlexeme (rword aMeta >> hcolon >>
    many (try (skipManyTill (noneOf ['\n', '\r']) citationParse)) <*
        (skipManyTill ((noneOf ['\n', '\r']) <?> "pullCitations") eol )))
            >>= check
    where
        check x = case kwSet == Set.empty of
            True  -> return x 
            False -> case Set.size kwSet of
                1 -> fail $ "Keyword " ++ (show $ head $ Set.toList kwSet) ++
                    " cannot be a citation key. "
                _ -> fail $ "Keywords " ++ (show $ Set.toList kwSet) ++
                    " cannot be a citation keys. "
            where
                kwSet = Set.filter (\y -> elem y rws) kSet
                kSet = Set.unions $ Set.fromList <$> x

-- Parse the keys of a LaTeX "\cite{}" command. 
citationParse :: Parser [T.Text]
citationParse = (between (hsymbol "\\cite{") (hsymbol "}")
    (sepBy1 citeKeyParse' comma) )

-- Parse Node metadata
nodeConfigParse :: Parser NodeMeta
nodeConfigParse = between (symbol "NodeMetaData{") (symbol "NodeMetaData}") 
    (runPermutation $ 
        NodeMeta <$> toPermutation (identifier "NodeName")
                 <*> toPermutation nodeGenesParse
                 <*> toPermutation nodeTypeParse
                 <*> toPermutation metaColor
                 <*> toPermutation coordinateParse
                 <*> (doubleUncurry LitInfo
                      <$> toPermutation (extractCitations "NodeDescription")
                      <*> toPermutation (extractCitations "NodeNotes")
                      )
-- We dont know what the inLink order is at this point in the parse, so we start
-- with an empty list and insert the Inlink NodesNames after we validate the
-- rest of the DMNode. 
                 <*> pure []
    )


-- Parse the (possibly empty) list of EntrezID genes associated with a node
nodeGenesParse :: Parser [EntrezGeneID]
nodeGenesParse = lexeme $ rword "NodeGenes" >> colon >>
    (sepBy integer comma)

-- Parse the type of a node. 
nodeTypeParse :: Parser NodeType
nodeTypeParse = lexeme $ rword "NodeType" >> 
    (try
        (colon >>
            (   Cell               <$ rword "Cell"
            <|> DM_Switch          <$ rword "DM_Switch"
            <|> Connector          <$ rword "Connector"
            <|> Environment        <$ rword "Environment"
            <|> Process            <$ rword "Process"
            <|> Macro_Structure    <$ rword "Macro_Structure"
            <|> Metabolite         <$ rword "Metabolite"
            <|> MRNA               <$ rword "MRNA"
            <|> MicroRNA           <$ rword "MicroRNA"
--          This must come before Protein, otherwise you will match on
--          that and then get confused
            <|> Protein_Complex    <$ rword "Protein_Complex"
            <|> Receptor           <$ rword "Receptor"
            <|> Adaptor_Protein    <$ rword "Adaptor_Protein"
            <|> Secreted_Protein   <$ rword "Secreted_Protein"
            <|> TF_Protein         <$ rword "TF_Protein"
            <|> Kinase             <$ rword "Kinase"
            <|> Phosphatase        <$ rword "Phosphatase"
            <|> Ubiquitin_Ligase   <$ rword "Ubiquitin_Ligase"
            <|> Protease           <$ rword "Protease"
            <|> DNase              <$ rword "DNase"
            <|> CAM                <$ rword "CAM"
            <|> CDK                <$ rword "CDK"
            <|> CDKI               <$ rword "CDKI"
            <|> GEF                <$ rword "GEF"
            <|> GAP                <$ rword "GAP"
            <|> GTPase             <$ rword "GTPase"
            <|> Enzyme             <$ rword "Enzyme"
            <|> Protein            <$ rword "Protein"
            <|> Membrane_Potential <$ rword "Membrane_Potential"
            <|> LncRNA             <$ rword "LncRNA"
            <|> Cell_Surgace_Ligand
                                   <$ rword "Cell_Surgace_Ligand"
            )
        ) 
    <|>
        (hcolon >>
            Undefined_NT <$ undefParse
        )
    )

-- a NodeType or LinkType may be undefined at this point. 
undefParse :: Parser T.Text
undefParse = T.pack <$> (someTill (alphaNumChar <|> char '_') eol)

-- a LinkEffect may be undefined or empty at this point. 
undefParseNone :: Parser T.Text
undefParseNone = T.pack <$> (manyTill (alphaNumChar <|> char '_') eol)

-- Parse the graphical position of a node. 
coordinateParse :: Parser (U.Vector Double)
coordinateParse = (lexeme . try) $ rword "NodeCoordinate" >> colon >>
    ((U.fromList .  fmap toRealFloat) <$> sepBy signedNumber comma)
        
-- Parse a DMLink
dMLinkParse :: Parser (NodeName, DMLink)
dMLinkParse = between (symbol "InLink{") (symbol "InLink}") (runPermutation $
  (,) <$> toPermutation (identifier "InputNode")
      <*> (DMLink <$> toPermutation linkEffectParse
                  <*> toPermutation (linkTypeParse <?> "LinkType")
                  <*> (doubleUncurry LitInfo 
                           <$> toPermutation (extractCitations"LinkDescription")
                           <*> toPermutation (extractCitations "LinkNotes"))))

-- Parse the effect of a link. 
linkEffectParse :: Parser LinkEffect
linkEffectParse = lexeme $ rword "LinkEffect" >>
    (try
        (colon >>
            (   Activation        <$ rword "Activation"
            <|> Repression        <$ rword "Repression"
            <|> Context_Dependent <$ rword "Context_Dependent"
            <|> Inapt             <$ rword "Inapt"
            )
        )
    <|>
        (hcolon >>
            Undefined_LE <$ undefParseNone
        )
    )

-- Parse the type of a link. 
linkTypeParse :: Parser LinkType
linkTypeParse = lexeme $ rword "LinkType" >>
    (try   
        (colon >>
            (   Enforced_Env         <$ rword "Enforced_Env"
            <|> Indirect             <$ rword "Indirect"
            <|> Complex_Process      <$ rword "Complex_Process"
            <|> Persistence          <$ rword "Persistence"
            <|> Transcription        <$ rword "Transcription"
            <|> Translation          <$ rword "Translation"
            <|> Ligand_Binding       <$ rword "Ligand_Binding"
            <|> Complex_Formation    <$ rword "Complex_Formation"
            <|> Inhibitory_Binding   <$ rword "Inhibitory_Binding"
            <|> Localization         <$ rword "Localization"
            <|> Binding_Localization <$ rword "Binding_Localization"
            <|> Protective_Binding   <$ rword "Protective_Binding"
            <|> Unbinding            <$ rword "Unbinding"
--          This must come before Phosphorylation, otherwise you will match on
--          that and then get confused. This is out of order from the Datatype,
--          but hopefully we won't need to edit too deep in this list. 
            <|> Phosphorylation_Localization
                                     <$ rword "Phosphorylation_Localization"
            <|> Phosphorylation      <$ rword "Phosphorylation"
            <|> Dephosphorylation    <$ rword "Dephosphorylation"
            <|> Ubiquitination       <$ rword "Ubiquitination"
            <|> Degradation          <$ rword "Degradation"
            <|> GEF_Activity         <$ rword "GEF_Activity"
            <|> GAP_Activity         <$ rword "GAP_Activity"
            <|> Proteolysis          <$ rword "Proteolysis"
            <|> Catalysis            <$ rword "Catalysis"
            <|> Epigenetic           <$ rword "Epigenetic"
            <|> Transcription_Conflict
                                     <$ rword "Transcription_Conflict"
            <|> Secretion            <$ rword "Secretion"
            <|> RNAi                 <$ rword "RNAi"
            <|> Acetylation          <$ rword "Acetylation"
            <|> Deacetylation        <$ rword "Deacetylation"
            <|> Hydroxylation        <$ rword "Hydroxylation"
            )
        )
    <|>
        (hcolon >>
            Undefined_LT <$ undefParse
        )
    )

-- Parse a NodeGate
gateParse :: Parser NodeGate
gateParse = gatePairParse >>= tableDisCheck

-- If we parse a logical expression AND a table, make sure they give the same
-- output for all possible inputs. Also, make sure that the table & discrete
-- have the same gNodeName. 
tableDisCheck :: (Maybe LogicalGate, Maybe TruthTableGate) -> Parser NodeGate
tableDisCheck (Just (nName, gOrder, assigns), Nothing) = return $ NodeGate
    nName gOrder assigns (assignsToTTable gOrder assigns) LogicalExpression
tableDisCheck (Nothing, Just (nName, gOrder, tTable)) = return $ NodeGate
    nName gOrder (tTableToAssigns nName gOrder tTable) tTable DMMSTruthTable
tableDisCheck (Just lG, Just tG) = case gateOrdCheck lG tG of
    Failure err -> fail $ show err
    Success (lGate@(lName, _, assigns), tGate@(tName, tOrder, tTable))
      -> case L.isSubsequenceOf tSortedtGorderedCLCombosList
                                tSortedtGorderedCTCombosList
         of
        False -> fail $ T.unpack $ "TableExprStateMismatch \n" <> T.unlines
            (tGatePrint:prettyCombos)
        True -> case accOutputMis tGate lGate of
            errs@(_:_) -> fail $ (T.unpack . T.replace (T.singleton '\t') "  ")
                                $ "TableExprOutputMismatch (Table, Logical): \n"
                                    <> T.unlines (tGatePrint:errs)
            [] | lName == tName -> return nGate
               | otherwise -> fail $ show $ TableDisNameMismatch (tName, lName)
        where            
            nGate = NodeGate lName tOrder assigns tTable Both
            prettyCombos :: [T.Text]
            prettyCombos = (T.concat . (L.intersperse "  ") . fmap showt) <$>
                tSortedtGOrderedELCombos
            tSortedtGOrderedELCombos = L.sort $ (snd <$>) <$> tGOrderedELCombos
            tGOrderedELCombos = (sortWithOrderOn fst tOrder) <$> excessLCombos
            excessLCombos = Map.toList <$> (lCombos L.\\ tCombos )
            tCombos = fst $ unzip $ tTInputOutput tOrder tTable
            lCombos = gateCombinations $ snd <$> assigns
            tGatePrint = (T.concat $ L.intersperse "  " $ tOrder <> [tName])
            lCombosList = Map.toList <$> lCombos
            tGorderedLCombosList = (sortWithOrderOn fst tOrder) <$> lCombosList
            tSortedtGorderedCLCombosList =
                L.sortOn (snd <$>) tGorderedLCombosList
            tCombosList = Map.toList <$> tCombos
            tGorderedTCombosList = (sortWithOrderOn fst tOrder) <$> tCombosList
            tSortedtGorderedCTCombosList =
                L.sortOn (snd <$>) tGorderedTCombosList
tableDisCheck (Nothing, Nothing) = fail "Expecting a NodeGate"

-- We want to accumulate from the TruthTable and NodeGate those rows whose
-- outputs to that input are different. 
accOutputMis :: TruthTableGate -> LogicalGate -> [T.Text]
accOutputMis (_, tOrder, tTable) (_, _, assigns) = textified
    where
        textified = textify <$> mismatches
        textify (t, (x, y)) = t <> "\t(" <> showt x <> ",  "
            <> showt y <> ")"
        mismatches = filter (\(_, (x, y)) -> x /= y) $ zip inputRowTexts outputs
        inputRowTexts = (T.intersperse '\t' . T.concat . fmap showt)
            <$> inputLists
        outputs = zip tOutputs lOutputs
        lOutputs = fromJust . gateEval assigns <$> tCombos
        tCombos = (Map.fromList . (zip tOrder)) <$> inputLists
        inputLists = U.toList <$> vecs
        (vecs, tOutputs) = unzip $ (L.sortOn fst . Map.toList) tTable

-- Check to make sure that the gateOrders of the parsed pairs of gates are
-- identical up to permutation. If they are, return them. 
-- If not, error out and return the two NodeName Lists. 
gateOrdCheck :: LogicalGate 
             -> TruthTableGate 
             -> Validation GateInvalid (LogicalGate, TruthTableGate)
gateOrdCheck lG@(_, lOrder, _) tG@(_, tOrder, _) =
  case arePermutes lOrder tOrder of
    False -> Failure $ TableExprInNodeMismatch $ tOrder \|\ lOrder
    True  -> Success $ (lG, tG)

-- Parse a NodeGate into a pair, where the first is the gate as parsed from a
-- discrete logical expression, if that exists in the dmms file, and the second
-- as parsed from a truth table, if that exists in the dmms file. If they both
-- exist, they will be compared after parsing to ensure consistency. 
gatePairParse :: Parser (Maybe LogicalGate, Maybe TruthTableGate)
gatePairParse = between (symbol "NodeGate{") (symbol "NodeGate}")
    (runPermutation $ (,)
        <$> toPermutationWithDefault Nothing (Just <$> parseDiscreteLogic)
        <*> toPermutationWithDefault Nothing (Just <$> truthTableParse))

parseDiscreteLogic :: Parser LogicalGate
parseDiscreteLogic = between (symbol "DiscreteLogic{") (symbol "DiscreteLogic}")
    ((try pInt <|> pBin) >>= gateConsistencyCheck)
        where pInt = some parseNodeStateAssign
            -- A boolean gate will by definition have 1 assignment. 
              pBin = (:[]) <$> parseBoolNodeStateAssign

-- Sanity check on gate definitions. Return a NodeGate on success. 
-- Return a useful error message on failure. Use mkLogicalGate. 
gateConsistencyCheck :: [(NodeName, NodeStateAssign)]
                     -> Parser LogicalGate
gateConsistencyCheck nPairs = case mkLogicalGate nPairs of
    Success gTriplet  -> return gTriplet
    Failure errs      -> fail $ "Error(s) in gate assignment: " <> show errs


-- This is the case where the gene is modeled as having more than 2 states.
parseNodeStateAssign :: Parser (NodeName, NodeStateAssign)
parseNodeStateAssign = do
    var <- variable
    void colon
    varState <- integer
    void stateAssign
    expr <- parseNodeExpr
    return (var, (varState, expr))

-- This is case where the gene is modeled as a Boolean. 
parseBoolNodeStateAssign :: Parser (NodeName, NodeStateAssign)
parseBoolNodeStateAssign = do
    var <- variable
    void stateAssign
    expr <- parseNodeExpr
    return (var, (1, expr))

-- Parse the logical expressions that assign gate states. 
parseNodeExpr :: Parser NodeExpr
parseNodeExpr = makeExprParser nTerm nOperators

nOperators :: [[Operator Parser NodeExpr]]
nOperators = 
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (Binary And <$ rword "and")
    , InfixL (Binary Or <$ rword "or")]
  ]

-- Parse out a Pars term, node name (w or w/o state), or a Bool literal from a
-- larger NodeExpr. 
nTerm :: Parser NodeExpr
nTerm = parens (Pars <$> parseNodeExpr)
    <|> (try integerNTerm)
    <|> (try booleanNTerm)
    <|> literalTerm

-- Parse a node name without an :Int at then end of the node name
booleanNTerm :: Parser NodeExpr
booleanNTerm = (flip GateConst 1) <$> variable

-- Parse a node name with an :Int at then end of the node name
integerNTerm:: Parser NodeExpr
integerNTerm = do 
    var <- variable
    void colon
    varState <- integer
    return (GateConst var varState)

literalTerm :: Parser NodeExpr
literalTerm = rword "GateLit" >> (GateLit <$> boolParse)

boolParse :: Parser Bool
boolParse = (try trueParse) <|> falseParse

trueParse :: Parser Bool
trueParse = True <$ rword "True"

falseParse :: Parser Bool
falseParse = False <$ rword "False"

truthTableParse :: Parser TruthTableGate
truthTableParse = between (symbol "TruthTable{")
                          (symbol "TruthTable}")
                           slurpTable
                  >>= tableConsistencyCheck

-- This checks for the following:
-- 1. Are all possible output states reached
-- 2. Are all possible input combinations listed?
-- 3. Exactly once?
-- 4. In strictly increasing order?
tableConsistencyCheck :: ([NodeName],[[NodeState]]) -> Parser TruthTableGate
tableConsistencyCheck (nodes, rows) = case mkTableGate (nodes, rows) of
    Success truthTable -> return truthTable
    Failure errs      -> fail $ show errs
            

slurpTable :: Parser ([NodeName],[[NodeState]])
slurpTable = do
    nodes <- some variable
    let n  = length nodes
    rows  <- some (parseTableRow n <?> show IncompleteOrOversizedRow)
    return (nodes, rows)


-- This parses a non-node row in a Truth Table, by explicitly paying attention
-- to spaces, tabs, and eol characters
parseTableRow :: Int -> Parser [NodeState]
parseTableRow n = do
    inputs <- count (n - 1) (intParse <?> show IncompleteOrOversizedRow)
    output <- LE.decimal
    skipMany (symbol " " <|> (T.singleton <$> tab))
    _ <- eol
    return (inputs ++ [output])

-- This is for the very specific case where, when parsing Truth Tables, I need
-- to parse EOL characters differently than other whitespace, so that I catch if
-- any individual row is incomplete or too long. 
intParse :: Parser Int
intParse = do
    n <- LE.decimal
    skipSome (symbol " " <|> (T.singleton <$> tab))
    return n

-- BibTeX is an old and layered format, but these parsers should get most of
-- what might be thrown at them. See http://www.bibtex.org/Format/ for details.
citeDictParse :: Parser CitationDictionary
citeDictParse = between
    (symbol "CitationDictionary{")
    (symbol "CitationDictionary}")
    (Map.fromList <$> ( (\es -> zip (entryKey <$> es) es) <$>
                        ((many citeEntryParse) >>= citeUniqueCheck)
                       )
    )

citeUniqueCheck :: [BibTeXEntry] -> Parser [BibTeXEntry]
citeUniqueCheck bes = case repeated (entryKey <$> bes) of
    []          -> return bes
    errs -> fail $ show $ RepeatedKeys errs

citeEntryParse :: Parser BibTeXEntry
citeEntryParse = do
    _ <- char '@'
    eType <- entryTypeParse
    _ <- symbol "{"
    eKey <- citeKeyParse
    _ <- comma
    fields <- some citeFieldParse 
    _ <- symbol "}"
    return $ BibTeXEntry eKey eType fields

entryTypeParse :: Parser T.Text
entryTypeParse = (lexeme . try) (p >>= check)
  where
    p       = T.pack <$> ((:) <$> letterChar <*> 
        (many (alphaNumChar <|> char '_')))
    check x = if x `elem` rws
                then fail $ "Keyword " ++ show x ++ " cannot be a BibTeX record\
                    \type. "
                else return x

citeKeyParse :: Parser BibTeXKey
citeKeyParse = hlexeme (p >>= check)
  where
    p       = T.pack <$> ((:) <$> (letterChar <|> char '_') <*> 
                    (many (alphaNumChar
                           <|> (oneOf ("&;:-_.?+/" :: [Char]) <?> "citeKey")
                           )
                    )
              )
    check x = if x `elem` rws
                then fail $ 
                    "Keyword " ++ show x ++ " cannot be a citation key. "
                else return x

-- When we parse citations out of a Description or Note, the error reporting
-- needs to be in pullCitations, or else the skipManyTill in that function will
-- clobber and error I report here. Thus we need a version of citeKeyParse that
-- doesn't check for Keywords. 
citeKeyParse' :: Parser BibTeXKey
citeKeyParse' = (lexeme . try) (T.pack <$> ((:) <$> (letterChar <|> char '_')
    <*> (many (alphaNumChar <|> (oneOf ("&;:-_.?+/" :: [Char])
        <?> "citeKey")))))



-- Parse a line in a BibTeX record
citeFieldParse :: Parser (BibTeXField, BibTeXRecord)
citeFieldParse = lexeme $ do
    field <- bibVariable
    _ <- symbol "="
    record <- bibRecordParse
    return (field, record)

-- Parse a BibTeX assignment variable. 
bibVariable :: Parser BibTeXField
bibVariable = lexeme (p >>= check)
  where
    p       = T.pack <$> ((:) <$> (letterChar <|> char '_') <*> 
        (many (alphaNumChar
              <|> (oneOf ("&;:-_.?+/" :: [Char]) <?> "bibVariable"))))
    check x = if x `elem` rws
                then fail $
                    "Keyword " ++ show x ++ " cannot be a BibTeX field name. "
                else return x

bibRecordParse :: Parser BibTeXRecord
bibRecordParse = ((lexeme . try) $ 
    T.pack <$> (someTill (anySingle <?> "bibRecordParse") eol)) >>= commaCheck

commaCheck :: BibTeXRecord -> Parser BibTeXRecord
commaCheck r = case T.last r of
    ',' -> return $ T.init r
    _   -> return r

-- These need to go here because they need both Types.DMModel and
-- Types.Simulation.
-- Checks on internal ModelLayer issues. 
mkModelLayer :: ModelLayer -> Validation [ModelInvalid] ModelLayer
mkModelLayer mL = case biasNodeNameCheck mL of
    Success bnnML -> case biasNodeStateCheck bnnML of
        Success bnsML -> Success bnsML
        Failure bnsErrs -> Failure bnsErrs
    Failure bnnErrs -> Failure bnnErrs

-- Do the BiasOrders have unknown NodeNames?
biasNodeNameCheck :: ModelLayer -> Validation [ModelInvalid] ModelLayer
biasNodeNameCheck mL = case filter (flip notElem lNames) bOFNames of
    [] -> case filter (flip notElem lNames) bOLNames of
        [] -> Success mL
        ms -> Failure $ [UnknownNodesInBiasOrder ms]
    ns -> Failure $ [UnknownNodesInBiasOrder ns]
    where
        bOFNames = biasOrderNodeName <$> bOF
        bOLNames = biasOrderNodeName <$> bOL
        bOF = (biasOrderFirst . modelMeta) mL
        bOL = (biasOrderLast . modelMeta) mL
        lNames = (fmap (nodeName . nodeMeta) . layerNodes) mL

-- Do any SpecificState BiasOrders contain any out-of-bounds states?
biasNodeStateCheck :: ModelLayer -> Validation [ModelInvalid] ModelLayer
biasNodeStateCheck mL = case mapMaybe checkState bOF of
    [] -> case mapMaybe checkState bOL of
        [] -> Success mL
        ms -> Failure $ [OOBBiasOrderStates ms]
    ns -> Failure $ [OOBBiasOrderStates ns]
    where
        checkState (WholeNode _) = Nothing
        checkState (SpecificState n i)
            | inRange nRange i = Nothing
            | otherwise = Just (n, nRange, i)
            where nRange = (lrVec U.! (lniBMap BM.! n))
        bOF = (biasOrderFirst . modelMeta) mL
        bOL = (biasOrderLast . modelMeta) mL
        S.LayerSpecs lniBMap lrVec _ _ = S.layerPrep mL

