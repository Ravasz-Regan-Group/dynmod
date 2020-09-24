{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parse.DMMS
    ( modelFileParse ) 
      where

import Types.DMModel
import Constants
import Utilities
import qualified Data.Text as T
import qualified Data.Versions as Ver
import qualified Data.Colour.Names as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Graph.Inductive as Gr
import qualified Data.Graph.Inductive.NodeMap as Grm
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Hashable as Hash
import Control.Monad.Combinators.Expr   -- from parser-combinators
import Control.Applicative.Permutations -- from parser-combinators
import Data.Validation
import Data.List.Unique (repeated)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Bifunctor as B
import Data.Void
import Data.Char (isSeparator)
import Control.Monad (void)

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Space handling that excludes eol
space1' :: (MonadParsec e s m, Token s ~ Char) => m ()
space1' = void $ takeWhile1P (Just "white space no eol") 
    (\c -> isSeparator c || (c == '\t'))

sc' :: Parser ()
sc' = L.space space1' lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme sc'

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

symbol' :: T.Text -> Parser T.Text
symbol' = L.symbol sc'

-- | 'braces' parses something between parenthesis.

-- braces :: Parser a -> Parser a
-- braces = between (symbol "(") (symbol ")")

-- braces' :: Parser a -> Parser a
-- braces' = between (symbol' "(") (symbol' ")")

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parens' :: Parser a -> Parser a
parens' = between (symbol' "(") (symbol' ")")

-- | 'number' parses an number in scientific format.

number :: Parser Scientific
number = lexeme L.scientific

-- | 'integer' parses an Int. (Note that this does not parse signs!)

integer :: Parser Int
integer = lexeme L.decimal


-- | 'colon' parses a colon.

colon :: Parser T.Text
colon = symbol ":"

colon' :: Parser T.Text
colon' = symbol' ":"

-- | 'stateAssign' parses a node state assignment operator.

stateAssign :: Parser T.Text
stateAssign = symbol "*="

-- | 'comma' parses a comma.

comma :: Parser T.Text
comma = symbol ","

comma' :: Parser T.Text
comma' = symbol' ","

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
                then fail $ "Keyword " ++ show x ++ " cannot be a nodeName. "
                else return x

-- Parse a dmms file. 
modelFileParse :: Parser (FileFormatVersion, (DMModel, CitationDictionary))
modelFileParse = (,) <$> dmmsVersion <*> modelCiteParse <* eof

-- Parse the dmms file format version. 
dmmsVersion :: Parser FileFormatVersion
dmmsVersion = versionParse "FormatVersion"

-- Parse a DMModel and CitationDictionary. 
modelCiteParse :: Parser (DMModel, CitationDictionary)
modelCiteParse = ((,) <$> modelParse <*> citeDictParse)

modelParse :: Parser DMModel
modelParse = between (symbol "Model{") (symbol "Model}") 
    (
        (try 
            ((runPermutation $
                (,,)
                    <$> toPermutation modelMappingParse
                    <*> (ModelLayer <$> toPermutation modelGraphParse
                                    <*> toPermutation modelConfigParse)
                    <*> toPermutation modelParse
             )
                >>= modelMappingCheck
            )
        )
        <|> (Fine <$> modelLayerParse)
    ) >>= nodeDupeCheck >>= nodeDifferentiate

-- Return a DMModel whose Nodes are unique in all layers. This is useful when
-- doing anything related to the ModelMappings, since by definition they work
-- with Gr.LNodes from different Gr.Gr DMNode DMLink graphs. 
nodeDifferentiate :: DMModel -> Parser DMModel
nodeDifferentiate dmm@(Fine _) = return dmm
nodeDifferentiate (LayerBinding mm ml dm) = return $ go mm ml dm 0
    where
        go mapping layer (LayerBinding mm' ml' dm') offSet =
          LayerBinding mapping shiftedLayer $ go mm' ml' dm' newOffset
            where
                newOffset = 1 + ((maximum . Gr.nodes) shiftedGraph)
                shiftedLayer = ModelLayer shiftedGraph meta
                shiftedGraph = indexShift offSet graph
                graph = modelGraph layer
                meta = modelMeta layer
        go mapping layer (Fine ml') offSet =
          LayerBinding mapping shiftedCoarseL $ Fine shiftedFineL
            where
                shiftedFineL = ModelLayer shiftedFineG fMeta
                shiftedFineG = indexShift fineOffset fGraph
                fGraph = modelGraph ml'
                fMeta = modelMeta ml'
                fineOffset = 1 + ((maximum . Gr.nodes) shiftedCoarseG)
                shiftedCoarseL = ModelLayer shiftedCoarseG cMeta
                shiftedCoarseG = indexShift offSet cGraph
                cGraph = modelGraph layer
                cMeta = modelMeta layer

indexShift :: Int -> Gr.Gr a b -> Gr.Gr a b
indexShift offSet g = Gr.mkGraph sNodes sEdges
    where
        sEdges = ((\i (x, y, z) -> (x + i, y + i, z)) offSet) <$> edges
        sNodes = (B.first (+ offSet)) <$> nodes
        edges  = Gr.labEdges g
        nodes = Gr.labNodes g


-- Check that ALL NodeNames in the entire parsed DMModel are unique
nodeDupeCheck :: DMModel -> Parser DMModel
nodeDupeCheck dM
    | repeats == [] = return dM
    | otherwise = fail $ show $ DuplicatedNodeNames repeats
    where
        repeats = repeated ns
        ns = (nodeName . nodeMeta) <$> (concatMap layerNodes layers)
        layers = modelLayers dM  

modelMappingCheck :: (ModelMapping, ModelLayer, DMModel) -> Parser DMModel
modelMappingCheck (mMap, mLayer, mModel) =
    case mkLayerBinding mMap mLayer mModel of
        Success dmModel -> return dmModel
        Failure errs    -> fail $ show errs

-- Parse the mapping from one layer of a model to the next. 
modelMappingParse :: Parser ModelMapping
modelMappingParse = between (symbol "ModelMapping{") (symbol "ModelMapping}") $
    some ((,) <$> identifier "Switch" <*> parens 
           ((fmap T.pack) <$> sepBy1 (some $ alphaNumChar <|> char '_') comma))

modelLayerParse :: Parser ModelLayer
modelLayerParse = runPermutation $
    ModelLayer <$> toPermutation modelGraphParse
               <*> toPermutation modelConfigParse


-- Parse the internal network of a particular model layer. 
modelGraphParse :: Parser ModelGraph
modelGraphParse = between (symbol "ModelGraph{") (symbol "ModelGraph}") $
    some nodeParse
    >>= nodeStateCheck
    >>= nodeLinkCheck
    >>= modelGraphAssembler

-- Check that, for every node and state reference in every NodeExpr, that node
-- exists in the DMModel layer that we are in, and has a NodeStateAssign for
-- that state
nodeStateCheck :: [(DMNode, [(NodeName, DMLink)])]
               -> Parser [(DMNode, [(NodeName, DMLink)])]
nodeStateCheck nodesWLinks
    | (isSubset exNodes nodes) && (isSubset exprStates nodeStates) =
        return nodesWLinks
    | (not $ isSubset exNodes nodes) && (isSubset exprStates nodeStates) =
        fail $ show $ NodeRefdNodesMismatch (exNodes L.\\ nodes)
    | (isSubset exNodes nodes) && (not $ isSubset exprStates nodeStates) =
        fail $ show $
            StatesRefdStatesMisMatch (exprRefs L.\\ nodeRefs)
    | (not $ isSubset exNodes nodes) && (not $ isSubset exprStates nodeStates)
        = fail $ show $
            StatesRefdStatesMisMatch (exprRefs L.\\ nodeRefs)
    | otherwise = return nodesWLinks
        where
            dmNodes = fst <$> nodesWLinks
            nodeGates = nodeGate <$> dmNodes
            exprRefs = L.sort $ refdNodesStates $
                concatMap (fmap snd . gateAssigns) nodeGates
            exNodes = fst <$> exprRefs
            exprStates = (L.sort . snd) <$> exprRefs
            nodes = gNodeName <$> nodeGates
            assigns = gateAssigns <$> nodeGates
            nodeRefs = L.sort $ zip nodes $ (fst <$>) <$> assigns
            nodeStates = snd <$> nodeRefs

-- Check that for the NodeName associated with each DMLink there exists an
-- actual DMNode with that NodeName
nodeLinkCheck :: [(DMNode, [(NodeName, DMLink)])]
               -> Parser [(DMNode, [(NodeName, DMLink)])]
nodeLinkCheck nodesWLinks = case lSet == nSet of
    True  -> return nodesWLinks
    False -> fail $ show $ err
    where
      nSet = Set.fromList dmNodeNames
      lSet = Set.fromList dmNodeNames
      dmNodeNames = (nodeName . nodeMeta . fst) <$> nodesWLinks
      err = NodeInlinkMismatch (nList, lList)
      nList = L.sort $ Set.toList nSet
      lList = L.sort $ Set.toList lSet

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

-- Parse Model metadata
modelConfigParse :: Parser ModelMeta
modelConfigParse = between (symbol "ModelMetaData{") (symbol "ModelMetaData}")
   (runPermutation $ 
      ModelMeta <$> toPermutation (identifier "ModelName")
                <*> toPermutation (versionParse "ModelVersion")
                <*> toPermutation modelPaperParse
                <*> toPermutation (biasPairParse "BiasOrderFirst")
                <*> toPermutation (biasPairParse "BiasOrderLast")
                <*> ((,) <$> toPermutation (extractCitations "ModelDescription")
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
    _ <- colon'
    paperKeys <- sepBy citeKeyParse comma
    _ <- eol
    return paperKeys

-- Parse a list of (NodeName, NodeState) for BiasOrder*
biasPairParse :: T.Text -> Parser [(NodeName, NodeState)]
biasPairParse bias = (lexeme . try) 
    (rword bias
        >> colon'
        >> sepBy (parens' ((,) <$> variable <*> (comma >> integer))) comma'
        <* eol
    )

-- Parse a Colour. 
metaColor :: T.Text -> Parser LocalColor
metaColor mColor = (metaItem mColor) >>= colorCheck

colorCheck :: T.Text -> Parser LocalColor
colorCheck ts
    | lowered `elem` svgColors = C.readColourName (T.unpack lowered)
    | ts == "" = return defaultColor
    | otherwise = fail $ show $ ts <> " is not an SVG color. "
        where lowered = T.toLower ts

-- Check if a Text is the name of an SVG color, and return it if it is. 
-- colorIdentifier :: Parser T.Text
-- colorIdentifier = lexeme (c >>= check)
--     where
--         c       = T.pack <$> (some letterChar)
--         check x = case lowered `elem` svgColors of
--                     True  -> return lowered
--                     False -> fail $ show $ x <> " is not an SVG color. "
--                     where lowered = T.toLower x

-- Parse a straight Text metadata item. 
metaItem :: T.Text -> Parser T.Text
metaItem aMeta = (lexeme . try) (rword aMeta >> colon' >>
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
    let assigns = gateAssigns pNode
        linkInputs = L.sort $ fst <$> links
        gExprs = snd <$> assigns
        gateInputs = L.sort $ fst <$> (refdNodesStates gExprs)
    in
    case gateInputs == linkInputs of
        True  -> return parsed
        False -> fail $ show $ GateInLinkMismatch (gateInputs, linkInputs)

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

-- Parse a Note or Description, along with any embedded BibTeX citations. 
extractCitations :: T.Text -> Parser (T.Text, [[T.Text]])
extractCitations aMeta = do
    lText <- lookAhead (metaItem aMeta)
    citations <- pullCitations aMeta
    return (lText, citations)

-- Return a (possibly empty) list of citation key lists. 
pullCitations :: T.Text -> Parser [[T.Text]]
pullCitations aMeta = (lexeme' (rword aMeta >> colon' >>
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
citationParse = (between (symbol "\\cite{") (symbol "}")
    (sepBy1 citeKeyParse' comma) )

-- Parse Node metadata
nodeConfigParse :: Parser NodeMeta
nodeConfigParse = between (symbol "NodeMetaData{") (symbol "NodeMetaData}") 
     (runPermutation $ 
        NodeMeta <$> toPermutation (identifier "NodeName")
                 <*> toPermutation nodeGenesParse
                 <*> toPermutation nodeTypeParse
                 <*> toPermutation (metaColor "NodeColor")
                 <*> toPermutation coordinateParse
                 <*> ((,) <$> toPermutation (extractCitations "NodeDescription")
                           <*> toPermutation (extractCitations "NodeNotes")))


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
            )
        ) 
    <|>
        (colon' >>
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
    ((U.fromList .  fmap toRealFloat) <$> sepBy number comma)
        
-- Parse a DMLink
dMLinkParse :: Parser (NodeName, DMLink)
dMLinkParse = between (symbol "InLink{") (symbol "InLink}") (runPermutation $
  (,) <$> toPermutation (identifier "InputNode")
      <*> (DMLink <$> toPermutation linkEffectParse
                  <*> toPermutation (linkTypeParse <?> "LinkType")
                  <*> ((,) <$> toPermutation (extractCitations"LinkDescription")
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
        (colon' >>
            Undefined_LE <$ undefParseNone
        )
    )

-- Parse the type of a link. 
linkTypeParse :: Parser LinkType
linkTypeParse = lexeme $ rword "LinkType" >>
    (try   
        (colon >>
            (   Enforced_Env        <$ rword "Enforced_Env"
            <|> Indirect            <$ rword "Indirect"
            <|> Complex_Process     <$ rword "Complex_Process"
            <|> Persistence         <$ rword "Persistence"
            <|> Transcription       <$ rword "Transcription"
            <|> Translation         <$ rword "Translation"
            <|> Ligand_Binding      <$ rword "Ligand_Binding"
            <|> Complex_Formation   <$ rword "Complex_Formation"
            <|> Inhibitory_Binding  <$ rword "Inhibitory_Binding"
            <|> Localization        <$ rword "Localization"
            <|> Binding_Localizaton <$ rword "Binding_Localizaton"
            <|> Protective_Binding  <$ rword "Protective_Binding"
            <|> Unbinding           <$ rword "Unbinding"
--          This must come before Phosphorylation, otherwise you will match on
--          that and then get confused. This is out of order from the Datatype,
--          but hopefully we won't need to edit too deep in this list. 
            <|> Phosphorylation_Localization
                                    <$ rword "Phosphorylation_Localization"
            <|> Phosphorylation     <$ rword "Phosphorylation"
            <|> Dephosphorylation   <$ rword "Dephosphorylation"
            <|> Ubiquitination      <$ rword "Ubiquitination"
            <|> Degradation         <$ rword "Degradation"
            <|> GEF_Activity        <$ rword "GEF_Activity"
            <|> GAP_Activity        <$ rword "GAP_Activity"
            <|> Proteolysis         <$ rword "Proteolysis"
            <|> Catalysis           <$ rword "Catalysis"
            )
        )
    <|>
        (colon' >>
            Undefined_LT <$ undefParse
        )
    )

-- Parse a NodeGate
gateParse :: Parser NodeGate
gateParse = gatePairParse >>= tableDisCheck

-- If we parse a logical expression AND a table, make sure they give the same
-- output for all possible inputs. Also, make sure that the table & discrete
-- have the same gNodeName. 
tableDisCheck :: (Maybe LogicalNodeGate, Maybe TableNodeGate) -> Parser NodeGate
tableDisCheck (Just lG, Nothing) = return lG
tableDisCheck (Nothing, Just tG) = return tG
tableDisCheck (Just lG, Just tG) = case gateOrdCheck lG tG of
    Failure err -> fail $ show err
    Success (lGate, tGate) -> case isSubset lpInputs tpInputs of
        False -> fail $ show $ TableExprStateMismatch $
                        T.unlines (tGatePrint:(deleteMult tpInputs lpInputs))
        True -> case accOutputMis tOutputs lOutputs of 
            errs@(_:_) -> fail $ (T.unpack . T.replace (T.singleton '\t') "  ")
                                $ "TableExprOutputMismatch (Table, Logical): \n"
                                    <> T.unlines (tGatePrint:errs)
            [] | lName == tName -> return lGate'
               | otherwise -> fail $ show $ TableDisNameMismatch (tNm, lName)
                where lName = gNodeName lGate
                      tNm = gNodeName tGate
        where
            tGatePrint = (T.concat $
                           L.intersperse "  " $ tOrder <> [tName])
            tName = gNodeName tGate
            tpInputs = T.init <$> tOutputs
            lpInputs = T.init <$> lOutputs
            tOutputs = L.sort (prettyGateEval tGate <$> tCombos)
--          Evaluate the logical gate with the table gate order and inputs, to
--          make the pretty representations match. This is OK, since we have
--          already made sure that the input nodes are the same up to
--          permutation, and that the logical inputs are a subset of the table
--          inputs. 
            lOutputs = L.sort (prettyGateEval lGate' <$> tCombos)
            tCombos = gateCombinations tExprs
--             lCombos = gateCombinations lExprs
            tExprs = snd <$> (gateAssigns tGate)
--             lExprs = snd <$> (gateAssigns lGate)
            lNodeName = gNodeName lGate
            lGateAssigns = gateAssigns lGate
            tOrder = gateOrder tGate
--          The logical gate, with the table node order
            lGate' = NodeGate lNodeName lGateAssigns tOrder
tableDisCheck (Nothing, Nothing) = fail "Expecting a NodeGate"

-- We want to accumulate from the two PrettyGateOutput lists those rows whose
-- outputs to that input are different. Call as: accOutputMis tOutput lOutput
accOutputMis :: [PrettyGateOutput] -> [PrettyGateOutput] -> [T.Text]
accOutputMis ts ls = L.foldl' go [] tSplits
    where
        go acc (key, state) = case L.lookup key lSplits of
              Nothing -> acc
              Just n  -> case n == state of
                  True  -> acc
                  False -> acc <> [key
                                    <> "("
                                    <> (T.singleton state)
                                    <> ", "
                                    <> (T.singleton n)
                                    <> ")"]
        tSplits = zip (T.init <$> ts) (T.last <$> ts)
        lSplits = zip (T.init <$> ls) (T.last <$> ls)


-- Check to make sure that the gateOrders of the parsed pairs of gates are
-- identical up to permutation. If they are, return them. 
-- If not, error out and return the two NodeName Lists. 
gateOrdCheck :: LogicalNodeGate 
             -> TableNodeGate 
             -> Validation GateInvalid (LogicalNodeGate, TableNodeGate)
gateOrdCheck lGate tGate = case arePermutes lOrder tOrder of
    False -> Failure $ TableExprInNodeMismatch (tOrder, lReOrder)
    True  -> Success $ (lGate, tGate)
    where
        lOrder = (gateOrder lGate)
        tOrder = (gateOrder tGate)
        lReOrder = sortWithOrder tOrder lOrder

-- Parse a NodeGate into a pair, where the first is the gate as parsed from a
-- discrete logical expression, if that exists in the dmms file, and the second
-- as parsed from a truth table, if that exists in the dmms file. If they both
-- exist, they will be compared after parsing to ensure consistency. 
gatePairParse :: Parser (Maybe LogicalNodeGate, Maybe TableNodeGate)
gatePairParse = between (symbol "NodeGate{") (symbol "NodeGate}")
    (runPermutation $ (,)
        <$> toPermutationWithDefault Nothing (Just <$> parseDiscreteLogic)
        <*> toPermutationWithDefault Nothing (Just <$> truthTableParse))
  
parseDiscreteLogic :: Parser LogicalNodeGate
parseDiscreteLogic = between (symbol "DiscreteLogic{") (symbol "DiscreteLogic}")
    ((try pInt <|> pBin) >>= gateConsistencyCheck)
        where pInt = some parseNodeStateAssign
              pBin = some parseBoolNodeStateAssign

-- Sanity check on gate definitions. Return a NodeGate on success. 
-- Return a useful error message on failure. Use mkLogicalGate. 
gateConsistencyCheck :: [(NodeName, NodeStateAssign)]
                     -> Parser LogicalNodeGate
gateConsistencyCheck nPairs = case mkLogicalGate nPairs of
    Success nGate -> return nGate
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

-- Parse out just a node name (w or w/o state) from a larger NodeExpr
nTerm :: Parser NodeExpr
nTerm = parens parseNodeExpr
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

truthTableParse :: Parser TableNodeGate
truthTableParse = between (symbol "TruthTable{")
                          (symbol "TruthTable}")
                           slurpTable
                  >>= tableConsistencyCheck



-- This checks for the following:
-- 1. Are all possible output states reached
-- 2. Are all possible input combinations listed?
-- 3. Exactly once?
-- 4. In strictly increasing order?
tableConsistencyCheck :: ([NodeName],[[NodeState]]) -> Parser TableNodeGate
tableConsistencyCheck (nodes, rows) = case mkTableGate (nodes, rows) of
    Success tableGate -> return tableGate
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
    output <- L.decimal
    skipMany (symbol " " <|> (T.singleton <$> tab))
    _ <- eol
    return (inputs ++ [output])

-- This is for the very specific case where, when parsing Truth Tables, I need
-- to parse EOL characters differently than other whitespace, so that I catch if
-- any individual row is incomplete or too long. 
intParse :: Parser Int
intParse = do
    n <- L.decimal
    skipSome (symbol " " <|> (T.singleton <$> tab))
    return n

-- BibTeX is an old and layered format, but these parsers should get most of
-- what might be thrown at them. See http://www.bibtex.org/Format/ for details.
citeDictParse :: Parser CitationDictionary
citeDictParse = between
    (symbol "CitationDictionary{")
    (symbol "CitationDictionary}")
    (Map.fromList <$> ( (\es -> zip (entryKey <$> es) es) <$>
                        ((some citeEntryParse) >>= citeUniqueCheck)
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
citeKeyParse = lexeme' (p >>= check)
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

-- Sort a list by the order of elements in the list order
sortWithOrder :: (Ord a, Hash.Hashable a) => [a] -> [a] -> [a]
sortWithOrder order = L.sortOn getOrder
    where
        getOrder k = Map.lookupDefault (-1) k orderHashMap
        orderHashMap = Map.fromList (zip order ([1..] :: [Int]))
