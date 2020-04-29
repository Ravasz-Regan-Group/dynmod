{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parsing
    ( modelFileParse ) 
      where

import Data.Colour
import qualified Data.Text as T
import qualified Data.Colour.Names as N
import qualified Data.Vector.Unboxed as U
import qualified Data.Graph.Inductive as Gr
import qualified Data.Graph.Inductive.NodeMap as Grm
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Hashable as Hash
import Control.Monad (void)
import Control.Monad.Combinators.Expr   -- from parser-combinators
import Control.Applicative.Permutations -- from parser-combinators
import Data.Validation
import Data.List.Unique (repeated)
import Data.List (transpose, sort, nub, sortOn, lookup, intersperse
                  , foldl')
import Data.Maybe (fromJust)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import Text.Megaparsec.Debug
import Data.Char (toLower, isSeparator)
import Data.Scientific
import qualified Data.Versions as Ver
import Types

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

-- Skip many Unicode separator characters, as well as tab, but not EOL. 
spaceNoEol :: Parser ()
spaceNoEol = skipMany (separatorChar <|> tab)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme sc'

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

symbol' :: T.Text -> Parser T.Text
symbol' = L.symbol sc'


-- | 'braces' parses something between braces.

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

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
identifier name = (lexeme . try) $ rword name >> colon >> (p >>= check)
  where
    p       = T.pack <$> ((:) <$> letterChar <*> 
        (many (alphaNumChar <|> char '_')))
    check x = if x `elem` rws
                then fail $ "Keyword " ++ show x ++ " cannot be a " ++ show name
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

-- Parse an rrms file. 
modelFileParse :: Parser (FileFormatVersion, (DMModel, CitationDictionary))
modelFileParse = (,) <$> rrmsVersion <*> modelCiteParse <* eof

-- Parse the rrms file format version. 
rrmsVersion :: Parser FileFormatVersion
rrmsVersion = versionParse "FormatVersion"

-- Parse a DMModel and CitationDictionary, and then make sure that the citations
-- in the former exist in the later. 
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
    )

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
   (some nodeParse) >>= nodeStateCheck >>= nodeLinkCheck >>= modelGraphAssembler

-- Check that, for every node and state reference in every NodeExpr, that node
-- exists in the DMModel layer that we are in, and has a NodeStateAssign for
-- that state
nodeStateCheck :: [(DMNode, [(NodeName, DMLink)])]
               -> Parser [(DMNode, [(NodeName, DMLink)])]
nodeStateCheck nodesWLinks
    | (isSubset exprNodes nodes) && (isSubset exprStates nodeStates) =
        return nodesWLinks
    | (not $ isSubset exprNodes nodes) && (isSubset exprStates nodeStates) =
        fail $ show $ NodeRefdNodesMismatch (exprNodes, nodes)
    | (isSubset exprNodes nodes) && (not $ isSubset exprStates nodeStates) =
        fail $ show $
            StatesRefdStatesMisMatch (zip exprNodes exprStates, nodeRefs)
    | (not $ isSubset exprNodes nodes) && (not $ isSubset exprStates nodeStates)
        = fail $ show $
            StatesRefdStatesMisMatch (zip exprNodes exprStates, nodeRefs)
        where
            dmNodes = fst <$> nodesWLinks
            extantNodeNames = (nodeName . nodeMeta) <$> dmNodes
            nodeGates = nodeGate <$> dmNodes
            exprRefs = sort $ refdNodesStates $
                concatMap (fmap snd . gateAssigns) nodeGates
            exprNodes = fst <$> exprRefs
            exprStates = (sort . snd) <$> exprRefs
            nodes = gNodeName <$> nodeGates
            assigns = gateAssigns <$> nodeGates
            nodeRefs = sort $ zip nodes $ (fst <$>) <$> assigns
            nodeStates = snd <$> nodeRefs

-- Check that the for the NodeName associated with each DMLink there exists an
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
      linkNodeNames = fst <$> (concat $ snd <$> nodesWLinks)
      err = NodeInlinkMismatch (nList, lList)
      nList = sort $ Set.toList nSet
      lList = sort $ Set.toList lSet

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
modelPaperParse = label "ModelPaper" (metaItem "ModelPaper") 
    >>= (\x -> return $ T.strip <$> (T.splitOn "," x))


-- Parse a list of (NodeName, NodeState) for BiasOrder*
biasPairParse :: T.Text -> Parser [(NodeName, NodeState)]
biasPairParse bias = (lexeme . try) 
    (rword bias
        >> colon'
        >> sepBy (parens' ((,) <$> variable <*> (comma >> integer))) comma'
        <* eol
    )

-- Parse a list of NodeNames for BiasOrder*
-- nodeNameListParse :: T.Text -> Parser [T.Text]
-- nodeNameListParse bias = (metaItem bias) 
--     >>= (\x -> return $ T.strip <$> (T.splitOn "," x))   

-- Parse a Colour. 
metaColor :: T.Text -> Parser LocalColor
metaColor mColor = (lexeme . try) $ rword mColor >> colon >>
    (N.readColourName =<< T.unpack <$> colorIdentifier)

-- Check if a Text is the name of an SVG color, and return it if it is. 
colorIdentifier :: Parser T.Text
colorIdentifier = (lexeme . try) (c >>= check)
    where
        c       = (T.toLower . T.pack) <$> some letterChar
        check x = if x `elem` svgColors
                    then return x
                    else fail $ show x ++ " is not an SVG color. "

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
              >>= gateLinkCheck >>= nodeNameCheck

-- Make sure that the InLinks in a Node match up with the input nodes in the 
-- NodeGate. 
gateLinkCheck :: (DMNode, [(NodeName, DMLink)])
              -> Parser (DMNode, [(NodeName, DMLink)])
gateLinkCheck parsed@((DMNode nMeta pNode), links) = 
    let gNode = gNodeName pNode
        assigns = gateAssigns pNode
        linkInputs = sort $ fst <$> links
        gExprs = snd <$> assigns
        gateInputs = sort $ fst <$> (refdNodesStates gExprs)
    in
    case gateInputs == linkInputs of
        True  -> return parsed
        False -> fail $ show $ GateInLinkMismatch (gateInputs, linkInputs)

-- Make sure that the NodeName from the parsed NodeGate is the same as the
-- NodeName from the parsed NodeMetaData
nodeNameCheck :: (DMNode, [(NodeName, DMLink)])
              -> Parser (DMNode, [(NodeName, DMLink)])
nodeNameCheck parsed@(pDMNode, links) = 
    let pGate = nodeGate pDMNode
        gName = (gNodeName . nodeGate) pDMNode
        mName = (nodeName . nodeMeta) pDMNode
    in
    case gName == mName of
        True  -> return parsed
        False -> fail $ show $ NodeMetaNameMismatch (gName, mName)

-- Parse the keys of a LaTeX "\cite{}" command. 
citationParse :: Parser [T.Text]
citationParse = fmap T.pack <$> (between (symbol "\\cite{") (symbol "}")
    (sepBy1 (many (noneOf [',', '}'])) comma) )


-- Return a (possibly empty) list of citation key lists. 
pullCitations :: T.Text -> Parser [[T.Text]]
pullCitations aMeta = lexeme' (rword aMeta >> colon' >>
    many (try (skipManyTill (noneOf ['\n', '\r']) citationParse)) <* 
        (skipManyTill ((noneOf ['\n', '\r']) <?> "pullCitations") eol ))

-- Parse a Note or Description, along with any embedded BibTeX citations. 
extractCitations :: T.Text -> Parser (T.Text, [[T.Text]])
extractCitations aMeta = do
    lText <- lookAhead (metaItem aMeta)
    citations <- pullCitations aMeta
    return (lText, citations)

-- Parse Node metadata
nodeConfigParse :: Parser NodeMeta
nodeConfigParse = between (symbol "NodeMetaData{") (symbol "NodeMetaData}") 
     (runPermutation $ 
        NodeMeta <$> toPermutation (identifier "NodeName")
                 <*> toPermutation parseNodeGenes
                 <*> toPermutation parseNodeType
                 <*> toPermutation (metaColor "NodeColor")
                 <*> toPermutation parseCoordinate
                 <*> ((,) <$> toPermutation (extractCitations "NodeDescription")
                           <*> toPermutation (extractCitations "NodeNotes")))


-- Parse the (possibly empty) list of EntrezID genes associated with a node
parseNodeGenes :: Parser [EntrezGeneID]
parseNodeGenes = (lexeme . try) $ rword "NodeGenes" >> colon >>
    (sepBy integer comma)

-- Parse the type of a node. 
parseNodeType :: Parser NodeType
parseNodeType = (lexeme . try) $ rword "NodeType" >> colon >>
    (   Cell <$ rword "Cell"
    <|> DM_Switch <$ rword "DM_Switch"
    <|> Connector <$ rword "Connector"
    <|> Environment <$ rword "Environment"
    <|> Process <$ rword "Process"
    <|> MRNA <$ rword "mRNA"
    <|> Protein <$ rword "Protein"
    <|> TFProtein <$ rword "TF_protein"
    <|> Metabolite <$ rword "Metabolite"
    <|> MacroStructure <$ rword "MacroStructure"
    <|> Kinase <$ rword "Kinase"
    <|> Phosphatase <$ rword "Phosphatase"
    <|> ProteinComplex <$ rword "protein_complex"
    <|> Ubiquitin_Ligase <$ rword "Ubiquitin_Ligase"
    )

-- Parse the graphical position of a node. 
parseCoordinate :: Parser (U.Vector Double)
parseCoordinate = (lexeme . try) $ rword "NodeCoordinate" >> colon >>
    ((U.fromList .  fmap toRealFloat) <$> sepBy number comma)
        
-- Parse a DMLink
dMLinkParse :: Parser (NodeName, DMLink)
dMLinkParse = between (symbol "InLink{") (symbol "InLink}") (runPermutation $
  (,) <$> toPermutation (identifier "InputNode")
      <*> (DMLink <$> toPermutation parseLinkEffect
                  <*> toPermutation (identifier "LinkType")
                  <*> ((,) <$> toPermutation (extractCitations "LinkDescription")
                           <*> toPermutation (extractCitations "LinkNotes"))))

dMLinkParseNodeless :: Parser DMLink
dMLinkParseNodeless = between (
    symbol "InLink{") (symbol "InLink}") (runPermutation $
        DMLink <$> toPermutation parseLinkEffect
               <*> toPermutation (identifier "LinkType")
               <*> ((,) <$> toPermutation (extractCitations "LinkDescription")
                        <*> toPermutation (extractCitations "LinkNotes")))

-- Parse the effect of a link. 
parseLinkEffect :: Parser LinkEffect
parseLinkEffect = (lexeme . try) $ rword "LinkEffect" >> colon >>
    (   Activation  <$ rword "Activation"
    <|> Repression  <$ rword "Repression"
    <|> Neutral     <$ rword "Neutral"
    )

-- Parse a NodeGate
gateParse :: Parser NodeGate
gateParse = gatePairParse >>= tableDisCheck

-- If we parse a logical expression AND a table, make sure they give the same
-- output for all possible inputs. 
tableDisCheck :: (Maybe LogicalNodeGate, Maybe TableNodeGate) -> Parser NodeGate
tableDisCheck (Just lG, Nothing) = return lG
tableDisCheck (Nothing, Just tG) = return tG
tableDisCheck (Just lG, Just tG) = case gateOrdCheck lG tG of
    Failure err -> fail $ show err
    Success (lGate, tGate) -> case isSubset lpInputs tpInputs of
        False -> fail $ show $ TableExprStateMismatch $
                        T.unlines (tGatePrint:(deleteMult tpInputs lpInputs))
        True -> case accOutputMis tOutputs lOutputs of
            []   -> return $ lGate'
            errs -> fail $ T.unpack
                        $ "TableExprOuputMismatch (Table, Logical): \n"
                            <> T.unlines (tGatePrint:errs)
        where
            tGatePrint = (T.concat $
                             intersperse (T.singleton '\t') $ tOrder <> [tName])
            tName = gNodeName tGate
            tpInputs = T.init <$> tOutputs
            lpInputs = T.init <$> lOutputs
            tOutputs = sort (prettyGateEval tGate <$> tCombos)
--          Evaluate the logical gate with the table gate order, to make the
--          pretty representations match. This is OK, since we have already made
--          sure that the input nodes are the same up to permutation. 
            lOutputs = sort (prettyGateEval lGate' <$> lCombos)
            tCombos = gateCombinations tExprs
            lCombos = gateCombinations lExprs
            tExprs = snd <$> (gateAssigns tGate)
            lExprs = snd <$> (gateAssigns lGate)
            lNodeName = gNodeName lGate
            lGateAssigns = gateAssigns lGate
            tOrder = gateOrder tGate
            lGate' = NodeGate lNodeName lGateAssigns tOrder

-- When the inputs of a logical gate are a subset of its corresponding table
-- we want to accumulate from the two PrettyGateOutput lists those rows where
-- both inputs exist, but whose outputs to that input are different. Call as:
-- dropExIns tOutput lOutput
accOutputMis :: [PrettyGateOutput] -> [PrettyGateOutput] -> [T.Text]
accOutputMis ts ls = foldl' go [] tSplits
    where
        go acc (key, state) = case lookup key lSplits of
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
-- identical up to permutation. If they are, set them both to be the order of
-- table gate. If not, error out and return the the two NodeName Lists. 
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
-- discrete logical expression, if that exists in the rrms file, and the second
-- as parsed from a truth table, if that exists in the rrms file. If they both
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
    Success nodeGate -> return nodeGate
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
    <|> booleanNTerm

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
    eol
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
    errs@(x:xs) -> fail $ show $ RepeatedKeys errs

citeEntryParse :: Parser BibTeXEntry
citeEntryParse = do
    char '@'
    entryType <- variable
    symbol "{"
    entryKey <- citeKeyParse
    comma
    fields <- sepEndBy1 citeFieldParse comma 
    symbol "}"
    return $ BibTeXEntry entryKey entryType fields

citeKeyParse :: Parser BibTeXKey
citeKeyParse = (lexeme . try) (p >>= check)
  where
    p       = T.pack <$> ((:) <$> (letterChar <|> char '_') <*> 
                    (many (alphaNumChar
                           <|> char '_'
                           <|> (oneOf ("&;:-_.?+/" :: [Char]) <?> "citeKey")
                           )
                    )
              )
    check x = if x `elem` rws
                then fail $ 
                    "Keyword " ++ show x ++ " cannot be a citation key. "
                else return x

-- Parse a line in a BibTeX record
citeFieldParse :: Parser (BibTeXField, BibTeXRecord)
citeFieldParse = (lexeme . try) $ do
    field <- bibVariable
    symbol "="
    record <- bibRecordParse
    return (field, record)

-- Parse a BibTeX assignment
bibVariable :: Parser BibTeXField
bibVariable = (lexeme . try) (p >>= check)
  where
    p       = T.pack <$> ((:) <$> (letterChar <|> char '_') <*> 
        (many (alphaNumChar
              <|> (oneOf ("&;:-_.?+/" :: [Char]) <?> "bibVariable"))))
    check x = if x `elem` rws
                then fail $
                    "Keyword " ++ show x ++ " cannot be a BibTeX field name. "
                else return x

bibRecordParse :: Parser BibTeXRecord
bibRecordParse = (lexeme . try) (
    -- Some fields have just letters and no enclosures.
    (T.pack <$> (some letterChar))
    -- Some fields have just digits and no enclosure. 
    <|> (T.pack <$> (some digitChar))
    <|> (braces (bibSeq '}')) -- Some fields are recursively enclosed in braces. 
    <|> (lexeme $ between (char '"') (char '"') (bibBlock '"'))
    )

bibSeq :: Char -> Parser T.Text
bibSeq c = (lexeme . try) (T.concat <$> (many $ bibBlock c))

bibBlock :: Char -> Parser T.Text
bibBlock c = (lexeme . try) (
      ((\start middle end -> (T.singleton start) <> middle <> (T.singleton end))
                        <$> (char '{') <*> (bibSeq '}') <*> (char '}')
      )
     <|> (T.pack <$> (sequenceA [ char '\\'
                                , (oneOf ("_{}[]$|'`^&%\".,~# " :: [Char])
                                        <?> "bibBlock")
                                   <|> letterChar]
                     )
         )
     <|> ((T.pack . (:[])) <$> (anySingleBut c))
     )

-- Make sure that the BibTeXKeys in a DMModel actually exist in the associated
-- CitationDictionary. 
bibKeyCheck :: (DMModel, CitationDictionary)
            -> Parser (DMModel, CitationDictionary)
bibKeyCheck x@(mo, cd) = case filter (\(x, y) -> y == False) keyCheck of
    []          -> return x
    errs@(x:xs) -> fail $ show $ MissingCitations (fst <$> errs)
    where
        keyCheck = zip mKeys $ sequenceA (Map.member <$> mKeys) cd
        mKeys = Set.toList $ citationsKeys mo

-- SVG Color Names:
svgColors :: [T.Text]
svgColors = ["aliceblue"
 ,"antiquewhite"
 ,"aqua"
 ,"aquamarine"
 ,"azure"
 ,"beige"
 ,"bisque"
 ,"black"
 ,"blanchedalmond"
 ,"blue"
 ,"blueviolet"
 ,"brown"
 ,"burlywood"
 ,"cadetblue"
 ,"chartreuse"
 ,"chocolate"
 ,"coral"
 ,"cornflowerblue"
 ,"cornsilk"
 ,"crimson"
 ,"cyan"
 ,"darkblue"
 ,"darkcyan"
 ,"darkgoldenrod"
 ,"darkgray"
 ,"darkgreen"
 ,"darkgrey"
 ,"darkkhaki"
 ,"darkmagenta"
 ,"darkolivegreen"
 ,"darkorange"
 ,"darkorchid"
 ,"darkred"
 ,"darksalmon"
 ,"darkseagreen"
 ,"darkslateblue"
 ,"darkslategray"
 ,"darkslategrey"
 ,"darkturquoise"
 ,"darkviolet"
 ,"deeppink"
 ,"deepskyblue"
 ,"dimgray"
 ,"dimgrey"
 ,"dodgerblue"
 ,"firebrick"
 ,"floralwhite"
 ,"forestgreen"
 ,"fuchsia"
 ,"gainsboro"
 ,"ghostwhite"
 ,"gold"
 ,"goldenrod"
 ,"gray"
 ,"grey"
 ,"green"
 ,"greenyellow"
 ,"honeydew"
 ,"hotpink"
 ,"indianred"
 ,"indigo"
 ,"ivory"
 ,"khaki"
 ,"lavender"
 ,"lavenderblush"
 ,"lawngreen"
 ,"lemonchiffon"
 ,"lightblue"
 ,"lightcoral"
 ,"lightcyan"
 ,"lightgoldenrodyellow"
 ,"lightgray"
 ,"lightgreen"
 ,"lightgrey"
 ,"lightpink"
 ,"lightsalmon"
 ,"lightseagreen"
 ,"lightskyblue"
 ,"lightslategray"
 ,"lightslategrey"
 ,"lightsteelblue"
 ,"lightyellow"
 ,"lime"
 ,"limegreen"
 ,"linen"
 ,"magenta"
 ,"maroon"
 ,"mediumaquamarine"
 ,"mediumblue"
 ,"mediumorchid"
 ,"mediumpurple"
 ,"mediumseagreen"
 ,"mediumslateblue"
 ,"mediumspringgreen"
 ,"mediumturquoise"
 ,"mediumvioletred"
 ,"midnightblue"
 ,"mintcream"
 ,"mistyrose"
 ,"moccasin"
 ,"navajowhite"
 ,"navy"
 ,"oldlace"
 ,"olive"
 ,"olivedrab"
 ,"orange"
 ,"orangered"
 ,"orchid"
 ,"palegoldenrod"
 ,"palegreen"
 ,"paleturquoise"
 ,"palevioletred"
 ,"papayawhip"
 ,"peachpuff"
 ,"peru"
 ,"pink"
 ,"plum"
 ,"powderblue"
 ,"purple"
 ,"red"
 ,"rosybrown"
 ,"royalblue"
 ,"saddlebrown"
 ,"salmon"
 ,"sandybrown"
 ,"seagreen"
 ,"seashell"
 ,"sienna"
 ,"silver"
 ,"skyblue"
 ,"slateblue"
 ,"slategray"
 ,"slategrey"
 ,"snow"
 ,"springgreen"
 ,"steelblue"
 ,"tan"
 ,"teal"
 ,"thistle"
 ,"tomato"
 ,"turquoise"
 ,"violet"
 ,"wheat"
 ,"white"
 ,"whitesmoke"
 ,"yellow"
 ,"yellowgreen"]

rws :: [T.Text]
rws = [ "ModelName"
    , "FormatVersion"
    , "ModelVersion"
    , "ModelPaper"
    , "Switch"
    , "BiasOrderFirst"
    , "BiasOrderLast"
    , "NodeName"
    , "NodeType"
    , "NodeColor"
    , "NodeCoordinate"
    , "NodeGenes"
    , "NodeDescription"
    , "NodeNotes"
    , "InputNode"
    , "LinkEffect"
    , "LinkType"
    , "LinkDescription"
    , "LinkNotes"
    , "ReferenceNotes"
    , "cite"
    , "textbf"
    , "textit"
    , "Model"
    , "ModelMapping"
    , "ModelGraph"
    , "Node"
    , "InLink"
    , "NodeGate"
    , "DiscreteLogic"
    , "TruthTable"
    , "NodeMetaData"
    , "CitationDictionary"
    ]


-- Testing Functions:
truthTableParseTest :: Parser ([NodeName],[[NodeState]])
truthTableParseTest = between (symbol "TruthTable{")
                          (symbol "TruthTable}")
                           slurpTable
                  >>= tableConsistencyCheckTest

tableConsistencyCheckTest :: ([NodeName],[[NodeState]])
                          -> Parser ([NodeName],[[NodeState]])
tableConsistencyCheckTest (nodes, rows) = case mkTableGateTest (nodes, rows) of
    Success tableGate -> return tableGate
    Failure errs      -> fail $ show errs

mkTableGateTest:: ([NodeName],[[NodeState]])
            -> Validation [TableInvalid] ([NodeName],[[NodeState]])
mkTableGateTest (nodes, rows)
    | testErrors == [] = Success (nodes, rows)
    | otherwise        = Failure testErrors
    where 
        cols = transpose rows
        inputRows = map init rows
        outputs = last cols
        testResults = [ allOutputsPresent outputs
                      , sufficientInputRows inputRows
                      , noDupeInputs inputRows
                      , rowsStrictlyIncreasing inputRows
                      ]
        testErrors = errorRollup testResults


-- Sort a list by the order of elements in the list order
sortWithOrder :: (Ord a, Hash.Hashable a) => [a] -> [a] -> [a]
sortWithOrder order = sortOn getOrder
    where
        getOrder k = Map.lookupDefault (-1) k orderHashMap
        orderHashMap = Map.fromList (zip order [1..])
