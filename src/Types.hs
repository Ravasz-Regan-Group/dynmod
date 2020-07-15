{-# LANGUAGE OverloadedStrings #-}

module Types where
--     ( DMModel(..)
--     , ModelMeta(..)
--     , FileFormatVersion
--     , PackageInfo(..)
--     ) where

import Utilities
import Text.LaTeX.Base.Class (fromLaTeX, commS, LaTeXC(..))
import Text.LaTeX.Base.Commands (footnotesize)
import Text.LaTeX.Base.Math (math)
import Text.LaTeX.Base.Syntax (LaTeX(..))
import Text.LaTeX.Base.Texy (Texy(..))
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SC
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import qualified Data.Graph.Inductive as Gr
import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.HashSet as Set
import Data.Validation
import qualified Data.List.Unique as Uniq
import qualified Data.Versions as Ver
import qualified Data.List as L
import Control.Applicative (liftA2)
import Data.Maybe (fromJust)
import Data.String (IsString(..))

-- Defining the types that will comprise a model,
-- to parse, verify, and run simulations

type LocalColor = C.Colour Double
defaultColor :: LocalColor -- Erzsó red
defaultColor = SC.sRGB24 148 17 0

type FileFormatVersion = Ver.SemVer

data DMModel = LayerBinding ModelMapping ModelLayer DMModel
             | Fine ModelLayer
           deriving (Show, Eq)

data ModelLayer = ModelLayer { modelGraph   :: ModelGraph
                             , modelMeta    :: ModelMeta
                             }
                             deriving (Show, Eq)

data ModelMeta = ModelMeta { modelName      :: ModelName
                           , modelVersion   :: Ver.SemVer
                           , modelPaper     :: [BibTeXKey]
                           , biasOrderFirst :: [(NodeName, NodeState)]
                           , biasOrderLast  :: [(NodeName, NodeState)]
                           , modelInfo :: LitInfo
                           }
                             deriving (Show, Eq)
type ModelName = T.Text


type ModelMapping = [(NodeName, [NodeName])]

type ModelGraph = Gr.Gr DMNode DMLink

data DMLink = DMLink { linkEffect :: LinkEffect
                     , linkType :: LinkType
                     , linkInfo :: LitInfo }
                       deriving (Show, Eq, Ord)

data DMNode = DMNode { nodeMeta :: NodeMeta
                     , nodeGate :: NodeGate }
                       deriving (Show, Eq)
instance Ord DMNode where
    compare x y = compare (nodeMeta x) (nodeMeta y)

data NodeMeta = NodeMeta { nodeName :: NodeName
                         , nodeGenes :: [EntrezGeneID]
                         , nodeType :: NodeType
                         , nodeColor :: LocalColor
                         , nodeCoordinate :: U.Vector Double
                         , nodeInfo :: LitInfo
                         }
                           deriving (Show, Eq)
instance Ord NodeMeta where
    compare x y = compare (nodeName x) (nodeName y)

-- Technically a NodeGate requires only a list of state assignments, since
-- "input gate order" only really makes sense in the context of a truth table,
-- which is not how we represent gates internally. However, most of the time
-- users will spend interacting with this software will be debugging
-- discrepancies between table and logical representations, so providing
-- feedback in the form of table rows is very important. Thus, we need to hang
-- on to the original order of nodes in that table representation. 
-- Also, the NodeName is contained in the
-- NodeMetaData, but from a human debugging point of view it seems wise to hang
-- on to it.
data NodeGate = NodeGate { gNodeName   :: NodeName
                         , gateAssigns :: [NodeStateAssign]
                         , gateOrder   :: [NodeName]
                         }
                         deriving (Show, Eq)

displayStyle :: LaTeXC l => l
displayStyle = commS "displaystyle"

type NodeStateAssign = (NodeState, NodeExpr)
type NodeRange = (NodeName, [NodeState])
type LayerRange = Map.HashMap NodeName [NodeState]

-- The first pair is a description, with accompanying list of \cites. 
-- The second pair is a note, with accompanying list of \cites. 
type LitInfo = ((Description, CitationLists), (Note, CitationLists))
type Description = T.Text
type Note = T.Text
type CitationLists = [[T.Text]]

-- The gates are initially implemented as dictionaries that I build while 
-- parsing, since I don't know how to parse a regular function (i.e. one that 
-- isn't an endlessly re-evaluated logical expression) without Template Haskell,
-- and that's a bridge too far for a first run. - Pete Regan 04/22/19

-- type NodeGate = Map.HashMap (U.Vector Int) NodeState

-- Update. The validation and simulation aspects are being separated, so
-- the gates using our own custom type here for validation will not be 
-- computationally punitive. - Pete Regan January 13, 2020


-- The following are essentially phantom types, so that I don't write a bug
-- based on forgetting which in the pair came from the discrete logical 
-- expression, and which from the truth table. 
type LogicalNodeGate = NodeGate
type TableNodeGate = NodeGate

data NodeType = Undefined_NT
              | Cell
              | DM_Switch
              | Connector
              | Environment
              | Process
              | Macro_Structure
              | Metabolite
              | MRNA
              | MicroRNA
              | Protein_Complex
              | Receptor
              | Adaptor_Protein
              | Secreted_Protein
              | TF_Protein
              | Kinase
              | Phosphatase
              | Ubiquitin_Ligase
              | Protease
              | DNase
              | CAM
              | CDK
              | CDKI
              | GEF
              | GAP
              | GTPase
              | Enzyme
              | Protein
                deriving (Show, Eq, Bounded, Enum)

instance Texy NodeType where
    texy Undefined_NT     = texy ("Undefined_NT" :: T.Text)
    texy Cell             = (footnotesize . fromLaTeX . TeXRaw) "Cell"
    texy DM_Switch        = (footnotesize . fromLaTeX . TeXRaw) "DM"
    texy Connector        = (footnotesize . fromLaTeX . TeXRaw) "Conn"
    texy Environment      = (footnotesize . fromLaTeX . TeXRaw) "Env"
    texy Process          = (footnotesize . fromLaTeX . TeXRaw) "Proc"
    texy Macro_Structure  = (footnotesize . fromLaTeX . TeXRaw) "MSt"
    texy Metabolite       = (footnotesize . fromLaTeX . TeXRaw) "Met"
    texy MRNA             = (footnotesize . fromLaTeX . TeXRaw) "mRNA"
    texy MicroRNA         = (footnotesize . fromLaTeX . TeXRaw) "miR"
    texy Protein_Complex  = (footnotesize . fromLaTeX . TeXRaw) "PC"
    texy Receptor         = (footnotesize . fromLaTeX . TeXRaw) "Rec"
    texy Adaptor_Protein  = (footnotesize . fromLaTeX . TeXRaw) "Adap"
    texy Secreted_Protein = (footnotesize . fromLaTeX . TeXRaw) "Secr"
    texy TF_Protein       = (footnotesize . fromLaTeX . TeXRaw) "TF"
    texy Kinase           = (footnotesize . fromLaTeX . TeXRaw) "K"
    texy Phosphatase      = (footnotesize . fromLaTeX . TeXRaw) "Ph"
    texy Ubiquitin_Ligase = (footnotesize . fromLaTeX . TeXRaw) "UbL"
    texy Protease         = (footnotesize . fromLaTeX . TeXRaw) "PTase"
    texy DNase            = (footnotesize . fromLaTeX . TeXRaw) "DNase"
    texy CAM              = (footnotesize . fromLaTeX . TeXRaw) "CAM"
    texy CDK              = (footnotesize . fromLaTeX . TeXRaw) "CDK"
    texy CDKI             = (footnotesize . fromLaTeX . TeXRaw) "CDKI"
    texy GEF              = (footnotesize . fromLaTeX . TeXRaw) "GEF"
    texy GAP              = (footnotesize . fromLaTeX . TeXRaw) "GAP"
    texy GTPase           = (footnotesize . fromLaTeX . TeXRaw) "GTPa"
    texy Enzyme           = (footnotesize . fromLaTeX . TeXRaw) "Enz"
    texy Protein          = (footnotesize . fromLaTeX . TeXRaw) "Prot"

data LinkEffect = Undefined_LE
                | Activation
                | Repression
                | Context_Dependent
                | Inapt
                  deriving (Show, Eq, Ord, Bounded, Enum)

instance Texy LinkEffect where
    texy Undefined_LE = texy ("Undefined_LE" :: T.Text)
    texy Activation = math $ commS "leftarrow"
    -- from fdsymbol font
    texy Repression = math $ commS "leftfootline"
    -- from fdsymbol font
    texy Context_Dependent = math $ commS "leftblackspoon" 
    texy Inapt = math $ commS "perp"

data LinkType =   Undefined_LT
                | Enforced_Env
                | Indirect
                | Complex_Process
                | Persistence
                | Transcription
                | Translation
                | Ligand_Binding
                | Complex_Formation
                | Inhibitory_Binding
                | Localization
                | Binding_Localizaton
                | Protective_Binding
                | Unbinding
                | Phosphorylation
                | Dephosphorylation
                | Phosphorylation_Localization
                | Ubiquitination
                | Degradation
                | GEF_Activity
                | GAP_Activity
                | Proteolysis
                | Catalysis
                  deriving (Show, Eq, Ord, Bounded, Enum)

instance Texy LinkType where
    texy Undefined_LT        = texy ("Undefined_LT" :: T.Text)
    texy Enforced_Env        = (footnotesize . fromLaTeX . TeXRaw) "Env"
    texy Indirect            = (footnotesize . fromLaTeX . TeXRaw) "Ind"
    texy Complex_Process     = (footnotesize . fromLaTeX . TeXRaw) "ComplProc"
    texy Persistence         = (footnotesize . fromLaTeX . TeXRaw) "Per"
    texy Transcription       = (footnotesize . fromLaTeX . TeXRaw) "TR"
    texy Translation         = (footnotesize . fromLaTeX . TeXRaw) "TL"
    texy Ligand_Binding      = (footnotesize . fromLaTeX . TeXRaw) "Ligand"
    texy Complex_Formation   = (footnotesize . fromLaTeX . TeXRaw) "Compl"
    texy Inhibitory_Binding  = (footnotesize . fromLaTeX . TeXRaw) "IBind"
    texy Localization        = (footnotesize . fromLaTeX . TeXRaw) "Loc"
    texy Binding_Localizaton = (footnotesize . fromLaTeX . TeXRaw) "BLoc"
    texy Protective_Binding  = (footnotesize . fromLaTeX . TeXRaw) "PBind"
    texy Unbinding           = (footnotesize . fromLaTeX . TeXRaw) "Unbind"
    texy Phosphorylation     = (footnotesize . fromLaTeX . TeXRaw) "P"
    texy Dephosphorylation   = (footnotesize . fromLaTeX . TeXRaw) "DP"
    texy Phosphorylation_Localization =
        (footnotesize . fromLaTeX . TeXRaw) "PLoc"
    texy Ubiquitination      = (footnotesize . fromLaTeX . TeXRaw) "Ubiq"
    texy Degradation         = (footnotesize . fromLaTeX . TeXRaw) "Deg"
    texy GEF_Activity        = (footnotesize . fromLaTeX . TeXRaw) "GEF"
    texy GAP_Activity        = (footnotesize . fromLaTeX . TeXRaw) "GAP"
    texy Proteolysis         = (footnotesize . fromLaTeX . TeXRaw) "Lysis"
    texy Catalysis           = (footnotesize . fromLaTeX . TeXRaw) "Cat"

type EntrezGeneID = Int
type NodeName = T.Text
type NodeState = Int
type ExprInput = Map.HashMap NodeName NodeState
data NodeCondition = NodeCondition { currentState :: NodeState 
                                   , destinationState :: NodeState
                                   , fractionDone :: Double
                                   , fractionDelta :: Double
                                   , transitionType :: TransitionType }
                                     deriving (Show, Eq)


type TransitionType = T.Text

type CitationDictionary = Map.HashMap BibTeXKey BibTeXEntry
type BibTeXKey = T.Text

data BibTeXEntry = BibTeXEntry {
                      entryKey    :: BibTeXKey
                    , entryType   :: T.Text
                    , entryFields :: [(BibTeXField, BibTeXRecord)]
                    }
                    deriving (Show, Eq)

type BibTeXField = T.Text
type BibTeXRecord = T.Text

data NodeExpr
  = GateLit Bool
  | GateConst NodeName NodeState
  | Not NodeExpr
  | Binary BinOp NodeExpr NodeExpr
  deriving (Eq)

instance Show NodeExpr where
    show (GateLit b) = show b
    show (GateConst n s) = (show n) ++ ":" ++ (show s)
    show (Not expr) = "not " ++ (exprPars show expr)
    show (Binary And expr1 expr2) =
        (exprPars show expr1) ++ " and " ++ (exprPars show expr2)
    show (Binary Or expr1 expr2) =
        (exprPars show expr1) ++ " or " ++ (exprPars show expr2)

-- NodeExpr should only be wrapped in parenthesis if they are not single terms. 
exprPars :: (IsString a, Semigroup a) => (NodeExpr -> a) -> NodeExpr -> a
exprPars f ex@(GateLit _) = f ex
exprPars f ex@(GateConst _ _) = f ex
exprPars f ex@(Not _) = "(" <> f ex <> ")"
exprPars f ex@(Binary And _ _) = "(" <> f ex <> ")"
exprPars f ex@(Binary Or _ _) = "(" <> f ex <> ")"

data BinOp
  = And
  | Or
  deriving (Show, Eq)

-- Evaluate a node expression
eval :: NodeExpr -> ExprInput -> Maybe Bool
eval (Not expr1) ns = not <$> (eval expr1 ns)
eval (Binary And expr1 expr2) ns =
    liftA2 (&&) (eval expr1 ns) (eval expr2 ns)
eval (Binary Or expr1 expr2) ns =
    liftA2 (||) (eval expr1 ns) (eval expr2 ns)
eval (GateConst nName nState) ns =
    (nState ==) <$> (Map.lookup nName ns)
eval (GateLit b) _ =  Just b
-- # SCC eval #-}
-- eval expr nodeStates = eval' expr
--   where
--     eval' (Not expr1) = not <$> (eval' expr1)
--     eval' (Binary And expr1 expr2) = liftA2 (&&) (eval' expr1) (eval' expr2)
--     eval' (Binary Or expr1 expr2) = liftA2 (||) (eval' expr1) (eval' expr2)
--     eval' (GateConst nName nState)
--       = (nState ==) <$> (Map.lookup nName nodeStates)
--     eval' (GateLit b) = Just b

-- Evaluate a gate against a given input HashMap
gateEval :: NodeGate -> ExprInput -> Maybe NodeState
gateEval nGate nInput
    | areInputsSufficient = (L.findIndex (== Just True) output)
    | otherwise = Nothing
    where
        assigns = gateAssigns nGate
        exprs = snd <$> assigns
        output = (flip eval nInput) <$> exprs
        areInputsSufficient = not $ elem Nothing output
-- # SCC gateEval #-}

-- Evaluate a gate against a given input HashMap, and return the result as tab
-- separated Text formatted integers in proper gateOrder order. Use only on
-- ExprInputs that contain all the necessary inputs
prettyGateEval :: NodeGate -> ExprInput -> PrettyGateOutput
prettyGateEval nGate nInput = prettify output
    where
        prettify = (((prettyInput <> T.singleton '\t') <>) . T.pack . show)
        prettyInput = T.intersperse '\t'
                        $ T.concat
                            $ (T.pack . show) <$> orderedInput
        orderedInput = fromJust <$> (sequenceA (Map.lookup <$> order) nInput)
        output = fromJust $ gateEval nGate nInput
        order = gateOrder nGate
-- # SCC prettyGateEval #-}

type PrettyGateOutput = T.Text
type LayerTTFiles = (T.Text, [(NodeName, T.Text)])
type ModelTTFiles = [LayerTTFiles]

-- Generate T.Texts for TT files (csv files with a truth table in each) from the
-- nodes in a ModelLayer, along with the name of that layer and names of the
-- nodes. 
layerTTs :: ModelLayer -> (T.Text, [(NodeName, T.Text)])
layerTTs mL = (mName, tablesWNames)
    where
        mName = (modelName . modelMeta) mL
        tablesWNames = zip nodeNames prettyTTables
        prettyTTables = T.unlines <$> (zipWith (:) gatePrints prettyRows)
        prettyRows :: [[PrettyGateOutput]]
        prettyRows = L.sort <$> (zipWith ($) fs comboLists)
        fs :: [[ExprInput] -> [PrettyGateOutput]]
        fs = go <$> nodeGates
--      apply prettyGateEval to a gate, and then a list of ExprInputs
        go :: NodeGate -> [ExprInput] -> [PrettyGateOutput]
        go g es = prettyGateEval g <$> es
        comboLists :: [[ExprInput]]
        comboLists = layerCombinations mL
        nodeGates = nodeGate <$> nodes
--      This is the top line of each truth table
        gatePrints :: [T.Text]
        gatePrints  = mkGatePrint <$> nodes
        nodeNames = (nodeName . nodeMeta) <$> nodes
        nodes = layerNodes mL

-- Make the top line of the appropriate Truth Table for a DMNode
mkGatePrint :: DMNode -> T.Text
mkGatePrint node = T.concat $ L.intersperse (T.singleton '\t') $ o <> [n]
    where
        o = (gateOrder . nodeGate) node
        n = (nodeName . nodeMeta) node

-- A DMNode and all of its InLinks. Useful in formating supplementary tables. 
type InAdj = (DMNode, [(DMLink, NodeName)])

-- Extract all of the DMNodes, along with their inbound DMLinks (and input
-- NodeNames), from a ModelGraph. N.B. We include self-loops as in-links, as
-- our context arose out of functions on boolean networks, where that is the
-- norm. 
inAdjs :: Gr.Gr DMNode DMLink -> [InAdj]
inAdjs mG = inLinkPrep <$> contexts
    where
        inLinkPrep (ins, n, dmNode, outs) =
            (dmNode, (linkNamePrep mG) <$> allIns)
            where
                allIns = (gLoop n outs) <> ins
        gLoop i ass = case L.find ((\k (_, j) -> k == j) i) ass of
            Nothing -> []
            Just x  -> [x]
        linkNamePrep gr (dmL, n) = (dmL, 
                (nodeName . nodeMeta . thdOf4 . fromJust . fst)
                (Gr.match n gr)
            )
        contexts = sequenceA ((flip Gr.context . fst) <$> graphNodes) mG
        graphNodes = Gr.labNodes mG

-- Given a list of InAdj and a NodeName, extract the corresponding
-- entry, if it exists
findInAdj :: NodeName -> [InAdj] -> Maybe InAdj
findInAdj n = L.find ((\x (dmN, _) -> x == (nodeName . nodeMeta) dmN) n)


-- Error handling types

data ModelInvalid = DuplicateCoarseMapNodes DuplicateCoarseMapNodes
                  | ExcessFineMapNodes ExcessFineMapNodes
                  | MissingFineMapNodes MissingFineMapNodes
                  | ExcessCoarseMapNodes ExcessCoarseMapNodes
                  | MissingCoarseMapNodes MissingCoarseMapNodes
                  | FineInMultipleCoarse FineInMultipleCoarse
                  | DuplicatedNodeNames DuplicatedNodeNames
                  | MissingCitations MissingCitations
    deriving (Show, Eq)
type DuplicateCoarseMapNodes = [NodeName]
type ExcessFineMapNodes      = [NodeName]
type MissingFineMapNodes     = [NodeName]
type ExcessCoarseMapNodes    = [NodeName]
type MissingCoarseMapNodes   = [NodeName]
type FineInMultipleCoarse    = [NodeName]
type DuplicatedNodeNames     = [NodeName]
type MissingCitations = [BibTeXKey]

data GateInvalid = InconsistentNames
                 | DuplicateAssigns
                 | ZeroAssigned
                 | MissingOrTooHigh [NodeState]
                 | OutOfOrder
                 | EmptyGate
                 | ContradictoryExprSet ContradictoryExprSet
                 | TableExprInNodeMismatch TableExprInNodeMismatch
                 | TableExprStateMismatch TableExprStateMismatch
                 | TableExprOuputMismatch TableExprOuputMismatch
                 | TableDisNameMismatch TableDisNameMismatch
    deriving (Show, Eq)

-- If there are internal contradictions in a gate, this provides the relevant
-- expressions, expression states, inputs, and gate states affected. 
type ContradictoryExprSet = 
        [([NodeExpr], [Int], ExprInput, [Maybe Bool])]
type TableExprMismatch = (([ExprInput], [Maybe NodeState])
                              , ([ExprInput], [Maybe NodeState]))
type TableExprInNodeMismatch = ([NodeName], [NodeName])
type TableExprStateMismatch = T.Text
type TableExprOuputMismatch = (T.Text, [NodeStateAssign])
type TableDisNameMismatch = (NodeName, NodeName)

data TableInvalid = IncompleteOrOversizedRow
                  | InsufficientRows
                  | ExcessRows
                  | OutOfOrderRows
                  | DuplicatedInputRows
                  | MissingOrTooHighOutputs 
    deriving (Show, Eq)

data GateInLinkInvalid = GateInLinkMismatch GateInLinkMismatch
    deriving (Show, Eq)
type GateInLinkMismatch = ([NodeName], [NodeName])

data NodeInvalid = NodeMetaNameMismatch NodeMetaNameMismatch
                 | DuplicateDMLinks DuplicateDMLinks
    deriving (Show, Eq)
type NodeMetaNameMismatch = (NodeName, NodeName) 
type DuplicateDMLinks = [NodeName]

data ModelLayerInvalid = 
                    NodeRefdNodesMismatch NodeRefdNodesMismatch
                  | StatesRefdStatesMisMatch StatesRefdStatesMisMatch
                  | NodeInlinkMismatch NodeInlinkMismatch
     deriving (Show, Eq)
type NodeRefdNodesMismatch = [NodeName] -- Nodes in NodeExprs that are not in
                                        -- any node
type StatesRefdStatesMisMatch = [(NodeName, [NodeState])]
type NodeInlinkMismatch = ([NodeName], [NodeName])

data CiteDictionaryInvalid = RepeatedKeys RepeatedKeys
    deriving (Show, Eq)
type RepeatedKeys = [BibTeXKey]

data PubInvalid =   PubMissingDesc MissingDescription
                  | UndefinedNodeType UndefinedNodeType
                  | UnspecifiedNodeColor UnspecifiedNodeColor
                  | MissingCoord MissingCoord
                  | CoordWrongDimension CoordWrongDimension
                  | UndefinedLinkType UndefinedLinkType
                  | UndefinedEffectType UndefinedEffectType
                  | OrphanedModelCites OrphanedModelCites
                  | ExcessDictCites ExcessDictCites
    deriving (Show, Eq, Ord)

 -- The name of the piece missing a description, and its associated type. 
data MissingDescription = ModelD T.Text
                        | NodeD T.Text
                        | InLinkD T.Text
                        deriving (Eq, Show, Ord)
type UndefinedNodeType = NodeName 
type UnspecifiedNodeColor = T.Text --Notice to pick an SVG color
type MissingCoord = T.Text
type CoordWrongDimension = T.Text
-- Associated NodeName and a list possible types.
type UndefinedLinkType = (NodeName, NodeName) -- The associated Node
type UndefinedEffectType = (NodeName, NodeName) -- The associated Node
type OrphanedModelCites = (T.Text, [BibTeXKey])
type ExcessDictCites = (T.Text, [BibTeXKey])


-- It would be very easy to assemble DMModels with nonsensical maps between
-- model layers, so we never create a LayerBinding directly, only through his
-- smart constructor. This ensures that the coarse nodes and fine nodes
-- from the map are bijective to the the nodes in the coarse and fine
-- ModelLayers, respectively, and also that the mapping itself from
-- fine -> coarse is surjective. 
mkLayerBinding :: ModelMapping -> ModelLayer -> DMModel
               -> Validation [ModelInvalid] DMModel
mkLayerBinding mMap mLayer mModel
  | errs == [] = Success $ LayerBinding mMap mLayer mModel
  | otherwise  = Failure errs
    where
      errs        = errorRollup testResults
      testResults = [ noCoarseDupes $ fst <$> mMap
                    , fineNodesmatch mapFineNodes mNodes
                    , coarseNodesMatch  mapCoarseNodes graphNodes
                    , isMapSurjective mMap
                    ]
      mapFineNodes = Set.toList $ Set.unions mapFineSets
      mapCoarseNodes = Set.toList $ Set.fromList $ fst <$> mMap
      graphNodes = (nodeName . nodeMeta . snd) <$> graphGrNodes
      mNodes = (nodeName . nodeMeta . snd) <$> modelGrNodes
      mapFineSets = (Set.fromList . snd) <$> mMap
      graphGrNodes = (Gr.labNodes . modelGraph) mLayer
      modelGrNodes = (Gr.labNodes . modelGraph . coarseLayer) mModel

-- Check that there are no duplicates in the coarse nodes
noCoarseDupes :: [NodeName] -> Validation ModelInvalid [NodeName]
noCoarseDupes cs = case Uniq.repeated cs of
    []   -> Success cs
    errs -> Failure $ DuplicateCoarseMapNodes errs

-- Check that the fine-grain nodes from a ModelMapping are the same up to
-- permutation as the nodes from the DMModel. 
fineNodesmatch :: [NodeName] -> [NodeName] -> Validation ModelInvalid [NodeName]
fineNodesmatch mapFineNodes moNodes
    | arePermutes mapFineNodes moNodes = Success mapFineNodes
    | isSubset mapFineNodes moNodes    = Failure $ MissingFineMapNodes $
                                            deleteMult mapFineNodes moNodes
    | otherwise                           = Failure $ ExcessFineMapNodes $
                                            deleteMult moNodes mapFineNodes

-- Check that the coarse-grain nodes from a ModelMapping are the same up to
-- permutation as the nodes from the ModelGraph. This assumes that there are no
-- duplicate coarse-grained nodes, as that is checked elsewhere. 
coarseNodesMatch :: [NodeName] -> [NodeName]
                 -> Validation ModelInvalid [NodeName]
coarseNodesMatch mapCoarseNodes graphNodes
    | arePermutes mapCoarseNodes graphNodes = Success mapCoarseNodes
    | isSubset mapCoarseNodes graphNodes    = Failure $ MissingCoarseMapNodes $
                                            deleteMult mapCoarseNodes graphNodes
    | otherwise                             = Failure $ ExcessCoarseMapNodes $
                                            deleteMult graphNodes mapCoarseNodes

-- Check that the ModelMapping itself is surjective. This assumes that the fine
-- and coarse nodes from the ModelMapping do in fact correspond exactly to the
-- nodes in the DMModel and ModelGraph, respectively, as this is tested
-- elsewhere. 
isMapSurjective :: ModelMapping -> Validation ModelInvalid [NodeName]
isMapSurjective ms
    | dupes == Set.empty = Success $ fst <$> ms
    | otherwise          = Failure $ FineInMultipleCoarse $ Set.toList dupes
    where
        dupes = nIntersection sets
        sets = (Set.fromList . snd) <$> ms


-- It would be very easy to blindly create nonsensical gates, so we never create
-- a NodeGate directly, only through these smart constructors.

-- In the case where we are parsing a discrete logical statment.  
mkLogicalGate :: [(NodeName, NodeStateAssign)]
              -> Validation [GateInvalid] NodeGate
mkLogicalGate xs =
    NodeGate <$> (nodeNamesOK ns) <*> (stateAssignsOK ass) <*> (pure order)
        where (ns, ass) = unzip xs
              order     = fst <$> (refdNodesStates $ snd <$> ass)


-- Several helper functions for gate sanity/consistency checks:

-- Logical Gates:
-- Check that the gate has actual terms in it. 
nonEmptyGate :: [(NodeName, NodeStateAssign)] 
             -> Validation [GateInvalid] [(NodeName, NodeStateAssign)]
nonEmptyGate nPairs = case nPairs == [] of
    False -> Success nPairs
    True  -> Failure [EmptyGate]

-- Check that all node state assignments use the same node names. Don't call 
-- this outside of the smart constructer, as it doesn't check for empty lists. 
nodeNamesOK :: [NodeName] -> Validation [GateInvalid] NodeName
nodeNamesOK names = case allTheSame names of
    True  -> Success (head names)
    False -> Failure [InconsistentNames]

-- Check that all node states are assigned in order, without 0 or duplicates. 

stateAssignsOK :: [NodeStateAssign]
                -> Validation [GateInvalid] [NodeStateAssign]
stateAssignsOK assigns
    | (allGood ns', errorRollup testResults)
        == (True, []) = Success $ zeroth:assigns
    | otherwise       = Failure $ errorRollup testResults
      where
        ns' =  states assigns
        n = length ns'
        allGood ns = [1..n] == ns
        zeroth = (0, Not $ foldr1 (Binary Or) (snd <$> assigns))
        testResults = [ isMonotonic assigns
                      , noMissingOrTooHigh assigns
                      , noZeros assigns
                      , noDupes assigns
                      , isConsistent (zeroth:assigns)
                      ]

states :: [NodeStateAssign] -> [NodeState]
states = fmap fst


-- Truth Table Gates:
mkTableGate :: ([NodeName],[[NodeState]])
            -> Validation [TableInvalid] TableNodeGate
mkTableGate (nodes, rows)
    | testErrors == [] = Success $ tableToLogical nodes inputRows outputs
    | otherwise        = Failure testErrors
    where 
        cols = L.transpose rows
        inputRows = map init rows
        outputs = last cols
        testResults = [ allOutputsPresent outputs
                      , sufficientInputRows inputRows
                      , noDupeInputs inputRows
                      , rowsStrictlyIncreasing inputRows
                      ]
        testErrors = errorRollup testResults

tableToLogical :: [NodeName] -> [[NodeState]] -> [NodeState] -> TableNodeGate
tableToLogical nodes inputRows outputs = tableNode
    where
--      Associate each input in each row with its corresponding node: 
        inputAssociations = zip (init nodes) <$> inputRows
--      Yank out the name of the node whose gate this is: 
        nName = last nodes
--      Turn all the pairs of NodeName and NodeState into GateConsts: 
        inputExprList = fmap (fmap (uncurry GateConst)) inputAssociations
--      AND together all the entries in each row: 
        inputExprs = fmap (foldr1 (Binary And)) inputExprList
--      Pair each ANDed row assignment with its associated output:
        inputAssigns = zip inputExprs outputs
--      Pull the range of possible output states
        possibleOutputStates :: [NodeState]
        possibleOutputStates = Uniq.sortUniq outputs
--      Match assignments for a given (s) state
        stateMatch :: NodeState -> (b, NodeState) -> Bool
        stateMatch s = (== s) . snd
--      Create a list of filter functions from the range of output states, so
--      that we can sort the list of assignments by output state:
        outputFilters :: [[(b, NodeState)] -> [(b, NodeState)]]
        outputFilters = filter <$> (stateMatch <$> possibleOutputStates)
--      Apply those filter functions each to the assignment statements, to get a
--      list of lists that is the sorted rows of assignments
        groupedOrs :: [[(NodeExpr,NodeState)]]
        groupedOrs = sequenceA outputFilters inputAssigns
--      Function that extracts the output state from each list in the list of
--      lists, so that it con be properly associated with the final ORed
--      assignment:
        extractOutput :: [(NodeExpr, NodeState)] -> (NodeState, [NodeExpr])
        extractOutput pairs = (nodeState, exprs)
            where
                nodeState = (snd . head) pairs
                exprs = map fst pairs
--      Create the (NodeState, [NodeExpr]) pairs:
        extractedOutputs = map extractOutput groupedOrs
--      Function that takes the pair (NodeState, [NodeExpr]) and returns the
--      pair (NodeState, NodeExpr) by ORing them together
        orAssignments (output, assigns) = (output, foldr1 (Binary Or) assigns)
--      Create a list which is the final NodeExpr for each state of the node
--      whose gate we are constructing:
        collapsedOrs = fmap orAssignments extractedOutputs
--      Create the final TableNodeGate:
        tableNode = NodeGate nName collapsedOrs (init nodes)
        


-- Check that there are no duplicate state assignments. 
noDupes :: [NodeStateAssign] -> Validation GateInvalid [NodeStateAssign]
noDupes ns = let st = states ns in 
             case (length st) == (length $ Uniq.sortUniq st) of
               True  -> Success ns
               False -> Failure DuplicateAssigns

-- Check that the zero (0) state is not assigned. 
noZeros :: [NodeStateAssign] -> Validation GateInvalid [NodeStateAssign]
noZeros ns = let st = states ns in 
              case elem 0 st of 
                False -> Success ns
                True  -> Failure ZeroAssigned

-- Check that there are no missing (or, equivalently, too high) state 
-- assignments. 
noMissingOrTooHigh :: [NodeStateAssign] 
                    -> Validation GateInvalid [NodeStateAssign] 
noMissingOrTooHigh ns = let st = states ns
--                          Remove any dupes and any assignments below 1. 
--                          That kind of error will be dealt with separately. 
                            clean = Uniq.sortUniq . (filter (> 0))
                            cleaned = clean st
                        in 
              case maximum st == (length cleaned) of
                True  -> Success ns
                False -> Failure $ MissingOrTooHigh cleaned
                
-- Check that the state assignments are listed in increasing monotonic order. 
isMonotonic :: [NodeStateAssign] -> Validation GateInvalid [NodeStateAssign]    
isMonotonic ns = let st = states ns in 
              case L.sort st == st of
                True  -> Success ns
                False -> Failure OutOfOrder

-- Check that no two state assignments are ever both True under the same inputs.
isConsistent :: [NodeStateAssign] -> Validation GateInvalid [NodeStateAssign]
isConsistent gAssigns
  | aberrantIndices == [] = Success gAssigns
  | otherwise             = Failure errs
    where 
        gateExprs :: [NodeExpr]
        gateExprs = snd <$> gAssigns
        gateCombos = gateCombinations gateExprs
--      These are the outputs from evaluating the expressions that define this
--      gate against all the inputs that might possibly produce a true output
--      from any of the them. Each of these Lists of Bool should contain exactly
--      one True. Any fewer and something is wrong with the null state (0). Any
--      more and the gate is internally self-contradictory. 
        gEval gateEx gCombo oPuts = (sequenceA (eval <$> gateEx) gCombo):oPuts
        gateOutputs :: [[Maybe Bool]]
        gateOutputs = foldr (gEval gateExprs) [] gateCombos
--      Find the number of times a True occurs in each batch of outputs. 
        trueOccurances = (numTimes (Just True)) <$> gateOutputs
--      Find the indices of those output batches where there are either too many
--      or too few Trues. 
        aberrantIndices = L.findIndices (/= 1) trueOccurances
--      Gather the aberrant outputs. 
        abOutputs = (!!) gateOutputs <$> aberrantIndices
--      Gather the aberrant inputs
        abInputs = (!!) gateCombos <$> aberrantIndices
--      Gather which expressions in each aberrant batch (by final output, e.g.
--      GF:1 or GF:0) are true. These will either show the states that are true
--      for the same inputs, or an empty list, which will show that something is
--      wrong with the null gate. 
        abExprStates = L.findIndices (== (Just True)) <$> abOutputs
--      Gather a List of Lists of the actual expressions that eval is using to
--      generate the aberrant inputs. 
        abExprs = zipWith
                    (fmap . (!!))
                    (replicate (length abExprStates) gateExprs)
                    abExprStates
--      Assemble the gate states and inputs for reporting. 
        errs = ContradictoryExprSet 
            $ L.zip4 abExprs abExprStates abInputs abOutputs

-- Table Gates

-- Does the output column of the table contain all possible output states of the
-- node at least once? On succes, the outputs are wrapped with another List, so
-- that the emitted type matches the rest of the testResults list. 
allOutputsPresent :: [NodeState] -> Validation TableInvalid [[NodeState]]
allOutputsPresent outputs =
    let top = maximum outputs
        expectedOutputs = [0..top]
        presentOutputs = Uniq.sortUniq outputs
    in
    case presentOutputs == expectedOutputs of
        True  -> Success [outputs]
        False -> Failure MissingOrTooHighOutputs

-- Are there the correct number of rows to exactly cover all possible 
-- combinations of the various inputs? 
sufficientInputRows :: [[NodeState]] -> Validation TableInvalid [[NodeState]]
sufficientInputRows inputs =
    let inputCols = L.transpose inputs
        inputCardinality = product $ map ((+1) . maximum) inputCols
        rowCardinality = length inputs
    in
    case inputCardinality == rowCardinality of
        True  -> Success inputs
        False -> case inputCardinality > rowCardinality of
                     True  -> Failure InsufficientRows
                     False -> Failure ExcessRows

-- Are the rows of inputs state, as written, in strictly increasing binary
-- (or trinary, quatenary, etc.) order?
rowsStrictlyIncreasing :: [[NodeState]]
                       -> Validation TableInvalid [[NodeState]]
rowsStrictlyIncreasing inputs = case isStrictlyIncreasing inputs of
    True  -> Success inputs
    False -> Failure OutOfOrderRows

-- Are all the input rows unique? If not, return the error and also a pair that
-- contains the duplicated rows and their (zero-indexed) locations. 
noDupeInputs :: [[NodeState]]
             -> Validation TableInvalid [[NodeState]]
noDupeInputs inputs =
    let (_, _, nonDuped) = Uniq.complex inputs
    in
    case inputs == nonDuped of
        True  -> Success inputs
        False -> Failure DuplicatedInputRows

-- In case it turns out that finding where duplicated rows are is challenging, 
-- This is how to return a map of them. It makes the error reporting harder, so
-- I'm not going to do it unless I have to. 
duplicatedRowMap :: [[NodeState]] -> ([[Int]], [[NodeState]])
duplicatedRowMap inputs = (coordinates, inputs)
    where (_, dupes, _) = Uniq.complex inputs
          coordinates = sequenceA (L.elemIndices <$> dupes) inputs



-- | Helper Functions:

-- This produces a list of ExprInputs which represent all the combinations of
-- input states in which a node state assignment might be true. It is not all
-- the possible inputs, just all the possible input sets you can make from the
-- list of gate state assignment expressions. This is still slight overkill, but
-- it is easy to extract and prepare. 
gateCombinations :: [NodeExpr] -> [ExprInput]
gateCombinations asns = fmap Map.fromList combosList
    where
        (nNames, nStates) = unzip $ refdNodesStates asns
        combosList = fmap (zip nNames) (sequenceA nStates)
-- # SCC gateCombinations #-}

-- Produces the nodes, and states of those nodes, that are referenced,
-- (explicitly or otherwise), in a given list of NodeExprs. 
refdNodesStates :: [NodeExpr] -> [(NodeName, [NodeState])]
refdNodesStates = MMap.toList . MMap.map L.nub . mconcat . fmap exprNodes

-- Extract a monoidal HashMap of the nodes of an expression, with all their
-- possible states. In case of a singular Not GateConst NodeName NodeState,
-- the function can't tell from inside the gate what other values the input
-- might take, except that we know that 0 and 1 must exist, so we guarantee 
-- them.
exprNodes :: NodeExpr -> MMap.MonoidalMap NodeName [NodeState]
exprNodes expr' = MMap.map (L.nub . ([0, 1] <>)) $ exprNodes' expr'
 where
  exprNodes' (GateLit _) = MMap.empty
  exprNodes' (GateConst nName nState) = MMap.singleton nName [nState]
  exprNodes' (Not expr) = exprNodes' expr
  exprNodes' (Binary And expr1 expr2) = (exprNodes' expr1) <> (exprNodes' expr2)
  exprNodes' (Binary Or expr1 expr2)  = (exprNodes' expr1) <> (exprNodes' expr2)


-- Generate all the possible inputs sets of a model, separated by layers. Each
-- ModelLayer will have a [[ExprInput]] of possible inputs, one [ExprInput] for
-- each node. The DMModel as a whole will have a [[[ExprInput]]]
modelCombinations :: DMModel -> [[[ExprInput]]]
modelCombinations = (layerCombinations <$>) . modelLayers
-- # SCC modelCombinations #-}

-- Generate all the possible inputs sets of a ModelLayer, one [ExprInput] for
-- each node. 
layerCombinations :: ModelLayer -> [[ExprInput]]
layerCombinations mL = fromJust <$> comboLists
    where
        comboLists = (nodeCombinations ranges) <$> nodes
        ranges :: LayerRange
        ranges = Map.fromList $ nodeRange <$> nodes
        nodes = layerNodes mL
-- # SCC layerCombinations #-}

-- If we have a LayerRange, this gives the possible inputs to a given node in
-- that ModelLayer. 
nodeCombinations :: LayerRange -> DMNode -> Maybe [ExprInput]
nodeCombinations r n = Map.fromList <<$>> combosList
    where
        combosList :: Maybe [[(NodeName, NodeState)]]
        combosList = (zip nNames) <<$>> (sequenceA <$> nStates)
        nStates :: Maybe [[NodeState]]
        nStates = sequenceA $ sequenceA (Map.lookup <$> nNames) r
        nNames :: [NodeName]
        nNames = fst <$> (refdNodesStates nExprs)
        nExprs = ((snd <$>) . gateAssigns . nodeGate) n
-- # SCC nodeCombinations #-}

-- Extract the names and ranges from a node. 
nodeRange :: DMNode -> NodeRange
nodeRange n = (name, range)
    where
        name = (nodeName . nodeMeta) n
        range = ((fst <$>) . gateAssigns . nodeGate) n


-- Extract all the DMNodes from a DMModel
modelNodes :: DMModel -> [[DMNode]]
modelNodes (Fine ml) = [layerNodes ml]
modelNodes (LayerBinding _ mL dmM) = (layerNodes mL) : (modelNodes dmM)

-- Extract all the ModelLayers from a DMModel
modelLayers :: DMModel ->  [ModelLayer]
modelLayers (Fine mL) = [mL]
modelLayers (LayerBinding _ mL dmM) = mL : (modelLayers dmM)

-- Extract the DMNodes from a ModelLayer
layerNodes :: ModelLayer -> [DMNode]
layerNodes = (snd <$>) . Gr.labNodes . modelGraph

-- Extract all the citation keys from a DMModel, include any ModelPapers. 
modelCiteKeys :: DMModel -> Set.HashSet BibTeXKey
modelCiteKeys (Fine ml) = Set.unions [modelKeys, linkKeys, nodeKeys, mPaperKeys]
  where
    modelKeys = (Set.fromList . lRefs . modelInfo . modelMeta) ml
    linkKeys = (Set.fromList . concat . ((lRefs . linkInfo . Gr.edgeLabel) <$>)
                . Gr.labEdges . modelGraph) ml
    nodeKeys = (Set.fromList . concat . ((lRefs . nodeInfo . nodeMeta . snd)
                <$>) . Gr.labNodes . modelGraph) ml
    mPaperKeys = (Set.fromList . modelPaper . modelMeta) ml
    lRefs ((_, xxs), (_, yys)) = (concat xxs) <> (concat yys)
modelCiteKeys (LayerBinding _ mLayer dmModel) =
    (modelCiteKeys (Fine mLayer)) `Set.union` (modelCiteKeys dmModel)

-- Extract all the names of the layers of  DMModel
layerNames :: DMModel -> [T.Text]
layerNames (Fine ml) = [(modelName . modelMeta) ml]
layerNames (LayerBinding _ mLayer dmModel) =
    ((modelName . modelMeta) mLayer) : (layerNames dmModel)

-- peel off the coarsest (topmost) ModelLayer of a DMModel
coarseLayer :: DMModel -> ModelLayer
coarseLayer (Fine ml) = ml
coarseLayer (LayerBinding _ mLayer _) = mLayer
