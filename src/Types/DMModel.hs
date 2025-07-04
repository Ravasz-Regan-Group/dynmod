{-# LANGUAGE OverloadedStrings #-}

module Types.DMModel
    ( DMModel(..)
    , ModelLayer(..)
    , ModelGraph
    , ModelMapping
    , Switch
    , Phenotype(..)
    , PhenotypeError(..)
    , SwitchName
    , PhenotypeName
    , SubSpace
    , IntSubSpace
    , DMMSModelMapping
    , SwitchProfile
    , ModelMeta(..)
    , ModelName
    , BiasOrder(..)
    , IntBiasOrder(..)
    , LocalColor
    , FileFormatVersion
    , DMNode(..)
    , NodeMeta(..)
    , LitInfo(..)
    , Note
    , Description
    , NodeGate(..)
    , GateOrder
    , TruthTable
    , GateOrigin(..)
    , NodeStateAssign
    , LogicalGate
    , TruthTableGate
    , NodeRange
    , LayerRange
    , NodeType(..)
    , EntrezGeneID
    , NodeName
    , NodeState
    , NodeIndex
    , NodeExpr(..)
    , BinOp(..)
    , ExprInput
    , DMLink(..)
    , LinkEffect(..)
    , LinkType(..)
    , ModelInvalid(..)
    , NodeInvalid(..)
    , GateInvalid(..)
    , NodeRefdNodesMismatch
    , StatesRefdStatesMisMatch
    , NodeInlinkMismatch
    , ModelLayerInvalid(..)
    , TableInvalid(..)
    , GateInLinkInvalid(..)
    , NodeMetaNameMismatch
    , DuplicateDMLinks
    , MissingDescription(..)
    , UndefinedNodeType
    , UnspecifiedNodeColor
    , MissingCoord
    , CoordWrongDimension
    , UndefinedLinkType
    , UndefinedEffectType
    , PrettyGateOutput
    , ModelTTFiles
    , DMMSNode
    , InAdj
    , biasOrderNodeName
    , inAdjs
    , findInAdjs
    , defaultColor
    , modelMappingSplit
    , gateEval
    , prettyGateEval
    , layerTTs
    , gateCombinations
    , refdNodesStatesNG
    , refdNodesStatesTM
    , exprNodes
    , modelLayers
    , modelNodes
    , modelMappings
    , coarseLayer
    , fineLayer
    , mkLayerBinding
    , findLayerWithBinding
    , layerNodes
    , layerRanges
    , nodeCombinations
    , nodeRange
    , mkLogicalGate
    , mkTableGate
    , tTableToAssigns
    , assignsToTTable
    , tTInputOutput
    , dmmsNodes
    , nonEmptyPhenotypes
    , modelNodes'
    , modelEdges'
    , modelCiteKeys
    , CitationDictionary
    , BibTeXKey
    , BibTeXEntry(..)
    , BibTeXField
    , BibTeXRecord
    , PubInvalid(..)
    , OrphanedModelCites
    , ExcessDictCites
    , CiteDictionaryInvalid(..)
    ) where

-- See the SUPPRESSED tag for functions that aren't used but might be useful in
-- the future. 

import Utilities
import Data.Hashable
import Text.LaTeX.Base.Class (fromLaTeX, commS)
import Text.LaTeX.Base.Commands (footnotesize)
import Text.LaTeX.Base.Math (math)
import Text.LaTeX.Base.Syntax (LaTeX(..))
import Text.LaTeX.Base.Texy (Texy(..))
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SC
import qualified Data.Vector.Unboxed as U
import Data.Vector.Instances()
import qualified Data.Text as T
import TextShow
import TextShow.Data.Char (showbLitString, showbChar)
import TextShow.Data.UnorderedContainers()
import TextShow.Data.Vector()
import qualified Data.Graph.Inductive as Gr
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Validation
import qualified Data.Versions as Ver
import qualified Data.List.Extra as L
import Data.Maybe (fromJust)
import qualified Data.Bifunctor as BF
import Data.Containers.ListUtils (nubInt)

-- We need a Hashable version of Colour to put them into HashMaps, but this will
-- really only work for a Double-esque a in Colour a. I'd rather not use a
-- newtype and then try to GeneralisedNewtypeDeriving all of Colour's instances.
-- I know I'd forget something. 
instance (Hashable a, Ord a, Floating a)=> Hashable (C.Colour a) where
    hashWithSalt salt colr =
        salt `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z
        where
            SC.RGB x y z = SC.toSRGB colr

-- Defining the types that will comprise a model,
-- to parse, verify, and run simulations

-- The Fine layer of a DMModel represents a genetic regualory network. Layers
-- above it are progressively coarser dynamically modular layers that represent 
-- the functions of that network. 

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
                           , biasOrderFirst :: [BiasOrder]
                           , biasOrderLast  :: [BiasOrder]
                           , modelInfo :: LitInfo
                           }
                           deriving (Show, Eq)
type ModelName = T.Text
data BiasOrder = WholeNode NodeName
               | SpecificState NodeName NodeState
               deriving (Show, Eq)
data IntBiasOrder = IntWholeNode NodeIndex
                  | IntSpecificState NodeIndex NodeState
                  deriving (Show, Eq)

biasOrderNodeName :: BiasOrder -> NodeName
biasOrderNodeName (WholeNode n) = n
biasOrderNodeName (SpecificState n _) = n

-- A bijective map from the nodes of a coarse layer to the elements of a
-- non-trivial partition of the nodes of its corresponding fine layer, and a
-- loose map from a subset of the states of the coarse node to subsets of
-- states of its fine nodes. 
type ModelMapping = [Switch]
type Switch = (SwitchName, ([NodeName],[Phenotype]))
-- To avoid confusion with NodeNames as individual DMNodes
type SwitchName = NodeName

-- A Phenotype maps a state of a switch node to a (possibly degenerate)
-- loop of SubSpaces of its constituent nodes, where SubSpaces are subsets
-- of the switch at particular states, thus constraining the whole ModelLayer
-- network to that particular subspace of possible states. If a Phenotype is a
-- loop, then at most one of its SubSpaces may be marked, i.e. it is
-- mutually satisfiable with at most one point Phenotype in that Switch. 
-- Loop Phenotypes may not share a point Phenotype each other. PhenotypeErrors
-- are arranged in a list of decreasing length. The starting Subspace of each
-- PhenotypeError must be the same as that of the parent Phenotype, and they
-- must be strict subloops of the parent Phenotype. This is because we do not
-- enforce the rule that each SubSpace in each PhenotypeError must be mutually
-- unsatisfiable with every other SubSpace in every other PhenotypeError of a
-- given Phenotype. Thus, we need to be careful when detecting PhenotypeErrors
-- in a thread, that we search for the the longest possible loop first, and then
-- move to progressively smaller incorrect Phenotypes. 
data Phenotype = Phenotype { phenotypeName :: PhenotypeName
                           , switchNodeState :: NodeState
                           , fingerprint :: [SubSpace]
                           , markedSubSpace :: Maybe SubSpace
                           , phenotypeErrors :: [PhenotypeError]
                           } deriving (Show, Eq, Ord)

type PhenotypeName = T.Text
type SubSpace = [(NodeName, NodeState)]
type IntSubSpace = [(NodeIndex, NodeState)]

-- PhenotypeErrors may not be the same as their parent Phenotypes, but they may
-- longer. They represent possible cellular disfunction. 
data PhenotypeError = PhError { phErrorName :: PhenotypeName
                              , phIndex :: Int
                              , phErrorFingerprint :: [SubSpace]
                              } deriving (Show, Eq, Ord)

-- These are some intermediary types we need to go from parsing ModelMapping{}
-- and SwitchProfiles{} in the DMMS file to a DMModel ModelMapping. They are
-- kept separate in the DMMS file to keep it easy see the whole structure of
-- each all at once. They are defined here to avoid an import cycle. 
type DMMSModelMapping = [(NodeName, [NodeName])]
type SwitchProfile = (NodeName, [Phenotype])

type ModelGraph = Gr.Gr DMNode DMLink

data DMLink = DMLink { linkEffect :: LinkEffect
                     , linkType :: LinkType
                     , linkInfo :: LitInfo }
                       deriving (Show, Eq, Ord)

data DMNode = DMNode { nodeMeta :: NodeMeta
                     , nodeGate :: NodeGate
                     } deriving (Show, Eq)

instance Ord DMNode where
    compare x y = compare (nodeMeta x) (nodeMeta y)

data NodeMeta = NodeMeta { nodeName :: NodeName
                         , nodeGenes :: [EntrezGeneID]
                         , nodeType :: NodeType
                         , nodeColor :: LocalColor
                         , nodeCoordinate :: U.Vector Double
                         , nodeInfo :: LitInfo
                         , inlinkOrder :: [NodeName]
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
data NodeGate = NodeGate { gNodeName :: NodeName
                         , gateOrder :: GateOrder
                         , gateAssigns :: [NodeStateAssign]
                         , gateTable :: TruthTable
                         , gateOrigin :: GateOrigin
                         }
                         deriving (Show, Eq)

type NodeStateAssign = (NodeState, NodeExpr)
type GateOrder = [NodeName]
type TruthTable = Map.HashMap (U.Vector NodeState) NodeState
data GateOrigin = LogicalExpression
                | DMMSTruthTable
                | Both
                deriving (Show, Eq)

-- The states a DMNode can take is checked at parse-time to be of the form
-- [0..n], where n is the highest state, and other states between it and zero
-- are included. So specifying the range of a DMNode requires only a single Int.
type NodeRange = (NodeName, NodeState)
type LayerRange = Map.HashMap NodeName NodeState


-- The first pair is a description, with accompanying list of \cites. 
-- The second pair is a note, with accompanying list of \cites. 
data LitInfo = LitInfo { desc :: Description
                       , descCiteLists :: CitationLists
                       , note :: Description
                       , noteCiteLists :: CitationLists
                       } deriving (Show, Eq, Ord)
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

-- Update. Because it will be important to change the influence of certain
-- clauses of discrete expressions on the fly, evaluating gates as discrete
-- logical expressions from the custom data type will be the way forward for
-- now. - Pete Regan January 31, 2022


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
              | Membrane_Potential
              | LncRNA
              | Cell_Surgace_Ligand
                deriving (Show, Eq, Bounded, Enum)

instance Texy NodeType where
    texy Undefined_NT       = (footnotesize . texy)
                                    ("Undefined_NT" :: T.Text)
    texy Cell               = (footnotesize . fromLaTeX . TeXRaw) "Cell"
    texy DM_Switch          = (footnotesize . fromLaTeX . TeXRaw) "DM"
    texy Connector          = (footnotesize . fromLaTeX . TeXRaw) "Conn"
    texy Environment        = (footnotesize . fromLaTeX . TeXRaw) "Env"
    texy Process            = (footnotesize . fromLaTeX . TeXRaw) "Proc"
    texy Macro_Structure    = (footnotesize . fromLaTeX . TeXRaw) "MSt"
    texy Metabolite         = (footnotesize . fromLaTeX . TeXRaw) "Met"
    texy MRNA               = (footnotesize . fromLaTeX . TeXRaw) "mRNA"
    texy MicroRNA           = (footnotesize . fromLaTeX . TeXRaw) "miR"
    texy Protein_Complex    = (footnotesize . fromLaTeX . TeXRaw) "PC"
    texy Receptor           = (footnotesize . fromLaTeX . TeXRaw) "Rec"
    texy Adaptor_Protein    = (footnotesize . fromLaTeX . TeXRaw) "Adap"
    texy Secreted_Protein   = (footnotesize . fromLaTeX . TeXRaw) "Secr"
    texy TF_Protein         = (footnotesize . fromLaTeX . TeXRaw) "TF"
    texy Kinase             = (footnotesize . fromLaTeX . TeXRaw) "K"
    texy Phosphatase        = (footnotesize . fromLaTeX . TeXRaw) "Ph"
    texy Ubiquitin_Ligase   = (footnotesize . fromLaTeX . TeXRaw) "UbL"
    texy Protease           = (footnotesize . fromLaTeX . TeXRaw) "PTase"
    texy DNase              = (footnotesize . fromLaTeX . TeXRaw) "DNase"
    texy CAM                = (footnotesize . fromLaTeX . TeXRaw) "CAM"
    texy CDK                = (footnotesize . fromLaTeX . TeXRaw) "CDK"
    texy CDKI               = (footnotesize . fromLaTeX . TeXRaw) "CDKI"
    texy GEF                = (footnotesize . fromLaTeX . TeXRaw) "GEF"
    texy GAP                = (footnotesize . fromLaTeX . TeXRaw) "GAP"
    texy GTPase             = (footnotesize . fromLaTeX . TeXRaw) "GTPa"
    texy Enzyme             = (footnotesize . fromLaTeX . TeXRaw) "Enz"
    texy Protein            = (footnotesize . fromLaTeX . TeXRaw) "Prot"
    texy Membrane_Potential = (footnotesize . fromLaTeX . TeXRaw) "MP"
    texy LncRNA             = (footnotesize . fromLaTeX . TeXRaw) "lncRNA"
    texy Cell_Surgace_Ligand
                            = (footnotesize . fromLaTeX . TeXRaw) "SLig"

instance TextShow NodeType where
    showb Undefined_NT = showbLitString "Undefined_NT"
    showb Cell = showbLitString "Cell"
    showb DM_Switch = showbLitString "DM_Switch"
    showb Connector = showbLitString "Connector"
    showb Environment = showbLitString "Environment"
    showb Process = showbLitString "Process"
    showb Macro_Structure = showbLitString "Macro_Structure"
    showb Metabolite = showbLitString "Metabolite"
    showb MRNA = showbLitString "MRNA"
    showb MicroRNA = showbLitString "MicroRNA"
    showb Protein_Complex = showbLitString "Protein_Complex"
    showb Receptor = showbLitString "Receptor"
    showb Adaptor_Protein = showbLitString "Adaptor_Protein"
    showb Secreted_Protein = showbLitString "Secreted_Protein"
    showb TF_Protein = showbLitString "TF_Protein"
    showb Kinase = showbLitString "Kinase"
    showb Phosphatase = showbLitString "Phosphatase"
    showb Ubiquitin_Ligase = showbLitString "Ubiquitin_Ligase"
    showb Protease = showbLitString "Protease"
    showb DNase = showbLitString "DNase"
    showb CAM = showbLitString "CAM"
    showb CDK = showbLitString "CDK"
    showb CDKI = showbLitString "CDKI"
    showb GEF = showbLitString "GEF"
    showb GAP = showbLitString "GAP"
    showb GTPase = showbLitString "GTPase"
    showb Enzyme = showbLitString "Enzyme"
    showb Protein = showbLitString "Protein"
    showb Membrane_Potential = showbLitString "Membrane_Potential"
    showb LncRNA = showbLitString "LncRNA"
    showb Cell_Surgace_Ligand = showbLitString "Cell_Surgace_Ligand"

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

instance TextShow LinkEffect where
    showb Undefined_LE = showbLitString "Undefined_LE"
    showb Activation = showbLitString "Activation"
    showb Repression = showbLitString "Repression"
    showb Context_Dependent = showbLitString "Context_Dependent"
    showb Inapt = showbLitString "Inapt"

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
                | Binding_Localization
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
                | Epigenetic
                | Transcription_Conflict
                | Secretion
                | RNAi
                | Acetylation
                | Deacetylation
                | Hydroxylation
                  deriving (Show, Eq, Ord, Bounded, Enum)

instance Texy LinkType where
    texy Undefined_LT         = (footnotesize . texy)
                                    ("Undefined_LT" :: T.Text)
    texy Enforced_Env         = (footnotesize . fromLaTeX . TeXRaw) "Env"
    texy Indirect             = (footnotesize . fromLaTeX . TeXRaw) "Ind"
    texy Complex_Process      = (footnotesize . fromLaTeX . TeXRaw) "ComplProc"
    texy Persistence          = (footnotesize . fromLaTeX . TeXRaw) "Per"
    texy Transcription        = (footnotesize . fromLaTeX . TeXRaw) "TR"
    texy Translation          = (footnotesize . fromLaTeX . TeXRaw) "TL"
    texy Ligand_Binding       = (footnotesize . fromLaTeX . TeXRaw) "Ligand"
    texy Complex_Formation    = (footnotesize . fromLaTeX . TeXRaw) "Compl"
    texy Inhibitory_Binding   = (footnotesize . fromLaTeX . TeXRaw) "IBind"
    texy Localization         = (footnotesize . fromLaTeX . TeXRaw) "Loc"
    texy Binding_Localization = (footnotesize . fromLaTeX . TeXRaw) "BLoc"
    texy Protective_Binding   = (footnotesize . fromLaTeX . TeXRaw) "PBind"
    texy Unbinding            = (footnotesize . fromLaTeX . TeXRaw) "Unbind"
    texy Phosphorylation      = (footnotesize . fromLaTeX . TeXRaw) "P"
    texy Dephosphorylation    = (footnotesize . fromLaTeX . TeXRaw) "DP"
    texy Phosphorylation_Localization
                              = (footnotesize . fromLaTeX . TeXRaw) "PLoc"
    texy Ubiquitination       = (footnotesize . fromLaTeX . TeXRaw) "Ubiq"
    texy Degradation          = (footnotesize . fromLaTeX . TeXRaw) "Deg"
    texy GEF_Activity         = (footnotesize . fromLaTeX . TeXRaw) "GEF"
    texy GAP_Activity         = (footnotesize . fromLaTeX . TeXRaw) "GAP"
    texy Proteolysis          = (footnotesize . fromLaTeX . TeXRaw) "Lysis"
    texy Catalysis            = (footnotesize . fromLaTeX . TeXRaw) "Cat"
    texy Epigenetic           = (footnotesize . fromLaTeX . TeXRaw) "Epi"
    texy Transcription_Conflict
                              = (footnotesize . fromLaTeX . TeXRaw) "TrConf"
    texy Secretion            = (footnotesize . fromLaTeX . TeXRaw) "Secr"
    texy RNAi                 = (footnotesize . fromLaTeX . TeXRaw) "RNAi"
    texy Acetylation          = (footnotesize . fromLaTeX . TeXRaw) "Acet"
    texy Deacetylation        = (footnotesize . fromLaTeX . TeXRaw) "Deacet"
    texy Hydroxylation        = (footnotesize . fromLaTeX . TeXRaw) "-OH"

instance TextShow LinkType where
    showb Undefined_LT = showbLitString "Undefined_LT"
    showb Enforced_Env = showbLitString "Enforced_Env"
    showb Indirect = showbLitString "Indirect"
    showb Complex_Process = showbLitString "Complex_Process"
    showb Persistence = showbLitString "Persistence"
    showb Transcription = showbLitString "Transcription"
    showb Translation = showbLitString "Translation"
    showb Ligand_Binding = showbLitString "Ligand_Binding"
    showb Complex_Formation = showbLitString "Complex_Formation"
    showb Inhibitory_Binding = showbLitString "Inhibitory_Binding"
    showb Localization = showbLitString "Localization"
    showb Binding_Localization = showbLitString "Binding_Localization"
    showb Protective_Binding = showbLitString "Protective_Binding"
    showb Unbinding = showbLitString "Unbinding"
    showb Phosphorylation = showbLitString "Phosphorylation"
    showb Dephosphorylation = showbLitString "Dephosphorylation"
    showb Phosphorylation_Localization =
        showbLitString "Phosphorylation_Localization"
    showb Ubiquitination = showbLitString "Ubiquitination"
    showb Degradation = showbLitString "Degradation"
    showb GEF_Activity = showbLitString "GEF_Activity"
    showb GAP_Activity = showbLitString "GAP_Activity"
    showb Proteolysis = showbLitString "Proteolysis"
    showb Catalysis = showbLitString "Catalysis"
    showb Epigenetic = showbLitString "Epigenetic"
    showb Transcription_Conflict = showbLitString "Transcription_Conflict"
    showb Secretion = showbLitString "Secretion"
    showb RNAi = showbLitString "RNAi"
    showb Acetylation = showbLitString "Acetylation"
    showb Deacetylation = showbLitString "Deacetylation"
    showb Hydroxylation = showbLitString "Hydroxylation"

type EntrezGeneID = Int
type NodeName = T.Text
type NodeState = Int
type NodeIndex = Int
type ExprInput = Map.HashMap NodeName NodeState
data NodeCondition = NodeCondition { currentState :: NodeState 
                                   , destinationState :: NodeState
                                   , fractionDone :: Double
                                   , fractionDelta :: Double
                                   , transitionType :: TransitionType }
                                     deriving (Show, Eq)


type TransitionType = T.Text

-- When we render a parsed DMModel back to T.Text, to write a dmms file, it is
-- convenient to have these, since that is how they are organized in the .dmms.
type DMMSNode = (DMNode, [DMMSLink])
type DMMSLink = (NodeName, DMLink)

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

prettyTTable :: NodeName -> GateOrder -> TruthTable -> (NodeName, T.Text)
prettyTTable nN gO tT = (nN, gatePrint <> T.singleton '\n' <> prettyRows)
    where
        prettyRows = T.unlines textRows
        textRows = (T.concat . (L.intersperse (T.singleton '\t')) . fmap showt)
            <$> joinedRows
        joinedRows = (\(v, y) -> U.toList v <> [y]) <$> rows
        rows = (L.sortOn fst . Map.toList) tT
        gatePrint = T.concat $ L.intersperse (T.singleton '\t') $ gO <> [nN]

-- Produce a [NodeStateAssign] from a TruthTable. 
tTableToAssigns :: NodeName -> GateOrder -> TruthTable -> [NodeStateAssign]
tTableToAssigns nN gO tT = tableToLogical (gO <> [nN]) inputRows outputs
    where
        (inputRows, outputs) = unzip $ deVec <$> (Map.toList tT)
        deVec (v, o) = (U.toList v, o)        

assignsToTTable :: GateOrder -> [NodeStateAssign] -> TruthTable
assignsToTTable gO ass = Map.fromList inputOutputPairs
    where
        inputOutputPairs = zip inputs $ (fromJust . gateEval ass) <$> combos
        inputs = U.fromList <$> ((snd <$>) <$> combosList)
        combos = Map.fromList <$> combosList 
        combosList = (zip nNames) <$> (sequenceA nStates)
        (nNames, nStates) = unzip orderedStateRanges
--     Make sure that the comboList ends up ordered by the NodeGates GateOrder
        orderedStateRanges = sortWithOrderOn fst gO stateRanges
        stateRanges = refdNodesStatesNG $ snd <$> ass

-- Convert a TruthTabe to a List of (ExprInput, NodeState). Useful when you need
-- to compare a TruthTable to NodeGate. Ordered as you would write out the rows
-- or a truth table (eg 0001, 0010, 0011, 0100, etc), though including the 
-- possibility of some columns whose maximum is > 1 in the case of integer
-- rather than boolean gates. 
tTInputOutput :: GateOrder -> TruthTable -> [(ExprInput, NodeState)]
tTInputOutput gO tT = zip exprInputs outputs
    where
        exprInputs = (Map.fromList . (zip gO)) <$> inputLists
        inputLists = U.toList <$> vecs
        (vecs, outputs) = unzip $ (L.sortOn fst . Map.toList) tT

data NodeExpr =
    GateLit Bool
  | GateConst NodeName NodeState
  | Not NodeExpr
  | Binary BinOp NodeExpr NodeExpr
  | Pars NodeExpr
  deriving (Eq)

instance Show NodeExpr where
    show (GateLit b) = show b
    show (GateConst n s) = show n ++ ":" ++ show s
    show (Not expr) = "not " ++ show expr
    show (Binary And expr1 expr2) =
        show expr1 ++ " and " ++ show expr2
    show (Binary Or expr1 expr2) =
        show expr1 ++ " or " ++ show expr2
    show (Pars expr) = "(" <> show expr <> ")"

instance TextShow NodeExpr where
    showb (GateLit b) = showb b
    showb (GateConst n s) = showb n <> showbChar ':' <> showb s
    showb (Not expr) = showbLitString "not " <> showb expr
    showb (Binary And expr1 expr2) =
        showb expr1 <> showbLitString " and " <> showb expr2
    showb (Binary Or expr1 expr2) =
        showb expr1 <> showbLitString " or " <> showb expr2
    showb (Pars expr) = showbChar '(' <> showb expr <> showbChar ')'

data BinOp
  = And
  | Or
  deriving (Show, Eq)

-- Evaluate a node expression
eval :: NodeExpr -> ExprInput -> Maybe Bool
eval (Not expr) ns = not <$> (eval expr ns)
eval (Pars expr) ns = eval expr ns
eval (Binary And expr1 expr2) ns =
    liftA2 (&&) (eval expr1 ns) (eval expr2 ns)
eval (Binary Or expr1 expr2) ns =
    liftA2 (||) (eval expr1 ns) (eval expr2 ns)
eval (GateConst nName nState) ns =
    (nState ==) <$> (Map.lookup nName ns)
eval (GateLit b) _ =  Just b

-- Evaluate a gate against a given input HashMap
gateEval :: [NodeStateAssign] -> ExprInput -> Maybe NodeState
gateEval assigns nInput
    | areInputsSufficient = (L.findIndex (== Just True) output)
    | otherwise = Nothing
    where
        exprs = snd <$> assigns
        output = (flip eval nInput) <$> exprs
        areInputsSufficient = not $ elem Nothing output

-- Evaluate a gate against a given input HashMap, and return the result as tab
-- separated Text formatted integers in proper gateOrder order. Use only on
-- ExprInputs that contain all the necessary inputs
prettyGateEval :: GateOrder
               -> [NodeStateAssign]
               -> ExprInput
               -> PrettyGateOutput
prettyGateEval gO assigns nInput = prettify output
    where
        prettify = (((prettyInput <> T.singleton '\t') <>) . T.pack . show)
        prettyInput = (T.intersperse '\t' . T.concat . fmap showt) orderedInput
        orderedInput = fromJust <$> (sequenceA (Map.lookup <$> gO) nInput)
        output = fromJust $ gateEval assigns nInput

type PrettyGateOutput = T.Text
type LayerTTFiles = (T.Text, [(NodeName, T.Text)])
type ModelTTFiles = [LayerTTFiles]

-- Generate T.Texts for TT files (csv files with a truth table in each) from the
-- nodes in a ModelLayer, along with the name of that layer and names of the
-- nodes. 
layerTTs :: ModelLayer -> LayerTTFiles
layerTTs mL = (mName, prettyTTables)
    where
        mName = (modelName . modelMeta) mL
        prettyTTables = (uncurry3 prettyTTable) <$> triplets
        triplets = zip3 nodeNames gateOrders nodeTables
        nodeTables = (gateTable . nodeGate) <$> nodes
        gateOrders = (gateOrder . nodeGate) <$> nodes
        nodeNames = (nodeName . nodeMeta) <$> nodes
        nodes = layerNodes mL

-- Make the top line of the appropriate Truth Table for a DMNode SUPPRESSED
-- mkGatePrint :: DMNode -> T.Text
-- mkGatePrint node = T.concat $ L.intersperse (T.singleton '\t') $ o <> [n]
--     where
--         o = (gateOrder . nodeGate) node
--         n = (nodeName . nodeMeta) node

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
findInAdjs :: NodeName -> [InAdj] -> Maybe InAdj
findInAdjs n = L.find ((\x (dmN, _) -> x == (nodeName . nodeMeta) dmN) n)


-- Error handling types

data ModelInvalid =
    DuplicateCoarseMapNodes [NodeName]
  | DuplicateFineMapNodes [NodeName]
  | ExcessFineMapNodes [NodeName]
  | MissingFineMapNodes [NodeName]
  | ExcessCoarseMapNodes [NodeName]
  | MissingCoarseMapNodes [NodeName]
  | FineInMultipleCoarse [NodeName]
  | DuplicatedNodeNames [NodeName]
  | DuplicateProfileSwitches [NodeName]
  | ExcessProfileSwitches [NodeName]
  | ProfileSwitchNotInModelGraph NodeName
  | MissingPhenotypes (NodeName, [NodeState])
  | ExcessPhenotypes (NodeName, [NodeState])
  | RepeatedSubSpaceNode (NodeName, PhenotypeName, NodeState, [NodeName])
  | RepeatedPhErrSubSpaceNode (NodeName, PhenotypeName, Int, [NodeName])
  | SubSpaceIsASubSet (SubSpace, [SubSpace])
  | DuplicatedPhenotypeNames [NodeName]
  | PhErrorsStartDifferentlyFromPh (PhenotypeName, [PhenotypeName])
  | PhenotypeandPhErrorCyclicPermute (PhenotypeName, [PhenotypeName])
  | CyclicPermutePhErrorSubSpaces [[PhenotypeName]]
  | PhenotypeReferenceAreCoarse [(PhenotypeName, [T.Text])]
  | PhNegStates (NodeName, [NodeState])
  | PhErrNegStates (PhenotypeName, [NodeState])
  | PhOutOfOrder (NodeName, [NodeState])
  | PhErrorOutOfOrder (PhenotypeName, [NodeState])
  | PhMissingOrTooHigh (NodeName, [NodeState])
  | PhErrMissingOrTooHigh (PhenotypeName, [NodeState])
  | PhDuplicateAssigns (NodeName, [NodeState])
  | PhErrDuplicateAssigns (NodeName, [Int])
  | SubSpaceNodesNotInSwitch (NodeName, [NodeName])
  | UnknownSubSpaceNode NodeName
  | InvalidSubSpaceNodeValue (NodeName, NodeState)
  | MutuallySatisfiablePointPhSubSpaces
                        ((PhenotypeName, SubSpace), (PhenotypeName, SubSpace))
  | MutuallySatisfiableLoopPhSubSpaces
                        ((PhenotypeName, SubSpace), (PhenotypeName, SubSpace))
  | ExcessLoopPointSubSpaceMatches PhenotypeName [(PhenotypeName, SubSpace)]
  | ExcessPointSubSpaceMatches (((PhenotypeName, SubSpace))
                              , ((PhenotypeName, [SubSpace])))
  | DuplicatedModelNames [NodeName]
  | DuplicatedBiasOrderNodeNames [NodeName]
  | DuplicatedBiasOrderNodeStates [(NodeName, NodeState)]
  | UnknownNodesInBiasOrder [NodeName]
  | OOBBiasOrderStates [(NodeName, (Int, Int), Int)]
-- | MissingCitations MissingCitations
    deriving (Show, Eq)
-- type MissingCitations = [BibTeXKey]

data GateInvalid = InconsistentNames
                 | DuplicateAssigns
                 | ZeroAssigned
                 | MissingOrTooHigh [NodeState]
                 | OutOfOrder
                 | EmptyGate
                 | ContradictoryExprSet ContradictoryExprSet
                 | TableExprInNodeMismatch TableExprInNodeMismatch
                 | TableExprStateMismatch TableExprStateMismatch
                 | TableExprOutputMismatch TableExprOutputMismatch
                 | TableDisNameMismatch TableDisNameMismatch
                 | TruthTableIncomplete TruthTableIncomplete
    deriving (Show, Eq)

instance TextShow GateInvalid where
    showb InconsistentNames = showbLitString "InconsistentNames"
    showb DuplicateAssigns = showbLitString "DuplicateAssigns"
    showb ZeroAssigned = showbLitString "ZeroAssigned"
    showb (MissingOrTooHigh nSts) =
        showbLitString "MissingOrTooHigh" <> showbSpace <> showbList nSts
    showb OutOfOrder = showbLitString "OutOfOrder"
    showb EmptyGate = showbLitString "EmptyGate"
    showb (ContradictoryExprSet ceSet) =
        showbLitString "ContradictoryExprSet" <> showbSpace <> showb ceSet
    showb (TableExprInNodeMismatch nNames) =
        showbLitString "TableExprInNodeMismatch" <> showbSpace <>
            showbList nNames
    showb (TableExprStateMismatch mMatchT) =
        showbLitString "TableExprStateMismatch" <> showbSpace <> showb mMatchT
    showb (TableExprOutputMismatch mMatch) =
        showbLitString "TableExprOutputMismatch" <> showbSpace <> showb mMatch
    showb (TableDisNameMismatch mMatch) =
        showbLitString "TableDisNameMismatch" <> showbSpace <> showb mMatch
    showb (TruthTableIncomplete ttInc) =
        showbLitString "TruthTableIncomplete" <> showbSpace <> showb ttInc

-- If there are internal contradictions in a gate, this provides the relevant
-- expressions, expression states, inputs, and gate states affected. 
type ContradictoryExprSet = 
        [([NodeExpr], [Int], ExprInput, [Maybe Bool])]
-- type TableExprMismatch = (([ExprInput], [Maybe NodeState])
--                               , ([ExprInput], [Maybe NodeState]))
type TableExprInNodeMismatch = [NodeName]
type TableExprStateMismatch = T.Text
type TableExprOutputMismatch = (T.Text, [NodeStateAssign])
type TableDisNameMismatch = (NodeName, NodeName)
type TruthTableIncomplete = [((NodeName, NodeState), (NodeName, NodeState))] 

data TableInvalid = IncompleteOrOversizedRow
                  | InsufficientRows
                  | ExcessRows
                  | OutOfOrderRows
                  | DuplicatedInputRows
                  | MissingOrTooHighOutputs 
    deriving (Show, Eq)

data GateInLinkInvalid = GateInLinkMismatch GateInLinkMismatch
    deriving (Show, Eq)
type GateInLinkMismatch = [NodeName]

data NodeInvalid = NodeMetaNameMismatch NodeMetaNameMismatch
                 | DuplicateDMLinks DuplicateDMLinks
    deriving (Show, Eq)
type NodeMetaNameMismatch = (NodeName, NodeName) 
type DuplicateDMLinks = [NodeName]

data ModelLayerInvalid = 
                    NodeRefdNodesMismatch NodeRefdNodesMismatch
                  | StatesRefdStatesMisMatch StatesRefdStatesMisMatch
                  | NodeInlinkMismatch NodeInlinkMismatch
--                   The InLink's LinkAffect is mismatched with its actual
--                   behavior. This LinkEffect is the correct one. 
                  | NodeInLinkLinkEffectMismatch LinkEffect
                  | NodeNamesRepeated NodeNamesRepeated
                  | NodeDimensionsInconsistent NodeDimensionsInconsistent
     deriving (Show, Eq)
type NodeRefdNodesMismatch = [NodeName] -- Nodes in NodeExprs that are not in
                                        -- any node
type StatesRefdStatesMisMatch = [(NodeName, ([NodeState], [NodeState]))]
type NodeInlinkMismatch = [NodeName]
type NodeNamesRepeated = [NodeName]
type NodeDimensionsInconsistent = [Int]

data CiteDictionaryInvalid = RepeatedKeys RepeatedKeys
    deriving (Show, Eq)
type RepeatedKeys = [BibTeXKey]

data PubInvalid =   UndefinedNodeType UndefinedNodeType
                  | UndefinedLinkType UndefinedLinkType
                  | UndefinedEffectType UndefinedEffectType
                  | PubMissingDesc MissingDescription
                  | DescUnescapedUnderScores DescText
                  | UnspecifiedNodeColor UnspecifiedNodeColor
                  | MissingCoord MissingCoord
                  | CoordWrongDimension CoordWrongDimension
                  | OrphanedModelCites OrphanedModelCites
                  | ExcessDictCites ExcessDictCites
                  | GateFromTable GateFromTable
                  | OverlappingNodes [((NodeName, NodeName), Double)]
    deriving (Show, Eq, Ord)

 -- The name of the piece missing a description, and its associated type. 
data MissingDescription = ModelD T.Text
                        | NodeD T.Text
                        | InLinkD T.Text
                        deriving (Eq, Show, Ord)
type DescText = T.Text -- A Text literal with an unescaped underscore somewhere.
type UndefinedNodeType = NodeName 
type UnspecifiedNodeColor = T.Text --Notice to pick an SVG color
type MissingCoord = T.Text
type CoordWrongDimension = T.Text
-- Associated NodeName and a list possible types.
type UndefinedLinkType = (NodeName, NodeName) -- The associated Node
type UndefinedEffectType = (NodeName, NodeName) -- The associated Node
type OrphanedModelCites = (T.Text, [BibTeXKey])
type ExcessDictCites = (T.Text, [BibTeXKey])
type GateFromTable = NodeName


-- It would be very easy to assemble DMModels with nonsensical maps between
-- model layers, so we never create a LayerBinding directly, only through this
-- smart constructor. This ensures that the coarse nodes and fine nodes
-- from the map are bijective to the the nodes in the coarse and fine
-- ModelLayers, respectively, and also that the mapping itself from
-- fine -> coarse is surjective. 
mkLayerBinding :: DMMSModelMapping -> [SwitchProfile] -> ModelLayer -> DMModel
               -> Validation [ModelInvalid] DMModel
mkLayerBinding dmmsMMap sProfiles mLayer mModel =
    ((dmmsMMap, sProfiles, mLayer, mModel)          <$
    (noCoarseDupes $ fst <$> dmmsMMap)              <*
    (traverse noFineDupes $ snd <$> dmmsMMap)       <*
    fineNodesmatch mapFineNodes mNodes              <*
    coarseNodesMatch  mapCoarseNodes graphNodes     <*
    isMapSurjective dmmsMMap) `bindValidation` mkModelMapping
        where
            mapFineNodes = Set.toList $ Set.unions mapFineSets
            mapCoarseNodes = Set.toList $ Set.fromList $ fst <$> dmmsMMap
            graphNodes = (nodeName . nodeMeta . snd) <$> graphGrNodes
            mNodes = (nodeName . nodeMeta . snd) <$> modelGrNodes
            mapFineSets = (Set.fromList . snd) <$> dmmsMMap
            graphGrNodes = (Gr.labNodes . modelGraph) mLayer
            modelGrNodes = (Gr.labNodes . modelGraph . coarseLayer) mModel

-- Check that there are no duplicates in the coarse nodes
noCoarseDupes :: [NodeName] -> Validation [ModelInvalid] [NodeName]
noCoarseDupes cs = case repeated cs of
    []   -> Success cs
    errs -> Failure $ [DuplicateCoarseMapNodes errs]

noFineDupes :: [NodeName] -> Validation [ModelInvalid] [NodeName]
noFineDupes cs = case repeated cs of
    []   -> Success cs
    errs -> Failure $ [DuplicateFineMapNodes errs]

-- Check that the fine-grain nodes from a ModelMapping are the same up to
-- permutation as the nodes from the DMModel. 
fineNodesmatch :: [NodeName] -> [NodeName]
               -> Validation [ModelInvalid] [NodeName]
fineNodesmatch mapFineNodes moNodes
    | arePermutes mapFineNodes moNodes = Success mapFineNodes
    | isSubset mapFineNodes moNodes    = Failure [MissingFineMapNodes $
                                            moNodes L.\\ mapFineNodes]
    | otherwise                           = Failure [ExcessFineMapNodes $
                                            mapFineNodes L.\\ moNodes]

-- Check that the coarse-grain nodes from a ModelMapping are the same up to
-- permutation as the nodes from the ModelGraph. This assumes that there are no
-- duplicate coarse-grained nodes, as that is checked elsewhere. 
coarseNodesMatch :: [NodeName] -> [NodeName]
                 -> Validation [ModelInvalid] [NodeName]
coarseNodesMatch mapCoarseNodes graphNodes
    | arePermutes mapCoarseNodes graphNodes = Success mapCoarseNodes
    | isSubset mapCoarseNodes graphNodes    = Failure [MissingCoarseMapNodes $
                                            graphNodes L.\\ mapCoarseNodes]
    | otherwise                             = Failure [ExcessCoarseMapNodes $
                                            mapCoarseNodes L.\\ graphNodes]

-- Check that the ModelMapping itself is surjective. This assumes that the fine
-- and coarse nodes from the ModelMapping do in fact correspond exactly to the
-- nodes in the DMModel and ModelGraph, respectively, as this is tested
-- elsewhere. 
isMapSurjective :: DMMSModelMapping -> Validation [ModelInvalid] [NodeName]
isMapSurjective ms
    | dupes == Set.empty = Success $ fst <$> ms
    | otherwise          = Failure [FineInMultipleCoarse $ Set.toList dupes]
    where
        dupes = nIntersection sets
        sets = (Set.fromList . snd) <$> ms

-- The Phenotypes and NodeNames in a ModelMapping are parsed separately in a
-- DMMS file. Here we combine them and check that the SwitchProfiles do not
-- conflict with the ModelMapping. This presumes that the DMMSModelMapping has
-- already been validated in mkLayerBinding. 
mkModelMapping :: (DMMSModelMapping, [SwitchProfile], ModelLayer, DMModel)
               -> Validation [ModelInvalid] DMModel
mkModelMapping (dmmsMMap, sProfiles, mLayer, mModel) = 
    (traverse phIsMonotonic sProfiles)             *>
    (traverse phNoMissingOrTooHigh sProfiles)      *>
    (traverse phNoDupes sProfiles)                 *>
    noDupeSwitches sProfiles                       *>
    noExtraSwitches dmmsMMap sProfiles             *>
    allSwitchStatesCovered sProfiles mLayer        *>
    noSubSpaceRepeatedNodes sProfiles              *>
    noSubSpaceSubSets sProfiles                    *>
    subSpaceNodesInSwitch dmmsMMap sProfiles       *>
    subSpaceNodeStatesCovered fineMLayer sProfiles *>
    (LayerBinding <$> mm <*> pure mLayer <*> pure mModel)
    where
        mm = case traverse wellFormedPh sProfiles of
            Failure errs -> Failure errs
            Success markedSProfiles -> Success $
                foldr (mappingsMatcher markedSProfiles) [] dmmsMMap
        fineMLayer = coarseLayer mModel

-- Fold together a DMMSModelMapping and a [SwitchProfile] into a ModelMapping.
-- NOTE! This assumes that all checks have been performed! Do not use outside of
-- mkModelMapping or subSpaceNodesInSwitch!
mappingsMatcher :: [SwitchProfile]
                -> (NodeName, [NodeName])
                -> [Switch]
                -> [Switch]
mappingsMatcher sPrfs (nN, nNs) switches = case L.find ((== nN) . fst) sPrfs of
    Nothing -> (nN, (nNs, [])):switches
    Just (_, phenotypes) -> (nN, (nNs, phenotypes)):switches
        

-- Check that the Phenotype state assignments are listed in increasing monotonic
-- order and non-negative. Repeat this for any PhenotypeErrors. 
phIsMonotonic :: SwitchProfile -> Validation [ModelInvalid] SwitchProfile
phIsMonotonic sProfile@(pName, phs)
    | (not . null . filter (< 0)) sts = Failure [PhNegStates (pName, sts)]
    | L.sort sts /= sts = Failure [PhOutOfOrder (fst sProfile, sts)]
    | otherwise = (traverse phErrsAreMonotonic taggedErrs) *> Success sProfile
    where
        taggedErrs = zip (repeat pName) phErrorss
        phErrorss = filter (not . null) $ phenotypeErrors <$> phs
        sts = switchNodeState <$> phs

-- Check that the PhenotypeError indices are listed in increasing monotonic
-- order and non-negative. 
phErrsAreMonotonic :: (PhenotypeName, [PhenotypeError])
                   -> Validation [ModelInvalid] [PhenotypeError]
phErrsAreMonotonic (phName, phErrs)
    | (not . null . filter (< 0)) sts = Failure [PhErrNegStates (phName, sts)]
    | L.sort sts /= sts = Failure [PhErrorOutOfOrder (phName, sts)]
    | otherwise = Success phErrs
    where
        sts = phIndex <$> phErrs

-- Check that there are no missing (or, equivalently, too high) Phenotype state 
-- assignments. Repeat this for any PhenotypeErrors. 
phNoMissingOrTooHigh :: SwitchProfile
                     -> Validation [ModelInvalid] SwitchProfile
phNoMissingOrTooHigh sProfile@(pName, phs)
    | (maximum sts + 1) == (length . nubInt) sts =
        (traverse phErrNoMissingOrTooHigh taggedErrs) *> Success sProfile
    | otherwise = Failure [PhMissingOrTooHigh (pName, sts)]
    where
        taggedErrs = zip (repeat pName) phErrorss
        phErrorss = filter (not . null) $ phenotypeErrors <$> phs
        sts = switchNodeState <$> phs

-- Check that there are no missing (or, equivalently, too high) PhenotypeError 
-- indices. 
phErrNoMissingOrTooHigh :: (PhenotypeName, [PhenotypeError])
                        -> Validation [ModelInvalid] [PhenotypeError]
phErrNoMissingOrTooHigh (phName, phErrs)
    | (maximum sts + 1) == (length . nubInt) sts = Success phErrs
    | otherwise = Failure [PhErrMissingOrTooHigh (phName, sts)]
    where
        sts = phIndex <$> phErrs

-- Check that there are no duplicate Phenotype state assignments. Repeat this
-- for any PhenotypeErrors. 
phNoDupes :: SwitchProfile -> Validation [ModelInvalid] SwitchProfile
phNoDupes sProfile@(pName, phs)
    | (length sts) == (length . nubInt) sts =
        (traverse phErrNoDupes taggedErrs) *> Success sProfile
    | otherwise = Failure [PhDuplicateAssigns (pName, sts)]
    where
        taggedErrs = zip (repeat pName) phErrorss
        phErrorss = filter (not . null) $ phenotypeErrors <$> phs
        sts = switchNodeState <$> phs

-- Check that there are no duplicate PhenotypeError assignments. 
phErrNoDupes :: (PhenotypeName, [PhenotypeError])
             -> Validation [ModelInvalid] [PhenotypeError]
phErrNoDupes (phName, phErrs)
    | (length sts) == (length . nubInt) sts = Success phErrs
    | otherwise = Failure [PhErrDuplicateAssigns (phName, sts)]
    where
        sts = phIndex <$> phErrs

-- Check that there are no two SwitchPhenotypes with the same SwitchName. 
noDupeSwitches :: [SwitchProfile] -> Validation [ModelInvalid] [SwitchProfile]
noDupeSwitches sProfiles = case repeated (fst <$> sProfiles) of
    []   -> Success sProfiles
    dSws -> Failure $ [DuplicateProfileSwitches dSws]

-- Check that there are no SwitchPhenotypes that do not have a corresponding
-- ModelMapping{ Switch. 
noExtraSwitches :: DMMSModelMapping -> [SwitchProfile]
                -> Validation [ModelInvalid] [SwitchProfile]
noExtraSwitches dmmsMMap sProfiles = case excessSWs of
    []  -> Success sProfiles
    _ -> Failure $ [ExcessProfileSwitches excessSWs]
    where
        uqs = (L.sort . L.nubOrd) (fst <$> sProfiles)
        excessSWs = uqs L.\\ (fst <$> dmmsMMap)

-- Check that each SwitchPhenotype has a corresponding DMNode, and that these
-- have the same number of states. 
allSwitchStatesCovered :: [SwitchProfile] -> ModelLayer
                       -> Validation [ModelInvalid] [SwitchProfile]
allSwitchStatesCovered sProfiles mLayer =
    traverse (sStatesCovered mLayer) sProfiles

sStatesCovered :: ModelLayer -> SwitchProfile
               -> Validation [ModelInvalid] SwitchProfile
sStatesCovered mLayer sProfile@(sName, phs) = case L.find isSName nsPairs of
    Nothing -> Failure [ProfileSwitchNotInModelGraph sName]
    Just (_, nStates)
        | L.sort sStates == L.sort nStates -> Success sProfile
        | sStates L.\\ nStates == []
                -> Failure [MissingPhenotypes (sName, nStates L.\\ sStates)]
        | otherwise
                -> Failure [ExcessPhenotypes (sName, sStates L.\\ nStates)]
    where
        isSName = (== sName) . fst
        sStates = switchNodeState <$> phs
        nsPairs = nsPair <$> (layerNodes mLayer)
        nsPair n = ( (gNodeName . nodeGate) n
                   , (states . gateAssigns . nodeGate) n)

-- Internally to a SubSpace, no node should be repeated. 
noSubSpaceRepeatedNodes :: [SwitchProfile]
                        -> Validation [ModelInvalid] [SwitchProfile]
noSubSpaceRepeatedNodes sProfiles = traverse go sProfiles
  where
    go :: SwitchProfile -> Validation [ModelInvalid] SwitchProfile
    go (sName, phenotypes) = case traverse (go' sName) phenotypes of
      Failure modInvss1 -> Failure modInvss1
      Success phTss -> Success $ (,) sName (concat phTss)
      where
        go' :: NodeName -> Phenotype -> Validation [ModelInvalid] [Phenotype]
        go' sN ph@(Phenotype phN snNS sbSps _ phErrs) =
          (traverse (go'' sN phN snNS) sbSps) *>
          (traverse (noPhErrSSRepdNodes sN) phErrs) *>
          Success [ph]
          where
            go'' :: NodeName -> PhenotypeName -> NodeState -> SubSpace
                 -> Validation [ModelInvalid] SubSpace
            go'' sN1 phN1 snNS1 sbSps1
              | null repeatedNNames = Success sbSps1
              | otherwise = Failure [RepeatedSubSpaceNode err]
              where
                repeatedNNames :: [NodeName]
                repeatedNNames = repeated (fst <$> sbSps1)
                err = (sN1, phN1, snNS1, repeatedNNames)

-- Internally to a SubSpace, no node should be repeated, PhenotypeError check. 
noPhErrSSRepdNodes :: NodeName -> PhenotypeError
                   -> Validation [ModelInvalid] PhenotypeError
noPhErrSSRepdNodes swName phError = 
    traverse (go phErrI) phErrSbSpcs *> Success phError
    where
        phErrI = phIndex phError
        phErrSbSpcs = phErrorFingerprint phError
        go :: Int -> SubSpace -> Validation [ModelInvalid] SubSpace
        go phEI sbSpc
            | null repeatedNNames = Success sbSpc
            | otherwise = Failure [RepeatedPhErrSubSpaceNode err]
            where
                repeatedNNames :: [NodeName]
                repeatedNNames = repeated $ fst <$> sbSpc
                err = (swName, phErrorName phError, phEI, repeatedNNames)

-- In the whole of a ModelMapping, no SubSpace of a Phenotype (as opposed to a 
-- PhenotypeError) should be a subset of any other.
noSubSpaceSubSets :: [SwitchProfile]
                  -> Validation [ModelInvalid] [SwitchProfile]
noSubSpaceSubSets sProfiles =
    case fst $ L.foldl' f ([], subSpaceSets) subSpaceSets of
    []   -> Success sProfiles
    errs -> sequenceA errs
    where
        f (acc, []) _ = (acc, [])
        f (acc, _:remainingHS) sSSet = (newAcc, remainingHS)
            where
                newAcc = case supersets of
                    []  -> acc
                    _ -> (Failure [SubSpaceIsASubSet 
                            (Set.toList sSSet, supersets)]) : acc
                    where
                        supersets = Set.toList <$>
                                (filter (Set.isSubsetOf sSSet) remainingHS)
        subSpaceSets = Set.fromList <$> subSpaces
        subSpaces = concatMap fingerprint (concatMap snd sProfiles)

-- Do the NodeNames in the SubSpaces of a SwitchProfile occur in the
-- corresponding DMMSModelMapping? Note that we already know that each Phenotype
-- and DMMSModelMapping SwitchName had a corresponding DMNode from
-- allSwitchStatesCovered and coarseNodesMatch, respectively, so we just need to
-- pair them up and check. This includes the SubSpaces of the PhenotypeErrors.
subSpaceNodesInSwitch :: DMMSModelMapping -> [SwitchProfile]
                      -> Validation [ModelInvalid] [SwitchProfile]
subSpaceNodesInSwitch dmmsMMap sProfiles =
    traverse subSpaceNodesInSwitch' mappingPairs
    where
        mappingPairs = foldr (mappingsMatcher sProfiles) [] dmmsMMap

subSpaceNodesInSwitch' :: (NodeName, ([NodeName],[Phenotype]))
                       -> Validation [ModelInvalid] SwitchProfile
subSpaceNodesInSwitch' (nName, (dmmsMMFineNNames, phs))
    | excessSSNNs == [] = Success (nName, phs)
    | otherwise = Failure $ [SubSpaceNodesNotInSwitch (nName, excessSSNNs)]
    where
        excessSSNNs = subSpaceNodeNames L.\\ dmmsMMFineNNames
        subSpaceNodeNames = (L.sort . L.nubOrd . fmap fst) subSpacePairs
        subSpacePairs :: [(NodeName, NodeState)]
        subSpacePairs = mconcat $ concatMap fingerprint phs <>
            (concatMap phErrorFingerprint phErrs)
        phErrs = concatMap phenotypeErrors phs

-- Check that each node in each SubSpace has a corresponding DMNode, and that
-- its referenced state exists. This includes the SubSpaces of the
-- PhenotypeErrors.
subSpaceNodeStatesCovered :: ModelLayer -> [SwitchProfile]
                          -> Validation [ModelInvalid] [(NodeName, NodeState)]
subSpaceNodeStatesCovered fineMLayer sProfiles =
    traverse (subSpaceNodeStatesCovered' fineRanges) subSpacePairs
    where
        fineRanges = layerRanges fineMLayer
        subSpacePairs = mconcat $ concatMap fingerprint phs <>
            (concatMap phErrorFingerprint phErrs)
        phErrs = concatMap phenotypeErrors phs
        phs = concatMap snd sProfiles

subSpaceNodeStatesCovered' :: LayerRange -> (NodeName, NodeState)
       -> Validation [ModelInvalid] (NodeName, NodeState)
subSpaceNodeStatesCovered' lRange (nName, nState) =
    case Map.lookup nName lRange of
    Just n
        | nState >= 0 && nState <= n -> Success (nName, nState)
        | otherwise -> Failure $ [InvalidSubSpaceNodeValue (nName, nState)]
    Nothing -> Failure $ [UnknownSubSpaceNode nName]
    
-- Internally to a Switch, for each phenotype which is a loop (that is, where
-- (length . fingerprint) ph > 1), there can be AT MOST one point phenotype
-- ((length . fingerprint) ph == 1) whose SubSpace is mutually satifiable with
-- AT MOST one SubSpace of the loop. That SubSpace MAY NOT repeat when searching
-- for phenotype matches on a thread, and is set at the marked SubSpace
-- Additionally, no two loop Phenotypes in the switch may have any mutually
-- satisfiable SubSpaces. 
wellFormedPh :: (NodeName, [Phenotype])
             -> Validation [ModelInvalid] (NodeName, [Phenotype])
wellFormedPh (nName, phs) =
    -- Point Phenotypes non-mutually satisfiable check. 
    sequenceA (pointPhsAreMU <$> pointPhs <*> pointPhs) *>
    -- Loop Phenotypes non-mutually satisfiable check. 
    sequenceA (loopPhsAreMU <$> loopPhs <*> loopPhs) *>
    traverse (($ pointPhs) . findMarkedSubSpace) loopPhs *>
    pure (nName, phs)
    where
        (pointPhs, loopPhs) = L.partition ((== 1) . L.length . fingerprint) phs

-- pointPhenotypesAreMutuallyUnSatisfiable
pointPhsAreMU :: Phenotype -> Phenotype
              -> Validation [ModelInvalid] (Phenotype, Phenotype)
pointPhsAreMU phX phY
    -- PhenotypeNames are checked to be globally unique at parse time, so this
    -- bit is just to avoid checking Phenotypes against themselves. 
    | xPhName == yPhName = Success (phX, phY)
    | otherwise = (sequenceA matches) *> pure (phX, phY)
    where
        matches :: [Validation [ModelInvalid] (Phenotype, Phenotype)]
        matches = checker <$> xPhNameSSExps <*> yPhNameSSExps
        checker (xN, xSS) (yN, ySS)
            | areMutuallySatisfiable xSS ySS = Failure
                [MutuallySatisfiablePointPhSubSpaces ((xN, xSS), (yN, ySS))]
            | otherwise = Success (phX, phY)
        (xPhNameSSExps, yPhNameSSExps) = isoBimap phExpander (phX, phY)
        phExpander ph = zip (repeat (phenotypeName ph)) (fingerprint ph)
        (xPhName, yPhName) = isoBimap phenotypeName (phX, phY)


-- loopPhenotypesAreMutuallyUnSatisfiable
loopPhsAreMU :: Phenotype -> Phenotype
             -> Validation [ModelInvalid] (Phenotype, Phenotype)
loopPhsAreMU phX phY
    -- PhenotypeNames are checked to be globally unique at parse time, so this
    -- bit is just to avoid checking Phenotypes against themselves. 
    | xPhName == yPhName = Success (phX, phY)
    | otherwise = (sequenceA matches) *> pure (phX, phY)
    where
        matches = checker <$> xPhNameSSExps <*> yPhNameSSExps
        checker (xN, xSS) (yN, ySS)
            | areMutuallySatisfiable xSS ySS = Failure
                [MutuallySatisfiableLoopPhSubSpaces ((xN, xSS), (yN, ySS))]
            | otherwise = Success (phX, phY)
        (xPhNameSSExps, yPhNameSSExps) = isoBimap phExpander (phX, phY)
        phExpander ph = zip (repeat (phenotypeName ph)) (fingerprint ph)
        (xPhName, yPhName) = isoBimap phenotypeName (phX, phY)
        
-- Consume a loop Phenotype and a list of point Phenotypes, and produce a
-- Just x marked Phenotype if a match exists. NOTE! This assumes that the point
-- Phenotypes have been checked to be non-mutually satisfiable! Do not use
-- outside of wellFormedPh!
findMarkedSubSpace :: Phenotype -> [Phenotype]
                   -> Validation [ModelInvalid] Phenotype
findMarkedSubSpace loopPh pointPhs = case mtchs of
    [] -> Success loopPh {markedSubSpace = Nothing}
    [ph] -> case filter (areMutuallySatisfiable (fFp ph)) (fP loopPh) of
        [] -> Success loopPh {markedSubSpace = Nothing}
        [_] -> Success $ loopPh {markedSubSpace = Just (fFp ph)}
        sSs -> Failure $ [ExcessPointSubSpaceMatches err]
            where
                err = ((errF ph), (phenotypeName loopPh, sSs))
    phs -> Failure $ [ExcessLoopPointSubSpaceMatches lpPHN (errF <$> phs)]
    where
        errF x = (phenotypeName x, fFp x)
        lpPHN = phenotypeName loopPh
        mtchs = filter (findPointPhs loopPh) pointPhs
        findPointPhs lPh pPh = any (areMutuallySatisfiable pPhSS) loopPhSSs
            where
                pPhSS = fFp pPh
                loopPhSSs = fingerprint lPh
        fFp = (head . fingerprint)
        fP = fingerprint

-- Can two SubSpaces both be matched at the same time?
areMutuallySatisfiable :: SubSpace -> SubSpace -> Bool
areMutuallySatisfiable ssX ssY
    | L.null mutualNodes = True
    | otherwise = all id matchedSSs
    where
        matchedSSs = zipWith (\(_, i) (_, j) -> i == j) ssYMutuals ssXMutuals
        ssYMutuals = pullMutuals ssY
        ssXMutuals = pullMutuals ssX
        pullMutuals xs = L.sort $ filter ((`elem` mutualNodes) . fst) xs
        mutualNodes = L.intersect (fst <$> ssX) (fst <$> ssY)

-- Split a ModelMapping into a DMMSModelMapping and a [SwitchProfile]. 
modelMappingSplit :: ModelMapping -> (DMMSModelMapping, [SwitchProfile])
modelMappingSplit = foldr f ([], [])
    where
        f (nN, (nns, phs)) acc
            | phs == [] = BF.first ((nN, nns):) acc
            | otherwise = BF.bimap ((nN, nns):) ((nN, phs):) acc

-- It would be very easy to blindly create nonsensical gates, so we never create
-- [NodeStateAssign] directly, only through these smart constructors.

-- In the case where we are parsing a discrete logical statment.  
mkLogicalGate :: [(NodeName, NodeStateAssign)]
              -> Validation [GateInvalid] LogicalGate
mkLogicalGate xs =
    (,,) <$> (nodeNamesOK ns) <*> pure order <*> (stateAssignsOK ass)
        where (ns, ass) = unzip xs
              order     = fst <$> (refdNodesStatesNG $ snd <$> ass)

type LogicalGate = (NodeName, GateOrder, [NodeStateAssign])

-- Several helper functions for gate sanity/consistency checks:


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
            -> Validation [TableInvalid] TruthTableGate
mkTableGate (nodes, rows)
    | testErrors == [] = Success $ ( (last nodes)
                                   , (init nodes)
                                   , (Map.fromList $ zip inputVectors outputs)
                                   )
    | otherwise        = Failure testErrors
    where 
        cols = L.transpose rows
        inputVectors = U.fromList <$> inputRows
        inputRows = init <$> rows
        outputs = last cols
        testResults = [ allOutputsPresent outputs
                      , sufficientInputRows inputRows
                      , noDupeInputs inputRows
                      , rowsStrictlyIncreasing inputRows
                      ]
        testErrors = errorRollup testResults

type TruthTableGate = (NodeName, GateOrder, TruthTable)

tableToLogical :: GateOrder -> [[NodeState]] -> [NodeState] -> [NodeStateAssign]
tableToLogical nodes inputRows outputs = collapsedOrs
    where
--      Associate each input in each row with its corresponding node: 
        inputAssociations = zip (init nodes) <$> inputRows
--      Turn all the pairs of NodeName and NodeState into GateConsts: 
        inputExprList = fmap (fmap (uncurry GateConst)) inputAssociations
--      AND together all the entries in each row: 
        inputExprs = fmap (foldr1 (Binary And)) inputExprList
--      Pair each ANDed row assignment with its associated output:
        inputAssigns = zip inputExprs outputs
--      Pull the range of possible output states
        possibleOutputStates :: [NodeState]
        possibleOutputStates = (L.sort . nubInt) outputs
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

-- Check that there are no duplicate state assignments. 
noDupes :: [NodeStateAssign] -> Validation GateInvalid [NodeStateAssign]
noDupes ns = let st = states ns in 
             case (length st) == (length . nubInt) st of
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
                            clean = (L.sort . nubInt) . (filter (> 0))
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
--      from any of them. Each of these Lists of Bool should contain exactly
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
        presentOutputs = (L.sort . L.nubOrd) outputs
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
noDupeInputs inputs
    | allUnique inputs = Success inputs
    | otherwise = Failure DuplicatedInputRows



-- | Helper Functions:

-- This produces a list of ExprInputs which represent all the combinations of
-- input states in which a node state assignment might be true. It is not all
-- the possible inputs, just all the possible input sets you can make from the
-- list of gate state assignment expressions. This is still slight overkill, but
-- it is easy to extract and prepare. 
gateCombinations :: [NodeExpr] -> [ExprInput]
gateCombinations asns = fmap Map.fromList combosList
    where
        (nNames, nStates) = unzip $ refdNodesStatesNG asns
        combosList = fmap (zip nNames) (sequenceA nStates)

-- Produces the nodes, and states of those nodes, that are referenced,
-- (explicitly or otherwise), in a given list of NodeExprs. Makes sure that, 
-- even if a node is only referenced as :1, the :0 state is also included. 
refdNodesStatesNG :: [NodeExpr] -> [(NodeName, [NodeState])]
refdNodesStatesNG exprs = (\(nName, s) -> (nName, [0..(max 1 s)])) <$>
                                                        (Map.toList nMap)
    where
        nMap = foldr (Map.unionWith max) Map.empty (exprNodes <$> exprs)

-- Produces the nodes, and states of those nodes, that are referenced in a given
-- TruthTable. 
refdNodesStatesTM :: GateOrder -> TruthTable -> [(NodeName, [NodeState])]
refdNodesStatesTM nodes tT = zip nodes columnRanges
    where
        columnRanges = ((\n -> [0..n]) . maximum) <$> (L.transpose keyList)
        keyList = U.toList <$> ((L.sort . Map.keys) tT)

-- Extract a HashMap of the nodes of a NodeExpr as keys and their highest
-- referenced state as values. 
exprNodes :: NodeExpr -> Map.HashMap NodeName NodeState
exprNodes (GateLit _) = Map.empty
exprNodes (GateConst nName nState) = Map.singleton nName nState
exprNodes (Not expr) = exprNodes expr
exprNodes (Pars expr) = exprNodes expr
exprNodes (Binary And expr1 expr2) =
    Map.unionWith max (exprNodes expr1) (exprNodes expr2)
exprNodes (Binary Or expr1 expr2) =
    Map.unionWith max (exprNodes expr1) (exprNodes expr2)


-- Generate all the possible inputs sets of a model, separated by layers. Each
-- ModelLayer will have a [[ExprInput]] of possible inputs, one [ExprInput] for
-- each node. The DMModel as a whole will have a [[[ExprInput]]] SUPPRESSED
-- modelCombinations :: DMModel -> [[[ExprInput]]]
-- modelCombinations = (layerCombinations <$>) . modelLayers

-- Generate all the possible inputs sets of a ModelLayer, one [ExprInput] for
-- each node. SUPPRESSED
-- layerCombinations :: ModelLayer -> [[ExprInput]]
-- layerCombinations mL = fromJust <$> comboLists
--     where
--         comboLists = (nodeCombinations (ranges)) <$> nodes
--         ranges :: LayerRange
--         ranges = layerRanges mL
--         nodes = layerNodes mL

-- If we have a LayerRange, this gives the possible inputs to a given node in
-- that ModelLayer. 
nodeCombinations :: LayerRange -> DMNode -> Maybe [ExprInput]
nodeCombinations r n = Map.fromList <<$>> combosList
    where
        combosList :: Maybe [[(NodeName, NodeState)]]
        combosList = (zip nNames) <<$>> (sequenceA <$> nStates)
        nStates :: Maybe [[NodeState]]
        nStates = sequenceA $ sequenceA (Map.lookup <$> nNames)
            (Map.map fillDown r)
        nNames :: [NodeName]
        nNames = fst <$> (refdNodesStatesNG nExprs)
        nExprs = ((snd <$>) . gateAssigns . nodeGate) n

-- Extract the names and ranges from a node. Zero-valued. 
nodeRange :: DMNode -> NodeRange
nodeRange n = (name, range)
    where
        name = (nodeName . nodeMeta) n
        range = maximum $ ((fst <$>) . gateAssigns . nodeGate) n


-- Extract all the DMNodes from a DMModel
modelNodes :: DMModel -> [[DMNode]]
modelNodes (Fine ml) = [layerNodes ml]
modelNodes (LayerBinding _ mL dmM) = (layerNodes mL) : (modelNodes dmM)

-- Extract all the Gr.LNode DMNodes from a DMModel
modelNodes' :: DMModel -> [[Gr.LNode DMNode]]
modelNodes' (Fine ml) = [layerNodes' ml]
modelNodes' (LayerBinding _ mL dmM) = (layerNodes' mL) : (modelNodes' dmM)

-- Extract all the Gr.LEdge DMLinks from a DMModel
modelEdges' :: DMModel -> [[Gr.LEdge DMLink]]
modelEdges' (Fine ml) = [layerEdges' ml]
modelEdges' (LayerBinding _ mL dmM) = (layerEdges' mL) : (modelEdges' dmM)

-- Extract all the ModelLayers from a DMModel
modelLayers :: DMModel ->  [ModelLayer]
modelLayers (Fine mL) = [mL]
modelLayers (LayerBinding _ mL dmM) = mL : (modelLayers dmM)

-- Extract all the ModelMappings from a DMModel
modelMappings :: DMModel ->  [ModelMapping]
modelMappings (Fine _) = []
modelMappings (LayerBinding mM _ dmM) = mM : (modelMappings dmM)

-- Extract the DMNodes from a ModelLayer
layerNodes :: ModelLayer -> [DMNode]
layerNodes = (snd <$>) . Gr.labNodes . modelGraph

-- Extract the node ranges from a ModeLayer
layerRanges :: ModelLayer -> LayerRange
layerRanges ml = Map.fromList $ nodeRange <$> (layerNodes ml)

-- Extract the Gr.LNode DMNodes from a ModelLayer
layerNodes' :: ModelLayer -> [Gr.LNode DMNode]
layerNodes' = Gr.labNodes . modelGraph

-- Extract the Gr.LEdge DMLinks from a ModelLayer
layerEdges' :: ModelLayer -> [Gr.LEdge DMLink]
layerEdges' = Gr.labEdges . modelGraph

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
    lRefs (LitInfo _ xxs _ yys) = (concat xxs) <> (concat yys)
modelCiteKeys (LayerBinding _ mLayer dmModel) =
    (modelCiteKeys (Fine mLayer)) `Set.union` (modelCiteKeys dmModel)

-- Extract all the names of the layers of a DMModel
layerNames :: DMModel -> [T.Text]
layerNames (Fine ml) = [(modelName . modelMeta) ml]
layerNames (LayerBinding _ mLayer dmModel) =
    ((modelName . modelMeta) mLayer) : (layerNames dmModel)

-- Find a ModelLayer and the ModelMapping that binds it to the next ModelLayer
-- up, if either of them exist
findLayerWithBinding :: T.Text
                     -> DMModel
                     -> Maybe (Maybe ModelMapping, ModelLayer)
findLayerWithBinding lName dmM = (,) foundMap <$> foundLayer
    where
        foundMap = ((-1 +) <$> layerIndex) >>= ((modelMappings dmM) L.!?)
        foundLayer = layerIndex >>= ((modelLayers dmM) L.!?)
        layerIndex = L.elemIndex lName (layerNames dmM)

-- Peel off the coarsest (topmost) ModelLayer of a DMModel
coarseLayer :: DMModel -> ModelLayer
coarseLayer (Fine ml) = ml
coarseLayer (LayerBinding _ mLayer _) = mLayer

-- Extract the Fine (bottom-most) ModelLayer of a DMModel. 
fineLayer :: DMModel -> ModelLayer
fineLayer (Fine ml) = ml
fineLayer (LayerBinding _ _ dmM) = fineLayer dmM

-- Turn a ModelGraph into a [DMMSNode], for when we want to render a ModelGraph
-- to T.Text
dmmsNodes :: ModelGraph -> [DMMSNode]
dmmsNodes mg = zip strippedNS namedES
    where
        strippedNS = snd <$> (L.sort ns)
        namedES = lNamer nMap <<$>> chunkedES
        lNamer m (i, _, dmL) = ((nodeName . nodeMeta)(m Map.! i), dmL)
--      We want to group nodes with their InLinks, so we sort the edges by the
--      second Node (their destination). 
        chunkedES = L.groupBy (\(_, j, _) (_, m, _) -> j == m) sortedES
        sortedES = L.sortOn (\(i, j, k) -> (j, i, k)) es
        nMap = Map.fromList ns
        ns = Gr.labNodes mg
        es = Gr.labEdges mg

-- Give me only those Switches from the ModelMapping whose Phenotypes are not
-- empty. 
nonEmptyPhenotypes :: ModelMapping -> [Switch]
nonEmptyPhenotypes = filter (not . null . snd . snd)
