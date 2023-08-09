{-# LANGUAGE OverloadedStrings #-}

module Types.DMInvestigation
    ( mkDMInvestigation
    , vexErrorPrep
    , layerMatch
    , inputOptions
    , textInputOptions
    , VEXInvestigation
    , VEXLayerExpSpec(..)
    , ISFSpec(..)
    , Sampling(..)
    , SamplingParameters(..)
    , BarcodeFilter(..)
    , VEXExperiment(..)
    , InitialEnvironment(..)
    , ExperimentStep(..)
    , VEXInputPulse(..)
    , NodeAlteration(..)
    , RealInputCoord
    , RealNodeState
    , NudgeDirection(..)
    , Duration
    , VEXInvestigationInvalid(..)
    , LayerResult(..)
    , ExperimentResult
    , ExpContext(..)
    , ExpKind(..)
    , AttractorResult
    , Timeline
    , WasForced
    , PulseSpacing
    , runInvestigation
    , pickStates
    ) where

import Utilities
import Types.DMModel
import Types.Simulation
import Figures.InputSpaceFigure
import Data.Validation
-- import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Vector.Instances()
import qualified Data.Vector as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.Bimap as BM
import qualified Data.Text as T
import qualified Data.List.Unique as Uniq
import System.Random
-- import Path
import qualified Data.List as L
import qualified Data.Bifunctor as BF
import Data.Maybe (mapMaybe, fromJust)
import Data.Bitraversable (bitraverse)
import GHC.Stack (HasCallStack)

-- Defining the types that comprise sampling preferences, input space diagram
-- details, and various virtual experiments to be run on the associated DMModel,
-- as well as functions to validate parsed investigation files. 

type DMInvestigation = [LayerExpSpec]

data LayerExpSpec = LayerExpSpec {
      layerExpMMapping :: ModelMapping
    , layerExpMLayer :: ModelLayer
    , invesIBundle :: Maybe InputBundle -- Should dynmod create a 5D figure?
    , experiments :: [DMExperiment]
    }

-- A general experiment is a time series wherein you start in a particular
-- state, usually an Attractor, let the system run for some duration, then set
-- some environment input to some different (possibly continuous) value and let
-- the system run some more. Keep doing this for as long as specified. Many
-- times this will only mean: Start in a place, flip an input and let it run, 
-- then flip back to where you started and run for a bit more before ending. 
-- Separately from this one can also knock in or out individual non-input
-- nodes, as in a wet-lab organism experiment.
data DMExperiment = DMExperiment {
      experimentName :: T.Text
--    Select the relevant Attractors from those of the whole layer. 
    , attFilter :: [(Barcode, Attractor)] -> [(Barcode, Attractor)]
    , expStepper :: ExpStepper
    , inputPulses :: [InputPulse]
    , expKind :: ExpKind -- Was the parsed VEXExperiment general or preset?
    }

data ExpKind = P1
             | KDOE
             | GenExp
             deriving (Eq, Show)

data InitialEnvironment = InEnv
    { initCoord :: [(NodeName, NodeState)] -- Every input, pinned
-- BarcodeFilter to set the starting Attractor(s) for the experiment. 
    , initFilters :: (Maybe BarcodeFilter)
-- Do experiment runs for attractors whose Barcodes are hidden in the input
-- figure. This is a Bool in the VEX file, but after parsing we check if it is
-- false, and make it a Right bcFilter if it is
    , showHidden :: (Either Bool BarcodeFilter)
    } deriving (Eq, Show)

data ExperimentStep = SynchronousExpStepper
                    | NoisyExpStepper Probability
                    | AsynchronousExpStepper
                    deriving (Eq, Show, Ord)

-- Note that (Left duration) is a default duration, and may be altered by the
-- length of the Attractor that the pulse starts in. 
-- (Right duration) is specified by the user, and may not be so altered. 
data InputPulse = InputPulse { realInputCoord :: RealInputCoord
                             , intNodeAlterations :: [IntNodeAlteration]
                             , inputDuration :: Either Duration Duration
                             } deriving (Eq, Show)

-- Set an environmental input (or inputs) to a particular value. Real
-- values will be stochastically set on each time-step. 
type RealInputCoord = U.Vector (NodeIndex, RealNodeState)

type RealNodeState = Double

type Duration = Int


-- The inner run of stepping through the network should be fast-ish, so looking
-- up the NodeIndex each time for node alterations is a bad idea. 
data IntNodeAlteration = IntNodeLock NodeIndex NodeState LockProbability
                       | IntGradientNudge NodeIndex
                                          RangeTop
                                          BoolNudgeDirection
                                          NudgeProbability
                       deriving (Eq, Show, Ord)

type LockProbability = Probability
-- NudgeDirection is its own sum type, but we need nudging to be fast-ish
-- when we alter NodeStates in the middle of a DMExperiment, so:
-- False = NudgeDown
-- True  = NudgeUp
type BoolNudgeDirection = Bool
type NudgeProbability = Probability

data LayerResult = LayerResult
    { layerResultMM :: ModelMapping
    , layerResultML :: ModelLayer
    , layerResultERs :: [ExperimentResult]
    , layerResultIB :: Maybe InputBundle -- Should dynmod create a 5D figure?
    }
type ExperimentResult = (ExpContext, [AttractorResult])

data ExpContext = ExpCon T.Text ExpKind deriving (Eq, Show)

type AttractorResult = (Barcode, ([Timeline], [PulseSpacing]))

type PulseSpacing = Int -- The spacing between pulses
type Timeline = B.Vector AnnotatedLayerVec
type AnnotatedLayerVec = U.Vector (NodeState, WasForced)

type WasForced = Bool -- Was the NodeState in question forced to this state?

data ExpStepper = SD PSStepper
                | SN PNStepper'
                | AD PAStepper'

-- Errors that might occur when combining parsed VEXLayerExpSpecs with a parsed
-- DMModel in order to make DMExperiments. 
data VEXInvestigationInvalid =
      DuplicatedLayerNames [NodeName]
    | VEXLayerNameNotInDMModel T.Text
    | MatchedModelIsCoarsest T.Text
    | AxesOrderHasRepeats [NodeName]
    | UnknownNodesInAxesOrder [NodeName]
    | NonInputNodesInAxesOrder [NodeName]
    | MultipleAxesNodesFromSingleInput [[NodeName]]
    | ISDPinnedInputsHaveRepeats [NodeName]
    | UnknownNodesInISDPinnedInputs [NodeName]
    | NonInputNodesInISDPinnedInput [NodeName]
    | InValidISDPinnedInputs [(NodeName, NodeState)] T.Text
    | ExcessUnpinnedISDInputs T.Text
    | BCFSwitchRepeats [NodeName]
    | UnknownSwitchesInBCFilter [NodeName]
    | UnknownPhenotypesInSwitches [(NodeName, PhenotypeName)]
    | Pulse1FlipDoesNotChangeStartingModelState (NodeName, RealNodeState) 
    | SMSPinnedInputsHaveRepeats [NodeName]
    | UnknownNodesInSMSPinnedInputs [NodeName]
    | NonInputNodesInSMSPinnedInput [NodeName]
    | InValidSMSPinnedInputs [(NodeName, NodeState)] T.Text
    | UnpinnedSMSInputs T.Text
    | NodeAltPinnedInputsHaveRepeats [NodeName]
    | UnknownNodesInNodeAltPinnedInputs [NodeName]
    | NonInputNodesInNodeAltPinnedInput [NodeName]
    | InValidRealPinnedInputs [(NodeName, RealNodeState)] T.Text
    | NodeAltsRepeat [NodeName]
    | UnknownNodesInNodeAlts [NodeName]
    | InputsInNodeAlts [NodeName]
    | InvalidNodeAltLocks [(NodeName, NodeState)] T.Text
    | AbsentSwitchProfiles
    deriving (Eq, Show, Ord)

vexErrorPrep :: VEXInvestigationInvalid -> T.Text
vexErrorPrep (DuplicatedLayerNames nNames) = "DuplicatedLayerNames: " <>
    T.intercalate ", " nNames
vexErrorPrep (VEXLayerNameNotInDMModel vLN) =
    "VEXLayerNameNotInDMModel: " <> vLN
vexErrorPrep (MatchedModelIsCoarsest mN) = "MatchedModelIsCoarsest: " <> mN
vexErrorPrep (AxesOrderHasRepeats nNms) = "AxesOrderHasRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInAxesOrder nNms) = "UnknownNodesInAxesOrder: " <>
    T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInAxesOrder nNms) = "NonInputNodesInAxesOrder: " <>
    T.intercalate ", " nNms
vexErrorPrep (MultipleAxesNodesFromSingleInput nNmss) =
    "MultipleAxesNodesFromSingleInput: " <>
        (T.intercalate "\n" . fmap (T.intercalate ", ")) nNmss
vexErrorPrep (ISDPinnedInputsHaveRepeats nNms) =
    "ISDPinnedInputsHaveRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInISDPinnedInputs nNms) =
    "UnknownNodesInISDPinnedInputs: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInISDPinnedInput nNms) =
    "NonInputNodesInISDPinnedInput: " <> T.intercalate ", " nNms
vexErrorPrep (InValidISDPinnedInputs badPairs properPairText) =
    "InValidISDPinnedInputs: " <>
        T.intercalate ", " ((T.pack . show) <$> badPairs) <> properPairText
vexErrorPrep (ExcessUnpinnedISDInputs pChs) =
    "ExcessUnpinnedISDInputs: " <> pChs
vexErrorPrep (BCFSwitchRepeats nNms) = "BCFSwitchRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownSwitchesInBCFilter nNms) =
    "UnknownSwitchesInBCFilter: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownPhenotypesInSwitches badPairs) =
    "UnknownPhenotypesInSwitches: " <>
        T.intercalate ", " ((T.pack . show) <$> badPairs)
vexErrorPrep (Pulse1FlipDoesNotChangeStartingModelState badPair) =
    "Pulse1FlipDoesNotChangeStartingModelState: " <> (T.pack . show) badPair
vexErrorPrep (SMSPinnedInputsHaveRepeats nNms) =
    "SMSPinnedInputsHaveRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInSMSPinnedInputs nNms) =
    "UnknownNodesInSMSPinnedInputs: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInSMSPinnedInput nNms) =
    "NonInputNodesInSMSPinnedInput: " <> T.intercalate ", " nNms
vexErrorPrep (InValidSMSPinnedInputs badPairs properPairText) =
    "InValidSMSPinnedInputs: " <>
        T.intercalate ", " ((T.pack . show) <$> badPairs) <> properPairText
vexErrorPrep (UnpinnedSMSInputs pChs) = "UnpinnedSMSInputs: " <> pChs
vexErrorPrep (NodeAltPinnedInputsHaveRepeats nNms) =
    "NodeAltPinnedInputsHaveRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInNodeAltPinnedInputs nNms) =
    "UnknownNodesInNodeAltPinnedInputs: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInNodeAltPinnedInput nNms) =
    "NonInputNodesInNodeAltPinnedInput: " <> T.intercalate ", " nNms
vexErrorPrep (InValidRealPinnedInputs badPairs properPairText) =
    "InValidRealPinnedInputs: " <>
        T.intercalate ", " ((T.pack . show) <$> badPairs) <> properPairText
vexErrorPrep (NodeAltsRepeat nNms) = "NodeAltsRepeat" <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInNodeAlts nNms) = "UnknownNodesInNodeAlts: " <>
    T.intercalate ", " nNms
vexErrorPrep (InputsInNodeAlts nNms) = "InputsInNodeAlts: " <>
    T.intercalate ", " nNms
vexErrorPrep (InvalidNodeAltLocks badPairs properPairText) =
    "InvalidNodeAltLocks: " <>
        T.intercalate ", " ((T.pack . show) <$> badPairs) <> properPairText
vexErrorPrep AbsentSwitchProfiles =
    "AbsentSwitchProfiles: " <>
        "There are no SwitchProfiles in the DMMS file, so we cannot make an \
            \environmental input figure. "

-- Parsed types from VEX files, before validation with a parsed DMMS file.

type VEXInvestigation = (FilePath, [VEXLayerExpSpec])

data VEXLayerExpSpec = VEXLayerExpSpec {
      vexLayerName :: ModelName
    , sampling :: Sampling
    , vexISpaceSpec :: Maybe ISFSpec -- Should dynmod create a 5D figure?
    , vexExperiments :: [VEXExperiment]
    } deriving (Eq, Show)

-- How should dynmod handle finding or loading the attractors of the DMModel?
data Sampling = SampleOnly SamplingParameters
              | ReadOnly FilePath
              | ReadAndSample SamplingParameters FilePath
              deriving (Eq, Show)

data SamplingParameters = SamplingParameters {
--  The number of random LayerVecs to generate for each combination of inputs.
      nStates :: Int
--  Number of noisy steps to take when gathering attractors/statistics for each
--  combination of inputs.
    , noisySteps :: Int
--  Probability that any given state slips on a noisy step.
    , noisyProb :: Probability
    } deriving (Eq, Show)

-- In a 5D figure, the order of axes is: x, y, z, w, v. The first five elements
-- of the [NodeName] will be so assigned. Any excess will be ignored and any
-- unspecified axes will be assigned by parsing order of the relevant DMNodes
data ISFSpec = ISFSpec { axesOrdering :: [NodeName]
                       , bcFilter :: Maybe BarcodeFilter
                       , pinnedInputs :: [(NodeName, Int)]
                       } deriving (Eq, Show)

data VEXExperiment =
-- A parsed GeneralExperiment{}.
    GeneralExp T.Text -- Experiment name
               InitialEnvironment
               ExperimentStep
               [VEXInputPulse]
-- A parsed Pulse1{}.
    | Pulse1 (Maybe Duration, Maybe Duration) -- t_0 and t_end
              InitialEnvironment
              Duration -- pulse duration
             (NodeName, RealNodeState)
-- A parsed KDOE{}.
    | KnockDOverE (Maybe Duration, Maybe Duration) -- t_0 and t_end
                   InitialEnvironment
                   Duration -- pulse duration
                  [NodeAlteration]
    deriving (Eq, Show)

-- Note that (Left duration) is a default duration, and may be altered by the
-- length of the Attractor that the pulse starts in. 
-- (Right duration) is specified by the user, and may not be so altered. 
data VEXInputPulse = VEXInPt { vexRealInputCoord :: [(NodeName, RealNodeState)]
                             , vexNodeAlterations :: [NodeAlteration]
                             , vexInputDuration :: Either Duration Duration
                             } deriving (Eq, Show)


data NodeAlteration = NodeLock NodeName NodeState LockProbability
                    | GradientNudge NodeName
                                    NudgeDirection
                                    NudgeProbability
                    deriving (Eq, Show, Ord)

nodeAltName :: NodeAlteration -> NodeName
nodeAltName (NodeLock nName _ _) = nName
nodeAltName (GradientNudge nName _ _) = nName

data NudgeDirection = NudgeUp
                    | NudgeDown
                    deriving (Eq, Show, Ord, Enum)

----------------------------------------------------------------------------
-- Validating vex files:

-- Produce a DMInvestigation, or a [VEXInvestigationInvalid] to tell us what
-- went wrong. The only thing that we validate at parse is that all layer names
-- in the VEX file are unique. 
mkDMInvestigation :: HasCallStack => DMModel -> [VEXLayerExpSpec]
                  -> Validation [VEXInvestigationInvalid] DMInvestigation
mkDMInvestigation dmM vlExSpecs = case traverse (layerMatch dmM) vlExSpecs of
    Failure err -> Failure err
    Success pairedLayers -> traverse mkLayerExpSpec pairedLayers

-- Match up VEXLayerExpSpecs with DMModel ModelLayers, if such layers exist and
-- are valid. 
layerMatch :: DMModel -> VEXLayerExpSpec
           -> Validation [VEXInvestigationInvalid]
                         ((ModelMapping, ModelLayer), VEXLayerExpSpec)
layerMatch dmM vLExSpec = case findLayerWithBinding vLName dmM of
    Just (Just mM, mL) -> Success ((mM, mL), vLExSpec)
    Just (Nothing, mL) -> Failure $ [MatchedModelIsCoarsest mName]
        where
            mName = (modelName . modelMeta) mL
    Nothing -> Failure $ [VEXLayerNameNotInDMModel vLName]
    where
        vLName = vexLayerName vLExSpec


mkLayerExpSpec :: HasCallStack => ((ModelMapping, ModelLayer), VEXLayerExpSpec)
               -> Validation [VEXInvestigationInvalid] LayerExpSpec
mkLayerExpSpec ((mM, mL), vLExSpec) = 
    LayerExpSpec <$> pure mM
                 <*> pure mL
                 <*> traverse (mkDMInvesIBundle mM mL) (vexISpaceSpec vLExSpec)
                 <*> traverse (mkDMExperiment mM mL) (vexExperiments vLExSpec)

mkDMInvesIBundle :: ModelMapping -> ModelLayer -> ISFSpec
                 -> Validation [VEXInvestigationInvalid] InputBundle
mkDMInvesIBundle mM mL isfSpec = case filter ((/= []) . snd . snd) mM of
    [] -> Failure [AbsentSwitchProfiles]
    _ -> InputBundle <$> mkInputNodes mL axesOrd pinnedNs
                     <*> mkInIBFixedVec mL pinnedNs
                     <*> traverse (mkIBBCFilter mM) (bcFilter isfSpec)
            where
                pinnedNs = pinnedInputs isfSpec
                axesOrd = axesOrdering isfSpec

-- Pull out the input nodes of a given ModelLayer, remove pinned input nodes,
-- and order them by the order given in the VEX file, if such an ordering exists
-- and is valid. 
mkInputNodes :: ModelLayer -> [NodeName] -> [(NodeName, Int)]
             -> Validation [VEXInvestigationInvalid] [[DMNode]]
mkInputNodes mL axesOrd inputChoices
    | (not . null) axesRepeats = Failure [AxesOrderHasRepeats axesRepeats]
    | (not . null) nonPresentAxes =
        Failure [UnknownNodesInAxesOrder nonPresentAxes]
    | (not . null) nonInputAxes =
        Failure [NonInputNodesInAxesOrder nonInputAxes]
    | (not . null) axesMultiplyInStacks =
        Failure [MultipleAxesNodesFromSingleInput axesMultiplyInStacks]
    | otherwise = Success orderedInputs
    where
        orderedInputs = extractedInputs <> (nonPinnedIs L.\\ extractedInputs)
        extractedInputs = inputExtract nonPinnedIs <$> axesOrd
        axesMultiplyInStacks = filter moreThanOne $ L.intersect axesOrd <$>
            inputNodeNames
        moreThanOne xs = L.length xs > 1
        nonPresentAxes = axesOrd L.\\ mlInputNames
        axesRepeats = Uniq.repeated axesOrd
        nonInputAxes = axesOrd L.\\ inputNodeNamesFlat
        inputNodeNamesFlat = mconcat inputNodeNames
        inputNodeNames = (nodeName . nodeMeta) <<$>> nonPinnedIs
        nonPinnedIs = L.filter (pinnedIF inputChoices) initialInputs
        pinnedIF iCs nodeStack = not $ any (`elem` (fst <$> iCs)) stackNames
            where stackNames = (nodeName . nodeMeta) <$> nodeStack
        initialInputs = (inputs . modelGraph) mL
        mlInputNames = (fmap (nodeName . nodeMeta) . layerNodes) mL

inputExtract :: [[DMNode]] -> NodeName -> [DMNode]
inputExtract iPts axesN = mconcat $ filter (f axesN) iPts
    where
        f nName inPt = nName `elem` ((nodeName . nodeMeta) <$> inPt)

-- Consumes pinned inputs from a parsed ISFSpec, and produces a FixedVec, if the
-- pinned inputs are valid. 
mkInIBFixedVec :: ModelLayer -> [(NodeName, Int)]
               -> Validation [VEXInvestigationInvalid] FixedVec
mkInIBFixedVec mL pinnedIs
    | (not . null) pinnedRepeats = Failure
        [ISDPinnedInputsHaveRepeats pinnedRepeats]
    | (not . null) nonPresentPinned =
        Failure [UnknownNodesInISDPinnedInputs nonPresentPinned]
    | (not . null) nonInputNs = Failure
        [NonInputNodesInISDPinnedInput nonInputNs]
    | (not . null) oobInputs = Failure
        [InValidISDPinnedInputs oobInputs properInputs]
    | L.length unpinnedInputs > 5 = Failure
        [ExcessUnpinnedISDInputs pinningChoices]
    | otherwise = Success $ mkPinnedVec inPts lniBMap pinnedIs
    where
        pinningChoices = "\nThere are more than 5 unpinned environmental \
            \inputs. Please choose at least " <> leftoverN <> " to pin:\n"
            <> tUPOpts
        leftoverN = (T.pack . show) $ L.length unpinnedInputs - 5
        tUPOpts = textInputOptions unpinnedInputs
        unpinnedInputs = inPts L.\\ extractedInputs
        extractedInputs = (inputExtract inPts . fst) <$> pinnedIs
        properInputs =
            "\nPinned Node(s) must be from listed inputs\n" <> txtInOpts
        oobInputs = filter (flip notElem (mconcat inOpts)) pinnedIs
        txtInOpts = textInputOptions inPts
        inOpts = inputOptions inPts
        nonInputNs = (fst <$> pinnedIs) L.\\ inputNodeNames
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        inPts = (inputs . modelGraph) mL
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        nonPresentPinned = (fst <$> pinnedIs) L.\\ mlNodeNames
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL
        pinnedRepeats = (Uniq.repeated . fmap fst) pinnedIs


-- Calculate the possible integer pinnings for environmental inputs. 
inputOptions :: [[DMNode]] -> [[(NodeName, Int)]]
inputOptions inPtNDs = inputOpt <$> inPtNDs
    where
        inputOpt :: [DMNode] -> [(NodeName, Int)]
        inputOpt [] = []
        inputOpt [n] = L.unfoldr nOpts 0
            where
                nOpts i
                    | i <= nRange = Just (iEntry, i + 1)
                    | otherwise = Nothing
                        where
                            iEntry = (nName, i)
                (nName, nRange) = nodeRange n
        inputOpt ns = (head rNS, 0):(zip rNS $ L.repeat 1)
            where
                rNS = L.reverse $ (nodeName . nodeMeta) <$> ns

-- Pretty print the possible integer pinnings for environmental inputs. 
textInputOptions :: [[DMNode]] -> T.Text
textInputOptions inPtNDs = T.intercalate "\n" $ txtInputOpt <$> inPtNDs
    where
        txtInputOpt :: [DMNode] -> T.Text
        txtInputOpt [] = T.empty
        txtInputOpt [n] = T.intercalate "\n" $ nName:(L.unfoldr nOpts 0)
            where
                nOpts i
                    | i <= nRange = Just (iLine, i + 1)
                    | otherwise = Nothing
                        where
                            iLine = "    " <> nName <> ":" <>
                                ((T.pack . show) i)
                (nName, nRange) = nodeRange n
        txtInputOpt ns = T.intercalate "\n" $ nName:("    " <> nName <> ":0"):
                 (otherOpts <$> rNS)
            where
                nName = head rNS
                rNS = L.reverse $ (nodeName . nodeMeta) <$> ns
                otherOpts nN = "    " <> nN <> ":1"


mkIBBCFilter :: ModelMapping -> BarcodeFilter
              -> Validation [VEXInvestigationInvalid] BarcodeFilter
mkIBBCFilter mM (OnlyBCF phs) = OnlyBCF <$> phValidate mM phs
mkIBBCFilter mM (ExcludeBCF phs) = ExcludeBCF <$> phValidate mM phs

phValidate :: ModelMapping -> [(NodeName, PhenotypeName)]
           -> Validation [VEXInvestigationInvalid] [(NodeName, PhenotypeName)]
phValidate mM phs
    | (not . null) bcfsRepeats = Failure $ [BCFSwitchRepeats bcfsRepeats]
    | (not . null) npSwitches = Failure $ [UnknownSwitchesInBCFilter npSwitches]
    | (not . null) npPhs = Failure $ [UnknownPhenotypesInSwitches npPhs]
    | otherwise = Success phs
    
    where
        npPhs :: [(NodeName, PhenotypeName)]
        npPhs = filter (not . phNameExists mMphMap) phs
        phNameExists phMap (sName, phName) = elem phName 
            (phenotypeName <$> (phMap M.! sName))
        npSwitches = filter (not . flip M.member mMphMap) (fst <$> phs)
        bcfsRepeats = (Uniq.repeated . fmap fst) phs
        mMphMap = M.fromList mMPhenotypes
        mMPhenotypes = filter (not . null) $ snd <<$>> mM


mkDMExperiment :: HasCallStack => ModelMapping -> ModelLayer -> VEXExperiment
               -> Validation [VEXInvestigationInvalid] DMExperiment
mkDMExperiment mM mL (GeneralExp exName inEnv expStep vexPulses) =
    DMExperiment <$> pure exName
                 <*> mkAttFilter mM mL inEnv
                 <*> pure (mkStepper expStep (layerPrep mL))
                 <*> traverse (mkInputPulse mL) vexPulses        
                 <*> pure GenExp
mkDMExperiment mM mL (Pulse1 (t_0, t_end) inEnv dur (pName, pState))
    | (isInt 7 pState) && elem (pName, round pState) (initCoord inEnv) = Failure
        [Pulse1FlipDoesNotChangeStartingModelState (pName, pState)]
    | otherwise = 
        DMExperiment <$> pure expName
                     <*> mkAttFilter mM mL inEnv
                     <*> pure (mkStepper SynchronousExpStepper (layerPrep mL))
                     <*> traverse (mkInputPulse mL) p1Alts
                     <*> pure P1
        where
            p1Alts = [startP, p1Pulse, endP]
            startP = maybe (VEXInPt initialCs [] (Left 50)) f t_0
                where f i = VEXInPt initialCs [] (Right i)
            p1Pulse = VEXInPt flipedInitialCs [] (Right dur)
            endP = maybe (VEXInPt initialCs [] (Left 50)) f t_end
                where f i = VEXInPt initialCs [] (Right i)
            flipedInitialCs :: [(NodeName, RealNodeState)]
            flipedInitialCs =
                case L.find (\x -> ((pName ==) . fst) x) initialCs of
                Nothing -> realFlip:initialCs
                Just a -> realFlip:(L.delete a initialCs)
            realFlip = (pName, pState)
            initialCs :: [(NodeName, RealNodeState)]
            initialCs = ((fromIntegral <<$>>) . initCoord) inEnv
            expName = "pulse1_" <> pName <> "-" <> (T.pack . show) pState <> "-"
                <> ((T.pack . show) dur) <> "_wInputs_" <> textInputs
            textInputs = T.intercalate "_" $ tShow <$> (initCoord inEnv)
            tShow (aNN, aNS) = aNN <> "-" <> (T.pack . show) aNS
mkDMExperiment mM mL (KnockDOverE (t_0, t_end) inEnv dur nAlts) =
    DMExperiment <$> pure expName
                 <*> mkAttFilter mM mL inEnv
                 <*> pure (mkStepper SynchronousExpStepper (layerPrep mL))
                 <*> traverse (mkInputPulse mL) [startP, kdoePulse, endP]
                 <*> pure KDOE
    where
        expName = "KD_OE_" <> kdoeName nAlts <> "_wInputs_" <> textInputs
        textInputs = T.intercalate "_" $ tShow <$> (initCoord inEnv)
        tShow (aNN, aNS) = aNN <> "-" <> (T.pack . show) aNS
        startP = maybe (VEXInPt initialCs [] (Left 50)) f t_0
            where f i = VEXInPt initialCs [] (Right i)
        kdoePulse = VEXInPt initialCs nAlts (Right dur)
        endP = maybe (VEXInPt initialCs [] (Left 50)) f t_end
            where f i = VEXInPt initialCs [] (Right i)
        -- Switch from NodeStates to RealNodeStates. 
        initialCs = ((fromIntegral <<$>>) . initCoord) inEnv


kdoeName :: [NodeAlteration] -> T.Text
kdoeName alts = T.intercalate "_" $ kdoeName' <$> alts
    where
        kdoeName' (NodeLock nN nS lP) = "Lock_" <> nN <> (T.pack . show) nS
            <> "-" <> (T.pack . show) lP
        kdoeName' (GradientNudge nN nD nP) = (T.pack . show) nD <> nN <> "-"
            <> (T.pack . show) nP

mkAttFilter :: ModelMapping -> ModelLayer -> InitialEnvironment
            -> Validation [VEXInvestigationInvalid]
                          ([(Barcode, Attractor)] -> [(Barcode, Attractor)])
mkAttFilter mM mL inEnv = filter <$> (expAttF <$>
    ((,) <$> mkInEnvFV mL (initCoord inEnv)
         <*> mkExpBCFilter mM (initFilters inEnv, showHidden inEnv)))

-- Filter (Barcode, Attractor) pairs from an experimental run. The first
-- Maybe BarcodeFilter is possibly from the individual experiment, while the
-- second is from the input space figure specification, if that exists and if
-- the Bool to include it in the experiment is set. 
expAttF :: (FixedVec, (Maybe BarcodeFilter, Maybe BarcodeFilter))
        -> (Barcode, Attractor) -> Bool
expAttF (fVec, (mExpBCF, mISFSpec)) (bc, att) =
    attMatch fVec att && bcFilterF mExpBCF bc && bcFilterF mISFSpec bc

mkInEnvFV :: ModelLayer -> [(NodeName, NodeState)]
          -> Validation [VEXInvestigationInvalid] FixedVec
mkInEnvFV mL inEnvPIs
    | (not . null) pinnedRepeats = Failure
        [SMSPinnedInputsHaveRepeats pinnedRepeats]
    | (not . null) nonPresentPinned =
        Failure [UnknownNodesInSMSPinnedInputs nonPresentPinned]
    | (not . null) nonInputNs = Failure
        [NonInputNodesInSMSPinnedInput nonInputNs]
    | (not . null) oobInputs = Failure
        [InValidSMSPinnedInputs oobInputs properInputs]
    | (not . null) unpinnedInputs = Failure [UnpinnedSMSInputs pinningChoices]
    | otherwise = Success $ mkPinnedVec inPts lniBMap inEnvPIs
    where
        pinningChoices = "There are unpinned environmental inputs. Please pin \
            \the following:\n" <> tUPOpts
        tUPOpts = textInputOptions unpinnedInputs
        unpinnedInputs = inPts L.\\ extractedInputs
        extractedInputs = (inputExtract inPts . fst) <$> inEnvPIs
        properInputs =
            "\nPinned Node(s) must be from listed inputs\n" <> txtInOpts
        oobInputs = filter (flip notElem (mconcat inOpts)) inEnvPIs
        txtInOpts = textInputOptions inPts
        inOpts = inputOptions inPts
        nonInputNs = (fst <$> inEnvPIs) L.\\ inputNodeNames
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        inPts = (inputs . modelGraph) mL
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        nonPresentPinned = (fst <$> inEnvPIs) L.\\ mlNodeNames
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL
        pinnedRepeats = (Uniq.repeated . fmap fst) inEnvPIs

mkExpBCFilter :: ModelMapping
              -> (Maybe BarcodeFilter, Either Bool BarcodeFilter)
              -> Validation [VEXInvestigationInvalid]
                         (Maybe BarcodeFilter, Maybe BarcodeFilter)
mkExpBCFilter mM  (maybeExpBCF, eitherISFSpec) =
    bitraverse vBC vBC (maybeExpBCF, dropLeft eitherISFSpec)
    where
        vBC :: Maybe BarcodeFilter
            -> Validation [VEXInvestigationInvalid] (Maybe BarcodeFilter)
        vBC Nothing = Success Nothing
        vBC (Just bc) = Just <$> mkIBBCFilter mM bc
        dropLeft :: Either e a -> Maybe a
        dropLeft (Left _) = Nothing
        dropLeft (Right r) = Just r

mkStepper :: HasCallStack => ExperimentStep -> LayerSpecs -> ExpStepper
mkStepper SynchronousExpStepper lSpecs = SD stepper
    where
        stepper = synchStep (iVecList lSpecs) (tTableList lSpecs)
mkStepper (NoisyExpStepper noiseLevel) lSpecs = SN stepper
    where
        stepper = noisyStep' psStepper noiseLevel lrVec
        lrVec = lRangeVec lSpecs
        psStepper = synchStep (iVecList lSpecs) (tTableList lSpecs)
mkStepper AsynchronousExpStepper lSpecs = AD stepper
    where
        stepper = asyncStep' (U.length (lRangeVec lSpecs)) ttMap ivMap
        ttMap = M.fromList $ zip [0..] $ tTableList lSpecs
        ivMap = M.fromList $ zip [0..] $ iVecList lSpecs


mkInputPulse :: ModelLayer -> VEXInputPulse
             -> Validation [VEXInvestigationInvalid] InputPulse
mkInputPulse mL (VEXInPt vexRICs vexNAlts vexDuration) =
    InputPulse <$> mkRealInputCoordinate mL vexRICs
               <*> mkIntNodeAlterations mL vexNAlts
               <*> pure vexDuration

mkRealInputCoordinate :: ModelLayer
                      -> [(NodeName, RealNodeState)]
                      -> Validation [VEXInvestigationInvalid] RealInputCoord
mkRealInputCoordinate mL vexRealPIs
    | (not . null) pinnedRepeats = Failure
        [NodeAltPinnedInputsHaveRepeats pinnedRepeats]
    | (not . null) nonPresentPinned =
        Failure [UnknownNodesInNodeAltPinnedInputs nonPresentPinned]
    | (not . null) nonInputNs = Failure
        [NonInputNodesInNodeAltPinnedInput nonInputNs]
    | (not . null) oobInputs = Failure
        [InValidRealPinnedInputs oobInputs properInputs]
    | otherwise = Success $ mkPinnedVec inPts lniBMap vexRealPIs
    where
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        properInputs = "\nPinned Node(s) must be real values from the following\
            \ ranges:\n" <> txtInOpts
        txtInOpts = realTextInputOptions inPts
        oobInputs = filter (not . isInBand (inputOptions inPts)) vexRealPIs
        nonInputNs = (fst <$> vexRealPIs) L.\\ inputNodeNames
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        inPts = (inputs . modelGraph) mL
        nonPresentPinned = (fst <$> vexRealPIs) L.\\ mlNodeNames
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL
        pinnedRepeats = (Uniq.repeated . fmap fst) vexRealPIs

-- Is a real-valued input pinned within its valid range?
isInBand :: [[(NodeName, Int)]] -> (NodeName, RealNodeState) -> Bool
isInBand inOpts (pinName, pinRealState) = case nodeOptsStates of
    [] -> False
-- If there is only one NodeName match, that means it is a binary node which is
-- part of a mulit-node input, and also not the bottom of such an input stack.
-- Otherwise it would show up at least twice. Thus, the bottom of the range is
-- taken by the node below it in the stack and the correct range is exclusive on
-- that end. 
    [i] -> pinRealState > (fromIntegral i - 1.0) &&
           pinRealState <= (fromIntegral i)
    is -> pinRealState >= ((fromIntegral . minimum) is) &&
          pinRealState <= ((fromIntegral . maximum) is) 
    where
        nodeOptsStates = snd <$> nodeOpts
        nodeOpts = filter ((==) pinName . fst) $ mconcat inOpts

-- Pretty print the possible real-valued pinnings for environmental inputs. 
realTextInputOptions :: [[DMNode]] -> T.Text
realTextInputOptions inPtNDs = T.intercalate "\n" $ realTxtInputOpt <$> inPtNDs
    where
        realTxtInputOpt :: [DMNode] -> T.Text
        realTxtInputOpt [] = T.empty
        realTxtInputOpt [n] = nName <> theRange
            where
                theRange = ":x, x ∈ [0, " <> (T.pack . show) nRange <> "]"
                (nName, nRange) = nodeRange n
        realTxtInputOpt ns = T.intercalate "\n" $ firstRange:
            (otherOpts <$> rNS)
            where
                otherOpts nN = nN <> ":x, x ∈ (0,1]"
                firstRange = head rNS <> ":x, x ∈ [0,1]"
                rNS = L.reverse $ (nodeName . nodeMeta) <$> ns

mkIntNodeAlterations :: ModelLayer -> [NodeAlteration]
                     -> Validation [VEXInvestigationInvalid] [IntNodeAlteration]
mkIntNodeAlterations mL nAlts
    | (not . null) altRepeats = Failure [NodeAltsRepeat altRepeats]
    | (not . null) nonPresentAlts =
        Failure [UnknownNodesInNodeAlts nonPresentAlts]
    | (not . null) inputAlts = Failure [InputsInNodeAlts inputAlts]
    | (not . null) oobLocks = Failure [InvalidNodeAltLocks oobLocks properLocks]
    | otherwise = Success intAlts
    where
        intAlts = nodeAltTo2IntNodeAlt mL <$> nAlts
        properLocks = "\nLocked Node(s) must be integer values from the\
            \ following options\n" <> txtNodeLockOpts oobLocks mlNodeRanges
        oobLocks = filter (flip notElem mlNodeStates) lockAltStates
        lockAltStates = mapMaybe lockState nAlts
        lockState (GradientNudge _ _ _) = Nothing
        lockState (NodeLock nName nState _) = Just (nName, nState)
        mlNodeStates = concatMap rangeSpread mlNodeRanges
        rangeSpread (nN, nR) = zip (L.repeat nN) [0..nR]
        mlNodeRanges = nodeRange <$> mlNodes
        inputAlts = altNodeNames `L.intersect` mlInputNodeNames
        mlInputNodeNames =
            (fmap (nodeName . nodeMeta) . mconcat . inputs . modelGraph) mL
        nonPresentAlts = altNodeNames L.\\ mlNodeNames
        mlNodeNames = (nodeName . nodeMeta) <$> mlNodes
        mlNodes = layerNodes mL
        altRepeats = Uniq.repeated altNodeNames
        altNodeNames = nodeAltName <$> nAlts

txtNodeLockOpts :: [(NodeName, NodeState)] -> [NodeRange] -> T.Text
txtNodeLockOpts oobLocks mlNodeRanges =
    T.intercalate "\n" $ txtNodeLockOpt mlNodeRanges <$> oobLocks
    where
        txtNodeLockOpt :: [NodeRange] -> (NodeName, NodeState) -> T.Text
        txtNodeLockOpt nRanges (altName, _) = altName <> theRange
            where
                theRange = ":x, x ∈ " <> (T.pack . show) [0..rTop]
                -- txtNodeLockOpts is never evaluated unless I already know that
                -- the node in the NodeLock exists in the ModelLayer
                rTop = (snd . fromJust . L.find ((==) altName . fst)) nRanges

nodeAltTo2IntNodeAlt :: HasCallStack
                     => ModelLayer
                     -> NodeAlteration
                     -> IntNodeAlteration
nodeAltTo2IntNodeAlt mL (NodeLock nName nState lProb) =
    IntNodeLock nIndex nState lProb
    where
        nIndex = lniBMap BM.! nName
        LayerSpecs lniBMap _ _ _ = layerPrep mL
nodeAltTo2IntNodeAlt mL (GradientNudge nName nDirection nProb) =
    IntGradientNudge nIndex rangeTop boolND nProb
    where
        rangeTop = snd $ lrVec U.! nIndex
        nIndex = lniBMap BM.! nName
        boolND = nDirection == NudgeUp
        LayerSpecs lniBMap lrVec _ _ = layerPrep mL

----------------------------------------------------------------------------
-- Conducting experiments:

-- Run all the experiments from a DMInvestigation. The paired Attractors must
-- be checked beforehand. 
runInvestigation :: HasCallStack => ColorMap
                 -> StdGen -> [(HS.HashSet Attractor, LayerExpSpec)]
                 -> [LayerResult]
runInvestigation cMap gen attLExpSpecPairs = snd $
    L.mapAccumL (runLayerExperiments cMap) gen attLExpSpecPairs

runLayerExperiments :: HasCallStack => ColorMap
                    -> StdGen -> (HS.HashSet Attractor, LayerExpSpec)
                    -> (StdGen, LayerResult)
runLayerExperiments cMap gen (atts, lExpSpec) = (newGen, lResult)
    where
        lResult = LayerResult mMap mL eResults $ invesIBundle lExpSpec
        (newGen, eResults) = L.mapAccumL (runExperiment layerBCG atts) gen exps
        layerBCG = mkBarcode cMap mMap lniBMap -- Make (BC, Att) pairs
        exps = experiments lExpSpec
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        mL = layerExpMLayer lExpSpec
        mMap = layerExpMMapping lExpSpec

-- Run a DMExperiment by folding up the InputPulses according to the chosen step
-- style. First filter the attractors available. 
-- Note that when running an experiment, it should be run n times for
-- each attractor in the set, where n is the length of the attractor, starting
-- at the next in the loop each time. AttractorResults are then combined if
-- their Barcodes are identical. 
runExperiment :: HasCallStack =>
            (Attractor -> (Barcode, Attractor)) -> HS.HashSet Attractor
              -> StdGen -> DMExperiment -> (StdGen, ExperimentResult)
runExperiment layerBCG attSet gen ex = (newGen, (exCon, attResults))
    where
        (newGen, attResults) = L.mapAccumL (runAttractor ex) gen filteredAtts
        filteredAtts = attFilter ex $ layerBCG <$> attList
        attList = HS.toList attSet
        exCon = ExpCon (experimentName ex) (expKind ex)


runAttractor :: HasCallStack => DMExperiment
             -> StdGen -> (Barcode, Attractor)
             -> (StdGen, AttractorResult)
runAttractor ex gen (bc, att) = (newGen, (bc, (tmLns, pSpaces)))
    where
        (newGen, tmLns) = L.mapAccumL (runLayerVec ex attL) gen attList
        attList = B.toList att
        -- We drop the last spacing because we do not need to put a pulse line
        -- at the end of the figure. 
        pSpaces = L.init $ pSpace <$> pDurs 
            where
                pSpace (Left dur) = max dur attL
                pSpace (Right dur) = dur
        pDurs = (fmap inputDuration . inputPulses) ex
        attL = length att

runLayerVec :: HasCallStack
            => DMExperiment
            -> Int
            -> StdGen
            -> LayerVec
            -> (StdGen, Timeline)
runLayerVec ex attL gen lVec = B.tail <$> (L.foldl' pulseF (gen, tmlnSeed) iPls)
    where
--      We drop the seed AnnotatedLayerVec because this way we can alter, step,
--      and annotate each AnnotatedLayerVec in the Timeline in its own unfoldr
--      step. 
        tmlnSeed = B.singleton $ U.map (\x -> (x, True)) lVec
        pulseF = pulseFold attL (expStepper ex)
        iPls = inputPulses ex

pulseFold :: HasCallStack
          => Int
          -> ExpStepper
          -> (StdGen, Timeline)
          -> InputPulse
          -> (StdGen, Timeline)
pulseFold attL sTPR (gen, tmLn) iPulse = case sTPR of
    (SD stepper) -> (newGen, tmLn <> newTmLn)
        where
            newTmLn = B.unfoldrExactN iDur sdUnfolder (intInputPrLV, uGen)
            sdUnfolder (aVec, aGen) = (anVec, (aNextVec, aNewGen))
                where
                    aNextVec = stepper $ (fst . U.unzip) anVec
                    (anVec, aNewGen) =
                        expStepPrime justRealICoords nAlts aVec aGen
    (SN stepper) -> (newGen, tmLn <> newTmLn)
        where
            newTmLn = B.unfoldrExactN iDur snUnfolder (intInputPrLV, uGen)
            snUnfolder (aVec, aGen) = (anVec, (aNextVec, aNewGen))
                where
                    (aNextVec, aNewGen) = stepper steppingVec noisyStepGen
                    steppingVec = (fst . U.unzip) anVec
                    (anVec, noisyStepGen) =
                        expStepPrime justRealICoords nAlts aVec aGen
    (AD stepper) -> (newGen, tmLn <> newTmLn)
        where
            newTmLn = B.unfoldrExactN iDur adUnfolder (intInputPrLV, uGen)
            adUnfolder (aVec, aGen) = (anVec, (aNextVec, aNewGen))
                where
                    (aNextVec, aNewGen) = stepper steppingVec asyncStepGen
                    steppingVec = (fst . U.unzip) anVec
                    (anVec, asyncStepGen) =
                        expStepPrime justRealICoords nAlts aVec aGen
    where
        (newGen, uGen) = split gen
        intInputPrLV = U.update startVec roundedIntICoords
        roundedIntICoords = U.map (round <$>) justIntICoords
        (justIntICoords, justRealICoords) = U.partition (isInt 7 . snd) iCoords
        iCoords = realInputCoord iPulse
        startVec = (fst . U.unzip . B.last) tmLn
        nAlts = intNodeAlterations iPulse
        iDur = case inputDuration iPulse of
            Left d -> max d attL
            Right d -> d

-- Fix an integer value for a real-valued input coordinate. 
coordFix :: HasCallStack =>
    RealInputCoord -> LayerVec -> StdGen -> (LayerVec, StdGen)
coordFix iCoord lVec gen = (U.update lVec fixedVec, newGen)
    where
        (fixedVec, newGen) = U.foldr' setInput (U.empty, gen) iCoord
        setInput (nIndex, realNState) (aVec, aGen)
            | realNState >= rand =
                (U.cons (nIndex, ceiling realNState) aVec, uGen)
            | otherwise = (U.cons (nIndex, floor realNState) aVec, uGen)
            where
                (rand, uGen) = uniformR iRange aGen
                iRange = ( (fromInteger . floor) realNState
                         , (fromInteger . ceiling) realNState)

-- Prime a LayerVec with any alterations that a continuous Input setting or
-- mutated DMNode might require, and return it with the vector that denotes
-- which nodes were altered. 
expStepPrime :: HasCallStack
             => RealInputCoord
             -> [IntNodeAlteration]
             -> LayerVec
             -> StdGen
             -> (AnnotatedLayerVec, StdGen)
expStepPrime iCoord nAlts lVec gen = (U.zip alteredVec wasForcedVec, newGen)
    where
        wasForcedVec = (U.replicate vecSize False) U.// wasForcedList
        wasForcedList = const True <<$>> alteredList
        alteredVec = coordFixedVec U.// alteredList
        (alteredList, newGen) = foldr (nodeAlter coordFixedVec) ([], nGen) nAlts
        (coordFixedVec, nGen) = coordFix iCoord lVec gen
        vecSize = U.length lVec

nodeAlter :: HasCallStack
          => LayerVec
          -> IntNodeAlteration
          -> ([(NodeIndex, NodeState)], StdGen)
          -> ([(NodeIndex, NodeState)], StdGen)
nodeAlter _ (IntNodeLock nIndex nState lProb) (fixedPairs, gen)
    | lProb >= rand = ((nIndex, nState):fixedPairs, newGen)
    | otherwise = (fixedPairs, newGen)
    where
        (rand, newGen) = uniformR (0, 1) gen
nodeAlter lVec (IntGradientNudge nIndex nRangeT nDirec nProb) (fixedPairs, gen)
    | (nProb >= rand) && nDirec && (cState < nRangeT) =
        (bumpedUp:fixedPairs, newGen)
    | (nProb >= rand) && (not nDirec) && (cState > 0) =
        (bumpedDown:fixedPairs, newGen)
    | otherwise = (fixedPairs, newGen)
    where
        bumpedDown = (nIndex, cState - 1)
        bumpedUp = (nIndex, cState + 1)
        (rand, newGen) = uniformR (0, 1) gen
        cState = lVec U.! nIndex

-- Consume a stack of environmental inputs, a LayerNameIndexBimap, and a list of
-- user pinning choices, and produce a Vector to set those inputs in a LayerVec,
-- optionally after a real-valued input is stochastically set. 
mkPinnedVec :: (Num a, U.Unbox a, HasCallStack)
            => [[DMNode]]
            -> LayerNameIndexBimap
            -> [(NodeName, a)]
            -> U.Vector (NodeIndex, a)
mkPinnedVec inPts lniBMap pinnedIs = pinnedVec
    where
        pinnedVec = U.fromList $ (BF.first (lniBMap BM.!)) <$> choiceAssocs
        choiceAssocs = concat $ pickStates pinnedIs <$> fixedINs
        fixedINs = L.filter (picker pinnedIs) inPts
        picker pIs nodeStack = any (`elem` (fst <$> pIs)) $
            (nodeName . nodeMeta) <$> nodeStack

-- Consume a List of user pinning choices, a [DMNode] which constitutes a
-- single pinned environmental input, and produce the NodeName-NodeState pairs
-- which represent that pinning in the in the input nodes. 
pickStates :: (Num a, U.Unbox a)
           => [(NodeName, a)]
           -> [DMNode]
           -> [(NodeName, a)]
pickStates inputChoices nodeStack = pickState keyChoice nodeStack
    where
        -- Find the pinning choice relevant to this input
        keyChoice = fromJust $ L.find (findChoice stackNames) inputChoices
        findChoice nNames (nName, _) = nName `elem` nNames
        stackNames = (nodeName . nodeMeta) <$> nodeStack

-- Remember that single-node inputs might be integer-valued, and so must be
-- dealt with separately. 
pickState :: (Num a, U.Unbox a) => (NodeName, a) -> [DMNode] -> [(NodeName, a)]
pickState iChoice@(inputName, _) ns = case L.length ns of
    1 -> [iChoice]
    _ -> (zip zeroNodes $ repeat 0) <> [iChoice] <> (zip oneNodes $ repeat 1)
        where
            zeroNodes = take choiceIndex stackNames
            oneNodes = drop (choiceIndex + 1) stackNames
            choiceIndex = (fromJust . L.findIndex (== inputName)) stackNames
            stackNames = (nodeName . nodeMeta) <$> ns


