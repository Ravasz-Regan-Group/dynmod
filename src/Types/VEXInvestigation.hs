{-# LANGUAGE OverloadedStrings #-}

module Types.VEXInvestigation where

import Utilities
import Types.DMModel
import Types.Simulation
import qualified Data.Text as T
import TextShow
import TextShow.Data.Char (showbString)
import Types.Figures

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

-- In a 5D figure, the order of axes is: x, y, z, w, v. The first five elements
-- of the [NodeName] will be so assigned. Any excess will be ignored and any
-- unspecified axes will be assigned by parsing order of the relevant DMNodes
data ISFSpec = ISFSpec { axesOrdering :: [NodeName]
                       , bcFilter :: Maybe BarcodeFilter
                       , pinnedInputs :: [(NodeName, Int)]
                       } deriving (Eq, Show)

data VEXExperiment = VXTC VEXTimeCourse
                   | TXSC VEXScan
                   deriving (Eq, Show)

-- TimeCourses
data VEXTimeCourse =
-- A parsed GeneralExperiment{}.
    GeneralTC T.Text -- Experiment name
               InitialEnvironment
               ExperimentStep
              [VEXInputPulse]
               ExperimentReps
               FigKinds
               ManualSeed
-- A parsed Pulse1{}.
    | Pulse1 (Duration, Duration) -- t_0 and t_end
              InitialEnvironment
              Duration -- pulse duration
             (NodeName, RealNodeState)
              ExperimentReps
              FigKinds
              ManualSeed
-- A parsed KDOE{}.
    | KnockDOverE (Duration, Duration) -- t_0 and t_end
                   InitialEnvironment
                   Duration -- pulse duration
                  [NodeAlteration]
                   ExperimentReps
                   FigKinds
                   ManualSeed
-- A parsed KDOEAtTransition
    | KDOEAtTransition (Duration, Duration) -- t_0 and t_end
                        InitialEnvironment
                        Duration -- pulse duration
                       (NodeName, RealNodeState)
                       [NodeAlteration]
                        ExperimentReps
                        FigKinds
                        ManualSeed
    deriving (Eq, Show)

-- An integer suppllied when the user wants to manually specify an experiment's
-- RPNG seed for reproducibility purposes. 
type ManualSeed = Maybe Int

data VEXInputPulse = VEXInPt
    { vexRealInputCoord :: [(NodeName, RealNodeState)]
    , vexNodeAlterations :: [NodeAlteration]
    , vexInputDuration :: Duration
    } deriving (Eq, Show)

-- Scans
data VEXScan = VEXScan
    ScanKind
    InitialEnvironment
    [NodeAlteration]
    [(NodeName, RealNodeState)]-- Start runs at real-valued inputs. 
    Max_N
    Relevant_N
    -- StopPhenotypes: Stop a run and restart if you hit one. 
    [(NodeName, PhenotypeName)]
    ExperimentStep
    PlottingNodes
    deriving (Eq, Show)

-- Maximum length of a given run. 
type Max_N = Int
-- The number of total network steps we need to collect. Restart the scan as
-- many times as necessary to reach requiredSteps. 
type Relevant_N = Int

-- Types for figures. Also, for EnvScan, KDOEScan, and EnvKDOEScan Scans, always
-- plot the average time-to-stop for each stop Phenotype. The x-axis for these
-- is always the scan steps, whatever those are. TwoDEnvScan and ThreeDEnvScan
-- are heat maps. 

type PlottingNodes = ([ScanSwitch], [ScanNode])

-- Which Switches' time-spent in each Phenotype distribution figure to plot. 
-- If some of the Phenotypes are loops, also plot how many loops were completed.
type ScanSwitch = NodeName
-- Which loop-Phenotype-and-errors' time-spent distribution figure to plot over
-- the course of the Scan. (Implement later --Pete Regan, April 2nd, 2024). 
-- type CycleErrors = (Phenotype, (CycleErrorName, [SubSpace]))
-- type CycleErrorName = T.Text

-- Which DMNodes average values to plot. 
type ScanNode = NodeName

data ScanKind =
      EnvSc EnvScan
    | KDOESc KDOEScan
    | EnvKDOEScan EnvScan KDOEScan XAxis
    | TwoDEnvScan EnvScan EnvScan DoOverlayValues (Maybe KDOEScan)
    | ThreeDEnvScan EnvScan
                    EnvScan
                    EnvScan
                    DoOverlayValues
                    [WildTypeVsMutantAlt]
    deriving (Eq, Show)

-- Do we overlay the values of a heat map onto the figure?
type DoOverlayValues = Bool

-- To distinguish from the Scan's regular node alterations
type WildTypeVsMutantAlt = NodeAlteration

-- In the list of 2D figures produced in a EnvKDOEScan, which variable is the
-- x-axis in each figure. The other will then be the variable which increments
-- in each figure. Defaults to KDOEX
data XAxis = EnvX
           | KDOEX
            deriving (Eq, Show)

instance TextShow XAxis where
    showb EnvX = showbString "EnvX"
    showb KDOEX = showbString "KDOEX"


data EnvScan =
-- Specify directly which input levels to scan. 
-- NodeName of the first DMNode in an input [[DMNode]]
    StepSpecESC NodeName [Double]
  | RangeESC NodeName
-- Start state, which might be any on the whole range of the input, and so not
-- necessarily a valid NodeState. 
             Double
             Double -- End state, ditto
             ScanSteps -- Steps to get there
  | WholeESC NodeName ScanSteps -- Scan over the whole input range. 
    deriving (Eq, Show)

envScanInputName :: EnvScan -> NodeName
envScanInputName (StepSpecESC nName _) = nName
envScanInputName (RangeESC nName _ _ _) = nName
envScanInputName (WholeESC nName _) = nName

-- All of the DMNodes in the ScanSteps Tuple will be probabilistically locked to
-- the specified states in sync, at the number of probabilities specified. 
data KDOEScan =
-- Specify directly which probabilities to scan. 
    StepSpecKDOESC [(NodeName, NodeState)] [Probability]
-- Pick two probabilities and the number of steps to get from one to the other. 
  | RangeKDOESC [(NodeName, NodeState)] Probability Probability ScanSteps
-- Start at 0% and get to 100% over the number of steps specified. 
  | WholeKDOESC [(NodeName, NodeState)] ScanSteps
    deriving (Eq, Show)
  

type ScanSteps = Int -- The number of steps from min to max in a Scan. 

-- Errors that might occur when combining parsed VEXLayerExpSpecs with a parsed
-- DMModel in order to make DMExperiments. 
data VEXInvestigationInvalid =
      DuplicatedLayerNames [NodeName]
    | VEXLayerNameNotInDMModel T.Text
    | MatchedModelIsCoarsest T.Text
    | AxesOrderHasRepeats [NodeName]
    | UnknownNodesInAxesOrder [NodeName]
    | NonInputOrPinnedNodesInAxesOrder [NodeName]
    | MultipleAxesNodesFromSingleInput [[NodeName]]
    | ISDPinnedInputsHaveRepeats [NodeName]
    | UnknownNodesInISDPinnedInputs [NodeName]
    | NonInputNodesInISDPinnedInput [NodeName]
    | InValidISDPinnedInputs [(NodeName, NodeState)] T.Text
    | ExcessUnpinnedISDInputs T.Text
    | BCFSwitchRepeats [NodeName]
    | BCFPhenotypeRepeats [NodeName]
    | UnknownSwitchesInBCFilter [NodeName]
    | UnknownPhenotypesInBCFilter [(NodeName, PhenotypeName)]
    | MismatchedBCFPhenotypes [(PhenotypeName, NodeName, T.Text)]
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
    | LimitedInputNodesRepeat [NodeName]
    | UnknownNodesInLimitedInputs [NodeName]
    | LimitedInputsNotInputs [NodeName]
    | LimitedInputsNotTopLevel [NodeName]
    | InvalidInputLimits [(NodeName, [Int], [Int])]
    | KDOEATFlipDoesNotChangeStartingModelState (NodeName, RealNodeState)
    | UnknownNodesInNodeBarChart [NodeName]
    | UnknownOrNonPhenotypedSwitchesInPHBarChart [NodeName]
    | NonInputNodeInInputScan NodeName
    | UnknownNodeInInputScan NodeName
    | DuplicateEnvScanNodeInScan NodeName
    | InputScanNodeNotTopLevel NodeName
    | InValidInputScanStart NodeName Double (Int, Int)
    | InValidInputScanEnd NodeName Double (Int, Int)
    | InvalidInputScanValues NodeName [Double] (Int, Int)
    | KDOEScanLocksRepeat [NodeName]
    | UnknownNodesInKDOEScanLocks [NodeName]
    | InputsInKDOEScanLocks [NodeName]
    | InvalidKDOEScanLocks [(NodeName, NodeState)] T.Text
    | KDOESharedNodesInSteppedAndAlts [NodeName]
    | KDOESharedNodesIn3DVSAndAlts [NodeName]
    | ScanSwitchRepeats [NodeName]
    | UnknownSwitchesInScan [NodeName]
    | NonPhenotypedSwitchesInScan [NodeName]
    | ScanNodeRepeats [NodeName]
    | UnknownScanNodes [NodeName]
    | StopSwitchRepeats [NodeName]
    | StopPhenotypeRepeats [NodeName]
    | UnknownSwitchesInStopPhenotype [NodeName]
    | UnknownPhenotypesInStopPhenotype [(NodeName, PhenotypeName)]
    | MismatchedStopPhenotypes [(PhenotypeName, NodeName, T.Text)]
    | LoopStopPhenotypes [NodeName]
    | EmptyScanSwitchesAndScanNodes
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
vexErrorPrep (NonInputOrPinnedNodesInAxesOrder nNms) =
    "NonInputOrPinnedNodesInAxesOrder: " <> T.intercalate ", " nNms
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
        T.intercalate ", " (showt <$> badPairs) <> properPairText
vexErrorPrep (ExcessUnpinnedISDInputs pChs) =
    "ExcessUnpinnedISDInputs: " <> pChs
vexErrorPrep (BCFSwitchRepeats nNms) = "BCFSwitchRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (BCFPhenotypeRepeats nNms) = "BCFPhenotypeRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownSwitchesInBCFilter nNms) =
    "UnknownSwitchesInBCFilter: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownPhenotypesInBCFilter badPairs) =
    "UnknownPhenotypesInBCFilter: " <> T.intercalate ", " (showt <$> badPairs)
vexErrorPrep (MismatchedBCFPhenotypes mismatches) =
    "MismatchedBCFPhenotypes: " <> T.intercalate ", " (showt <$> mismatches)
vexErrorPrep (Pulse1FlipDoesNotChangeStartingModelState badPair) =
    "Pulse1FlipDoesNotChangeStartingModelState: " <> showt badPair
vexErrorPrep (SMSPinnedInputsHaveRepeats nNms) =
    "SMSPinnedInputsHaveRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInSMSPinnedInputs nNms) =
    "UnknownNodesInSMSPinnedInputs: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInSMSPinnedInput nNms) =
    "NonInputNodesInSMSPinnedInput: " <> T.intercalate ", " nNms
vexErrorPrep (InValidSMSPinnedInputs badPairs properPairText) =
    "InValidSMSPinnedInputs: " <>
        T.intercalate ", " (showt <$> badPairs) <> properPairText
vexErrorPrep (UnpinnedSMSInputs pChs) = "UnpinnedSMSInputs: " <> pChs
vexErrorPrep (NodeAltPinnedInputsHaveRepeats nNms) =
    "NodeAltPinnedInputsHaveRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInNodeAltPinnedInputs nNms) =
    "UnknownNodesInNodeAltPinnedInputs: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInNodeAltPinnedInput nNms) =
    "NonInputNodesInNodeAltPinnedInput: " <> T.intercalate ", " nNms
vexErrorPrep (InValidRealPinnedInputs badPairs properPairText) =
    "InValidRealPinnedInputs: " <>
        T.intercalate ", " (showt <$> badPairs) <> properPairText
vexErrorPrep (NodeAltsRepeat nNms) = "NodeAltsRepeat" <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInNodeAlts nNms) = "UnknownNodesInNodeAlts: " <>
    T.intercalate ", " nNms
vexErrorPrep (InputsInNodeAlts nNms) = "InputsInNodeAlts: " <>
    T.intercalate ", " nNms
vexErrorPrep (InvalidNodeAltLocks badPairs properPairText) =
    "InvalidNodeAltLocks: " <>
        T.intercalate ", " (showt <$> badPairs) <> properPairText
vexErrorPrep AbsentSwitchProfiles =
    "AbsentSwitchProfiles: " <>
        "There are no SwitchProfiles in the DMMS file, so we cannot make an \
            \environmental input figure. "
vexErrorPrep (LimitedInputNodesRepeat limRepeats) = "LimitedInputNodesRepeat: "
    <> "There are repeats in the LimitedTo nodes: " <>
    T.intercalate ", " limRepeats
vexErrorPrep (UnknownNodesInLimitedInputs nonPresLim) =
    "UnknownNodesInLimitedInputs: " <> T.intercalate ", " nonPresLim
vexErrorPrep (LimitedInputsNotInputs nonInput) = "LimitedInputsNotInputs: " <>
    T.intercalate ", " nonInput
vexErrorPrep (LimitedInputsNotTopLevel nonTopLevel) =
    "LimitedInputsNotTopLevel; The following are part, but not the top level \
    \of, their respective inputs:\n" <> T.intercalate ", " nonTopLevel
vexErrorPrep (InvalidInputLimits oobLimitations) = "InvalidInputLimits; The \
    \following are out-of-bounds for their inputs, together with the ranges \
    \of those inputs: " <> oobText
    where
        oobText = T.intercalate "\n" oobTexts
        oobTexts = oobPrep <$> oobLimitations
        oobPrep :: (NodeName, [Int], [Int]) -> T.Text
        oobPrep (x, y, z) = x <> ", " <> psh y <> ", " <> psh z
            where
                psh = T.pack . show
vexErrorPrep (KDOEATFlipDoesNotChangeStartingModelState (pN, pSt)) =
    "KDOEATFlipDoesNotChangeStartingModelState: " <> showt (pN, pSt)
vexErrorPrep (UnknownNodesInNodeBarChart nNms) =
    "UnknownNodesInNodeBarChart: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownOrNonPhenotypedSwitchesInPHBarChart nNms) =
    "UnknownOrNonPhenotypedSwitchesInPHBarChart: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodeInInputScan nName) =
    "NonInputNodeInInputScan: " <> nName
vexErrorPrep (UnknownNodeInInputScan nName) =
    "UnknownNodeInInputScan: " <> nName
vexErrorPrep (DuplicateEnvScanNodeInScan nName) =
    "DuplicateEnvScanNodeInScan: " <> nName
vexErrorPrep (InputScanNodeNotTopLevel nName) =
    "InputScanNodeNotTopLevel: " <> nName
vexErrorPrep (InValidInputScanStart nName startSt rnge) =
    "InValidInputScanStart: " <> nName <> ": " <> showt startSt <>
        "; actual range: " <> showt rnge
vexErrorPrep (InValidInputScanEnd nName startSt rnge) =
    "InValidInputScanEnd: " <> nName <> ": " <> showt startSt <>
        "; actual range: " <> showt rnge
vexErrorPrep (InvalidInputScanValues nName oobSteps rnge) =
    "InvalidInputScanValues: " <> nName <> ": " <> showt oobSteps <>
        "; actual range: " <> showt rnge
vexErrorPrep (KDOEScanLocksRepeat nNms) =
    "KDOEScanLocksRepeat" <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInKDOEScanLocks nNms) =
    "UnknownNodesInKDOEScanLocks: " <> T.intercalate ", " nNms
vexErrorPrep (InputsInKDOEScanLocks nNms) = "InputsInKDOEScanLocks: " <>
    T.intercalate ", " nNms
vexErrorPrep (InvalidKDOEScanLocks badPairs properPairText) =
    "InvalidKDOEScanLocks: " <>
        T.intercalate ", " (showt <$> badPairs) <> properPairText
vexErrorPrep (KDOESharedNodesInSteppedAndAlts sharedNodeAlts) =
    "KDOESharedNodesInSteppedAndAlts: " <>
        T.intercalate ", " sharedNodeAlts
vexErrorPrep (KDOESharedNodesIn3DVSAndAlts sharedNodeAlts) =
    "KDOESharedNodesIn3DVSAndAlts: " <>
        T.intercalate ", " sharedNodeAlts
vexErrorPrep (ScanSwitchRepeats nNms) = "ScanSwitchRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownSwitchesInScan nNms) =
    "UnknownSwitchesInScan: " <> T.intercalate ", " nNms
vexErrorPrep (NonPhenotypedSwitchesInScan nNms) =
    "NonPhenotypedSwitchesInScan: " <> T.intercalate ", " nNms
vexErrorPrep (ScanNodeRepeats nNms) =
    "ScanNodeRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownScanNodes nNms) =
    "UnknownScanNodes: " <> T.intercalate ", " nNms
vexErrorPrep (StopSwitchRepeats nNms) = "StopSwitchRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (StopPhenotypeRepeats nNms) = "StopPhenotypeRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownSwitchesInStopPhenotype nNms) =
    "UnknownSwitchesInStopPhenotype: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownPhenotypesInStopPhenotype badPairs) =
    "UnknownPhenotypesInStopPhenotype: " <>
        T.intercalate ", " (showt <$> badPairs)
vexErrorPrep (MismatchedStopPhenotypes mismatches) =
    "MismatchedStopPhenotypes: " <> T.intercalate ", " (showt <$> mismatches)
vexErrorPrep (LoopStopPhenotypes loopPhNames) = "LoopStopPhenotypes: " <> 
    "Only point phenotypes may be stop conditions: " <>
    T.intercalate ", " (showt <$> loopPhNames)
vexErrorPrep EmptyScanSwitchesAndScanNodes = "EmptyScanSwitchesAndScanNodes: "
    <> "At least one ScanSwitch or ScanNode is needed"

-- Base types needed in all of Types.DMInvestigation

data InitialEnvironment = InEnv
    { initCoord :: [(NodeName, NodeState)] -- Every input, pinned
-- BarcodeFilter to set the starting Attractor(s) for the experiment. 
    , initFilters :: Maybe BarcodeFilter
    } deriving (Eq, Show)

data ExperimentStep = SynchronousExpStepper
                    | NoisyExpStepper Probability
                    | AsynchronousExpStepper
                    deriving (Eq, Show, Ord)

instance TextShow ExperimentStep where
    showb SynchronousExpStepper = showbString "SynchronousExpStepper"
    showb (NoisyExpStepper prob) = showbString "NoisyExpStepper " <> showb prob
    showb AsynchronousExpStepper = showbString "AsynchronousExpStepper"

type ExperimentReps = Int

data FigKinds = FigKinds {
      nodeTimeCourse :: DoNodeTimeCourse
    , phenotypeTimeCourse :: DoPhenotypeTimeCourse
    , nodeAvgBars :: AvgBChartNodes
    , phenotypeAvgBars :: AvgBChartSwitches
    } deriving (Eq, Show)

type DoNodeTimeCourse = Bool
type DoPhenotypeTimeCourse = Bool
type AvgBChartNodes = [NodeName]
type AvgBChartSwitches = [NodeName]

-- Default FigKinds
defFigKinds :: FigKinds
defFigKinds = FigKinds True False [] []

