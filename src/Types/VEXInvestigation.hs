{-# LANGUAGE OverloadedStrings #-}

module Types.VEXInvestigation where

import Utilities
import Types.DMModel
import Types.Simulation
import qualified Data.Text as T
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
    GeneralExp T.Text -- Experiment name
               InitialEnvironment
               ExperimentStep
              [VEXInputPulse]
               ExperimentReps
               FigKinds
-- A parsed Pulse1{}.
    | Pulse1 (Duration, Duration) -- t_0 and t_end
              InitialEnvironment
              Duration -- pulse duration
             (NodeName, RealNodeState)
              ExperimentReps
              FigKinds
-- A parsed KDOE{}.
    | KnockDOverE (Duration, Duration) -- t_0 and t_end
                   InitialEnvironment
                   Duration -- pulse duration
                  [NodeAlteration]
                   ExperimentReps
                   FigKinds
-- A parsed KDOEAtTransition
    | KDOEAtTransition (Duration, Duration) -- t_0 and t_end
                        InitialEnvironment
                        Duration -- pulse duration
                       (NodeName, RealNodeState)
                       [NodeAlteration]
                        ExperimentReps
                        FigKinds
    deriving (Eq, Show)


data VEXInputPulse = VEXInPt
    { vexRealInputCoord :: [(NodeName, RealNodeState)]
    , vexNodeAlterations :: [NodeAlteration]
    , vexInputDuration :: Duration
    } deriving (Eq, Show)

data NodeAlteration = NodeLock NodeName NodeState LockProbability
                    | GradientNudge NodeName
                                    NudgeDirection
                                    NudgeProbability
                    deriving (Eq, Show, Ord)

isNodeLock :: NodeAlteration -> Bool
isNodeLock (NodeLock _ _ _) = True
isNodeLock (GradientNudge _ _ _) = False

-- isGradientNudge :: NodeAlteration -> Bool
-- isGradientNudge (GradientNudge _ _ _) = True
-- isGradientNudge (NodeLock _ _ _) = False

data NudgeDirection = NudgeUp
                    | NudgeDown
                    deriving (Eq, Show, Ord, Enum)

-- Scans
data VEXScan = VEXScan
    ScanKind
    InitialEnvironment
    (Maybe [(NodeName, RealNodeState)]) -- Start runs at real-valued inputs. 
    Max_N
    Relevant_N
    -- StopPhenotypes: Stop a run and restart if you hit one. 
    [(ScanSwitch, PhenotypeName)]
    ExperimentStep
    [ScanSwitch]
    {-[CycleErrors]-}
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

-- Which Switches' time-spent in each Phenotype distribution figure to plot. 
-- If some of the Phenotypes are loops, also plot how many loops were completed.
type ScanSwitch = NodeName
-- Which loop-Phenotype-and-errors' time-spent distribution figure to plot over
-- the course of the Scan. (Implement later --Pete Regan, April 2nd, 2024). 
-- type CycleErrors = (Phenotype, (CycleErrorName, [SubSpace]))
-- type CycleErrorName = T.Text

data ScanKind =
      EnvSc EnvScan
    | KDOESc KDOEScan
    | EnvKDOEScan EnvScan KDOEScan X_Axis
    | TwoDEnvScan EnvScan EnvScan (Maybe KDOEScan)
    | ThreeDEnvScan EnvScan EnvScan EnvScan [NodeAlteration]
    deriving (Eq, Show)

-- In the list of 2D figures produced in a EnvKDOEScan, which variable is the
-- x-axis in each figure. The other will then be the variable which increments
-- in each figure. Defaults to KDOEX
data X_Axis = EnvX
            | KDOEX
            deriving (Eq, Show)

data EnvScan = ESC NodeName  -- Input DMNode to scan over
                   NodeState -- Start State
                   NodeState -- End State
                   ScanSteps -- Steps to get there
                   deriving (Eq, Show)

-- All of the DMNodes in the ScanSteps Tuple will be gradually locked to the
-- specified states in sync, over the number of steps specified, starting at 0%
-- and ending at 100%. 
data KDOEScan = KDOESC [NodeAlteration]
                       (ScanSteps, [(NodeName, NodeState)])
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
    | InValidInputScanInputs [(NodeName, NodeState)] T.Text
    | KDOEScanLocksRepeat [NodeName]
    | UnknownNodesInKDOEScanLocks [NodeName]
    | InputsInKDOEScanLocks [NodeName]
    | InvalidKDOEScanLocks [(NodeName, NodeState)] T.Text
    | KDOESharedNodesInSteppedAndAlts [NodeName]
    | ScanSwitchRepeats [NodeName]
    | UnknownSwitchesInScan [NodeName]
    | NonPhenotypedSwitchesInScan [NodeName]
    | StopSwitchRepeats [NodeName]
    | StopPhenotypeRepeats [NodeName]
    | UnknownSwitchesInStopPhenotype [NodeName]
    | UnknownPhenotypesInStopPhenotype [(NodeName, PhenotypeName)]
    | MismatchedStopPhenotypes [(PhenotypeName, NodeName, T.Text)]
    | LoopStopPhenotypes [NodeName]
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
        T.intercalate ", " (tShow <$> badPairs) <> properPairText
vexErrorPrep (ExcessUnpinnedISDInputs pChs) =
    "ExcessUnpinnedISDInputs: " <> pChs
vexErrorPrep (BCFSwitchRepeats nNms) = "BCFSwitchRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (BCFPhenotypeRepeats nNms) = "BCFPhenotypeRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownSwitchesInBCFilter nNms) =
    "UnknownSwitchesInBCFilter: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownPhenotypesInBCFilter badPairs) =
    "UnknownPhenotypesInBCFilter: " <> T.intercalate ", " (tShow <$> badPairs)
vexErrorPrep (MismatchedBCFPhenotypes mismatches) =
    "MismatchedBCFPhenotypes: " <> T.intercalate ", " (tShow <$> mismatches)
vexErrorPrep (Pulse1FlipDoesNotChangeStartingModelState badPair) =
    "Pulse1FlipDoesNotChangeStartingModelState: " <> tShow badPair
vexErrorPrep (SMSPinnedInputsHaveRepeats nNms) =
    "SMSPinnedInputsHaveRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInSMSPinnedInputs nNms) =
    "UnknownNodesInSMSPinnedInputs: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInSMSPinnedInput nNms) =
    "NonInputNodesInSMSPinnedInput: " <> T.intercalate ", " nNms
vexErrorPrep (InValidSMSPinnedInputs badPairs properPairText) =
    "InValidSMSPinnedInputs: " <>
        T.intercalate ", " (tShow <$> badPairs) <> properPairText
vexErrorPrep (UnpinnedSMSInputs pChs) = "UnpinnedSMSInputs: " <> pChs
vexErrorPrep (NodeAltPinnedInputsHaveRepeats nNms) =
    "NodeAltPinnedInputsHaveRepeats: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInNodeAltPinnedInputs nNms) =
    "UnknownNodesInNodeAltPinnedInputs: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodesInNodeAltPinnedInput nNms) =
    "NonInputNodesInNodeAltPinnedInput: " <> T.intercalate ", " nNms
vexErrorPrep (InValidRealPinnedInputs badPairs properPairText) =
    "InValidRealPinnedInputs: " <>
        T.intercalate ", " (tShow <$> badPairs) <> properPairText
vexErrorPrep (NodeAltsRepeat nNms) = "NodeAltsRepeat" <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInNodeAlts nNms) = "UnknownNodesInNodeAlts: " <>
    T.intercalate ", " nNms
vexErrorPrep (InputsInNodeAlts nNms) = "InputsInNodeAlts: " <>
    T.intercalate ", " nNms
vexErrorPrep (InvalidNodeAltLocks badPairs properPairText) =
    "InvalidNodeAltLocks: " <>
        T.intercalate ", " (tShow <$> badPairs) <> properPairText
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
    "KDOEATFlipDoesNotChangeStartingModelState: " <> tShow (pN, pSt)
vexErrorPrep (UnknownNodesInNodeBarChart nNms) =
    "UnknownNodesInNodeBarChart: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownOrNonPhenotypedSwitchesInPHBarChart nNms) =
    "UnknownOrNonPhenotypedSwitchesInPHBarChart: " <> T.intercalate ", " nNms
vexErrorPrep (NonInputNodeInInputScan nName) =
    "NonInputNodeInInputScan: " <> nName
vexErrorPrep (UnknownNodeInInputScan nName) =
    "UnknownNodeInInputScan: " <> nName
vexErrorPrep (InValidInputScanInputs badPairs properPairText) =
    "InValidInputScanInputs: " <>
        T.intercalate ", " (tShow <$> badPairs) <> properPairText
vexErrorPrep (KDOEScanLocksRepeat nNms) =
    "KDOEScanLocksRepeat" <> T.intercalate ", " nNms
vexErrorPrep (UnknownNodesInKDOEScanLocks nNms) =
    "UnknownNodesInKDOEScanLocks: " <> T.intercalate ", " nNms
vexErrorPrep (InputsInKDOEScanLocks nNms) = "InputsInKDOEScanLocks: " <>
    T.intercalate ", " nNms
vexErrorPrep (InvalidKDOEScanLocks badPairs properPairText) =
    "InvalidKDOEScanLocks: " <>
        T.intercalate ", " (tShow <$> badPairs) <> properPairText
vexErrorPrep (KDOESharedNodesInSteppedAndAlts sharedNodeAlts) =
    "KDOESharedNodesInSteppedAndAlts: " <>
        T.intercalate ", " sharedNodeAlts
vexErrorPrep (ScanSwitchRepeats nNms) = "ScanSwitchRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownSwitchesInScan nNms) =
    "UnknownSwitchesInScan: " <> T.intercalate ", " nNms
vexErrorPrep (NonPhenotypedSwitchesInScan nNms) =
    "NonPhenotypedSwitchesInScan: " <> T.intercalate ", " nNms
vexErrorPrep (StopSwitchRepeats nNms) = "StopSwitchRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (StopPhenotypeRepeats nNms) = "StopPhenotypeRepeats: " <>
    T.intercalate ", " nNms
vexErrorPrep (UnknownSwitchesInStopPhenotype nNms) =
    "UnknownSwitchesInStopPhenotype: " <> T.intercalate ", " nNms
vexErrorPrep (UnknownPhenotypesInStopPhenotype badPairs) =
    "UnknownPhenotypesInStopPhenotype: " <>
        T.intercalate ", " (tShow <$> badPairs)
vexErrorPrep (MismatchedStopPhenotypes mismatches) =
    "MismatchedStopPhenotypes: " <> T.intercalate ", " (tShow <$> mismatches)
vexErrorPrep (LoopStopPhenotypes loopPhNames) = "LoopStopPhenotypes" <> 
    " Only point phenotypes may be stop conditions" <>
    T.intercalate ", " (tShow <$> loopPhNames)

-- Base types needed in all of Types.DMInvestigation

type RealNodeState = Double

data InitialEnvironment = InEnv
    { initCoord :: [(NodeName, NodeState)] -- Every input, pinned
-- BarcodeFilter to set the starting Attractor(s) for the experiment. 
    , initFilters :: Maybe BarcodeFilter
    } deriving (Eq, Show)

data ExperimentStep = SynchronousExpStepper
                    | NoisyExpStepper Probability
                    | AsynchronousExpStepper
                    deriving (Eq, Show, Ord)

-- DefaultD is a default duration, and may be altered by the length of the
-- Attractor that the pulse starts in. UserD is specified by the user, and may
-- not be so altered. 
type Duration = GeneralDuration Int

durationMagnitude :: Duration -> Int
durationMagnitude (DefaultD i) = i
durationMagnitude (UserD i) = i

type LockProbability = Probability
-- NudgeDirection is its own sum type, but we need nudging to be fast-ish
-- when we alter NodeStates in the middle of a DMExperiment, so:
-- False = NudgeDown
-- True  = NudgeUp
type BoolNudgeDirection = Bool
type NudgeProbability = Probability

data GeneralDuration a = DefaultD a | UserD a
                       deriving (Eq, Ord, Read, Show)

instance Functor GeneralDuration where
    fmap f (DefaultD x) = DefaultD $ f x
    fmap f (UserD x) = UserD $ f x

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

