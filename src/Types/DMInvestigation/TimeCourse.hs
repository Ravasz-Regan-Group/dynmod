{-# LANGUAGE OverloadedStrings #-}

module Types.DMInvestigation.TimeCourse where

import Utilities
import Types.DMModel
import Types.Simulation
import Types.Figures
import Types.VEXInvestigation
import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import TextShow
import TextShow.Data.Char (showbString)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Bifunctor as BF
import qualified Data.List as L
import qualified Control.Parallel.Strategies as P
import System.Random
import Data.Maybe (mapMaybe, fromJust)

data DMTimeCourse = TCExp {
      tcExpMeta :: TCExpMeta
--    Select the relevant Attractors from those of the whole layer. 
    , tcAttFilter :: [(Barcode, Attractor)] -> [(Barcode, Attractor)]
    , tcExpStepper :: ExpStepper
-- Each run is an [InputPulse], but there is at least one kind of experiment
-- (KDOEAtTransition) where the point is to do a run many times, starting a
-- knockdown/over-expression at a spread of values before, at, and after the
-- start of an input pulse. This spread is generated by the Attractor we start
-- the experiment in, timing the start at [-(L + 1)..(L + 1)], relative to the
-- pulse, where L is the Attractor's length. inputPulseF generates those runs.
-- There will be more of these sorts of experiments in the future. 
    , inputPulseF :: InputPulseF -- Attractor -> [[InputPulse]]
    , manualTCPRNGSeed :: Maybe StdGen
    }

data TCExpMeta = TCEMeta {
      tcExpName :: T.Text
    , tcExpDetails :: T.Text
    , tcExpInitCoord :: RealInputCoord -- Every input, pinned
    , expReps :: ExperimentReps -- Times to repeat this experiment.
    , tcExpKind :: TCExpKind -- Was the parsed VEXTimeCourse general or preset?
    , tcExpFigures :: FigKinds
    , tcManualSeed :: ManualSeed
    }  deriving (Eq, Show)

data TCExpKind = P1
               | KDOE
               | KDOEAtTr
               | GenExp
               deriving (Eq, Show)

instance TextShow TCExpKind where
    showb P1 = showbString "P1"
    showb KDOE = showbString "KDOE"
    showb KDOEAtTr = showbString "KDOEAtTr"
    showb GenExp = showbString "GenExp"

data InputPulse = InputPulse { realInputCoord :: RealInputCoord
                             , intNodeAlterations :: [IntNodeAlteration]
                             , inputDuration :: Duration
                             } deriving (Eq, Show)
type InputPulseF = Attractor -> [[InputPulse]]


-- The commented and real RepResults are equivalent. 
-- type RepResults = ([[[Timeline]]], [[PulseSpacing]])
-- Run the experiment the number of times specified. 
type RepResults = ([ExpSpreadResults], [[PulseSpacing]])
-- Run the experiment over the spread of InputPulses. 
type ExpSpreadResults =  [AttBatch]
-- Run the experiment starting at each point in the Attractor. 
type AttBatch = [Timeline]

type Timeline = B.Vector (AnnotatedLayerVec, [PhenotypeName])
type AnnotatedLayerVec = U.Vector (NodeState, WasForced)
-- Was the NodeState in question forced to this state?
type WasForced = Bool
-- Partial Timeline. This comes up often enough that it saves space
type PTimeLine = B.Vector (U.Vector (NodeState, WasForced))

type RealExpSpreadResults = [RealTimeline]
type RealTimeline = B.Vector (RealAnnotatedLayerVec, PhenotypeWeights)
-- RealAnnotatedLayerVec represents the average NodeState of a series of
-- repeated experiments, associated with various properties.
type RealAnnotatedLayerVec = U.Vector (RealNodeState, AvgWasForced)
-- the "average" amount of forcing, using a simple average over the sum of
-- False(not forced) = 0.0 and True(forced) = 1.0. 0 <= awf <= 1
type AvgWasForced = Double
-- Represents the distribution of present Phenotypes at this time-step. It is
-- weighted over all runs, NOT form among the Switch's Phenotypes. 
type PhenotypeWeights = M.HashMap PhenotypeName Double

data ExpStepper = SD PSStepper
                | SN PNStepper'
                | AD PAStepper'

data ExpStepperKind = SynchronousDeterministic
                    | SynchronousNoisy
                    | AsynchronousDeterministic
                    deriving (Eq, Show, Ord)

instance TextShow ExpStepperKind where
    showb SynchronousDeterministic = showbString "SynchronousDeterministic"
    showb SynchronousNoisy = showbString "SynchronousNoisy"
    showb AsynchronousDeterministic = showbString "AsynchronousDeterministic"

-- The inner run of stepping through the network should be fast-ish, so looking
-- up the NodeIndex each time for node alterations is a bad idea. 
data IntNodeAlteration = IntNodeLock NodeIndex NodeState LockProbability
                       | IntGradientNudge NodeIndex
                                          RangeTop
                                          BoolNudgeDirection
                                          NudgeProbability
                       deriving (Eq, Show, Ord)

-- The output of a TimeCourse experiment, to be rendered to disk for future use. 
data TimeCourseOutput = TCOutput
    { tcOutputParams :: TCOutputParameters
    , tcOutput :: [(Barcode, RepResults)] -- TCResultOutput
    } deriving (Eq, Show)

-- Cogent details of a TimeCourse's specification, in case the output is not
-- being read back in by dynmod in conjuntion with a VEX file. 
data TCOutputParameters = TCPParams
    { tcExpOPMeta :: TCExpOPMeta
    , tcExpOPStepper :: ExpStepperKind
    , tcExpOPPulseSps :: [[PulseSpacing]]
    , tcExpOPMPRNGSeed :: ManualSeed
    } deriving (Eq, Show)
data TCExpOPMeta = TCEOPM
    { tcExpOPName :: TL.Text
    , tcExpOPDetails :: TL.Text
    , tcExpOPReps :: ExperimentReps
    , tcExpOPKind :: TCExpKind
    } deriving (Eq, Show)

-- Eventualy we will also want to write just processed result to disk. 
-- Results to be output to disk
-- data TCResultOutput = TCFull [(Barcode, RepResults)]
--                     | TCProc TCProcdOutput
--                     deriving (Eq, Show)

-- data TCProcdOutput = TCProcdOutput
--   { timeCourseOP :: Maybe (Barcode, [[(RealExpSpreadResults
--                                                     , [PulseSpacing])]])
--   , nodeBarChartOP :: Maybe 
--   , phBarChartOP :: Maybe
--   } deriving (Eq, Show)

mkTimeCourse :: ModelMapping -> ModelLayer -> VEXTimeCourse
             -> Validation [VEXInvestigationInvalid] DMTimeCourse
mkTimeCourse mM mL (GeneralTC exNm inEnv expStep vexPlss exReps fkds
                                                                mPRMNGSeed) =
    TCExp <$> expMeta
          <*> mkAttFilter mM mL inEnv
          <*> pure (mkStepper expStep mL)
          <*> (const <$> listedPulses)
          <*> pure (mkStdGen <$> mPRMNGSeed)
        where
            expMeta = mkTCExpMeta mM
                                  mL
                                  exNm
                                  exNm
                                  initialCs
                                  exReps
                                  GenExp
                                  fkds
                                  mPRMNGSeed
            initialCs = ((fromIntegral <<$>>) . initCoord) inEnv
            listedPulses = pure <$> (traverse (mkInputPulse mL) vexPlss)
mkTimeCourse mM mL (Pulse1 (t_0, t_end) inEnv dur (pName, pState) exReps fkds
                                                                mPRMNGSeed)
    | (isInt 7 pState) && elem (pName, round pState) (initCoord inEnv) = Failure
        [Pulse1FlipDoesNotChangeStartingModelState (pName, pState)]
    | otherwise = 
        TCExp <$> expM
              <*> mkAttFilter mM mL inEnv
              <*> pure (mkStepper SynchronousExpStepper mL)
              <*> (const <$> listedPulses)
              <*> pure (mkStdGen <$> mPRMNGSeed)
        where
            expM = mkTCExpMeta mM
                               mL
                               expName
                               expDetails
                               initialCs
                               exReps
                               P1
                               fkds
                               mPRMNGSeed
            listedPulses = pure <$> (traverse (mkInputPulse mL) vexPlss)
            vexPlss = [startPl, p1Pl, endPl]
            startPl = VEXInPt initialCs [] t_0
            p1Pl = VEXInPt flipedInitialCs [] dur
            endPl = VEXInPt initialCs [] t_end
            flipedInitialCs = case L.find ((pName ==) . fst) initialCs of
                Nothing -> (pName, pState):initialCs
                Just a -> (pName, pState):(L.delete a initialCs)
            -- Switch from NodeStates to RealNodeStates. 
            initialCs = ((fromIntegral <<$>>) . initCoord) inEnv
            expName = "pulse1_" <> pName <> "_wInputs_" <> textInputs
            expDetails = "pulse1_" <> pName <> "-" <> showt pState <>
                "-" <> showt dur <> "_wInputs_" <> textInputs
            textInputs = T.intercalate "_" $ inishowt <$> (initCoord inEnv)
            inishowt (aNN, aNS) = aNN <> "-" <> showt aNS
mkTimeCourse mM mL (KnockDOverE (t_0, t_end) inEnv dur nAlts exReps fkds
                                                                mPRMNGSeed) =
    TCExp <$> expM
          <*> mkAttFilter mM mL inEnv
          <*> pure (mkStepper SynchronousExpStepper mL)
          <*> (const <$> listedPulses)
          <*> pure (mkStdGen <$> mPRMNGSeed)
    where
        expM = mkTCExpMeta mM
                           mL
                           expName
                           expDetails
                           initialCs
                           exReps
                           KDOE
                           fkds
                           mPRMNGSeed
        listedPulses = pure <$> (traverse (mkInputPulse mL) vexPlss)
        vexPlss = [startPl, kdoePl, endPl]
        startPl = VEXInPt initialCs [] t_0
        kdoePl = VEXInPt initialCs nAlts dur
        endPl = VEXInPt initialCs [] t_end
        -- Switch from NodeStates to RealNodeStates. 
        initialCs = ((fromIntegral <<$>>) . initCoord) inEnv
        expName = "KD_OE_" <> kdoeName nAlts <> "_wInputs_" <> textInputs
        expDetails = "KD_OE_" <> kdoeDetails nAlts <> "_wInputs_" <> textInputs
        textInputs = T.intercalate "_" $ inishowt <$> (initCoord inEnv)
        inishowt (aNN, aNS) = aNN <> "-" <> showt aNS
mkTimeCourse mM mL (KDOEAtTransition (t_0, t_end) inEnv pDur (pN, pSt) nAlts 
                                                    exReps fkds mPRMNGSeed)
    | (isInt 7 pSt) && elem (pN, round pSt) (initCoord inEnv) =
        Failure [KDOEATFlipDoesNotChangeStartingModelState (pN, pSt)]
    | otherwise =
        TCExp <$> expM
              <*> mkAttFilter mM mL inEnv
              <*> pure (mkStepper SynchronousExpStepper mL)
              <*> mkKDOEAtTrF mL initialCs flipedInitialCs nAlts stp pDur
              <*> pure (mkStdGen <$> mPRMNGSeed)
        where
            expM = mkTCExpMeta mM
                               mL
                               expNm
                               expDtls
                               initialCs
                               exReps
                               KDOEAtTr
                               fkds
                               mPRMNGSeed
            stp = (t_0, t_end)
            flipedInitialCs = case L.find ((pN ==) . fst) initialCs of
                Nothing -> (pN, pSt):initialCs
                Just a -> (pN, pSt):(L.delete a initialCs)
            -- Switch from NodeStates to RealNodeStates. 
            initialCs = ((fromIntegral <<$>>) . initCoord) inEnv
            expNm = "KDOEAtTr_" <> kdoeName nAlts <> "_wPulse_" <> pN <>
                "_wInputs_" <> textInputs
            expDtls = "KDOEAtTr_" <> kdoeDetails nAlts <> "_wPulse_" <>
                pulseDetails <> "_wInputs_" <> textInputs
            pulseDetails = pN <> "-" <> showt pSt <> "-" <> showt pDur
            textInputs = T.intercalate "_" $ inishowt <$> (initCoord inEnv)
            inishowt (aNN, aNS) = aNN <> "-" <> showt aNS

kdoeName :: [NodeAlteration] -> T.Text
kdoeName alts = T.intercalate "_" $ kdoeName' <$> alts
    where
        kdoeName' (NodeLock nN _ _) = "Lock_" <> nN
        kdoeName' (GradientNudge nN nD _) = showt nD <> nN

kdoeDetails :: [NodeAlteration] -> T.Text
kdoeDetails alts = T.intercalate "_" $ kdoeName' <$> alts
    where
        kdoeName' (NodeLock nN nS lP) = "Lock_" <> nN <> "-" <> showt nS <>
            "-" <> showt lP
        kdoeName' (GradientNudge nN nD nP) = showt nD <> nN <> "-"
            <> showt nP

mkAttFilter :: ModelMapping -> ModelLayer -> InitialEnvironment
            -> Validation [VEXInvestigationInvalid]
                          ([(Barcode, Attractor)] -> [(Barcode, Attractor)])
mkAttFilter mM mL inEnv = filter <$> (expAttF <$>
    ((,) <$> mkInEnvFV mL (initCoord inEnv)
         <*> mkExpBCFilter (initFilters inEnv)))
    where
        mkExpBCFilter Nothing = Success Nothing
        mkExpBCFilter (Just bc) = Just <$> mkIBBCFilter mM bc

-- Filter (Barcode, Attractor) pairs from an experimental run. The 
-- Maybe BarcodeFilter is from the individual experiment. 
expAttF :: (FixedVec, Maybe BarcodeFilter) -> (Barcode, Attractor) -> Bool
expAttF (fVec, mExpBCF) (bc, att) = attMatch fVec att && bcFilterF mExpBCF bc

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
        pinnedRepeats = (repeated . fmap fst) inEnvPIs

inputExtract :: [[DMNode]] -> NodeName -> [DMNode]
inputExtract iPts axesN = mconcat $ filter (f axesN) iPts
    where
        f nName inPt = nName `elem` ((nodeName . nodeMeta) <$> inPt)


-- Consume a stack of environmental inputs, a LayerNameIndexBimap, and a list of
-- user pinning choices, and produce a Vector to set those inputs in a LayerVec,
-- optionally after a real-valued input is stochastically set. 
mkPinnedVec :: (Num a, U.Unbox a)
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


mkIBBCFilter :: ModelMapping -> BarcodeFilter
             -> Validation [VEXInvestigationInvalid] BarcodeFilter
mkIBBCFilter mM bc = case bc of
  OnlyBarCodesWithAny phens -> OnlyBarCodesWithAny <$> phValidate phens
  OnlyBarCodesWithAll phens -> OnlyBarCodesWithAll <$> phValidate phens
  ExcludeBarCodesWithAny phens -> ExcludeBarCodesWithAny <$> phValidate phens
  ExcludeBarCodesWithAll phens -> ExcludeBarCodesWithAll <$> phValidate phens
  where
    phValidate phs
      | (not . null) bcfsRepeats = Failure $ [BCFSwitchRepeats bcfsRepeats]
      | (not . null) bcfPhRepeats =
          Failure $ [BCFPhenotypeRepeats bcfPhRepeats]
      | (not . null) npSwitches =
        Failure $ [UnknownSwitchesInBCFilter npSwitches]
      | (not . null) npPhs = Failure $ [UnknownPhenotypesInBCFilter npPhs]
      | (not . null) misMatchedSwPhs = Failure $
          [MismatchedBCFPhenotypes misMatchedSwPhs]
      | otherwise = Success phs
      where    
        misMatchedSwPhs = mapMaybe phChecker phs
          where
            phChecker (swN, phN)
              | correctSwitch == swN = Nothing
              | otherwise = Just (phN, swN, "Correct Switch: " <> correctSwitch)
              where correctSwitch = swMap M.! phN
        swMap = foldr inverterF M.empty mMPhenotypes
          where
             inverterF (swName, phNms) phM =
                    foldr (\phN mP -> M.insert phN swName mP) phM phNms
        npPhs = filter ((`notElem` phNames) . snd) phs
        bcfPhRepeats = repeated phNames
        phNames = concatMap snd mMPhenotypes
        npSwitches = filter (`notElem` switchNames) (fst <$> phs)
        switchNames = fst <$> mMPhenotypes
        bcfsRepeats = (repeated . fmap fst) phs
        mMPhenotypes = ((fmap . fmap . fmap) phenotypeName .
          (filter (not . null . snd)) . ((fmap . fmap) snd)) mM


mkStepper :: ExperimentStep -> ModelLayer -> ExpStepper
mkStepper SynchronousExpStepper mL = SD stepper
    where
        stepper = synchStep ivList ttList
        LayerSpecs _ _ ttList ivList = layerPrep mL
mkStepper (NoisyExpStepper noiseLevel) mL = SN stepper
    where
        stepper = noisyStep' psStepper noiseLevel lrVec
        psStepper = synchStep ivList ttList
        LayerSpecs _ lrVec ttList ivList = layerPrep mL
mkStepper AsynchronousExpStepper mL = AD stepper
    where
        stepper = asyncStep'' intBOF intBOL ttMap ivMap
        intBOL = (fmap (toIntBiasOrder lniBMap) . biasOrderLast) mMeta
        intBOF = (fmap (toIntBiasOrder lniBMap) . biasOrderFirst) mMeta
        mMeta = modelMeta mL
        ttMap = M.fromList $ zip [0..] ttList
        ivMap = M.fromList $ zip [0..] ivList
        LayerSpecs lniBMap _ ttList ivList = layerPrep mL

toIntBiasOrder :: LayerNameIndexBimap -> BiasOrder -> IntBiasOrder
toIntBiasOrder lniBMap (WholeNode nName) = IntWholeNode (lniBMap BM.! nName)
toIntBiasOrder lniBMap (SpecificState nName nState) =
    IntSpecificState (lniBMap BM.! nName) nState

-- Make the Attractor -> [[InputPulse]] function for the
-- Knockdown/Over-Expression At Transition TimeCourse
mkKDOEAtTrF :: ModelLayer
            -> [(NodeName, RealNodeState)]
            -> [(NodeName, RealNodeState)]
            -> [NodeAlteration]
            -> (Duration, Duration)
            -> Duration
            -> Validation [VEXInvestigationInvalid] InputPulseF
mkKDOEAtTrF mL initialCs flInitialCs nAlts stp pDur =
    case (vInitCs, vFlInitCs, vAlts) of
        (Failure errs, Success _, Success _) -> Failure errs
        (Success _, Failure errs, Success _) -> Failure errs
        (Success _, Success _, Failure errs) -> Failure errs
        (Failure errsA, Failure errsB, Success _) -> Failure (errsA <> errsB)
        (Failure errsA, Success _, Failure errsB) -> Failure (errsA <> errsB)
        (Success _, Failure errsA, Failure errsB) -> Failure (errsA <> errsB)
        (Failure eA, Failure eB, Failure eC) -> Failure (eA <> eB <> eC)
        (Success intInitialCs, Success intFlInitialCs, Success nIAlts) ->
          Success $ attF
            where
                attF :: InputPulseF
                attF y = mkF <$> (spreadF y)
                mkF = mkKDOEAtTrRun intInitialCs intFlInitialCs nIAlts stp pDur
                spreadF att = [-(attL + 1)..(attL + 1)]
                    where attL = B.length att
    where
        vInitCs = mkRealInputCoordinate mL initialCs
        vFlInitCs = mkRealInputCoordinate mL flInitialCs
        vAlts = mkIntNodeAlterations mL nAlts

mkKDOEAtTrRun :: U.Vector (NodeIndex, RealNodeState)
              -> U.Vector (NodeIndex, RealNodeState)
              -> [IntNodeAlteration]
              -> (Duration, Duration)
              -> Duration
              -> Int
              -> [InputPulse]
mkKDOEAtTrRun intInitialCs intFlipedInitialCs nIAlts (t_0, t_end) pDur offSet
    | offSet == 0 = [startPl, kdoeAndPulse, kdoeEnd]
    | offSet < 0 = startNegativePls <> [kdoeAndPulse, kdoeEnd]
    | otherwise = startPl:kdoeAPPostivePls <> [kdoeEnd]
    where
        startNegativePls
            | abs offSet < durationMagnitude t_0 =
                [ InputPulse intInitialCs [] ((offSet +) <$> t_0)
                , InputPulse intInitialCs nIAlts (UserD (-offSet))]
            | otherwise = [InputPulse intInitialCs nIAlts (UserD (-offSet))]
        startPl = InputPulse intInitialCs [] t_0
        kdoeAPPostivePls
            | offSet < durationMagnitude pDur =
                [ InputPulse intFlipedInitialCs [] (UserD offSet)
                , InputPulse intFlipedInitialCs nIAlts ((-offSet +) <$> pDur)]
            | otherwise = [justPulse]
        kdoeAndPulse = InputPulse intFlipedInitialCs nIAlts pDur
        kdoeEnd = InputPulse intInitialCs nIAlts t_end
        justPulse = InputPulse intFlipedInitialCs [] pDur

mkInputPulse :: ModelLayer -> VEXInputPulse
             -> Validation [VEXInvestigationInvalid] InputPulse
mkInputPulse mL (VEXInPt vexRICs vexNAlts vexDuration) =
    InputPulse <$> mkRealInputCoordinate mL vexRICs
               <*> mkIntNodeAlterations mL vexNAlts
               <*> pure vexDuration

mkTCExpMeta :: ModelMapping -> ModelLayer -> T.Text -> T.Text
            -> [(NodeName, RealNodeState)] -> ExperimentReps -> TCExpKind
            -> FigKinds -> ManualSeed
            -> Validation [VEXInvestigationInvalid] TCExpMeta
mkTCExpMeta mM mL expName expDetails initialCs exReps exKnd figKinds mMSeed = 
    TCEMeta <$> pure expName
            <*> pure expDetails
            <*> mkRealInputCoordinate mL initialCs
            <*> pure exReps
            <*> pure exKnd
            <*> mkFigKinds mM mL figKinds
            <*> pure mMSeed

-- Construct a RealInputCoord that pins inputs. This follows the convention that
-- each level of a multi-node input is indicated by a one of the nodenames
-- in its DMNode stack. 
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
        pinnedRepeats = (repeated . fmap fst) vexRealPIs

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
                theRange = ":x, x ∈ [0, " <> showt nRange <> "]"
                (nName, nRange) = nodeRange n
        realTxtInputOpt ns = T.intercalate "\n" $ firstRange:
            (otherOpts <$> rNS)
            where
                otherOpts nN = nN <> ":x, x ∈ (0,1]"
                firstRange = head rNS <> ":x, x ∈ [0,1]"
                rNS = L.reverse $ (nodeName . nodeMeta) <$> ns

-- Make sure that AvgBChartNodes are real NodeNames, and that AvgBChartSwitches
-- are real Switches which are phenotyped. 
mkFigKinds :: ModelMapping -> ModelLayer -> FigKinds
           -> Validation [VEXInvestigationInvalid] FigKinds
mkFigKinds mM mL (FigKinds ntcB phtcB nNs sNs) = 
    FigKinds <$> pure ntcB <*> pure phtcB <*> nNCheck <*> sNsCheck
    where
        nNCheck = case filter (`notElem` layerNNames) nNs of
            [] -> Success nNs
            ns -> Failure $ [UnknownNodesInNodeBarChart ns]
        sNsCheck = case filter (`notElem` switchNames) sNs of
            [] -> Success sNs
            sns -> Failure $ [UnknownOrNonPhenotypedSwitchesInPHBarChart sns]
        layerNNames = (fmap (nodeName . nodeMeta) . layerNodes) mL
        switchNames = fst <$> nonEmptyPhs
        nonEmptyPhs = nonEmptyPhenotypes mM


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
        altRepeats = repeated altNodeNames
        altNodeNames = nodeAltName <$> nAlts

txtNodeLockOpts :: [(NodeName, NodeState)] -> [NodeRange] -> T.Text
txtNodeLockOpts oobLocks mlNodeRanges =
    T.intercalate "\n" $ txtNodeLockOpt mlNodeRanges <$> oobLocks
    where
        txtNodeLockOpt :: [NodeRange] -> (NodeName, NodeState) -> T.Text
        txtNodeLockOpt nRanges (altName, _) = altName <> theRange
            where
                theRange = ":x, x ∈ " <> showt [0..rTop]
                -- txtNodeLockOpts is never evaluated unless I already know that
                -- the node in the NodeLock exists in the ModelLayer
                rTop = (snd . fromJust . L.find ((==) altName . fst)) nRanges

nodeAltTo2IntNodeAlt :: ModelLayer -> NodeAlteration -> IntNodeAlteration
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

nodeAltName :: NodeAlteration -> NodeName
nodeAltName (NodeLock nName _ _) = nName
nodeAltName (GradientNudge nName _ _) = nName


-- Calculate the possible integer pinnings for environmental inputs. 
inputOptions :: [[DMNode]] -> [[(NodeName, Int)]]
inputOptions inPtNDs = inputOpt <$> inPtNDs
    where
        inputOpt :: [DMNode] -> [(NodeName, Int)]
        inputOpt [] = []
        inputOpt [n] = zip (repeat nName) [0..nRange]
            where (nName, nRange) = nodeRange n
        inputOpt ns = (head rNS, 0):(zip rNS $ L.repeat 1)
            where rNS = L.reverse $ (nodeName . nodeMeta) <$> ns

-- Pretty print the possible integer pinnings for environmental inputs. 
textInputOptions :: [[DMNode]] -> T.Text
textInputOptions inPtNDs = T.intercalate "\n" $ txtInputOpt <$> inPtNDs
    where
        txtInputOpt :: [DMNode] -> T.Text
        txtInputOpt [] = T.empty
        txtInputOpt [n] = T.intercalate "\n" $ nName:(iLine <$> [0..nRange])
            where
                iLine i = "    " <> nName <> ":" <> showt i
                (nName, nRange) = nodeRange n
        txtInputOpt ns = T.intercalate "\n" $ nName:("    " <> nName <> ":0"):
                 (otherOpts <$> rNS)
            where
                nName = head rNS
                rNS = L.reverse $ (nodeName . nodeMeta) <$> ns
                otherOpts nN = "    " <> nN <> ":1"

runTimeCourse :: (LayerNameIndexBimap, [Phenotype])
             -> DMTimeCourse
             -> StdGen
             -> (Barcode, Attractor)
             -> (StdGen, (Barcode, RepResults))
runTimeCourse (lniBMap, phs) tcEx gen (bc, att) = (newGen, (bc, bundledRs))
    where
        bundledRs = (annIplResultReps, pSpacess)
        annIplResultReps = (fmap . fmap . fmap) zipper iplResultReps
        iplResultReps = P.parMap P.rdeepseq unpacker seeds
        unpacker (g, ipss) = snd $ L.mapAccumL rIPLF g ipss
        rIPLF = runInputPulseList att pulseF
        pSpacess = pulseSpacing attL lniBMap <<$>> iPulsess
        seeds = zip gens $ L.repeat iPulsess
        (gens, newGen) = genGen ((expReps . tcExpMeta) tcEx) gen
        iPulsess = inputPulseF tcEx $ att
        pulseF = pulseFold attL (tcExpStepper tcEx)
        attL = length att 
        zipper ptl = B.zip ptl $
            phenotypeMatch lniBMap phs (B.map (fst . U.unzip) ptl)

-- Produce the duration of an InputPulse, along with any input changes or node
-- alterations. 
pulseSpacing :: Int -> LayerNameIndexBimap -> InputPulse -> PulseSpacing
pulseSpacing attL lniBMap ipls = (pSpacing, realInputCoord ipls, nodeAltChanges)
    where
        nodeAltChanges = (fmap nAlts . intNodeAlterations) ipls
        nAlts (IntNodeLock nI nS lP) = NodeLock (lniBMap BM.!> nI) nS lP
        nAlts (IntGradientNudge nI _ bND nP) =
            GradientNudge (lniBMap BM.!> nI) nudgeD nP
            where nudgeD | bND = NudgeUp | otherwise = NudgeDown
        pSpacing = (pSpace . inputDuration) ipls
        pSpace (DefaultD dur) = max dur attL
        pSpace (UserD dur) = dur


-- We drop the seed AnnotatedLayerVec (tmlnSeed) on each Timeline because this
-- way we can alter, step, and annotate each AnnotatedLayerVec in the Timeline
-- in its own unfoldr step. 
runInputPulseList :: Attractor
                  -> ((StdGen, PTimeLine) -> InputPulse -> (StdGen, PTimeLine))
                  -> StdGen
                  -> [InputPulse]
                  -> (StdGen, [PTimeLine])
runInputPulseList att pulseF gen iPulses =
    L.mapAccumL (runLayerVec iPulses pulseF) gen lVecList
    where
        runLayerVec iPs pF g lV = B.tail <$> (L.foldl' pF (g, tmlnSeed) iPs)
            where
                tmlnSeed = B.singleton $ U.map (\x -> (x, True)) lV
        lVecList = B.toList att

-- L.foldl' function to consume an InputPulse and add to a Timeline, with
-- various types of steppers. 
pulseFold :: Int -> ExpStepper
          -> (StdGen, PTimeLine) -> InputPulse -> (StdGen, PTimeLine)
pulseFold attL sTPR (gen, tmLn) iPulse = case sTPR of
    (SD stepper) -> (newGen, tmLn <> newTmLn)
        where
            newTmLn = B.unfoldrExactN iDur sdUnfolder (intInputPrLV, uGen)
            sdUnfolder (aVec, aGen) = (anVec, (seedVec, aNewGen))
                where
                    seedVec = (fst . U.unzip) anVec
                    (anVec, aNewGen) =
                        expStepPrime justRealICoords nAlts aGen nextVec
                    nextVec = stepper aVec
    (SN stepper) -> (newGen, tmLn <> newTmLn)
        where
            newTmLn = B.unfoldrExactN iDur snUnfolder (intInputPrLV, uGen)
            snUnfolder (aVec, aGen) = (anVec, (seedVec, aNewGen))
                where
                    seedVec = (fst . U.unzip) anVec
                    (anVec, aNewGen) =
                        expStepPrime justRealICoords nAlts inputGen nextVec
                    (nextVec, inputGen) = stepper aVec aGen
    (AD stepper) -> (newGen, tmLn <> newTmLn)
        where
            newTmLn = B.unfoldrExactN iDur adUnfolder (intInputPrLV, uGen)
            adUnfolder (aVec, aGen) = (anVec, (seedVec, aNewGen))
                where
                    seedVec = (fst . U.unzip) anVec
                    (anVec, aNewGen) =
                        expStepPrime justRealICoords nAlts inputGen nextVec
                    (nextVec, inputGen) = stepper aVec aGen
    where
        (newGen, uGen) = split gen
        intInputPrLV = U.update startVec roundedIntICoords
        roundedIntICoords = U.map (round <$>) justIntICoords
        (justIntICoords, justRealICoords) = U.partition (isInt 7 . snd) iCoords
        iCoords = realInputCoord iPulse
        startVec = (fst . U.unzip . B.last) tmLn
        nAlts = intNodeAlterations iPulse
        iDur = case inputDuration iPulse of
            DefaultD d -> max d attL
            UserD d -> d

-- Prime a LayerVec with any alterations that a continuous Input setting or
-- mutated DMNode might require, and return it with the vector that denotes
-- which nodes were altered. 
expStepPrime :: RealInputCoord
             -> [IntNodeAlteration]
             -> StdGen
             -> LayerVec
             -> (AnnotatedLayerVec, StdGen)
expStepPrime iCoord nAlts gen lVec = (U.zip alteredVec wasForcedVec, newGen)
    where
        wasForcedVec = (U.replicate vecSize False) U.// wasForcedList
        wasForcedList = const True <<$>> alteredList
        alteredVec = coordFixedVec U.// alteredList
        (alteredList, newGen) = foldr (nodeAlter coordFixedVec) ([], nGen) nAlts
        (coordFixedVec, nGen) = coordFix iCoord lVec gen
        vecSize = U.length lVec

nodeAlter :: LayerVec
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

-- Fix an integer value for a real-valued input coordinate. 
coordFix :: RealInputCoord -> LayerVec -> StdGen -> (LayerVec, StdGen)
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



