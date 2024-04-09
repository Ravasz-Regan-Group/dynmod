{-# LANGUAGE OverloadedStrings #-}

module Types.DMInvestigation
    ( mkDMInvestigation
    , samplingVal
    , vexErrorPrep
    , layerMatch
    , inputOptions
    , textInputOptions
    , TCExpMeta(..)
    , TCExpKind(..)
    , VEXInvestigation
    , VEXLayerExpSpec(..)
    , ISFSpec(..)
    , Sampling(..)
    , SamplingParameters(..)
    , BarcodeFilter(..)
    , VEXExperiment(..)
    , VEXTimeCourse(..)
    , InitialEnvironment(..)
    , ExperimentStep(..)
    , VEXInputPulse(..)
    , NodeAlteration(..)
    , isNodeLock
    , RealInputCoord
    , RealNodeState
    , NudgeDirection(..)
    , GeneralDuration(..)
    , Duration
    , ExperimentReps
    , VEXInvestigationInvalid(..)
    , LayerResult(..)
    , ExperimentResult(..)
    , FigKinds(..)
    , DoNodeTimeCourse
    , DoPhenotypeTimeCourse
    , AvgBChartNodes
    , AvgBChartSwitches
    , AttractorResult
    , RepResults
    , ExpSpreadResults
    , Timeline
    , AnnotatedLayerVec
    , RealTimeline
    , RealAnnotatedLayerVec
    , WasForced
    , AvgWasForced
    , PhenotypeWeights
    , PulseSpacing
    , runInvestigation
    , pickStates
    ) where

import Utilities
import Types.DMModel
import Types.Simulation
import Types.Figures
import Types.DMInvestigation.TimeCourse
import Types.DMInvestigation.Scan
import Types.VEXInvestigation
import Data.Validation
import qualified Data.Vector.Unboxed as U
import Data.Vector.Instances()
import qualified Data.Vector as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.Bimap as BM
import qualified Control.Parallel.Strategies as P
import System.Random
import qualified Data.List as L

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

data LayerResult = LayerResult
    { layerResultMM :: ModelMapping
    , layerResultML :: ModelLayer
    , layerResultERs :: [ExperimentResult]
    , layerResultIB :: Maybe InputBundle -- Should dynmod create a 5D figure?
    } deriving (Eq, Show)

data ExperimentResult = TCExpRes TimeCourseResult
                      | ScanExpRes ScanResult
                      deriving (Eq, Show)

data DMExperiment = TCDMEx DMTimeCourse
                  | ScDMex DMScan

----------------------------------------------------------------------------
-- Validating vex files:

-- Produce a DMInvestigation, or a [VEXInvestigationInvalid] to tell us what
-- went wrong. The only thing that we validate at parse is that all layer names
-- in the VEX file are unique. 
mkDMInvestigation :: DMModel -> [VEXLayerExpSpec]
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


mkLayerExpSpec :: ((ModelMapping, ModelLayer), VEXLayerExpSpec)
               -> Validation [VEXInvestigationInvalid] LayerExpSpec
mkLayerExpSpec ((mM, mL), vLExSpec) =
    LayerExpSpec <$> pure mM
                 <*> pure mL
                 <*> traverse (mkDMInvesIBundle mM mL) (vexISpaceSpec vLExSpec)
                 <*> traverse (mkDMExperiment mM mL) (vexExperiments vLExSpec)

mkDMInvesIBundle :: ModelMapping -> ModelLayer -> ISFSpec
                 -> Validation [VEXInvestigationInvalid] InputBundle
mkDMInvesIBundle mM mL isfSpec = case filter (not . L.null . snd . snd) mM of
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
        Failure [NonInputOrPinnedNodesInAxesOrder nonInputAxes]
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
        axesRepeats = repeated axesOrd
        nonInputAxes = filter (flip notElem inputNodeNamesFlat) axesOrd
        inputNodeNamesFlat = mconcat inputNodeNames
        inputNodeNames = (nodeName . nodeMeta) <<$>> nonPinnedIs
        nonPinnedIs = L.filter (pinnedIF inputChoices) initialInputs
        pinnedIF iCs nodeStack = not $ any (`elem` (fst <$> iCs)) stackNames
            where stackNames = (nodeName . nodeMeta) <$> nodeStack
        initialInputs = (inputs . modelGraph) mL
        mlInputNames = (fmap (nodeName . nodeMeta) . layerNodes) mL

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
        leftoverN = tShow $ (L.length unpinnedInputs) - 5
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
        pinnedRepeats = (repeated . fmap fst) pinnedIs

mkDMExperiment :: ModelMapping -> ModelLayer -> VEXExperiment
               -> Validation [VEXInvestigationInvalid] DMExperiment
mkDMExperiment mM mL (VXTC v) = TCDMEx <$> (mkTimeCourse mM mL v)
mkDMExperiment mM mL (TXSC e) = ScDMex <$> (mkDMScan mM mL e)

----------------------------------------------------------------------------
-- Conducting experiments:

-- Run all the experiments from a DMInvestigation. The paired Attractors must
-- be checked beforehand. 
runInvestigation :: ColorMap
                 -> StdGen
                 -> [(HS.HashSet Attractor, LayerExpSpec)]
                 -> [LayerResult]
runInvestigation cMap gen attLExpSpecPairs = snd $
    L.mapAccumL (runLayerExperiments cMap) gen attLExpSpecPairs

runLayerExperiments :: ColorMap
                    -> StdGen
                    -> (HS.HashSet Attractor, LayerExpSpec)
                    -> (StdGen, LayerResult)
runLayerExperiments cMap gen (atts, lExpSpec) = (newGen, lResult)
    where
        lResult = LayerResult mMap mL eResults $ invesIBundle lExpSpec
        (newGen, eResults) =
            L.mapAccumL (runExperiment phData layerBCG atts) gen exps
        layerBCG = mkBarcode cMap mMap lniBMap -- Make (BC, Att) pairs
        exps = experiments lExpSpec
        phData = (lniBMap, phs)
        phs = concatMap (snd . snd) mMap
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        mL = layerExpMLayer lExpSpec
        mMap = layerExpMMapping lExpSpec

-- Run a DMExperiment by folding up the InputPulses according to the chosen step
-- style. First filter the attractors available. 
-- Note that when running an experiment, it should be run n times for
-- each attractor in the set, where n is the length of the attractor, starting
-- at the next in the loop each time. AttractorResults are then combined if
-- their Barcodes are identical. 
runExperiment :: (LayerNameIndexBimap, [Phenotype])
              -> (Attractor -> (Barcode, Attractor))
              -> HS.HashSet Attractor
              -> StdGen
              -> DMExperiment
              -> (StdGen, ExperimentResult)
runExperiment phData layerBCG attSet gen ex = case ex of
    TCDMEx tcExp -> (newGen, TCExpRes (expMeta, attResults))
        where
            (newGen, attResults) =
                L.mapAccumL (runAttractor phData tcExp) gen filteredAtts
            filteredAtts = tcAttFilter tcExp $ layerBCG <$> attList
            attList = HS.toList attSet
            expMeta = tcExpMeta tcExp
--     ScDMex scanExp ->

runAttractor :: (LayerNameIndexBimap, [Phenotype])
             -> DMTimeCourse
             -> StdGen
             -> (Barcode, Attractor)
             -> (StdGen, AttractorResult)
runAttractor (lniBMap, phs) tcEx gen (bc, att) = (newGen, (bc, bundledRs))
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

-- Check the validity of any LimitedTo inputs. 
samplingVal :: ModelLayer -> Sampling
            -> Validation [VEXInvestigationInvalid] Sampling
samplingVal _ r@(ReadOnly _) = Success r
samplingVal mL (SampleOnly sParams) =
    SampleOnly <$> (SamplingParameters rN nN nP <$> limitedInputsV mL lims)
    where
        (rN, nN) = (randomN sParams, noisyN sParams)
        (nP, lims) = (noisyP sParams, limitedInputs sParams)
samplingVal mL (ReadAndSample sParams f) =
    ReadAndSample
        <$> (SamplingParameters rN nN nP <$> limitedInputsV mL lims)
        <*> pure f
    where
        (rN, nN) = (randomN sParams, noisyN sParams)
        (nP, lims) = (noisyP sParams, limitedInputs sParams)

limitedInputsV :: ModelLayer -> [(NodeName, [Int])]
               -> Validation [VEXInvestigationInvalid] [(NodeName, [Int])]
limitedInputsV mL lims
    | (not . null) limRepeats = Failure [LimitedInputNodesRepeat limRepeats]
    | (not . null) nonPresLim = Failure [UnknownNodesInLimitedInputs nonPresLim]
    | (not . null) nonInput = Failure [LimitedInputsNotInputs nonInput]
    | (not . null) nonTopLevel = Failure [LimitedInputsNotTopLevel nonTopLevel]
    | (not . null) oobLimitations = Failure [InvalidInputLimits oobLimitations]
    | otherwise = Success lims
    where
        oobLimitations = foldr (oobF iDegs) [] lims
        oobF degs (limN, limStates) oobs = case degs M.!? limN of
            Nothing -> oobs
            Just d -> case (filter (flip notElem [0..(d-1)]) limStates) of
                [] -> oobs
                xs -> (limN, xs, [0..(d-1)]):oobs
        iDegs = M.fromList $ zip topLevelNames $ inputDegrees <$> inputStacks
        nonTopLevel = filter (flip notElem topLevelNames) limNames
        nonInput = filter (flip notElem inputStackNames) limNames
        nonPresLim = filter (flip notElem layerNodeNames) limNames
        limRepeats = repeated limNames
        limNames = fst <$> lims
        topLevelNames = (nodeName . nodeMeta) <$> topLevels
--      Here we take advantage of the fact that inputs calls soleSelfLoops on
--      the LayerGraph as the seeds for finding the layer inputs, so they will
--      always be the head of the input lists. 
        topLevels = head <$> inputStacks
        inputStackNames = concatMap (fmap (nodeName . nodeMeta)) inputStacks
        inputStacks = (inputs . modelGraph) mL
        layerNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL

