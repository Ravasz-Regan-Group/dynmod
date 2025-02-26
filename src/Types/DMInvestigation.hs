{-# LANGUAGE OverloadedStrings #-}

module Types.DMInvestigation
    ( mkDMInvestigation
    , samplingVal
    , vexErrorPrep
    , layerMatch
    , inputOptions
    , textInputOptions
    , DMExperiment(..)
    , DMTimeCourse(..)
    , TCExpMeta(..)
    , TCExpKind(..)
    , SCExpMeta(..)
    , ExpMeta(..)
    , VEXInvestigation
    , VEXLayerExpSpec(..)
    , LayerExpSpec(..)
    , ISFSpec(..)
    , Sampling(..)
    , SamplingParameters(..)
    , BarcodeFilter(..)
    , VEXExperiment(..)
    , VEXTimeCourse(..)
    , InitialEnvironment(..)
    , ExperimentStep(..)
    , VEXInputPulse(..)
    , ManualSeed
    , NodeAlteration(..)
    , WildTypeVsMutantAlt
    , nodeAltName
    , isNodeLock
    , RealInputCoord
    , RealNodeState
    , NudgeDirection(..)
    , GeneralDuration(..)
    , Duration
    , ExperimentReps
    , VEXInvestigationInvalid(..)
    , LayerResultIO(..)
    , LayerResult(..)
    , ExperimentResult(..)
    , FigKinds(..)
    , defFigKinds
    , DoNodeTimeCourse
    , DoPhenotypeTimeCourse
    , AvgBChartNodes
    , AvgBChartSwitches
    , RepResults
    , ExpSpreadResults
    , Timeline
    , AnnotatedLayerVec
    , RealExpSpreadResults
    , RealTimeline
    , RealAnnotatedLayerVec
    , WasForced
    , AvgWasForced
    , PhenotypeWeights
    , PulseSpacing
    , runInvestigation
    , runTimeCourse
    , DMExpOutput(..)
    , ExpOutput(..)
    , ExpOP(..)
    , ExperimentHook
    , ExperimentMark
    , TimeCourseOutput
    , TCOutputParameters(..)
    , TCExpOPMeta(..)
    , ScanOutput
    , pickStates
    , envScanInputName
    , VEXScan(..)
    , PlottingNodes
    , ScanSwitch
    , ScanNode
    , ScanKind(..)
    , EnvScan(..)
    , KDOEScan(..)
    , kdoeScNLocks
    , DoOverlayValues
    , XAxis(..)
    , ScanResult(..)
    , DMScan(..)
    , MetaScanKind(..)
    , SCExpKind(..)
    , IntEnvScan
    , IntKDOEScan
    , ScanVariation
    , mkDMScan
    , runScan
    ) where

import Utilities
import Types.DMModel
import Types.Simulation
import Types.Figures
import Types.DMInvestigation.TimeCourse
import Types.DMInvestigation.Scan
import Types.VEXInvestigation
import Data.Validation
import Path
import TextShow
import qualified Data.HashSet as HS
import System.Random (StdGen)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as M
import qualified Data.List as L

-- Defining the types that comprise sampling preferences, input space diagram
-- details, and various virtual experiments to be run on the associated DMModel,
-- as well as functions to validate parsed investigation files. After running
-- we can also write the results to disk for future use. 

type DMInvestigation = [LayerExpSpec]

data LayerExpSpec = LayerExpSpec {
      layerExpMMapping :: ModelMapping
    , layerExpMLayer :: ModelLayer
    , invesIBundle :: Maybe InputBundle -- Should dynmod create a 5D figure?
    , experiments :: [(DMExperiment, VEXExperiment)]
    }

data LayerResult = LayerResult
    { layerResultMM :: ModelMapping
    , layerResultML :: ModelLayer
    , layerResultERs :: [ExperimentResult]
    , layerResultIB :: Maybe InputBundle -- Should dynmod create a 5D figure?
    } deriving (Eq, Show)

data ExperimentResult = TCExpRes (TCExpMeta, [(Barcode, RepResults)])
                      | ScanExpRes (SCExpMeta, [(Barcode, ScanResult)])
                      deriving (Eq, Show)


data ExpMeta = TCEM TCExpMeta
             | SCEM SCExpMeta
             deriving (Eq, Show)

data DMExperiment = TCDMEx DMTimeCourse
                  | ScDMex DMScan

data LayerResultIO = LayerResultIO {
      layerResultMMIO :: ModelMapping
    , layerResultMLIO :: ModelLayer
    , layerExperimentHooksIO :: [ExperimentHook]
    } 


data DMExpOutput = DMExpOutput { layerGateSet :: [NodeGate]
                               , layerNIBM :: LayerNameIndexBimap
                               , layerMM :: ModelMapping
                               , dmExpOutput :: ExpOutput
                               } deriving (Eq, Show)
data ExpOutput = ExpOutput
    { opVexExp :: VEXExperiment
    , expOP :: ExpOP
    , expMark :: ExperimentMark
    } deriving (Eq, Show)


data ExpOP = TCO TimeCourseOutput
           | SCO ScanOutput
           deriving (Eq, Show)


type ExperimentHook = (ExperimentMark, Path Abs File)
-- An Int to mark a particular run of a particular experiment, so that we don't
-- have to validate an experiment that we literally just did. 
type ExperimentMark = Word


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
        leftoverN = showt $ (L.length unpinnedInputs) - 5
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
        -> Validation [VEXInvestigationInvalid] (DMExperiment, VEXExperiment)
mkDMExperiment mM mL vexExp@(VXTC v) =
    (flip (,) vexExp . TCDMEx) <$> (mkTimeCourse mM mL v)
mkDMExperiment mM mL vexExp@(TXSC e) =
    (flip (,) vexExp . ScDMex) <$> (mkDMScan mM mL e)

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
        exps = (fmap fst . experiments) lExpSpec
        phData = (lniBMap, phs)
        phs = concatMap (snd . snd) mMap
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        mL = layerExpMLayer lExpSpec
        mMap = layerExpMMapping lExpSpec

-- Run a DMExperiment by folding up the InputPulses according to the chosen step
-- style. First filter the attractors available. 
-- Note that when running an experiment, it should be run n times for
-- each attractor in the set, where n is the length of the attractor, starting
-- at the next in the loop each time. (Barcode, RepResults or ScanResult)s are
-- then combined if their Barcodes are identical. 
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
                L.mapAccumL (runTimeCourse phData tcExp) expGen filteredAtts
            expMeta = tcExpMeta tcExp
            filteredAtts = tcAttFilter tcExp $ layerBCG <$> attList
            expGen = fromMaybe gen (manualTCPRNGSeed tcExp)
    ScDMex scanExp -> (newGen, ScanExpRes (expMeta, attResults))
        where
            (newGen, attResults) =
                L.mapAccumL (runScan phData scanExp) gen filteredAtts
            filteredAtts = scAttFilter scanExp $ layerBCG <$> attList
            expMeta = scExpMeta scanExp
    where
        attList = HS.toList attSet


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

-- The convention is to denote the limits on an inputs which are composed of
-- multiple boolean DMNodes by referring only to its highest level NodeName. 
-- e.g. refer to the levels of [GF_High, GF] by GF_High 0, 1, or 2
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

