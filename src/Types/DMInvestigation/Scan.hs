{-# LANGUAGE OverloadedStrings #-}

module Types.DMInvestigation.Scan
    ( DMScan(..)
    , SCExpMeta(..)
    , MetaScanKind(..)
    , SCExpKind(..)
    , IntEnvScan
    , IntKDOEScan
    , ScanVariation
    , ScanOutput(..)
    , ScanResult(..)
    , ScanPrep(..)
    , ScanStats
    , StopDistribution
    , PhDistribution
    , ScanNodeStats
    , kdoeScNLocks
    , mkDMScan
    , runScanPrep
    , runScanRaw
--     , mkScanPrep
    ) where

import Utilities
import Types.DMModel
import Types.Simulation
import Types.Figures
import Types.VEXInvestigation
import Types.DMInvestigation.TimeCourse
import Data.Validation
import qualified Data.Text as T
import TextShow
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Statistics.Sample (meanVarianceUnb)
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as M
import qualified Control.Parallel.Strategies as P
import qualified Data.Bifunctor as BF
import qualified Data.List as L
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Control.DeepSeq (force)
import System.Random

data DMScan = Sc {
      scanKind :: SCExpKind
    , scExpMeta :: SCExpMeta
    , maxRunSteps :: Max_N
    , requiredSteps :: Relevant_N
    , scanNodeAlts :: [IntNodeAlteration] -- Optional, static node alterations
--    Select the relevant Attractors from those of the whole layer. 
    , scAttFilter :: [(Barcode, Attractor)] -> [(Barcode, Attractor)]
    , scExpStepper :: ExpStepper
    , scanInputFix :: Maybe RealInputCoord -- Start runs at real-valued inputs. 
    }

data SCExpMeta = SCEMeta {
      scExpName :: T.Text
    , scExpDetails :: T.Text
    , scMetaScanKind :: MetaScanKind
    , scExpSwitches :: [ScanSwitch]
    , scExpScanNodes :: [ScanNode]
    , stopPhenotypes :: [(PhenotypeName, SubSpace)]
    } deriving (Eq, Show)

data MetaScanKind =
      MetaEnvSc NodeName [Double]
    | MetaKDOESc [(NodeName, NodeState)] [Double]
    | MetaEnvKDOEScan (NodeName, [Double])
                      ([(NodeName, NodeState)], [Double])
                      XAxis
    | MetaTwoDEnvScan (NodeName, [Double])
                      (NodeName, [Double])
                      (Maybe ([(NodeName, NodeState)], [Double]))
                      DoOverlayValues
    | MetaThreeDEnvScan (NodeName, [Double])
                        (NodeName, [Double])
                        (NodeName, [Double])
                        DoOverlayValues
    deriving (Eq, Show)

data SCExpKind = 
      IntEnvSc IntEnvScan
    | IntKDOESc IntKDOEScan
    | IntEnvKDOEScan IntEnvScan IntKDOEScan XAxis
    | IntTwoDEnvScan IntEnvScan IntEnvScan (Maybe IntKDOEScan)
    | IntThreeDEnvScan IntEnvScan IntEnvScan IntEnvScan [IntNodeAlteration]
    deriving (Eq, Show)

data IntEnvScan = IntESC RealInputCoord --[[DMNode]] input start state
                         RealInputCoord -- [[DMNode]] input end state
                         ScanSteps -- Steps to get there
                | IntStepSpecESC [RealInputCoord]
                deriving (Eq, Show)

type IntKDOEScan = ([LockProbability], [(NodeIndex, NodeState)])

data ScanVariation = SCVar { scanVrealIC :: RealInputCoord
                           , scanVRNAlts :: [IntNodeAlteration]
                           } deriving (Eq, Show)

data ScanResult = SKREnv [[Timeline]]
                | SKRKDOE [[Timeline]]
                | SKREnvKDOE [[[Timeline]]]
                | SKRTwoEnvWithoutKDOE [[[Timeline]]]
                | SKRTwoEnvWithKDOE [[[[Timeline]]]]
                | SKRThreeEnv ([[[[Timeline]]]], Maybe [[[[Timeline]]]])
                  deriving (Eq, Show)


-- Prepped data for making scan figures and writing out the results of a Scan. 
data ScanPrep =
      SPREnv ScanStats
    | SPRKDOE ScanStats
    | SPREnvKDOE [ScanStats]
    | SPRTwoEnvWithoutKDOE [ScanStats]
    | SPRTwoEnvWithKDOE [[ScanStats]]
    | SPRThreeEnv [[ScanStats]] (Maybe [[ScanStats]])
    deriving (Eq, Show)
-- the basic unit of statistics that we collect for Scans. Represents a single
-- dimension of ScanVariations. 
type ScanStats = ([StopDistribution], [PhDistribution], [ScanNodeStats])
-- What fraction of the Timelines in a ScanVariation is each stop Phenotype
-- responsible for stopping, plus the fraction of Timelines in the Scan run
-- which a Max_N or Relevant_N is responsible for stopping.
type StopDistribution = (M.HashMap PhenotypeName Double, Double)
-- What fraction of the Timelines in a ScanVariation is each Phenotype present?
type PhDistribution = M.HashMap PhenotypeName Double
-- Average and StdDev information of a layer's DMNodes over the result of a
-- single ScanVariation. 
type ScanNodeStats = M.HashMap NodeName (RealNodeState, StdDev)
-- The output of a Scan experiment, to be rendered to disk for future use. 
data ScanOutput = ScRe [(Barcode, ScanResult)]
                | ScPr [(Barcode, ScanPrep)]
                deriving (Eq, Show)

mkDMScan :: ModelMapping -> ModelLayer -> VEXScan
         -> Validation [VEXInvestigationInvalid] DMScan
mkDMScan mM mL
  (VEXScan scKnd inEnv mScanName nAlts iFix maxN relN stopPhs exStep pltNds) =
    case mkSCExpKind mL scKnd of
    Failure errs -> Failure errs
    Success intScKnd ->
        Sc <$> pure intScKnd
           <*> mkSCExpMeta mM mL expName expDetails pltNds stopPhs scKnd
           <*> pure maxN
           <*> pure relN
           <*> mkIntNodeAlterations mL nAlts
           <*> mkAttFilter mM mL inEnv
           <*> pure (mkStepper exStep mL)
           <*> (traverse (mkRealInputCoordinate mL) mIFix)
        where
            expDetails = mkScDetails scKnd
            expName = fromMaybe (mkScName scKnd) mScanName
            mIFix = case iFix of
                [] -> Nothing
                iFxs -> Just iFxs

mkSCExpKind :: ModelLayer -> ScanKind
           -> Validation [VEXInvestigationInvalid] SCExpKind
mkSCExpKind mL (EnvSc soloEnv) = IntEnvSc <$> (mkIntEnvScan mL soloEnv)
mkSCExpKind mL (KDOESc soloKDOE) = IntKDOESc <$> (mkIntKDOEScan mL soloKDOE)
mkSCExpKind mL (EnvKDOEScan envScan kdoeScan xAx) =
    IntEnvKDOEScan <$> mkIntEnvScan mL envScan
                   <*> mkIntKDOEScan mL kdoeScan
                   <*> pure xAx
mkSCExpKind mL (TwoDEnvScan envScan1 envScan2 _ mKDOESc) =
        IntTwoDEnvScan <$> mkIntEnvScan mL envScan1
                       <*> mkIntEnvScan mL envScan2
                       <*> (traverse (mkIntKDOEScan mL) mKDOESc)
mkSCExpKind mL  (ThreeDEnvScan envScan1 envScan2 envScan3 _ nAlts) =
    IntThreeDEnvScan <$> mkIntEnvScan mL envScan1
                     <*> mkIntEnvScan mL envScan2
                     <*> mkIntEnvScan mL envScan3
                     <*> mkIntNodeAlterations mL nAlts

-- The convention is to denote the limits on inputs which are composed of
-- multiple boolean DMNodes by referring only to its highest level NodeName. 
-- e.g. refer to the levels of [GF_High, GF] by GF_High 0, 1, or 2
mkIntEnvScan :: ModelLayer -> EnvScan
             -> Validation [VEXInvestigationInvalid] IntEnvScan
mkIntEnvScan mL (StepSpecESC nName stepValues)
    | notElem nName mlNodeNames = Failure [UnknownNodeInInputScan nName]
    | notElem nName inputNodeNames = Failure [NonInputNodeInInputScan nName]
    | notElem nName topLevelNames = Failure [InputScanNodeNotTopLevel nName]
    | (not . L.null) oobSteps =
        Failure [InvalidInputScanValues nName oobSteps (0, inputTop)]
    | otherwise = Success $ IntStepSpecESC preppedRICs
    where
        preppedRICs = mkSingleRealInputCoord lniBMap inputStack <$>
                                                            (L.sort stepValues)
        oobSteps = filter oobF stepValues
        oobF x = not $ (x >= 0) && (x <= fromIntegral inputTop)
        inputTop = inputDegrees inputStack - 1
        inputStack = (fromJust . L.find ipFF) inPts
        ipFF = (== nName) . nodeName . nodeMeta . head
        topLevelNames = (nodeName . nodeMeta) <$> topLevels
--      Here we take advantage of the fact that inputs calls soleSelfLoops on
--      the LayerGraph as the seeds for finding the layer inputs, so they will
--      always be the head of the input lists. 
        topLevels = head <$> inPts
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        inPts = (inputs . modelGraph) mL
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL
mkIntEnvScan mL (RangeESC nName startSt endSt sSteps)
    | notElem nName mlNodeNames = Failure [UnknownNodeInInputScan nName]
    | notElem nName inputNodeNames = Failure [NonInputNodeInInputScan nName]
    | notElem nName topLevelNames = Failure [InputScanNodeNotTopLevel nName]
    | not inbStart = Failure [InValidInputScanStart nName startSt (0, inputTop)]
    | not inbEnd = Failure [InValidInputScanEnd nName endSt (0, inputTop)]
    | otherwise = Success $
        IntESC (mkSingleRealInputCoord lniBMap inputStack startSt)
               (mkSingleRealInputCoord lniBMap inputStack endSt)
               sSteps
    where
        inbEnd = (endSt >= 0) && (endSt <= (fromIntegral inputTop))
        inbStart = (startSt >= 0) && (startSt <= (fromIntegral inputTop))
        inputTop = (inputDegrees inputStack) - 1
        inputStack = (fromJust . L.find ipFF) inPts
        ipFF = (== nName) . nodeName . nodeMeta . head
        topLevelNames = (nodeName . nodeMeta) <$> topLevels
--      Here we take advantage of the fact that inputs calls soleSelfLoops on
--      the LayerGraph as the seeds for finding the layer inputs, so they will
--      always be the head of the input lists. 
        topLevels = head <$> inPts
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        inPts = (inputs . modelGraph) mL
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL
mkIntEnvScan mL (WholeESC nName sSteps)
    | notElem nName mlNodeNames = Failure [UnknownNodeInInputScan nName]
    | notElem nName inputNodeNames = Failure [NonInputNodeInInputScan nName]
    | notElem nName topLevelNames = Failure [InputScanNodeNotTopLevel nName]
    | otherwise = Success $ IntESC startRIC endRIC sSteps
    where
        startRIC = (U.map (fmap fromIntegral) . head) inputFVecs
        endRIC = (U.map (fmap fromIntegral) . last) inputFVecs
        inputFVecs = inputLevels lniBMap Nothing inputStack
        inputStack = (fromJust . L.find ipFF) inPts
        ipFF = (== nName) . nodeName . nodeMeta . head
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        topLevelNames = (nodeName . nodeMeta) <$> topLevels
        topLevels = head <$> inPts
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        inPts = (inputs . modelGraph) mL
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL

-- Consume a LayerNameIndexBimap, an input stack, and a Double that represents
-- that input's level across its entire range, and produce a RealInputCoord. 
-- This presumes that inputLevel is in band. Do not use outside of mkIntEnvScan!
mkSingleRealInputCoord :: LayerNameIndexBimap
               -> [DMNode]
               -> Double
               -> RealInputCoord
mkSingleRealInputCoord lniBMap inputStack inputLevel = case length inputStack of
    0 -> mempty
    1 -> U.singleton (head inputIndices, inputLevel)
    iSize -> U.fromList $ zip inputIndices digitsList
        where
            digitsList
                | iSize == nOnes = replicate nOnes 1
                | otherwise = zeroes <> (residue:ones)
            zeroes = replicate (iSize - (nOnes + 1)) 0
            ones = replicate nOnes 1
            (nOnes, residue) = properFraction inputLevel
    where
        inputIndices = ((lniBMap BM.!) . nodeName . nodeMeta) <$> inputStack

mkIntKDOEScan :: ModelLayer -> KDOEScan
              -> Validation [VEXInvestigationInvalid] IntKDOEScan
mkIntKDOEScan mL kdoeScan
    | (not . null) lockRepeats = Failure [KDOEScanLocksRepeat lockRepeats]
    | (not . null) nonPresentLocks =
        Failure [UnknownNodesInKDOEScanLocks nonPresentLocks]
    | (not . null) inputLocks = Failure [InputsInKDOEScanLocks inputLocks]
    | (not . null) oobLocks =
        Failure [InvalidKDOEScanLocks oobLocks properLocks]
    | otherwise = case kdoeScan of
        (StepSpecKDOESC _ probValues) -> Success (probValues, intNodeLocks)
        (RangeKDOESC _ startProb endProb scSteps) ->
            Success (mkSteps startProb endProb scSteps, intNodeLocks)
        (WholeKDOESC _ scSteps) -> Success (mkSteps 0 1 scSteps, intNodeLocks)
    where
        intNodeLocks = (BF.first (lniBMap BM.!)) <$> nodeLocks
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        properLocks = "\nLocked Node(s) must be integer values from the\
            \ following options\n" <> txtNodeLockOpts oobLocks mlNodeRanges
        oobLocks = filter (flip notElem mlNodeStates) nodeLocks
        mlNodeStates = concatMap rangeSpread mlNodeRanges
        rangeSpread (nN, nR) = zip (L.repeat nN) [0..nR]
        mlNodeRanges = nodeRange <$> mlNodes
        inputLocks = nodeLockNames `L.intersect` mlInputNodeNames
        mlInputNodeNames =
            (fmap (nodeName . nodeMeta) . mconcat . inputs . modelGraph) mL
        nonPresentLocks = nodeLockNames L.\\ mlNodeNames
        mlNodeNames = (nodeName . nodeMeta) <$> mlNodes
        mlNodes = layerNodes mL
        lockRepeats = repeated nodeLockNames
        nodeLockNames = fst <$> nodeLocks
        nodeLocks = kdoeScNLocks kdoeScan


kdoeScNLocks :: KDOEScan -> [(NodeName, NodeState)]
kdoeScNLocks (StepSpecKDOESC nodeLocks _) = nodeLocks
kdoeScNLocks (RangeKDOESC nodeLocks _ _ _) = nodeLocks
kdoeScNLocks (WholeKDOESC nodeLocks _) = nodeLocks

-- This presumes that mkSCExpKind has passed. Do not use outside of mkDMScan!
mkSCExpMeta :: ModelMapping -> ModelLayer -> T.Text -> T.Text
            -> PlottingNodes -> [(ScanSwitch, PhenotypeName)] -> ScanKind
            -> Validation [VEXInvestigationInvalid] SCExpMeta
mkSCExpMeta mM mL expName expDetails (scSws, scNodes) stopPhs scKnd =
    SCEMeta <$> pure expName
            <*> pure expDetails
            <*> (pure . mkMetaScanKind inPts) scKnd
            <*> validateSCSwitches mM scSws
            <*> validateSCNodes mL scNodes
            <*> mkStopPhenotypes mM stopPhs
    where inPts = (inputs . modelGraph) mL

-- This presumes that mkSCExpKind has passed. Do not use outside of mkSCExpMeta!
mkMetaScanKind :: [[DMNode]] -> ScanKind -> MetaScanKind
mkMetaScanKind inPts sc = case sc of
    (EnvSc envScan) -> uncurry MetaEnvSc (mkMetaEnvSc inPts envScan)
    (KDOESc kdoeScan) -> uncurry MetaKDOESc (mkMetaKDOESc kdoeScan)
    (EnvKDOEScan envScan kdoeScan xAx) ->
        MetaEnvKDOEScan (mkMetaEnvSc inPts envScan) (mkMetaKDOESc kdoeScan) xAx
    (TwoDEnvScan envScan1 envScan2 overLayVs mKDOEScan) ->
        MetaTwoDEnvScan (mkMetaEnvSc inPts envScan1)
                        (mkMetaEnvSc inPts envScan2)
                        (mkMetaKDOESc <$> mKDOEScan)
                        overLayVs
    (ThreeDEnvScan envScan1 envScan2 envScan3 overLayVs _) ->
        MetaThreeDEnvScan (mkMetaEnvSc inPts envScan1)
                          (mkMetaEnvSc inPts envScan2)
                          (mkMetaEnvSc inPts envScan3)
                          overLayVs
    where
        mkMetaKDOESc (StepSpecKDOESC lockNodes probValues) =
            (lockNodes, probValues)
        mkMetaKDOESc (RangeKDOESC lockNodes startProb endProb scStps) =
            (lockNodes, mkSteps startProb endProb scStps)
        mkMetaKDOESc (WholeKDOESC lockNodes scStps) =
            (lockNodes, mkSteps 0 1 scStps)

-- This presumes that mkSCExpKind has passed. Do not use outside of
-- mkMetaScanKind!
mkMetaEnvSc :: [[DMNode]] -> EnvScan -> (NodeName, [Double])
mkMetaEnvSc inPts envScan = case envScan of
    (StepSpecESC nName stepValues) -> (nName, stepValues)
    (RangeESC nName sSt eSt scStps) -> (nName, mkSteps sSt eSt scStps)
    (WholeESC nName scStps) -> (nName, mkSteps 0 (fromIntegral inputTop) scStps)
        where
            inputTop = (inputDegrees ((fromJust . L.find ipFF) inPts)) - 1
            ipFF = (== nName) . nodeName . nodeMeta . head

validateSCSwitches :: ModelMapping -> [ScanSwitch]
                   -> Validation [VEXInvestigationInvalid] [ScanSwitch]
validateSCSwitches mM scSws
    | (not . null) scanSwRepeats = Failure $ [ScanSwitchRepeats scanSwRepeats]
    | (not . null) npSwitches = Failure $ [UnknownSwitchesInScan npSwitches]
    | (not . null) nonPhSws = Failure $ [NonPhenotypedSwitchesInScan nonPhSws]
    | otherwise = Success scSws
    where
        nonPhSws = filter (`notElem` mMPhSwitches) scSws
        npSwitches = filter (`notElem` mMSwitches) scSws
        scanSwRepeats = repeated scSws
        mMPhSwitches = (fmap fst . filter (not . L.null . snd . snd)) mM
        mMSwitches = fst <$> mM

validateSCNodes :: ModelLayer -> [ScanNode]
                -> Validation [VEXInvestigationInvalid] [ScanNode]
validateSCNodes mL scanNodes
    | scanNdRepeats /= [] = Failure $ [ScanNodeRepeats scanNdRepeats]
    | npNodes /= [] = Failure $ [UnknownScanNodes npNodes]
    | otherwise = Success scanNodes
    where
        npNodes = filter (`notElem` layerNNames) scanNodes
        scanNdRepeats = repeated scanNodes
        layerNNames = (fmap (nodeName . nodeMeta) . layerNodes) mL


mkStopPhenotypes :: ModelMapping ->  [(ScanSwitch, PhenotypeName)]
        -> Validation [VEXInvestigationInvalid] [(PhenotypeName, SubSpace)]
mkStopPhenotypes mM stopPhs
  | (not . null) stpSRepeats = Failure $ [StopSwitchRepeats stpSRepeats]
  | (not . null) stpPhRepeats = Failure $ [StopPhenotypeRepeats stpPhRepeats]
  | (not . null) npSwitches =
    Failure $ [UnknownSwitchesInStopPhenotype npSwitches]
  | (not . null) npPhs = Failure $ [UnknownPhenotypesInStopPhenotype npPhs]
  | (not . null) misMatchedSwPhs =
    Failure $ [MismatchedStopPhenotypes misMatchedSwPhs]
  | (not . null) loopPhNames = Failure $ [LoopStopPhenotypes loopPhNames]
  | otherwise = Success stopPHSubSps
  where    
    stopPHSubSps = findPh mM <$> stopPhs
      where
        findPh mMap (swN, phN) = (phN, (head . fingerprint) matchPH)
          where
            matchPH = (fromJust . L.find ((== phN) . phenotypeName)) matchPhs
            matchPhs = (snd . snd . fromJust . L.find ((== swN) . fst)) mMap
    loopPhNames = filter (`notElem` pointPhNames) (snd <$> stopPhs)
    pointPhNames = phenotypeName <$> pointPhs
    pointPhs = L.filter ((== 1) . L.length . fingerprint) swPhs
    misMatchedSwPhs = mapMaybe phChecker stopPhs
      where
        phChecker (swN, phN)
          | correctSwitch == swN = Nothing
          | otherwise = Just (phN, swN, "Correct Switch: " <> correctSwitch)
          where
            correctSwitch = swMap M.! phN
            swMap = foldr inverterF M.empty (mMSwsWithPhNames)
            inverterF (swName, phNms) phM = foldr invFF phM phNms
              where
                invFF pN mP = M.insert pN swName mP
    npPhs = filter ((`notElem` swPhNames) . snd) stopPhs
    stpPhRepeats = repeated swPhNames
    swPhNames = phenotypeName <$> swPhs
    swPhs = concatMap snd mMPhSwitches
    npSwitches = filter (`notElem` switchNames) (fst <$> stopPhs)
    switchNames = fst <$> mMPhSwitches
    stpSRepeats = (repeated . fmap fst) stopPhs
    mMSwsWithPhNames = (fmap . fmap . fmap) phenotypeName mMPhSwitches
    mMPhSwitches = ((filter (not . null . snd)) . ((fmap . fmap) snd)) mM

mkScName :: ScanKind -> T.Text
mkScName (EnvSc soloEnv)  = mkEnvName soloEnv
mkScName (KDOESc soloKDOE)= mkKDOEName soloKDOE
mkScName (EnvKDOEScan envScan kdoeScan _) = "EnvKDOEScan_" <>
    mkEnvName envScan <> "_" <> mkKDOEName kdoeScan
mkScName (TwoDEnvScan envScan1 envScan2 _ mKDOEScan) = case mKDOEScan of
    Just kdoeSc -> "EnvKDOEScan_" <> mkEnvName envScan1 <> "_" <>
        mkEnvName envScan2 <> "_" <> mkKDOEName kdoeSc
    Nothing -> "EnvKDOEScan_" <> mkEnvName envScan1 <> "_" <> mkEnvName envScan2
mkScName (ThreeDEnvScan envScan1 envScan2 envScan3 _ nAlts) =
    "ThreeDEnvScan_" <> mkEnvName envScan1 <> "_" <> mkEnvName envScan2 <> "_"
        <> mkEnvName envScan3 <> kName
    where
        kName
            | L.null nAlts = ""
            | otherwise = "_" <> kdoeName nAlts

mkEnvName :: EnvScan -> T.Text
mkEnvName envScan = "EnvScan_" <> envScanInputName envScan

mkKDOEName :: KDOEScan -> T.Text
mkKDOEName kdoeScan = "KDOEScan_" <> nLocksT
    where
        nLocksT = T.intercalate "," (lockShow <$> nodeLocks)
        nodeLocks = kdoeScNLocks kdoeScan
        lockShow (nN, nLSt) = nN <> "_" <> showt nLSt

mkScDetails :: ScanKind -> T.Text
mkScDetails (EnvSc soloEnv)  = mkEnvDetails soloEnv
mkScDetails (KDOESc soloKDOE) = mkKDOEDetails soloKDOE
mkScDetails (EnvKDOEScan envScan kdoeScan _) = "EnvKDOEScan_" <>
    mkEnvDetails envScan <> "_" <> mkKDOEDetails kdoeScan
mkScDetails (TwoDEnvScan envScan1 envScan2 _ mKDOEScan) = case mKDOEScan of
    Just kdoeSc -> "EnvKDOEScan_" <> mkEnvDetails envScan1 <> "_" <>
        mkEnvDetails envScan2 <> "_" <> mkKDOEDetails kdoeSc
    Nothing -> "EnvKDOEScan_" <> mkEnvDetails envScan1 <> "_" <>
                mkEnvDetails envScan2
mkScDetails (ThreeDEnvScan envScan1 envScan2 envScan3 _ nAlts) =
    "ThreeDEnvScan_" <> mkEnvDetails envScan1 <> "_" <> mkEnvDetails envScan2 <>
        "_" <> mkEnvDetails envScan3 <> kDets
    where
        kDets
            | L.null nAlts = ""
            | otherwise = "_" <> kdoeDetails nAlts

mkEnvDetails :: EnvScan -> T.Text
mkEnvDetails (StepSpecESC nName stepValues) =
    "StepSpecESC" <> nName <> "_" <> showt stepValues
mkEnvDetails (RangeESC nName startSt endSt scSteps) = "RangeESC_" <> nName <>
    "_" <> showt startSt <> "_" <> showt endSt <> "_over_" <> showt scSteps
mkEnvDetails (WholeESC nName scSteps) = "WholeESC_" <> nName <> "_over_" <>
    showt scSteps

mkKDOEDetails :: KDOEScan -> T.Text
mkKDOEDetails kdoeScan = "KDOEScan_" <> nLocksT <> "_over_" <> showt rnge
    where
        rnge = case kdoeScan of
            (StepSpecKDOESC _ probValues) -> probValues
            (RangeKDOESC _ startProb endProb scSteps) ->
                mkSteps startProb endProb scSteps
            (WholeKDOESC _ scSteps) -> mkSteps 0 1 scSteps
        nLocksT = T.intercalate "," (lockShow <$> nodeLocks)
        nodeLocks = kdoeScNLocks kdoeScan
        lockShow (nN, nLSt) = nN <> "_" <> showt nLSt

runScanPrep :: (LayerNameIndexBimap, ModelMapping)
            -> DMScan
            -> StdGen
            -> (Barcode, Attractor)
            -> (StdGen, (Barcode, ScanPrep))
runScanPrep lInfo scanEx gen (bc, att) = case scanKind scanEx of
  IntEnvSc intEnv -> (newGen, (bc, SPREnv (mconcat res)))
    where            
      res = paraDSMap1 (uncurry rVarF) seedVPs
      (newGen, seedVPs) = L.mapAccumL genPair gen envVars
      envVars = mkEnvVars intEnv
  IntKDOESc intKDOE -> (newGen, (bc, SPRKDOE (mconcat res)))
    where
      res = paraDSMap1 (uncurry rVarF) seedVPs
      (newGen, seedVPs) = L.mapAccumL genPair gen kdoeVars
      kdoeVars = mkKDOEVars intKDOE
  IntEnvKDOEScan intEnv intKDOE xAx -> (newGen, (bc, SPREnvKDOE concatRuns))
    where
      concatRuns = mconcat <$> res
      res = paraDSMap2 (uncurry rVarF) seedVPs
      (newGen, seedVPs) = (L.mapAccumL . L.mapAccumL)
            genPair gen variations
      variations = case xAx of
        EnvX -> fmap (kdoeSpread intKDOE) (mkEnvVars intEnv)
        KDOEX -> fmap (envSpread intEnv) (mkKDOEVars intKDOE)
  IntTwoDEnvScan intEnv1 intEnv2 Nothing ->
    (newGen, (bc, SPRTwoEnvWithoutKDOE (mconcat <$> res)))
    where
        res = paraDSMap2 (uncurry rVarF) seedVPs
        (newGen, seedVPs) = (L.mapAccumL . L.mapAccumL)
            genPair gen variations
        variations = fmap (envSpread intEnv1) (mkEnvVars intEnv2)
  IntTwoDEnvScan intEnv1 intEnv2 (Just intKDOE) ->
    (newGen, (bc, SPRTwoEnvWithKDOE (mconcat <<$>> res)))
    where
        res = paraDSMap3 (uncurry rVarF) seedVPs
        (newGen, seedVPs) = (L.mapAccumL . L.mapAccumL . L.mapAccumL)
            genPair gen vars
        vars = (fmap . fmap) (kdoeSpread intKDOE) tier2
        tier2 = fmap (envSpread intEnv1) tier1
        tier1 = mkEnvVars intEnv2
  IntThreeDEnvScan intEnv1 intEnv2 intEnv3 intNAlts ->
    (newGen, (bc, SPRThreeEnv (force concatRes) (force concatResWMutations)))
    where
        concatResWMutations = (fmap . fmap . fmap) mconcat resWMutations
        concatRes = mconcat <<$>> res
        resWMutations
            | L.null intNAlts = Nothing
            | otherwise = Just $ paraDSMap3 (uncurry rVarF) mSeedVPs
        (newGen, mSeedVPs) = (L.mapAccumL . L.mapAccumL . L.mapAccumL)
            genPair tGen varsWMutations
        res = paraDSMap3 (uncurry rVarF) seedVPs        
        seedVPs :: [[[(StdGen, ScanVariation)]]]
        (tGen, seedVPs) = (L.mapAccumL . L.mapAccumL . L.mapAccumL)
            genPair gen vars
        varsWMutations = (fmap . fmap . fmap) (insertNAlts intNAlts) vars
        vars = (fmap . fmap) (envSpread intEnv1) tier2
        tier2 = fmap (envSpread intEnv2) tier1
        tier1 = mkEnvVars intEnv3
  where
    rVarF = force . runVariationPrep lInfo scanEx att
    paraDSMap3 = P.parMap P.rdeepseq . P.parMap P.rdeepseq . P.parMap P.rdeepseq
    paraDSMap2 = P.parMap P.rdeepseq . P.parMap P.rdeepseq
    paraDSMap1 = P.parMap P.rdeepseq

insertNAlts :: [IntNodeAlteration] -> ScanVariation -> ScanVariation
insertNAlts intNAlts (SCVar rIC scanNAlts) = SCVar rIC (intNAlts <> scanNAlts)

mkEnvVars :: IntEnvScan -> [ScanVariation]
mkEnvVars intEnv = zipWith SCVar (mkRealICS intEnv) (repeat [])

mkRealICS :: IntEnvScan -> [RealInputCoord]
mkRealICS (IntStepSpecESC rICs) = rICs
mkRealICS (IntESC startRIC endRIC nSts) = case U.length startRIC of
    0 -> []
    1 -> (U.singleton . (,) nI) <$> (mkSteps sSt eSt nSts)
        where
            (_, eSt) = U.head endRIC
            (nI, sSt) = U.head startRIC
    iSize -> frILevel nIndices <$> fractionalLevels
        where
            frILevel nIds x = U.zip nIds $ U.fromList digitsList
                where
                    digitsList
                        | iSize == nOnes = replicate nOnes 1
                        | otherwise = zeroes <> (residue:ones)
                    zeroes = replicate (iSize - (nOnes + 1)) 0
                    ones = replicate nOnes 1
                    (nOnes, residue) = properFraction x
            fractionalLevels = mkSteps sSt eSt nSts
            sSt = U.sum stSts
            eSt = U.sum endSts
            (nIndices, stSts) = U.unzip startRIC
            endSts = (snd . U.unzip) endRIC
            
mkKDOEVars :: IntKDOEScan -> [ScanVariation]
mkKDOEVars (lockProbs, stNalts) = zipWith SCVar (repeat U.empty) $ 
    rangeInserter stNalts <$> lockProbs

rangeInserter :: [(NodeIndex, NodeState)]
              -> LockProbability
              -> [IntNodeAlteration]
rangeInserter stNalts lockProb = sequenceA primedFs lockProb
    where primedFs = (uncurry IntNodeLock) <$> stNalts
        

mkSteps :: Double -> Double -> Int -> [Double]
mkSteps startSt endSt nSts
    | (startSt /= endSt) && (nSts > 1) = (init spreadL) <> [endSt]
    | otherwise = [startSt]
    where
        spreadL = [startSt,listSpacer..endSt]
        listSpacer = startSt + ((endSt - startSt)/(scanStps - 1))
        scanStps = fromIntegral nSts

-- Spread an IntKDOEScan over an existing ScanVariation. 
kdoeSpread :: IntKDOEScan -> ScanVariation -> [ScanVariation]
kdoeSpread (lockProbs, stNalts) (SCVar rIC nAlts) = SCVar rIC <$> newNAlts
    where
        newNAlts = (nAlts <>) <$> scanNAlts
        scanNAlts = rangeInserter stNalts <$> lockProbs

-- Spread an IntEnvScan over an existing ScanVariation. 
envSpread :: IntEnvScan -> ScanVariation -> [ScanVariation]
envSpread intEnvSc (SCVar rIC nAlts) = flip SCVar nAlts <$> newRICs
    where
        newRICs = (U.++ rIC) <$> scanRics
        scanRics = mkRealICS intEnvSc

runVariationPrep ::
    (LayerNameIndexBimap, ModelMapping)
    -> DMScan
    -> Attractor
    -> StdGen
    -> ScanVariation
    -> ([StopDistribution], [PhDistribution], [ScanNodeStats])
runVariationPrep (lniBMap, mM) scanEx att gen (SCVar rIC actNAlts) =
     (stopDMap, phDistMap, nodeStatMap)
  where
    nodeStatMap = pure $ force $ scanNodeStats lniBMap trScanRun
    phDistMap = pure $ force $ phDistribution allPhNames trScanRun
    stopDMap = pure $ force $ stopDistribution stopPhNames trScanRun
    trScanRun = trimPhStoppedTmln stopPhNames <$> scanRun
    stopPhNames = (fmap fst . stopPhenotypes . scExpMeta) scanEx
    allPhNames :: [PhenotypeName]
    allPhNames = concatMap (fmap phenotypeName . snd . snd) nonEPhs
    nonEPhs = nonEmptyPhenotypes mM
    scanRun = L.unfoldr scanResUnfoldF unFSeed
    unFSeed = (0, gen)
    mRIC = scanInputFix scanEx
    relN = requiredSteps scanEx
    maxN = maxRunSteps scanEx
    phs = concatMap (snd . snd) mM
    scanResUnfoldF (relStAcc, g)
      | relStAcc >= relN = Nothing
      | otherwise = Just (tmln, newSeed)
        where
          newSeed = (relStAcc + B.length tmln, nG)
          tmln = B.zip pTmln $ phenotypeMatch lniBMap phs lVecs
          lVecs = B.map (fst . U.unzip) pTmln
--        We keep the last of a Timeline stopped by a stop Phenotype in order
--        to be able to do stats on it later. 
          pTmln = case scExpStepper scanEx of
            (SD stepper) -> B.unfoldr sdUnfoldF tmlnSeed
              where
                sdUnfoldF (iALV, rNAcc, mNAcc, aGen, isStopPh)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | isStopPh = Nothing
                  | otherwise = Just (iALV, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen, nStP)
                    nStP = any (isAtStopPH lniBMap lVec)
                            ((stopPhenotypes . scExpMeta) scanEx)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts aGen nextVec
                    nextVec = stepper lVec
                    lVec = (fst . U.unzip) iALV
            (SN stepper) -> B.unfoldr snUnfoldF tmlnSeed
              where
                snUnfoldF (iALV, rNAcc, mNAcc, aGen, isStopPh)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | isStopPh = Nothing
                  | otherwise = Just (iALV, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen, nStP)
                    nStP = any (isAtStopPH lniBMap lVec)
                            ((stopPhenotypes . scExpMeta) scanEx)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts iGen nVec
                    (nVec, iGen) = (flip stepper aGen) lVec
                    lVec = (fst . U.unzip) iALV
            (AD stepper) -> B.unfoldr adUnfoldF tmlnSeed
              where
                adUnfoldF (iALV, rNAcc, mNAcc, aGen, isStopPh)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | isStopPh = Nothing
                  | otherwise = Just (iALV, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen, nStP)
                    nStP = any (isAtStopPH lniBMap lVec)
                            ((stopPhenotypes . scExpMeta) scanEx)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts iGen nVec
                    (nVec, iGen) = (flip stepper aGen) lVec
                    lVec = (fst . U.unzip) iALV
          iNAlts = (scanNodeAlts scanEx) <> actNAlts
          tmlnSeed = (initLVec, relStAcc, 0, tmlnGen, False)
          (initLVec, nG) = mkInitAnnoLayerVec mRIC att initLVGen
          (tmlnGen, initLVGen) = split g

isAtStopPH :: LayerNameIndexBimap
           -> LayerVec
           -> (PhenotypeName, SubSpace)
           -> Bool
isAtStopPH lniBMap lVec (_, subSp) = isSSMatch lVec intSubSp
    where intSubSp = (BF.first (lniBMap BM.!)) <$> subSp

-- Randomly pick one of the states of an Attractor and fix its inputs with the
-- optional RealInputCoord for the start of Scan runs. 
mkInitAnnoLayerVec :: (Maybe RealInputCoord)
                   -> Attractor
                   -> StdGen
                   -> (AnnotatedLayerVec, StdGen)
mkInitAnnoLayerVec mRIC att gen
    | attL == 1 = case mRIC of
        Nothing -> ((U.map (\x -> (x, False)) . B.head) att, gen)
        Just rIC -> (expStepPrime rIC [] gen . B.head) att
    | otherwise = case mRIC of
        Nothing -> (U.map (\x -> (x, False)) rAtt, nGen)
            where
                (rAtt, nGen) = BF.first (att B.!) (randomR (0, attL - 1) gen)
        Just rIC -> expStepPrime rIC [] nGen rAtt
            where
                (rAtt, nGen) = BF.first (att B.!) (randomR (0, attL - 1) gen)
    where
    attL = B.length att

-----------------------------------------------------------------------------
-- Functions for prepping the results of Scans for figure- and output-making.

-- What fraction of all the Timelines in a Scan run is each stop Phenotype
-- responsible for stopping, plus the fraction of Timelines in the Scan run
-- which a Max_N or Relevant_N is responsible for stopping.
stopDistribution :: [PhenotypeName]
                 -> [Timeline]
                 -> StopDistribution
stopDistribution [] scanRun =
    (mempty, sum $ (fromIntegral . B.length) <$> scanRun)
stopDistribution stopPhNames scanRun = (stPhMap, fracMaxStop)
    where
        fracMaxStop = force (fromIntegral rMaxStop / fracDiv)
        stPhMap = force (M.map (\x -> fromIntegral x / fracDiv) rMap)
        fracDiv = (fromIntegral divisor) :: Double
        (rMap, rMaxStop, divisor) =
            L.foldl' foldF (mapAccum, 0, 0) stopDurations
        mapAccum = M.fromList $ zip stopPhNames $ repeat 0
        foldF (durMap, maxOutAccum, totalAccum) (dur, phNs) = case phNs of
            [] -> (durMap, maxOutAccum + dur, totalAccum + dur)
            _ -> (L.foldl' foldF' durMap phNs, maxOutAccum, totalAccum + dur)
                where foldF' dMap phN = M.adjust (+ dur) phN dMap
        stopDurations :: [(Int, [PhenotypeName])]
        stopDurations = zipWith (,) runDurations stoppedAtPhs
        runDurations = B.length <$> trScanRun
        trScanRun = trimPhStoppedTmln stopPhNames <$> scanRun
        stoppedAtPhs = getStopPhs stopPhNames <$> scanRun

-- Which, if any, stop Phenotypes are present at the end of a Timeline? 
getStopPhs :: [PhenotypeName] -> Timeline -> [PhenotypeName]
getStopPhs stopPHNs = filter (flip elem stopPHNs) . snd . B.last

-- If a Timeline was stopped at a stop Phenotype, we don't want its last step. 
trimPhStoppedTmln :: [PhenotypeName] -> Timeline -> Timeline
trimPhStoppedTmln stopPHNs tmln = case getStopPhs stopPHNs tmln of
    [] -> tmln
    _  -> B.init tmln

-- What fraction of all the time steps in a [Timeline] is each Phenotype
-- present? 
phDistribution :: [PhenotypeName]
               -> [Timeline]
               -> PhDistribution
phDistribution switchPhNs scanRun = force $ M.map (/ divisor) rMap
    where
        rMap = L.foldl' foldF mapAccum allSteps
        mapAccum :: M.HashMap PhenotypeName Double
        mapAccum = M.fromList $ zip switchPhNs $ repeat 0
        foldF phMap (_, presentPhNs) = L.foldl' foldF' phMap presentPhNs
            where foldF' phM phN = M.adjust (+1) phN phM
        divisor = ((fromIntegral . B.length) allSteps) :: Double
        allSteps = B.concat scanRun

scanNodeStats :: LayerNameIndexBimap -> [Timeline] -> ScanNodeStats
scanNodeStats lniBMap trSCRun = M.fromList statPairs
    where
        statPairs = zip ((lniBMap BM.!>) <$> [0..]) statList
        statList = (force . meanVarianceUnb . U.fromList) <$> bareStateLs
        bareStateLs :: [[RealNodeState]]
        bareStateLs = ((fmap . fmap) fromIntegral . L.transpose . B.toList .
            B.map U.toList) bareStateVs
        bareStateVs :: B.Vector (U.Vector NodeState)
        bareStateVs = (B.map (fst . U.unzip . fst) . B.concat) trSCRun

-- Run a Scan, but do not prep the data for figures
runScanRaw :: (LayerNameIndexBimap, [Phenotype])
           -> DMScan
           -> StdGen
           -> (Barcode, Attractor)
           -> (StdGen, (Barcode, ScanResult))
runScanRaw lInfo scanEx gen (bc, att) = case scanKind scanEx of
  IntEnvSc intEnv -> (newGen, (bc, SKREnv res))
    where            
      (newGen, res) = L.mapAccumL rVarF gen envVars
      envVars = mkEnvVars intEnv
  IntKDOESc intKDOE -> (newGen, (bc, SKRKDOE res))
    where
      (newGen, res) = L.mapAccumL rVarF gen kdoeVars
      kdoeVars = mkKDOEVars intKDOE
  IntEnvKDOEScan intEnv intKDOE xAx -> (newGen, (bc, SKREnvKDOE res))
    where
      (newGen, res) = (L.mapAccumL . L.mapAccumL) rVarF gen variations
      variations = case xAx of
        EnvX -> fmap (kdoeSpread intKDOE) (mkEnvVars intEnv)
        KDOEX -> fmap (envSpread intEnv) (mkKDOEVars intKDOE)
  IntTwoDEnvScan intEnv1 intEnv2 Nothing ->
    (newGen, (bc, SKRTwoEnvWithoutKDOE res))
    where
        (newGen, res) = (L.mapAccumL . L.mapAccumL) rVarF gen variations
        variations = fmap (envSpread intEnv1) (mkEnvVars intEnv2)
  IntTwoDEnvScan intEnv1 intEnv2 (Just intKDOE) ->
    (newGen, (bc, SKRTwoEnvWithKDOE res))
    where
        (newGen, res) =
            (L.mapAccumL . L.mapAccumL . L.mapAccumL) rVarF gen vars
        vars = (fmap . fmap) (kdoeSpread intKDOE) tier2
        tier2 = fmap (envSpread intEnv1) tier1
        tier1 = mkEnvVars intEnv2
  IntThreeDEnvScan intEnv1 intEnv2 intEnv3 intNAlts ->
    (newGen, (bc, SKRThreeEnv (res, resWMutations)))
    where
        (newGen, resWMutations)
            | L.null intNAlts = (resWMutationsGen, Nothing)
            | otherwise = fmap Just $
                (L.mapAccumL . L.mapAccumL . L.mapAccumL)
                  rVarF resWMutationsGen varsWMutations
--         res = (fmap . fmap . fmap) (uncurry rVarF) seedVPs
        (resWMutationsGen, res) =
            (L.mapAccumL . L.mapAccumL . L.mapAccumL) rVarF gen vars
--         seedVPs :: [[[(StdGen, ScanVariation)]]]
--         (tGen, seedVPs) = (L.mapAccumL . L.mapAccumL . L.mapAccumL)
--             genPair gen vars
        varsWMutations = (fmap . fmap . fmap) (insertNAlts intNAlts) vars
        vars = (fmap . fmap) (envSpread intEnv1) tier2
        tier2 = fmap (envSpread intEnv2) tier1
        tier1 = mkEnvVars intEnv3
  where
    rVarF = runVariationRaw lInfo scanEx att

-- Run an ScanVariation, but do not prep for figures. 
runVariationRaw :: (LayerNameIndexBimap, [Phenotype])
                -> DMScan
                -> Attractor
                -> StdGen
                -> ScanVariation
                -> (StdGen, [Timeline])
runVariationRaw (lniBMap, phs) scanEx att gen (SCVar rIC actNAlts) =
    (newGen, res)
  where
    res = L.unfoldr scanResUnfoldF unFSeed
    unFSeed = (0, expGen)
    (expGen, newGen) = split gen
    mRIC = scanInputFix scanEx
    relN = requiredSteps scanEx
    maxN = maxRunSteps scanEx
    scanResUnfoldF (relStAcc, g)
      | relStAcc >= relN = Nothing
      | otherwise = Just (tmln, newSeed)
        where
          newSeed = (relStAcc + B.length tmln, nG)
          tmln = B.zip pTmln $ phenotypeMatch lniBMap phs lVecs
          lVecs = B.map (fst . U.unzip) pTmln
--        We keep the last of a Timeline stopped by a stop Phenotype in order
--        to be able to do stats on it later. 
          pTmln = case scExpStepper scanEx of
            (SD stepper) -> B.unfoldr sdUnfoldF tmlnSeed
              where
                sdUnfoldF (iALV, rNAcc, mNAcc, aGen, isStopPh)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | isStopPh = Nothing
                  | otherwise = Just (iALV, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen, nStP)
                    nStP = any (isAtStopPH lniBMap lVec)
                            ((stopPhenotypes . scExpMeta) scanEx)
                    (nextAnolVec, nGen) =
                      expStepPrime rIC iNAlts aGen nextVec
                    nextVec = stepper lVec
                    lVec = (fst . U.unzip) iALV
            (SN stepper) -> B.unfoldr snUnfoldF tmlnSeed
              where
                snUnfoldF (iALV, rNAcc, mNAcc, aGen, isStopPh)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | isStopPh = Nothing
                  | otherwise = Just (iALV, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen, nStP)
                    nStP = any (isAtStopPH lniBMap lVec)
                            ((stopPhenotypes . scExpMeta) scanEx)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts iGen nVec
                    (nVec, iGen) = (flip stepper aGen) lVec
                    lVec = (fst . U.unzip) iALV
            (AD stepper) -> B.unfoldr adUnfoldF tmlnSeed
              where
                adUnfoldF (iALV, rNAcc, mNAcc, aGen, isStopPh)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | isStopPh = Nothing
                  | otherwise = Just (iALV, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen, nStP)
                    nStP = any (isAtStopPH lniBMap lVec)
                            ((stopPhenotypes . scExpMeta) scanEx)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts iGen nVec
                    (nVec, iGen) = (flip stepper aGen) lVec
                    lVec = (fst . U.unzip) iALV
          iNAlts = (scanNodeAlts scanEx) <> actNAlts
          tmlnSeed = (initLVec, relStAcc, 0, tmlnGen, False)
          (initLVec, nG) = mkInitAnnoLayerVec mRIC att initLVGen
          (tmlnGen, initLVGen) = split g


-- mkScanPrep :: ModelMapping
--            -> ModelLayer
--            -> SCExpMeta
--            -> ScanResult
--            -> ScanPrep
-- mkScanPrep mM mL exMeta scRes = case scRes of
--     SKREnv scanRuns -> SPREnv (stopDs, phDists, nodeStats)
--         where
--             nodeStats = scanNodeStats lniBMap <$> trScanRuns
--             phDists = phDistribution allPhNames <$> scanRuns
--             stopDs = stopDistribution stopPhNames <$> trScanRuns
--             trScanRuns = trimPhStoppedTmln stopPhNames <<$>> scanRuns
--     SKRKDOE scanRuns -> SPRKDOE (stopDs, phDists, nodeStats)
--         where
--             nodeStats = scanNodeStats lniBMap <$> trScanRuns
--             phDists = phDistribution allPhNames <$> scanRuns
--             stopDs = stopDistribution stopPhNames <$> trScanRuns
--             trScanRuns = trimPhStoppedTmln stopPhNames <<$>> scanRuns
--     SKREnvKDOE scanRunss -> SPREnvKDOE $ zip3 stopDss phDistss nodeStatss
--         where
--             nodeStatss = scanNodeStats lniBMap <<$>> trScanRunss
--             phDistss = phDistribution allPhNames <<$>> scanRunss
--             stopDss = stopDistribution stopPhNames <<$>> trScanRunss
--             trScanRunss = (fmap . fmap . fmap)
--                 (trimPhStoppedTmln stopPhNames) scanRunss
--     SKRTwoEnvWithoutKDOE scanRunss ->
--         SPRTwoEnvWithoutKDOE $ zip3 stopDss phDistss nodeStatss
--         where
--             nodeStatss = scanNodeStats lniBMap <<$>> trScanRunss
--             phDistss = phDistribution allPhNames <<$>> scanRunss
--             stopDss = stopDistribution stopPhNames <<$>> trScanRunss
--             trScanRunss = (fmap . fmap . fmap)
--                 (trimPhStoppedTmln stopPhNames) scanRunss
--     SKRTwoEnvWithKDOE scanRunsss -> SPRTwoEnvWithKDOE $
--         ((zipWith3 . zipWith3) (,,)) stopDsss phDistsss nodeStatsss
--         where
--             nodeStatsss = (fmap . fmap . fmap)
--                 (scanNodeStats lniBMap) trScanRunsss
--             phDistsss = (fmap . fmap . fmap)
--                 (phDistribution allPhNames) scanRunsss
--             stopDsss = (fmap . fmap . fmap)
--                 (stopDistribution stopPhNames) trScanRunsss
--             trScanRunsss = (fmap . fmap . fmap . fmap)
--                 (trimPhStoppedTmln stopPhNames) scanRunsss
--     SKRThreeEnv (scanRunsss, mScanRunsss) -> SPRThreeEnv wTriple mTriple
--         where
--             mTriple = case mScanRunsss of
--                 Nothing -> Nothing
--                 Just mutantScanRunsss -> Just $ ((zipWith3 . zipWith3) (,,))
--                     mStopDsss mPhDistsss mNodeStatsss
--                     where
--                         mNodeStatsss = (fmap . fmap . fmap)
--                             (scanNodeStats lniBMap) mTrScanRunsss
--                         mPhDistsss = (fmap . fmap . fmap)
--                             (phDistribution allPhNames) mutantScanRunsss
--                         mStopDsss = (fmap . fmap . fmap)
--                             (stopDistribution stopPhNames) mTrScanRunsss
--                         mTrScanRunsss = (fmap . fmap . fmap . fmap)
--                             (trimPhStoppedTmln stopPhNames) mutantScanRunsss
--             wTriple =
--                 ((zipWith3 . zipWith3) (,,)) stopDsss phDistsss nodeStatsss
--             nodeStatsss = (fmap . fmap . fmap)
--                 (scanNodeStats lniBMap) trScanRunsss
--             phDistsss = (fmap . fmap . fmap)
--                 (phDistribution allPhNames) scanRunsss
--             stopDsss = (fmap . fmap . fmap)
--                 (stopDistribution stopPhNames) trScanRunsss
--             trScanRunsss = (fmap . fmap . fmap . fmap)
--                 (trimPhStoppedTmln stopPhNames) scanRunsss
--     where
--         stopPhNames = (fmap fst . stopPhenotypes) exMeta
--         allPhNames :: [PhenotypeName]
--         allPhNames = concatMap (fmap phenotypeName . snd . snd) nonEPhs
--         nonEPhs = nonEmptyPhenotypes mM
--         LayerSpecs lniBMap _ _ _ = layerPrep mL

