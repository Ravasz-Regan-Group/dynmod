{-# LANGUAGE OverloadedStrings #-}

module Types.DMInvestigation.Scan
    ( DMScan(..)
    , SCExpMeta(..)
    , MetaScanKind(..)
    , SCExpKind(..)
    , IntEnvScan
    , IntKDOEScan
    , ScanVariation
    , ScanResult(..)
    , mkDMScan
    , runScan
    ) where

import Utilities
import Types.DMModel
import Types.Simulation
import Types.Figures
import Types.VEXInvestigation
import Types.DMInvestigation.TimeCourse
import Data.Validation
import qualified Data.Text as T
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as M
import qualified Data.Bifunctor as BF
import qualified Data.List as L
import Data.Maybe (mapMaybe, fromJust)
import System.Random


data DMScan = Sc {
      scanKind :: SCExpKind
    , scExpMeta :: SCExpMeta
    , maxRunSteps :: Max_N
    , requiredSteps :: Relevant_N
--    Select the relevant Attractors from those of the whole layer. 
    , scAttFilter :: [(Barcode, Attractor)] -> [(Barcode, Attractor)]
    , scExpStepper :: ExpStepper
    , scanInputFix :: Maybe RealInputCoord -- Start runs at real-valued inputs. 
    , manualSCPRNGSeed :: Maybe StdGen
    }

data SCExpMeta = SCEMeta {
      scExpName :: T.Text
    , scExpDetails :: T.Text
    , scMetaScanKind :: MetaScanKind
    , scExpSwitches :: [ScanSwitch]
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
    | MetaThreeDEnvScan (NodeName, [Double])
                        (NodeName, [Double])
                        (NodeName, [Double])
    deriving (Eq, Show)

data SCExpKind = 
      IntEnvSc IntEnvScan
    | IntKDOESc IntKDOEScan
    | IntEnvKDOEScan IntEnvScan IntKDOEScan XAxis
    | IntTwoDEnvScan IntEnvScan IntEnvScan (Maybe IntKDOEScan)
    | IntThreeDEnvScan IntEnvScan IntEnvScan IntEnvScan [IntNodeAlteration]
    deriving (Eq, Show)

data IntEnvScan = IntESC FixedVec --[[DMNode]] input start state
                         FixedVec -- [[DMNode]] input end state
                         ScanSteps -- Steps to get there
                         deriving (Eq, Show)

data IntKDOEScan = IntKDOESC [IntNodeAlteration]
                             (ScanSteps, [(NodeIndex, NodeState)])
                             deriving (Eq, Show)

data ScanVariation = SCVar { scanVrealIC :: RealInputCoord
                           , scanVRNAlts :: [IntNodeAlteration]
                           } deriving (Eq, Show)

data ScanResult = SKREnv [[Timeline]]
                | SKRKDOE [[Timeline]]
                | SKREnvKDOE [[[Timeline]]]
                | SKRTwoEnvWithoutKDOE [[[Timeline]]]
                | SKRTwoEnvWithKDOE [[[[Timeline]]]]
                | SKRThreeEnv ([[[[Timeline]]]], [[[[Timeline]]]])
                  deriving (Eq, Show)

mkDMScan :: ModelMapping -> ModelLayer -> VEXScan
       -> Validation [VEXInvestigationInvalid] DMScan
mkDMScan mM mL
  (VEXScan scKnd inEnv iFix maxN relN stopPhs exStep scSws mPRMNGSeed) =
    case mkSCExpKind mL scKnd of
    Failure errs -> Failure errs
    Success intScKnd ->
        Sc <$> pure intScKnd
           <*> mkSCExpMeta mM inPts expName expDetails scSws stopPhs scKnd
           <*> pure maxN
           <*> pure relN
           <*> mkAttFilter mM mL inEnv
           <*> pure (mkStepper exStep mL)
           <*> (traverse (mkRealInputCoordinate mL) mIFix)
           <*> pure (mkStdGen <$> mPRMNGSeed)
        where
            inPts = (inputs . modelGraph) mL
            expDetails = mkScDetails scKnd
            expName = mkScName scKnd
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
mkSCExpKind mL (TwoDEnvScan envScan1 envScan2 mKDOESc) =
    IntTwoDEnvScan <$> mkIntEnvScan mL envScan1
                   <*> mkIntEnvScan mL envScan2
                   <*> (traverse (mkIntKDOEScan mL) mKDOESc)
mkSCExpKind mL  (ThreeDEnvScan envScan1 envScan2 envScan3 nAlts) =
    IntThreeDEnvScan <$> mkIntEnvScan mL envScan1
                     <*> mkIntEnvScan mL envScan2
                     <*> mkIntEnvScan mL envScan3
                     <*> mkIntNodeAlterations mL nAlts

-- The convention is to denote the limits on inputs which are composed of
-- multiple boolean DMNodes by referring only to its highest level NodeName. 
-- e.g. refer to the levels of [GF_High, GF] by GF_High 0, 1, or 2
mkIntEnvScan :: ModelLayer -> EnvScan
             -> Validation [VEXInvestigationInvalid] IntEnvScan
mkIntEnvScan mL (RangeESC nName startSt endSt sSteps)
    | notElem nName mlNodeNames = Failure [UnknownNodeInInputScan nName]
    | notElem nName inputNodeNames = Failure [NonInputNodeInInputScan nName]
    | notElem nName topLevelNames = Failure [InputScanNodeNotTopLevel nName]
    | not inbStart = Failure [InValidInputScanStart nName startSt (0, inputTop)]
    | not inbEnd = Failure [InValidInputScanEnd nName endSt (0, inputTop)]
    | otherwise = Success $ IntESC startFixedVec endFixedVec sSteps
    where
        startFixedVec = inputFVecs L.!! startSt
        endFixedVec = inputFVecs L.!! endSt 
        inputFVecs = inputLevels lniBMap Nothing inputStack
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        inbEnd = (endSt >= 0) && (endSt <= inputTop)
        inbStart = (startSt >= 0) && (startSt <= inputTop)
        inputTop = inputDegrees inputStack - 1
        inputStack = (fromJust . L.find ipFF) inPts
        ipFF = (== nName) . nodeName . nodeMeta . head
        topLevelNames = (nodeName . nodeMeta) <$> topLevels
--      Here we take advantage of the fact that inputs calls soleSelfLoops on
--      the LayerGraph as the seeds for finding the layer inputs, so they will
--      always be the head of the input lists. 
        topLevels = head <$> inPts
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        inPts = (inputs . modelGraph) mL
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL
mkIntEnvScan mL (WholeESC nName sSteps)
    | notElem nName mlNodeNames = Failure [UnknownNodeInInputScan nName]
    | notElem nName inputNodeNames = Failure [NonInputNodeInInputScan nName]
    | notElem nName topLevelNames = Failure [InputScanNodeNotTopLevel nName]
    | otherwise = Success $ IntESC startFixedVec endFixedVec sSteps
    where
        startFixedVec = head inputFVecs
        endFixedVec = last inputFVecs
        inputFVecs = inputLevels lniBMap Nothing inputStack
        inputStack = (fromJust . L.find ipFF) inPts
        ipFF = (== nName) . nodeName . nodeMeta . head
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        topLevelNames = (nodeName . nodeMeta) <$> topLevels
        topLevels = head <$> inPts
        inputNodeNames = mconcat $ (nodeName . nodeMeta) <<$>> inPts
        inPts = (inputs . modelGraph) mL
        mlNodeNames = (fmap (nodeName . nodeMeta) . layerNodes) mL

mkIntKDOEScan :: ModelLayer -> KDOEScan
              -> Validation [VEXInvestigationInvalid] IntKDOEScan
mkIntKDOEScan mL (KDOESC nAlts (scSteps, nodeLocks))
    | (not . null) lockRepeats = Failure [KDOEScanLocksRepeat lockRepeats]
    | (not . null) nonPresentLocks =
        Failure [UnknownNodesInKDOEScanLocks nonPresentLocks]
    | (not . null) inputLocks = Failure [InputsInKDOEScanLocks inputLocks]
    | (not . null) oobLocks =
        Failure [InvalidKDOEScanLocks oobLocks properLocks]
    | otherwise = IntKDOESC <$> (mkIntNodeAlterations mL nAlts)
                            <*> pure (scSteps, intNodeLocks)
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

mkSCExpMeta :: ModelMapping -> [[DMNode]] -> T.Text -> T.Text -> [ScanSwitch]
            -> [(ScanSwitch, PhenotypeName)] -> ScanKind
            -> Validation [VEXInvestigationInvalid] SCExpMeta
mkSCExpMeta mM inPts expName expDetails scSws stopPhs scKnd =
    SCEMeta <$> pure expName
            <*> pure expDetails
            <*> (pure . mkMetaScanKind inPts) scKnd
            <*> validateSCSwitches mM scSws
            <*> mkStopPhenotypes mM stopPhs

-- This presumes that mkSCExpKind has passed. Do not use outside of mkSCExpMeta!
mkMetaScanKind :: [[DMNode]] -> ScanKind -> MetaScanKind
mkMetaScanKind inPts sc = case sc of
    (EnvSc envScan) -> uncurry MetaEnvSc (mkMetaEnvSc inPts envScan)
    (KDOESc kdoeScan) -> uncurry MetaKDOESc (mkMetaKDOESc kdoeScan)
    (EnvKDOEScan envScan kdoeScan xAx) ->
        MetaEnvKDOEScan (mkMetaEnvSc inPts envScan) (mkMetaKDOESc kdoeScan) xAx
    (TwoDEnvScan envScan1 envScan2 mKDOEScan) ->
        MetaTwoDEnvScan (mkMetaEnvSc inPts envScan1)
                        (mkMetaEnvSc inPts envScan2)
                        (mkMetaKDOESc <$> mKDOEScan)
    (ThreeDEnvScan envScan1 envScan2 envScan3 _) ->
        MetaThreeDEnvScan (mkMetaEnvSc inPts envScan1)
                          (mkMetaEnvSc inPts envScan2)
                          (mkMetaEnvSc inPts envScan3)

-- This presumes that mkSCExpKind has passed. Do not use outside of
-- mkMetaScanKind!
mkMetaEnvSc :: [[DMNode]] -> EnvScan -> (NodeName, [Double])
mkMetaEnvSc inPts envScan = case envScan of
    (RangeESC nName sSt eSt scStps) -> (nName, mkSteps sSt eSt scStps)
    (WholeESC nName scStps) -> (nName, mkSteps 0 inputTop scStps)
        where
            inputTop = (inputDegrees ((fromJust . L.find ipFF) inPts)) - 1
            ipFF = (== nName) . nodeName . nodeMeta . head

-- This presumes that mkSCExpKind has passed. Do not use outside of
-- mkMetaScanKind!
mkMetaKDOESc :: KDOEScan -> ([(NodeName, NodeState)], [Double])
mkMetaKDOESc (KDOESC _ (scStps, lockNodes)) = (lockNodes, mkSteps 0 1 scStps)

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
mkScName (TwoDEnvScan envScan1 envScan2 mKDOEScan) = case mKDOEScan of
    Just kdoeSc -> "EnvKDOEScan_" <> mkEnvName envScan1 <> "_" <>
        mkEnvName envScan2 <> "_" <> mkKDOEName kdoeSc
    Nothing -> "EnvKDOEScan_" <> mkEnvName envScan1 <> "_" <> mkEnvName envScan2
mkScName (ThreeDEnvScan envScan1 envScan2 envScan3 nAlts) =
    "ThreeDEnvScan_" <> mkEnvName envScan1 <> "_" <> mkEnvName envScan2 <> "_"
        <> mkEnvName envScan3 <> kName
    where
        kName
            | L.null nAlts = ""
            | otherwise = "_" <> kdoeName nAlts

mkEnvName :: EnvScan -> T.Text
mkEnvName envScan = "EnvScan_" <> envScanInputName envScan

mkKDOEName :: KDOEScan -> T.Text
mkKDOEName (KDOESC _ (_, nodeLocks)) = "KDOEScan_" <> nLocksT
    where nLocksT = T.intercalate "," (tShow <$> nodeLocks)

mkScDetails :: ScanKind -> T.Text
mkScDetails (EnvSc soloEnv)  = mkEnvDetails soloEnv
mkScDetails (KDOESc soloKDOE) = mkKDOEDetails soloKDOE
mkScDetails (EnvKDOEScan envScan kdoeScan _) = "EnvKDOEScan_" <>
    mkEnvDetails envScan <> "_" <> mkKDOEDetails kdoeScan
mkScDetails (TwoDEnvScan envScan1 envScan2 mKDOEScan) = case mKDOEScan of
    Just kdoeSc -> "EnvKDOEScan_" <> mkEnvDetails envScan1 <> "_" <>
        mkEnvDetails envScan2 <> "_" <> mkKDOEDetails kdoeSc
    Nothing -> "EnvKDOEScan_" <> mkEnvDetails envScan1 <> "_" <>
                mkEnvDetails envScan2
mkScDetails (ThreeDEnvScan envScan1 envScan2 envScan3 nAlts) =
    "ThreeDEnvScan_" <> mkEnvDetails envScan1 <> "_" <> mkEnvDetails envScan2 <>
        "_" <> mkEnvDetails envScan3 <> kDets
    where
        kDets
            | L.null nAlts = ""
            | otherwise = "_" <> kdoeDetails nAlts

mkEnvDetails :: EnvScan -> T.Text
mkEnvDetails (RangeESC nName startSt endSt scSteps) = "RangeESC_" <> nName <>
    "_" <> tShow startSt <> "_" <> tShow endSt <> "_over_" <> tShow scSteps
mkEnvDetails (WholeESC nName scSteps) = "WholeESC_" <> nName <> "_over_" <>
    tShow scSteps

mkKDOEDetails :: KDOEScan -> T.Text
mkKDOEDetails (KDOESC nAlts (scSteps, nodeLocks)) = "KDOEScan_" <> nLocksT <>
    "_over_" <> tShow scSteps <> kAltDetails
    where
        kAltDetails
            | L.null nAlts = ""
            | otherwise = "_wAlts_" <> kdoeName nAlts
        nLocksT = T.intercalate "," (tShow <$> nodeLocks)

runScan :: (LayerNameIndexBimap, [Phenotype])
        -> DMScan
        -> StdGen
        -> (Barcode, Attractor)
        -> (StdGen, (Barcode, ScanResult))
runScan lInfo scanEx gen (bc, att) = case scanKind scanEx of
  IntEnvSc intEnv -> (newGen, (bc, SKREnv res))
    where            
      (newGen, res) = L.mapAccumL rVarF gen (mkEnvVars intEnv)
  IntKDOESc intKDOE -> (newGen, (bc, SKRKDOE res))
    where
      (newGen, res) = L.mapAccumL rVarF gen (mkKDOEVars intKDOE)
  IntEnvKDOEScan intEnv intKDOE xAx -> (newGen, (bc, SKREnvKDOE res))
    where
      (newGen, res) = (L.mapAccumL . L.mapAccumL) rVarF gen variations
      variations :: [[ScanVariation]]
      variations = case xAx of
        EnvX -> fmap (kdoeSpread intKDOE) (mkEnvVars intEnv)
        KDOEX -> fmap (envSpread intEnv) (mkKDOEVars intKDOE)
  IntTwoDEnvScan intEnv1 intEnv2 Nothing ->
    (newGen, (bc, SKRTwoEnvWithoutKDOE res))
    where
        (newGen, res) = (L.mapAccumL . L.mapAccumL) rVarF gen variations
        variations = fmap (envSpread intEnv2) (mkEnvVars intEnv1)
  IntTwoDEnvScan intEnv1 intEnv2 (Just intKDOE) ->
    (newGen, (bc, SKRTwoEnvWithKDOE res))
    where
        (newGen, res) = (L.mapAccumL . L.mapAccumL . L.mapAccumL) rVarF gen vars
        vars = (fmap . fmap) (kdoeSpread intKDOE) tier2
        tier2 = fmap (envSpread intEnv2) tier1
        tier1 = mkEnvVars intEnv1
  IntThreeDEnvScan intEnv1 intEnv2 intEnv3 intNAlts ->
    (newGen, (bc, SKRThreeEnv (res, resWMutations)))
    where
        (newGen, resWMutations) = (L.mapAccumL . L.mapAccumL . L.mapAccumL)
            rVarF resWMutationsGen varsWMutations
        (resWMutationsGen, res) =
            (L.mapAccumL . L.mapAccumL . L.mapAccumL) rVarF gen vars
        varsWMutations = (fmap . fmap . fmap) (insertNAlts intNAlts) vars
        vars = (fmap . fmap) (envSpread intEnv3) tier2
        tier2 = fmap (envSpread intEnv2) tier1
        tier1 = mkEnvVars intEnv1
  where
    rVarF = runVariation lInfo scanEx att

insertNAlts :: [IntNodeAlteration] -> ScanVariation -> ScanVariation
insertNAlts intNAlts (SCVar rIC scanNAlts) = SCVar rIC (intNAlts <> scanNAlts)

mkEnvVars :: IntEnvScan -> [ScanVariation]
mkEnvVars intEnv = zipWith SCVar (mkRealICS intEnv) (repeat [])

mkRealICS :: IntEnvScan -> [RealInputCoord]
mkRealICS (IntESC startFixedVec endFixedVec nSts) =
    case U.length startFixedVec of
    0 -> []
    1 -> (U.singleton . (,) nI) <$> (mkSteps sSt eSt nSts)
        where
            (_, eSt) = U.head endFixedVec
            (nI, sSt) = U.head startFixedVec
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
            (nIndices, stSts) = U.unzip startFixedVec
            endSts = (snd . U.unzip) endFixedVec
            
mkKDOEVars :: IntKDOEScan -> [ScanVariation]
mkKDOEVars intKDOE = zipWith SCVar (repeat U.empty) (mkIntNAltss intKDOE)

mkIntNAltss :: IntKDOEScan -> [[IntNodeAlteration]]
mkIntNAltss (IntKDOESC intNAlts (scSteps, stNAlts)) =
    zipWith (<>) (L.repeat intNAlts) steppedNAlts
    where
        steppedNAlts :: [[IntNodeAlteration]]
        steppedNAlts = rangeInserter rnge <$> stNAlts
        rangeInserter :: [LockProbability]
                      -> (NodeIndex, NodeState)
                      -> [IntNodeAlteration]
        rangeInserter stps (nI, nSt) = IntNodeLock nI nSt <$> stps
        rnge = mkSteps 0 1 scSteps

mkSteps :: Int -> Int -> Int -> [Double]
mkSteps sSt eSt nSts = (init spreadL) <> [endSt, endSt]
    where
        spreadL = [startSt,listSpacer..endSt]
        listSpacer = startSt + ((endSt - startSt)/(scanStps - 1))
        startSt = fromIntegral sSt
        endSt = fromIntegral eSt
        scanStps = fromIntegral nSts

-- Spread an IntKDOEScan over an existing ScanVariation. 
kdoeSpread :: IntKDOEScan -> ScanVariation -> [ScanVariation]
kdoeSpread intKSOESc (SCVar rIC nAlts) = SCVar rIC <$> newNAlts
    where
        newNAlts = (nAlts <>) <$> scanNAlts
        scanNAlts = mkIntNAltss intKSOESc

-- Spread an IntEnvScan over an existing ScanVariation. 
envSpread :: IntEnvScan -> ScanVariation -> [ScanVariation]
envSpread intEnvSc (SCVar rIC nAlts) = flip SCVar nAlts <$> newRICs
    where
        newRICs = (U.++ rIC) <$> scanRics
        scanRics = mkRealICS intEnvSc

runVariation :: (LayerNameIndexBimap, [Phenotype])
             -> DMScan
             -> Attractor
             -> StdGen
             -> ScanVariation
             -> (StdGen, [Timeline])
runVariation (lniBMap, phs) scanEx att gen (SCVar rIC iNAlts) = (newGen, res)
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
