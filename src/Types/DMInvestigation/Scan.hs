{-# LANGUAGE OverloadedStrings #-}

module Types.DMInvestigation.Scan where

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
    , stopPhenotypes :: [(PhenotypeName, SubSpace)]
    , maxRunSteps :: Max_N
    , requiredSteps :: Relevant_N
--    Select the relevant Attractors from those of the whole layer. 
    , scAttFilter :: [(Barcode, Attractor)] -> [(Barcode, Attractor)]
    , scExpStepper :: ExpStepper
    , scanInputFix :: Maybe RealInputCoord -- Start runs at real-valued inputs. 
    }

data SCExpMeta = SCEMeta {
      scExpName :: T.Text
    , scExpDetails :: T.Text
    , scMetaScanKind :: ScanKind
    , scExpSwitches :: [ScanSwitch]
--     , scCycleErrors :: [CycleErrors]
    } deriving (Eq, Show)

data SCExpKind = 
      IntEnvSc IntEnvScan
    | IntKDOESc IntKDOEScan
    | IntEnvKDOEScan IntEnvScan IntKDOEScan X_Axis
    | IntTwoDEnvScan IntEnvScan IntEnvScan (Maybe IntKDOEScan)
    | IntThreeDEnvScan IntEnvScan IntEnvScan IntEnvScan [IntNodeAlteration]
    deriving (Eq, Show)

data IntEnvScan = IntESC NodeIndex -- Input DMNode to scan over
                         NodeState -- Start State
                         NodeState -- End State
                         ScanSteps -- Steps to get there
                         deriving (Eq, Show)

data IntKDOEScan = IntKDOESC [IntNodeAlteration]
                             (ScanSteps, [(NodeIndex, NodeState)])
                             deriving (Eq, Show)

data ScanVariation = SCVar { scanVrealIC :: RealInputCoord
                           , scanVRNAlts :: [IntNodeAlteration]
                           } deriving (Eq, Show)

data ScanResult = SKREnv ScanBundle
                | SKRKDOE ScanBundle
                | SKREnvKDOE [ScanBundle]
                | SKRTwoEnvWithoutKDOE [ScanBundle]
                | SKRTwoEnvWithKDOE [[ScanBundle]]
                | SKRThreeEnv [[ScanBundle]]
                  deriving (Eq, Show)

type ScanBundle = [[Timeline]]

mkDMScan :: ModelMapping -> ModelLayer -> VEXScan
       -> Validation [VEXInvestigationInvalid] DMScan
mkDMScan mM mL (VEXScan scKnd inEnv mIFix maxN relN stopPhs exStep scSws) =
    case mkSCExpKind mL scKnd of
    Failure errs -> Failure errs
    Success intScKnd ->
        Sc <$> pure intScKnd
           <*> mkSCExpMeta mM expName expDetails scSws scKnd
           <*> mkStopPhenotypes mM stopPhs
           <*> pure maxN
           <*> pure relN
           <*> mkAttFilter mM mL inEnv
           <*> pure (mkStepper exStep mL)
           <*> (traverse (mkRealInputCoordinate mL) mIFix)
        where
            expDetails = mkScDetails scKnd
            expName = mkScName scKnd

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

mkIntEnvScan :: ModelLayer -> EnvScan
             -> Validation [VEXInvestigationInvalid] IntEnvScan
mkIntEnvScan mL (ESC nName startSt endSt sSteps)
    | notElem nName mlNodeNames = Failure [UnknownNodeInInputScan nName]
    | notElem nName inputNodeNames = Failure [NonInputNodeInInputScan nName]
    | (not . null) oobInputs =
        Failure [InValidInputScanInputs oobInputs properInputs]
    | otherwise = Success $ IntESC (lniBMap BM.! nName) startSt endSt sSteps
    where
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        properInputs =
            "\nScan Node(s) must be from listed inputs\n" <> txtInOpts
        oobInputs = filter (flip notElem (mconcat inOpts)) scanInputs
        scanInputs = [(nName, startSt), (nName, endSt)]
        txtInOpts = textInputOptions inPts
        inOpts = inputOptions inPts
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

mkSCExpMeta :: ModelMapping -> T.Text -> T.Text -> [ScanSwitch] -> ScanKind
            -> Validation [VEXInvestigationInvalid] SCExpMeta
mkSCExpMeta mM expName expDetails scSws scKnd =
    SCEMeta <$> pure expName
            <*> pure expDetails
            <*> pure scKnd
            <*> validateSCSwitches mM scSws

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
    pointPhs = L.filter ((> 1) . L.length . fingerprint) swPhs
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
mkEnvName (ESC nName startSt endSt scSteps) = "EnvScan_" <> nName <> "_" <>
    tShow startSt <> "_" <> tShow endSt <> "_" <> tShow scSteps

mkKDOEName :: KDOEScan -> T.Text
mkKDOEName (KDOESC nAlts (scSteps, nodeLocks)) = "KDOEScan_" <> nLocksT <>
    "_over_" <> tShow scSteps <> kAlts
    where
        kAlts
            | L.null nAlts = ""
            | otherwise = "_wAlts_" <> kdoeName nAlts
        nLocksT = T.intercalate "," (tShow <$> nodeLocks)

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
mkEnvDetails (ESC nName startSt endSt scSteps) = "EnvScan_" <> nName <> "_" <>
    tShow startSt <> "_" <> tShow endSt <> "_" <> tShow scSteps

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
      (newGen, res) = L.mapAccumL rVarF gen variations
      variations = zipWith SCVar (mkRealICS intEnv) (repeat [])
  IntKDOESc intKDOE -> (newGen, (bc, SKRKDOE res))
    where
      (newGen, res) = L.mapAccumL rVarF gen variations
      variations = zipWith SCVar (repeat U.empty) (mkIntNAltss intKDOE)
--   IntEnvKDOEScan intEnv intKDOE -> (newGen, (bc, SKREnvKDOE res))
--     where
--       (newGen, res) = L.mapAccumL rVarF gen variations
--       variations :: [[ScanVariation]]
--       variations = case xAx of
--         EnvX ->  kdoeVars
--           where
--             kdoeVars = zipWith SCVar (repeat U.empty) (mkIntNAltss intKDOE)
--         KDOEX -> 
--   IntTwoDEnvScan intEnv intEnv Nothing ->
--     (newGen, (bc, SKRTwoEnvWithoutKDOE res))
--   IntTwoDEnvScan intEnv intEnv (Just intKDOE) ->
--     (newGen, (bc, SKRTwoEnvWithKDOE res))
--   IntThreeDEnvScan intEnv intEnv intEnv nAlts ->
--     (newGen, (bc, SKRThreeEnv res))
  where
    rVarF = runVariation lInfo scanEx att

mkRealICS :: IntEnvScan -> [RealInputCoord]
mkRealICS (IntESC nI sSt eSt nSts) =
    (U.singleton . (,) nI) <$> (mkSteps sSt eSt nSts)


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
mkSteps sSt eSt nSts = [startSt,listSpacer..endSt]
    where
        listSpacer = startSt + ((endSt - startSt)/(scanStps - 1))
        startSt = fromIntegral sSt
        endSt = fromIntegral eSt
        scanStps = fromIntegral nSts

-- Spread an IntKDOEScan over an existing ScanVariation. 
-- kdoeSpread :: IntKDOEScan -> ScanVariation -> [ScanVariation]
-- kdoeSpread (IntKDOESC intNAlts (scSteps, stNAlts)) (SCVar rIC nAlts) =
--     SCVar rIC newNAlts
-- 
-- -- Spread an IntEnvScan over an existing ScanVariation. 
-- envSpread :: IntEnvScan -> ScanVariation -> [ScanVariation]
-- envSpread (IntESC nI sSt eSt nSts) (SCVar rIC nAlts) =
--     flip SCVar nAlts <$> newRICs
--     where
--         newRICs = 
--         newRICPairs = (,) nI) <$> (mkSteps sSt eSt nSts)
--         zipWith (<>) () mkRealICS intEnv

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
          newSeed = (relN + B.length tmln, nG)
          tmln = B.zip pTmln $ phenotypeMatch lniBMap phs lVecs
          lVecs = B.map (fst . U.unzip) pTmln
          pTmln = case scExpStepper scanEx of
            (SD stepper) -> B.unfoldr sdUnfoldF tmlnSeed
              where
                sdUnfoldF (iALV, rNAcc, mNAcc, aGen)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | any (isAtStopPH lniBMap lVec) (stopPhenotypes scanEx) =
                                                                        Nothing
                  | otherwise = Just (iALV, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts aGen nextVec
                    nextVec = stepper lVec
                    lVec = (fst . U.unzip) iALV
            (SN stepper) -> B.unfoldr snUnfoldF tmlnSeed
              where
                snUnfoldF (iALV, rNAcc, mNAcc, aGen)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | any (isAtStopPH lniBMap lVec) (stopPhenotypes scanEx) =
                                                                        Nothing
                  | otherwise = Just (nextAnolVec, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts iGen nVec
                    (nVec, iGen) = (flip stepper aGen) lVec
                    lVec = (fst . U.unzip) iALV
            (AD stepper) -> B.unfoldr adUnfoldF tmlnSeed
              where
                adUnfoldF (iALV, rNAcc, mNAcc, aGen)
                  | (rNAcc >= relN) || (mNAcc >= maxN) = Nothing
                  | any (isAtStopPH lniBMap lVec) (stopPhenotypes scanEx) =
                                                                        Nothing
                  | otherwise = Just (nextAnolVec, nSeed)
                  where
                    nSeed = (nextAnolVec, rNAcc + 1, mNAcc + 1, nGen)
                    (nextAnolVec, nGen) = expStepPrime rIC iNAlts iGen nVec
                    (nVec, iGen) = (flip stepper aGen) lVec
                    lVec = (fst . U.unzip) iALV
          tmlnSeed = (initLVec, relStAcc, 0, tmlnGen)
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

