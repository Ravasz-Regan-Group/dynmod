{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Properties.LayerCharacteristics 
    ( characteristics
    , LayerCharacteristics (..)
    ) where

import Data.Function ((&))
import Data.Tuple (swap)
-- import qualified Data.Sequence as S
import Control.DeepSeq
import GHC.Generics (Generic)
-- import Control.Monad.State.Strict
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
-- import Types.DMModel
import Types.Simulation
-- import Utilities

newtype LayerCharacteristics = LayerCharacteristics {
        unLayerChars :: (AttractorSet, LayerStats)
        } deriving (Eq, Show, Generic, NFData)

type LayerStats = M.HashMap LayerVec StateBin
data StateBin = StateBin {
    sAttractor  :: (Maybe Attractor) -- Which attractor basin is it in, if we 
     -- know yet. Marked by the attractor itself. 
  , noisyVisits :: Int -- How often have we noisily walked to this state?
  , asyncVisits :: Int -- How often have we asynchronously walked to this
                        -- state?
  , aSyncSteps  :: (M.HashMap LayerVec Int)
     -- Which states and how often have we asynchronously walked to from here?
    } deriving (Eq, Show, Generic, NFData)

instance Semigroup LayerCharacteristics where
    (<>) (LayerCharacteristics (attSet1, stats1))
         (LayerCharacteristics (attSet2, stats2)) = LayerCharacteristics
            (attCombine attSet1 attSet2, statCombine stats1 stats2)
                where
                    statCombine = M.unionWithKey statsMerge

instance Monoid LayerCharacteristics where
    mempty = LayerCharacteristics (M.empty, M.empty)

statsMerge :: LayerVec -> StateBin -> StateBin -> StateBin
statsMerge lVec
           (StateBin att1 nVs1 aVs1 aSMap1)
           (StateBin att2 nVs2 aVs2 aSMap2) = StateBin nA nNV nAV nASM
    where
        nA = case (att1, att2) of
            (Just n, Nothing) -> Just n
            (Nothing, Just n) -> Just n
            (Nothing, Nothing) -> Nothing
            (Just n, Just m) -> case n == m of
                True  -> Just n
                False -> error $ show lVec ++ " is in 2 different\
                        \ attractors. "
        nNV = nVs1 + nVs2
        nAV = aVs1 + aVs2
        nASM = M.unionWith (+) aSMap1 aSMap2


attCombine :: AttractorSet -> AttractorSet -> AttractorSet
attCombine = M.unionWith HS.union

-- Return the attractors, visitation statistics, and State Transition Graph
-- information of the ModelLayer
characteristics :: ModelEnv -> Simulation LayerCharacteristics
characteristics = topStates layerCharFolders

layerCharFolders :: Folder LayerCharacteristics
layerCharFolders = Folder lcRandFold lcNoisyFold lcRunDown

lcRandFold :: LayerCharacteristics -> LayerVec -> LayerCharacteristics
lcRandFold lC _ = lC

lcNoisyFold :: LayerCharacteristics -> LayerVec -> LayerCharacteristics
lcNoisyFold lC lV = LayerCharacteristics (aS, newLS)
    where
        newLS = M.alter noisyVisitIncrement lV lS
        (aS, lS) = unLayerChars lC

noisyVisitIncrement :: Maybe StateBin -> Maybe StateBin
noisyVisitIncrement Nothing = Just $ StateBin Nothing 1 0 M.empty
noisyVisitIncrement (Just (StateBin att  nV aV aMap)) =
                     Just (StateBin att (nV + 1) aV aMap)

-- Run down from a LayerVec to its attractor, updating the LayerCharacteristics
-- for all the found states. Importantly, stop if you encounter a state with a
-- know attractor basin. 
lcRunDown :: PSStepper
          -> LayerCharacteristics
          -> LayerVec
          -> LayerCharacteristics
lcRunDown stepper lC lV = runDown' lV M.empty 0 lC
  where
    runDown' lVec sMap i rlC = case basin rlC lVec of
        Just att -> LayerCharacteristics (newAS, newLS)
            where
                newAS = M.insertWith HS.union att vecHS aS
                (vecHS, newLS) = M.foldlWithKey' builder (HS.empty, lS) sMap
                builder (vhs, aLS) smV _ =
                                (HS.insert smV vhs, basinUpdate smV att aLS)
                (aS, lS) = unLayerChars rlC
        Nothing
            | not $ M.member lVec sMap ->
                let nextVec = stepper lVec
                    newMap = M.insert lVec i sMap
                    newI = i + 1
                in runDown' nextVec newMap newI rlC
            | otherwise ->
                let partitionIndex = sMap M.! lVec
                    updateVec = (B.fromList . (swap <$>) . M.toList) sMap
                    vLength = B.length updateVec
                    emptyVec = B.replicate vLength U.empty
                    orderedVec = B.update emptyVec updateVec
                    att = mkAttractor $ B.drop partitionIndex orderedVec
                    (aS, lS) = unLayerChars rlC
                    newLS = B.foldl' (\aLS aAV -> basinUpdate aAV att aLS)
                                lS orderedVec
                    threadHS = (HS.fromList . B.toList) orderedVec
                    newAS = M.insertWith HS.union att threadHS aS
                in LayerCharacteristics (newAS, newLS)

-- The attractor whose basin the LayerVec is in, if that is yet known. 
basin :: LayerCharacteristics -> LayerVec -> Maybe Attractor
basin lC v = ((snd . unLayerChars) lC & M.lookup v) >>= sAttractor

-- Update the basin of a LayerVec in a LayerStats. 
basinUpdate :: LayerVec 
            -> Attractor
            -> LayerStats
            -> LayerStats
basinUpdate lVec att lS = M.alter adjuster lVec lS
    where
        adjuster Nothing = Just $ StateBin (Just att) 0 0 M.empty
        adjuster (Just sB) = Just $ sB {sAttractor = Just att}

-- Take n asynchronous, deterministic steps through the network, collecting
-- statistics as you go. Stop if you hit a point attractor, as it will never
-- leave and we'll just waste compute. 
-- asynchWalk :: Int
--            -> LayerRangeVec
--            -> PAStepper
--            -> PSStepper
--            -> U.Vector (NodeIndex, NodeState)
--            -> LayerCharacteristics
--            -> Simulation LayerCharacteristics
-- asynchWalk n lrVec aStepper sStepper fixed startlC = do
--     lSeq <- state $ (\g -> U.foldl' chainSR (S.empty, g) lrVec)
--     let startVec = U.update (seqToVec lSeq) fixed
--         go lV lC i
--             | i < n = do
--                 newVec <- aStepper lV
--                 let (aS, lS) = unLayerChars lC
--                 case lV == newVec && (sStepper lV == newVec) of
--                     True -> do
--                         let att = B.singleton newVec
--                             newAS = M.alter (insertAsyncAttHS newVec) att aS
--                             attSB = StateBin (Just att) 0 0 M.empty
--                             newLS = M.insertWith (statsMerge lV) lV attSB lS
--                         return $ LayerCharacteristics (newAS, newLS)
--                     False -> do
--                         let newLS = M.alter (aStepIncrement newVec) lV lS
--                             newLC = LayerCharacteristics (aS, newLS)
--                             newI = i + 1
--                         nextGo <- go newVec newLC newI
--                         return nextGo
--             | otherwise = return lC
--     results <- go startVec startlC 0
--     return results 

-- Functions to update LayerCharacteristics parts as we asynchronously walk 
-- insertAsyncAttHS :: LayerVec
--                  -> Maybe (HS.HashSet LayerVec)
--                  -> Maybe (HS.HashSet LayerVec)
-- insertAsyncAttHS v Nothing = Just $ HS.singleton v
-- insertAsyncAttHS v (Just hs) = Just $ HS.insert v hs    
-- 
-- aStepIncrement :: LayerVec -> Maybe StateBin -> Maybe StateBin
-- aStepIncrement v Nothing = Just $ StateBin Nothing 0 1 $ M.singleton v 1
-- aStepIncrement v (Just (StateBin att nV aV aMap)) = Just $
--     StateBin att nV (aV + 1) $ M.insertWith (+) v 1 aMap

--         allStats = evalState pAsynchWalk asyncGen
--         ttMap = M.fromList $ zip [0..] ttList
--         ivMap = M.fromList $ zip [0..] ivList
--         pAStepper = asyncStep (U.length lrVec) ttMap ivMap
--         asN = asynchN mEnv

