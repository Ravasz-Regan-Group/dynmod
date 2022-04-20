{-# LANGUAGE OverloadedStrings #-}

module Properties.Attractors 
    ( attractors
    , attractors'
    ) where

import Data.Tuple (swap)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import Types.Simulation

-- Return just the attractors of the ModelLayer. This is slower, but uses less
-- memory. We cannot check if we are runing down a chain of states we have
-- encountered before, but we also do not have to carry around a HashMap of all
-- the states we have ever encountered. 
attractors :: ModelEnv -> Simulation (HS.HashSet Attractor)
attractors = topStates attFolders

attFolders :: Folder (HS.HashSet Attractor)
attFolders = Folder attRandFold attNoisyFold attRunDown

attRandFold :: (HS.HashSet Attractor) -> LayerVec -> (HS.HashSet Attractor)
attRandFold attHS _ = attHS

attNoisyFold :: (HS.HashSet Attractor) -> LayerVec -> (HS.HashSet Attractor)
attNoisyFold attHS _ = attHS

attRunDown :: PSStepper
           -> (HS.HashSet Attractor)
           -> LayerVec
           -> (HS.HashSet Attractor)
attRunDown stepper attHS lV = runDown' lV M.empty 0 attHS
    where
        runDown' lVec sMap i rAttHS
            | not $ M.member lVec sMap =
                let nextVec = stepper lVec
                    newMap = M.insert lVec i sMap
                    newI = i + 1
                in runDown' nextVec newMap newI rAttHS
            | otherwise =
                let partitionIndex = sMap M.! lVec
                    updateVec = (B.fromList . (swap <$>) . M.toList) sMap
                    vLength = B.length updateVec
                    emptyVec = B.replicate vLength U.empty
                    orderedVec = B.update emptyVec updateVec
                    att = mkAttractor $ B.drop partitionIndex orderedVec
                in HS.insert att rAttHS

type ASetTracker = (AttractorSet, M.HashMap LayerVec Attractor)

-- Return the AttractorSet of the ModelLayer. This is faster, but uses more
-- memory. We check if we are runing down a chain of states we have
-- encountered before and stop if we have, but we must carry around a HashMap of
-- all the states we have ever encountered in order to do so. 
attractors' :: ModelEnv -> Simulation ASetTracker
attractors' =  topStates attFolders'

attFolders' :: Folder ASetTracker
attFolders' = Folder attRandFold' attNoisyFold' attRunDown'

attRandFold' :: ASetTracker -> LayerVec -> ASetTracker
attRandFold' aSetTr _ = aSetTr

attNoisyFold' :: ASetTracker -> LayerVec -> ASetTracker
attNoisyFold' aSetTr _ = aSetTr

attRunDown' :: PSStepper -> ASetTracker -> LayerVec -> ASetTracker
attRunDown' stepper aSetTr lV = runDown' lV M.empty 0 aSetTr
    where
        runDown' lVec sMap i (aS, vecMap) = case M.lookup lVec vecMap of
            Just att -> (newAS, newVecMap)
                where
                    newAS = M.insertWith HS.union att vecHS aS
                    (vecHS, newVecMap) =
                        M.foldlWithKey' builder (HS.empty, vecMap) sMap
                    builder (vhs, aVecMap) smV _ =
                        (HS.insert smV vhs, M.insert smV att aVecMap)
            Nothing
                | not $ M.member lVec sMap ->
                    let nextVec = stepper lVec
                        newMap = M.insert lVec i sMap
                        newI = i + 1
                    in runDown' nextVec newMap newI (aS, vecMap)
                | otherwise ->
                    let partitionIndex = sMap M.! lVec
                        updateVec = (B.fromList . (swap <$>) . M.toList) sMap
                        vLength = B.length updateVec
                        emptyVec = B.replicate vLength U.empty
                        orderedVec = B.update emptyVec updateVec
                        att = mkAttractor $ B.drop partitionIndex orderedVec
                        newVecMap = B.foldl'
                            (\aVecMap aAV -> M.insert aAV att aVecMap)
                            vecMap
                            orderedVec
                        threadHS = (HS.fromList . B.toList) orderedVec
                        newAS = M.insertWith HS.union att threadHS aS
                    in (newAS, newVecMap)
