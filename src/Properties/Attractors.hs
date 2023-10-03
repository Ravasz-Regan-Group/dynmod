{-# LANGUAGE OverloadedStrings #-}

module Properties.Attractors 
    ( attractors
    , attractors'
    , attractorCheck
    ) where

import Types.Simulation
import Types.DMModel
import Utilities
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import Data.Validation
import Data.Tuple (swap)
import qualified Data.List as L
import Control.Applicative (liftA2)

data AttractorsInvalid =
      SwitcheNamesDifferent ([NodeName], [NodeName])
    | SwitcheNodesDifferent (NodeName, ([NodeName], [NodeName]))
    | NotAnAttractor Thread
    | InvalidLVReorder InvalidLVReorder
    deriving (Show, Eq)


-- Starting from every attractor, alter each environment, one at a time, to
-- every other option, and see where the network goes from this attractor. Test
-- if the resulting attractor is new. This happens a lot when a phenotype is
-- relatively stable in one environment, but gets overshadowed by a large basin,
-- say apoptosis, in another env. The procedure helps complete the picture. If
-- you find new attractors this way, repeat until you don't.
largeBasinAtts :: ModelEnv
               -> HS.HashSet Attractor
               -> Simulation (HS.HashSet Attractor)
largeBasinAtts mEnv atts = do 
    let updatedAtts = largeBasinAtts' mEnv atts
    return updatedAtts

largeBasinAtts' :: ModelEnv -> HS.HashSet Attractor -> HS.HashSet Attractor
largeBasinAtts' (mL, SamplingParameters _ _ _ limitedInps) atts =
    atts <> go atts atts
    where
        go checkAtts accumAtts
            | HS.null excessAtts = HS.empty
            | otherwise = excessAtts <> go (excessAtts <> checkAtts) excessAtts
            where
                excessAtts = HS.fromList $ filter attFilterF rundowns
                attFilterF = not . flip elem checkAtts
                rundowns = attRunDownSimple stepper <$> fixedVecs
                fixedVecs = liftA2 U.update attVecs iLevels
                attVecs = (mconcat . fmap B.toList . HS.toList) accumAtts
        iLevels = mconcat $ inputSolos iPts limitedInps lniBMap
        iPts = (inputs . modelGraph) mL
        stepper = synchStep ivList ttList
        LayerSpecs lniBMap _ ttList ivList = layerPrep mL

-- Return just the attractors of the ModelLayer. This is slower, but uses less
-- memory. We cannot check if we are runing down a chain of states we have
-- encountered before, but we also do not have to carry around a HashMap of all
-- the states we have ever encountered. 
attractors :: ModelEnv -> Simulation (HS.HashSet Attractor)
attractors mEnv = topStates attFolders mEnv >>= largeBasinAtts mEnv

attFolders :: Folder (HS.HashSet Attractor)
attFolders = Folder attRandFold attNoisyFold attRunDown

attRandFold :: (HS.HashSet Attractor) -> LayerVec -> (HS.HashSet Attractor)
attRandFold attHS _ = attHS

attNoisyFold :: (HS.HashSet Attractor) -> LayerVec -> (HS.HashSet Attractor)
attNoisyFold attHS _ = attHS

attRunDown :: PSStepper
           -> HS.HashSet Attractor
           -> LayerVec
           -> HS.HashSet Attractor
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

attRunDownSimple :: PSStepper -> LayerVec -> Attractor
attRunDownSimple stepper lV = runDown'' lV M.empty 0
    where
        runDown'' lVec sMap i
            | not $ M.member lVec sMap =
                let nextVec = stepper lVec
                    newMap = M.insert lVec i sMap
                    newI = i + 1
                in runDown'' nextVec newMap newI
            | otherwise =
                let partitionIndex = sMap M.! lVec
                    updateVec = (B.fromList . (swap <$>) . M.toList) sMap
                    vLength = B.length updateVec
                    emptyVec = B.replicate vLength U.empty
                    orderedVec = B.update emptyVec updateVec
                in mkAttractor $ B.drop partitionIndex orderedVec

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

-- minAttractors :: Int -> ModelLayer -> Simulation (HS.HashSet Attractor)
-- minAttractors minS mL = do
--     atts <- attractors $ ModelEnv mL 100 0.02 50 0 []
--     case ((HS.size atts) < minS) of 
--                     True  -> minAttractors minS mL
--                     False -> return atts

-- Are the Attractors from a parsed attractor.csv actually Attractors of the
-- given ModelLayer from the parsed DMMS file? 
attractorCheck :: (DMMSModelMapping, ModelLayer)
               -> AttractorBundle
               -> (Validation [AttractorsInvalid] (HS.HashSet Attractor))
attractorCheck (dmmsMMap, mL) (csvMMap, csvLNIBMap, atts) =
    case mmCheck csvMMap dmmsMMap of
    err:errs -> Failure (err:errs)
    [] -> (validationed valF orderedAtts) <* checkedAtts
        where
            checkedAtts :: Validation [AttractorsInvalid] (HS.HashSet Attractor)
            checkedAtts = HS.fromList <$> (traverse (attCheck
                dmmsLNIBMap dmmsPSStepper csvLNIBMap) (HS.toList atts)) 
            orderedAtts :: Validation [InvalidLVReorder] (HS.HashSet Attractor)
            orderedAtts = HS.fromList <$> (traverse
                (lNISwitchThread csvLNIBMap dmmsLNIBMap) (HS.toList atts))
            dmmsPSStepper = synchStep dmmsIVList dmmsTTList
            LayerSpecs dmmsLNIBMap _ dmmsTTList dmmsIVList = layerPrep mL

valF :: Validation [InvalidLVReorder] (HS.HashSet Attractor)
     -> Validation [AttractorsInvalid] (HS.HashSet Attractor)
valF (Success x) = Success x
valF (Failure x) = Failure $ ilvL2aiL x

ilvL2aiL :: [InvalidLVReorder] -> [AttractorsInvalid]
ilvL2aiL = fmap ilv2ai
    where
        ilv2ai NewOldOrderingMismatch = InvalidLVReorder NewOldOrderingMismatch
        ilv2ai OldOrderingLVMismatch = InvalidLVReorder OldOrderingLVMismatch

-- Is the given Thread an Attractor of the given PSStepper?
attCheck :: LayerNameIndexBimap
         -> PSStepper
         -> LayerNameIndexBimap
         -> Thread
         -> Validation [AttractorsInvalid] Thread
attCheck dmmsLNIBMap dmmsPSStepper csvLNIBMap thread
    | isAtt dmmsLNIBMap dmmsPSStepper csvLNIBMap thread = Success thread
    | otherwise = Failure [NotAnAttractor thread]

-- Note that both ModelMappings here have already gone through a parsing, which
-- means that every switch name is unique, as is its associated NodeName list,
-- and no two switches share any NodeNames in common. 
mmCheck :: DMMSModelMapping -> DMMSModelMapping -> [AttractorsInvalid]
mmCheck csvMMap dmmsMMap = case lrUniques cSwitches dSwitches of
    ([], []) -> foldr switchContentsF [] pairedMMs
    err -> [SwitcheNamesDifferent err]
    where
        pairedMMs = zipWith zipper sortedCSV sortedDMMS
        zipper (x, xs) (_, ys) = (x, xs, ys)
        sortedCSV = L.sortOn fst csvMMap
        sortedDMMS = L.sortOn fst dmmsMMap
        cSwitches = fst <$> csvMMap
        dSwitches = fst <$> dmmsMMap

switchContentsF :: (NodeName, [NodeName], [NodeName])
                -> [AttractorsInvalid]
                -> [AttractorsInvalid]
switchContentsF (sName, csVNNs, dmmsNNs) ais = case lrUniques csVNNs dmmsNNs of
    ([], []) -> ais
    errs     -> (SwitcheNodesDifferent (sName, errs)) : ais


