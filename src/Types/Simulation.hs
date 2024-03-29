{-# LANGUAGE OverloadedStrings #-}


module Types.Simulation
    ( Simulation
    , ModelEnv
    , SamplingParameters (..)
    , Thread
    , Attractor
    , AttractorSet
    , AttractorBundle
    , Folder (..)
    , LayerVec
    , FixedVec
    , RangeTop
    , LayerSpecs (..)
    , layerPrep
    , LayerNameIndexBimap
    , LayerRangeVec
    , PSStepper
    , PNStepper'
    , PAStepper'
    , InvalidLVReorder (..)
    , synchStep
    , noisyStep'
    , asyncStep''
    , topStates
    , mkAttractor
    , layerVecReorder
    , attractorMMReorder
    , inputs
    , inputCombos
    , inputSolos
    , inputLevels
    , inputDegrees
    , isAtt
    , lNISwitchThread
    ) where

import Types.DMModel
import Utilities
import Data.Validation
import Control.Monad.State.Strict
import System.Random.Stateful
import System.Random.Shuffle (shuffle')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.Bimap as BM
import qualified Data.Graph.Inductive as Gr
import qualified Control.Parallel.Strategies as P
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Sequence as S
import qualified Data.List.Extra as L


type Simulation = State StdGen

type ModelEnv = (ModelLayer, SamplingParameters)

data SamplingParameters = SamplingParameters {
--  The number of random LayerVecs to generate for each combination of inputs.
      randomN :: Int
--  Number of noisy steps to take when gathering attractors/statistics for each
--  combination of inputs.
    , noisyN :: Int
--  Probability that any given state slips on a noisy step.
    , noisyP :: Probability
--  Each Layer has inputs, which are nodes that do not listen to other nodes,
--  and which represent external influence on whatever set of biological
--  functions that the ModelLayer represent (e.g. gamma radiation, fixed,
--  expression levels of growth factor, etc…). We study how the ModelLayer
--  behaves under the influence of these inputs by finding those attractors 
--  of the whole network that are reached from each subset of the state space
--  where initial states are chosen that reflect a set value (or combination of
--  values) of each input. For example, which attractors do we reach from
--  initial states chosen randomly except that gamma is on and growth factor is
--  high? Many DMMS files will have a large number of inputs, some of which we
--  are not investigating. limitedInputs tells us which inputs are constrained
--  to certain values.
    , limitedInputs :: [(NodeName, [Int])]
    } deriving (Eq, Show)

-- foldl' functions that accumulate a network property at random, noisy, and
-- neighbor steps. 
data Folder a = Folder {
                randomFolder :: a -> LayerVec -> a
              , noisyFolder :: a -> LayerVec -> a
              , runDown :: PSStepper -> a -> LayerVec -> a
              }

-- Vector representation of the state of the ModelLayer whose environment you
-- are in.
type LayerVec = U.Vector NodeState
-- Bijective map between NodeNames and LayerVec positions. Possible because
-- NodeNames are validated unique in any given DMMS file. 
type LayerNameIndexBimap = BM.Bimap NodeName NodeIndex
-- For a given gate, the indices for its inputs in a LayerVec, in its GateOrder.
type IndexVec = U.Vector Int
-- All of the IndexVecs for the ModelLayer, in LayerVec order. 
type IndexVecList = [IndexVec]
-- All of the TruthTables for the ModelLayer, in LayerVec order. 
type TruthTableList = [TruthTable]
-- The ranges for all of the DMNodes for the ModelLayer, in LayerVec order. 
type LayerRangeVec = U.Vector (Int, RangeTop)
data LayerSpecs = LayerSpecs { 
                  lNameIndexM :: LayerNameIndexBimap
                , lRangeVec   :: LayerRangeVec
                , tTableList  :: TruthTableList
                , iVecList    :: IndexVecList
                } deriving (Show, Eq)
-- Specify an input combination, with each input NodeIndex and the state each is
-- fixed to. 
type FixedVec = U.Vector (NodeIndex, NodeState)
type RangeTop = Int
type Thread = B.Vector LayerVec

-- Attractors are loops of LayerVecs (whose size may be 1). ONLY create
-- Attractors with mkAttractor, which guarantees that the attractor will begin
-- with its "smallest" member. Otherwise every time we find the same Attractor,
-- but enter at a different place in the loop, it looks like a new Attractor. 
type Attractor = Thread
-- Collection of Attractors, mapped to the HashSet of their attractor basin.
type AttractorSet = M.HashMap Attractor (HS.HashSet LayerVec)
-- This is what's written when we write attractors out to storage. It
-- contains the attractors from the Fine layer of the DMModel and the switch
-- mappings from the next layer up. 
type AttractorBundle = ( DMMSModelMapping
                       , LayerNameIndexBimap
                       , HS.HashSet Attractor)
-- Steppers primed with everything they need except a LayerVec
-- Synchronous, deterministic
type PSStepper = LayerVec -> LayerVec
-- Noisy, synchronous, non-monadic
type PNStepper' = LayerVec -> StdGen -> (LayerVec, StdGen)
-- Asynchronous, deterministic (noisy in which DMNode is selected to update at a
-- time, but deterministic in that no DMNode slips), non-monadic. 
type PAStepper' = LayerVec -> StdGen -> (LayerVec, StdGen)
-- Asynchronous, deterministic (noisy in which DMNode is selected to update at a
-- time, but deterministic in that no DMNode slips), monadic. 
-- type PAStepper = LayerVec -> Simulation LayerVec

data InvalidLVReorder = NewOldOrderingMismatch
                      | OldOrderingLVMismatch
                      deriving (Show, Eq)

-- Top level accumulation of some property of the ModelLayer that we are
-- collecting data on, when we follow the algorithm diagrammed below. 
topStates :: (Monoid a, P.NFData a) => Folder a -> ModelEnv -> Simulation a
topStates folders mEnv = do
    let lInputs = inputs ((modelGraph . fst) mEnv)
        lSpecs = (layerPrep . fst) mEnv
        limiteds = (limitedInputs . snd) mEnv
        inCombos = inputCombos lInputs limiteds (lNameIndexM lSpecs)
    iGens <- state $ genGen $ length inCombos
    let seeds = zip iGens inCombos
    let batches = P.parMap P.rdeepseq (inputStats mEnv lSpecs folders) seeds
    return $ L.foldl' (<>) mempty batches

-- Collect property statistics in the region of a given input combination. 
inputStats :: (Monoid a, P.NFData a)
           => ModelEnv
           -> LayerSpecs
           -> Folder a
           -> (StdGen, FixedVec)
           -> a
inputStats mEnv (LayerSpecs lniMap lrVec ttList ivList) folders (gen, fixed)
    = L.foldl' rFold mempty $ L.unfoldr randomUnfold (0, randomGen)
    where
        randN = (randomN . snd) mEnv
        noiseN = (noisyN . snd) mEnv
        noiseLevel = (noisyP . snd) mEnv
        pSStepper = synchStep ivList ttList
        inputNeighborF = neighbors ((layerRanges . fst) mEnv) lniMap
        rFold = randomFold
            noisyGen noiseN noiseLevel pSStepper inputNeighborF lrVec folders
        (noisyGen, randomGen) = split gen
        randomUnfold (i, g)
            | i >= randN = Nothing
            | otherwise  = Just (lVec, (i + 1, newG))
                where
                    (lSeq , newG) = U.foldl' chainSR (S.empty, g) lrVec
                    lVec = U.update (seqToVec lSeq) fixed


-- Fold up a list of randomly generated states that the ModelLayer can be in,
-- generating and folding up a noisy walk, neighbor states for each noisy step,
-- attractor Threads with Attractors for each neighbor state and LayerStats for
-- everything along the way. 
randomFold :: (Monoid a, P.NFData a)
           => StdGen
           -> Int -- How many noisy steps to take
           -> Double -- noise level
           -> PSStepper -- ModelLayer specific synchronous stepper
           -> (LayerVec -> [[LayerVec]]) --ModelLayer specific neighbor function
           -> LayerRangeVec
           -> Folder a
           -> a
           -> LayerVec
           -> a
randomFold gen noiseN nLevel pSStepper inputNeighborF lrVec folders prop lV =
    L.foldl' nFold newProp $ L.unfoldr noisyUnfold (0, lV, gen)
    where
        nFold = noisyFold pSStepper inputNeighborF folders
        newProp = (randomFolder folders) prop lV
        noisyUnfold (i, lVec, g)
            | i >= noiseN = Nothing
            | otherwise = Just (slipVec, (i + 1, slipVec, newG))
                where
                    nextVec = pSStepper lVec
                    rangedNVec = U.zip nextVec lrVec
                    (slipVec, newG) = nStepFunc nLevel rangedNVec g

-- Fold up a list of noisily stepped states, generating and folding up neighbor
-- states for each noisy step, attractor Threads with Attractors for each
-- neighbor state and LayerStats for everything along the way. 
noisyFold :: (Monoid a, P.NFData a)
          => PSStepper -- ModelLayer specific synchronous stepper
          -> (LayerVec -> [[LayerVec]]) -- ModelLayer specific neighbor function
          -> Folder a
          -> a
          -> LayerVec
          -> a
noisyFold pSStepper inputNeighborF folders prop lV = L.foldl' nsFold newProp lVs
    where
        lVs = inputNeighborF lV
        newProp = (noisyFolder folders) prop lV
        nsFold = neighborsFold pSStepper folders

-- Fold up a list of lists of neighbors, running down from the neighbors to
-- their attractors. 
neighborsFold :: (Monoid a, P.NFData a)
              => PSStepper -- ModelLayer specific synchronous stepper
              -> Folder a
              -> a
              -> [LayerVec]
              -> a
neighborsFold pSStepper folders prop lVs = L.foldl' runner prop lVs
  where
    runner = (runDown folders) pSStepper

-- Make an Attractor by shifting the loop around so that the smallest LayerVec
-- comes first in the Vector. 
mkAttractor :: Thread -> Attractor
mkAttractor vecVec = B.backpermute vecVec $ B.generate vLength indexShift
    where
        smallestIndex = B.minIndex vecVec
        vLength = B.length vecVec
        indexShift i = (i + smallestIndex) `rem` vLength

-- Chain a uniformR function so that I can fold up the LayerRange to get a
-- random LayerVec, and also end up with a single g -> (a, g) function to pass
-- to state. 
chainSR :: (S.Seq NodeState, StdGen)
           -> (NodeState, NodeState)
           -> (S.Seq NodeState, StdGen)
chainSR (buildSeq, g) nRange = (buildSeq S.|> nState, newG) 
    where
        (nState, newG) = uniformR nRange g

-- Compute all the LayerVecs that are one NodeState change away in the state
-- transition graph. If a DMNode has more than 2 states neighbors will include
-- a separate LayerVec for each other state the node could be in, which is why
-- neighbors returns a [[LayerVec]] instead of a [LayerVec]. 
neighbors :: LayerRange
          -> LayerNameIndexBimap
          -> LayerVec
          -> [[LayerVec]]
neighbors r o s = [s]:neighborhood
    where
        neighborhood = ((uncurry (otherStates s o)) . inZip) <$> nodeNames
        inZip x = (x, ((s U.! ) . (o BM.!)) x)
        nodeNames = M.keys r

-- Compute LayerVecs resulting from a change in the state of the given node to
-- any of the other states it could be in. 
otherStates :: LayerVec
            -> LayerNameIndexBimap
            -> NodeName
            -> NodeState
            -> [LayerVec]
otherStates lVec lniBMap nName rangeTop = vecs
    
    where
        vecs = sequenceA ((flip (U.//)) <$> updateList) lVec
        currentState = lVec U.! nodeIndex
        updateList = (:[]) <$> (zip (repeat nodeIndex) others)
        others = filter (/= currentState) $ fillDown rangeTop
        nodeIndex = lniBMap BM.! nName


tableGateEval :: LayerVec -> IndexVec -> TruthTable -> NodeState
tableGateEval lVec iVec tTable = tTable M.! (U.backpermute lVec iVec)

-- Synchronous, deterministic network update
synchStep :: IndexVecList -> TruthTableList -> LayerVec -> LayerVec
synchStep ivs tts lVec = newVec
    where
        newVec = U.fromList $ L.zipWith (tableGateEval lVec) ivs tts 

-- Synchronous, noisy network update. Non-monadic
noisyStep' :: PSStepper -> Probability -> LayerRangeVec
           -> LayerVec -> StdGen
           -> (LayerVec, StdGen)
noisyStep' psStepper noiseLevel lrVec lVec gen = (slippedVec, newGen)
    where
        (slippedVec, newGen) = nStepFunc noiseLevel rangedNVec gen
        rangedNVec = U.zip nextVec lrVec
        nextVec = psStepper lVec

nStepFunc :: Double
-- A zipped LayerVec and LayerRangeVec 
          -> U.Vector (NodeState, (NodeState, RangeTop))
          -> StdGen
          -> (LayerVec, StdGen)
nStepFunc noiseLevel rangedNVec g = (lVec, newG)
    where
        (lSeq , newG) = U.foldl' (chainP noiseLevel) (S.empty, g) rangedNVec
        lVec = seqToVec lSeq

-- Chain a uniformR function so that I can fold up a (LayerVec, LayerRange)
-- to get see if any if the LayerVec's states slip due to noise, and also end up
-- with a single g -> (a, g) function to pass to state. 
chainP :: Double
       -> (S.Seq NodeState, StdGen)
       -> (NodeState, (NodeState, RangeTop))
       -> (S.Seq NodeState, StdGen)
chainP p (buildSeq, g) (nState, (low, high))
    | chance > p = (buildSeq S.|> nState, newG)
    | otherwise =
        let possibleSlips = [n | n <- [low, high], n /= nState]
            (slipIndex, finalG) =
                uniformR (0, (length possibleSlips) - 1) newG
            newState = possibleSlips L.!! slipIndex
        in (buildSeq S.|> newState, finalG)
    where
        (chance, newG) = uniformR (0 :: Double, 1 :: Double) g

-- Asynchronous, deterministic network update. 
-- asyncStep :: Int -- Length of a LayerVec in this ModelLayer
--           -> M.HashMap NodeIndex TruthTable
--           -> M.HashMap NodeIndex IndexVec
--           -> LayerVec
--           -> Simulation LayerVec
-- asyncStep vecLength ttMap ivMap lVec = do
--     updateIndex <- state $ uniformR (0, vecLength - 1)
--     let tTable = ttMap M.! updateIndex
--         iVec = ivMap M.! updateIndex
--         newVec = lVec U.// [(updateIndex, tableGateEval lVec iVec tTable)]
--     return newVec

-- Asynchronous, deterministic network update. Non-monadic
-- asyncStep' :: Int -- Length of a LayerVec in this ModelLayer
--            -> M.HashMap NodeIndex TruthTable
--            -> M.HashMap NodeIndex IndexVec
--            -> LayerVec -> StdGen
--            -> (LayerVec, StdGen)
-- asyncStep' vecLength ttMap ivMap lVec gen = (nextVec, newGen)
--     where
--         (updateIndex, newGen) = uniformR (0, vecLength - 1) gen
--         tTable = ttMap M.! updateIndex
--         iVec = ivMap M.! updateIndex
--         nextVec = lVec U.// [(updateIndex, tableGateEval lVec iVec tTable)]

-- Asynchronous, deterministic network update where all gates are updated in
-- each step, in random permutation, save for gates deliberately biased to
-- always go first or last. 
asyncStep'' :: [IntBiasOrder]
            -> [IntBiasOrder]
            -> M.HashMap NodeIndex TruthTable
            -> M.HashMap NodeIndex IndexVec
            -> LayerVec -> StdGen
            -> (LayerVec, StdGen)
asyncStep'' intBiasOF intBiasOL ttMap ivMap lVec gen = (nextVec, newGen)
    where
        nextVec = L.foldl' (modifyUpdater ttMap ivMap) lVec mergedIL
        mergedIL = pfxIL <> shuffledIL <> sfxIL
        shuffledIL = shuffle' freeIL shuffleI shuffleGen
        shuffleI = L.length freeIL
        freeIL = createFreeIndicies (U.length lVec - 1) pfxIL sfxIL
        sfxIL = biasResolve intBiasOL lVec
        pfxIL = biasResolve intBiasOF lVec
        (newGen, shuffleGen) = split gen

createFreeIndicies :: Int -> [NodeIndex] -> [NodeIndex] -> [NodeIndex]
createFreeIndicies maxI pfxIL sfxIL = filter fF [0..maxI]
    where fF i = L.notElem i pfxIL && L.notElem i sfxIL

-- We do not need to keep any of the intermediary LayerVecs when we fold up the
-- list of update NodeIndices, so it is better for memory to destructively
-- modify the LayerVec long the way. 
modifyUpdater :: M.HashMap NodeIndex TruthTable
              -> M.HashMap NodeIndex IndexVec
              -> LayerVec
              -> NodeIndex
              -> LayerVec
modifyUpdater ttMap ivMap lVec updateIndex =
    U.modify (\v -> MU.write v updateIndex newState) lVec
    where
        newState = tableGateEval lVec iVec tTable
        tTable = ttMap M.! updateIndex
        iVec = ivMap M.! updateIndex

-- Create a [NodeIndex] from a [BiasOrder] 
biasResolve :: [IntBiasOrder] -> LayerVec -> [NodeIndex]
biasResolve biasOrder lVec = mapMaybe bResolve biasOrder
    where
        bResolve (IntWholeNode nIndex) = Just nIndex
        bResolve (IntSpecificState nIndex nState)
            | lVec U.! nIndex == nState = Just nIndex
            | otherwise = Nothing

-- Construct (LayerNameIndexBimap, LayerRangeVec, TruthTableList, IndexVecList)
-- from a ModelLayer. Ordering is alphabetical by node name in each. 
layerPrep :: ModelLayer -> LayerSpecs
layerPrep mL = LayerSpecs lniBMap lrVec ttList ivList 
    where
        lniBMap = BM.fromList $ zip (gNodeName <$> orderedNGates) [0..]
        lrVec = U.fromList $ ((\(_, n) -> (0, n)) . nodeRange) <$> orderedNodes
        ttList = gateTable <$> orderedNGates
        ivList = U.fromList <$>
            ((lniBMap BM.!) <<$>> gateOrder <$> orderedNGates)
        orderedNGates = nodeGate <$> orderedNodes
        orderedNodes = L.sortOn (nodeName . nodeMeta) nodes
        nodes = layerNodes mL

-- Find the inputs of the ModelGraph. Inductively, a node is an input if it has
-- a self-loop and NO other inlinks, or is a node which has a self-loop, sibling
-- nodes and which has an inlink from every ancestor back to that first
-- self-loop. See the diagrams below. Afterwards, be sure to exclude those
-- DMNodes in constrainedInputs. 
inputs :: ModelGraph -> [[DMNode]]
inputs mG = (fromJust . Gr.lab mG) <<$>> (L.nubOrd <$> reps)
  where
    reps = (uncurry go) <$> goStart
    goStart :: [([Gr.Node], Gr.Node)]
    goStart = (\n -> ([], n)) <$> startGRInts
    startGRInts = soleSelfLoops mG
    go :: [Gr.Node] -> Gr.Node -> [Gr.Node]
    go ns n
        | n `elem` inList && (L.delete n inList) == ns = case outList of
            [] -> ns ++ [n]
            _  ->  concatMap (go (ns ++ [n])) outList
        | otherwise = ns
        where
            inList = Gr.pre mG n
            outList = L.delete n $ Gr.suc mG n

-- Prepare the combinations of the various inputs, so that we can sample
-- randomly in these areas of the state space. Constrain those inputs that are
-- in the limitedInputs. This is a hack, because we need to be able to share
-- networks with software that can't do integer-valued nodes. 
inputCombos :: [[DMNode]]
            -> [(NodeName, [Int])]
            -> LayerNameIndexBimap
            -> [FixedVec]
inputCombos nodeLists limiteds lniBMap = U.concat <$> (sequenceA iLevels)
    where
        iLevels = inputSolos nodeLists limiteds lniBMap

-- Prepare a [FixedVec] for the levels of each input, alone. Constrain those
-- inputs that are in the limitedInputs. The list of contraints have already
-- been validated to contain only top-level nodes of real inputs, and only
-- levels that do exist.  
inputSolos :: [[DMNode]]
           -> [(NodeName, [Int])]
           -> LayerNameIndexBimap
           -> [[FixedVec]]
inputSolos nodeLists lims lniBMap = uncurry (inputLevels lniBMap) <$> pairedNLs
    where
        pairedNLs = foldr (findF lims) [] nodeLists
        findF ls inPT annotatedDMNodes = (lim, inPT):annotatedDMNodes
            where
                lim = snd <$> (L.find ((flip elem dmNames) . fst) ls)
                dmNames = (nodeName . nodeMeta) <$> inPT

-- We know that all the multi-node inputs are binary from
-- nonBinaryMultiInputNodesCheck at parse. We assume they are wired such that,
-- from first to last in the list, for eg a 3-node input, 000, 001, 011, and 111
-- will be the attractors of the input that represent the zeroth through third
-- levels. In this way, an n-level input will be represented by n-1 nodes.
-- Single-node inputs MAY be integer-valued, so take that into account. 
inputLevels :: LayerNameIndexBimap -> Maybe [Int] -> [DMNode] -> [FixedVec]
inputLevels _ _ [] = []
inputLevels lniBMap mPerms [n] = vecs
    where
        levels = case mPerms of
            Nothing -> naiveLevels
            Just permitteds -> filter (flip elem permitteds) naiveLevels
        naiveLevels = fst <$> ((gateAssigns . nodeGate) n)
        nName = (nodeName . nodeMeta) n
        vecs = U.singleton <$> (zip (repeat $ lniBMap BM.! nName) levels)
inputLevels lniBMap mPerms ns = 
    let pairsLists = (zip indices) <$> levelLists
        indices = ((lniBMap BM.!) . nodeName . nodeMeta) <$> ns
        levelLists = mkLevels mPerms levels
        levels = (L.length ns) + 1
    in  U.fromList <$> pairsLists

-- Generate [[Int]] that denotes the state each DMNode of an input must be in to
-- specify each degree of input. 
mkLevels :: Maybe [Int] -> Int -> [[Int]]
mkLevels mPerms n
    | n < 3 = [] -- levels must be at least 3, because ns must have at least 2
    -- elements, because we already pattern-matched on [] and [n]. 
    | otherwise = mkLevels' <$> lStates
        where
            lStates = case mPerms of
                Nothing -> [0..(n - 1)]
                Just permitteds -> filter (flip elem permitteds) [0..(n - 1)]
            mkLevels' m = (replicate ((n - 1) - m) 0) ++ (replicate m 1)

-- How many states does an input have? Not the state of the component DMNodes, 
-- but the input itself. NOT ZERO INDEXED! Think `length`, not (!!).
inputDegrees :: [DMNode] -> Int
inputDegrees [] = 0
inputDegrees [n] = (L.length . gateAssigns . nodeGate) n
inputDegrees ns = (L.length ns) + 1

-- Find those Nodes whose only incoming link is themselves. 
soleSelfLoops :: ModelGraph -> [Gr.Node]
soleSelfLoops mG = onlys
    where
        onlys = filter (onlyMe mG) nodes
        nodes = Gr.nodes mG
        onlyMe g n = Gr.pre g n == [n]

-- Order the nodes in an attractor according to the Switch partitioning of the
-- layer above. 
attractorMMReorder :: DMMSModelMapping
                   -> LayerNameIndexBimap
                   -> Attractor
                   -> Either InvalidLVReorder Attractor
attractorMMReorder dmmsMM lniBMap att =
    traverse (layerVecReorder lniBMap switchNodeOrder) att
    where
        switchNodeOrder = concatMap snd dmmsMM

-- Re-order a LayerVec according to a new List of NodeNames. Basic error
-- checking, but use at your own risk. 
layerVecReorder :: LayerNameIndexBimap
                -> [NodeName]
                -> LayerVec
                -> Either InvalidLVReorder LayerVec
layerVecReorder lniBMap newOrder lVec
    | (L.sort . BM.keys) lniBMap /= L.sort newOrder =
        Left NewOldOrderingMismatch
    | BM.size lniBMap /= U.length lVec = Left OldOrderingLVMismatch
    | otherwise = Right $ U.backpermute lVec permuteVec
        where
             permuteVec = U.fromList $ (lniBMap BM.!) <$> newOrder

-- Re-order a LayerVec according to a new LayerNameIndexBimap. Basic error
-- checking, but use at your own risk. 
lNISwitchVec :: LayerNameIndexBimap
             -> LayerNameIndexBimap
             -> LayerVec
             -> Either InvalidLVReorder LayerVec
lNISwitchVec oldLNIBM newLNIBM lVec
    | (L.sort . BM.keys) oldLNIBM /= (L.sort . BM.keys) newLNIBM =
        Left NewOldOrderingMismatch
    | BM.size oldLNIBM /= U.length lVec = Left OldOrderingLVMismatch
    | otherwise = Right $ U.backpermute lVec permuteVec
        where
            permuteVec = U.fromList $ (oldLNIBM BM.!) <$> newOrder
            newOrder = ((fst <$>) . L.sortOn snd . BM.toList) newLNIBM

-- Reorder the individual LayerVecs in the thread according to the
-- new LayerNameIndexBimap, and then cycle the thread so that it is a proper
-- (possible) Attractor for that LayerNameIndexBimap. Basic error checking, but
-- use at your own risk. 
lNISwitchThread :: LayerNameIndexBimap
                -> LayerNameIndexBimap
                -> Thread
                -> Validation [InvalidLVReorder] Attractor
lNISwitchThread oldLNIBM newLNIBM thread =
                    mkAttractor <$> preppedValid
    where
        preppedValid :: Validation [InvalidLVReorder] Attractor
        preppedValid = sequenceA $ (liftError (:[])) <$> switchedVecs
        switchedVecs = lNISwitchVec oldLNIBM newLNIBM <$> thread

-- Check if a Thread (with its LayerNameIndexBimap), is an attractor of a 
-- PSStepper (with its LayerNameIndexBimap). 
isAtt :: LayerNameIndexBimap
      -> PSStepper
      -> LayerNameIndexBimap
      -> Thread
      -> Bool
isAtt dmmsLNIBMap dmmsPSStepper csvLNIBMap thread = case testThread of
    Failure _ -> False
    Success testAtt -> (go . B.toList) testAtt
    where
        testThread = lNISwitchThread csvLNIBMap dmmsLNIBMap thread
        go :: [LayerVec] -> Bool
        go [] = True
        go [v] = dmmsPSStepper v == v
        go (v1:v2:vs) = go' $ (v1:v2:vs) <> [v1]
            where
                go' :: [LayerVec] -> Bool
                go' [] = True
                go' [_] = True
                go' (vA:vB:vss) = (dmmsPSStepper vA == vB) && (go' (vB:vss))

-- Basic network property algorithm step (Klemm algorithm):
--    ┌────────────┐                 ┌───────────┐                              
--    │Random State│                 │Noisy Steps│                              
--    └────────────┘                 └───────────┘                              
--           │                             │                                    
--      ┌────┘      ┌──────────────────────┼──────────────────────┐             
--      ▼           │                      │                      │             
--     .─.          ▼         .─.          ▼         .─.          ▼         .─. 
--    (   )  ─────────────▶  (   )  ─────────────▶  (   )  ─────────────▶  (   )
--     `─'                    `─'                    `─'                    `─' 
--      │    ┌───────────┐                            │                         
--      │◀┐  │Synchronous│                  ┌────────┬┴───────┐                 
--      │ └──│   Steps   │                  │        │        │                 
--      ▼    └───────────┘                  ▼        ▼        ▼                 
--     .─.         │                        .        .        .                 
--    (   )        │                       ( )      ( )      ( )                
--     `─'         │                        '        '        '                 
--      │          │                       ┌───────────────────┐                
--      │ ◀────────┤                       │    Single-Step    │                
--      │          │                       │     Neighbors     │                
--      ▼          │                       │ (Also find all of │                
--     .─.         │                       │ their attractors) │                
--    (   )        │                       └───────────────────┘                
--     `─'         │                                                            
--      │      ┌────                                                            
--      │  ◀───┘                                                                
--      │           ┌─────────────────────────────────────────────────┐         
--      ▼           │  For each combination of inputs, do this for n  │         
--   .─────.        │   randomly picked states, where n is a number   │         
--  ;       :       │  sufficient that n/10 additional randoms steps  │         
--  :       ;       │       doesn't get you any new attractors.       │         
--   ╲     ╱        └─────────────────────────────────────────────────┘         
--    `───'                                                                     
--      ▲                                                                       
--      └┐                                                                      
-- ┌───────────┐                                                                
-- │ Attractor │                                                                
-- └───────────┘                                                                

-- 00 01 11 (GF_High, GF)
-- 4-node input, 0000, 0001, 0011, 0111, and 1111 (ABCD ordering)
-- Input graphs:
-- 
--  ┌───┐                                           
--  │   │                                           
--  ▼   │                                           
--  .   │                                           
-- (A)──┘                                           
--  '                                               
--  │                                               
--  │                                               
--  │                                               
--  │            ┌───┐                              
--  │            │   │                              
--  │            │   │                              
--  │            .   │                              
--  ├──────────▶(B)◀─┘                              
--  │            '                                  
--  │            │                                  
--  │            │                                  
--  │            │                                  
--  │            │                ┌───┐             
--  │            │                │   │             
--  │            │                │   │             
--  │            │                .   │             
--  ├────────────┼──────────────▶(C)◀─┘             
--  │            │                '                 
--  │            │                │                 
--  │            │                │                 
--  │            │                │                 
--  │            │                │            ┌───┐
--  │            │                │            │   │
--  │            │                │            │   │
--  │            │                │            .   │
--  └────────────┴────────────────┴──────────▶(D)◀─┘
--                                             '    


