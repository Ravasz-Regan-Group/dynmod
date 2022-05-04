{-# LANGUAGE OverloadedStrings #-}


module Types.Simulation
    ( Simulation
    , ModelEnv (..)
    , Attractor
    , AttractorSet
    , Folder (..)
    , LayerVec
    , LayerSpecs (..)
    , layerPrep
    , LayerNameIndexMap
    , PSStepper
    , topStates
    , mkAttractor
    , layerVecReorder
    , inputs
    , inputCombos
    ) where

-- import Debug.Trace
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import Control.Monad.State.Strict
import System.Random.Stateful
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Graph.Inductive as Gr
import qualified Control.Parallel.Strategies as P
import qualified Data.List.Unique as Uniq
import Types.DMModel
import Utilities

type Simulation = State StdGen

data ModelEnv = ModelEnv {
--  The ModelLayer to characterize
      mLayer  :: ModelLayer
--  The number of random LayerVecs to generate for each combination of inputs.
    , randomN :: Int
--  Probability that any given state slips on a noisy step.
    , noisyP  :: Double
--  Number of noisy steps to take when gathering attractors/statistics for each
--  combination of inputs.
    , noisyN  :: Int
--  Number of asynchronous steps to take when gathering attractors/statistics
--  for each combination of inputs.
    , asynchN :: Int
--  Each Layer has inputs, which are nodes that do not listen to other nodes,
--  and which represent external influence on whatever set of biological
--  functionality that the ModelLayer represent (e.g. gamma radiation, fixed,
--  expression leves of growth factor, etc…). We study how the ModelLayer
--  behaves under the influence of these inputs by finding those attractors 
--  of the whole network that are reached from each subset of the state space
--  where initial states are chosen that reflect a set value (or combination of
--  values) of each input. For example, which attractors do we reach from
--  initial states chosen randomly except that gamma is on and growth factor is
--  high? Many DMMS files will have a large number of inputs, some of which we
--  are not investigating. constrainedInputs tells us which inputs to so ignore.
    , constrainedInputs :: [NodeName]
    } deriving (Show, Eq)

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
-- Lookup order to identify NodeNames with LayerVec positions. 
type LayerNameIndexMap = M.HashMap NodeName NodeIndex
-- For a given gate, the indices for its inputs in a LayerVec, in its GateOrder.
type IndexVec = U.Vector Int
-- All of the IndexVecs for the ModelLayer, in LayerVec order. 
type IndexVecList = [IndexVec]
-- All of the TruthTables for the ModelLayer, in LayerVec order. 
type TruthTableList = [TruthTable]
-- The ranges for all of the DMNodes for the ModelLayer, in LayerVec order. 
type LayerRangeVec = U.Vector (Int, RangeTop)
data LayerSpecs = LayerSpecs { 
                  lNameIndexM :: LayerNameIndexMap
                , lRangeVec   :: LayerRangeVec
                , tTableList  :: TruthTableList
                , iVecList    :: IndexVecList
                } deriving (Show, Eq)
type NodeIndex = Int
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

-- Steppers primed with everything they need except a LayerVec
-- Synchronous, deterministic
type PSStepper = LayerVec -> LayerVec
-- Noisy, synchronous
type PAStepper = LayerVec -> Simulation LayerVec

-- Top level accumulation of some property of the ModeLayer that we are
-- collecting data on, when we follow the algorithm diagrammed below. 
topStates :: (Monoid a, P.NFData a) => Folder a -> ModelEnv -> Simulation a
topStates folders mEnv = do
    let lInputs = inputs ((modelGraph . mLayer) mEnv)
        lSpecs = (layerPrep . mLayer) mEnv
        cons = constrainedInputs mEnv
        inCombos = inputCombos lInputs cons (lNameIndexM lSpecs)
    iGens <- state $ genGen $ length inCombos
    let seeds = zip iGens inCombos
    {- P.parMap P.rdeepseq -} 
    let batches = P.parMap P.rdeepseq (inputStats mEnv lSpecs folders) seeds
    return $ L.foldl' (<>) mempty batches

genGen :: Int -> StdGen -> ([StdGen], StdGen)
genGen i gen = go 0 [] gen
    where 
        go k gs g
            | k >= i = (gs, g)
            | otherwise = let (newG, seed) = split gen
                          in go (k + 1) (newG:gs) seed        

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
        randN = randomN mEnv
        noiseN = noisyN mEnv
        noiseLevel = noisyP mEnv
        pSStepper = synchStep ivList ttList
        inputNeighborF = neighbors ((layerRanges . mLayer) mEnv) lniMap
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

-- Chain a uniformR  function so that I can fold up the LayerRange to get a
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
          -> LayerNameIndexMap
          -> LayerVec
          -> [[LayerVec]]
neighbors r o s = [s]:neighborhood
    where
        neighborhood = ((uncurry (otherStates s o)) . inZip) <$> nodeNames
        inZip x = (x, ((s U.! ) . (o M.!)) x)
        nodeNames = M.keys r

-- Compute LayerVecs resulting from a change in the state of the given node to
-- any of the other states it could be in. 
otherStates :: LayerVec
            -> LayerNameIndexMap
            -> NodeName
            -> NodeState
            -> [LayerVec]
otherStates lVec orderMap nName rangeTop = vecs
    
    where
        vecs = sequenceA ((flip (U.//)) <$> updateList) lVec
        currentState = lVec U.! nodeIndex
        updateList = (:[]) <$> (zip (repeat nodeIndex) others)
        others = filter (/= currentState) $ fillDown rangeTop
        nodeIndex = orderMap M.! nName


tableGateEval :: LayerVec -> IndexVec -> TruthTable -> NodeState
tableGateEval lVec iVec tTable =
    let n = tTable M.! (U.backpermute lVec iVec) in n

-- Synchronous, deterministic network update
synchStep :: IndexVecList -> TruthTableList -> LayerVec -> LayerVec
synchStep ivs tts lVec = newVec
    where
        newVec = U.fromList $ L.zipWith (tableGateEval lVec) ivs tts 

nStepFunc :: Double
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

-- Asynchronous, deterministic network update
asyncStep :: Int -- Length of a LayerVec in this ModelLayer
          -> M.HashMap NodeIndex TruthTable
          -> M.HashMap NodeIndex IndexVec
          -> LayerVec
          -> Simulation LayerVec
asyncStep vecLength ttMap ivMap lVec = do
    updateIndex <- state $ uniformR (0, vecLength - 1)
    let tTable = ttMap M.! updateIndex
        iVec = ivMap M.! updateIndex
        newVec = lVec U.// [(updateIndex, tableGateEval lVec iVec tTable)]
    return newVec

-- Construct (LayerNameIndexMap, LayerRangeVec, TruthTableList, IndexVecList)
-- from a ModelLayer. Ordering is alphabetical by node name in each. 
layerPrep :: ModelLayer -> LayerSpecs
layerPrep mL = LayerSpecs lniMap lrVec ttList ivList
    where
        lniMap = M.fromList $ zip (gNodeName <$> orderedNGates) [0..]
        lrVec = U.fromList $ ((\(_, n) -> (0, n)) . nodeRange) <$> orderedNodes
        ttList = gateTable <$> orderedNGates
        ivList = U.fromList <$> ((lniMap M.!) <<$>> gateOrder <$> orderedNGates)
        orderedNGates = nodeGate <$> orderedNodes
        orderedNodes = L.sortOn (nodeName . nodeMeta) nodes
        nodes = layerNodes mL

-- Find the inputs of the ModelGraph. Inductively, a node is an input if it has
-- a self-loop and NO other inlinks, or is a node which has a self-loop, sibling
-- nodes and which has an inlink from every ancestor back to that first
-- self-loop. See the diagrams below. Afterwards, be sure to exclude those
-- DMNodes in constrainedInputs. 
inputs :: ModelGraph -> [[DMNode]]
inputs mG = (fromJust . Gr.lab mG) <<$>> ((fstOf3 . Uniq.complex) <$> reps)
  where
    reps = (uncurry go) <$> goStart
    goStart = (\n -> ([], n)) <$> startGRInts
    startGRInts = soleSelfLoops mG
    go :: [Gr.Node] -> Gr.Node -> [Gr.Node]
    go ns n
        | n `elem` inList && (L.delete n inList) == ns =
            concatMap (go (ns ++ [n])) outList
        | otherwise = ns
        where
            inList = Gr.pre mG n
            outList = L.delete n $ Gr.suc mG n

-- Prepare the combinations of the various inputs, so that we can sample
-- randomly in these areas of the state space. Remove those inputs that are in
-- constrainedInputs. This is a hack, because we need to be able to share
-- networks with software that can't do integer-valued nodes. 
inputCombos :: [[DMNode]]
            -> [NodeName]
            -> LayerNameIndexMap
            -> [FixedVec]
inputCombos nodeLists nos lniMap = U.concat <$> (sequenceA iLevels)
    where
        iLevels :: [[FixedVec]]
        iLevels = inputLevels lniMap <$> strippedNLs
        strippedNLs = filter stripper nodeLists
        stripper = not . (flip elem nos) . nodeName . nodeMeta . head

-- We check that all the nodes are binary, else why bother with a multi-node
-- input. We assume they are wired such that, from first to last in the list,
-- , for eg a 2-node input, 000, 001, 011, and 111 will be the attractors of the
-- input that represent the zeroth through third levels. In this way, an n-level
-- input will be represented by n-1 nodes. 
inputLevels :: LayerNameIndexMap
            -> [DMNode]
            -> [FixedVec]
inputLevels _ [] = []
inputLevels lniMap [n] = vecs
    where
        levels = fst <$> ((gateAssigns . nodeGate) n)
        nName = (nodeName . nodeMeta) n
        vecs = U.singleton <$> (zip (repeat $ lniMap M.! nName) levels)
inputLevels lniMap ns
    | enbyList /= [] = error $ "Non-binary nodes in multi-node input: " ++
            (show enbyList)
    | otherwise = let pairsLists = (zip indices) <$> levelLists
                      indices = ((lniMap M.!) . nodeName . nodeMeta) <$> ns
                      levelLists = mkLevels levels
                      levels = (length ns) + 1
                  in U.fromList <$> pairsLists
    where
        enbyList = filter enbys nameAssigns
        enbys xs = ((length . snd) xs) > 2
        nameAssigns = (\n -> (gNodeName n, gateAssigns n)) <$> gates
        gates = nodeGate <$> ns

mkLevels :: Int -> [[Int]]
mkLevels n
    | n < 2 = []
    | otherwise = mkLevels' <$> [0..(n - 1)]
        where
            mkLevels' m = (replicate ((n - 1) - m) 0) ++ (replicate m 1)
    
-- Find those Nodes whose only incoming link is themselves. 
soleSelfLoops :: ModelGraph -> [Gr.Node]
soleSelfLoops mG = onlys
    where
        onlys = filter (onlyMe mG) nodes
        nodes = Gr.nodes mG
        onlyMe g n = Gr.pre g n == [n]

-- Re-order a LayerVec according to a new List of NodeNames. Basic error
-- checking, but use at your own risk. 
layerVecReorder :: LayerNameIndexMap
                -> [NodeName]
                -> LayerVec
                -> Either T.Text LayerVec
layerVecReorder lniMap newOrder lVec
    | (L.sort . M.keys) lniMap /= L.sort newOrder =
        Left "NewOldOrderingMismatch"
    | M.size lniMap /= U.length lVec = Left "OldOrderingLayerVecMismatch"
    | otherwise = Right reorderedVec
        where
             permuteVec = U.fromList $ (lniMap M.!) <$> newOrder
             reorderedVec = U.backpermute lVec permuteVec

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

-- Input graphs:
-- 
--  ┌───┐                                           
--  │   │                                           
--  ▼   │                                           
--  .   │                                           
-- ( )──┘                                           
--  '                                               
--  │                                               
--  │                                               
--  │                                               
--  │            ┌───┐                              
--  │            │   │                              
--  │            │   │                              
--  │            .   │                              
--  ├──────────▶( )◀─┘                              
--  │            '                                  
--  │            │                                  
--  │            │                                  
--  │            │                                  
--  │            │                ┌───┐             
--  │            │                │   │             
--  │            │                │   │             
--  │            │                .   │             
--  ├────────────┼──────────────▶( )◀─┘             
--  │            │                '                 
--  │            │                │                 
--  │            │                │                 
--  │            │                │                 
--  │            │                │            ┌───┐
--  │            │                │            │   │
--  │            │                │            │   │
--  │            │                │            .   │
--  └────────────┴────────────────┴──────────▶( )◀─┘
--                                             '    


