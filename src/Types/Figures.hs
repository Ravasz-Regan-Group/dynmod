{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Figures
    ( PUCGradient
    , SVGText
    , InputBundle(..)
    , Barcode
    , BarcodeFilter(..)
    , Bar(..)
    , BarKind(..)
    , Slice(..)
    , mkBarcode
    , ColorMap
    , mkColorMap
    , gradientPick
    , attMatch
    , bcFilterF
    , barPhenotype
    , StdDev
    ) where    

import Types.DMModel
import Types.Simulation
import Utilities
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.List as L
import Data.Maybe (isNothing, catMaybes)


-- Basic types to support all types of figures.

type SVGText = T.Text
type ColorMap = M.HashMap NodeName LocalColor
type PUCGradient = B.Vector LocalColor
type StdDev = Double

mkColorMap :: DMModel -> ColorMap
mkColorMap dmm = M.fromList nameColorPairs
    where
        nameColorPairs = (\n -> (nodeName n, nodeColor n)) <$> nodesMetas
        nodesMetas = nodeMeta <$> ((concat . modelNodes) dmm)


-- Pick a color from one of the perceptually uniform color gradients in
-- Constants. 
gradientPick :: RealFrac a => PUCGradient
                             -> (a, a)
                             -> a
                             -> Maybe LocalColor
gradientPick pucGr (low, high) pick
    | low >= high = Nothing
    | pick < low = Nothing
    | pick > high = Nothing
    | otherwise = Just $ pucGr B.! pickIndex
    where
        pickIndex = round (pick * transform)
        transform = (gradientSize - 1) / magnitude -- Avoid off-by-one error
        magnitude = high - low
        gradientSize = (fromIntegral . B.length) pucGr



type Barcode = [Bar]
data Bar = BR { barKind :: BarKind
              , attractorSize :: Int
              , switchName :: NodeName
              , phenotypeNames :: [PhenotypeName]
              } deriving (Eq, Show, Generic)
instance Hashable Bar

-- Types to construct 5D diagrams with BarCodeClusters.


-- For a given Phenotype, where (if anywhere) do the SubSpaces of the Phenotype
-- match to state(s) of the resident global Attractor. Both Phenotypes and
-- Attractors can be points or loops, so the matching deserves some comment.
-- A Phenotype which maps, in its entirety, onto a subset of the Attractor is a
-- match, for those states of the attractor where the SubSpaces line up with the
-- it. A Phenotype whose length is longer than the resident Attractor, or
-- whose SubSpaces match out of order, or none of whose SubSpaces match at all,
-- is a Miss. If some, but not all, of a Phenotype's Subspaces match IN ORDER,
-- then that Phenotype is in the running to be the titular red line in a
-- RedLineBar. 
data BarKind = FullMiss BarHeight
             | RedLineBar BarHeight PhenotypeIndex PhenotypeName -- One
-- Phenotype in the Switch is clearly the least bad match to the resident global
-- Attractor (an n-way tie prevents RedLineBars)
             | MatchBar [Slice]
                        LocalColor
             deriving (Eq, Show, Generic)
instance Hashable BarKind
type BarHeight = Int
type PhenotypeIndex = Int


data Slice = Match AttractorSize AttractorMatchIndices PhenotypeName
           | Miss AttractorSize
           deriving (Eq, Show, Generic)
instance Hashable Slice
type AttractorSize = Int
type AttractorMatchIndices = [[Int]]

-- The 1-5 Environments that will form the axes of environmental diagrams,
-- possibly in an order specified in an ISFSpec, any remaining inputs fixed to
-- particular values to prevent the diagram from containing many extraneous
-- attractors, and maybe a BarcodeFilter. 
data InputBundle = InputBundle { ibInputs :: [[DMNode]]
                               , ibFixedVec :: FixedVec
                               , ibBCFilter :: Maybe BarcodeFilter
                               } deriving (Eq, Show)

data SliceCandidate = MissCandidate AttractorSize
                    | RedLineCandidate BarHeight MatchCount PhenotypeName
                    | MatchCandidate AttractorSize
                                     AttractorMatchIndices
                                     PhenotypeName
                    deriving (Eq, Show)
type MatchCount = Int

-- Sometimes input space figures have many Barcodes at each input coordinate. 
-- Barcode filters allow the user to exclude irrelevant Barcodes, or only
-- include particularly relevant Barcodes. 
data BarcodeFilter =
-- Only include if at ANY point along an attractor it matches to the phenotype.
      OnlyBarCodesWithAny [(NodeName, PhenotypeName)]
-- Only include if at EVERY point along an attractor it matches to the
-- phenotype. 
    | OnlyBarCodesWithAll [(NodeName, PhenotypeName)]
-- Exclude if at ANY point along an attractor it matches to the phenotype. 
    | ExcludeBarCodesWithAny [(NodeName, PhenotypeName)]
-- Exclude if at EVERY point along an attractor it matches to the phenotype
    | ExcludeBarCodesWithAll [(NodeName, PhenotypeName)]
      deriving (Eq, Show, Ord)

-- Does an attractor exist at a particular point in the space of environmental
-- inputs? Using any to reject Attractors which do not match a bit faster, since
-- it will stop when it finds a gate which does not match. 
attMatch :: FixedVec -> Attractor -> Bool
attMatch fVec att = not $ U.any (checkV att) fVec
    where
        checkV anAtt (nIndex, nState) = ((B.head anAtt) U.! nIndex) /= nState

-- Generate a Barcode to represent Attractors on environment-space figures.
-- Assumes the ColorMap order matches that of the Attractor. Return the
-- Attractor as well, because we will often want to filter Attractors by the
-- properties of their associated Barcodes. 
mkBarcode :: ColorMap
          -> ModelMapping
          -> LayerNameIndexBimap
          -> Attractor
          -> (Barcode, Attractor)
mkBarcode cM mM lniBMap att = (bc, att)
    where
        bc = (uncurry (mkBar lniBMap att)) <$> colorSwitchPairs
        colorSwitchPairs = (\(sN, ps) -> (cM M.! sN, (sN, ps))) <$> nameSPairs
        nameSPairs = (\(nName, (_, phs)) -> (nName, phs)) <$> nonEmptyPhs
        nonEmptyPhs = filter ((/= []) . snd . snd) mM


-- Make a single Bar in a Barcode. 
mkBar :: LayerNameIndexBimap
      -> Attractor
      -> LocalColor
      -> (NodeName, [Phenotype])
      -> Bar
mkBar lniBMap att sColor (sName, phs)
    | areMatches =
        BR (MatchBar sweptSlices sColor)
            attSize
            sName
            phNames
    | otherwise  = case foldr redLinePrune (Nothing, 0, (-1)) sCandidates of
        (Just rlb, _, _) ->
            BR rlb attSize sName phNames
        (Nothing, _, _)  ->
            BR (FullMiss (length sCandidates)) attSize sName phNames
    where
        (sweptSlices, areMatches) =
            foldr (matchSweep attSize) ([], False) sCandidates
        sCandidates = (mkSliceCandidate lniBMap att) <$> orderedPHs
--      We order the phenotypes by switchNodeState descending so the the Bar
--      will have the 0 state at the bottom, rather than the top. 
        orderedPHs = (L.reverse . L.sortOn switchNodeState) phs
        attSize = B.length att
        phNames = phenotypeName <$> phs

bcFilterF :: Maybe BarcodeFilter -> Barcode -> Bool
bcFilterF Nothing _ = True
bcFilterF (Just (OnlyBarCodesWithAny sPhPairs)) bc =
    all (phCheckAny (bcPairs bc)) sPhPairs
bcFilterF (Just (OnlyBarCodesWithAll sPhPairs)) bc =
    all (phCheckAll (bcPairs bc)) sPhPairs
bcFilterF (Just (ExcludeBarCodesWithAny sPhPairs)) bc = not $
    all (phCheckAny (bcPairs bc)) sPhPairs
bcFilterF (Just (ExcludeBarCodesWithAll sPhPairs)) bc = not $
    all (phCheckAll (bcPairs bc)) sPhPairs

bcPairs :: Barcode -> [(NodeName, BarKind)]
bcPairs = fmap (\x -> (switchName x, barKind x))

phCheckAny :: [(NodeName, BarKind)] -> (NodeName, PhenotypeName) -> Bool
phCheckAny bcPs (nName, phName) = case L.find ((==) nName . fst) bcPs of
    Nothing -> False
    Just (_, FullMiss _) -> False
    Just (_, RedLineBar _ _ checkedPhN) -> checkedPhN == phName
    Just (_, MatchBar slcs _) -> case L.find (matchSlice phName) slcs of
        Nothing -> False
        Just (Match _ _ _) -> True
        Just (Miss _) -> False

phCheckAll :: [(NodeName, BarKind)] -> (NodeName, PhenotypeName) -> Bool
phCheckAll bcPs (nName, phName) = case L.find ((==) nName . fst) bcPs of
    Nothing -> False
    Just (_, FullMiss _) -> False
    Just (_, RedLineBar _ _ checkedPhN) -> checkedPhN == phName
    Just (_, MatchBar slcs _) -> case L.find (matchSlice phName) slcs of
        Nothing -> False
        Just (Match attSize attMatchess _) ->
            attSize == (sum . fmap length) attMatchess
        Just (Miss _) -> False

matchSlice :: PhenotypeName
           -> Slice
           -> Bool
matchSlice _ (Miss _) = False
matchSlice phName (Match _ _ checkedPhN) =  phName == checkedPhN

barPhenotype :: Bar -> Maybe PhenotypeName
barPhenotype br = case barKind br of
    FullMiss _ -> Nothing
    RedLineBar _ _ phName -> Just phName
    MatchBar slcs _ -> foldr bestSl Nothing slcs
        where
            bestSl sl bestS = case sl of
                Miss _ -> bestS
                Match attSize attMatchess phName
                    | attSize ==  (sum . fmap length) attMatchess -> Just phName
                    | otherwise -> bestS

-- Scan a [SliceCandidate] for good matches:
matchSweep :: Int
           -> SliceCandidate
           -> ([Slice], Bool)
           -> ([Slice], Bool)
matchSweep _ (MissCandidate attSize) (slcs, areMs) =
    ((Miss attSize):slcs, areMs)
matchSweep attSize (RedLineCandidate _ _ _) (slcs, areMs) =
    ((Miss attSize):slcs, areMs)
matchSweep _ (MatchCandidate i jss phName) (slcs, _) =
    ((Match i jss phName):slcs, True)


-- Scan a [SliceCandidate] for a RedLineBar:
redLinePrune :: SliceCandidate
             -> (Maybe BarKind, Int, Int)
             -> (Maybe BarKind, Int, Int)
redLinePrune (MissCandidate _) (mRLB, highestRS, phIndex) =
    (mRLB, highestRS, phIndex + 1)
redLinePrune (MatchCandidate _ _ _) (mRLB, highestRS, phIndex) =
    (mRLB, highestRS, phIndex + 1)
redLinePrune (RedLineCandidate phSize i phName) (mRLB, highestRS, phIndex)
    | i < highestRS = (mRLB, highestRS, phIndex + 1)
    | i == highestRS = (Nothing, highestRS, phIndex + 1)
    | otherwise = (Just (RedLineBar phSize phIndex phName), i, phIndex + 1)

-- Make a single SliceCandidate. 
mkSliceCandidate :: LayerNameIndexBimap
                 -> Attractor
                 -> Phenotype
                 -> SliceCandidate
mkSliceCandidate lniBMap att ph
    | fPrintSize > attSize = MissCandidate attSize
    | otherwise  = case anyMatchReorder intPh att of
        Nothing -> MissCandidate attSize
        Just (ordIntPh, ordAtt, attOffset)
            | not $ isStepIncreasing matchInts -> MissCandidate attSize
            | any isNothing matches ->
                RedLineCandidate rlcCount (fPrintSize - 1) phName
            | otherwise ->
                MatchCandidate attSize rightOrderedLoops phName
            where
                rightOrderedLoops = (\i -> (attOffset + i) `rem` attSize) <<$>>
                    allLoops
                allLoops = matchInts:extraLoops
                extraLoops = loopCheck ordIntPh ordAtt lastMatchIndex
                lastMatchIndex = last matchInts
                rlcCount = length matchInts
                matchInts = catMaybes matches
                matches = matchLocation ordAtt <$> ordIntPh
    where
        intPh = f <<$>> fPrint
            where f (x, y) = (lniBMap BM.! x, y)
--         (BF.first (lniBMap BM.!)) <<$>> fPrint
        fPrintSize = length fPrint
        phName = phenotypeName ph
        fPrint = fingerprint ph
        attSize = B.length att

-- Do any of the Int-converted SubSpaces in the Phenotype match to any
-- state in the Attractor? If so, reorder both Phenotype and Attractor at the
-- first Phenotype SubSpace that matches any Attractor state. Return them along
-- with the index offset for the Attractor, so as to be able to construct a
-- properly indexed Match. 
anyMatchReorder :: [IntSubSpace]
                -> Attractor
                -> Maybe ([IntSubSpace], Attractor, Int)
anyMatchReorder intPh att
    | B.null frontThread = Nothing
    | otherwise = Just (newSS, newThread, attOffset)
    where
        (backThread, frontThread) = B.break (isAttMatch intPh) att
        isAttMatch iph attLVec = any (isSSMatch attLVec) iph
        newSS = frontSS <> backSS
        newThread = frontThread <> backThread
        attOffset = B.length backThread
        (backSS, frontSS) = L.break (isSSMatch (B.head frontThread)) intPh

