{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Figures
    ( attractorGrid
    , rnGrid
    , attractorHMSVG
    , attractorESpaceFigure
    , SVGText
    , InputBundle
    ) where

import Types.DMModel
import Types.Simulation
import Properties.Attractors
import Utilities
import Plots
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Svg.Core (renderText, Element)
import qualified Data.List as L
import qualified Data.Bifunctor as BF
import Control.Monad.Reader (Reader, runReader, ask)
import Data.Maybe (isNothing, catMaybes)

type SVGText = T.Text
type AttractorIndex = Int


type ColorMap = M.HashMap NodeName LocalColor

-- Types to construct 5D diagrams with BarCodeClusters.
-- The 1-5 Environments that will form the axes of environmental diagrams, plus
-- any remaining inputs fixed to particular values, to prevent the diagram from
-- containing many extraneous attractors. 
type InputBundle = ([[DMNode]], FixedVec)
type FigureClusters = [BarcodeCluster]
type BarcodeCluster = [Barcode]
type Barcode = [Bar]
data Bar = BR { barKind :: BarKind
              , attractorSize :: Int
              } deriving (Eq, Show)

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
             | RedLineBar BarHeight PhenotypeIndex -- One Phenotype in the
-- Switch is clearly the least bad match to the resident global Attractor
-- (an n-way tie prevents RedLineBars)
             | MatchBar [Slice] LocalColor
             deriving (Eq, Show)
type BarHeight = Int
type PhenotypeIndex = Int


data Slice = Match AttractorSize AttractorMatchIndices
           | Miss
           deriving (Eq, Show)
type AttractorSize = Int
type AttractorMatchIndices = [[Int]]

data SliceCandidate = MissCandidate
                    | RedLineCandidate BarHeight MatchCount
                    | MatchCandidate AttractorSize AttractorMatchIndices
                    deriving (Eq, Show)
type MatchCount = Int

-- Here we actively misinterpret a ModelEnv as specifying random state slots and
-- noisy step slots instead of a single run with those numbers, but the data is
-- there and a ModelEnvs is a convenient package. 
attractorGrid :: ModelEnv -> Int -> Simulation [[HS.HashSet Attractor]]
attractorGrid (ModelEnv mL rN nProb nN _ _) multiplier =
    (traverse . traverse) attractors (mEnvGrid rN nN multiplier nProb mL)

mEnvGrid :: Int -> Int -> Int -> Double -> ModelLayer -> [[ModelEnv]]
mEnvGrid rN nN multiplier nProb mL = mkRow <$> [1..rN]
    where
        mkRow m = ((mkMEnv (multiplier * m)) . (multiplier *)) <$> [1..nN]
        mkMEnv i j = ModelEnv mL i nProb j 0 []


heatMapAxis :: [[Double]] -> Int -> Axis B V2 Double
heatMapAxis atts mult = r2Axis &~ do
  display colourBar
  axisExtend .= noExtend
  axisColourMap .= Plots.viridis
  xLabel .= "Noisy Steps"
  yLabel .= "r_States"
  heatMap atts $ heatMapSize .= V2 (fromIntegral mult) (fromIntegral mult)

heatMapDia :: [[Double]] -> Int -> QDiagram B V2 Double Any
heatMapDia ass mult = renderAxis $ heatMapAxis ass mult

attractorHMElement :: [[Double]] -> Int -> Element
attractorHMElement ass mult = renderDia SVG
                                   (SVGOptions (mkWidth 800)
                                   Nothing
                                   ""
                                   []
                                   True
                                   )
                                   (heatMapDia ass mult)

attractorHMSVG :: [[Double]] -> Int -> SVGText
attractorHMSVG ass mult = LT.toStrict $ renderText $
    attractorHMElement ass mult


rnGrid :: Int -> Int -> Int -> [[(Double, Double)]]
rnGrid r n mult = mkRow <$> [1..rD]
    where
        mkRow m = (\p -> (multD * m, multD * p)) <$> [1..nD]
        rD = fromIntegral r
        nD = fromIntegral n
        multD = fromIntegral mult

------------------------------------------------------------------------------

-- Create an up to 5-D figure to check how well attractors behave under changes
-- in inputs, as well as how well Phenotypes match up to derived Attractors. 

-- If there are more than 5 inputs, those inputs that are not in the diagram
-- will be have been pinned by the user. 


attractorESpaceFigure :: DMModel -> AttractorBundle -> InputBundle -> SVGText
attractorESpaceFigure dmModel aBundle iBundle = undefined
--     where
--         netInputCombos = (fixedINodeFixVec <>) <$> freeINodeCombos
--         figOrdFreeINodeCombos = 
--         freeINodeCombos = inputCombos freeINodes [] lniBMap
--         (dmmsMMap, lniBMap, atts) = aBundle
-- --   The order that an input is displayed in a figure is big-endian,
-- --   rather than the small-endian storage- and inputCombo-order. 
--         figOrdFreeINode = L.reverse <$> freeINodes
--         (freeINodes, fixedINodeFixVec) = iBundle
    
figureClusters :: Reader (DMModel, AttractorBundle, InputBundle)
                         [BarcodeCluster]
figureClusters = undefined

barcodeCluster :: Reader (DMModel, AttractorBundle, InputBundle)
                         BarcodeCluster
barcodeCluster = undefined


               

-- -- Generate barcodes for all the attractors
-- barcodes :: Reader (DMModel, AttractorBundle, InputBundle) [Barcode]
-- barcodes


mkColorMap :: DMModel -> ColorMap
mkColorMap dmm = M.fromList nameColorPairs
    where
        nameColorPairs = (\n -> (nodeName n, nodeColor n)) <$> nodesMetas
        nodesMetas = nodeMeta <$> ((concat . modelNodes) dmm)


-- inputCheck :: Reader (DMModel, AttractorBundle, InputBundle)
--                      (Validation [InputBundleInvalid] InputBundle) 
-- inputCheck = do
--     (dmModel, _, (runningInputs, fixedInputs)) <- ask
--     let dmmsMMap = (fst . modelMappingSplit . last . modelMappings) dmModel
--         mL = fineLayer dmModel
--         inPts = (inputs . modelGraph) mL

-- Generate a Barcode to represent Attractors on environment-space figures.
-- Assumes the ColorMap order matches that of the Attractor. 
mkBarcode :: ColorMap
          -> ModelMapping
          -> LayerNameIndexBimap
          -> Attractor
          -> Barcode
mkBarcode cM mM lniBMap att = (uncurry (mkBar lniBMap att)) <$>
    colorSwitchPairs
    where
        colorSwitchPairs = ((cM M.!) `BF.first`) <$> nameSwitchPairs
        nameSwitchPairs = (\(nName, (_, phs)) -> (nName, phs)) <$> mM


-- Make a single Bar in a Barcode. 
mkBar :: LayerNameIndexBimap -> Attractor -> LocalColor -> [Phenotype] -> Bar
mkBar lniBMap att sColor phs
    | areMatches = BR (MatchBar sweptSlices sColor) attSize
    | otherwise  = case foldr redLinePrune (Nothing, 0, 0) sCandidates of
        (Just rlb, _, _) -> BR rlb attSize
        (Nothing, _, _)  -> BR (FullMiss (length sCandidates)) attSize
    where
        (sweptSlices, areMatches) = foldr matchSweep ([], False) sCandidates
        sCandidates = (mkSliceCandidate lniBMap att) <$> orderedPHs
--      We order the phenotypes by switchNodeState descending so the the Bar
--      will have the 0 state at the bottom, rather than the top. 
        orderedPHs = (L.reverse . L.sortOn switchNodeState) phs
        attSize = B.length att

-- Scan a [SliceCandidate] for good matches:
matchSweep :: SliceCandidate -> ([Slice], Bool) -> ([Slice], Bool)
matchSweep MissCandidate (slcs, areMs) = (Miss:slcs, areMs)
matchSweep (RedLineCandidate _ _) (slcs, areMs) = (Miss:slcs, areMs)
matchSweep (MatchCandidate i j) (slcs, _) = ((Match i j):slcs, True)


-- Scan a [SliceCandidate] for a RedLineBar:
redLinePrune :: SliceCandidate
             -> ((Maybe BarKind), Int, Int)
             -> ((Maybe BarKind), Int, Int)
redLinePrune MissCandidate (mRLB, highestRS, phIndex) =
    (mRLB, highestRS, phIndex + 1)
redLinePrune (MatchCandidate _ _) (mRLB, highestRS, phIndex) =
    (mRLB, highestRS, phIndex + 1)
redLinePrune (RedLineCandidate phSize i) (mRLB, highestRS, phIndex)
    | i < highestRS = (mRLB, highestRS, phIndex + 1)
    | i == highestRS = (Nothing, highestRS, phIndex + 1)
    | i > highestRS = (Just (RedLineBar phSize phIndex), i, phIndex + 1)

-- Make a single SliceCandidate. 
mkSliceCandidate :: LayerNameIndexBimap
                 -> Attractor
                 -> Phenotype
                 -> SliceCandidate
mkSliceCandidate lniBMap att ph
    | fPrintSize > attSize = MissCandidate
    | otherwise  = case anyMatchReorder intPh att of
        Nothing -> MissCandidate
        Just (ordIntPh@(_:remainingOrderedIntPh), ordAtt, attOffset)
            | not $ isStrictlyIncreasing oMatchInts -> MissCandidate
            | any isNothing otherMatches -> RedLineCandidate rlcCount fPrintSize
            | otherwise -> MatchCandidate attSize rightOrderedLoops
            where
                rightOrderedLoops = (\i -> (attOffset + i) `rem` attSize) <<$>>
                    allLoops
                allLoops = (0:oMatchInts):extraLoops
                extraLoops = loopCheck ordIntPh ordAtt lastMatchIndex
                lastMatchIndex = last oMatchInts
                rlcCount = length oMatchInts + 1
                oMatchInts = catMaybes otherMatches
                otherMatches = matchLocation ordAtt <$> remainingOrderedIntPh
    where
        intPh = (BF.first (lniBMap BM.!)) <<$>> fPrint
        fPrintSize = length fPrint
        fPrint = fingerprint ph
        attSize = B.length att

-- Do any of the Int-converted SubSpaces in the Phenotype have match to any
-- state in theAttractor? If so, reorder both Phenotype and Attractor at that
-- match. Return them along with the index offset for the Attractor, so as to be
-- able to construct a properly indexed Match. 
anyMatchReorder :: [IntSubSpace]
                -> Attractor
                -> Maybe ([IntSubSpace], Attractor, Int)
anyMatchReorder intPh att = go (head intPh) (drop 1 intPh) ([],intPh)
    where
        go sS (nsS:sSs) (backSS, frontSS)
            | B.null frontThread = go nsS sSs ([sS], nsS:sSs)
            | otherwise = Just (newSS, newThread, attOffset)
                where
                    (backThread, frontThread) = B.break (isSSMatch sS) att
                    newSS     = frontSS <> backSS
                    newThread = frontThread <> backThread
                    attOffset = B.length backThread
        go sS [] (backSS, frontSS)
            | B.null frontThread = Nothing
            | otherwise = Just (newSS, newThread, attOffset)
                where
                    (backThread, frontThread) = B.break (isSSMatch sS) att
                    newSS     = frontSS <> backSS
                    newThread = frontThread <> backThread
                    attOffset = B.length backThread

-- Do the states in the Int-converted Subspace match the equivalent states in
-- the LayerVec
isSSMatch :: IntSubSpace -> LayerVec -> Bool
isSSMatch sS lV = all ((\v (i, s) -> s == v U.! i) lV) sS


-- Find the location of the first place in the Attractor, if any, that the Int-
-- converted SubSpace matches.
matchLocation :: Attractor -> IntSubSpace -> Maybe AttractorIndex
matchLocation att sS = B.findIndex (isSSMatch sS) att

-- Given an [IntSubSpace] that we know matches completely onto the given
-- Attractor, does that [IntSubSpace] completely match an integer number of
-- additional times? Returns lists of NodeIndices of the Attractor. 
loopCheck :: [IntSubSpace] -> Attractor -> AttractorIndex -> [[AttractorIndex]]
loopCheck sSs att lastIndex = go croppedAtt [[]] (lastIndex + 1)
    where
        croppedAtt = B.drop (lastIndex + 1) att
        go anAtt matches indexOffset
            | length sSs > B.length anAtt = matches
            | not $ isStrictlyIncreasing oMatchInts = matches
            | any isNothing otherMatches = matches
            | otherwise = go newCroppedAtt newMatches newIndexOffset
            where
                newCroppedAtt = B.drop (newLastIndex + 1) anAtt 
                newMatches = matches <> [((indexOffset +) <$> oMatchInts)]
                newIndexOffset = indexOffset + newLastIndex + 1
                newLastIndex = last oMatchInts
                oMatchInts = catMaybes otherMatches
                otherMatches = matchLocation croppedAtt <$> sSs
