{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}

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
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Svg.Core (renderText, Element)
import qualified Graphics.SVGFonts as F
import qualified Data.List.Split as Split
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import qualified Data.List as L
import qualified Data.Bifunctor as BF
import Data.Maybe (isNothing, catMaybes)

type SVGText = T.Text
type AttractorIndex = Int


type ColorMap = M.HashMap NodeName LocalColor

-- Types to construct 5D diagrams with BarCodeClusters.
-- The 1-5 Environments that will form the axes of environmental diagrams, plus
-- any remaining inputs fixed to particular values, to prevent the diagram from
-- containing many extraneous attractors. 
type InputBundle = ([[DMNode]], FixedVec)
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
           | Miss AttractorSize
           deriving (Eq, Show)
type AttractorSize = Int
type AttractorMatchIndices = [[Int]]

data SliceCandidate = MissCandidate AttractorSize
                    | RedLineCandidate BarHeight MatchCount
                    | MatchCandidate AttractorSize AttractorMatchIndices
                    deriving (Eq, Show)
type MatchCount = Int

type ESpacePointDia = Diagram B
type BarcodeDia = Diagram B


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
                                   (SVGOptions (mkWidth 1600)
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
-- will be have been pinned by the user. If there are none, the user will have
-- been notified. 

attractorESpaceFigure :: DMModel -> AttractorBundle -> InputBundle -> SVGText
attractorESpaceFigure dmModel aBundle (freeINodes, fixedINodeFixVec) =
    eSpaceRenderText $ frame 3.0 $ figureDia
    where
        figureDia
            | L.length dimList <= 3 = allLines5DFigure dimList 
                                                       eSpacePointDias
                                                       $ eSpaceNames freeINodes
            | otherwise = someSpaces5DFigure dimList
                                             eSpacePointDias
                                             $ eSpaceNames freeINodes
        eSpacePointDias = eSpacePointDia <$> barcodeClusters
        barcodeClusters = ((barcodeCluster dmModel lniBMap) . HS.toList) <$>
                                                            attClusters
        attClusters = attPartition atts <$> netInputCombos
        netInputCombos = (fixedINodeFixVec <>) <$> figOrdFINodeCs
--   The order that an input is displayed in a figure is big-endian,
--   rather than the small-endian storage- and inputCombo-order. Each
--   [FixedVec] produced by inputLevels must be reversed, before we
--   concatenate them. Thus, we cannot use inputCombos directly. 
        figOrdFINodeCs = levelReorder naiveLevels
        levelReorder = (((U.concat . L.reverse) <$>) . sequenceA . reverse)
        dimList = ((+(-1)) . L.length) <$> naiveLevels
        naiveLevels = inputLevels lniBMap <$> freeINodes
        (_, lniBMap, atts) = aBundle

allLines5DFigure :: [Int] -> [Diagram B] -> [[NodeName]] -> Diagram B
allLines5DFigure dimList clusters iNames = dFigure <> axisLabels
    where
        dFigure = foldr folder placedClusters allGridEdges
        folder (x, y) nCl = nCl # connectOutside' aStyle x y
        allGridEdges = mkGridEdges dimList
        placedClusters = mconcat $ zipWith place namedClusters ptList
--         Apparently one must name a Diagram, and then place it, or the
--         movement doesn't take? 
        namedClusters = zipWith nameZip nameList clusters
            where nameZip pName p = p # named pName
        nameList = mkPointNames dimList
        axisLabels = mkLabels iNames unitScale
        ptList = mkPoints dimList unitScale
        unitScale = 1.5 * ((maximum . (fmap width)) clusters)
        aStyle = with & arrowHead .~ noHead

someSpaces5DFigure :: [Int] -> [Diagram B] -> [[NodeName]] -> Diagram B
someSpaces5DFigure dimList clusters iNames = mconcat $ axisLabels:gridDBs
    where
        gridDBs :: [Diagram B]
        gridDBs = flip (foldr addEdge) allGridEdges <$> placedClusterChunks
        addEdge (x, y) nCl = nCl # connectOutside' aStyle x y
        allGridEdges = mkGridEdges $ take 3 dimList
        placedClusterChunks :: [Diagram B]
        placedClusterChunks = (mconcat . (uncurry (zipWith place))) <$>
            (zip namedClustersChunks ptChunks)
        ptChunks :: [[P2 Double]]
        ptChunks = Split.chunksOf threeGS ptList
        namedClustersChunks :: [[Diagram B]]
        namedClustersChunks = zipWith nameZip nameList <$>clusterChunks
            where
                nameZip pName p = p # named pName
        clusterChunks :: [[Diagram B]]
        clusterChunks = Split.chunksOf threeGS clusters
        threeGS = threeChunkS dimList
        nameList = mkPointNames $ take 3 dimList
        axisLabels = mkLabels iNames unitScale
        ptList = mkPoints dimList unitScale
        unitScale = 1.5 * ((maximum . (fmap width)) clusters)
        aStyle = with & arrowHead .~ noHead
                      & shaftStyle %~ lw ultraThin

threeChunkS :: [Int] -> Int
threeChunkS ds = (xi + 1) * (yi + 1) * (zi + 1)
    where
        [xi, yi, zi] = take 3 ds


-- Consume a HS.HashSet Attractor and a FixedVec that represents a point in the
-- environmental space, and produce a [Attractor] of all the attractors which
-- exist there. 
attPartition ::  HS.HashSet Attractor -> FixedVec -> HS.HashSet Attractor
attPartition atts fVec = HS.filter (attMatch fVec) atts

-- Does an attractor exist at a particular point in the space of environmental
-- inputs? Using any to reject Attractors which do not match a bit faster, since
-- it will stop when it finds a gate which does not match. 
attMatch :: FixedVec -> Attractor -> Bool
attMatch fVec att = not $ U.any (checkV att) fVec
    where
        checkV anAtt (nIndex, nState) = ((B.head anAtt) U.! nIndex) /= nState


-- Make the NodeNames that will go on diagram axes. The reverse gives the
-- correct order for display in the figure. 
eSpaceNames :: [[DMNode]] -> [[T.Text]]
eSpaceNames nns = (eSpaceName . reverse) <$> nns
    where
        eSpaceName :: [DMNode] -> [T.Text]
        eSpaceName [] = error "Empty environmental input."
        eSpaceName [n] = L.unfoldr nOpts 0
            where
                nOpts i
                    | (i == 0) && (nRange == 1) = Just (nName <> " off", i + 1)
                    | (i == 1) && (nRange == 1) = Just (nName <> " on", i + 1)
                    | (i <= nRange) = Just (iLine, i + 1)
                    |otherwise = Nothing
                    where
                        iLine = nName <> ":" <> ((T.pack . show) i)
                (nName, nRange) = nodeRange n
        eSpaceName ns = [headName <> " off"] <> [headName <> " on"] <>
                                ((nodeName . nodeMeta) <$> (tail ns))
            where
                headName = (nodeName . nodeMeta . head) ns


barcodeCluster :: DMModel -> LayerNameIndexBimap -> [Attractor] -> [Barcode]
barcodeCluster dmModel lniBMap atts = mkBarcode cMap mMap lniBMap <$> atts
    where
        cMap = mkColorMap dmModel
        mMap = (last . modelMappings) dmModel


mkColorMap :: DMModel -> ColorMap
mkColorMap dmm = M.fromList nameColorPairs
    where
        nameColorPairs = (\n -> (nodeName n, nodeColor n)) <$> nodesMetas
        nodesMetas = nodeMeta <$> ((concat . modelNodes) dmm)



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
        (sweptSlices, areMatches) =
            foldr (matchSweep attSize) ([], False) sCandidates
        sCandidates = (mkSliceCandidate lniBMap att) <$> orderedPHs
--      We order the phenotypes by switchNodeState descending so the the Bar
--      will have the 0 state at the bottom, rather than the top. 
        orderedPHs = (L.reverse . L.sortOn switchNodeState) phs
        attSize = B.length att

-- Scan a [SliceCandidate] for good matches:
matchSweep :: Int -> SliceCandidate -> ([Slice], Bool) -> ([Slice], Bool)
matchSweep _ (MissCandidate attSize) (slcs, areMs) =
    ((Miss attSize):slcs, areMs)
matchSweep attSize (RedLineCandidate _ _) (slcs, areMs) =
    ((Miss attSize):slcs, areMs)
matchSweep _ (MatchCandidate i j) (slcs, _) = ((Match i j):slcs, True)


-- Scan a [SliceCandidate] for a RedLineBar:
redLinePrune :: SliceCandidate
             -> ((Maybe BarKind), Int, Int)
             -> ((Maybe BarKind), Int, Int)
redLinePrune (MissCandidate _) (mRLB, highestRS, phIndex) =
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
    | fPrintSize > attSize = MissCandidate attSize
    | otherwise  = case anyMatchReorder intPh att of
        Nothing -> MissCandidate attSize
        Just (ordIntPh, ordAtt, attOffset)
            | not $ isStrictlyIncreasing matchInts -> MissCandidate attSize
            | any isNothing matches -> RedLineCandidate rlcCount fPrintSize
            | otherwise -> MatchCandidate attSize rightOrderedLoops
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
        intPh = (BF.first (lniBMap BM.!)) <<$>> fPrint
        fPrintSize = length fPrint
        fPrint = fingerprint ph
        attSize = B.length att

-- Do any of the Int-converted SubSpaces in the Phenotype match to any
-- state in theAttractor? If so, reorder both Phenotype and Attractor at that
-- match. Return them along with the index offset for the Attractor, so as to be
-- able to construct a properly indexed Match. 
anyMatchReorder :: [IntSubSpace]
                -> Attractor
                -> Maybe ([IntSubSpace], Attractor, Int)
anyMatchReorder intPh att = go (head intPh) (tail intPh) ([],intPh)
    where
        go sS (nsS:sSs) (backSS, frontSS)
            | B.null frontThread = go nsS sSs (backSS <> [sS], nsS:sSs)
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
isSSMatch sS lV = all (isStateMatch lV) sS
    where
        isStateMatch lVec (nodeNameInt, nState) = nState == lVec U.! nodeNameInt


-- Find the location of the first place in the Attractor, if any, that the Int-
-- converted SubSpace matches.
matchLocation :: Attractor -> IntSubSpace -> Maybe AttractorIndex
matchLocation att sS = B.findIndex (isSSMatch sS) att

-- Given an [IntSubSpace] that we know matches completely onto the given
-- Attractor, does that [IntSubSpace] completely match an integer number of
-- additional times? Returns lists of NodeIndices of the Attractor. 
loopCheck :: [IntSubSpace] -> Attractor -> AttractorIndex -> [[AttractorIndex]]
loopCheck sSs att lastIndex = go croppedAtt [] (lastIndex + 1)
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
                otherMatches = matchLocation anAtt <$> sSs

------------------------------------------------------------------------------
-- Diagrams functions

-- The scaling factor for widening bars in 5D figures. 
alpha :: Double
alpha = 0.8

eSpacePointDia :: [Barcode] -> ESpacePointDia
eSpacePointDia bCodes = eSpacePointDia' $ barcodeDia <$> bCodes
    where
        eSpacePointDia' :: [BarcodeDia] -> ESpacePointDia
        eSpacePointDia' [] = mempty
        eSpacePointDia' [bDia] = bDia
        eSpacePointDia' bCodeDias = bRect <> bSquare
            where
                bWidth = (((maximum . (fmap width)) bCodeDias) :: Double)
                totalLength = bWidth * ((fromIntegral . length) bCodeDias)
                squareSideL = sqrt totalLength
                rowSize = ceiling (squareSideL / bWidth)
                bRows = Split.chunksOf rowSize bCodeDias
                bSquare = (vsep 1.0 (fmap (hsep 1.0) bRows)) # center
                bRect = (rect (width bSquare) (height bSquare) # lw 0.1)

barcodeDia :: Barcode -> BarcodeDia
barcodeDia bCode = hcat evenedBars # center
    where
        evenedBars = (alignT . scaleToY maxBarHeight) <$> roughBars
        maxBarHeight = maximum $ height <$> roughBars
        roughBars = barDia <$> bCode

barDia :: Bar -> Diagram B
barDia bar = case barKind bar of
    FullMiss bHeight -> bDia
        where
            barRect :: Diagram B
            barRect = (rect (width bDia) (height bDia)) # lw 0.3
            bDia = vcat slices # center
            slices = L.replicate bHeight missSlice
            missSlice = rect barwidth slHeight # fillColor white
                                               # lineWidth 0.1
    RedLineBar bHeight phIndex -> bDia <> barRect
        where
            barRect :: Diagram B
            barRect = (rect (width bDia) (height bDia)) # lw 0.3
            bDia = vcat slices # center
            slices :: [Diagram B]
            slices = B.toList $ missVec B.// [rlPair]
            missVec :: B.Vector (Diagram B)
            missVec = B.replicate bHeight missSlice
            rlPair = (phIndex, redSlice)
            redSlice :: Diagram B
            redSlice = halfSlice === redLine === halfSlice
            redLine :: Diagram B
            redLine = hrule barwidth # lc red
            halfSlice = rect barwidth halfHeight # fillColor white
                                                 # lineWidth none
            missSlice = rect barwidth slHeight # fillColor white
                                               # lineWidth none
            halfHeight :: Double
            halfHeight = slHeight / 2.0
    MatchBar slices lColor -> bDia <> barRect
        where
            barRect :: Diagram B
            barRect = (rect (width bDia) (height bDia)) # lw 0.3
            bDia = vcat diaSlices # center
            diaSlices = sliceDia lColor <$> slices
    where
        barwidth = 2.0 * (fromIntegral attSize)**alpha
        slHeight = 1.0 :: Double
        attSize = attractorSize bar

sliceDia :: LocalColor -> Slice -> Diagram B
sliceDia _ (Miss attSize) =
    rect slicewidth sliceHeight # fillColor white # lineWidth 0.1
    where
        slicewidth = (2.0 :: Double) * (fromIntegral attSize)**alpha
        sliceHeight = 1.0 :: Double
sliceDia c (Match attSize matchLoops) = (hcat slivers)
    where
        slivers = B.toList $ missVec B.// matchPairs
        missVec = B.replicate attSize missSliver
        matchPairs = zip matchIndices $ L.repeat matchSliver
        matchIndices = concat matchLoops
        matchSliver = rect sliverWidth sliceHeight # fillColor c
                                                   # lineWidth 0.1
        missSliver = rect sliverWidth sliceHeight # fillColor white
                                                  # lineWidth 0.1
        sliverWidth = slicewidth / (fromIntegral attSize)
        slicewidth = (2.0 :: Double) * (fromIntegral attSize)**alpha
        sliceHeight = 1.0 :: Double

eSpaceRenderDia :: Diagram B -> Element
eSpaceRenderDia = renderDia  SVG
                            (SVGOptions (mkWidth 1600)
                             Nothing
                             ""
                             []
                             True
                             )

eSpaceRenderText :: Diagram B -> T.Text
eSpaceRenderText = LT.toStrict . renderText . eSpaceRenderDia

mkGridEdges :: [Int] -> [([Int], [Int])]
mkGridEdges dimList = concat $ mkEdges <$> vertices
    where
        mkEdges :: [Int] -> [([Int], [Int])]
        mkEdges vert = catMaybes $ mkEds vertVec <$> [0..((length dimList) - 1)]
            where
                mkEds coordinates i
                    | dim > coord =
                        Just (vert, U.toList $ vertVec U.// [(i, coord + 1)])
                    | otherwise = Nothing
                    where
                        dim = dimList L.!! i
                        coord = coordinates U.! i
                vertVec = U.fromList vert
        vertices = mkPointNames dimList

mkPointNames :: [Int] -> [[Int]]
mkPointNames = (reverse <$>) . sequenceA . reverse . (f <$>)
    where f i = [0..i]


mkPoints :: [Int] -> Double -> [P2 Double]
mkPoints dimList unitScale = case dimList of
    [] -> error "No Dimensions!"
    [x] -> linePoints x unitScale
    [x, y] -> squarePoints x y unitScale
    [x, y, z] -> cubePoints x y z unitScale
    [x, y, z, m] -> tesseractPoints x y z m unitScale
    [x, y, z, m, n] -> penteractPoints x y z m n unitScale
    ds -> error $ ((show . L.length) ds) ++ " is too many dimensions!"

-- Create sets of points at which to place barcode clusters. Note that each is a
-- grid of multiples of such shapes, as the environmental inputs whose values
-- form the axes of fiveDfigures may be integer-valued (with big-endian ordering
-- , i.e. xyz). We use 0-indexing, so a square is squarePoints 1 1 uScale, and a
-- 3x2x2 block is cubePoints 3 2 2 uScale
penteractPoints :: Int -> Int -> Int -> Int -> Int -> Double -> [P2 Double]
penteractPoints i j k m n unitScale =(L.concat . L.transpose) spreadPoints
    where
        spreadPoints = dimSpread n displacement <$> lPts
        displacement = 0 ^& (unitScale * 2 * fromIntegral (maximum [i, j, k]))
        lPts = tesseractPoints i j k m unitScale

tesseractPoints :: Int -> Int -> Int -> Int -> Double -> [P2 Double]
tesseractPoints i j k m unitScale = (L.concat . L.transpose) spreadPoints
    where
        spreadPoints = dimSpread m displacement <$> lPts
        displacement = (unitScale * 2 * fromIntegral (maximum [i, j, k])) ^& 0
        lPts = cubePoints i j k unitScale

cubePoints :: Int -> Int -> Int -> Double -> [P2 Double]
cubePoints i j k unitScale = (L.concat . L.transpose) spreadPoints
    where
        spreadPoints = dimSpread k displacement <$> lPts
        displacement = unitScale *^ cubeVector
        lPts = squarePoints i j unitScale

squarePoints :: Int -> Int -> Double -> [P2 Double]
squarePoints i j unitScale = (L.concat . L.transpose) $ spreadPoints
    where
        spreadPoints = dimSpread j displacement <$> lPts
        displacement = 0 ^& unitScale
        lPts = linePoints i unitScale

linePoints :: Int -> Double -> [P2 Double]
linePoints i unitScale = ((^& 0) . (unitScale *)) <$> [0..(fromIntegral i)]

-- Consume a Int, vector, and point, and replicate that point n more times in
-- that direction. 
dimSpread :: Int -> (V2 Double) -> (P2 Double) -> [P2 Double]
dimSpread n vec pt = pDisplace <$> [0..n] <*> pure pt
    where
        pDisplace m aPt= aPt .+^ (fromIntegral m *^ vec)

-- Create axis labels for variously dimensioned figures. 

mkLabels :: [[NodeName]] -> Double -> Diagram B
mkLabels inputsList unitScale = case inputsList of
    [] -> error "No Inputs!"
    [xs] -> lineLabels xs unitScale
    [xs, ys] -> squareLabels xs ys unitScale
    [xs, ys, zs] -> cubeLabels xs ys zs unitScale
    [xs, ys, zs, ms] -> tesseractLabels xs ys zs ms unitScale
    [xs, ys, zs, ms, ns] -> penteractLabels xs ys zs ms ns unitScale
    ils -> error $ ((show . L.length) ils) ++ " is too many inputs!"

penteractLabels :: [NodeName] -> [NodeName] -> [NodeName] -> [NodeName]
                -> [NodeName] -> Double -> Diagram B
penteractLabels xNames yNames zNames mNames nNames unitScale = finalLs
    where
        finalLs = xyzmLabels <> nLabels
        nLabels = mconcat $ zipWith place nTexts shiftedNPts
        shiftedNPts = (flip (.+^) nLabelDisp) <$> nPts
        nLabelDisp = (unitScale / (-2)) ^& 0
        nPts = dimSpread (gridEdges nNames) displacement $ p2 (0,0)
        displacement = 0 ^& (unitScale * 2 * largestLinedDim)
        largestLinedDim = (fromIntegral . maximum . (gridEdges <$>))
            [xNames, yNames, zNames]
        nTexts = tText'' <$> nNames
        xyzmLabels = tesseractLabels xNames yNames zNames mNames unitScale
        tText'' t = tText' t # alignR

tesseractLabels :: [NodeName] -> [NodeName] -> [NodeName] -> [NodeName]
                -> Double -> Diagram B
tesseractLabels xNames yNames zNames mNames unitScale = xyzLabels <> mLabels
    where
        mLabels = mconcat $ zipWith place mTexts shiftedMPts
        shiftedMPts = (flip (.+^) mLabelDisp) <$> mPts
        mLabelDisp = 0 ^& (unitScale / (-2))
        mPts = dimSpread (gridEdges mNames) displacement $ p2 (0,0)
        displacement = (unitScale * 2 * largestLinedDim) ^& 0
        largestLinedDim = (fromIntegral . maximum . (gridEdges <$>))
            [xNames, yNames, zNames]
        mTexts = tText' <$> mNames
        xyzLabels = cubeLabels xNames yNames zNames unitScale

cubeLabels :: [NodeName] -> [NodeName] -> [NodeName] -> Double -> Diagram B
cubeLabels xNames yNames zNames unitScale = xyLabels <> zLabels
    where
        zLabels = mconcat $ zipWith place zTexts shiftedZPts
        shiftedZPts = (flip (.+^) zLabelDisp) <$> zPts
        zLabelDisp = ((1/8) *^ displacement) ^+^ ((unitScale / 8) ^& 0)
        zPts = dimSpread (gridEdges zNames) displacement $ p2 (0,0)
        displacement = unitScale *^ cubeVector
        zTexts = tText'' <$> zNames
        xyLabels = squareLabels xNames yNames unitScale
        tText'' t = tText' t # alignL

squareLabels :: [NodeName] -> [NodeName] -> Double -> Diagram B
squareLabels xNames yNames unitScale = xLabels <> (mconcat yLabels)
    where
        yLabels = zipWith place yTexts shiftedYPts
        shiftedYPts = (flip (.+^) yLabelDisp) <$> yPts
        yLabelDisp = (unitScale / (-8)) ^& 0
        yPts = dimSpread (gridEdges yNames) displacement $ p2 (0,0)
        displacement = 0 ^& unitScale
        yTexts = tText'' <$> yNames
        xLabels = lineLabels xNames unitScale
        tText'' t = tText' t # alignR

lineLabels :: [NodeName] -> Double -> Diagram B
lineLabels xNames unitScale = mconcat $ zipWith place texts shiftedPts
    where
        shiftedPts = (flip (.+^) labelDisp) <$> pts
        pts = linePoints (gridEdges xNames) unitScale
        labelDisp = 0 ^& (unitScale / (-8))
        texts = tText' <$> xNames

tText' :: T.Text -> Diagram B
tText' t = F.svgText def (T.unpack t) # F.fit_height 4
                                      # F.set_envelope
                                      # fillColor black
                                      # lineWidth none
                                      # center

-- sText :: String -> Diagram B
-- sText s = F.svgText def s # F.fit_height 2
--                                       # F.set_envelope
--                                       # fillColor black
--                                       # lineWidth none
--                                       # center

gridEdges :: [a] -> Int
gridEdges xs = length xs - 1

cubeVector :: V2 Double
cubeVector = 0.32 ^& 0.44
