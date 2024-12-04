{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}

module Figures.InputSpaceFigure
    ( attractorESpaceFigure
    ) where    

import Types.DMModel
import Types.Simulation
import Types.Figures
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Graphics.SVGFonts as F
import qualified Data.List.Split as Split
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import TextShow
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import Data.Maybe (catMaybes)


type ESpacePointDia = Diagram B
type BarcodeDia = Diagram B

-- Create an up to 5-D figure to check how well attractors behave under changes
-- in inputs, as well as how well Phenotypes match up to derived Attractors. 

-- If there are more than 5 inputs, those inputs that are not in the diagram
-- will be have been pinned by the user. If there are none, the user will have
-- been notified. 

attractorESpaceFigure :: ColorMap
                      -> ModelMapping
                      -> LayerNameIndexBimap
                      -> HS.HashSet Attractor
                      -> InputBundle
                      -> Diagram B
attractorESpaceFigure cMap mMap lniBMap atts iBundle =
    frame 3.0 $ figureDia === (scale (0.75 * sFactor) legendDia)
    where
        sFactor = width figureDia / width legendDia
        figureDia
            | L.null dimList =
                let bcCluster = mkBCCluster atts
                    fBCCl = filter (bcFilterF (ibBCFilter iBundle)) bcCluster
                in eSpacePointDia fBCCl
            | L.length dimList <= 3 = allLines5DFigure dimList 
                                                       eSpacePointDias
                                                       $ eSpaceNames freeINodes
            | otherwise = someSpaces5DFigure dimList
                                             eSpacePointDias
                                             $ eSpaceNames freeINodes
        eSpacePointDias = eSpacePointDia <$> filteredBCClusters
        filteredBCClusters = filter (bcFilterF (ibBCFilter iBundle)) <$>
                                                        barcodeClusters
        barcodeClusters = mkBCCluster <$> attClusters
        mkBCCluster = fmap (fst . mkBarcode cMap mMap lniBMap) . HS.toList
        attClusters = attPartition atts <$> netInputCombos
        netInputCombos = (fixedINodeFixVec <>) <$> figOrdFINodeCs
--   The order that an input is displayed in a figure is big-endian,
--   rather than the small-endian storage- and inputCombo-order. Each
--   [FixedVec] produced by inputLevels must be reversed, before we
--   concatenate them. Thus, we cannot use inputCombos directly. 
        figOrdFINodeCs = levelReorder naiveLevels
        levelReorder = (((U.concat . L.reverse) <$>) . sequenceA . reverse)
        dimList = ((+(-1)) . L.length) <$> naiveLevels
        naiveLevels = inputLevels lniBMap Nothing <$> freeINodes
        legendDia = attESpaceFigLegend cMap mMap
        fixedINodeFixVec = ibFixedVec iBundle
        freeINodes = ibInputs iBundle        

-- Consume a HS.HashSet Attractor and a FixedVec that represents a point in the
-- environmental space, and produce a HS.HashSet Attractor of all the attractors
-- which exist there. 
attPartition :: HS.HashSet Attractor -> FixedVec -> HS.HashSet Attractor
attPartition atts fVec = HS.filter (attMatch fVec) atts

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
threeChunkS [] = 0
threeChunkS [i] = i + 1
threeChunkS [i, j] = (i + 1) * (j + 1)
threeChunkS [i, j, k] = (i + 1) * (j + 1) * (k + 1)
threeChunkS ds = (product . fmap (+1) . take 3) ds

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
                        iLine = nName <> ":" <> showt i
                (nName, nRange) = nodeRange n
        eSpaceName ns = [headName <> " off"] <> [headName <> " on"] <>
                                ((nodeName . nodeMeta) <$> (tail ns))
            where
                headName = (nodeName . nodeMeta . head) ns

------------------------------------------------------------------------------
-- Diagrams functions

-- The scaling factor for widening bars in 5D figures. 
alpha :: Double
alpha = 0.4

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
                bRows = hsep 1.0 <$> Split.chunksOf rowSize bCodeDias
                bSquare = vsep 1.0 bRows # center
                bRect = (rect (width bSquare) (height bSquare) # lw 0.1)

barcodeDia :: Barcode -> BarcodeDia
barcodeDia bCode = (hcat $ L.intersperse separator wrappedBars) # center
    where
        separator = vrule maxBarHeight # lineWidth 0.4 # alignT
        wrappedBars = zipWith (<>) evenedBars barRects
        barRects = (alignT . wrapRect) <$> evenedBars
        wrapRect d = (rect (width d) (height d)) # lw 0.3
        evenedBars = (alignT . scaleToY maxBarHeight) <$> roughBars
        maxBarHeight = maximum $ height <$> roughBars
        roughBars = barDia <$> bCode

barDia :: Bar -> Diagram B
barDia bar = case barKind bar of
    FullMiss bHeight -> bDia
        where
            bDia = vcat slices # center
            slices = L.replicate bHeight missSlice
            missSlice = rect barwidth slHeight # fillColor white
                                               # lineWidth 0.1
    MatchBar slices lColor -> bDia
        where
            bDia = vcat diaSlices # center
            diaSlices = sliceDia lColor <$> slices
    where
        barwidth = 2.0 * (fromIntegral attSize)**alpha
        slHeight = 1.0 :: Double
        attSize = attractorSize bar

sliceDia :: LocalColor -> Slice -> Diagram B
sliceDia _ (Miss attSize) = (hcat slivers) # center
    where
        slivers = L.replicate attSize missSliver
        missSliver = rect sliverWidth sliceHeight # fillColor white
                                                  # lineWidth 0.1
        sliverWidth = slicewidth / (fromIntegral attSize)
        slicewidth = (2.0 :: Double) * (fromIntegral attSize)**alpha
        sliceHeight = 1.0 :: Double
sliceDia c (Match attSize matchLoops _) = (hcat slivers) # center
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
    [x, y, z, w] -> tesseractPoints x y z w unitScale
    [x, y, z, w, v] -> penteractPoints x y z w v unitScale
    ds -> error $ ((show) ds) ++ " is too many dimensions!"

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
    [xs, ys, zs, ws] -> tesseractLabels xs ys zs ws unitScale
    [xs, ys, zs, ws, vs] -> penteractLabels xs ys zs ws vs unitScale
    ils -> error $ ((show . L.length) ils) ++ " is too many inputs!"

penteractLabels :: [NodeName] -> [NodeName] -> [NodeName] -> [NodeName]
                -> [NodeName] -> Double -> Diagram B
penteractLabels xNames yNames zNames wNames vNames unitScale = finalLs
    where
        finalLs = xyzwLabels <> vLabels
        vLabels = mconcat $ zipWith place vTexts shiftedNPts
        shiftedNPts = (flip (.+^) vLabelDisp) <$> vPts
        vLabelDisp = (unitScale * (-0.75)) ^& (unitScale / 2)
        vPts = dimSpread (gridEdges vNames) displacement $ p2 (0,0)
        displacement = 0 ^& (unitScale * 2 * largestLinedDim)
        largestLinedDim = (fromIntegral . maximum . (gridEdges <$>))
            [xNames, yNames, zNames]
        vTexts = textF <$> vNames
        textF = rotateBy (1/4) . variableText (unitScale/8.0)
        xyzwLabels = tesseractLabels xNames yNames zNames wNames unitScale

tesseractLabels :: [NodeName] -> [NodeName] -> [NodeName] -> [NodeName]
                -> Double -> Diagram B
tesseractLabels xNames yNames zNames wNames unitScale = xyzLabels <> wLabels
    where
        wLabels = mconcat $ zipWith place wTexts shiftedWPts
        shiftedWPts = (flip (.+^) wLabelDisp) <$> wPts
        wLabelDisp = 0 ^& (unitScale / (-2))
        wPts = dimSpread (gridEdges wNames) displacement $ p2 (0,0)
        displacement = (unitScale * 2 * largestLinedDim) ^& 0
        largestLinedDim = (fromIntegral . maximum . (gridEdges <$>))
            [xNames, yNames, zNames]
        wTexts = variableText (unitScale/8.0) <$> wNames
        xyzLabels = cubeLabels xNames yNames zNames unitScale

cubeLabels :: [NodeName] -> [NodeName] -> [NodeName] -> Double -> Diagram B
cubeLabels xNames yNames zNames unitScale = xyLabels <> zLabels
    where
        zLabels = mconcat $ zipWith place zTexts shiftedZPts
        shiftedZPts = (flip (.+^) zLabelDisp) <$> zPts
        zLabelDisp = ((1/8) *^ displacement) ^+^ ((unitScale / 8) ^& 0)
        zPts = dimSpread (gridEdges zNames) displacement $ p2 (0,0)
        displacement = unitScale *^ cubeVector
        zTexts = (alignL . variableText (unitScale/8.0)) <$> zNames
        xyLabels = squareLabels xNames yNames unitScale

squareLabels :: [NodeName] -> [NodeName] -> Double -> Diagram B
squareLabels xNames yNames unitScale = xLabels <> (mconcat yLabels)
    where
        yLabels = zipWith place yTexts shiftedYPts
        shiftedYPts = (flip (.+^) yLabelDisp) <$> yPts
        yLabelDisp = (unitScale / (-2.9)) ^& 0
        yPts = dimSpread (gridEdges yNames) displacement $ p2 (0,0)
        displacement = 0 ^& unitScale
        yTexts = (alignR . variableText (unitScale/8.0)) <$> yNames
        xLabels = lineLabels xNames unitScale

lineLabels :: [NodeName] -> Double -> Diagram B
lineLabels xNames unitScale = mconcat $ zipWith place texts shiftedPts
    where
        shiftedPts = (flip (.+^) labelDisp) <$> pts
        pts = linePoints (gridEdges xNames) unitScale
        labelDisp = 0 ^& (unitScale / (-5))
        texts = variableText (unitScale/8.0) <$> xNames

gridEdges :: [a] -> Int
gridEdges xs = length xs - 1

cubeVector :: V2 Double
cubeVector = 0.32 ^& 0.44

variableText :: Double -> T.Text -> Diagram B
variableText h t = F.svgText def (T.unpack t) # F.fit_height h
                                              # F.set_envelope
                                              # fillColor black
                                              # lineWidth none
                                              # center


attESpaceFigLegend :: ColorMap -> ModelMapping -> Diagram B
attESpaceFigLegend cMap mMap = hsep 1.0 evenedBlocks
    where
        evenedBlocks = zipWith (switchLegend cMap) phHeights nonEmptyPhs
        phHeights = ((maxBlockHeight * lScale) / ) <$> phSizes
        maxBlockHeight = maximum phSizes
        phSizes :: [Double]
        phSizes = (fromIntegral . length . snd . snd) <$> nonEmptyPhs
        nonEmptyPhs = nonEmptyPhenotypes mMap
        lScale = 2.0 :: Double

switchLegend :: ColorMap -> Double -> Switch -> Diagram B
switchLegend cMap slHeight (swName, (_, phs)) = vcat $ (tText'' swName):slices
    where
        slices = (sliceRect <>) <$> coloredLabels
        coloredLabels = (center . (colorRect |||)) <$> paddedPhLabels
        sliceRect = (rect (slwidth + lScale) slHeight) # lw 0.1
        paddedPhLabels = lLabelPad slwidth <$> phLabels
        slwidth = maximum $ width <$> phLabels
        phLabels = (padX 1.1 . tText'' . phenotypeName) <$> orderedPHs
        orderedPHs = (L.reverse . L.sortOn switchNodeState) phs
        colorRect = rect lScale slHeight # fillColor (cMap M.! swName) # lw none
        tText'' t = F.svgText def (T.unpack t) # F.fit_height (0.75 * lScale)
                                               # F.set_envelope
                                               # fillColor black
                                               # lineWidth none
                                               # center
        lScale = 2.0 :: Double

lLabelPad :: Double -> Diagram B -> Diagram B
lLabelPad widest lLabel = padX tweak lLabel
    where
        tweak = widest / (width lLabel)

