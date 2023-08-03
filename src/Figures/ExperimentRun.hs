{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}

module Figures.ExperimentRun
    ( layerRunFigure
    ) where

import Types.DMModel
import Constants
import Types.Simulation
import Types.DMInvestigation
import Types.Figures
import Utilities
import Figures.InputSpaceFigure
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.Bimap as BM
import Data.List.Split (splitPlaces)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Svg.Core (renderText, Element)
import qualified Graphics.SVGFonts as F
import qualified Data.List as L
import System.IO.Unsafe (unsafePerformIO)

-- BarcodeResult combines AttractorResults by equality on Barcodes, because all
-- of the AttractorResults for a given Barcode will be turned into figures in
-- ine SVG file. 
type BarcodeResult = (Barcode, [([Timeline], [PulseSpacing])])

attResCombine :: [AttractorResult] -> [BarcodeResult]
attResCombine ars = M.toList $ M.fromListWith (<>) preppedArs
    where
        preppedArs = pure <<$>> ars

-- The color gradient we use to indicate locked or nudged nodes. 
trimmedRocketCM :: PUCGradient
trimmedRocketCM = (B.reverse . B.drop trimsize . B.reverse) rocketCM
    where trimsize = length rocketCM `quot` 4

layerRunFigure :: ColorMap
               -> HS.HashSet Attractor
               -> LayerResult
               -> ([(ExpContext, [(Barcode, SVGText)])], Maybe SVGText)
layerRunFigure cMap atts (LayerResult lrfMM lrfML eResults mIB) =
    ( expRunFigure cMap lrfMM lrfML <$> eResults
    , attractorESpaceFigure cMap lrfMM lniBMap atts <$> mIB)
    where
        LayerSpecs lniBMap _ _ _ = layerPrep lrfML

-- Create run figures for an individual experiment. Each AttractorResult is
-- turned into a single SVG, no matter how many Threads it has. This is in
-- addition to the fact that AttractorResults were combined if their Barcodes
-- were identical. 
expRunFigure :: ColorMap
             -> ModelMapping
             -> ModelLayer
             -> ExperimentResult
             -> (ExpContext, [(Barcode, SVGText)])
expRunFigure cMap mM mL (exCon, attResults) = (exCon, bcSVGsWBCs)
    where
        bcSVGsWBCs = expRunRenderText <<$>> bcDiasWBCs
        bcDiasWBCs = bcRunDia cMap mM mL <$> (attResCombine attResults)

expRunRenderText :: Diagram B -> SVGText
expRunRenderText = LT.toStrict . renderText . expRunRenderDia

expRunRenderDia :: Diagram B -> Element
expRunRenderDia = renderDia  SVG
                            (SVGOptions (mkWidth 1600)
                             Nothing
                             ""
                             []
                             True
                             )


bcRunDia :: ColorMap
         -> ModelMapping
         -> ModelLayer
         -> BarcodeResult
         -> (Barcode, Diagram B)
bcRunDia cMap mM mL (bc, tmLnPIs) = (bc, vsep 5.0 (legendDia:bcFigs))
    where
        bcFigs = mconcat $ attRunDia <$> tmLnPIs
        attRunDia (tmLns, pIs) = runDia cMap mM mL bc pIs <$> tmLns
        legendDia = bcRunFigLegendDia cMap bc

bcRunFigLegendDia :: ColorMap -> Barcode -> Diagram B
bcRunFigLegendDia cMap bc = hsep 1.0 evenedBlocks
    where
        evenedBlocks = zipWith (switchLegend' cMap) phHeights bc
        phHeights = ((maxBlockHeight * lScale) / ) <$> phSizes
        maxBlockHeight = maximum phSizes
        phSizes :: [Double]
        phSizes = (fromIntegral . length . phenotypeNames) <$> bc
        lScale = 5.0 :: Double

switchLegend' :: ColorMap -> Double -> Bar -> Diagram B
switchLegend' cMap slHeight br = vcat $ (tText'' swName):slices
    where
        slices = (sliceRect <>) <$> coloredLabels
        coloredLabels = (center . (colorRect |||)) <$> paddedPhLabels
        sliceRect = (rect (slwidth + lScale) slHeight) # lw 0.1
        paddedPhLabels = lLabelPad' slwidth <$> phLabels
        slwidth = maximum $ width <$> phLabels
        phLabels = (padX 1.1 . phTextLabel (barPhenotype br)) <$> orderedPHNames
        orderedPHNames = (L.reverse . phenotypeNames) br
        colorRect = rect lScale slHeight # fillColor (cMap M.! swName) # lw none
        swName = switchName br
        tText'' t = F.svgText def (T.unpack t) # F.fit_height (0.75 * lScale)
                                               # F.set_envelope
                                               # fillColor black
                                               # lineWidth none
                                               # center
        lScale = 5.0 :: Double 

lLabelPad' :: Double -> Diagram B -> Diagram B
lLabelPad' widest lLabel = padX tweak lLabel
    where
        tweak = widest / (width lLabel)

phTextLabel :: Maybe PhenotypeName -> PhenotypeName -> Diagram B
phTextLabel mPhName phName
    | mPhName == Just phName = bText phName
    | otherwise = tText'' phName
    where
        tText'' t = F.svgText def (T.unpack t) # F.fit_height (0.75 * lScale)
                                               # F.set_envelope
                                               # fillColor black
                                               # lineWidth none
                                               # center
        bText t = F.svgText tOpts (T.unpack t) # F.fit_height (0.75 * lScale)
                                               # F.set_envelope
                                               # fillColor black
                                               # italic
                                               # lineWidth none
                                               # center
        tOpts = F.TextOpts (unsafePerformIO F.lin) F.KERN False
        lScale = 5.0 :: Double

runDia :: ColorMap
       -> ModelMapping
       -> ModelLayer
       -> Barcode
       -> [PulseSpacing]
       -> Timeline
       -> Diagram B
runDia cMap mM mL bc pulseSps tmLn = (switchFig ||| nodeNameBlock |||
            (vsep 2.0 [pulseLines `atop` nodeStripBlock, timeAxis]))
    where
        timeAxis = timeAxisDia stripHt $ B.length reordTmLn
        switchFig = vsep stripHt switchFigs
        switchFigs = runSwitchDia cMap stripHt <$> switchPHPairs
        -- We want to annotate the Switch name with the phenotype this run
        -- started in, if such exists, so we pair them before passing to
        -- runSwitchDia
        switchPHPairs = findBar bc <$> mM
        pulseLines = translateX (-0.5) $ translateY 1 $
            hcat $ (pulseLine (height nodeStripBlock)) <$> (pulseSps)
        nodeStripBlock = (vsep stripHt . fmap vcat) chunkedNodeStrips
        chunkedNodeStrips = splitPlaces switchLs nodeStrips
        nodeNameBlock = nodeNameBlockDia switchLs stripHt switchNodeOrder
        nodeStrips =
            nodeStripDia reordLNIBMap reordRTs stripHt <$> (annTimeStrips)
        stripHt = 2
        annTimeStrips = zip switchNodeOrder transpRTmLn
        transpRTmLn = (L.transpose . B.toList . fmap U.toList) reordTmLn
        -- We want to striate the runs by Switch, so we will need these spacings
        switchLs = (L.length . fst . snd) <$> mM
        -- We need to group the figure by Switch, so we first reorder all of the
        -- AnnotatedLayerVecs in the Timeline, as well as all of our helper data
        -- structures
        reordRTs = U.backpermute rangeTs permuteVec
            where permuteVec = U.fromList $ (lniBMap BM.!) <$> switchNodeOrder
        reordLNIBMap = BM.fromList $ zip switchNodeOrder [0..]
        reordTmLn = case timelineMMReorder (fst <<$>> mM) lniBMap tmLn of
            Right r -> r
            Left errs -> error $ "reordTmLn in ExperimentRun.hs has: " <>
                                                                    show errs
        LayerSpecs lniBMap rangeTs _ _ = layerPrep mL
        switchNodeOrder = concatMap (fst . snd) mM

-- Mark where each new pulse begins. 
pulseLine :: Double -> Int -> Diagram B
pulseLine lHeight pIndex = ((strutX strutWidth) ||| vLine) # alignY (1) # alignL
    where 
        vLine = vrule lHeight # lc red # lw ultraThin
        strutWidth = fromIntegral pIndex

-- Order the nodes in an Timeline according to the Switch partitioning
-- of the layer above. 
timelineMMReorder :: U.Unbox a
                  => DMMSModelMapping
                  -> LayerNameIndexBimap
                  -> B.Vector (U.Vector (NodeState, a))
                  -> Either InvalidLVReorder
                           (B.Vector (U.Vector (NodeState, a)))
timelineMMReorder dmmsMM lniBMap tmLn =
    traverse (annotatedLayerVecReorder lniBMap switchNodeOrder) tmLn
    where
        switchNodeOrder = concatMap snd dmmsMM

-- Re-order an AnnotatedLayerVec according to a new List of NodeNames. Basic
-- error checking, but use at your own risk. 
annotatedLayerVecReorder :: U.Unbox a
                         => LayerNameIndexBimap
                         -> [NodeName]
                         -> U.Vector (NodeState, a)
                         -> Either InvalidLVReorder (U.Vector (NodeState, a))
annotatedLayerVecReorder lniBMap newOrder annLVec
    | (L.sort . BM.keys) lniBMap /= L.sort newOrder =
        Left NewOldOrderingMismatch
    | BM.size lniBMap /= U.length lVec = Left OldOrderingLVMismatch
    | otherwise = Right $ U.zip reorderedLVec reorderedAnnVec
        where
             reorderedAnnVec = U.backpermute annVec permuteVec
             reorderedLVec = U.backpermute lVec permuteVec
             permuteVec = U.fromList $ (lniBMap BM.!) <$> newOrder
             (lVec, annVec) = U.unzip annLVec

findBar :: Barcode -> Switch -> (Switch, Maybe PhenotypeName)
findBar bc sw = (sw, phN)
    where phN = (L.find (\b -> switchName b == fst sw) bc) >>= barPhenotype

nodeStripDia :: LayerNameIndexBimap
             -> LayerRangeVec
             -> Int
             -> (NodeName, [(NodeState, WasForced)])
             -> Diagram B
nodeStripDia reordLNIBMap reordRTs stripHt (nName, annTLine) = timeStrip
    where
        timeStrip = hcat $ nodeStateDia nRange stripHt <$> annTLine
        nRange = reordRTs U.! (reordLNIBMap BM.! nName)

nodeStateDia :: (Int, Int) -> Int -> (NodeState, WasForced) -> Diagram B
nodeStateDia (rangeB, rangeT) stripHt (nState, wForced) = dia
    where
        dia = rect 1 (fromIntegral stripHt) # theFillColor # lineWidth none
        theFillColor = case cPick of
            Nothing -> fcA transparent
            Just c -> fillColor c
        cPick | not wForced = gradientPick cividisCM fracRange realNState
              | otherwise   = gradientPick trimmedRocketCM fracRange realNState
        realNState = fromIntegral nState
        fracRange = ((fromIntegral rangeB) :: Double, fromIntegral rangeT)

runSwitchDia :: ColorMap -> Int -> (Switch, Maybe PhenotypeName) -> Diagram B
runSwitchDia cMap stripHt (sw, mPhName) = (tText' textH swText) <> swBox
    where
        swBox :: Diagram B
        swBox = rect rectW rectH # fillColor swColor # lineWidth none
        swText = case mPhName of
            Nothing -> swName
            Just phName -> swName <> " - " <> phName
        rectW = 24.0
        textH = fromIntegral stripHt
        rectH = fromIntegral $ stripHt * ((length . fst . snd) sw)
        swColor = cMap M.! swName
        swName = fst sw

-- Make the timeline for the figure. 
timeAxisDia :: Int -> Int -> Diagram B
timeAxisDia strpHght runLength = axisArrow ===
                                 tText' ((fromIntegral strpHght) * 2) "Time"
    where
        axisArrow =
            arrow' (with & headLength .~ tiny)
                      (fromIntegral runLength) # lw thin

-- Make the vertical block of NodeName labels, grouped and spaced out into
-- switches. 
nodeNameBlockDia :: [Int] -> Int -> [NodeName] -> Diagram B
nodeNameBlockDia switchLs stripHt switchNodeOrder = dia
    where
        dia = (vsep (fromIntegral stripHt) . fmap vcat) chunkedPaddednLabels
        chunkedPaddednLabels = splitPlaces switchLs paddedLabels
        paddedLabels = padder maxW <$> nLabels
        padder m d = padX (m / (width d)) d
        maxW = maximum $ width <$> nLabels
        nLabels = nodeStripLabelDia stripHt <$> switchNodeOrder

-- Make a NodeName label. 
nodeStripLabelDia :: Int -> T.Text -> Diagram B
nodeStripLabelDia stripHt nName = tText' (fromIntegral stripHt) nName # alignL

tText' :: Double -> T.Text -> Diagram B
tText' tHt t = F.svgText def (T.unpack t) # F.fit_height tHt
                                      # F.set_envelope
                                      # fillColor black
                                      # lineWidth none
                                      # center



