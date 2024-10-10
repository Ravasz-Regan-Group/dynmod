{-# LANGUAGE OverloadedStrings #-}


module Figures.BarCharts
    ( nBChartDia
    , phBChartDia
    ) where    

import Types.DMModel
import Types.DMInvestigation
import Types.Simulation
import Types.Figures
import Utilities
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Graphics.Rendering.Chart hiding (scale)
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Graphics.SVGFonts as F
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import qualified Data.Text as T
import qualified Data.List.Split as Split
import qualified Data.List as L
import GHC.IO (unsafePerformIO)

-- Make a bar chart with average of the selected DMNodes during each pulse, plus
-- an error bar with the standard deviation.
nBChartDia :: ColorMap
           -> ModelLayer
           -> TCExpMeta
           -> AvgBChartNodes
           -> [B.Vector (U.Vector RealNodeState, (RealNodeState, StdDev))]
           -> (Diagram B, [[(NodeName, U.Vector RealNodeState)]])
nBChartDia cMap mL exMeta bCHNodeNs statVs = (fig, namedPulseVecs)
    where
        fig = (fst . runBackendR dEnv . toRenderable) layout
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 1600 1200
        layout =
            layout_title .~ (T.unpack . tcExpName) exMeta
          $ layout_title_style . font_size .~ 24
          $ layout_legend .~ legend
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ layout_x_axis . laxis_generate .~ autoIndexAxis xAxisNs
          $ layout_left_axis_visibility . axis_show_ticks .~ False
          $ layout_plots .~ ((plotBars . uncurry mkNodePlot) <$>
                (zip (plotValues) (reverse nColors)))
                -- [plotBars barsAvgs]
          $ def
            where
                legend = Just $
                    legend_label_style . font_size .~ 12
                  $ def
        
        mkNodePlot someValue nClr = 
--             plot_bars_titles .~ legendNames
            plot_bars_values .~ [someValue]
          $ plot_bars_style .~ BarsClustered
          $ plot_bars_spacing .~ BarsFixWidth 50
          $ plot_bars_item_styles .~
                (mkStyle <$> (replicate (length avgs) nClr))
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        
--         legendNames = zipWith zipper (repeat "Pulse ") [0..]
--         zipper :: String -> Int -> String ; zipper x y = x ++ show y
        plotValues = addIndexes transposedAvgs
        xAxisNs = T.unpack <$> bCHNodeNs
        transposedAvgs = L.transpose avgs
        (avgs, _ {-stdDevs-}) =
            (isoBimap (fmap B.toList) . L.unzip . fmap B.unzip) pulseStats
-- For now we will keep both the averages and the vectors of NodeState values
-- that they came from. They will be written out with the figure, to be
-- available for additional analysis. Eventually this will be replaced by the
-- ability to do statistics across experiments internally. 
        namedPulseVecs :: [[(NodeName, U.Vector RealNodeState)]]
        namedPulseVecs = (L.zip bCHNodeNs . B.toList) <$> pulseVecs
        pulseVecs :: [B.Vector (U.Vector RealNodeState)]
        pulseStats :: [B.Vector (RealNodeState, StdDev)]
        (pulseVecs, pulseStats) = (L.unzip . fmap B.unzip) selectedVecs
        selectedVecs = flip B.backpermute nIndices <$> statVs
        nIndices = B.fromList $ (lniBMap BM.!) <$> bCHNodeNs
        nColors = L.reverse $ (opaque . (cMap M.!)) <$> bCHNodeNs
        LayerSpecs lniBMap _ _ _ = layerPrep mL

phBChartDia :: ColorMap
            -> ModelLayer
            -> M.HashMap NodeName [PhenotypeName]
            -> TCExpMeta
            -> AvgBChartSwitches
            -> [PulseSpacing]
            -> [M.HashMap PhenotypeName (U.Vector Double, (Double, StdDev))]
            -> (Diagram B, [(NodeName, [[(PhenotypeName, U.Vector Double)]])])
phBChartDia cMap mL switchMap exMeta bChSwitchNs pSps statMs = (fig, chartVecs)
    where
        fig = (extrudeBottom 100 chBlLabel) === chartBlock
        chBlLabel = expLabelDia (3 * floatfSSetting) (tcExpName exMeta)
        chartBlock = phBChArrange 3 chartDias
        chartDias = (resizer expGuide) <$> chartRes
        resizer dx dy = dy === resized # alignL
            where
                resized = extrudeLeft 100 (scale scaleN dx)
                scaleN = 0.75 * (width dy / width dx)
        (chartRes, chartVecs) =  unzip $ (uncurry phDiaF) <$> pairs
        phDiaF = phSingleBChDia fontSizeSetting cMap switchMap
        pairs = zip bChSwitchNs avgs
        avgs::[[[(U.Vector Double, Double)]]]
        avgs =  (fmap . fmap . fmap . fmap) fst pulseStats
-- For now we will keep both the averages and the vectors of Phenotype
-- prevalence that they came from. They will be written out with the figure, to
-- be available for additional analysis. Eventually this will be replaced by the
-- ability to do statistics across experiments internally. 
        pulseStats :: [[[(U.Vector Double, (Double, StdDev))]]]
        pulseStats = L.transpose $ extractor phNamess <$> statMs
        phNamess = (switchMap M.!) <$> bChSwitchNs
        extractor phNss statM = (fmap . fmap)
            (flip (M.findWithDefault (U.empty, (0, 0))) statM) phNss
        floatfSSetting = fromIntegral fontSizeSetting
        fontSizeSetting = 48 :: Int
        expGuide = bChartExpGuide mL exMeta pSps

phSingleBChDia :: Int
               -> ColorMap
               -> M.HashMap NodeName [PhenotypeName]
               -> NodeName
               -> [[(U.Vector Double, Double)]]
               -> (Diagram B, (NodeName, [[(PhenotypeName, U.Vector Double)]]))
phSingleBChDia fontSizeSetting cMap switchMap swName pairs = (fig, swPhLbldVec)
    where
        fig = fst $ (runBackendR dEnv . toRenderable) layout
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 1600 1200
        layout =
            layout_title .~ T.unpack swName 
          $ layout_title_style . font_size .~ (fromIntegral fontSizeSetting)
          $ layout_margin .~ 20
          $ layout_legend .~ legend
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ layout_axes_styles . axis_label_style . font_size .~
                (fromIntegral fontSizeSetting)
          $ layout_left_axis_visibility . axis_show_ticks .~ False
          $ layout_plots .~ [plotBars barsAvgs]
          $ def
        barsAvgs = 
            plot_bars_titles .~ (T.unpack <$> phNames)
          $ plot_bars_values .~ addIndexes avgs
          $ plot_bars_style .~ BarsStacked
          $ plot_bars_spacing .~ BarsFixGap 30 5
          $ plot_bars_item_styles .~ (mkStyle <$> blendedColors)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        legend = Just $
            legend_label_style . font_size .~ (fromIntegral fontSizeSetting)
          $ legend_position .~ LegendRight
          $ legend_orientation .~ LOCols 4
          $ def
        blendedColors = opaque <$> (phTCBlend 0.85 swColor (L.length phNames))
        swColor = cMap M.! swName
        swPhLbldVec = (swName, phLabeledVecs)
        phLabeledVecs = (zip phNames) <$> vecs
        phNames = switchMap M.! swName
        vecs = fst <<$>> pairs
        avgs = snd <<$>> pairs

-- Arrange multiple Phenotype bar charts in a block with minimum row length i
phBChArrange :: Int -> [Diagram B] -> Diagram B
phBChArrange minRowSize dias
    | (minRowSize * minRowSize) + minRowSize >= minRowSize * numDias = let
        dRows = hsep 2.0 <$> Split.chunksOf minRowSize dias
        in vsep 2.0 dRows # center
    | otherwise = let
        rowSize = (ceiling . sqrt) floatMinRowSize
        dRows = hsep 2.0 <$> Split.chunksOf rowSize dias
        in vsep 2.0 dRows # center
    where
        numDias = L.length dias
        floatMinRowSize = fromIntegral minRowSize :: Double

-- Make a experiment name label. 
expLabelDia :: Double -> T.Text -> Diagram B
expLabelDia stripHt exName = tText' stripHt exName # center

bChartExpGuide :: ModelLayer
               -> TCExpMeta
               -> [PulseSpacing]
               -> Diagram B
bChartExpGuide mL exMeta pSps = pulseDia
    where
        pulseDia = hcat $ L.intersperse tickDia pulseDias
        tickDia = vrule (maximum (height <$> pulseDias)) # lw ultraThin # alignT
        pulseDias = bCHpulseSpacingDia 12 <$> inputStrippedPIs
        inputStrippedPIs = case expKnd of
            GenExp -> inputStrip mLInputNames lniBMap (Just initialRIC) pSps
                where initialRIC = tcExpInitCoord exMeta
            _ -> inputStrip mLInputNames lniBMap Nothing pSps
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        mLInputNames =
            ((fmap . fmap) (nodeName . nodeMeta) . inputs . modelGraph) mL
        expKnd = tcExpKind exMeta

bCHpulseSpacingDia :: Double
                   -> (Int, [[(NodeName, RealNodeState)]], [NodeAlteration])
                   -> Diagram B
bCHpulseSpacingDia stripHt (_, inputLs, nAlts) = iptSetDia === hDv === nAltsDia
    where
        hDv = hrule (width iptSetDia) # lw ultraThin
        nAltsDia = vcat $ tText' stripHt <$> nAltTexts
        iptSetDia = tText' stripHt iptSetT
        iptSetT = T.intercalate ", " $ inputCoordText <$> inputLs
        nAltTexts
            | L.null nudges && L.null locks = []
            | L.null nudges = [lockText]
            | L.null locks = [nudgeText]
            | otherwise = [lockText, nudgeText]
        nudgeText = "Nudge: " <> T.intercalate ", "  (nAltTPrep <$> locks)
        lockText = "Lock: " <> T.intercalate ", "  (nAltTPrep <$> locks)
        (locks, nudges) = L.partition isNodeLock nAlts
--         hDiv = hrule (fromIntegral pW) # lw ultraThin

tText' :: Double -> T.Text -> Diagram B
tText' tHt t = F.svgText def (T.unpack t) # F.fit_height tHt
                                      # F.set_envelope
                                      # fillColor black
                                      # lineWidth none
                                      # center

-- Make a [T.Text] of the changing inputs in each PulseSpacing of a
-- [PulseSpacing]. 
-- pulseSpacingTexts :: ModelLayer -> TCExpMeta -> [PulseSpacing] -> [T.Text]
-- pulseSpacingTexts mL exMeta pSps = pulseST <$> inputStrippedPIs
--     where
--         pulseST (_, inputLs, nAlts) =
--             iptSetT <> "\n" <> (T.intercalate "\n" nAltTexts)
--             where
--                 iptSetT = T.intercalate ", " $ inputCoordText <$> inputLs
--                 nAltTexts
--                     | L.null nudges && L.null locks = []
--                     | L.null nudges = [lockText]
--                     | L.null locks = [ndgText]
--                     | otherwise = [lockText, ndgText]
--                 ndgText = "Nudge: " <> T.intercalate ", "
--                                                      (nAltTPrep <$> locks)
--                 lockText = "Lock: " <> T.intercalate ", "
--                                                      (nAltTPrep <$> locks)
--                 (locks, nudges) = L.partition isNodeLock nAlts
--         inputStrippedPIs = case expKnd of
--             GenExp -> inputStrip mLInputNames lniBMap (Just initialRIC) pSps
--                 where initialRIC = tcExpInitCoord exMeta
--             _ -> inputStrip mLInputNames lniBMap Nothing pSps
--         LayerSpecs lniBMap _ _ _ = layerPrep mL
--         mLInputNames =
--             ((fmap . fmap) (nodeName . nodeMeta) . inputs . modelGraph) mL
--         expKnd = tcExpKind exMeta

