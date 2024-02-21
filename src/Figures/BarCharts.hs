{-# LANGUAGE OverloadedStrings #-}


module Figures.BarCharts
    ( nBChartDia
    ) where    

import Types.DMModel
import Types.DMInvestigation
import Types.Simulation
import Types.Figures
import Utilities
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import qualified Data.List as L
import GHC.IO (unsafePerformIO)

-- Make a bar chart with average of the selected DMNodes during each pulse, plus
-- an error bar with the standard deviation.
nBChartDia :: ColorMap
           -> ModelLayer
           -> DMExperimentMeta
           -> AvgBChartNodes
           -> ([U.Vector (RealNodeState, StdDev)], [PulseSpacing])
           -> Diagram B
nBChartDia cMap mL exMeta bCHNodeNs (statVs, pulseSps) = fst $
    (runBackendR dEnv . toRenderable) layout
    where
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 1600 1200
        layout =
            layout_title .~ (T.unpack . experimentName) exMeta
          $ layout_title_style . font_size .~ 24
          $ layout_legend .~ legend
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ layout_left_axis_visibility . axis_show_ticks .~ False
          $ layout_plots .~ [plotBars barsAvgs, plotBars barsFull]
          $ def
        barsFull =
--             plot_bars_titles .~ []
            plot_bars_values .~ addIndexes boxes
          $ plot_bars_style .~ BarsClustered
          $ plot_bars_spacing .~ BarsFixGap 30 5
          $ plot_bars_item_styles .~ (mkStyle <$> (repeat transparent))
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 1.0 $ opaque black) 
        legend =  Just $
            legend_label_style . font_size .~ 12
            $ def
        barsAvgs =
            plot_bars_titles .~ (T.unpack <$> bCHNodeNs)
          $ plot_bars_values .~ addIndexes avgs
          $ plot_bars_style .~ BarsClustered
          $ plot_bars_spacing .~ BarsFixGap 30 5
          $ plot_bars_item_styles .~ (mkStyle <$> nColors)
          $ def
            where
                mkStyle c = (solidFillStyle c, Nothing)
--                 test = [[1,2,3], [1,2,3], [1,2,3]] :: [[Double]]
        boxes = replicate (length avgs) (fromIntegral <$> nRangeTops)
        (avgs, _ {-stdDevs-}) =
            (isoBimap (fmap U.toList) . L.unzip . fmap U.unzip) nodeStats
        nodeStats = flip U.backpermute nIndices <$> statVs
        nRangeTops = (fmap snd . U.toList . U.backpermute rangeTs) nIndices
        nIndices = U.fromList $ (lniBMap BM.!) <$> bCHNodeNs
        nColors = (opaque . (cMap M.!)) <$> bCHNodeNs
--         pulseLengths = fstOf3 <$> pulseSps
        LayerSpecs lniBMap rangeTs _ _ = layerPrep mL

