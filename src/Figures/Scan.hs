{-# LANGUAGE OverloadedStrings #-}


module Figures.Scan
    ( ScanExpFigure (..)
    , BaseScanFigs(..)
    , sc3DHeatMapDia
    , scDifferenceHeatMapDia
    , scanXAxisData
    , scHeatMapDias
    , labelMutantHMDias
    , envKDOENodeDia
    , envKDOESWDia
    , baseScDia
    , needOverlays
    ) where    

import Types.DMModel
import Types.DMInvestigation
import Types.Figures
import Utilities
import Diagrams.Prelude
import Diagrams.TwoD.Text (mkText, TextAlignment)
import Diagrams.Backend.Cairo
import qualified Graphics.SVGFonts as F
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Plots as P
import Plots.Axis.Line (axisLine, axisLineStyle)
import qualified Data.Colour.Names as CN
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import TextShow
import qualified Data.List as L
import qualified Data.Bifunctor as BF
import Text.Printf (printf)
import GHC.IO (unsafePerformIO)

data ScanExpFigure =
      EnvScFig BaseScanFigs
    | KDOEScFig BaseScanFigs
    | EnvKDOESc (Diagram B) (Diagram B)
    | TwoDEnvScWWOKDOE [StopDistributionHMFig]
                       [SwitchHMFig]
                       [NodeHMFig]
    | ThreeDEnvSc [StopDistributionHMFig]
                  [SwitchHMFig]
                  [NodeHMFig]

data BaseScanFigs = BSFgs StopDistributionFig
                          [TimeInSwitchPhsFig]
                          [AverageNodeValueFig]

type NamedFigure = (FigureFileName, Diagram B)
type FigureFileName = T.Text

type StopDistributionFig = NamedFigure
type TimeInSwitchPhsFig = NamedFigure
-- Average DMNode value over a Scan. 
type AverageNodeValueFig = NamedFigure

type StopDistributionHMFig = NamedFigure
type SwitchHMFig = NamedFigure
type NodeHMFig = NamedFigure        

labelMutantHMDias :: ([NamedFigure], [NamedFigure], [NamedFigure])
                  -> (String, Double)
                  -> ([NamedFigure], [NamedFigure], [NamedFigure])
labelMutantHMDias (stpFigs, swFigs, nFigs) (offAxisTitle, stepDouble) =
    (fmapLMHMDF stpFigs, fmapLMHMDF swFigs, fmapLMHMDF nFigs)
    where
        fmapLMHMDF = (fmap . fmap) labelMutantHMD
        labelMutantHMD fig = (fig === strutY 5 === textFig)
        textFig = tText' 75 titleText # center
        titleText = T.pack offAxisTitle <> ": " <> fpShow stepDouble
        fpShow :: Double -> T.Text
        fpShow = T.pack . printf "%.2f"

baseScDia :: ColorMap
          -> PhColorMap
          -> M.HashMap ScanSwitch [PhenotypeName]
          -> SCExpMeta
          -> ScanStats
          -> BaseScanFigs
baseScDia cMap phCMap switchMap exMeta
    (allStopDs, allPhDists, allNStats) =
        BSFgs stopDistFig timeInSwFigs nodeAvgsFig
    where
        nodeAvgsFig = nodeAvgFig cMap exMeta allNStats <$> scanNNs
        scanNNs = scExpScanNodes exMeta
        timeInSwFigs = timeInSwPhsDia phCMap exMeta allPhDists <$> scanSwsPairs
        scanSwsPairs = (zip scanSws . fmap (switchMap M.!)) scanSws
        scanSws = scExpSwitches exMeta
        stopDistFig =("stopPhs", (fst . runBackendR dEnv . toRenderable) layout)
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 300 325
        layout =
            layout_title .~ (T.unpack . scExpName) exMeta
          $ layout_title_style . font_size .~ 12
          $ layout_x_axis . laxis_override .~ axisGridHide
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ layout_x_axis . laxis_generate .~ autoIndexAxis xPlotLabels
          $ layout_x_axis . laxis_title .~ xTitle
          $ layout_x_axis . laxis_title_style . font_size .~ 14
          $ layout_legend .~ legend
          $ layout_plots .~ [plotBars barsStopD]
          $ def
            where
                legend = Just 
--                     legend_label_style . font_size .~ 12
                  $ def
        barsStopD =
            plot_bars_titles .~ (T.unpack <$> ("NoStopPhenotype":stopPhNames))
          $ plot_bars_values .~ addIndexes plotValues
          $ plot_bars_style .~ BarsStacked
          $ plot_bars_item_styles .~ (mkStyle <$> stopDColors)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        stopDColors = (fmap opaque . (CN.whitesmoke:) . fmap (phCMap M.!))
                        stopPhNames
        xPlotLabels = showRound <$> xRange
        showRound :: Double -> String; showRound = printf "%.2f"
        (xTitle, xRange) = (head . scanXAxisData . scMetaScanKind) exMeta
        plotValues = (\(xs, y) -> y:(snd <$> xs)) <$> stopDs
        stopDs = (BF.first stopDF) <$> allStopDs
        stopDF stM = zip stopPhNames ((stM M.!) <$> stopPhNames)
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

timeInSwPhsDia :: ColorMap
               -> SCExpMeta
               -> [PhDistribution]
               -> (ScanSwitch, [PhenotypeName])
               -> TimeInSwitchPhsFig
timeInSwPhsDia cMap exMeta allPhDists (scSw, phNs) = (,) scSw $
    (fst . runBackendR dEnv . toRenderable) layout
    where
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 300 325
        layout =
            layout_title .~ (T.unpack layoutTitle)
          $ layout_title_style . font_size .~ 12
--           $ layout_axes_styles . axis_label_style . font_size .~ 6
          $ layout_x_axis . laxis_override .~ axisGridHide
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ layout_x_axis . laxis_generate .~ autoIndexAxis xPlotLabels
          $ layout_x_axis . laxis_title .~ xTitle
          $ layout_x_axis . laxis_title_style . font_size .~ 14
          $ layout_legend .~ legend
          $ layout_plots .~ [plotBars barsPhDist]
          $ def
            where
                legend = Just 
--                     legend_label_style . font_size .~ 36
                  $ def
        barsPhDist =
            plot_bars_titles .~ (T.unpack <$> phNs)
          $ plot_bars_values .~ addIndexes plotValues
          $ plot_bars_style .~ BarsStacked
          $ plot_bars_item_styles .~ (mkStyle <$> phNColors)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        xPlotLabels = showRound <$> xRange
        showRound :: Double -> String
        showRound = printf "%.2f"
        plotValues = plotVF phNs <$> allPhDists
        plotVF ns phDM = (phDM M.!) <$> ns
        layoutTitle = scExpName exMeta <> ", Time In: " <> scSw
        (xTitle, xRange) = (head . scanXAxisData . scMetaScanKind) exMeta
        phNColors = (opaque . (cMap M.!)) <$> phNs

envKDOESWDia :: PhColorMap
             -> M.HashMap ScanSwitch [PhenotypeName]
             -> SCExpMeta
             -> [ScanStats]
             -> Diagram B
envKDOESWDia phCMap switchMap exMeta scanStatss =
    frame 10 $ swTitleDia === swDiaBlock
    where
        swTitleDia = center $ (text ((T.unpack . scExpName) exMeta)
            # fontSize (local (0.015 * swBlockWidth))
            # bold) <> strutY (0.03 * swBlockWidth)
        swBlockWidth = Diagrams.Prelude.width swDiaBlock
        swDiaBlock = (vsep 10 $ hsep 10 <$> swDiaArray) # center
        swDiaArray = mkDia <<$>> strippedSWLayouts
        mkDia = fst . runBackendR dEnv . toRenderable
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 300 325
        strippedSWLayouts = case swappedSWLayouts of
            [] -> []
            stoppedLs:lss -> stoppedLs:(stripLRows <$> lss)
        swappedSWLayouts = L.transpose titledSWLayouts
        titledSWLayouts = zipWith
            (xTitleColumn exMeta offAxisTitle) offAxisRange swLayouts
        (offAxisTitle, offAxisRange) = 
            (last . scanXAxisData . scMetaScanKind) exMeta
        swLayouts :: [[Layout PlotIndex Double]]
        swLayouts = envKDOEVertLayout phCMap switchMap exMeta <$> scanStatss

envKDOEVertLayout :: PhColorMap
                  -> M.HashMap ScanSwitch [PhenotypeName]
                  -> SCExpMeta
                  -> ScanStats
                  -> [Layout PlotIndex Double]
envKDOEVertLayout cMap switchMap exMeta (allStopDs, allPhDists, _) =
    stopPHlayout:timeInSwlayouts
    where
        timeInSwlayouts = envKDOELayout cMap exMeta allPhDists <$> scanSwsPairs
        scanSwsPairs = (zip scanSws . fmap (switchMap M.!)) scanSws
        scanSws = scExpSwitches exMeta
        stopPHlayout =
            layout_legend .~ legend
          $ layout_plots .~ [plotBars barsStopD]
          $ layout_x_axis . laxis_generate .~ autoIndexAxis xPlotLabels
          $ layout_x_axis . laxis_override .~ axisGridHide
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ def
            where
                legend = Just $
                    legend_margin .~ 10
                  $ legend_orientation .~ LORows 2
                  $ def
        barsStopD =
            plot_bars_titles .~ (T.unpack <$> ("NoStopPhenotype":stopPhNames))
          $ plot_bars_values .~ addIndexes plotValues
          $ plot_bars_style .~ BarsStacked
          $ plot_bars_item_styles .~ (mkStyle <$> stopDColors)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        stopDColors = (fmap opaque . (CN.whitesmoke:) . fmap (cMap M.!))
                    stopPhNames
        xPlotLabels = show <$> xRange
        xRange = (snd . head . scanXAxisData . scMetaScanKind) exMeta
        plotValues = (\(xs, y) -> y:(snd <$> xs)) <$> stopDs
        stopDs = (BF.first stopDF) <$> allStopDs
        stopDF stM = zip stopPhNames ((stM M.!) <$> stopPhNames)
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

envKDOELayout :: ColorMap
              -> SCExpMeta
              -> [PhDistribution]
              -> (ScanSwitch, [PhenotypeName])
              -> Layout PlotIndex Double
envKDOELayout cMap exMeta allPhDists (swName, phNs) = layout
  where
    layout =
        layout_legend .~ legend
      $ layout_y_axis . laxis_title .~ yTitle
      $ layout_y_axis . laxis_title_style . font_size .~ 12
      $ layout_x_axis . laxis_override .~ axisGridHide
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_x_axis . laxis_title .~ xTitle
      $ layout_x_axis . laxis_title_style . font_size .~ 16
      $ layout_x_axis . laxis_generate .~ autoIndexAxis xPlotLabels
      $ layout_plots .~ [plotBars barsPhDist]
      $ def
        where
          legend = Just $
              legend_margin .~ 7
            $ legend_orientation .~ LORows 2
            $ def
              
          barsPhDist =
              plot_bars_titles .~ (T.unpack <$> phNs)
            $ plot_bars_values .~ addIndexes plotValues
            $ plot_bars_style .~ BarsStacked
            $ plot_bars_item_styles .~ (mkStyle <$> phNColors)
            $ def
            where
              mkStyle c = (solidFillStyle c, bstyles)
              bstyles = Just (solidLine 0.5 $ opaque black)
          xTitle = case scMetaScanKind exMeta of
            MetaEnvKDOEScan _ _ xAx -> case xAx of
              KDOEX -> xTitleBase
              EnvX -> "% lock of " <> xTitleBase
            _ -> ""
          yTitle = T.unpack swName ++ ": % in Phenotypes"
          plotValues = plotVF phNs <$> allPhDists
          plotVF ns phDM = (phDM M.!) <$> ns
          xPlotLabels = showRound <$> xRange
          showRound :: Double -> String
          showRound = printf "%.2f"
          (xTitleBase, xRange) = (head . scanXAxisData . scMetaScanKind) exMeta
          phNColors = (opaque . (cMap M.!)) <$> phNs

-- Pull out what being scanned over, and the range of the scan. 
scanXAxisData :: MetaScanKind -> [(String, [Double])]
scanXAxisData (MetaEnvSc inptName spread) = [(T.unpack inptName, spread)]
scanXAxisData (MetaKDOESc lockNodes spread) = [(T.unpack lockT, spread)]
    where
        lockT = (T.intercalate ", " . fmap lockF) lockNodes
        lockF (nN, nSt) = nN <> ":" <> showt nSt
scanXAxisData (MetaEnvKDOEScan envD kdoeD xAx) = case xAx of
    KDOEX -> scanXAxisData (uncurry MetaEnvSc envD) <>
        scanXAxisData (uncurry MetaKDOESc kdoeD)
    EnvX -> scanXAxisData (uncurry MetaKDOESc kdoeD) <>
        scanXAxisData (uncurry MetaEnvSc envD)
scanXAxisData (MetaTwoDEnvScan envD1 envD2 mkdD _) = case mkdD of
    Nothing -> (scanXAxisData (uncurry MetaEnvSc envD1)) <>
        (scanXAxisData (uncurry MetaEnvSc envD2))
    Just kdoeD -> (scanXAxisData (uncurry MetaEnvSc envD1)) <>
        (scanXAxisData (uncurry MetaEnvSc envD2)) <>
        (scanXAxisData (uncurry MetaKDOESc kdoeD))
scanXAxisData (MetaThreeDEnvScan envD1 envD2 envD3 _) =
    (scanXAxisData (uncurry MetaEnvSc envD1)) <>
    (scanXAxisData (uncurry MetaEnvSc envD2)) <>
    (scanXAxisData (uncurry MetaEnvSc envD3))

-- Do we overlay numbers on heat maps?
needOverlays :: SCExpMeta -> Bool
needOverlays exMeta = case scMetaScanKind exMeta of
    (MetaEnvSc _ _) -> False
    (MetaKDOESc _ _) -> False
    (MetaEnvKDOEScan _ _ _) -> False
    (MetaTwoDEnvScan _ _ _ needOLs) -> needOLs
    (MetaThreeDEnvScan _ _ _ needOLs) -> needOLs

-- Add a x-axis title to the top Layout in a column of an EnvKDOE Scan. 
xTitleColumn :: SCExpMeta
             -> String
             -> Double
             -> [Layout PlotIndex Double]
             -> [Layout PlotIndex Double]
xTitleColumn _ _ _ [] = []
xTitleColumn exMeta xAxisName xAxisValue (layout:layouts) = titledLayout:layouts
    where
        titledLayout = (layout_title .~ titleString) $ layout
        titleString = case scMetaScanKind exMeta of
            MetaEnvKDOEScan _ _ xAx -> case xAx of
                EnvX -> xAxisName <> ": " <> fpShow xAxisValue
                KDOEX -> xAxisName <> "@" <> fpShow (100 * xAxisValue) <> "%"
            _ -> ""
        fpShow :: Double -> String
        fpShow = printf "%.0f"

-- Add y-axis titles from the right Layouts in a row of an EnvKDOE Scan. 
stripLRows :: [Layout PlotIndex Double]
           -> [Layout PlotIndex Double]
stripLRows [] = []
stripLRows (leftL:lss) = leftL:(stripper <$> lss)
    where
        stripper layout = layout_y_axis . laxis_title .~ ""
                        $ layout

nodeAvgFig :: ColorMap
           -> SCExpMeta
           -> [ScanNodeStats]
           -> ScanNode
           -> AverageNodeValueFig
nodeAvgFig cMap exMeta allNStats scanNode = (,) scanNode $
    (fst . runBackendR dEnv . toRenderable) layout
    where
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 300 325
        layout =
            layout_title .~ (T.unpack layoutTitle)
          $ layout_title_style . font_size .~ 12
--           $ layout_axes_styles . axis_label_style . font_size .~ 6
          $ layout_x_axis . laxis_override .~ axisGridHide
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ layout_x_axis . laxis_generate .~ autoIndexAxis xPlotLabels
          $ layout_x_axis . laxis_title .~ xTitle
          $ layout_x_axis . laxis_title_style . font_size .~ 14
          $ layout_legend .~ legend
          $ layout_plots .~ [plotBars barsNodeAvgs]
          $ def
            where
                legend = Just def
        barsNodeAvgs =
            plot_bars_titles .~ (T.unpack <$> [scanNode])
          $ plot_bars_values .~ addIndexes plotValues
          $ plot_bars_item_styles .~ (mkStyle <$> nColor)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        xPlotLabels = showRound <$> xRange
        showRound :: Double -> String
        showRound = printf "%.2f"
--         Just the average values for now. 
        plotValues = (\(x, _) -> [x]) <$> plotData
        plotData = (flip (M.!) scanNode) <$> allNStats
        (xTitle, xRange) = (head . scanXAxisData . scMetaScanKind) exMeta
        layoutTitle = scExpName exMeta <> ", Time In: " <> scanNode
        nColor = (opaque . (cMap M.!)) <$> [scanNode]

envKDOENodeDia :: ColorMap
               -> ModelLayer
               -> SCExpMeta
               -> [ScanStats]
               -> Diagram B
envKDOENodeDia cMap mL exMeta scanStatss = nDia
    where
        nDia = frame 10 $ nTitleDia === nDiaBlock
        nTitleDia = center $ (text ((T.unpack . scExpName) exMeta)
            # fontSize (local (0.015 * nBlockWidth))
            # bold) <> strutY (0.03 * nBlockWidth)
        nBlockWidth = Diagrams.Prelude.width nDiaBlock
        nDiaBlock = (vsep 10 $ hsep 10 <$> nDiaArray) # center
        nDiaArray = mkDia <<$>> strippedNLayouts
        mkDia = fst . runBackendR dEnv . toRenderable
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 300 325
        strippedNLayouts = case swappedNLayouts of
            [] -> []
            stoppedLs:lss -> stoppedLs:(stripLRows <$> lss)
        swappedNLayouts = L.transpose titledNLayouts
        titledNLayouts = zipWith
            (xTitleColumn exMeta offAxisTitle) offAxisRange nLayouts
        (offAxisTitle, offAxisRange) = 
            (last . scanXAxisData . scMetaScanKind) exMeta
        nLayouts :: [[Layout PlotIndex Double]]
        nLayouts = envKDOENodeLayouts cMap mL exMeta <$> scanStatss
        
envKDOENodeLayouts :: ColorMap
                   -> ModelLayer
                   -> SCExpMeta
                   -> ScanStats
                   -> [Layout PlotIndex Double]
envKDOENodeLayouts cMap mL exMeta (_, _, allNStats) = nLayouts
    where
        nLayouts = envKDOENodeLayout cMap mL exMeta allNStats <$> scNodes
        scNodes = scExpScanNodes exMeta

envKDOENodeLayout :: ColorMap
                  -> ModelLayer
                  -> SCExpMeta
                  -> [ScanNodeStats]
                  -> ScanNode
                  -> Layout PlotIndex Double
envKDOENodeLayout cMap mL exMeta allNStats scNName = layout
    where
        layout =
            layout_y_axis . laxis_generate .~ scaledAxis def yBoundsPair
          $ layout_y_axis . laxis_override .~ axisGridHide
          $ layout_x_axis . laxis_override .~ axisGridHide
          $ layout_x_axis . laxis_generate .~ autoIndexAxis xPlotLabels
          $ layout_x_axis . laxis_title .~ xTitle
          $ layout_x_axis . laxis_title_style . font_size .~ 14
          $ layout_legend .~ legend
          $ layout_plots .~ [plotBars barsNodeAvgs]
          $ def
            where
                legend = Just def
        barsNodeAvgs =
            plot_bars_titles .~ (T.unpack <$> [scNName])
          $ plot_bars_values .~ addIndexes plotValues
          $ plot_bars_item_styles .~ (mkStyle <$> nColor)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        xPlotLabels = showRound <$> xRange
        showRound :: Double -> String
        showRound = printf "%.2f"
--         Just the average values for now. 
        plotValues = (\(x, _) -> [x]) <$> plotData
        plotData = (flip (M.!) scNName) <$> allNStats
        (xTitle, xRange) = (head . scanXAxisData . scMetaScanKind) exMeta
        nColor = (opaque . (cMap M.!)) <$> [scNName]
        yBoundsPair :: (Double, Double)
        yBoundsPair = (0, fromIntegral $ lRangeMap M.! scNName)
        lRangeMap = layerRanges mL

scHeatMapDias :: ModelLayer
              -> DoOverlayValues
              -> M.HashMap ScanSwitch [PhenotypeName]
              -> SCExpMeta
              -> [ScanStats]
              -> ([StopDistributionHMFig], [SwitchHMFig], [NodeHMFig])
scHeatMapDias mL overLayVs switchMap exMeta scanStatss =
    (stopPhFigs, swfigs, nodeFigs)
    where
        nodeFigs = nodeHeatMapF <$> nodeValues
        nodeHeatMapF nodePair@(scNN, _) = ("Node_" <> scNN, nHMFig)
            where
                nHMFig = scHeatMapDia overLayVs boundsP rangeData nodePair
                boundsP = (0, fromIntegral $ lRangeMap M.! scNN)
        nodeValues :: [(ScanNode, [[Double]])]
--         Ditch the StdDevs for now. 
        nodeValues = (fmap . fmap . fmap . fmap) fst $
            mkSSNodeValues (thdOf3 <$> scanStatss) <$> scNNames
        scNNames = scExpScanNodes exMeta
        lRangeMap = layerRanges mL
        stopPhFigs = stopHMF <$> stopPlotValuess
        stopHMF stpPair = ("StopAt_" <> (fst stpPair), stpHMFig)
            where stpHMFig = scHeatMapDia overLayVs (0, 1) rangeData stpPair
        swfigs = swHeatMapF <$> phValues
        swHeatMapF swPair = ("Switch_" <> (fst swPair), swHMFig)
            where swHMFig = scHeatMapDia overLayVs (0, 1) rangeData swPair
        rangeData = case (take 2 . scanXAxisData . scMetaScanKind) exMeta of
            [] -> (("blank", [0 :: Double, 1]), ("blank", [0 :: Double, 1]))
            [x] -> (x, x)
            x:y:_ -> (x, y)
        phValues :: [(PhenotypeName, [[Double]])]
        phValues =  (zip phNames . L.transpose . fmap L.transpose) barePhValues
        barePhValues :: [[[Double]]]
        barePhValues = (fmap (plotVF phNames) . sndOf3) <$> scanStatss
        plotVF ns phDM = (phDM M.!) <$> ns
        phNames = (concatMap (switchMap M.!) . scExpSwitches) exMeta
        stopPlotValuess :: [(T.Text, [[Double]])]
        stopPlotValuess = mkStopPhValues2D stopDss
        stopDss :: [[([(PhenotypeName, Double)], Double)]]
        stopDss = (fmap (BF.first stopDF) . fstOf3) <$> scanStatss
        stopDF stM = zip stopPhNames ((stM M.!) <$> stopPhNames)
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

scHeatMapDia :: DoOverlayValues
             -> (Double, Double)
             -> ((String, [Double]), (String, [Double]))
             -> (T.Text, [[Double]])
             -> Diagram B
scHeatMapDia overLayVs colorLimitPair rangeData (titleT, hmValues)
  | overLayVs && (fst colorLimitPair < 0) =
        (gridNudge (scHeatMapNumberDVDia hmValues # center)) <> hmDia
  | overLayVs && (fst colorLimitPair == 0) =
        (gridNudge (scHeatMapNumberDia hmValues # center)) <> hmDia
  | otherwise = hmDia
  where
    gridNudge = Diagrams.Prelude.translate (V2 (2 :: Double) (8.25 :: Double))
    hmDia = center $ P.renderAxis $ P.r2Axis &~ do
      let ((xTitle, xRange), (yTitle, yRange)) = rangeData 
          xSize = (fromIntegral . length) xRange
          xRanks = zip [1..] xRange
          xStepSize = 1.0 / xSize
          ySize = (fromIntegral . length) yRange
          yRanks = zip [1..] yRange
          yStepSize = 1.0 / ySize
          textDouble = 10
          verticalTextF :: TextAlignment Double
                        -> String
                        -> QDiagram Cairo V2 Double Any
          verticalTextF txtAln lblTxt = mkText txtAln lblTxt # rotateBy (1/4)
          cMap
            | fst colorLimitPair == -1 = divergingP
            | otherwise = P.viridis
      P.display P.colourBar
      P.axisColourMap .= cMap
      P.titleText .= T.unpack titleT
      P.titleStyle . _fontSize .= local (1.75 * textDouble)
      P.xLabel .= xTitle
      P.xAxis . P.axisLabelStyle . _fontSize .= local (1.5 * textDouble)
      P.xAxis . P.axisLabelGap .= 3 * textDouble
      P.xAxis . P.tickLabelStyle . _fontSize .= local textDouble
      P.xAxis . axisLine . axisLineStyle . _lineWidth .= thin
      P.xAxis . P.tickLabelPositions .= (mkTickLabel xSize <$> xRanks)
      P.yLabel .= yTitle
      P.yAxis . P.axisLabelStyle . _fontSize .= local (1.5 * textDouble)
      P.yAxis . P.axisLabelGap .= 5.5 * textDouble
      P.yAxis . P.axisLabelTextFunction .= verticalTextF
      P.yAxis . P.tickLabelStyle . _fontSize .= local textDouble
      P.yAxis . axisLine . axisLineStyle . _lineWidth .= thin
      P.yAxis . P.tickLabelPositions .= (mkTickLabel ySize <$> yRanks)
      P.colourBar . P.tickLabelStyle . _fontSize .= local textDouble
      P.colourBar . P.colourBarStyle . _lineWidth .= thin    
      P.axisExtend .= P.noExtend
      P.heatMap hmValues $ do
        P.heatMapSize .= V2 xStepSize yStepSize
        P.heatMapLimits .= (Just colorLimitPair)
      P.colourBarRange .= colorLimitPair

-- Make a tick number label for heat maps. This exists for non-linear scan
-- ranges (e.g. [0.1, 0.75, 0.9]). scanPlace is 1-indexed. 
mkTickLabel :: Double -> (Double, Double) -> (Double, String)
mkTickLabel scanSize (scanPlace, scanValue) = (tickLoc, showRound scanValue)
    where
        tickLoc = (scanPlace - 0.5)/scanSize
        showRound :: Double -> String
        showRound = printf "%.2f"

-- Number overlay for sequential heat maps. 
scHeatMapNumberDia :: [[Double]] -> Diagram B
scHeatMapNumberDia [[]] = mempty
scHeatMapNumberDia hmValuess = dia # center
    where
        dia = (vsep vSepD . reverse . fmap (hsep hSepD)) numDias
        vSepD, hSepD :: Double
        vSepD = ((530 * 0.756) / vSepL) - textScale
        vSepL = (fromIntegral . length) hmValuess
        hSepD
            | hSepL == 0 = 16.5
            | otherwise = ((587 * 0.683) / hSepL) - (1.17 * textScale)
        hSepL = (fromIntegral . length . head) hmValuess
        numDias = stringT <<$>> hmValuess
        showRound :: Double -> String
        showRound = printf "%.2f"
        stringT :: Double -> Diagram B
        stringT x
            | x >= 0.45 = (stringText' textScale . showRound) x
            | otherwise = (stringText'' textScale . showRound) x
        stringText'' :: Double -> String -> Diagram B
        stringText'' tHt t = F.svgText def t # F.fit_height tHt
                                      # F.set_envelope
                                      # fillColor white
                                      # lineWidth none
                                      # center
                                      # bold
        textScale = 20

-- Number overlay for diverging heat maps. 
scHeatMapNumberDVDia :: [[Double]] -> Diagram B
scHeatMapNumberDVDia [[]] = mempty
scHeatMapNumberDVDia hmValuess = dia # center
    where
        dia = (vsep vSepD . reverse . fmap (hsep hSepD)) numDias
        vSepD, hSepD :: Double
        vSepD = ((530 * 0.756) / vSepL) - textScale
        vSepL = (fromIntegral . length) hmValuess
        hSepD
            | hSepL == 0 = 16.5
            | otherwise = ((587 * 0.683) / hSepL) - (1.5 * textScale)
        hSepL = (fromIntegral . length . head) hmValuess
        numDias = stringT <<$>> hmValuess
        showRound :: Double -> String
        showRound = printf "%.3f"
        stringT :: Double -> Diagram B
        stringT x = (stringText' textScale . showRound) x
        textScale = 20

scDifferenceHeatMapDia :: ModelLayer
                       -> DoOverlayValues
                       -> M.HashMap ScanSwitch [PhenotypeName]
                       -> SCExpMeta
                       -> ([[ScanStats]], [[ScanStats]])
                       -> ([StopDistributionHMFig], [SwitchHMFig], [NodeHMFig])
scDifferenceHeatMapDia mL overLayVs switchMap exMeta scBsPair =
    (stopPlotFigs, swFigs, nFigs)
    where
        nFigs = scDiffHMBlock "Node" overLayVs rangeData <$> nodeVals
        nodeVals = zipWith3 ndTupleValsF nodeValuesWOM nodeValuesWM diffNValues
        ndTupleValsF x y z = (tagPair, (snd x, snd y, snd z))
            where tagPair = (fst x, (fromIntegral . (lRangeMap M.!)) (fst x))
        diffNValues = zipWith diffF nodeValuesWM nodeValuesWOM
        nodeValuesWOM, nodeValuesWM :: [(ScanNode, [[[Double]]])]
        (nodeValuesWOM, nodeValuesWM) = isoBimap nodeValuesF scBsPair
        nodeValuesF :: [[ScanStats]] -> [(ScanNode, [[[Double]]])]
        nodeValuesF = concatF . removeStdDevs . seqAF . pickerF
        concatF :: [[(ScanNode, [[Double]])]] -> [(ScanNode, [[[Double]]])]
        concatF xs = concatF' <$> xs
            where
                concatF' [] = ("", [])
                concatF' excessPs = ((fst . head) excessPs, snd <$> excessPs)
        seqAF tvs = (sequenceA (mkSSNodeValues <$> tvs)) <$> scNNames
--         Ditch the StdDevs for now. 
        removeStdDevs = (fmap . fmap . fmap . fmap . fmap) fst
--         Pull out the ScanNodeStats
        pickerF = (fmap . fmap) thdOf3
        scNNames = scExpScanNodes exMeta
        lRangeMap = layerRanges mL
        swFigs = scDiffHMBlock "Switch" overLayVs rangeData <$> phVals
        phVals = zipWith3 swTupleValsF phValuessWOM phValuessWM diffPhValues
        swTupleValsF x y z = ((fst x, 1), (snd x, snd y, snd z))
        diffPhValues = zipWith diffF phValuessWM phValuessWOM
        (phValuessWOM, phValuessWM) = isoBimap phValuesF bareValuessPair
        phValuesF = zip phNames . L.transpose . fmap L.transpose .
            (fmap . fmap) L.transpose
        bareValuessPair = isoBimap bValuesF scBsPair
        bValuesF = (fmap . fmap) (fmap (plotVF phNames) . sndOf3)
        plotVF ns phDM = (phDM M.!) <$> ns
        phNames = (concatMap (switchMap M.!) . scExpSwitches) exMeta
        stopPlotFigs = scDiffHMBlock "StopAt" overLayVs rangeData <$> stPlValues
        stPlValues = zipWith3 tupleValsF
            stopPlotValuesWOM stopPlotValuesWM diffStopPlotValues
        tupleValsF x y z = (fst x, (snd x, snd y, snd z))
        diffStopPlotValues = zipWith diffF stopPlotValuesWOM stopPlotValuesWM
        diffF (diffN, vs) (_, mVs) = (diffN, (zipWith . zipWith . zipWith) (-)
            vs mVs)
        (stopPlotValuesWOM, stopPlotValuesWM) =
            isoBimap mkStopPhValues3D stopDsssPair
        stopDsssPair :: ([[[([(PhenotypeName, Double)], Double)]]], 
                     [[[([(PhenotypeName, Double)], Double)]]])
        stopDsssPair = isoBimap ((fmap . fmap)
                            (fmap (BF.first stopDF) . fstOf3)) scBsPair
        stopDF stM = zip stopPhNames ((stM M.!) <$> stopPhNames)
        rangeData :: (((String,[Double]),(String,[Double])),(String, [Double]))
        rangeData = case (take 3 . scanXAxisData . scMetaScanKind) exMeta of
            [] -> ((blankR, blankR), blankR)
            [x] -> ((x,x),x)
            [x,y] -> ((x,y),y)
            x:y:z:_ -> ((x,y),z)
        blankR = ("blank", [0 :: Double, 1])
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

-- Produce a figure for a mutant ThreeDEnvSc.
scDiffHMBlock :: T.Text
              -> DoOverlayValues
              -> (((String,[Double]),(String,[Double])),(String, [Double]))
              -> ((T.Text, Double), ([[[Double]]], [[[Double]]], [[[Double]]]))
              -> NamedFigure
scDiffHMBlock fileTag
              overLayVs
              range3Data
              ((featureName, rTop), (plainD, mutantD, diffD)) =
    (fileTag <> "_" <> featureName, fig)
    where
        fig = vsep 2 [dTitle # center, blockBlock # center]
        dTitle = tText' 40 featureName
        blockBlock = vsep 5 [plainDRow, mutantDRow, diffDRow]
        diffDRow = hsep 5 $ scHeatMapDia overLayVs (-rTop, rTop) range2Data <$>
            (zip rowTitles diffD)
        mutantDRow = hsep 5 $ scHeatMapDia overLayVs (0, rTop) range2Data <$>
            (zip rowTitles mutantD)
        plainDRow = hsep 5 $ scHeatMapDia overLayVs (0, rTop) range2Data <$>
            (zip rowTitles plainD)
        rowTitles = (T.pack . ((zTitle ++ "@") ++) . show) <$> zRange
        (range2Data, (zTitle, zRange)) = range3Data

sc3DHeatMapDia :: ModelLayer
               -> DoOverlayValues
               -> M.HashMap ScanSwitch [PhenotypeName]
               -> SCExpMeta
               -> [[ScanStats]]
               -> ([StopDistributionHMFig], [SwitchHMFig], [NodeHMFig])
sc3DHeatMapDia mL overLayVs switchMap exMeta scanStatsss =
    (stPhFigs, swFigs, nodeFigs)
    where
        nodeFigs = sc3DBlock "Node" overLayVs rangeData <$> rangedNodeValues
        rangedNodeValues :: [((ScanNode, Double), [[[Double]]])]
        rangedNodeValues = concatF <$> nodeValues
        concatF [] = (("", 1), [])
        concatF excessPs = ((plainN, rTop), snd <$> excessPs)
            where
                rTop = fromIntegral $ lRangeMap M.! plainN
                plainN = (fst . head) excessPs
        nodeValues :: [[(ScanNode, [[Double]])]]
--         Ditch the StdDevs for now. 
        nodeValues = (fmap . fmap . fmap . fmap . fmap) fst $
            (sequenceA (mkSSNodeValues <$> allScanNodeStatssss)) <$> scNNames
        allScanNodeStatssss = thdOf3 <<$>> scanStatsss
        scNNames = scExpScanNodes exMeta
        lRangeMap = layerRanges mL
        swFigs = sc3DBlock "Switch" overLayVs rangeData <$> phValuess
        phValuess = phValuesF bareValuess
        phValuesF = zip (zip phNames (repeat 1)) . L.transpose .
            fmap L.transpose . (fmap . fmap) L.transpose
        bareValuess = (fmap (plotVF phNames) . sndOf3) <<$>> scanStatsss
        plotVF ns phDM = (phDM M.!) <$> ns
        phNames = (concatMap (switchMap M.!) . scExpSwitches) exMeta
        stPhFigs = sc3DBlock "StopAt" overLayVs rangeData <$> stopPlotValuesss
        stopPlotValuesss :: [((T.Text, Double), [[[Double]]])]
        stopPlotValuesss = mkStopPhValues3D stopDsss
        stopDsss :: [[[([(PhenotypeName, Double)], Double)]]]
        stopDsss = (fmap (BF.first stopDF) . fstOf3) <<$>> scanStatsss
        stopDF stM = zip stopPhNames ((stM M.!) <$> stopPhNames)
        rangeData = case (take 3 . scanXAxisData . scMetaScanKind) exMeta of
            [] -> ((blankR, blankR), blankR)
            [x] -> ((x,x),x)
            [x,y] -> ((x,y),y)
            x:y:z:_ -> ((x,y),z)
        blankR = ("blank", [0 :: Double, 1])
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

-- Produce a figure for a non-mutant ThreeDEnvSc.
sc3DBlock :: T.Text
          -> DoOverlayValues
          -> (((String,[Double]),(String,[Double])),(String, [Double]))
          -> ((T.Text, Double), [[[Double]]])
          -> NamedFigure
sc3DBlock fileTag overLayVs range3Data ((plainN, rTop), plainD) =
    (fileTag <> "_" <> plainN, vsep 2 figs)
    where
        figs = [titleFig, plainDRow]
        titleFig = tText' 45 plainN # center
        plainDRow = hsep 5 (scHeadDiaF <$> (zip rowTitles plainD)) # center
        scHeadDiaF = scHeatMapDia overLayVs (0, rTop) range2Data
        rowTitles = (T.pack . ((zTitle ++ "@") ++) . show) <$> zRange
        (range2Data, (zTitle, zRange)) = range3Data

-- Prepare DMNode average and StdDev values for heat map figures. 
mkSSNodeValues :: [[ScanNodeStats]]
               -> ScanNode
               -> (ScanNode, [[(RealNodeState, StdDev)]])
mkSSNodeValues allNStatss snNName = (snNName, nodeValues)
    where nodeValues = (flip (M.!) snNName) <<$>> allNStatss


-- Prepare stop Phenotype data for 2D heat map Scans. 
mkStopPhValues2D :: [[([(PhenotypeName, Double)], Double)]]
               -> [(T.Text, [[Double]])]
mkStopPhValues2D stopData = noStopDataPair : (mkStPhV <$> tpStPhData)
    where
        mkStPhV [] = ("", [])
        mkStPhV xs = ((fst . head . head) xs, snd <<$>> xs)
        tpStPhData = (L.transpose . fmap L.transpose) stPhData
        noStopDataPair = ("NoStopPhenotype", noStopData)
--         tnNoStData = L.transpose noStopData
        noStopData = snd <<$>> stopData
        stPhData = fst <<$>> stopData

-- Prepare stop Phenotype data for 3D heat map Scans. 
mkStopPhValues3D :: [[[([(PhenotypeName, Double)], Double)]]]
                 -> [((T.Text, Double), [[[Double]]])]
mkStopPhValues3D stopData = noStopDataPair : (mkStPhV <$> tpStPhData)
    where
        mkStPhV [] = (("", 0), [])
        mkStPhV xs = (((fst . head . head . head) xs, 1),
                     (fmap . fmap . fmap) snd xs)
        tpStPhData = (L.transpose . fmap L.transpose. (fmap . fmap) L.transpose)
            stPhData
        noStopDataPair = (("NoStopPhenotype", 1), noStopData)
--         tnNoStData =  noStopData
        noStopData = (fmap . fmap . fmap) snd stopData
        stPhData = (fmap . fmap . fmap) fst stopData

tText' :: Double -> T.Text -> Diagram B
tText' tHt t = F.svgText def (T.unpack t) # F.fit_height tHt
                                      # F.set_envelope
                                      # fillColor black
                                      # lineWidth none
                                      # center
                                      # bold

stringText' :: Double -> String -> Diagram B
stringText' tHt t = F.svgText def t # F.fit_height tHt
                                      # F.set_envelope
                                      # fillColor black
                                      # lineWidth none
                                      # center
                                      # bold

-- A diverging pallete for differences of heatmap data. 
divergingP :: P.ColourMap
divergingP = P.colourMap $ zip [1..] --[blue, white, red]
    [sRGB24 202 0 32, sRGB24 244 165 130, sRGB24 247 247 247, sRGB24 146 197 222
    , sRGB24 5 113 176]
