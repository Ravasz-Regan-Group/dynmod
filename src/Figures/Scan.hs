{-# LANGUAGE OverloadedStrings #-}


module Figures.Scan
    ( scRunDia
    , ScanExpFigure (..)
    , BaseScanFigs(..)
    ) where    

import Types.DMModel
import Types.DMInvestigation
import Types.Figures
import Utilities
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Graphics.SVGFonts as F
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Plots as P
import qualified Data.Colour.Names as CN
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as B
import qualified Data.Text as T
import qualified Data.List as L
import GHC.IO (unsafePerformIO)



data ScanExpFigure = EnvScFig BaseScanFigs
                   | KDOEScFig BaseScanFigs
                   | EnvKDOESc (Diagram B)
                   | TwoDEnvScWWOKDOE [Diagram B]
                   | ThreeDEnvSc [Diagram B]

data BaseScanFigs = BSFgs StopDistributionFig [TimeInSwitchPhsFig]

type StopDistributionFig = Diagram B
type TimeInSwitchPhsFig = Diagram B

scRunDia :: PhColorMap
         -> ModelMapping
         -> SCExpMeta
         -> ScanResult
         -> ScanExpFigure
scRunDia phCMap mM exMeta scRes = case scRes of
    (SKREnv scBundle) -> EnvScFig $ baseScDia phCMap switchMap exMeta scBundle
    (SKRKDOE scBundle) -> KDOEScFig $ baseScDia phCMap switchMap exMeta scBundle
    (SKREnvKDOE scBundles) -> EnvKDOESc (hsep 5 figs)
        where
            figs = zipWith stackBaseDias baseFigs offAxisPairs
            offAxisPairs = zip (repeat offAxisTitle) offAxisRange
            (offAxisTitle, offAxisRange) =
                (last . scanXAxisData . scMetaScanKind) exMeta
            baseFigs = baseScDia phCMap switchMap exMeta <$> scBundles
    (SKRTwoEnvWithoutKDOE scBundles) -> TwoDEnvScWWOKDOE $
        scHeatMapDias switchMap exMeta scBundles
    (SKRTwoEnvWithKDOE scBundless) -> TwoDEnvScWWOKDOE joinedFigs
        where
--     The L.transpose is so that the different phenotypes are the top level, 
--     and the KDOE scans are joined into single Diagram Bs. 
            joinedFigs = hsep 2 <$> (L.transpose figs)
            figs = zipWith labelMutantHMDias hmFigs offAxisPairs
            offAxisPairs = zip (repeat offAxisTitle) offAxisRange
            (offAxisTitle, offAxisRange) =
                (last . scanXAxisData . scMetaScanKind) exMeta
            hmFigs = scHeatMapDias switchMap exMeta <$> scBundless
--  Take care to distinguish between when [WildTypeVsMutantAlt] are [] and not. 
    (SKRThreeEnv scBundless) -> case snd scBundless of
        Just scbs -> ThreeDEnvSc $ scDifferenceHeatMapDia switchMap exMeta sPair
            where sPair = (fst scBundless, scbs)
        Nothing -> ThreeDEnvSc $ sc3DHeatMapDia switchMap exMeta plainBundless
            where plainBundless = fst scBundless
    where
        switchMap = M.fromList nonEmptySwPhNs
        nonEmptySwPhNs = (fmap . fmap . fmap) phenotypeName nonEmptySwPhs
        nonEmptySwPhs = snd <<$>> (nonEmptyPhenotypes mM)

labelMutantHMDias :: [Diagram B] -> (String, Double) -> [Diagram B]
labelMutantHMDias hmFigs (offAxisTitle, stepDouble) = labelMutantHMD <$> hmFigs
    where
        labelMutantHMD fig = fig === strutY 5 === textFig
        textFig = tText' 5 (T.pack offAxisTitle <> ": " <> tShow stepDouble)

stackBaseDias :: BaseScanFigs -> (String, Double) -> Diagram B
stackBaseDias (BSFgs stopDFig swDFigs) (offAxisTitle, stepDouble) = vsep 5 figs
    where
        figs = swDFigs <> [stopDFig, textFig]
        textFig = tText' 5 (T.pack offAxisTitle <> ": " <> tShow stepDouble)

baseScDia :: PhColorMap
          -> M.HashMap ScanSwitch [PhenotypeName]
          -> SCExpMeta
          -> [[Timeline]]
          -> BaseScanFigs
baseScDia cMap switchMap exMeta scanRuns = BSFgs stopDistFig timeInSwFigs
    where
        timeInSwFigs = timeInSwPhsDia cMap exMeta trScanRuns <$> scanSwsPairs
        scanSwsPairs = (zip scanSws . fmap (switchMap M.!)) scanSws
        scanSws = scExpSwitches exMeta
        stopDistFig = (fst . runBackendR dEnv . toRenderable) layout
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 1200 1200
        layout =
            layout_title .~ (T.unpack . scExpName) exMeta
          $ layout_title_style . font_size .~ 24
          $ layout_x_axis . laxis_title .~ xTitle
          $ layout_legend .~ legend
          $ layout_plots .~ [plotBars barsStopD]
          $ def
            where
                legend = Just $
                    legend_label_style . font_size .~ 12
                  $ def
        barsStopD =
            plot_bars_titles .~ (T.unpack <$> ("NoStopPhenotype":stopPhNames))
          $ plot_bars_values .~ (zip xRange plotValues)
          $ plot_bars_style .~ BarsStacked
          $ plot_bars_item_styles .~ (mkStyle <$> stopDColors)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        stopDColors = (fmap opaque . (CN.white:) . fmap (cMap M.!)) stopPhNames
        (xTitle, xRange) = (head . scanXAxisData . scMetaScanKind) exMeta
        plotValues = (\(xs, y) -> y:(snd <$> xs)) <$> stopDs
        stopDs = stopDistribution stopPhNames <$> stopDurationss
        stopDurationss = (zipWith . zipWith) (,) runDurationss stoppedAtPhss
        runDurationss = B.length <<$>> trScanRuns
        trScanRuns = trimPhStoppedTmln stopPhNames <<$>> scanRuns
        stoppedAtPhss :: [[[PhenotypeName]]]
        stoppedAtPhss = getStopPhs stopPhNames <<$>> scanRuns
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

timeInSwPhsDia :: ColorMap
               -> SCExpMeta
               -> [[Timeline]]
               -> (ScanSwitch, [PhenotypeName])
               -> TimeInSwitchPhsFig
timeInSwPhsDia cMap exMeta trScanRuns (scSw, phNs) =
    (fst . runBackendR dEnv . toRenderable) layout
    where
        dEnv = unsafePerformIO $ defaultEnv vectorAlignmentFns 1200 1200
        layout =
            layout_title .~ (T.unpack layoutTitle)
          $ layout_title_style . font_size .~ 24
          $ layout_x_axis . laxis_title .~ xTitle
          $ layout_legend .~ legend
          $ layout_plots .~ [plotBars barsPhDist]
          $ def
            where
                legend = Just $
                    legend_label_style . font_size .~ 12
                  $ def
        barsPhDist =
            plot_bars_titles .~ (T.unpack <$> phNs)
          $ plot_bars_values .~ (zip xRange plotValues)
          $ plot_bars_style .~ BarsStacked
          $ plot_bars_item_styles .~ (mkStyle <$> phNColors)
          $ def
            where
                mkStyle c = (solidFillStyle c, bstyles)
                bstyles = Just (solidLine 0.5 $ opaque black)
        plotValues = phDistribution phNs <$> trScanRuns
--         trSwapped = trInit2 <> [trLast, trL2]
--         trInit2 = init trInit
--         trL2 = last trInit
--         trLast = last trScanRuns
--         trInit = init trScanRuns
        layoutTitle = scExpName exMeta <> ", Time In: " <> scSw
        (xTitle, xRange) = (head . scanXAxisData . scMetaScanKind) exMeta
        phNColors = (opaque . (cMap M.!)) <$> phNs

-- What fraction of all the time steps in a [Timeline] is each Phenotype
-- present? 
phDistribution :: [PhenotypeName]
               -> [Timeline]
               -> [Double]
phDistribution switchPhNs scanRun = snd <$> presPhList
    where
        presPhList = (sortWithOrderOn fst switchPhNs . M.toList) presPhMap
        presPhMap = M.map (\x -> fromIntegral x / divisor) rMap
        rMap = L.foldl' foldF mapAccum allStepps
        mapAccum = M.fromList $ zip switchPhNs $ repeat (0 :: Int)
        foldF phMap (_, presentPhNs) = L.foldl' foldF' phMap presentPhNs
            where foldF' phM phN = M.adjust (+1) phN phM
        allStepps = B.concat scanRun
        divisor = ((fromIntegral . sum . fmap B.length) scanRun) :: Double

-- Pull out what being scanned over, and the range of the scan. 
scanXAxisData :: MetaScanKind -> [(String, [Double])]
scanXAxisData (MetaEnvSc inptName spread) = [(T.unpack inptName, spread)]
scanXAxisData (MetaKDOESc lockNodes spread) = [(T.unpack lockT, spread)]
    where
        lockT = (T.intercalate ", " . fmap lockF) lockNodes
        lockF (nN, nSt) = nN <> ":" <>tShow nSt
scanXAxisData (MetaEnvKDOEScan envD kdoeD xAx) = case xAx of
    EnvX -> scanXAxisData (uncurry MetaEnvSc envD) <>
        scanXAxisData (uncurry MetaKDOESc kdoeD)
    KDOEX -> scanXAxisData (uncurry MetaKDOESc kdoeD) <>
        scanXAxisData (uncurry MetaEnvSc envD)
scanXAxisData (MetaTwoDEnvScan envD1 envD2 mkdD) = case mkdD of
    Nothing -> (scanXAxisData (uncurry MetaEnvSc envD1)) <>
        (scanXAxisData (uncurry MetaEnvSc envD2))
    Just kdoeD -> (scanXAxisData (uncurry MetaEnvSc envD1)) <>
        (scanXAxisData (uncurry MetaEnvSc envD2)) <>
        (scanXAxisData (uncurry MetaKDOESc kdoeD))
scanXAxisData (MetaThreeDEnvScan envD1 envD2 envD3) =
    (scanXAxisData (uncurry MetaEnvSc envD1)) <>
    (scanXAxisData (uncurry MetaEnvSc envD2)) <>
    (scanXAxisData (uncurry MetaEnvSc envD3))

-- What fraction of all the time steps in a [Timeline] is each stop Phenotype
-- responsible for stopping, plus the fraction of time steps in the [Timeline]
-- which a Max_N or Relevant_N is responsible for stopping.
stopDistribution :: [PhenotypeName]
                 -> [(Int, [PhenotypeName])]
                 -> ([(PhenotypeName, Double)], Double)
stopDistribution stopPhNs stopDurations = (stPhList, fracMaxStop)
    where
        stPhList = (sortWithOrderOn fst stopPhNs . M.toList) stPhMap
        fracMaxStop = fromIntegral rMaxStop / fracDiv
        stPhMap = M.map (\x -> fromIntegral x / fracDiv) rMap
        fracDiv = (fromIntegral divisor) :: Double
        (rMap, rMaxStop, divisor) = L.foldl' foldF (mapAccum, 0, 0) stopDurations
        mapAccum = M.fromList $ zip stopPhNs $ repeat 0
        foldF (durMap, maxOutAccum, totalAccum) (dur, phNs) = case phNs of
            [] -> (durMap, maxOutAccum + dur, totalAccum + dur)
            _ -> (L.foldl' foldF' durMap phNs, maxOutAccum, totalAccum + dur)
                where foldF' dMap phN = M.adjust (+ dur) phN dMap

getStopPhs :: [PhenotypeName] -> Timeline -> [PhenotypeName]
getStopPhs stopPHNs = filter (flip elem stopPHNs) . snd . B.last

-- If a Timeline was stopped at a stop Phenotype, we don't want its last step. 
trimPhStoppedTmln :: [PhenotypeName] -> Timeline -> Timeline
trimPhStoppedTmln stopPHNs tmln = case getStopPhs stopPHNs tmln of
    [] -> tmln
    _  -> B.init tmln

scHeatMapDias :: M.HashMap ScanSwitch [PhenotypeName]
              -> SCExpMeta
              -> [[[Timeline]]]
              -> [Diagram B]
scHeatMapDias switchMap exMeta scanRunss = scHeatMapDia rangeData <$> phValues
    where
        rangeData = case (take 2 . scanXAxisData . scMetaScanKind) exMeta of
            [] -> (("blank", [0 :: Double, 1]), ("blank", [0 :: Double, 1]))
            [x] -> (x, x)
            x:y:_ -> (x, y)
        phValues :: [(PhenotypeName, [[Double]])]
        phValues = zip phNames $ (L.transpose . fmap L.transpose) bareValues
        bareValues = phDistribution phNames <<$>> trScanRunss
        phNames = (concatMap (switchMap M.!) . scExpSwitches) exMeta
        trScanRunss = (fmap . fmap . fmap) (trimPhStoppedTmln stopPhNames)
                        scanRunss
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

scHeatMapDia :: ((String, [Double]), (String, [Double]))
             -> (T.Text, [[Double]])
             -> Diagram B
scHeatMapDia rangeData (titleT, hmValues) = P.renderAxis $ P.r2Axis &~ do
    let ((xTitle, xRange), (yTitle, yRange)) = rangeData 
--         xStepSize = abs $ (xRange L.!! 1) - (xRange L.!! 0)
--         yStepSize = abs $ (yRange L.!! 1) - (yRange L.!! 0)
    P.display P.colourBar
    P.titleText .= T.unpack titleT
    P.axisExtend .= P.noExtend
    P.axisColourMap .= P.viridis
    P.xLabel .= xTitle
    P.xMin .= Just (head xRange)
    P.xMax .= Just (last xRange)
    P.yLabel .= yTitle
    P.yMin .= Just (head yRange)
    P.yMax .= Just (last yRange)
    P.heatMap' hmValues
--         $ P.heatMapSize .= V2 xStepSize yStepSize


scDifferenceHeatMapDia :: M.HashMap ScanSwitch [PhenotypeName]
                       -> SCExpMeta
                       -> (([[[[Timeline]]]], [[[[Timeline]]]]))
                       -> [Diagram B]
scDifferenceHeatMapDia switchMap exMeta scBsPair = figs
    where
        figs = scDiffHMBlock rangeData <$> phVals
        phVals = zipWith3 phValsF (fst phValuessPair) (snd phValuessPair)
                                                            diffPhValues
        phValsF x y z = (fst x, (snd x, snd y, snd z))
        diffPhValues = uncurry (zipWith diffF) phValuessPair
        diffF (phN, vs) (_, mVs) = (phN, (zipWith . zipWith . zipWith) (-)
            vs mVs)
        phValuessPair = isoBimap phValuesF bareValuessPair
        phValuesF = zip phNames . L.transpose . fmap L.transpose .
            (fmap . fmap) L.transpose
        bareValuessPair = isoBimap bValuesF trimmedSCBsPair
        bValuesF = (fmap . fmap . fmap) (phDistribution phNames)
        phNames = (concatMap (switchMap M.!) . scExpSwitches) exMeta
        trimmedSCBsPair = isoBimap trimmer scBsPair
        trimmer = (fmap . fmap . fmap . fmap) (trimPhStoppedTmln stopPhNames)
        rangeData :: (((String,[Double]),(String,[Double])),(String, [Double]))
        rangeData = case (take 3 . scanXAxisData . scMetaScanKind) exMeta of
            [] -> ((blankR, blankR), blankR)
            [x] -> ((x,x),x)
            [x,y] -> ((x,y),y)
            x:y:z:_ -> ((x,y),z)
        blankR = ("blank", [0 :: Double, 1])
        stopPhNames = (fmap fst . stopPhenotypes) exMeta
        
scDiffHMBlock :: (((String,[Double]),(String,[Double])),(String, [Double]))
              -> (PhenotypeName, ([[[Double]]], [[[Double]]], [[[Double]]]))
              -> Diagram B
scDiffHMBlock range3Data (phN, (plainD, mutantD, diffD)) = fig
    where
        fig = vsep 2 [phTitle, blockBlock]
        phTitle = tText' 5 phN
        blockBlock = vsep 5 [plainDRow, mutantDRow, diffDRow]
        diffDRow = hsep 5 $ scHeatMapDia range2Data <$> (zip rowTitles diffD)
        mutantDRow =hsep 5 $ scHeatMapDia range2Data <$> (zip rowTitles mutantD)
        plainDRow = hsep 5 $ scHeatMapDia range2Data <$> (zip rowTitles plainD)
        rowTitles = (T.pack . ((zTitle ++ "@") ++) . show) <$> zRange
        (range2Data, (zTitle, zRange)) = range3Data
        
sc3DHeatMapDia :: M.HashMap ScanSwitch [PhenotypeName]
               -> SCExpMeta
               -> [[[[Timeline]]]]
               -> [Diagram B]
sc3DHeatMapDia switchMap exMeta scBs = sc3DBlock rangeData <$> phValuess
    where
        phValuess = phValuesF bareValuess
        phValuesF = zip phNames . L.transpose . fmap L.transpose .
            (fmap . fmap) L.transpose
        bareValuess = bValuesF trimmedSCBs
        bValuesF = (fmap . fmap . fmap) (phDistribution phNames)
        phNames = (concatMap (switchMap M.!) . scExpSwitches) exMeta
        trimmedSCBs = trimmer scBs
        trimmer = (fmap . fmap . fmap . fmap) (trimPhStoppedTmln stopPhNames)
        rangeData = case (take 3 . scanXAxisData . scMetaScanKind) exMeta of
            [] -> ((blankR, blankR), blankR)
            [x] -> ((x,x),x)
            [x,y] -> ((x,y),y)
            x:y:z:_ -> ((x,y),z)
        blankR = ("blank", [0 :: Double, 1])
        stopPhNames = (fmap fst . stopPhenotypes) exMeta

sc3DBlock :: (((String,[Double]),(String,[Double])),(String, [Double]))
          -> (PhenotypeName, [[[Double]]])
          -> Diagram B
sc3DBlock range3Data (phN, plainD) = vsep 2 [phTitle, plainDRow]
    where
        phTitle = tText' 5 phN
        plainDRow = hsep 5 $ scHeatMapDia range2Data <$> (zip rowTitles plainD)
        rowTitles = (T.pack . ((zTitle ++ "@") ++) . show) <$> zRange
        (range2Data, (zTitle, zRange)) = range3Data


tText' :: Double -> T.Text -> Diagram B
tText' tHt t = F.svgText def (T.unpack t) # F.fit_height tHt
                                      # F.set_envelope
                                      # fillColor black
                                      # lineWidth none
                                      # center
