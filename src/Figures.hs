{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Figures 
    ( layerRunFigure
    , BCExpFigures(..)
    , ScanExpFigure(..)
    , BaseScanFigs(..)
    , ExperimentFigures(..)
    , attractorESpaceFigure
    , attractorGrid
    , attractorHeatMapDia
    , Barcode
    , Bar(..)
    , barPhenotype
    , InputBundle(..)
    , mkColorMap
    ) where

import Types.DMModel
import Types.DMInvestigation
import Types.Simulation
import Types.Figures
import Utilities
import Figures.AttHeatMap
import Figures.InputSpaceFigure
import Figures.TimeCourse
import Figures.BarCharts
import Figures.Scan
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import Data.Vector.Split (splitPlaces)
import Statistics.Sample (meanVarianceUnb, mean)
import qualified Data.Bifunctor as BF
import qualified Data.List as L

type RealExpSpreadResults = [RealTimeline]

data ExperimentFigures = TCExpFigs (TCExpMeta, [(Barcode, BCExpFigures)])
                       | ScanExpFigs (SCExpMeta, [(Barcode, [ScanExpFigure])])

-- The figures produced from each BarcodeResult in an experiment. 
data BCExpFigures = BCEXFS
    { nodeBCTCFigs :: Maybe [Diagram B]
    , phenotypeBCTCFigs :: Maybe [Diagram B]
    , nodeBCAvgBarFig :: Maybe [(Diagram B,
                            [[[(NodeName, U.Vector RealNodeState)]]])]
    , phenotypeBCAvgBarFigs :: Maybe [(Diagram B,
                          [[(NodeName, [[(PhenotypeName, U.Vector Double)]])]])]
    }

layerRunFigure :: ColorMap
    -> HS.HashSet Attractor
    -> LayerResult
    -> ([ExperimentFigures], Maybe (Diagram B))
layerRunFigure cMap atts (LayerResult lrfMM lrfML eResults mIB) = (expF, fDF)
    where
        expF = expRunFigure cMap lrfMM lrfML <$> eResults
        fDF = attractorESpaceFigure cMap lrfMM lniBMap atts <$> mIB
        LayerSpecs lniBMap _ _ _ = layerPrep lrfML

-- Create figures for an individual experiment. For TimeCourse experiments, 
-- Each element in the [([Timeline], [PulseSpacing])] in the
-- (Barcode, RepResults) is turned into a single PDF, no matter how many
-- Timelines it has. As of 10/24/23, only KDOEAtTransition experiments will have
-- more than one, because only they alter the experiment as a function of the
-- size of the Attractor they start in, but this will change in the future. This
-- is in addition to the fact that (Barcode, RepResults) were combined if their
-- Barcodes were identical. 
expRunFigure :: ColorMap
             -> ModelMapping
             -> ModelLayer
             -> ExperimentResult
             -> ExperimentFigures
expRunFigure cMap mM mL expRes = case expRes of
    TCExpRes (xMeta, ress) -> TCExpFigs $ (xMeta, figs)
        where
            figs = (fmap bcRunDiaF . resCombine) ress
            bcRunDiaF :: BarcodeResult RepResults -> (Barcode, BCExpFigures)
            bcRunDiaF = bcRunDia cMap mM mL xMeta
    ScanExpRes (xMeta, ress) -> ScanExpFigs $ (xMeta, figs)
        where
            figs :: [(Barcode, [ScanExpFigure])]
            figs = (resCombine . (fmap . fmap)
                (scRunDia cMap mM mL xMeta)) ress


bcRunDia :: ColorMap
         -> ModelMapping
         -> ModelLayer
         -> TCExpMeta
         -> BarcodeResult RepResults
         -> (Barcode, BCExpFigures)
bcRunDia cMap mM mL exMeta (bc, repRs) = (bc, expFgs)
    where
        expFgs = BCEXFS tcFigs phFigs nBChFig phBChFig
        tcFigs
            | nodeTimeCourse figKs = Just $ (legendDia ===) <$> horizontalBCFigs
            | otherwise = Nothing
            where
                horizontalBCFigs :: [Diagram B]
                horizontalBCFigs = mconcat verticalBCFigss
                verticalBCFigss :: [[Diagram B]]
                verticalBCFigss = (fmap . fmap) (vsep 5.0) nodeBCFigss
                nodeBCFigss :: [[[Diagram B]]]
                nodeBCFigss = (fmap . fmap) nodeBCRunDia avgTmlnPSs
                nodeBCRunDia (tmLns, pSs) =
                    nodeRunDia cMap mM mL params bc pSs <$> tcTmLns
                    where tcTmLns = (B.map fst) <$> tmLns -- The node U.Vectors
                legendDia = bcRunFigLegendDia cMap bc
        phFigs
            | phenotypeTimeCourse figKs = Just $ (hsep 5.0) <$> figs
            | otherwise = Nothing
            where
                figs :: [[Diagram B]]
                figs = (zipWith . zipWith) padder expGuides phenotypeBCFigss
                padder eGfig phBCFigs = vsep 5.0 (alignR <$> (eGfig:phBCFigs))
                phenotypeBCFigss :: [[[Diagram B]]]
                phenotypeBCFigss = (fmap . fmap) phBCRunDia avgTmlnPSs
                phBCRunDia (tmLns, pIs) = phRunDia cMap mM bc pIs <$> phTmLns
                    where phTmLns = (B.map snd) <$> tmLns -- PhenotypeWeights
        nBChFig = case nodeAvgBars figKs of
            [] -> Nothing
            bCHNodeNs -> Just $ mrgeSprFgs <$> nBChartFigs
                where
                    mrgeSprFgs ::
                         [(Diagram B,[[(NodeName, U.Vector RealNodeState)]])]
                      -> (Diagram B,[[[(NodeName, U.Vector RealNodeState)]]])
                    mrgeSprFgs = BF.first (vsep 5.0) . L.unzip
                    nBChartFigs :: [[(Diagram B,
                                    [[(NodeName, U.Vector RealNodeState)]])]]
                    nBChartFigs = (nBChartDiaF <<$>> statRepRs)
                    statRepRs :: [[[B.Vector (U.Vector RealNodeState
                                    , (RealNodeState, StdDev))]]]
                    statRepRs = nodeBarChartStats <$> repRs
                    nBChartDiaF = nBChartDia cMap mL exMeta bCHNodeNs
        phBChFig = case phenotypeAvgBars figKs of
            [] -> Nothing
            phCHNodeNs -> Just $ mrgeSprFgs <$> phBChartFigs
                where
                  mrgeSprFgs :: [(Diagram B,
                       [(NodeName, [[(PhenotypeName, U.Vector Double)]])])]
                    -> (Diagram B,
                          [[(NodeName, [[(PhenotypeName, U.Vector Double)]])]])
                  mrgeSprFgs = BF.first (vsep 5.0) . L.unzip
                  phBChartFigs :: [[(Diagram B,
                        [(NodeName, [[(PhenotypeName, U.Vector Double)]])])]]
                  phBChartFigs = (uncurry phBCF) <<$>> (zipZip pulseSpcs pSts)
                    where
                      phBCF = phBChartDia cMap mL switchMap exMeta phCHNodeNs
                      pSts :: [[[M.HashMap PhenotypeName
                                        (U.Vector Double, (Double, StdDev))]]]
                      pSts = phBarChartStats allPhNs <$> repRs
        zipZip = (zipWith . zipWith) (,)
        switchMap = M.fromList nonEmptySwPhNs
        allPhNs = concatMap snd nonEmptySwPhNs
        nonEmptySwPhNs = (fmap . fmap . fmap) phenotypeName nonEmptySwPhs
        nonEmptySwPhs = snd <<$>> (nonEmptyPhenotypes mM)
        expGuides :: [[Diagram B]]
        expGuides = (fmap . fmap) (expGuideDia mL exMeta stripHt) pulseSpcs
        pulseSpcs = (fmap . fmap) snd avgTmlnPSs
        -- Integrate the [[PulseSpacing]] with the RealExpSpreadResults. 
        avgTmlnPSs :: [[(RealExpSpreadResults, [PulseSpacing])]]
        avgTmlnPSs = (uncurry zip) <$> avgRepRs
        avgRepRs :: [([RealExpSpreadResults], [[PulseSpacing]])]
        avgRepRs = (fmap . BF.first) (averagedAttResults reps) repRs
        reps = ((fromIntegral . expReps) exMeta) :: Double
        figKs = tcExpFigures exMeta
        params = (stripHt, 24.0) :: (Double, Double)
        stripHt = 2.0 :: Double

-- Average Timelines into RealTimeLines. 
averagedAttResults :: Double -> [ExpSpreadResults] -> [RealExpSpreadResults]
averagedAttResults reps tlss = (fmap . fmap . B.map) (divider reps) summedVecs
    where
        summedVecs = L.foldr deepZip tlssAccum (tail tlss)
        tlssAccum = (mkAnnoLVAcc . head) tlss

-- These are all kept as top-level functions to be very clear in the type
-- signatures what structures are being transformed. In particular, we make the
-- accumulator out of the head of the [[[Timeline]]] because we don't know its
-- structure beforehand, just that it will be identical accros the list. 
divider :: Double
        -> (U.Vector (Int, Double), M.HashMap PhenotypeName Int)
        -> (RealAnnotatedLayerVec, PhenotypeWeights)
divider reps (accVec, phHM) = (U.map uVecDivider accVec, M.map hmDivider phHM)
    where
        hmDivider x = fromIntegral x / reps
        uVecDivider (stSum, sFSum) = (fromIntegral stSum / reps, sFSum / reps)

deepZip :: [[Timeline]]
   -> [[B.Vector (U.Vector (Int, Double), M.HashMap PhenotypeName Int)]]
   -> [[B.Vector (U.Vector (Int, Double), M.HashMap PhenotypeName Int)]]
deepZip = (L.zipWith . L.zipWith . B.zipWith) annoLVAccum

mkAnnoLVAcc :: [[Timeline]]
   -> [[B.Vector (U.Vector (Int, Double), M.HashMap PhenotypeName Int)]]
mkAnnoLVAcc = (fmap . fmap . B.map) prepAcc
    where
        prepAcc (uVec, phNs) = (U.map prepUVec uVec, mkPhNameMap phNs)        
        prepUVec (nSt, wF) = (nSt, bConv wF)
        mkPhNameMap phN = M.fromList $ (\x -> (x, 1)) <$> phN

annoLVAccum :: (AnnotatedLayerVec, [PhenotypeName])
            -> (U.Vector (Int, Double), M.HashMap PhenotypeName Int)
            -> (U.Vector (Int, Double), M.HashMap PhenotypeName Int)
annoLVAccum (uVec, phNs) (accVec, phHM) = (newVec, newPhHM)
    where
        newPhHM = foldr hFold phHM phNs
        hFold phN phMap = M.insertWith (+) phN 1 phMap
        newVec = U.zipWith uZip uVec accVec
        uZip (nSt, wF) (stSum, sFSum) = (nSt + stSum, bConv wF + sFSum)

bConv :: Bool -> Double
bConv False = 0.0
bConv True = 1.0


-- Statistics across a RepResults for individual nodes. Here we model the
-- Timelines as the lifetimes of individual cells in some experiment. Each
-- element in the resultant list represents statistics over an ExpSpreadResult,
-- since they represent running the experiment with different NodeAlteration
-- timings. 
nodeBarChartStats :: RepResults
        -> [[B.Vector (U.Vector RealNodeState, (RealNodeState, StdDev))]]
nodeBarChartStats (exSpRs, pSPss) = statsCalc <<$>> preppedStates
    where
     -- Collapse the results on pulses within like pulse spreads across
     -- repetitions, then group by pulse spreads and then pulses. 
        preppedStates :: [[[B.Vector (U.Vector NodeState)]]]
        preppedStates = (L.transpose . mconcat) <$> splitByIpSpStateVs
     -- Use the PulseSpacings to break up the timelines. 
        splitByIpSpStateVs :: [[[[B.Vector (U.Vector NodeState)]]]]
        splitByIpSpStateVs = zipWith tmSplitter pulseIntss tpStates
        pulseIntss :: [[Int]]
        pulseIntss = (fstOf3 . unzip3) <$> pSPss
     -- Rearrange the results so that experiment spreads are top level, and not
     -- experiment repetitions. 
        tpStates :: [[[B.Vector (U.Vector NodeState)]]]
        tpStates = L.transpose justStateVecs
     -- I just want the bare timeline of state vectors
        justStateVecs :: [[[B.Vector (U.Vector NodeState)]]]
        justStateVecs = (fmap . fmap . fmap . B.map) (U.map fst . fst) exSpRs

-- Split time series into their InputPulse spacings
tmSplitter :: [Int]
           -> [[B.Vector a]]
           -> [[[B.Vector a]]]
tmSplitter plsSpaces tmlnss = splitPlaces plsSpaces <<$>> tmlnss

-- Calculate the mean and standard deviation of NodeStates of an experiment
-- population during a Pulse. Average each node over each individual pulse
-- first, then return those means as well as the mean and standard deviation of
-- those averages. 
-- For now we will keep both the averages and the vectors of NodeState values
-- that they came from. They will be written out with the figure, to be
-- available for additional analysis. Eventually this will be replaced by the
-- ability to do statistics across experiments internally. 
statsCalc :: [B.Vector (U.Vector NodeState)]
          -> B.Vector (U.Vector RealNodeState, (RealNodeState, StdDev))
statsCalc tmlns = B.fromList $ (varianceF . U.fromList) <$> averagedLs
    where
        varianceF v = (v, meanVarianceUnb v)
        averagedLs :: [[Double]]
        averagedLs = (mean . U.fromList) <<$>> transposedFTLs
     -- Arrange the node data so that the pulses are top-level
        transposedFTLs = (L.transpose . fmap L.transpose) floatTmlnLs
        floatTmlnLs :: [[[Double]]]
        floatTmlnLs = ((fmap . fmap . fmap) fromIntegral tmlnLs)
        tmlnLs = (fmap U.toList . B.toList) <$> tmlns


-- Statistics across a RepResults for individual phenotypes. As in
-- nodeBarChartStats, we model the Timelines as the lifetimes of individual
-- cells in some experiment. Each element in the resultant list represents
-- statistics over an ExpSpreadResult, since they represent running the
-- experiment with different NodeAlteration timings. 
phBarChartStats :: [PhenotypeName] -> RepResults
    -> [[M.HashMap PhenotypeName (U.Vector Double, (Double, StdDev))]]
phBarChartStats allPhNs (exSRs, pSPss) = phStatsCalc allPhNs <<$>> preppedStates
    where
        preppedStates = (L.transpose . mconcat) <$> splitByIpSpPhNs
        splitByIpSpPhNs :: [[[[B.Vector [PhenotypeName]]]]]
        splitByIpSpPhNs = zipWith tmSplitter pulseIntss tpPhNames
        pulseIntss = (fstOf3 . unzip3) <$> pSPss
        tpPhNames = L.transpose justPhenotypes
        justPhenotypes :: [[[B.Vector [PhenotypeName]]]]
        justPhenotypes = (fmap . fmap . fmap . B.map) snd exSRs

phStatsCalc :: [PhenotypeName]
            -> [B.Vector [PhenotypeName]]
            -> M.HashMap PhenotypeName (U.Vector Double, (Double, StdDev))
phStatsCalc allPhNs phVecs = M.map varianceF phListMap
    where
-- For now we will keep both the averages and the vectors of Phenotype
-- prevalence that they came from. They will be written out with the figure, to
-- be available for additional analysis. Eventually this will be replaced by the
-- ability to do statistics across experiments internally. 
        varianceF v = (v, meanVarianceUnb v)
        phListMap = M.map U.fromList (L.foldr unionF accMap consdFOPhMaps)
        unionF fOPhMp accM = M.foldrWithKey (M.insertWith (<>)) accM fOPhMp
        accMap = M.fromList $ zip allPhNs $ L.repeat []
        consdFOPhMaps = fmap (M.map pure) filledOutPhMaps
        filledOutPhMaps = phenotypeFraction allPhNs <$> phVecs
        

-- Make sure every map contains every PhenotypeName, so all of the Vectors are
-- the same length. 
phenotypeFraction :: [PhenotypeName]
                  -> B.Vector [PhenotypeName]
                  -> M.HashMap PhenotypeName Double
phenotypeFraction allPhNs phVec = M.union presentMaps blankMaps
    where
        blankMaps :: M.HashMap PhenotypeName Double
        blankMaps = M.fromList $ zip allPhNs $ repeat 0.0
        presentMaps :: M.HashMap PhenotypeName Double
        presentMaps = M.map (/divisor) (B.foldl' insertF M.empty phVec)
        insertF cMp phNs = L.foldl' insertF' cMp phNs
        insertF' mp phN = M.insertWith (+) phN 1.0 mp
        divisor = (fromIntegral . B.length) phVec :: Double
