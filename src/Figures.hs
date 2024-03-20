{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Figures 
    ( layerRunFigure
    , BCExpFigures(..)
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
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Data.Vector as B
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as U
import Data.Vector.Split (splitPlaces)
import Statistics.Sample (meanVarianceUnb)
import qualified Data.Bifunctor as BF
import qualified Data.List as L

-- BarcodeResult combines AttractorResults by equality on Barcodes, because all
-- of the AttractorResults for a given Barcode will be turned into figures in
-- one PDF file. 
type BarcodeResult = (Barcode, [RepResults])

type RealExpSpreadResults = [RealTimeline]

-- The figures produced from each BarcodeResult in an experiment. 
data BCExpFigures = BCEXFS { nodeBCTCFigs :: Maybe [Diagram B]
                           , phenotypeBCTCFigs :: Maybe [Diagram B]
                           , nodeBCAvgBarFig :: Maybe [Diagram B]
                           , phenotypeBCAvgBarFigs :: Maybe [Diagram B]
                           }

layerRunFigure :: ColorMap
    -> HS.HashSet Attractor
    -> LayerResult
    -> ([(DMExperimentMeta, [(Barcode, BCExpFigures)])], Maybe (Diagram B))
layerRunFigure cMap atts (LayerResult lrfMM lrfML eResults mIB) = (expF, fDF)
    where
        expF :: [(DMExperimentMeta, [(Barcode, BCExpFigures)])]
        expF = expRunFigure cMap lrfMM lrfML <$> eResults
        fDF = attractorESpaceFigure cMap lrfMM lniBMap atts <$> mIB
        LayerSpecs lniBMap _ _ _ = layerPrep lrfML

-- Create TimeCourse figures for an individual experiment. Each element in
-- the [([Timeline], [PulseSpacing])] in the AttractorResult is
-- turned into a single PDF, no matter how many Timelines it has. 
-- As of 10/24/23, only KDOEAtTransition experiments will have more than one, 
-- because only they alter the experiment as a function of the size of the
-- Attractor they start in, but this will change in the future. This is in
-- addition to the fact that AttractorResults were combined if their Barcodes
-- were identical. 
expRunFigure :: ColorMap
             -> ModelMapping
             -> ModelLayer
             -> ExperimentResult
             -> (DMExperimentMeta, [(Barcode, BCExpFigures)])
expRunFigure cMap mM mL (exMeta, attResults) = (exMeta, bcDiasWBCs)
    where
        bcDiasWBCs = bcRunDia cMap mM mL exMeta <$> (attResCombine attResults)

attResCombine :: [AttractorResult] -> [BarcodeResult]
attResCombine ars = M.toList $ M.fromListWith (<>) preppedArs
    where preppedArs = pure <<$>> ars

bcRunDia :: ColorMap
         -> ModelMapping
         -> ModelLayer
         -> DMExperimentMeta
         -> BarcodeResult
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
                expGuides :: [[Diagram B]]
                expGuides = (fmap . fmap)
                    (expGuideDia mL exMeta (stripHt) . snd) avgTmlnPSs
                phenotypeBCFigss :: [[[Diagram B]]]
                phenotypeBCFigss = (fmap . fmap) phBCRunDia avgTmlnPSs
                phBCRunDia (tmLns, pIs) = phRunDia cMap mM bc pIs <$> phTmLns
                    where phTmLns = (B.map snd) <$> tmLns -- PhenotypeWeights
        nBChFig = case nodeAvgBars figKs of
            [] -> Nothing
            bCHNodeNs -> Just $ vsep 5.0 <$> nBChartFigs
                where
                    nBChartFigs :: [[Diagram B]]
                    nBChartFigs = nBChartDia cMap mL exMeta bCHNodeNs <<$>>
                                            (nodeBarChartStats <$> repRs)
        phBChFig = case phenotypeAvgBars figKs of
            [] -> Nothing
            phCHNodeNs -> Just $ vsep 5.0 <$> phBChartFigs
                where
                    phBChartFigs :: [[Diagram B]]
                    phBChartFigs = phBChartDia cMap switchMap exMeta phCHNodeNs
                                            <<$>> (phBarChartStats <$> repRs)
        switchMap = M.fromList nonEmptySwPhNs
        nonEmptySwPhNs = (fmap . fmap . fmap) phenotypeName nonEmptySwPhs
        nonEmptySwPhs = snd <<$>> (filter (not . null . snd . snd) mM)
        -- Integrate the [[PulseSpacing]] with the RealExpSpreadResults. 
        avgTmlnPSs :: [[(RealExpSpreadResults, [PulseSpacing])]]
        avgTmlnPSs = (uncurry zip) <$> avgRepRs
        avgRepRs :: [([RealExpSpreadResults], [[PulseSpacing]])]
        avgRepRs = (fmap . BF.first) (averagedAttResults reps) repRs
        reps = ((fromIntegral . expReps) exMeta) :: Double
        figKs = expFigures exMeta
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
nodeBarChartStats :: RepResults -> [[U.Vector (RealNodeState, StdDev)]]
nodeBarChartStats (exSpRs, pSPss) = statsCalc <<$>> preppedStates
    where
        preppedStates = (L.transpose . mconcat) <$> splitByIpSpStateVs
        splitByIpSpStateVs = zipWith tmSplitter pulseIntss tpStates
        pulseIntss = (fstOf3 . unzip3) <$> pSPss
        tpStates = L.transpose justStateVecs
        justStateVecs =
            (fmap . fmap . fmap . B.map) (U.map fst . fst) exSpRs

-- Split time series into their InputPulse spacings
tmSplitter :: [Int]
           -> [[B.Vector a]]
           -> [[[B.Vector a]]]
tmSplitter plsSpaces tmlnss = splitPlaces plsSpaces <<$>> tmlnss

-- Calculate the mean and standard deviation of NodeStates of an experiment
-- population. 
statsCalc :: [B.Vector (U.Vector NodeState)]
          -> U.Vector (RealNodeState, StdDev)
statsCalc tmlns = U.fromList $ meanVarianceUnb <$> momentVs
    where
        momentVs = (fmap U.fromList . L.transpose . mconcat) floatTmlnLs
        floatTmlnLs :: [[[Double]]]
        floatTmlnLs = ((fmap . fmap .fmap) fromIntegral tmlnLs)
        tmlnLs = (fmap U.toList . B.toList) <$> tmlns

-- Statistics across a RepResults for individual phenotypes. As in
-- nodeBarChartStats, we model the Timelines as the lifetimes of individual
-- cells in some experiment. Each element in the resultant list represents
-- statistics over an ExpSpreadResult, since they represent running the
-- experiment with different NodeAlteration timings. 
phBarChartStats :: RepResults -> [[M.HashMap PhenotypeName (Double, StdDev)]]
phBarChartStats (exSpRs, pSPss) = phStatsCalc <<$>> preppedStates
    where
        preppedStates = (L.transpose . mconcat) <$> splitByIpSpPhNs
        splitByIpSpPhNs :: [[[[B.Vector [PhenotypeName]]]]]
        splitByIpSpPhNs = zipWith tmSplitter pulseIntss tpPhNames
        pulseIntss = (fstOf3 . unzip3) <$> pSPss
        tpPhNames = L.transpose justPhenotypes
        justPhenotypes :: [[[B.Vector [PhenotypeName]]]]
        justPhenotypes = (fmap . fmap . fmap . B.map) snd exSpRs

phStatsCalc :: [B.Vector [PhenotypeName]]
            -> M.HashMap PhenotypeName (Double, StdDev)
phStatsCalc phVecs = M.map (meanVarianceUnb . U.fromList) phListMap
    where
        phListMap = L.foldr unionF accMap consdFOPhMaps
        unionF fOPhMp accM = M.foldrWithKey (M.insertWith (<>)) accM fOPhMp
        accMap = M.fromList $ zip allPhs $ L.repeat []
        consdFOPhMaps = fmap (M.map (:[])) filledOutPhMaps
--      Make sure every map contains every PhenotypeName, so all of the Vectors
--      are the same length. 
        filledOutPhMaps = (\phM -> L.foldl' finisher phM allPhs) <$> phFMaps
            where finisher phMp phN = M.insertWith (flip const) phN 0.0 phMp
        allPhs = HS.toList $ L.foldl' (flip HS.insert) HS.empty flatPHList
        flatPHList = (mconcat . concatMap B.toList) phVecs
        phFMaps = phenotypeFraction <$> phVecs

phenotypeFraction :: B.Vector [PhenotypeName] -> M.HashMap PhenotypeName Double
phenotypeFraction phVec = M.map (/divisor) (B.foldl' insertF M.empty phVec)
    where
        insertF cMp phNs = L.foldl' (\m n -> M.insertWith (+) n 1.0 m ) cMp phNs
        divisor = (fromIntegral . B.length) phVec :: Double
