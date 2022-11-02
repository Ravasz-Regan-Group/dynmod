{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Figures
    ( attractorGrid
    , rnGrid
    , attractorHMSVGText
    , attractorCheck
    , mkBarcode
    ) where

import Types.DMModel
import Types.Simulation
import Properties.Attractors
import Utilities
import Data.Validation
import Plots
import qualified Data.Vector as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as M
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Svg.Core (renderText, Element)
import qualified Data.List as L
import Data.Bifunctor
import Control.Monad.Reader (Reader, runReader, ask)


data AttractorsInvalid =
      SwitcheNamesDifferent ([NodeName], [NodeName])
    | SwitcheNodesDifferent (NodeName, ([NodeName], [NodeName]))
    | NotAnAttractor Thread
    | InvalidLVReorder InvalidLVReorder
    deriving (Show, Eq)

type ColorMap = M.HashMap NodeName LocalColor

type EnvironMentSpaceMapFigure = []
type BarcodeCluster = [Barcode]
type Barcode = [Bar]
type Bar = (LocalColor, [Slice])
data Slice = EmptySlice
           | MSlice MatchSlice
           deriving (Eq, Show)

data MatchSlice = MatchSlice { isPhenotypeLoop :: Bool
                             , totalAttractorStates :: Int
                             -- Matched Attractor states
                             , attractorStateMatches :: [Int]
                             } deriving (Eq, Show)


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

attractorHMSVGText :: [[Double]] -> Int -> T.Text
attractorHMSVGText ass mult = LT.toStrict $ renderText $
    attractorHMElement ass mult


rnGrid :: Int -> Int -> Int -> [[(Double, Double)]]
rnGrid r n mult = mkRow <$> [1..rD]
    where
        mkRow m = (\p -> (multD * m, multD * p)) <$> [1..nD]
        rD = fromIntegral r
        nD = fromIntegral n
        multD = fromIntegral mult

-- Are the Attractors from the parsed attractor.csv actually Attractors of the
-- given ModelLayer form the parsed DMMS file?
attractorCheck :: (DMMSModelMapping, LayerNameIndexBimap, HS.HashSet Attractor)
               -> (DMMSModelMapping, ModelLayer)
               -> Validation [AttractorsInvalid] (HS.HashSet Attractor)
attractorCheck (csvMMap, csvLNIBMap, atts) (dmmsMMap, mL) = case mapResults of
    err:errs -> Failure (err:errs)
    [] -> (validationed valF orderedAtts) <* checkedAtts
    where
        checkedAtts :: Validation [AttractorsInvalid] (HS.HashSet Attractor)
        checkedAtts = HS.fromList <$> (sequenceA $
            attCheck dmmsLNIBMap dmmsPSStepper csvLNIBMap <$> (HS.toList atts))
        orderedAtts :: Validation [InvalidLVReorder] (HS.HashSet Attractor)
        orderedAtts = HS.fromList <$> (sequenceA $
            lNISwitchThread csvLNIBMap dmmsLNIBMap <$> (HS.toList atts))
        dmmsPSStepper = synchStep dmmsIVList dmmsTTList
        LayerSpecs dmmsLNIBMap _ dmmsTTList dmmsIVList = layerPrep mL
        mapResults = mmCheck csvMMap dmmsMMap

valF :: Validation [InvalidLVReorder] (HS.HashSet Attractor)
     -> Validation [AttractorsInvalid] (HS.HashSet Attractor)
valF (Success x) = Success x
valF (Failure x) = Failure $ ilvL2aiL x

ilvL2aiL :: [InvalidLVReorder] -> [AttractorsInvalid]
ilvL2aiL = fmap ilv2ai
    where
        ilv2ai NewOldOrderingMismatch = InvalidLVReorder NewOldOrderingMismatch
        ilv2ai OldOrderingLVMismatch = InvalidLVReorder OldOrderingLVMismatch

-- Is the given Thread an Attractor of the given PSStepper?
attCheck :: LayerNameIndexBimap
         -> PSStepper
         -> LayerNameIndexBimap
         -> Thread
         -> Validation [AttractorsInvalid] Thread
attCheck dmmsLNIBMap dmmsPSStepper csvLNIBMap thread
    | isAtt dmmsLNIBMap dmmsPSStepper csvLNIBMap thread = Success thread
    | otherwise = Failure [NotAnAttractor thread]

-- Note that both ModelMappings here have already gone through a parsing, which
-- means that every switch name is unique, as is its associated NodeName list,
-- and no two switches share any NodeNames in common. 
mmCheck :: DMMSModelMapping -> DMMSModelMapping -> [AttractorsInvalid]
mmCheck csvMMap dmmsMMap = case lrUniques cSwitches dSwitches of
    ([], []) -> foldr switchContentsF [] pairedMMs
    err -> [SwitcheNamesDifferent err]
    where
        pairedMMs = zipWith zipper sortedCSV sortedDMMS
        zipper (x, xs) (_, ys) = (x, xs, ys)
        sortedCSV = L.sortOn fst csvMMap
        sortedDMMS = L.sortOn fst dmmsMMap
        cSwitches = fst <$> csvMMap
        dSwitches = fst <$> dmmsMMap

switchContentsF :: (NodeName, [NodeName], [NodeName])
                -> [AttractorsInvalid]
                -> [AttractorsInvalid]
switchContentsF (sName, csVNNs, dmmsNNs) ais = case lrUniques csVNNs dmmsNNs of
    ([], []) -> ais
    errs     -> (SwitcheNodesDifferent (sName, errs)) : ais

mkColorMap :: DMModel -> ColorMap
mkColorMap dmm = M.fromList nameColorPairs
    where
        nameColorPairs = (\n -> (nodeName n, nodeColor n)) <$> nodesMetas
        nodesMetas = nodeMeta <$> ((concat . modelNodes) dmm)


-- mkBarcodeArray :: Reader (DMModel, HS.HashSet Attractor)
--                          (A.Array A.B A.IxN (NodeName, BarcodeCluster))
-- mkBarcodeArray = undefined -- do
--     attPart <- attractorPartition

-- Generate a Barcode to represent Attractors on environment-space figures.
-- Assumes the ColorMap order matches that of the Attractor. 
mkBarcode :: ColorMap -> ModelMapping -> Attractor -> Barcode
mkBarcode cM mM att = (uncurry (mkBar att)) <$> colorSwitchPairs
    where
        colorSwitchPairs = ((cM M.!) `first`) <$> nameSwitchPairs
        nameSwitchPairs = (\(nName, (_, phs)) -> (nName, phs)) <$> mM

mkBar :: Attractor -> LocalColor -> [Phenotype] -> Bar
mkBar att sColor phs = (sColor, mkSlice att sColor <$> orderedPHs)
    where
--      We order the phenotypes by switchNodeState descending so the the Bar
--      will have the 0 state at the bottom, rather than the top. 
        orderedPHs = (L.reverse . L.sortOn switchNodeState) phs

mkSlice :: Attractor -> LocalColor -> Phenotype -> Slice
mkSlice att sColor ph = case matchCheck att fPrint of
    [] -> EmptySlice
    ms -> MSlice $ MatchSlice pLoopBool attSize ms
    where
        pLoopBool = length fPrint > 1
        fPrint = fingerprint ph
        attSize = B.length att

matchCheck :: Attractor -> [SubSpace] -> [Int]
matchCheck att sSs = undefined
-- foldr go [] sSs
--     where
--         go sS acc = 




