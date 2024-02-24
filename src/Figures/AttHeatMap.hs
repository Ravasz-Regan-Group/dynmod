{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Figures.AttHeatMap
    ( attractorGrid
    , attractorHeatMapDia
    ) where

import Types.DMModel
import Types.Simulation
import Properties.Attractors
import Plots
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Data.HashSet as HS

-- Here we actively misinterpret a ModelEnv as specifying random state slots and
-- noisy step slots instead of a single run with those numbers, but the data is
-- there and a ModelEnvs is a convenient package. 
attractorGrid :: ModelEnv -> Int -> Simulation [[HS.HashSet Attractor]]
attractorGrid (mL, SamplingParameters rN nN nProb _) multiplier =
    (traverse . traverse) attractors (mEnvGrid rN nN multiplier nProb mL)

mEnvGrid :: Int -> Int -> Int -> Double -> ModelLayer -> [[ModelEnv]]
mEnvGrid rN nN multiplier nProb mL = mkRow <$> [1..rN]
    where
        mkRow m = ((mkMEnv (multiplier * m)) . (multiplier *)) <$> [1..nN]
        mkMEnv i j = (mL, SamplingParameters i j nProb [])


heatMapAxis :: [[Double]] -> Int -> Axis B V2 Double
heatMapAxis atts mult = r2Axis &~ do
  display colourBar
  axisExtend .= noExtend
  axisColourMap .= Plots.viridis
  xLabel .= "Noisy Steps"
  yLabel .= "r_States"
  heatMap atts $ heatMapSize .= V2 (fromIntegral mult) (fromIntegral mult)

attractorHeatMapDia :: [[Double]] -> Int -> QDiagram B V2 Double Any
attractorHeatMapDia ass mult = renderAxis $ heatMapAxis ass mult

-- rnGrid :: Int -> Int -> Int -> [[(Double, Double)]]
-- rnGrid r n mult = mkRow <$> [1..rD]
--     where
--         mkRow m = (\p -> (multD * m, multD * p)) <$> [1..nD]
--         rD = fromIntegral r
--         nD = fromIntegral n
--         multD = fromIntegral mult

