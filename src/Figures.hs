{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Figures
    ( attractorGrid
    , rnGrid
    , attractorHMSVGText
    ) where

import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Svg.Core (renderText, Element)
import Plots
import Types.DMModel
import Types.Simulation
import Properties.Attractors

-- (fromIntegral . HS.size) <<<$>>> atts

-- Here we actively misinterpret a ModelEnv as specifying random state slots and
-- noisy step slots instead of a single run with those numbers, but the data is
-- there and this a ModelEnvs is a convenient package. 
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

