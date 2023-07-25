{-# LANGUAGE OverloadedStrings #-}

module Types.Figures where

import Types.DMModel
import Constants
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as B

-- Basic types to support all types of figures.

type SVGText = T.Text
type ColorMap = M.HashMap NodeName LocalColor

-- Pick a color from one of the perceptually uniform color gradients in
-- Constants. They all have 256 colors. Do not apply this to anything else! 
gradientPick :: RealFrac a => PUCGradient
                             -> (a, a)
                             -> a
                             -> Maybe LocalColor
gradientPick pucGr (low, high) pick
    | low >= high = Nothing
    | pick < low = Nothing
    | pick > high = Nothing
    | otherwise = Just $ pucGr B.! pickIndex
    where
        pickIndex = round (pick * transform)
        transform = 255 / magnitude
        magnitude = high - low



