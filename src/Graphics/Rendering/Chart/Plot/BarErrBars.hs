{-# LANGUAGE TemplateHaskell #-}


-- | Everything in this module is stolen eother from ErrBars or Bars. Stacked
-- error bars are nonsensical, so we son't replicate that part of Bars. 

module Graphics.Rendering.Chart.Plot.BarErrBars
    ( PlotBarErrBars(..)
    , BEBValue(..)
    , PlotBEBsSpacing(..)
    , PlotBEBsAlignment(..)
    , symBEBPoint
    
    , plotBEBs
    , plotHBEBs
    
    -- * Accessors
    , plot_bebs_values
    , plot_bebs_title
    , plot_bebs_settings
    
    , plot_bebs_line_style
    , plot_bebs_spacing
    , plot_bebs_alignment
    , plot_bebs_singleton_width
    , plot_bebs_tick_length
    , plot_bebs_overhang
    
    ) where


import Control.Monad
import Control.Lens (makeLenses, Lens')

import Data.Colour (opaque)
import Data.Colour.Names (black)
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Utils
import Data.Default.Class
import qualified Data.List as L
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)


-- | Value for holding a point with associated error bounds for the y axis.
-- No x axis error here, as they make no sense for barcharts. 

class (PlotValue a, Ord a) => BEBValue a where
    bebsIsNull    :: a -> Bool
    -- | The starting level for the chart, a function of some statistic
    --   (normally the lowest value or just const 0).
    bebsReference :: [a] -> a

instance BEBValue Double where
    bebsIsNull a = a == 0.0
    bebsReference = const 0

instance BEBValue Int where
    bebsIsNull a = a == 0
    bebsReference = const 0

instance BEBValue LogValue where
    bebsIsNull (LogValue a) = a == 0.0
    bebsReference as =
      10.0 ^^ (floor (log10 $ minimum $ filter (/= 0.0) as) :: Integer)

data PlotBEBsSpacing
    = BEBsFixWidth Double       -- ^ All bars have the same width in pixels.
    | BEBsFixGap Double Double  -- ^ (BEBsFixGap g mw) means make the gaps
                                --   between the bars equal to g, but with a
                                --   minimum bar width of mw
     deriving Show

-- | How bars for a given (x,[y]) are aligned with respect to screen
--   coordinate corresponding to x (deviceX).
data PlotBEBsAlignment = BEBsLeft      -- ^ The left edge of bars is at deviceX
                       | BEBsCentered  -- ^ Bars are centered around deviceX
                       | BEBsRight     -- ^ The right edge of bars is at deviceX
     deriving Show

-- | Value describing how to plot a set of bars.
--   Note that the input data is typed [(x,[y])], ie for each x value
--   we plot several y values. Typically the size of each [y] list would
--   be the same.
data BEBsSettings = BEBsSettings {
    -- | The style in which to draw each error bar y.
      _bebs_settings_line_style :: (FillStyle,Maybe LineStyle) 

    -- | Implicity, we are putting these error bars on top of bar charts, so we
    --   keep all the bar spacing information to make that alignment
    --   happens. 

    -- | This value controls how the widths of the bars are
    --   calculated. Either the widths of the bars, or the gaps between
    --   them can be fixed.
    , _bebs_settings_spacing :: PlotBEBsSpacing

   -- | This value controls how bars for a fixed x are aligned with
   --   respect to the device coordinate corresponding to x.
    , _bebs_settings_alignment :: PlotBEBsAlignment

    , _bebs_settings_singleton_width :: Double
    
    , _plot_settings_bebs_tick_length :: Double
    
   -- | This value controls how much farther past the error value to plot the
   -- cross tick
    , _plot_settings_bebs_overhang :: Double
}

instance Default BEBsSettings where
    def = BEBsSettings
        { _bebs_settings_line_style      = mkstyle
        , _bebs_settings_spacing         = BEBsFixGap 10 2
        , _bebs_settings_alignment       = BEBsCentered
        , _bebs_settings_singleton_width = 20
        , _plot_settings_bebs_tick_length         = 3
        , _plot_settings_bebs_overhang            = 0
        }
        where
            mkstyle = (solidFillStyle (opaque black)
                        , Just (solidLine 1.0 $ opaque black))

data PlotBarErrBars x y = PlotBarErrBars {
      _plot_bebs_settings :: BEBsSettings
    , _plot_bebs_title :: String
-- (yBest, (yLow, yHigh)), where: 
-- yLow = yBest - yLowerError and yHigh = yBest + yUpperError
-- For reasons beyond my understanding, calculating this has to happen before
-- the pmap transformation, or the values are uselessly wacky. 
    , _plot_bebs_values :: [(x, [(y, (y, y))])]
    }

instance Default (PlotBarErrBars x y) where
    def = PlotBarErrBars
        { _plot_bebs_settings = def
        , _plot_bebs_title = ""
        , _plot_bebs_values = []
        }

plotBEBs :: (BEBValue y) => PlotBarErrBars x y -> Plot x y
plotBEBs p = Plot {
      _plot_render = \pmap -> renderBEBs s vals (errPoint pmap) (mapX pmap)
    , _plot_legend = [(_plot_bebs_title p,
                            renderPlotLegendBEBs (_bebs_settings_line_style s)
                                     (_plot_settings_bebs_tick_length s))]
    , _plot_all_points = allBEBPoints vals
    }
    where
        s = _plot_bebs_settings p
        vals = _plot_bebs_values p
        
        errPoint pmap xos width x (yBest, (yLow, yHigh)) = mappedVals
            where
                mappedVals = (startPoint, (yLow', yHigh'))
                startPoint = Point (x' + xos + (width / 2)) y'
                Point x' y' = mapXY pmap (x, yBest)
                Point _ yLow' = mapXY pmap (x, yLow)
                Point _ yHigh' = mapXY pmap (x, yHigh)
    
        mapX pmap x = p_x (mapXY pmap (x, refBEBVal vals))

-- Horizontal Barchart error bars. 
plotHBEBs :: (BEBValue x) => PlotBarErrBars y x -> Plot x y
plotHBEBs p = Plot {
      _plot_render     = \pmap -> renderBEBs s vals (errPoint pmap) (mapY pmap)
    , _plot_legend     = [(_plot_bebs_title p,
                            renderPlotLegendBEBs (_bebs_settings_line_style s)
                                     (_plot_settings_bebs_tick_length s))]
    , _plot_all_points = swap $ allBEBPoints vals
    }
    where
        s = _plot_bebs_settings p
        vals = _plot_bebs_values p
        
        errPoint pmap yos height y (xBest, (xLow, xHigh)) = mappedVals
            where
                mappedVals = (startPoint, (xLow', xHigh'))
                startPoint = Point x' (y' + yos + (height / 2))
                Point x' y' = mapXY pmap (xBest, y)
                Point _ xLow' = mapXY pmap (xLow, y)
                Point _ xHigh'= mapXY pmap (xHigh, y)
                
        mapY pmap y = p_y (mapXY pmap (refBEBVal vals, y))

renderBEBs :: (BEBValue v)
         => BEBsSettings
         -> [(k, [(v, (v, v))])]
         -> (Double -> Double -> k -> (v, (v, v)) -> (Point, (Double, Double)))
         -> (k -> Double)
         -> BackendProgram ()
renderBEBs p vals pointF mapk = forM_ vals barErrorBars
  where
    barErrorBars (k,vs) = do
        let offset i = case _bebs_settings_alignment p of
             BEBsLeft     -> fromIntegral i * bsize
             BEBsRight    -> fromIntegral (i-nvs) * bsize
             BEBsCentered -> fromIntegral (2*i-nvs) * bsize/2
        forM_ (zip3 [0,1..] vs (repeat style)) $ \(i, v, (_,mlstyle)) ->
           unless ((bebsIsNull . fst) v) $
           whenJust mlstyle $ \lstyle ->
             withLineStyle lstyle $
               alignStrokePath (errorPath (offset i) k v)
               >>= strokePath
    style = _bebs_settings_line_style p
    bsize = case _bebs_settings_spacing p of
        BEBsFixGap gap minw -> let w = max (minKInterval - gap) minw in
            w / fromIntegral nvs
        BEBsFixWidth width' -> width'
    minKInterval = let diffs = zipWith (-) (tail mks) mks
                   in if null diffs
                        then _bebs_settings_singleton_width p
                        else minimum diffs
      where
        mks = L.nub $ L.sort $ fmap (mapk . fst) vals
    nvs = maximum $ fmap (length . snd) vals
    errorPath os k v1 = errPath startPoint (dyLow, dyHigh)
        where
            errPath (Point x _) (yL, yH) = moveTo' x (yL - oh)
                                          <> lineTo' x (yH + oh)
                                          <> moveTo' (x - tl) (yH + oh)
                                          <> lineTo' (x + tl) (yH + oh)
                                          <> moveTo' (x - tl) (yL - oh)
                                          <> lineTo' (x + tl) (yL - oh)
            (startPoint, (dyLow, dyHigh)) = pointF os bsize k v1
            tl = _plot_settings_bebs_tick_length p
            oh = _plot_settings_bebs_overhang p


renderPlotLegendBEBs :: (BEBValue y)
                     => (FillStyle, Maybe LineStyle)
                     -> y
                     -> Rect
                     -> BackendProgram ()
renderPlotLegendBEBs (_, mLStyle) tl' (Rect p1 p2) = do
    let lStyle = fromMaybe (solidLine 1.0 $ opaque black) mLStyle
        dx = min ((p_x p2 - p_x p1)/6) ((p_y p2 - p_y p1)/2)
        y = (p_y p1 + p_y p2)/2
        tl = toValue tl'
        drawErrBar x1 y1 dy1 = withLineStyle lStyle $ strokePath $
               moveTo' x1 (y1 - dy1)
            <> lineTo' x1 (y1 + dy1)
            <> moveTo' (x1 - tl) (y1 + dy1)
            <> lineTo' (x1 + tl) (y1 + dy1)
            <> moveTo' (x1 - tl) (y1 - dy1)
            <> lineTo' (x1 + tl) (y1 - dy1)
    drawErrBar (p_x p1)              y dx
    drawErrBar ((p_x p1 + p_x p2)/2) y dx
    drawErrBar (p_x p2)              y dx
        

allBEBPoints :: (BEBValue y) => [(x, [(y, (y, y))])] -> ([x], [y])
allBEBPoints vals = (xs, (bebsReference bebYs):bebYs)
    where (xs, bebYs) = ((fmap . fmap) fst . fmap concat . unzip) vals

refBEBVal :: (BEBValue y) => [(x, [(y, (y, y))])] -> y
refBEBVal = bebsReference . fmap fst  . concatMap snd


$( makeLenses ''BEBsSettings )
$( makeLenses ''PlotBarErrBars )

-- Lens provided for backward compat.

plot_bebs_line_style :: Lens' (PlotBarErrBars x y) (FillStyle,Maybe LineStyle) 
plot_bebs_line_style = plot_bebs_settings . bebs_settings_line_style

plot_bebs_spacing :: Lens' (PlotBarErrBars x y) PlotBEBsSpacing
plot_bebs_spacing = plot_bebs_settings . bebs_settings_spacing

plot_bebs_alignment :: Lens' (PlotBarErrBars x y) PlotBEBsAlignment
plot_bebs_alignment = plot_bebs_settings . bebs_settings_alignment

plot_bebs_singleton_width :: Lens' (PlotBarErrBars x y) Double
plot_bebs_singleton_width = plot_bebs_settings . bebs_settings_singleton_width

plot_bebs_tick_length :: Lens' (PlotBarErrBars x y) Double
plot_bebs_tick_length = plot_bebs_settings . plot_settings_bebs_tick_length

plot_bebs_overhang :: Lens' (PlotBarErrBars x y) Double
plot_bebs_overhang = plot_bebs_settings . plot_settings_bebs_overhang


-- | When the error is symmetric, we can simply pass in dx for the error. 
symBEBPoint :: (Num a) => a -> a -> (a, (a, a))
symBEBPoint y dy = (y, (y - dy, y + dy))
