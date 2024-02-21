{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}

module Figures.TimeCourse
    ( bcRunFigLegendDia
    , nodeRunDia
    , expGuideDia
    , phRunDia
    ) where

import Types.DMModel
import Types.Simulation
import Types.DMInvestigation
import Types.Figures
import Utilities
import Constants
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Colour as C
import Data.List.Split (splitPlaces)
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Graphics.SVGFonts as F
import qualified Data.List as L
import qualified Data.Bifunctor as BF
import Data.Maybe (fromMaybe, mapMaybe)



bcRunFigLegendDia :: ColorMap -> Barcode -> Diagram B
bcRunFigLegendDia cMap bc = hsep 1.0 evenedBlocks
    where
        evenedBlocks = zipWith (switchLegend cMap) phHeights bc
        phHeights = ((maxBlockHeight * lScale) / ) <$> phSizes
        maxBlockHeight = maximum phSizes
        phSizes = ((fromIntegral . length . phenotypeNames) <$> bc) :: [Double]
        lScale = 5.0 :: Double

switchLegend :: ColorMap -> Double -> Bar -> Diagram B
switchLegend cMap slHeight br = vcat $ (tText'' swName):slices
    where
        slices = (sliceRect <>) <$> coloredLabels
        coloredLabels = swLColorer <$> paddedPhLabels
        swLColorer (isPh, bareLb)
            | isPh = (center . (dottedColorRect |||)) bareLb
            | otherwise = (center . (colorRect |||)) bareLb
        dottedColorRect = cDot `atop` colorRect
        cDot = circle (lScale * 0.5 * 0.6) # fc black # lw none
        colorRect = rect lScale slHeight # fc (cMap M.! swName) # lw none
        sliceRect = (rect (slwidth + lScale) slHeight) # lw 0.1
        paddedPhLabels = lLabelPad' slwidth <<$>> phLabels
        slwidth = maximum $ (width . snd) <$> phLabels
        phLabels = phTextLabel (barPhenotype br) <$> orderedPHNames
        orderedPHNames = (L.reverse . phenotypeNames) br
        swName = switchName br
        tText'' t = F.svgText def (T.unpack t) # F.fit_height (0.75 * lScale)
                                               # F.set_envelope
                                               # fillColor black
                                               # lineWidth none
                                               # center
        lScale = 5.0 :: Double 


lLabelPad' :: Double -> Diagram B -> Diagram B
lLabelPad' widest lLabel = padX tweak lLabel
    where
        tweak = widest / (width lLabel)

phTextLabel :: Maybe PhenotypeName -> PhenotypeName -> (Bool, Diagram B)
phTextLabel mPhName phName = padX 1.1 <$> phLabel
    where
        phLabel
            | mPhName == Just phName = (True, {-bText-} tText'' phName)
            | otherwise = (False, tText'' phName)
        tText'' t = F.svgText def (T.unpack t) # F.fit_height (0.75 * lScale)
                                               # F.set_envelope
                                               # fillColor black
                                               # lineWidth none
                                               # center
        lScale = 5.0 :: Double

-- Make a Node TimeCourse Diagram B, complete with legend on the left. 
nodeRunDia :: ColorMap
           -> ModelMapping
           -> ModelLayer
           -> (Double, Double)
           -> Barcode
           -> [PulseSpacing]
           -> B.Vector RealAnnotatedLayerVec
           -> Diagram B
nodeRunDia cMap mM mL (stripHt, swFWidth) bc pulseSps tmLn =
            (vsep stripHt [pulseLineFig `atop` figBlock, timeAxis])
    where
        timeAxis = alignL $ switchSpacer |||
             (timeAxisDia stripHt $ B.length reordTmLn)
        pulseLineFig = alignL $ switchSpacer ||| pulselines
        switchSpacer = strutX ((width . head) switchFigs)
        pulselines = hcat $ (pulseLine (height figBlock)) <$> pulseInts
        -- We drop the last spacing because we do not need to put a pulse line
        -- at the end of the figure. 
        pulseInts = (L.init . fstOf3 . unzip3) pulseSps
        figBlock = alignL $ vsep stripHt chunkedFigBlocks
        chunkedFigBlocks = zipWith (|||) switchFigs nodeStripBlocks
        switchFigs = zipWith (|||) switchNameFigs nnBlocks
        switchNameFigs = runSwitchTCDia cMap stripHt swFWidth <$> switchPHPairs
        -- We want to annotate the Switch name with the phenotype this run
        -- started in, if such exists, so we pair them before passing to
        -- runSwitchTCDia
        switchPHPairs = findBar bc <$> mM
        nodeStripBlocks = (alignY 0 . vcat) <$>
            (splitPlaces switchLs nodeStrips)
        nnBlocks = nodeNameBlockDia switchLs stripHt switchNodeOrder
        nodeStrips = nodeStripDia reordLNIBMap reordRTs stripHt <$> annTStrips
        annTStrips = zip switchNodeOrder transposedRTmLn
        transposedRTmLn = (L.transpose . B.toList . fmap U.toList) reordTmLn
        -- We want to striate the runs by Switch, so we will need these spacings
        switchLs = (L.length . fst . snd) <$> mM
        -- We need to group the figure by Switch, so we first reorder all of the
        -- AnnotatedLayerVecs in the Timeline, as well as all of our helper data
        -- structures
        reordRTs = U.backpermute rangeTs permuteVec
            where permuteVec = U.fromList $ (lniBMap BM.!) <$> switchNodeOrder
        reordLNIBMap = BM.fromList $ zip switchNodeOrder [0..]
        reordTmLn = case timelineMMReorder (fst <<$>> mM) lniBMap tmLn of
            Right r -> r
            Left errs ->
                error $ "reordTmLn in TimeCourse.hs has: " <> show errs
        LayerSpecs lniBMap rangeTs _ _ = layerPrep mL
        switchNodeOrder = concatMap (fst . snd) mM

-- Mark where each new pulse begins. 
pulseLine :: Double -> Int -> Diagram B
pulseLine lHeight pIndex = lBlock # alignY 0.975  # alignL
    where 
        lBlock = strutX strutWidth ||| vLine
        vLine = vrule lHeight # lc red # lw ultraThin
        strutWidth = fromIntegral pIndex

-- Order the nodes in a node Timeline according to the Switch partitioning
-- of the layer above. 
timelineMMReorder :: U.Unbox a
                  => DMMSModelMapping
                  -> LayerNameIndexBimap
                  -> B.Vector (U.Vector (RealNodeState, a))
                  -> Either InvalidLVReorder
                           (B.Vector (U.Vector (RealNodeState, a)))
timelineMMReorder dmmsMM lniBMap tmLn =
    traverse (annotatedLayerVecReorder lniBMap switchNodeOrder) tmLn
    where
        switchNodeOrder = concatMap snd dmmsMM

-- Re-order an AnnotatedLayerVec according to a new List of NodeNames. Basic
-- error checking, but use at your own risk. 
annotatedLayerVecReorder :: U.Unbox a
                     => LayerNameIndexBimap
                     -> [NodeName]
                     -> U.Vector (RealNodeState, a)
                     -> Either InvalidLVReorder (U.Vector (RealNodeState, a))
annotatedLayerVecReorder lniBMap newOrder annLVec
    | (L.sort . BM.keys) lniBMap /= L.sort newOrder =
        Left NewOldOrderingMismatch
    | BM.size lniBMap /= U.length lVec = Left OldOrderingLVMismatch
    | otherwise = Right $ U.zip reorderedLVec reorderedAnnVec
        where
             reorderedAnnVec = U.backpermute annVec permuteVec
             reorderedLVec = U.backpermute lVec permuteVec
             permuteVec = U.fromList $ (lniBMap BM.!) <$> newOrder
             (lVec, annVec) = U.unzip annLVec

findBar :: Barcode -> Switch -> (Switch, Maybe PhenotypeName)
findBar bc sw = (sw, phN)
    where phN = (L.find (\b -> switchName b == fst sw) bc) >>= barPhenotype

nodeStripDia :: LayerNameIndexBimap
             -> LayerRangeVec
             -> Double
             -> (NodeName, [(RealNodeState, AvgWasForced)])
             -> Diagram B
nodeStripDia reordLNIBMap reordRTs stripHt (nName, annTLine) = timeStrip
    where
        timeStrip = hcat $ nodeStateDia nRange stripHt <$> annTLine
        nRange = reordRTs U.! (reordLNIBMap BM.! nName)

nodeStateDia :: (Int, Int)
             -> Double
             -> (RealNodeState, AvgWasForced)
             -> Diagram B
nodeStateDia (rangeB, rangeT) stripHt (realNState, avgWForced) = dia
    where
--        0 <= avgWForced <= 1 by construction, see averagedAttResults
        dia
            | avgWForced < 0.1 = rect 1 stripHt # plainCPick # lineWidth none
            | avgWForced >= 0.9 = rect 1 stripHt # alteredCPick # lineWidth none
            | otherwise = center (plainRect === alteredRect)
        plainRect = rect 1 plainStripHt # plainCPick # lineWidth none
        alteredRect = rect 1 alteredStripHt # alteredCPick # lineWidth none 
        plainStripHt = stripHt * (1 - avgWForced)
        alteredStripHt = stripHt * avgWForced
        plainCPick = case gradientPick plasmaCM fracRange realNState of
            Nothing -> fcA transparent
            Just cp -> fillColor cp
        alteredCPick = case gradientPick alteredNodeCM fracRange realNState of
            Nothing -> fcA transparent
            Just ca -> fillColor ca
        fracRange = ((fromIntegral rangeB) :: Double, fromIntegral rangeT)

runSwitchTCDia :: ColorMap
               -> Double
               -> Double
               -> (Switch, Maybe PhenotypeName)
               -> Diagram B
runSwitchTCDia cMap stripHt rectW (sw, mPhName) =
    (tText' stripHt swText) <> swBox
    where
        swBox :: Diagram B
        swBox = rect rectW rectH # fillColor swColor # lineWidth none
        swText = case mPhName of
            Nothing -> swName
            Just phName -> swName <> " - " <> phName
        rectH = stripHt * ((fromIntegral . length . fst . snd) sw)
        swColor = cMap M.! swName
        swName = fst sw

-- Make the timeline for the figure. 
timeAxisDia :: Double -> Int -> Diagram B
timeAxisDia strpHght runLength = axisArrow === tLabel
    where
        tLabel = alignL $ tText' (strpHght * 2) "Time"
        axisArrow = alignL $ arrow' (with & headLength .~ tiny)
                      (fromIntegral runLength) # lw thin

-- Make the vertical blocks of NodeName labels, grouped by switch. 
nodeNameBlockDia :: [Int] -> Double -> [NodeName] -> [Diagram B]
nodeNameBlockDia switchLs stripHt switchNodeOrder = alignY 0 <$>  dia
    where
        dia = fmap vcat chunkedPaddednLabels
        chunkedPaddednLabels = splitPlaces switchLs paddedLabels
        paddedLabels = padder maxW <$> nLabels
        padder m d = extrudeLeft 1.0 $ padX (m / (width d)) d
        maxW = maximum $ width <$> nLabels
        nLabels = nodeStripLabelDia stripHt <$> switchNodeOrder

-- Make a NodeName label. 
nodeStripLabelDia :: Double -> T.Text -> Diagram B
nodeStripLabelDia stripHt nName = tText' stripHt nName # alignL

tText' :: Double -> T.Text -> Diagram B
tText' tHt t = F.svgText def (T.unpack t) # F.fit_height tHt
                                      # F.set_envelope
                                      # fillColor black
                                      # lineWidth none
                                      # center

-- Make a Phenotype TimeCourse Diagram B, complete with legend on the left. 
phRunDia :: ColorMap
         -> ModelMapping
         -> Barcode
         -> [PulseSpacing]
         -> B.Vector PhenotypeWeights
         -> Diagram B 
phRunDia cMap mM bc pulseSps tmLn =
    (vsep stripHt [pulseLineFig `atop` figBlock, timeAxis])
    where
        timeAxis = alignL $ switchSpacer |||
             (timeAxisDia stripHt $ B.length tmLn)
        pulseLineFig = alignL $ switchSpacer ||| pulselines
        switchSpacer = strutX ((width . head) switchFigs)
        pulselines = hcat $ (pulseLine (height figBlock)) <$> (pulseInts)
        -- We drop the last spacing because we do not need to put a pulse line
        -- at the end of the figure. 
        pulseInts = (L.init . fstOf3 . unzip3) pulseSps
        figBlock = alignL $ vsep stripHt chunkedFigBlocks
        chunkedFigBlocks = zipWith (|||) switchFigs switchStripBlocks
        switchFigs = zipWith (|||) switchNameFigs paddedPHNBlocks
        switchNameFigs = runSwitchPHDia cMap stripHt swFWidth <$> switchPHPairs
        -- We want to annotate the Switch name with the phenotype this run
        -- started in, if such exists, so we pair them before passing to
        -- runSwitchPHDia
        switchPHPairs = findBar bc <$> strippedSwitches
        paddedPHNBlocks = padder maxphNBlockW <$> phNBlocks
        maxphNBlockW = maximum $ width <$> phNBlocks
        padder m d = extrudeLeft 1.0 $ padX (m / (width d)) d
        phNBlocks = phNameBlockDia cMap stripHt <$> switchPhs 
        switchStripBlocks = (alignY 0 . vcat) <$> switchStrips
        switchStrips :: [[Diagram B]]
        switchStrips = switchStripsDia cMap stripHt tmLn <$> switchPhs
        (stripHt, swFWidth) = (2.0, 24.0) :: (Double, Double)
        switchPhs = snd <<$>> strippedSwitches
        -- Note that some Switches do not have Phenotypes, so these must be
        -- stripped out. 
        strippedSwitches = filter (not . L.null . snd . snd) mM

phNameBlockDia :: ColorMap -> Double -> (NodeName, [Phenotype]) -> Diagram B
phNameBlockDia cMap stripHt (nName, phs) = (alignY 0 . vcat) paddedLabels
    where
        paddedLabels = padder maxW <$> phLabels
        padder m d = extrudeLeft 1.0 $ padX (m / (width d)) d
        maxW = maximum $ width <$> phLabels
        phLabels = phStripLabelDia stripHt <$> coloredPhs
        coloredPhs = zip blendedColors (phenotypeName <$> phs)
        blendedColors = phTCBlend baseColor (L.length phs)
        baseColor = cMap M.! nName

phStripLabelDia :: Double -> (LocalColor, PhenotypeName) -> Diagram B
phStripLabelDia stripHt (phC, phN) = tText' stripHt phN # alignL # fc phC

switchStripsDia :: ColorMap
                -> Double
                -> B.Vector PhenotypeWeights
                -> (NodeName, [Phenotype])
                -> [Diagram B]
switchStripsDia cMap stripHt tmLn (nName, phs) = dias
    where
        dias = phStripDia stripHt tmLn <$> coloredPhs
        coloredPhs = zip blendedColors (phenotypeName <$> phs)
        blendedColors = phTCBlend baseColor (L.length phs)
        baseColor = cMap M.! nName

-- Produce a spread of colors for the Phenotypes of a
-- Switch. 
phTCBlend :: LocalColor -> Int -> [LocalColor]
phTCBlend swColor phCount
    | phCount <= 0 = []
    | phCount == 1 = [swColor]
    | otherwise = flip C.darken swColor <$> stepF phCount
    where
        stepF :: Int -> [Double]
        stepF i = ((1 -) . ((darkAnchor/(x-1)) *)) <$> [0..x-1]
            where x = fromIntegral i
        darkAnchor = 0.65 -- How close to black do we want to go?

phStripDia :: Double
           -> B.Vector PhenotypeWeights
           -> (LocalColor, PhenotypeName)
           -> Diagram B
phStripDia stripHt tmLn phPair = timeStrip
    where
        timeStrip = hcat $ phStateDia stripHt phPair <$> B.toList tmLn

phStateDia :: Double
           -> (LocalColor, PhenotypeName)
           -> PhenotypeWeights
           -> Diagram B
phStateDia stripHt (phColor, phName) phWeights = dia
    where
        dia = rect 1 stripHt # fcA cPick # lineWidth none
        cPick
            | phWeight == 0 = C.transparent
            | otherwise = C.withOpacity phColor phWeight
        phWeight = M.findWithDefault 0 phName phWeights

runSwitchPHDia :: ColorMap
               -> Double
               -> Double
               -> (Switch, Maybe PhenotypeName)
               -> Diagram B
runSwitchPHDia cMap stripHt rectW (sw, mPhName) =
    (tText' stripHt swText) <> swBox
    where
        swBox :: Diagram B
        swBox = rect rectW rectH # fillColor swColor # lineWidth none
        swText = case mPhName of
            Nothing -> swName
            Just phName -> swName <> " - " <> phName
        rectH = stripHt * ((fromIntegral . length . snd . snd) sw)
        swColor = cMap M.! swName
        swName = fst sw

-- Make a clarifying guide for phenotype time course figures. 
expGuideDia :: ModelLayer
            -> DMExperimentMeta
            -> Double
            -> [PulseSpacing]
            -> Diagram B
expGuideDia mL exMeta stripHt pIs = expTypeText ||| pulseDia
    where
        pulseDia = hcat $ L.intersperse tickDia pulseDias
        tickDia = vrule (maximum (height <$> pulseDias)) # lw ultraThin # alignT
        pulseDias = pulseSpacingDia stripHt <$> inputStrippedPIs
        inputStrippedPIs = inputStrip mLInputNames lniBMap pIs
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        mLInputNames =
            ((fmap . fmap) (nodeName . nodeMeta) . inputs . modelGraph) mL
        expTypeText = tText' stripHt ((T.pack . show . expKind) exMeta <> " ")

-- Strip out unchanging inputs from a PulseSpacing series, and prep those inputs
-- for display. 
inputStrip :: [[NodeName]]
           -> LayerNameIndexBimap
           -> [PulseSpacing]
           -> [(Int, [[(NodeName, RealNodeState)]], [NodeAlteration])]
inputStrip mLInputNames lniBMap pIs = stripper <$> groupedIptIPs
    where
        stripper (a, inpts, c) = (a, filter stripper' inpts, c)
            where
                stripper' inpt = snd $ inputsHaveChanged M.! (fst <$> inpt)
--      If a [NodeName] key is associated with a False, then it remains
--      constant throughout. If True, then it changes as some point. 
        inputsHaveChanged :: M.HashMap [NodeName] ([RealNodeState], Bool)
        inputsHaveChanged = foldr changeCheck mempty groupedIptIPs
        changeCheck (_, inpSts, _) inputM = foldr chCk inputM inpSts
            where
                chCk inpt iM = M.insertWith inserter ns (sts, True) iM
                    where
                        inserter (nSTS, _) (oSts, _)
                            | oSts == nSTS = (oSts, False)
                            | otherwise = (nSTS, True)
                        ns = fst <$> inpt
                        sts = snd <$> inpt
        groupedIptIPs = groupInputs mLInputNames lniBMap <$> pIs

-- Given an [[NodeName]] that represents the NodeName of the inputs of a
-- ModelLayer and a RealInputCoord, produce the [[(Nodename, RealNodeState)]]
-- that are the inputs actually set by that RealInputCoord. 
groupInputs :: [[NodeName]]
            -> LayerNameIndexBimap
            -> PulseSpacing
            -> (Int, [[(NodeName, RealNodeState)]], [NodeAlteration])
groupInputs inputNames lniBMap (a, inputV, c) = (a, inputStates, c)
    where
        inputStates = mapMaybe (grouper namedCoordL) inputNames
--      This convoluted nonsense to separate out each input is necessary to keep
--      the correct node order from the [[DMNode]] inputs. That then lets us
--      determine what level that input is set to. 
        grouper iVL ns = traverse (grouper' iVL) ns
        grouper' vL n = L.find ((== n) . fst) vL
        namedCoordL :: [(NodeName, RealNodeState)]
        namedCoordL = (BF.first (lniBMap BM.!>)) <$> (U.toList inputV)

pulseSpacingDia :: Double
                -> (Int, [[(NodeName, RealNodeState)]], [NodeAlteration])
                -> Diagram B
pulseSpacingDia stripHt (pW, inputLs, nAlts) =
    tText' stripHt iptSetT === hDiv === vcat (tText' stripHt <$> nAltTexts)
    where
        iptSetT = T.intercalate ", " $ inputCoordText <$> inputLs
        nAltTexts
            | L.null nudges && L.null locks = []
            | L.null nudges = [lockText]
            | L.null locks = [nudgeText]
            | otherwise = [lockText, nudgeText]
        nudgeText = "Nudge: " <> T.intercalate ", "  (nAltTPrep <$> locks)
        lockText = "Lock: " <> T.intercalate ", "  (nAltTPrep <$> locks)
        (locks, nudges) = L.partition isNodeLock nAlts
        hDiv = hrule (fromIntegral pW) # lw ultraThin

nAltTPrep :: NodeAlteration -> T.Text
nAltTPrep (NodeLock nlN nlS nlP) = nlN <> " to " <> tpsh nlS <> "@" <> tpsh nlP
nAltTPrep (GradientNudge gN gDir gP) = gN <> " " <> shND gDir <> "@" <> tpsh gP
    where
        shND NudgeUp = "up"
        shND NudgeDown = "down"

-- Given a RealInputCoord, which environmental inputs are being set, and to
-- which real-valued levels? Presumes that the RealInputCoord is properly formed
-- and belongs to the [[DMNode]] (inputs) and LayerNameIndexBimap in question. 
inputCoordText :: [(NodeName, RealNodeState)] -> T.Text
inputCoordText [] = ""
inputCoordText [(nN, nS)] = nN <> ":" <> tpsh nS
inputCoordText ipts = nN <> ":" <> tpsh nS
    where (nN, nS) = fromMaybe (L.last ipts) (L.find ((> 0) . snd) ipts)

tpsh :: Show a => a -> T.Text
tpsh = T.pack . show
