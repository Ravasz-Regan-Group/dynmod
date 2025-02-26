{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Figures
    ( PUCGradient
    , SVGText
    , InputBundle(..)
    , Barcode
    , BarcodeFilter(..)
    , Bar(..)
    , BarKind(..)
    , Slice(..)
    , RealNodeState
    , NodeAlteration(..)
    , LockProbability
    , NudgeProbability
    , RealInputCoord
    , NudgeDirection(..)
    , BoolNudgeDirection
    , GeneralDuration(..)
    , Duration
    , PulseSpacing
    , phenotypeMatch
    , mkBarcode
    , ColorMap
    , mkColorMap
    , PhColorMap
    , mkPhColorMap
    , phTCBlend
    , gradientPick
    , attMatch
    , bcFilterF
    , barPhenotype
    , StdDev
    , ThreadSlice
    , isSSMatch
    , isNodeLock
    , durationMagnitude
    , inputStrip
    , groupInputs
    , nAltTPrep
    , inputCoordText
    , resCombine
    ) where    

import Types.DMModel
import Types.Simulation
import Utilities
import qualified Data.Text as T
import TextShow
import TextShow.Data.Char (showbString, showbChar)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.Bimap as BM
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Hashable
import qualified Data.List.Extra as L
import qualified Data.Colour as C
import GHC.Generics (Generic)
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Data.Ix (range)
import qualified Data.Bifunctor as BF

-- Basic types to support all types of figures.

type SVGText = T.Text
type ColorMap = M.HashMap NodeName LocalColor
type PhColorMap = M.HashMap PhenotypeName LocalColor
type PUCGradient = B.Vector LocalColor
type StdDev = Double

-- Combines results from TimeCourses or Scans by equality on Barcodes, because
-- all of the (Barcode, RepResults or ScanResult)s for a given Barcode will be
-- turned into figures in one PDF file. 
resCombine :: [(Barcode, a)] -> [(Barcode, [a])]
resCombine ars = M.toList $ M.fromListWith (<>) preppedArs
    where preppedArs = pure <<$>> ars

type RealNodeState = Double

-- Set an environmental input (or inputs) to a particular value. Real
-- values will be stochastically set on each time-step. 
type RealInputCoord = U.Vector (NodeIndex, RealNodeState)

data NodeAlteration = NodeLock NodeName NodeState LockProbability
                    | GradientNudge NodeName
                                    NudgeDirection
                                    NudgeProbability
                    deriving (Eq, Show, Ord)

isNodeLock :: NodeAlteration -> Bool
isNodeLock (NodeLock _ _ _) = True
isNodeLock (GradientNudge _ _ _) = False

-- isGradientNudge :: NodeAlteration -> Bool
-- isGradientNudge (GradientNudge _ _ _) = True
-- isGradientNudge (NodeLock _ _ _) = False

data NudgeDirection = NudgeUp
                    | NudgeDown
                    deriving (Eq, Show, Ord, Enum)

instance TextShow NudgeDirection where
    showb NudgeUp = showbString "NudgeUp"
    showb NudgeDown = showbString "NudgeDown"

type LockProbability = Probability
-- NudgeDirection is its own sum type, but we need nudging to be fast-ish
-- when we alter NodeStates in the middle of a DMExperiment, so:
-- False = NudgeDown
-- True  = NudgeUp
type BoolNudgeDirection = Bool
type NudgeProbability = Probability

data GeneralDuration a = DefaultD a | UserD a
                       deriving (Eq, Ord, Read, Show)

instance Functor GeneralDuration where
    fmap f (DefaultD x) = DefaultD $ f x
    fmap f (UserD x) = UserD $ f x

instance (TextShow a) => TextShow (GeneralDuration a) where
    showb (DefaultD x) = showbString "DefaultD" <> showbSpace <> showb x
    showb (UserD x) = showbString "UserD" <> showbSpace <> showb x

-- DefaultD is a default duration, and may be altered by the length of the
-- Attractor that the pulse starts in. UserD is specified by the user, and may
-- not be so altered. 
type Duration = GeneralDuration Int

durationMagnitude :: Duration -> Int
durationMagnitude (DefaultD i) = i
durationMagnitude (UserD i) = i

-- The spacing between pulses, along with any input changes or node alterations.
type PulseSpacing = (Int, RealInputCoord, [NodeAlteration])

-- slices of a timeline, of the form (a, b) | b >= a >= 0
type ThreadSlice = (Int, Int)


mkColorMap :: DMModel -> ColorMap
mkColorMap dmm = M.fromList nameColorPairs
    where
        nameColorPairs = (\n -> (nodeName n, nodeColor n)) <$> nodesMetas
        nodesMetas = nodeMeta <$> ((concat . modelNodes) dmm)

mkPhColorMap :: ModelMapping -> ColorMap -> PhColorMap
mkPhColorMap mM cMap = (M.fromList . concatMap phBlendF) nonEmptyPhs
    where
        phBlendF (nN, phs) = zip (phenotypeName <$> phs) cBlends
            where
                cBlends = phTCBlend 0.85 (cMap M.! nN) (L.length phs)
        nonEmptyPhs = ((fmap . fmap) snd . nonEmptyPhenotypes) mM

-- Pick a color from one of the perceptually uniform color gradients in
-- Constants. 
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
        transform = (gradientSize - 1) / magnitude -- Avoid off-by-one error
        magnitude = high - low
        gradientSize = (fromIntegral . B.length) pucGr

-- Produce a spread of colors for the Phenotypes of a
-- Switch. 
phTCBlend :: C.ColourOps a => Double -> a Double -> Int -> [a Double]
phTCBlend darkAnchor swColor phCount
    | phCount <= 0 = []
    | phCount == 1 = [swColor]
    | otherwise = flip C.darken swColor <$> stepF phCount
    where
        stepF :: Int -> [Double]
        stepF i = ((1 -) . ((darkAnchor/(x-1)) *)) <$> [0..x-1]
            where x = fromIntegral i
        -- darkAnchor: How close to black do we want to go?


type Barcode = [Bar]
data Bar = BR { barKind :: BarKind
              , attractorSize :: Int
              , switchName :: NodeName
              , phenotypeNames :: [PhenotypeName]
              } deriving (Eq, Show, Generic)
instance Hashable Bar

-- Types to construct 5D diagrams with BarCodeClusters.


-- For a given Phenotype, where (if anywhere) do the SubSpaces of the Phenotype
-- match to state(s) of the resident global Attractor. Both Phenotypes and
-- Attractors can be points or loops, so the matching deserves some comment.
-- A Phenotype which maps, in its entirety, onto a subset of the Attractor is a
-- match, for those states of the attractor where the SubSpaces line up with the
-- it. A Phenotype whose length is longer than the resident Attractor, or
-- whose SubSpaces match out of order, or none of whose SubSpaces match at all,
-- is a Miss. 
data BarKind = FullMiss BarHeight
             | MatchBar [Slice]
                        LocalColor
             deriving (Eq, Show, Generic)
instance Hashable BarKind

type BarHeight = Int

data Slice = Match AttractorSize AttractorMatchIndices PhenotypeName
           | Miss AttractorSize
           deriving (Eq, Show, Generic)
instance Hashable Slice
instance TextShow Slice where
    showb (Match attSize attMatchIndices phName) = showbString "Match " <>
        showb attSize <> showbChar ' ' <> showb attMatchIndices <>
        showbChar ' ' <> showb phName
    showb (Miss attSize) = showbString "Miss " <> showb attSize

type AttractorSize = Int
type AttractorMatchIndices = [[Int]]

-- The 1-5 Environments that will form the axes of environmental diagrams,
-- possibly in an order specified in an ISFSpec, any remaining inputs fixed to
-- particular values to prevent the diagram from containing many extraneous
-- attractors, and maybe a BarcodeFilter. 
data InputBundle = InputBundle { ibInputs :: [[DMNode]]
                               , ibFixedVec :: FixedVec
                               , ibBCFilter :: Maybe BarcodeFilter
                               } deriving (Eq, Show)

data SliceCandidate = MissCandidate AttractorSize
                    | RedLineCandidate BarHeight MatchCount PhenotypeName
                    | MatchCandidate AttractorSize
                                     AttractorMatchIndices
                                     PhenotypeName
                    deriving (Eq, Show)
type MatchCount = Int

-- Sometimes input space figures have many Barcodes at each input coordinate. 
-- Barcode filters allow the user to exclude irrelevant Barcodes, or only
-- include particularly relevant Barcodes. 
data BarcodeFilter =
-- Only include if at ANY point along an attractor it matches to the phenotype.
      OnlyBarCodesWithAny [(NodeName, PhenotypeName)]
-- Only include if at EVERY point along an attractor it matches to the
-- phenotype. 
    | OnlyBarCodesWithAll [(NodeName, PhenotypeName)]
-- Exclude if at ANY point along an attractor it matches to the phenotype. 
    | ExcludeBarCodesWithAny [(NodeName, PhenotypeName)]
-- Exclude if at EVERY point along an attractor it matches to the phenotype
    | ExcludeBarCodesWithAll [(NodeName, PhenotypeName)]
      deriving (Eq, Show, Ord)

instance TextShow BarcodeFilter where
    showb (OnlyBarCodesWithAny flts) = showbString "OnlyBarCodesWithAny" <>
        showbList flts
    showb (OnlyBarCodesWithAll flts) = showbString "OnlyBarCodesWithAll" <>
        showbList flts
    showb (ExcludeBarCodesWithAny flts) = showbString "ExcludeBarCodesWithAny" <>
        showbList flts
    showb (ExcludeBarCodesWithAll flts) = showbString "ExcludeBarCodesWithAll" <>
        showbList flts

-- Does an attractor exist at a particular point in the space of environmental
-- inputs?
attMatch :: FixedVec -> Attractor -> Bool
attMatch fVec att = U.all (checkV att) fVec
    where
        checkV anAtt (nIndex, nState) = ((B.head anAtt) U.! nIndex) == nState

-- Generate a Barcode to represent Attractors on environment-space figures.
-- Assumes the ColorMap order matches that of the Attractor. Return the
-- Attractor as well, because we will often want to filter Attractors by the
-- properties of their associated Barcodes. 
mkBarcode :: ColorMap
          -> ModelMapping
          -> LayerNameIndexBimap
          -> Attractor
          -> (Barcode, Attractor)
mkBarcode cM mM lniBMap att = (bc, att)
    where
        bc = (uncurry (mkBar lniBMap att)) <$> colorSwitchPairs
        colorSwitchPairs = (\(sN, ps) -> (cM M.! sN, (sN, ps))) <$> nameSPairs
        nameSPairs = (\(nName, (_, phs)) -> (nName, phs)) <$> nonEmptyPhs
        nonEmptyPhs = nonEmptyPhenotypes mM


-- Make a single Bar in a Barcode. 
mkBar :: LayerNameIndexBimap
      -> Attractor
      -> LocalColor
      -> (NodeName, [Phenotype])
      -> Bar
mkBar lniBMap att sColor (sName, phs)
    | L.all L.null matchTHSlicess = BR (FullMiss swSize) attSize sName phNames
    | otherwise = BR (MatchBar slices sColor) attSize sName phNames
    where
        slices = (uncurry (mkSlice attSize)) <$> (zip phNames matchTHSlicess)
        matchTHSlicess = phMatch 0 lniBMap att <$> orderedPHs 
--      We order the Phenotypes by switchNodeState descending so that the Bar
--      will have the 0 state at the bottom, rather than the top. 
        phNames = phenotypeName <$> orderedPHs
        orderedPHs = (L.reverse . L.sortOn switchNodeState) phs
        attSize = L.length att
        swSize = L.length orderedPHs


bcFilterF :: Maybe BarcodeFilter -> Barcode -> Bool
bcFilterF Nothing _ = True
bcFilterF (Just (OnlyBarCodesWithAny sPhPairs)) bc =
    all (phCheckAny (bcPairs bc)) sPhPairs
bcFilterF (Just (OnlyBarCodesWithAll sPhPairs)) bc =
    all (phCheckAll (bcPairs bc)) sPhPairs
bcFilterF (Just (ExcludeBarCodesWithAny sPhPairs)) bc = not $
    all (phCheckAny (bcPairs bc)) sPhPairs
bcFilterF (Just (ExcludeBarCodesWithAll sPhPairs)) bc = not $
    all (phCheckAll (bcPairs bc)) sPhPairs

bcPairs :: Barcode -> [(NodeName, BarKind)]
bcPairs = fmap (\x -> (switchName x, barKind x))

phCheckAny :: [(NodeName, BarKind)] -> (NodeName, PhenotypeName) -> Bool
phCheckAny bcPs (nName, phName) = case L.find ((==) nName . fst) bcPs of
    Nothing -> False
    Just (_, FullMiss _) -> False
    Just (_, MatchBar slcs _) -> case L.find (matchSlice phName) slcs of
        Nothing -> False
        Just (Match _ _ _) -> True
        Just (Miss _) -> False

phCheckAll :: [(NodeName, BarKind)] -> (NodeName, PhenotypeName) -> Bool
phCheckAll bcPs (nName, phName) = case L.find ((==) nName . fst) bcPs of
    Nothing -> False
    Just (_, FullMiss _) -> False
    Just (_, MatchBar slcs _) -> case L.find (matchSlice phName) slcs of
        Nothing -> False
        Just (Match attSize attMatchess _) ->
            attSize == (sum . fmap length) attMatchess
        Just (Miss _) -> False

matchSlice :: PhenotypeName
           -> Slice
           -> Bool
matchSlice _ (Miss _) = False
matchSlice phName (Match _ _ checkedPhN) =  phName == checkedPhN

barPhenotype :: Bar -> Maybe PhenotypeName
barPhenotype br = case barKind br of
    FullMiss _ -> Nothing
    MatchBar slcs _ -> foldr bestSl Nothing slcs
        where
            bestSl sl bestS = case sl of
                Miss _ -> bestS
                Match attSize attMatchess phName
                    | attSize ==  (sum . fmap length) attMatchess -> Just phName
                    | otherwise -> bestS

mkSlice :: AttractorSize -> PhenotypeName -> [ThreadSlice] -> Slice
mkSlice attSize _ [] = Miss attSize
mkSlice attSize phName thSlices = Match attSize (range <$> thSlices) phName

-- Mark where on a Thread the Phenotypes of its ModelMapping match.
phenotypeMatch :: LayerNameIndexBimap
               -> [Phenotype]
               -> Thread
               -> B.Vector [PhenotypeName]
phenotypeMatch lniBMap phs thread = B.generate (B.length thread) lookuper
    where
        lookuper i = M.findWithDefault [] i indexMap
        indexMap = L.foldl' integrator M.empty phSlices
        integrator :: M.HashMap Int [PhenotypeName]
                   -> (PhenotypeName, [ThreadSlice])
                   -> M.HashMap Int [PhenotypeName]
        integrator indxM (phName, thSlices) = L.foldl' integ indxM thSlices
            where
                integ :: M.HashMap Int [PhenotypeName]
                      -> (Int, Int)
                      -> M.HashMap Int [PhenotypeName]
                integ iM (start, end) = L.foldl' itg iM [start..end]
                    where
                        itg :: M.HashMap Int [PhenotypeName]
                            -> Int
                            -> M.HashMap Int [PhenotypeName]
                        itg aM j = M.insertWith (<>) j [phName] aM
        phSlices = zip phNames $ phMatch 0 lniBMap thread <$> phs
        phNames = phenotypeName <$> phs

-- Find the places a Phenotype is present in a Thread.
phMatch :: Int -> LayerNameIndexBimap -> Thread -> Phenotype -> [ThreadSlice]
phMatch offSet lniBMap thread ph
  | fPrintSize > thSize = []
  | otherwise = case phMatchReorder intPh thread of
    Nothing -> []
    Just ordIntPh
      | not $ areStrictlyIncreasing preppedMatches -> []
      | any (== []) preppedMatches -> []
      | otherwise -> case mIntNoRepeatSS of
        Just intNoRepeatSS
          | nRSSIndex == 0 -> (mkRange preppedTFMs):extraSlices
          | nRSSIndex == ((L.length matches) - 1) ->
            (mkRange preppedTLMs):extraLSlices
          | otherwise -> case (length . fst . (matches L.!!)) nRSSIndex of
            1 -> (mkRange preppedMatches):extraSlices
            _ -> []
          where
            nRSSIndex = fromJust (L.findIndex ((==intNoRepeatSS) . snd) matches)
        Nothing -> (mkRange preppedMatches):extraSlices
        where
          preppedTFMs = fst <$> trimmedFMatches
          trimmedFMatches = trimmedFHead:(tail matches)
          trimmedFHead :: ([Int], IntSubSpace)
          trimmedFHead = (BF.first ((:[]) . last) . head) matches
          extraLSlices
            | B.null dpLThread = []
            | otherwise = phMatch newOffset lniBMap dpLThread ph
            where
                newOffset = trimmedLMatchesSize + offSet
                dpLThread = B.drop trimmedLMatchesSize thread
          preppedTLMs = fst <$> trimmedLMatches
          trimmedLMatchesSize = lmIndexF trimmedLMatches
          trimmedLMatches = (init matches) `L.snoc` trimmedLLast
          trimmedLLast :: ([Int], IntSubSpace)
          trimmedLLast = (BF.first ((:[]) . head) . last) matches
          extraSlices
            | B.null dpThread = []
            | otherwise = phMatch newOffSet lniBMap dpThread ph
            where
                newOffSet = lmIndexF matches + offSet
                dpThread = B.drop (lmIndexF matches) thread
          lmIndexF :: [([Int], IntSubSpace)] -> Int
          lmIndexF = ((+1) . last . fst . last) 
          preppedMatches = fst <$> matches
          matches = (matchLocation thread) <$> ordIntPh
          mkRange zs = ( offSet + (minimum . head) zs
                       , offSet + (maximum . last) zs)
  where
    mIntNoRepeatSS :: Maybe IntSubSpace
    mIntNoRepeatSS = (fmap toIntSubSpace . markedSubSpace) ph
    intPh = toIntSubSpace <$> fPrint
    toIntSubSpace = fmap (BF.first (lniBMap BM.!))
    (thSize, fPrintSize) = (B.length thread, L.length fPrint)
    fPrint = fingerprint ph

-- Do any of the Int-converted SubSpaces in the Phenotype match to any
-- state in the timeline? If so, reorder the Phenotype at the first Phenotype
-- SubSpace that matches any Attractor state and return it. 
phMatchReorder :: [IntSubSpace] -> Thread -> Maybe [IntSubSpace]
phMatchReorder intPh thread = breakF <$> (B.findIndex (isAttMatch intPh) thread)
    where
        breakF i = mergeTuple $ L.break (isSSMatch (thread B.! i)) intPh
        isAttMatch :: [IntSubSpace] -> LayerVec -> Bool
        isAttMatch iph lVec = any (isSSMatch lVec) iph
        mergeTuple (x, y) = y <> x

-- Do the states in the Int-converted Subspace match the equivalent states in
-- the LayerVec?
isSSMatch :: LayerVec -> IntSubSpace -> Bool
isSSMatch lV sS = all (isStateMatch lV) sS
    where
        isStateMatch lVec (nodeNameInt, nState) = nState == lVec U.! nodeNameInt

-- Find the location of the first places in the Thread, if any, that the Int-
-- converted SubSpace matches. Repeats are permitted at this step, so we return
-- a (possibly empty) list of succesive Ints. 
matchLocation :: Thread -> IntSubSpace -> ([Int], IntSubSpace)
matchLocation thread sS = case B.findIndex (flip isSSMatch sS) thread of
    Nothing -> ([], sS)
    Just firstMatchI -> (unfolderF firstMatchI, sS)
    where
        unfolderF i = i : L.unfoldr unF (i + 1)
        unF j
            | j >= B.length thread = Nothing 
            | isSSMatch (thread B.! j) sS = Just (j, j + 1)
            | otherwise = Nothing

-- We need this in Figures.TimeCourse and Figures.BarCharts. 
-- Strip out unchanging inputs from a PulseSpacing series, and prep those inputs
-- for display. 
inputStrip :: [[NodeName]]
           -> LayerNameIndexBimap
           -> Maybe RealInputCoord
           -> [PulseSpacing]
           -> [(Int, [[(NodeName, RealNodeState)]], [NodeAlteration])]
inputStrip mLInputNames lniBMap mRIC pSps = stripper <$> grpdIptIPs
    where
        stripper (a, inpts, c) = (a, filter stripper' inpts, c)
            where
                stripper' inpt = inputsHaveChanged M.! (fst <$> inpt)
--      If a [NodeName] key is associated with a False, then it remains
--      constant throughout. If True, then it changes as some point. 
        inputsHaveChanged :: M.HashMap [NodeName] Bool
        inputsHaveChanged = M.map (\hs -> HS.size hs > 1) gatheredHM
        gatheredHM :: M.HashMap [NodeName] (HS.HashSet [Double])
        gatheredHM = foldr changeCheck initialHM (sndOf3 <$> grpdIptIPs)
        initialHM = maybe mempty (flip changeCheck mempty) grpdMRIC
        grpdMRIC = groupInputs mLInputNames lniBMap <$> mRIC
        changeCheck inpSts inputM = foldr chCk inputM inpSts
            where
                chCk inpt iM = M.insertWith HS.union ns (HS.singleton sts) iM
                    where
                        ns = fst <$> inpt
                        sts = snd <$> inpt
        grpdIptIPs = (fmap . BF.first) (groupInputs mLInputNames lniBMap) pSps

-- Given an [[NodeName]] that represents the NodeName of the inputs of a
-- ModelLayer and a RealInputCoord, produce the [[(Nodename, RealNodeState)]]
-- that are the inputs actually set by that RealInputCoord. 
groupInputs :: [[NodeName]]
            -> LayerNameIndexBimap
            -> RealInputCoord
            -> [[(NodeName, RealNodeState)]]
groupInputs inputNames lniBMap inputV = inputStates
    where
        inputStates = mapMaybe (grouper namedCoordL) inputNames
--      This convoluted nonsense to separate out each input is necessary to keep
--      the correct node order from the [[DMNode]] inputs. That then lets us
--      determine what level that input is set to. 
        grouper iVL ns = traverse (grouper' iVL) ns
        grouper' vL n = L.find ((== n) . fst) vL
        namedCoordL :: [(NodeName, RealNodeState)]
        namedCoordL = (BF.first (lniBMap BM.!>)) <$> (U.toList inputV)

nAltTPrep :: NodeAlteration -> T.Text
nAltTPrep (NodeLock nlN nlS nlP) = nlN <> " to " <> showt nlS <> "@" <>
    showt nlP
nAltTPrep (GradientNudge gN gDir gP) = gN <> " " <> shND gDir <> "@" <> showt gP
    where
        shND NudgeUp = "up"
        shND NudgeDown = "down"

-- Given a RealInputCoord, which environmental inputs are being set, and to
-- which real-valued levels? Presumes that the RealInputCoord is properly formed
-- and belongs to the [[DMNode]] (inputs) and LayerNameIndexBimap in question. 
inputCoordText :: [(NodeName, RealNodeState)] -> T.Text
inputCoordText [] = ""
inputCoordText [(nN, nS)] = nN <> ":" <> showt nS
inputCoordText ipts = nN <> ":" <> showt nS
    where (nN, nS) = fromMaybe (L.last ipts) (L.find ((> 0) . snd) ipts)

