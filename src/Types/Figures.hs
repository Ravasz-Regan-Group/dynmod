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
    , PHEIndex
    , ErrorLength
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
    , toIntSubSpace
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
import TextShow.Data.Char (showbLitString, showbChar)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.Bimap as BM
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Hashable
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NL
import qualified Data.Colour as C
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Data.Ix (range)
import qualified Data.Bifunctor as BF

-- Basic types to support all manner of figures.

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
    where preppedArs = L.singleton <<$>> ars

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
    showb NudgeUp = showbLitString "NudgeUp"
    showb NudgeDown = showbLitString "NudgeDown"

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
    showb (DefaultD x) = showbLitString "DefaultD" <> showbSpace <> showb x
    showb (UserD x) = showbLitString "UserD" <> showbSpace <> showb x

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
             | MatchBar [Slice] LocalColor
             deriving (Eq, Show, Generic)
instance Hashable BarKind

type BarHeight = Int

data Slice = Match AttractorSize
                   AttractorMatchIndices
                   PhenotypeName
           | Miss AttractorSize
           deriving (Eq, Show, Generic)
instance Hashable Slice
instance TextShow Slice where
    showb (Match attSize attMatchIndices phName) = showbLitString "Match " <>
        showb attSize <> showbChar ' ' <> showb attMatchIndices <>
        showbChar ' ' <> showb phName
    showb (Miss attSize) = showbLitString "Miss " <> showb attSize

type AttractorSize = Int
-- For now, PhenenotypeErrors are strict subloops of their parent Phenotype.
type AttractorMatchIndices = NL.NonEmpty
    (PhenotypeName, [[Int]], Maybe (PHEIndex, ErrorLength))
-- If the match is a PhenotypeError, include its error index and length
type ErrorLength = Int

-- The 1-5 Environments that will form the axes of environmental diagrams,
-- possibly in an order specified in an ISFSpec, any remaining inputs fixed to
-- particular values to prevent the diagram from containing many extraneous
-- attractors, and maybe a BarcodeFilter. 
data InputBundle = InputBundle { ibInputs :: [[DMNode]]
                               , ibFixedVec :: FixedVec
                               , ibBCFilter :: Maybe BarcodeFilter
                               } deriving (Eq, Show)

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
    showb (OnlyBarCodesWithAny flts) = showbLitString "OnlyBarCodesWithAny" <>
        showbList flts
    showb (OnlyBarCodesWithAll flts) = showbLitString "OnlyBarCodesWithAll" <>
        showbList flts
    showb (ExcludeBarCodesWithAny flts) =
        showbLitString "ExcludeBarCodesWithAny" <> showbList flts
    showb (ExcludeBarCodesWithAll flts) =
        showbLitString "ExcludeBarCodesWithAll" <> showbList flts

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
    | L.null matchTHSlices = BR (FullMiss swSize) attSize sName phNames
    | otherwise = BR (MatchBar slices sColor) attSize sName phNames
    where
        slices = (uncurry (mkSlice attSize phErrMap)) <$> 
            (zip phNames matchTHSlices)
        matchTHSlices = (fmap . fmap . fmap . fmap) mkRange matchTHInts
        matchTHInts = uncurry (wholePhMatch lniBMap) <$> orderedPairs
        orderedPairs = attractorReorder lniBMap att <$> orderedPHs
--      An Attractor might contain a Phenotype in an incorrect order, because
--      the begining of an Attractor is found randomnly in state space. If we
--      match the first SubSpace of a loop Phenotype anywhere in an attractor,
--      we reorder the Attractor to put that step first. 
        
--      We order the Phenotypes by switchNodeState descending so that the Bar
--      will have the 0 state at the bottom, rather than the top. 
        phNames = phenotypeName <$> orderedPHs
        phErrMap = mkPhErrorMap phs
        orderedPHs = (L.reverse . L.sortOn switchNodeState) phs
        attSize = L.length att
        swSize = L.length orderedPHs

-- Sometimes we need a map of which PhenotypeNames are PhenotypeErrorNames, and
-- their properties. 
mkPhErrorMap :: [Phenotype] -> M.HashMap PhenotypeName (PHEIndex, ErrorLength)
mkPhErrorMap phs = M.fromList $ builderF <$> phErrors
    where
        builderF phE = (phErrorName phE, phEData)
            where phEData = (phIndex phE, (length . phErrorFingerprint) phE)
        phErrors = concatMap phenotypeErrors phs

attractorReorder :: LayerNameIndexBimap
                 -> Attractor
                 -> Phenotype
                 -> (Attractor, Phenotype)
attractorReorder lniBMap att ph = (reorderedAtt, ph)
    where
        reorderedAtt = mergePair $ B.break (fst . flip isSSMatch firstIntSS) att
        mergePair (x, y) = y <> x
        firstIntSS = (head . fmap (toIntSubSpace lniBMap) . fingerprint) ph

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
    Just (_, MatchBar slcs _) -> isJust $ L.find (matchSlice phName) slcs

phCheckAll :: [(NodeName, BarKind)] -> (NodeName, PhenotypeName) -> Bool
phCheckAll bcPs (nName, phName) = case L.find ((==) nName . fst) bcPs of
    Nothing -> False
    Just (_, FullMiss _) -> False
    Just (_, MatchBar slcs _) -> case L.find (matchSlice phName) slcs of
        Nothing -> False
        Just (Match attSize attMatchess _) -> case phENMatch of
            Just (_, matchess, _) -> attSize == (sum . fmap length) matchess
            Nothing -> False
            where
                phENMatch = L.find (\x -> phName == (fstOf3 x)) attMatchess
        Just (Miss _) -> False        

matchSlice :: PhenotypeName
           -> Slice
           -> Bool
matchSlice _ (Miss _) = False
matchSlice phName (Match _ attMatchess checkedPhN)
    | phName == checkedPhN = True
    | otherwise = isJust $ L.find (\x -> phName == (fstOf3 x)) attMatchess

barPhenotype :: Bar -> Maybe PhenotypeName
barPhenotype br = case barKind br of
    FullMiss _ -> Nothing
    MatchBar slcs _ -> foldr bestSl Nothing slcs
        where
            bestSl sl bestS = case sl of
                Miss _ -> bestS
                Match attSize attMatchess phName -> case fstPhName == phName of
                    False -> bestS
                    True
                        | attSize == attMatchIntssSum -> Just phName
                        | otherwise -> bestS
                    where
                        fstPhName = (fstOf3 . NL.head) attMatchess
                        attMatchIntssSum =
                            (sum . fmap length . sndOf3 . NL.head) attMatchess


mkSlice :: AttractorSize
        -> M.HashMap PhenotypeName (PHEIndex, ErrorLength)
        -> PhenotypeName
        -> [(PhenotypeName, [ThreadSlice])]
        -> Slice
mkSlice attSize _ _ [] = Miss attSize
mkSlice attSize phErrMap phName phSlicess = Match attSize attMIndicies phName
    where
        attMIndicies = (NL.fromList . fmap attMIndexF) phSlicess
        attMIndexF (phN, phSlices) =
            (phN, range <$> phSlices, (phErrMap M.!? phName))

-- Mark where on a Thread the Phenotypes (or their PhenotypeErrors) of its
-- ModelMapping match.
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
        phSlices :: [(PhenotypeName, [ThreadSlice])]
        phSlices = (fmap . fmap . fmap) mkRange phInts
        phInts = concatMap (wholePhMatch lniBMap thread) phs

-- Find the places a Phenotype, or its associated PhenotypeErrors, is present in
-- a Thread. Note that a Phenotype or PhenotypeError MUST start at its first
-- state to match, we may not treat it as a generic loop, even for Phenotypes,
-- like circadian rhythms, that do cycle endlessly. 
wholePhMatch :: LayerNameIndexBimap
             -> Thread
             -> Phenotype
             -> [(PhenotypeName, [[Int]])]
wholePhMatch lniBMap thread ph = filter (not . L.null . snd) purgedMatches
  where
--     rangedMatches = (fmap . fmap . fmap) mkRange purgedMatches
    purgedMatches = B.toList purgedMatchVec
    purgedMatchVec = fst $ B.foldl' purgerF purgeAcc phMatchVec
--     Remove the PhentypeError matches that are just the starts of Phenotype
--     or longer PhenotypeError matches
    purgeAcc = (B.empty, [])
    purgerF (cleanMatches, fullerMatches) (phName, phEMatches) = newAcc
        where
            newAcc = ( B.snoc cleanMatches (phName, newCleanMatches)
                     , newCleanMatches <> fullerMatches)
            newCleanMatches = filter (purgeFilterF fullerMatches) phEMatches
            purgeFilterF fMatches phEMatch = not $
                any (L.isPrefixOf phEMatch) fMatches
    phMatchVec = fst $ B.ifoldr phMatch (rAcc, sAcc) thread
    sAcc = B.replicate (length allPhNames) (0, [])
    rAcc = (B.fromList . fmap (\x -> (x, []))) allPhNames
    allPhNames = (phenotypeName ph):phErrNames
    phErrNames = phErrorName <$> phErrs
    phMatch :: Int -> LayerVec
            -> (PHMResultVec, PHMStateVec)
            -> (PHMResultVec, PHMStateVec)
    phMatch threadIndex lVec accVecs = builtMatches
      where
        builtMatches = B.ifoldr phFoldF (fst accVecs, B.empty) (snd accVecs)
        phFoldF :: Int -> (PhLoopIndex, [Int])
                -> (PHMResultVec, PHMStateVec)
                -> (PHMResultVec, PHMStateVec)
        phFoldF pheIndex (phSSIndex, matchAcc) (rVec, sVec) = (newRVec, newSVec)
          where
            (newRVec, newSVec) = case isSSMatch lVec sSpace of
              (True, _) -> case phSSIndex == ssLoopMax of
                True -> (nRVec, nSVec)
                    where
                        nRVec = B.accum rUpdate rVec [(pheIndex, newMatchAcc)]
                        rUpdate (phN, matchIntss) newMatchInts =
                                    (phN, L.snoc matchIntss newMatchInts)
                        nSVec = B.cons (0, []) sVec
                False -> (rVec, B.cons (phSSIndex + 1, newMatchAcc) sVec)
              (False, Just True) -> (rVec, B.cons (0, []) sVec)
              (False, _) -> (rVec, B.cons (phSSIndex, matchAcc) sVec)
            newMatchAcc = L.snoc matchAcc threadIndex
            sSpace = (phIntSSVecVec B.! pheIndex) B.! phSSIndex
            ssLoopMax = phSSLoopMaxIVec B.! pheIndex
    phSSLoopMaxIVec = B.map (pred . B.length) phSSVecVec
    phIntSSVecVec = (B.map . B.map) (toIntSubSpace lniBMap) phSSVecVec
    phSSVecVec = (B.fromList . fmap B.fromList) ((fingerprint ph):phErrFPs)
    phErrFPs = phErrorFingerprint <$> phErrs
    phErrs = phenotypeErrors ph

-- Sometimes we want only the range of a Phenotype match. Partial; use only on
-- the output of wholePhMatch. 
mkRange :: [Int] -> ThreadSlice
mkRange matchIndices = (head matchIndices, last matchIndices)

toIntSubSpace :: LayerNameIndexBimap -> SubSpace -> IntSubSpace
toIntSubSpace lniBMap = BF.bimap toIntF (fmap toIntF)
    where toIntF = (fmap . BF.first) (lniBMap BM.!)

-- Results Vector of the places the Phenotype and its PhenotypeErrors occur in
-- the Thread. 
type PHMResultVec = B.Vector (PhenotypeName, [[Int]])
-- Vector whose index is the Phenotype and PhenotypeErrors. Content is the
-- SubSpace in the Phenotype loop we are currently looking for, and the
-- LayerVec locations of matches of earlier Phenotype loop SubSpaces
type PHMStateVec = B.Vector (PhLoopIndex, [Int])
type PhLoopIndex = Int


-- Do the states in the Int-converted constraint or (possiblly) blocking
-- Subspace match the equivalent states in the LayerVec?
isSSMatch :: LayerVec -> IntSubSpace -> (Bool, Maybe Bool)
isSSMatch lV (consSS, mBlockSS) = (isCons, mIsBlock)
    where
        mIsBlock = (all (isStateMatch lV)) <$> mBlockSS
        isCons = all (isStateMatch lV) consSS
        isStateMatch lVec (nodeNameInt, nState) = nState == lVec U.! nodeNameInt


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

