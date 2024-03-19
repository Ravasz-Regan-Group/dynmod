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
    , phenotypeMatch
    , mkBarcode
    , ColorMap
    , mkColorMap
    , gradientPick
    , attMatch
    , bcFilterF
    , barPhenotype
    , StdDev
    , ThreadSlice
    ) where    

import Types.DMModel
import Types.Simulation
import Utilities
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.List.Extra as L
import Data.Maybe (fromJust, mapMaybe)
import Data.Ix (range)
import qualified Data.Bifunctor as BF

-- Basic types to support all types of figures.

type SVGText = T.Text
type ColorMap = M.HashMap NodeName LocalColor
type PUCGradient = B.Vector LocalColor
type StdDev = Double

-- slices of a timeline, of the form (a, b) | b >= a >= 0
type ThreadSlice = (Int, Int)

mkColorMap :: DMModel -> ColorMap
mkColorMap dmm = M.fromList nameColorPairs
    where
        nameColorPairs = (\n -> (nodeName n, nodeColor n)) <$> nodesMetas
        nodesMetas = nodeMeta <$> ((concat . modelNodes) dmm)


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
        nonEmptyPhs = filter ((/= []) . snd . snd) mM


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
        matchTHSlicess = phMatch lniBMap att <$> orderedPHs 
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
        phSlices = zip phNames $ phMatch lniBMap thread <$> phs
        phNames = phenotypeName <$> phs

-- Find the places a Phenotypes is present in the given Thread.
phMatch :: LayerNameIndexBimap -> Thread -> Phenotype -> [ThreadSlice]
phMatch lniBMap thread ph
  | fPrintSize > thSize = []
  | otherwise = case phMatchReorder intPh thread of
    Nothing -> []
    Just ordIntPh
      | not $ areStrictlyIncreasing preppedMatches -> []
      | any null matches -> []
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
            | otherwise = phMatch lniBMap dpLThread ph
            where dpLThread = B.drop trimmedLMatchesSize thread
          preppedTLMs = fst <$> trimmedLMatches
          trimmedLMatchesSize = lmIndexF trimmedLMatches
          trimmedLMatches = (init matches) `L.snoc` trimmedLLast
          trimmedLLast :: ([Int], IntSubSpace)
          trimmedLLast = (BF.first ((:[]) . head) . last) matches
          extraSlices
            | B.null dpThread = []
            | otherwise = phMatch lniBMap dpThread ph
            where dpThread = B.drop (lmIndexF matches) thread
          lmIndexF :: [([Int], IntSubSpace)] -> Int
          lmIndexF = ((+1) . last . fst . last) 
          preppedMatches = fst <$> matches
          matches = mapMaybe (matchLocation thread) ordIntPh
          mkRange zs = ((minimum . head) zs, (maximum . last) zs)
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
phMatchReorder intPh thread = case attOffset of
    Nothing -> Nothing
    Just i -> Just (frontSS <> backSS)
        where
            (backSS, frontSS) = L.break (isSSMatch (thread B.! i)) intPh
    where
        attOffset :: Maybe Int
        attOffset = B.findIndex (isAttMatch intPh) thread
        isAttMatch iph attLVec = (any (isSSMatch attLVec) iph)

-- Do the states in the Int-converted Subspace match the equivalent states in
-- the LayerVec?
isSSMatch :: LayerVec -> IntSubSpace -> Bool
isSSMatch lV sS = all (isStateMatch lV) sS
    where
        isStateMatch lVec (nodeNameInt, nState) = nState == lVec U.! nodeNameInt

-- Find the location of the first places in the Thread, if any, that the Int-
-- converted SubSpace matches. Repeats are permitted at this step, so we return
-- a (possibly empty) list of succesive Ints. 
matchLocation :: Thread -> IntSubSpace -> Maybe ([Int], IntSubSpace)
matchLocation thread sS = (,) <$> (unfolderF <$> firstMatchI) <*> pure sS
    where
        unfolderF i = i : L.unfoldr unF (i + 1)
        firstMatchI = B.findIndex (flip isSSMatch sS) thread
        unF j
            | j >= B.length thread = Nothing 
            | isSSMatch (thread B.! j) sS = Just (j, j + 1)
            | otherwise = Nothing

