{-# LANGUAGE OverloadedStrings #-}

module Utilities where

import System.Random.Stateful
import qualified Data.Sequence as Seq
import qualified Data.List as L
import Data.Foldable (foldl', toList)
import Data.Bifunctor (Bifunctor(..), bimap)
-- import Control.Applicative (liftA2)
import qualified Data.Vector.Unboxed as UVec
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as M
import qualified Data.Hashable as Hash
import qualified Text.Pretty.Simple as PS
import Data.Validation
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)

infixl 4 <<$>>{-, <<*>>-}, <<<$>>>

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

-- (<<*>>) :: (Applicative f1, Applicative f2) =>
--            f1 (f2 (a -> b))
--         -> f1 (f2  a      )
--         -> f1 (f2       b )
-- (<<*>>) = liftA2 (<*>)

(<<<$>>>) :: (Functor f1, Functor f2, Functor f3) => 
             (a -> b)
          -> f1 (f2 (f3 a))
          -> f1 (f2 (f3 b))
(<<<$>>>) = fmap . fmap . fmap

-- General purpose functions

-- Strict version of pShowNoColor
sPShowNoColor :: Show a => a -> T.Text
sPShowNoColor = toStrict . PS.pShowNoColor

-- Are the elements in a list strictly increasing?
isStrictlyIncreasing :: (Ord a) => [a] -> Bool
isStrictlyIncreasing [] = True
isStrictlyIncreasing [_] = True
isStrictlyIncreasing (x:y:ys) = x < y && isStrictlyIncreasing (y:ys)

-- Are all the Ints in each list strictly larger than the list before, as well
-- as strictly increasing internally?
areStrictlyIncreasing :: [[Int]] -> Bool
areStrictlyIncreasing [] = True
areStrictlyIncreasing [x] = isStrictlyIncreasing x
areStrictlyIncreasing (x:y:ys) = case (x, y) of
    ([], []) -> areStrictlyIncreasing ys
    (t, []) -> isStrictlyIncreasing t && areStrictlyIncreasing (t:ys)
    ([], u) -> isStrictlyIncreasing u && areStrictlyIncreasing (u:ys)
    (t, u) -> isStrictlyIncreasing t && isStrictlyIncreasing u &&
              maximum t < minimum u && areStrictlyIncreasing (u:ys)

-- Are the elements in the List increasing one-by-one, according to their Ord
-- instance?
-- isStepIncreasing :: (Ord a, Enum a) => [a] -> Bool
-- isStepIncreasing [] = True
-- isStepIncreasing [_] = True
-- isStepIncreasing (x:y:xs) = succ x == y && isStepIncreasing (y:xs)

-- Is List xs a subset of List ys? Not efficient. Do not use for n > 10000. 
isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset [] _     = True
isSubset (_:_) [] = False
isSubset (x:xs) ys = elem x ys && isSubset xs (L.delete x ys)

-- Are 2 Lists cyclical permutations of each other?
areCyclicPermutes :: (Eq a) => [a] -> [a] -> Bool
areCyclicPermutes xs ys = (length xs == length ys) && (cPerm xs ys)
    where cPerm bs cs = L.isInfixOf bs $ cs <> cs

-- Are 2 Lists the same up to permutations?
arePermutes :: (Eq a) => [a] ->[a] -> Bool
arePermutes xs ys = (isSubset xs ys) && (isSubset ys xs)

-- Are all the elements in a list identical?
allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame (x:xs) = all (== x) xs

-- Combine Validation Failures monoidally in the error. 
errorRollup :: [Validation a b] -> [a]
errorRollup = foldr ePop []
                where ePop (Failure e) es = e:es
                      ePop (Success _) es = es


-- In a list of n HashSets, this finds any element in any set that occurs in
-- more than one set. 
nIntersection :: Hash.Hashable a => [Set.HashSet a] -> Set.HashSet a
nIntersection = snd . go 
    where go = foldl' rollingI (Set.empty, Set.empty)

rollingI :: Hash.Hashable a => 
            (Set.HashSet a, Set.HashSet a)
         ->  Set.HashSet a
         -> (Set.HashSet a, Set.HashSet a)
rollingI (sUnion, sDupes) s =
    (sUnion `Set.union` s, (sUnion `Set.intersection` s) `Set.union` sDupes)

isSuccess :: Validation a b -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False

isFailure :: Validation a b -> Bool
isFailure (Failure _) = True
isFailure (Success _) = False

numTimes :: (Eq a) => a -> [a] -> Int
numTimes x = length . (filter (== x))

fstOf3 :: (a, b, c) -> a
fstOf3 (l, _, _) = l

sndOf3 :: (a, b, c) -> b
sndOf3 (_, m, _) = m

thdOf3 :: (a, b, c) -> c
thdOf3 (_, _, n) = n

fstOf4 :: (a, b, c, d) -> a
fstOf4 (l, _, _, _) = l

sndOf4 :: (a, b, c, d) -> b
sndOf4 (_, m, _, _) = m

thdOf4 :: (a, b, c, d) -> c
thdOf4 (_, _, n, _) = n

fthOf4 :: (a, b, c, d) -> d
fthOf4 (_, _, _, o) = o

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

-- Convert an uncurried function to a curried function.
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

-- Convert a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

concatPair :: Monoid a => (a, a) -> a
concatPair (x, y) = x <> y

differenceWithKey :: Hash.Hashable k => (k -> v -> w -> Maybe v)
                                        -> M.HashMap k v
                                        -> M.HashMap k w
                                        -> M.HashMap k v
differenceWithKey f a b = M.foldlWithKey' go M.empty a
  where
    go m k v = case M.lookup k b of
                 Nothing -> M.insert k v m
                 Just w  -> maybe m (\y -> M.insert k y m) (f k v w)

-- Sort the second list by the order of elements in the first list. 
sortWithOrder :: (Ord a, Hash.Hashable a) => [a] -> [a] -> [a]
sortWithOrder = sortWithOrderOn id

sortWithOrderOn :: (Ord b, Hash.Hashable b)
                => (a -> b) -> [b] -> [a] -> [a]
sortWithOrderOn f order = L.sortOn (getOrder . f)
    where getOrder k = M.lookupDefault (-1) k $ mkOrderHashMap order

mkOrderHashMap :: (Ord a, Hash.Hashable a) => [a] -> M.HashMap a Int
mkOrderHashMap xs = M.fromList (zip xs ([1..] :: [Int]))

-- The symmetric difference of two lists, ie the bit outside the intersection.
-- Also know as the Sheffer stroke, or NANDing two lists. 
--     .───────..-─--───.     
--   *'*******,'`********`.   
--  *********╱    ╲******* ╲  
-- ;********;      :********* 
-- :*********      ;********; 
--  ╲********╲    *********╱  
--   *********╲  ╱********╱   
--    `******* `. *******'    
--      `──---'  `─────'      
(\|\) :: Ord a => [a] -> [a] -> [a]
(\|\) xs ys = (xs L.\\ ys) `L.union` (ys L.\\ xs)

lrUniques :: Ord a => [a] -> [a] -> ([a], [a])
lrUniques xs ys = (xs L.\\ ys, ys L.\\ xs)

pairSame :: Eq a => (a, a) -> Bool
pairSame (x, y) = x == y

isoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
isoBimap f = bimap f f

fillDown :: Int -> [Int]
fillDown n = [0..n]

seqToVec :: UVec.Unbox a => Seq.Seq a -> UVec.Vector a
seqToVec = UVec.fromList . toList

-- The following should go away when we move to GHC 9.2.1. Done: Jan 25, 2023
-- initStdGen :: IO StdGen
-- initStdGen = StdGen <$> SM.initSMGen

genGen :: Int -> StdGen -> ([StdGen], StdGen)
genGen i gen = go 0 [] gen
    where 
        go k gs g
            | k >= i = (gs, g)
            | otherwise = let (newG, seed) = split g
                          in go (k + 1) (newG:gs) seed        

genPair :: StdGen -> a -> (StdGen, (StdGen, a))
genPair gen x = let (newGen, seed) = split gen in (newGen, (seed, x))

quadUncurry :: (a -> b -> c -> d -> e) -> ((a, b), (c, d)) -> e
quadUncurry f ((a, b), (c, d)) = f a b c d

doubleUncurry :: (a -> b -> c -> d -> e) -> (a, b) -> (c, d) -> e
doubleUncurry f (a, b) (c, d) = f a b c d

type Probability = Double

mkProb :: Double -> Maybe Probability
mkProb p
    | (0 <= p) && (p <= 1) = Just p
    | otherwise = Nothing

-- I want to know if an input in an InputCoord is set to an integer rather than
-- a real, so as to avoid trying to stochastically set a value that can only
-- ever have one result. The problem is that checking if a Float is "actually"
-- an Int is fraught with rounding errors and other floating point trouble. 
-- The following, from:
-- https://stackoverflow.com/questions/1164003/how-do-i-test-if-a-floating-point
--                                              -number-is-an-integer-in-haskell
-- is good enough for my purposes. 
-- True if x is an int to n decimal places
-- isInt :: (Integral a, RealFrac b) => a -> b -> Bool
-- isInt n x =
--     (round $ 10^(fromIntegral n) * (x - (fromIntegral $ round x))) == 0
-- Non-polymorphic version (which is all I need)
isInt :: Int -> Double -> Bool
isInt n x = ((round $ 10^n * (x - (fromInteger $ round x))) :: Int) == 0

-- repeated and allUniqiue are lifted wholesale from the Unique package, which
-- is not as actively maintained as it was, and which I don't want to keep in my
-- extra-deps. 
-- https://hackage.haskell.org/package/Unique

-- Find only the elements that are present more than once in a list.
repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

-- Find only the elements that are present more than n times in a list.
repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = map head . filterByLength p 

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . L.group . L.sort

allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . L.group . L.sort

-- Very naively, make some guess as to how wide given T.Text would be printed, 
-- by counting capital letters. Take a lower case m as the base. 
-- textWidth :: T.Text -> Double
-- textWidth t = T.length t + (capitalBonus * capitalCount)
--     where
--         capitalCount = (T.length . T.filter isUpper) t
--         capitalBonus = 140/120
