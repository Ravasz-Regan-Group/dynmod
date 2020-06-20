{-# LANGUAGE OverloadedStrings #-}

module Utilities where

import Data.Validation
import qualified Data.HashSet as Set
import qualified Data.Hashable as Hash
import qualified Text.Pretty.Simple as PS
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.List as L
import Data.Foldable (foldl')
import Control.Applicative (liftA2)

infixl 4 <<$>>, <<*>>, <<<$>>>

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<*>>) :: (Applicative f1, Applicative f2) =>
           f1 (f2 (a -> b))
        -> f1 (f2  a      )
        -> f1 (f2       b )
(<<*>>) = liftA2 (<*>)

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
isStrictlyIncreasing (x:y:xs) = x < y && isStrictlyIncreasing (y:xs)

-- Is List xs a subset of List ys? Not efficient. Do not use for n > 10000. 
isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset [] _     = True
isSubset (_:_) [] = False
isSubset (x:xs) ys = elem x ys && isSubset xs (L.delete x ys)

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
                where ePop v es = case isFailure v of
                                  True  -> ((\(Failure e) -> e) v) : es
                                  False -> es

-- Delete all the items in xs list from ys. 
deleteMult :: (Eq a) => [a] -> [a] -> [a]
deleteMult _ [] = []
deleteMult [] ys = ys
deleteMult (x:xs) ys = deleteMult xs $ L.delete x ys

-- In a list of n HashSets, this finds any element in any set that occurs in
-- more than one set. 
nIntersection :: (Eq a, Hash.Hashable a) => [Set.HashSet a] -> Set.HashSet a
nIntersection = snd . go 
    where go = foldl' rollingI (Set.empty, Set.empty)

rollingI :: (Eq a, Hash.Hashable a) => 
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