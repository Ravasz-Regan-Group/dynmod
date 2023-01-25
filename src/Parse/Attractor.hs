{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parse.Attractor
    ( attractorFileParse
    , AttractorBundle
    ) where

import Utilities
import Types.Simulation
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Bimap as BM
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.List as L


-- Parse out the DMMSModelMapping, LayerNameIndexBimap, and HS.HashSet Attractor
-- (an AttractorBundle) from an attractor CSV file. 
attractorFileParse :: T.Text -> AttractorBundle
attractorFileParse csv = (mm, lniBMap, atts)
    where
        mm = ((F.toList <<$>>) . F.toList) $ L.foldl' mmFolder S.Empty mmPairs
        lniBMap = BM.fromList $ zip (snd <$> mmPairs) [0..]
        mmPairs = ((\[x, y] -> (x, y)) . take 2) <$> rows
        atts = HS.fromList $ (mkAttractor . B.fromList) <$> attVecs
        attVecs :: [[U.Vector Int]]
        attVecs = U.fromList <<$>> (((L.transpose <$>) . L.transpose) attInts)
        attInts :: [[[Int]]]
        attInts = (((read . T.unpack) <$>) . T.split (== ',')) <<$>> attTexts
        attTexts :: [[T.Text]]
        attTexts = (T.splitOn ",," . mconcat . drop 3) <$> rows
        rows = T.split (== ',') <$> attLines
        attLines = take ((length ls) - 2) ls
        ls = T.lines csv

-- Fold up a list of the form:
-- [(Clock_Env, Light)
--  (""       , Temp),
--  (""       , Cdk1),
--  (""       , Cdk1_H),
--  (Env_Sensitive, PER1_mRNA),
--  (""           , PER1_cyto),
--  (""           , PER1_cytoHI),
--  (""           , PER1_nuc),
--  (""           , CKIe_nuc),
--  (""           , CKIe_cyto),
--  (""           , CKId_nuc),
--  (""           , CKId_cyto),
--  (Circadian_Clock, CLOCK),
--  (""             , BMAL1),
--  (""             , BC),
--  (""             , CRY1),
--  (""             , CRY2),
--  (""             , PER2_mRNA),
--  (""             , PER3_mRNA),
--  (""             , PER2_cyto),
--  (""             , PER3_cyto),
--  (""             , PER2_cytoHI),
--  (""             , PER3_cytoHI),
--  (""             , PER2_nuc),
--  (""             , PER3_nuc),
--  (""             , PC),
--  (""             , PC_nuc),
--  (""             , REV_ERBa),
--  (""             , REV_ERBb),
--  (""             , RORa),
--  (""             , RORb),
--  (""             , RORg)
-- ]
-- Into the form:
-- [(Clock_Env, [Light, Temp, Cdk1, Cdk1_H]),
--  (Env_Sensitive, [PER1_mRNA
--                 , PER1_cyto
--                 , PER1_cytoHI
--                 , PER1_nuc
--                 , CKIe_nuc
--                 , CKIe_cyto
--                 , CKId_nuc
--                 , CKId_cyto]
--  ),
--  (Circadian_Clock, [CLOCK
--                   , BMAL1
--                   , BC
--                   , CRY1
--                   , CRY2
--                   , PER2_mRNA
--                   , PER3_mRNA
--                   , PER2_cyto
--                   , PER3_cyto
--                   , PER2_cytoHI
--                   , PER3_cytoHI
--                   , PER2_nuc
--                   , PER3_nuc
--                   , PC
--                   , PC_nuc
--                   , REV_ERBa
--                   , REV_ERBb
--                   , RORa
--                   , RORb
--                   , RORg]
--  )
-- ]
-- Using Sequences as intermediary structures
mmFolder :: S.Seq (T.Text, S.Seq T.Text)
         -> (T.Text, T.Text)
         -> S.Seq (T.Text, S.Seq T.Text)
mmFolder S.Empty (s, nn) = S.Empty S.:|> (s, S.Empty S.:|> nn)
mmFolder (ss S.:|> (s, nns)) ("", nn) = ss S.:|> (s, nns S.:|> nn)
mmFolder ss (s, nn) = ss S.:|> (s, S.Empty S.:|> nn)