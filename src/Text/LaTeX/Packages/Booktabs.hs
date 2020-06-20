{-# LANGUAGE OverloadedStrings #-}

-- | https://ctan.org/pkg/booktabs
-- The package enhances the quality of tables in LATEX, providing extra commands
--  as well as behind-the-scenes optimisation. Guidelines are given as to what 
-- constitutes a good table in this context. From version 1.61, the package 
-- offers longtable compatibility.

module Text.LaTeX.Packages.Booktabs
  ( booktabs
  , CTrim(..)
  , CTLeft(..)
  , CTRight(..)
  , toprule
  , midrule
  , bottomrule
  , cmidrule
  , addLineSpace
  , defaultAddSpace
--  , morecmidrules
--  , specialrule
--  , arrayrulecolor
  ) 
 where
 
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
 
 
 -- | Booktabs package. Use it to import it like this:
--
-- > usepackage [] booktabs
booktabs :: PackageName
booktabs = "booktabs"

data CTrim   = CTrim CTLeft CTRight deriving (Show, Eq)

data CTLeft  = CTLEmpty
             | CTLLetter
             | CTLMeasure Measure
             deriving (Show, Eq)

data CTRight = CTREmpty
             | CTRLetter
             | CTRMeasure Measure
             deriving (Show, Eq)

toprule :: LaTeXC l => Maybe Measure -> l
toprule Nothing  = fromLaTeX $ TeXCommS "toprule"
toprule (Just m) = fromLaTeX $ TeXComm "toprule"
    [OptArg $ TeXRaw $ render m]

midrule :: LaTeXC l => Maybe Measure -> l
midrule Nothing  = fromLaTeX $ TeXCommS "midrule"
midrule (Just m) = fromLaTeX $ TeXComm "midrule"
    [OptArg $ TeXRaw $ render m]

bottomrule :: LaTeXC l => Maybe Measure -> l
bottomrule Nothing  = fromLaTeX $ TeXCommS "bottomrule"
bottomrule (Just m) = fromLaTeX $ TeXComm "bottomrule"
    [OptArg $ TeXRaw $ render m]

cmidrule :: LaTeXC l => Maybe Measure -> CTrim -> Int -> Int -> l
cmidrule mM tr i j = fromLaTeX $ TeXComm "cmidrule" com
  where 
    com = case (mM, tr) of
      (Nothing, CTrim CTLEmpty CTREmpty) -> [iJ]
      (Nothing, tr')                     -> [trim tr', iJ]
      (Just mW, CTrim CTLEmpty CTREmpty) -> [OptArg $ TeXRaw $ render mW, iJ]
      (Just mW, tr')  -> [OptArg $ TeXRaw $ render mW, trim tr', iJ]
    iJ = FixArg $ TeXRaw $ render i <> "-" <> render j
    trim (CTrim ltr rtr) = ParArg $ (ltrim ltr) <> (rtrim rtr)
    ltrim CTLEmpty  = TeXEmpty
    ltrim CTLLetter = TeXRaw "l"
    ltrim (CTLMeasure mL) = (TeXRaw "l") <> (TeXRaw $ render mL)
    rtrim CTREmpty  = TeXEmpty
    rtrim CTRLetter = TeXRaw "r"
    rtrim (CTRMeasure mR) = (TeXRaw "r") <> (TeXRaw $ render mR)

addLineSpace :: LaTeXC l => Maybe Measure -> l
addLineSpace Nothing = commS "addlinespace"
addLineSpace (Just m) = fromLaTeX $ TeXComm "addlinespace" [OptArg $ texy m]

defaultAddSpace :: LaTeXC l => l
defaultAddSpace = commS "defaultaddspace"
