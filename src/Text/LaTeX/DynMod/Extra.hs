{-# LANGUAGE OverloadedStrings #-}


module Text.LaTeX.DynMod.Extra where
 
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax hiding ((<>))
import qualified Data.Text as T

-- | usepackage [] subfloat
subfloatp :: PackageName
subfloatp = "subfloat"

-- Start a subtable environment. First argument textwidth. 
subtables :: LaTeXC l => l -> l
subtables = env0 "subtables"

-- | usepackage [] makecell
makecellp :: PackageName
makecellp = "makecell"

makecell :: LaTeXC l => l -> l
makecell = comm1 "makecell"

-- | usepackage [] newpxtext
newpxtextp :: PackageName
newpxtextp = "newpxtext"

-- | usepackage [] newpxmath
newpxmathp :: PackageName
newpxmathp = "newpxmath"

-- | usepackage [] amsopn
amsopnp :: PackageName
amsopnp = "amsopn"

-- | usepackage [] breqn
breqnp :: PackageName
breqnp = "breqn"

-- | usepackage [] flexisym
flexisymp :: PackageName
flexisymp = "flexisym"

-- | usepackage [] microtype
microtypep :: PackageName
microtypep = "microtype"

-- | usepackage [] xcolor
pxcolor :: PackageName
pxcolor = "xcolor"

-- | usepackage [] calc
calcp :: PackageName
calcp = "calc"

-- | usepackage [] calc
titlesecp :: PackageName
titlesecp = "titlesec"

-- Denote an expression that calculates a dimension of some sort
dimexpr :: LaTeXC l => l
dimexpr = commS "dimexpr"

-- Return the current width of the page sans empty braces ({})
textwidth' :: LaTeXC l => l
textwidth' = commS "textwidth"

-- \let\foo\bar defines \foo to have the value that \bar had at the point of
-- definition.
-- https://tex.stackexchange.com/questions/258
lLet :: LaTeXC l => l
lLet = commS "let"

lLeft :: LaTeXC l => l
lLeft = commS "left"
lRight :: LaTeXC l => l
lRight = commS "right"

-- \relax is a single token, an unexpandable TeX primitive that does nothing
-- when executed. 
-- https://tex.stackexchange.com/questions/86385
relax :: LaTeXC l => l
relax = commS "relax"

-- | Globally set the counter `counter` to have the value of the `value`
-- argument, which must be an integer. Thus, you can set a counter’s value as
-- \setcounter{section}{5}. Note that the counter name does not start with a
-- backslash.

setCounter :: LaTeXC l => T.Text -> Int -> l
setCounter cntr i = comm2 "setcounter" (fromLaTeX $ TeXRaw cntr) (rendertex i)

-- Creates a new command. 
newCommand :: LaTeXC l => T.Text -> Int -> l -> l -> l
newCommand nm numOpts optDef def
    | numOpts <= 0 = comm2 "newcommand" teXNm def
    | otherwise = optFixComm "newcommand" 2
        [rendertex numOpts, optDef, teXNm, def]
        where
            teXNm = (commS . T.unpack) nm

-- Starred version. 
newCommand' :: LaTeXC l => T.Text -> Int -> l -> l -> l
newCommand' nm numOpts optDef def
    | numOpts <= 0 = comm2 "newcommand*" teXNm def
    | otherwise = optFixComm "newcommand*" 2
        [rendertex numOpts, optDef, teXNm, def]
        where
            teXNm = (commS . T.unpack) nm

-- Locally override a command. 
renewCommand :: LaTeXC l => T.Text -> Int -> l -> l -> l
renewCommand nm numOpts optDef def
    | numOpts <= 0 = comm2 "renewcommand" teXNm def
    | otherwise = optFixComm "renewcommand" 2
        [rendertex numOpts, optDef, teXNm, def]
        where
            teXNm = (commS . T.unpack) nm

-- Starred version. 
renewCommand' :: LaTeXC l => T.Text -> Int -> l -> l -> l
renewCommand' nm numOpts optDef def
    | numOpts <= 0 = comm2 "renewcommand*" teXNm def
    | otherwise = optFixComm "renewcommand*" 2
        [rendertex numOpts, optDef, teXNm, def]
        where
            teXNm = (commS . T.unpack) nm

-- We need a few symbols from the fdsymbol package, so we set up to import them
-- here. 
declareFontFamily :: LaTeXC l => l -> l -> l -> l
declareFontFamily = comm3 "DeclareFontFamily"
declareFontShape :: LaTeXC l => l -> l -> l -> l -> l -> l -> l
declareFontShape = comm6 "DeclareFontShape"
declareSymbolFont :: LaTeXC l => l -> l -> l -> l -> l -> l
declareSymbolFont = comm5 "DeclareSymbolFont"
declareMathSymbol :: LaTeXC l => l -> l -> l -> l -> l
declareMathSymbol = comm4 "DeclareMathSymbol"

-- Id there is ever a need for operators likt "lim", which take arguments. 
declareMathOperator :: LaTeXC l => String -> l
declareMathOperator xs = comm2 "DeclareMathOperator" (commS (xs ++ "op"))
                            (fromLaTeX $ TeXRaw $ T.pack xs)

-- Define a binary operator
mathbin :: LaTeXC l => l -> l
mathbin = comm1 "mathbin"

-- Define an unary operator
mathop :: LaTeXC l => l -> l
mathop = comm1 "mathop"
