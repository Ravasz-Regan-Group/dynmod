{-# LANGUAGE OverloadedStrings #-}

-- | https://ctan.org/pkg/colortbl
-- The package allows rows and columns to be coloured, and even individual
-- cells.

module Text.LaTeX.Packages.ColorTbl
--  ( toprule
--  , midrule
--  , bottomrule
--  , cmidrule
--  , addlinespace
--  , morecmidrules
--  , specialrule
--  ) 
 where
 
import Text.LaTeX.Base.Types
 
 
 -- | Colortable package. Use it to import it like this:
--
-- > usepackage [] colortable
colortable :: PackageName
colortable = "colortable"