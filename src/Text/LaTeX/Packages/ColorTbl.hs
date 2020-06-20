{-# LANGUAGE OverloadedStrings #-}

-- | https://ctan.org/pkg/booktabs
-- The package enhances the quality of tables in LATEX, providing extra commands
--  as well as behind-the-scenes optimisation. Guidelines are given as to what 
-- constitutes a good table in this context. From version 1.61, the package 
-- offers longtable compatibility.

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