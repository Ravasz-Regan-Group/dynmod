{-# LANGUAGE OverloadedStrings #-}

module Parse.UI (
      eParamParse
    , gParamParse  
    , pinsParse
    ) where

import Types.DMModel
import Utilities
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import Control.Monad.Combinators.Expr   -- from parser-combinators
-- import Data.Validation
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- hsc :: Parser ()
-- hsc = L.space hspace1 lineCmnt blockCmnt
--   where
--     lineCmnt  = L.skipLineComment "//"
--     blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- hlexeme :: Parser a -> Parser a
-- hlexeme = L.lexeme hsc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc
-- 
-- hsymbol :: T.Text -> Parser T.Text
-- hsymbol = L.symbol hsc

real :: Parser Double
real = lexeme L.float

integer :: Parser Int
integer = lexeme L.decimal

colon :: Parser T.Text
colon = symbol ":"

-- Parse Attractor search parameters. 
eParamParse :: Parser (Int, Int, Probability)
eParamParse = (,,) <$> integer <*> integer <*> probParse

-- Parse Attractor grid-search parameters. 
gParamParse :: Parser ((Int, Int, Probability), Int)
gParamParse = (,) <$> ((,,) <$> integer <*> integer <*> probParse) <*> integer

-- Parse a probability
probParse :: Parser Probability
probParse = do
    num <- real
    case mkProb num of
        Just p -> return p
        Nothing -> fail $ show num ++ " is not between 0 and 1, and so \
            \cannot be a probability"

-- Parse pinned environmental inputs
pinsParse :: [[(NodeName, Int)]] -> Parser [(NodeName, Int)]
pinsParse inputs = some (pinParse >>= check)
    where
        pinParse :: Parser (NodeName, Int)
        pinParse = do
            nNameM <- T.pack <$> ((:) <$> letterChar <*>
                (many (alphaNumChar <|> char '_')))
            void colon
            ste <- integer
            return (nNameM, ste)
        check x | x `elem` nNames = return x
                | otherwise = fail $ "Pinned Node(s) must be from listed inputs"
        nNames = concat inputs

