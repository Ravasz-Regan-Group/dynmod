{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parse.GML
    ( gmlParse
    )
    where

import Types.GML
import qualified Data.Text as T
-- import Control.Applicative.Permutations -- from parser-combinators
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific
import Data.Void

type Parser = Parsec Void T.Text


sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- | 'brackets' parses something between brackets.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | 'quotes' parses something between double quotes.
quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

-- | 'ampSemi' parses something between a '&' and a ';'.
-- ampSemi :: Parser a -> Parser a
-- ampSemi = between (symbol "&") (symbol ";")

-- | 'number' parses an number in scientific format.

number :: Parser Scientific
number = lexeme L.scientific

signedNumber :: Parser Scientific
signedNumber = L.signed sc number

gmlParse :: Parser GML
gmlParse = sc >> (some $ (,) <$> keyParse <*> gValueParse)

keyParse :: Parser GKey
keyParse = lexeme $ T.pack <$> ((:) <$> letterChar <*> (many alphaNumChar))

gValueParse :: Parser GValue
gValueParse =  try $ gNumVParse
           <|> GStr <$> stringParse
           <|> GList <$> brackets gmlParse

gNumVParse :: Parser GValue
gNumVParse = do
    x <- signedNumber
    case (floatingOrInteger x :: Either Double Int) of
        (Left r)  -> return $ GReal r
        (Right i) -> return $ GInt i
    

-- Parse the particular way GML defines strings:
stringParse :: Parser T.Text
stringParse = quotes $ T.pack <$> (many (anySingleBut '\"'))
