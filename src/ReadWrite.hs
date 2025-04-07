{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Read and write UTF-8 text files.
module ReadWrite
  (
  -- * Reading
    readFile
  , readFileL
  , tryReadFile
  , ReadError (..)
  -- * Writing
  , writeFile
  , writeFileL
  , tryWriteFile
  , WriteError
  -- * Re-exports
  , IOError
  , UnicodeException (DecodeError)
  , parseAbsFile
  , parseRelFile
  ) where

-- base
import Data.Either     (Either (..))
import Data.Functor    ((<$>))
import System.IO       (IO)
import System.IO.Error (IOError)

-- safe-exceptions
import qualified Control.Exception.Safe as Exception

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- text
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Lazy.Encoding  as TEL
import Data.Text.Encoding.Error (UnicodeException (..))

-- path
import           Path (Path, parseAbsFile, parseRelFile)
import qualified Path

data ReadError
  = ReadErrorIO IOError
  | ReadErrorDecode UnicodeException

type WriteError = IOError

-- | Read the contents of a UTF-8 encoded text file.
--
-- May throw 'IOError' or 'UnicodeException'. To handle these errors in 'Either'
-- instead, use 'tryReadFile'.
readFile :: Path base Path.File -> IO T.Text
readFile path =
  f <$> BS.readFile (Path.toFilePath path)
  where
    f bs = let !text = TE.decodeUtf8 bs in text

-- | Read the contents of a UTF-8 encoded text file.
--
-- Any 'IOError' or 'UnicodeException' that occurs is caught and returned as a
-- 'ReadError' on the 'Left' side of the 'Either'. To throw these exceptions
-- instead, use 'readFile'.
tryReadFile :: Path base Path.File -> IO (Either ReadError T.Text)
tryReadFile path =
  f <$> Exception.tryIO (BS.readFile (Path.toFilePath path))
  where
    f (Left e) = Left (ReadErrorIO e)
    f (Right bs) = first ReadErrorDecode (TE.decodeUtf8' bs)

-- | Write text to a file in a UTF-8 encoding.
--
-- May throw 'IOError'. To handle this error in 'Either' instead, use
-- 'tryWriteFile'.
writeFile :: Path base Path.File -> T.Text -> IO ()
writeFile path text =
  BS.writeFile (Path.toFilePath path) (TE.encodeUtf8 text)

-- | Write text to a file in a UTF-8 encoding.
--
-- Any 'IOError' that occurs is caught and returned on the 'Left' side of the
-- 'Either'. To throw the exception instead, use 'writeFile'.
tryWriteFile :: Path base Path.File -> T.Text -> IO (Either WriteError ())
tryWriteFile path text =
  Exception.tryIO (writeFile path text)

first :: (a -> a') -> Either a b -> Either a' b
first f (Left x) = Left (f x)
first _ (Right x) = Right x

-- | Lazy read and write.
readFileL :: Path base Path.File -> IO TL.Text
readFileL path = TEL.decodeUtf8 <$> BSL.readFile (Path.toFilePath path)

writeFileL :: Path base Path.File -> TL.Text -> IO ()
writeFileL path text =
    BSL.writeFile (Path.toFilePath path) (TEL.encodeUtf8 text)

