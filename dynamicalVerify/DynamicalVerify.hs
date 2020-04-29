{-# LANGUAGE OverloadedStrings #-}

module DynamicalVerify where

import Parsing
import Types
import qualified ReadWrite as RW
import qualified Options.Applicative as O
import Path
import Path.IO
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as T
import qualified Text.Pretty.Simple as PS
import qualified Data.Void
import Control.Monad (zipWithM, zipWithM_)
import qualified Text.Megaparsec as M
import Data.Either (fromRight)


dynamicalVerify :: IO ()
dynamicalVerify = do
    input <- O.execParser opts
    let strFileName = T.unpack $ fileN input
    mPath <- resolveFile' strFileName 
    (mFileName, ext) <- splitExtension (filename mPath)
    mDir <- parseRelDir (fromRelFile mFileName)
    fileInput <- RW.readFile mPath
    case ext == ".rrms" of
        False -> fail $ "Not an rrms file: " <> ext
        True  -> do
            case M.runParser (modelFileParse) strFileName fileInput of
                Left err      -> PS.pPrint (M.errorBundlePretty err)
                Right parsed  -> do
                    let dmModel = (fst . snd) parsed
                        layers = modelLayers dmModel
                        tts = layerTTs <$> layers
                    ttWrite mDir tts
 

-- Write TT files to disk, as extracted from an rrms file to disk, in a
-- directory with the rrms' name. 
ttWrite :: Path Rel Dir -> ModelTTFiles -> IO ()
ttWrite mDir fs = do
    let files = snd <<$>> (snd <$> fs)
        fileNames = (T.unpack . fst) <<$>> (snd <$> fs)
    fileNameRels <- mapM (mapM parseRelFile) fileNames
    fileNamesWExt <- mapM (mapM (addExtension ".csv")) fileNameRels
    layerDirs <- mapM parseRelDir ((T.unpack . fst) <$> fs)
    let dirs = (<$>) (mDir </>) layerDirs
    mapM_ ensureDir dirs
    let paths = zipWith (<$>) ((</>) <$> dirs) fileNamesWExt
    zipWithM_ (zipWithM_ RW.writeFile) paths files

data Input = Input
    { fileN :: T.Text
    }

opts = O.info (input O.<**> O.helper)
    ( O.fullDesc
   <> O.progDesc "Parse an rrms file and output a TT diectory"
   <> O.header   "dynamicalverify - processing rrms files")

input :: O.Parser Input
input = Input
    <$> O.strArgument
        (O.metavar "FILE"
        <> O.help "Path to an rrms file to parse")
