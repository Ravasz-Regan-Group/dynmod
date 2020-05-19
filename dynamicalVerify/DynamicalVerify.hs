{-# LANGUAGE OverloadedStrings #-}

module DynamicalVerify where

import Parsing
import Types
import Publish
import qualified ReadWrite as RW
import qualified Options.Applicative as O
import Path
import Path.IO
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Text.Pretty.Simple as PS
import qualified Data.Void
import Control.Monad (zipWithM, zipWithM_, when)
import qualified Text.Megaparsec as M
import Data.Either (fromRight)


dynamicalVerify :: IO ()
dynamicalVerify = do
    input <- O.execParser opts
    mFilePath <- resolveFile' $ T.unpack $ fileN input
    (mFilePathNoExt, ext) <- splitExtension mFilePath
    fileInput <- RW.readFile mFilePath
    let strFileName = toFilePath mFilePath
    case ext == ".rrms" of
        False -> fail $ "Not an rrms file: " <> ext
        True  -> do
            case M.runParser (modelFileParse) strFileName fileInput of
                Left err      -> PS.pPrint (M.errorBundlePretty err)
                Right parsed  -> do
                    let dmModel = (fst . snd) parsed
                        citeDict = (snd . snd) parsed
                        layers = modelLayers dmModel
                        tts = layerTTs <$> layers
                    parseTest <- parseRelFile "test/parseTest.hs"
                    putStrLn "Good parse"
                    RW.writeFile parseTest $ LT.toStrict
                        $ PS.pShowNoColor $ dmModel
                    ttWrite mFilePath tts
                    when (pubWarn input)
                        (pubWWrite mFilePath (dmModel, citeDict))
 

-- Write TT files to disk, as extracted from an rrms file, in a
-- directory with the rrms' name. 
ttWrite :: Path Abs File -> ModelTTFiles -> IO ()
ttWrite mFilePath fs = do
    (pathNoExt, ext) <- splitExtension mFilePath
    let files = snd <<$>> (snd <$> fs)
        fileNames = (T.unpack . fst) <<$>> (snd <$> fs)
    topDir <- parseAbsDir $ fromAbsFile pathNoExt
    fileNameRels <- mapM (mapM parseRelFile) fileNames
    fileNamesWExt <- mapM (mapM (addExtension ".csv")) fileNameRels
    layerDirs <- mapM parseRelDir ((T.unpack . fst) <$> fs)
    let dirs = (topDir </>) <$> layerDirs
    mapM_ ensureDir dirs
    let paths = zipWith (<$>) ((</>) <$> dirs) fileNamesWExt
    zipWithM_ (zipWithM_ RW.writeFile) paths files

-- Write publication warnings to disk
pubWWrite :: Path Abs File -> (DMModel, CitationDictionary) -> IO ()
pubWWrite file x = do
    (fName, ext) <- splitExtension $ filename file
    let dir = parent file
        fWarningStr = fNameStr ++ "_warnings"
        fNameStr = fromRelFile fName
    warningFileName <- parseRelFile fWarningStr
    let warningFile = dir </> warningFileName
    warningFileNameWExt <- addExtension ".txt" warningFile
    let warnings = (prettyPublish . mkPublish) x
    RW.writeFile warningFileNameWExt warnings

data Input = Input
    { fileN       :: T.Text
    , pubWarn :: Bool
    }

input :: O.Parser Input
input = Input
    <$> O.strArgument
        (O.metavar "FILE"
        <> O.help "Path to an rrms file to parse")
    <*> O.switch
        ( O.long "warnings"
       <> O.short 'w'
       <> O.help "Whether to write publication warnings to <FILE>_warnings.txt")

opts = O.info (input O.<**> O.helper)
    ( O.fullDesc
   <> O.progDesc "Parse an rrms file and output a TT diectory. Optionally write\
   \ publication level warnings to a file"
   <> O.header   "dynamicalverify - processing rrms files")


