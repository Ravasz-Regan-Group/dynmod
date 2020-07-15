{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parse.DMMS
import Types
import Publish
import Utilities
import qualified ReadWrite as RW
import SuppMat
import Text.LaTeX.Base.Render (render)
import qualified Options.Applicative as O
import Path
import Path.IO
import Data.Validation (Validation(..))
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Text.Pretty.Simple as PS
import qualified Data.Graph.Inductive as Gr
import qualified Data.Void
import Control.Monad (zipWithM, zipWithM_, when)
import qualified Text.Megaparsec as M
import Data.Either (fromRight)
import Control.Monad.Reader (runReader)
import Paths_dynmod (version)
import Data.Version (showVersion)


main :: IO ()
main = do
    input <- O.execParser opts
    mFilePath <- resolveFile' $ T.unpack $ fileN input
    (mFilePathNoExt, ext) <- splitExtension mFilePath
    fileInput <- RW.readFile mFilePath
    let strFileName = toFilePath mFilePath
    case ext == ".dmms" of
        False -> fail $ "Not a dmms file: " <> ext
        True  -> do
            case M.runParser (modelFileParse) strFileName fileInput of
                Left err      -> PS.pPrint (M.errorBundlePretty err)
                Right parsed  -> do
                    let dmModel = (fst . snd) parsed
                        citeDict = (snd . snd) parsed
                        layers = modelLayers dmModel
                        tts = layerTTs <$> layers
                        bNPs = mkBooleanNet dmModel
                    putStrLn "Good parse"
                    ttWrite mFilePath tts bNPs
                    when (pubWarn input)
                        (pubWWrite mFilePath (dmModel, citeDict))
                    when (graphDetail input)
                        (graphDetailWrite mFilePath dmModel)
                    when (suppPDF input)
                        (renderSupp mFilePath (snd parsed))
                    when (parseTest input) $ do
                        parseTest <- parseRelFile "test/parseTest.hs"
                        RW.writeFile parseTest $ LT.toStrict
                            $ PS.pShowNoColor $ dmModel
 

-- Write TT & BooleanNet files to disk, as extracted from a dmms file, in a
-- directory with the dmms' name. 
ttWrite :: Path Abs File -> ModelTTFiles -> [(ModelName, BooleanNet)] -> IO ()
ttWrite mFilePath fs bNetPs = do
    (pathNoExt, ext) <- splitExtension mFilePath
    let ttFiles = snd <<$>> (snd <$> fs)
        ttFileNames = (T.unpack . fst) <<$>> (snd <$> fs)
        bNFiles = snd <$> bNetPs
        bNFileNames = (T.unpack . fst) <$> bNetPs
    topDir <- parseAbsDir $ fromAbsFile pathNoExt
    ttFileNameRels <- mapM (mapM parseRelFile) ttFileNames
    ttFileNamesWExt <- mapM (mapM (addExtension ".csv")) ttFileNameRels
    bNFileNameRels <- mapM parseRelFile bNFileNames
    bNFileNamesWExt <- mapM (addExtension ".booleannet") bNFileNameRels
    layerDirs <- mapM parseRelDir ((T.unpack . fst) <$> fs)
    let ttDirs = (topDir </>) <$> layerDirs
    mapM_ ensureDir ttDirs
    let ttPaths = zipWith (<$>) ((</>) <$> ttDirs) ttFileNamesWExt
        bNPaths = (topDir </>) <$> bNFileNamesWExt
    zipWithM_ (zipWithM_ RW.writeFile) ttPaths ttFiles
    zipWithM_ RW.writeFile bNPaths bNFiles

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
    let warnings =  mkPublish x
        prettyWarnings = prettyPublish warnings
        areWarnings x = case x of
            Failure _ -> False
            Success _ -> True
    putStrLn $ case warnings of
        Failure _ -> "See Metadata Warnings"
        Success _ -> "No Warnings"
    RW.writeFile warningFileNameWExt prettyWarnings

-- Write DMNode coordinates & colors to a file
graphDetailWrite :: Path Abs File -> DMModel -> IO ()
graphDetailWrite file dmM = do
    (fName, ext) <- splitExtension $ filename file
    let dir = parent file
        layers = modelLayers dmM
        gDetail n = (nodeName md, nodeColor md, nodeCoordinate md)
            where md = nodeMeta n
        nodes = ((snd <$>) . Gr.labNodes . modelGraph) <$> layers
        details = gDetail <<$>> nodes
        textDetails = sPShowNoColor <$> details
        fileNameStrs = 
            (T.unpack . (flip T.append) "_gDetails" . modelName . modelMeta)
            <$> layers
    fileNames <- mapM ((addExtension ".txt" =<<) . parseRelFile) fileNameStrs
    let paths = (dir </>) <$> fileNames
    zipWithM_ RW.writeFile paths textDetails
 
renderSupp :: Path Abs File -> (DMModel, CitationDictionary) -> IO ()
renderSupp f (dmM, cd) = do
    let texText = render $ runReader sM_LaTeX dmM
    let bibText = mkBibFile cd
    (mFileNameNoExt, ext) <- splitExtension $ filename f
    let mDirPath = parent f
        dirFileStr = (toFilePath mFileNameNoExt) ++ "_SM"
    dir <- parseRelDir dirFileStr 
    let fPath :: Path Abs Dir; fPath = mDirPath </> dir
    ensureDir fPath
    tFile <- parseRelFile dirFileStr
    bStem <- parseRelFile "references"
    let file = fPath </> tFile
        bFile = fPath </> bStem
    texFileWExt <- addExtension ".tex" file
    bibFileWExt <- addExtension ".bib" bFile
    RW.writeFile texFileWExt texText
    RW.writeFile bibFileWExt bibText        

-- Option Parsing
data Input = Input
    { fileN       :: T.Text
    , pubWarn     :: Bool
    , graphDetail :: Bool
    , suppPDF     :: Bool
    , parseTest   :: Bool
    }

input :: O.Parser Input
input = Input
    <$> O.strArgument
        (O.metavar "FILE"
        <> O.help "Path to a dmms file to parse")
    <*> O.switch
        ( O.long "warnings"
       <> O.short 'w'
       <> O.help "Whether to write publication warnings to <FILE>_warnings.txt")
    <*> O.switch
        ( O.long "graph_details"
       <> O.short 'g'
       <> O.help "Whether to write node coordinates & colors to \
            \<FILE>_graph_details.txt")
    <*> O.switch
        ( O.long "supplementary"
       <> O.short 's'
       <> O.help "Whether to write a supplementary publication of the rmms file\
            \ to PDF. ")
    -- Whether to write a pShowNoColor of the DMModel we've parsed.
    <*> O.switch
        ( O.long "parsetest"
       <> O.short 'p'
       <> O.help ""
       <> O.hidden
       <> O.internal)

versionOpt :: O.Parser (a -> a)
versionOpt = O.infoOption
                (showVersion version)
                (O.long "version" <> O.help "Show version")

opts = O.info (O.helper <*> versionOpt <*> input)
    ( O.fullDesc
   <> O.progDesc "Parse a dmms file and output a Truth Table directory. \
   \Options: write publication level warnings to a file, write node coordinates\
   \ and color to a file, write a supplementary publication of the rmms file to\
   \ PDF. "
   <> O.header "dynmod - processing dmms files")


