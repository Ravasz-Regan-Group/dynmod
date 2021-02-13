{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types.DMModel
import Types.GML
import Parse.DMMS
import Parse.GML
import Render
import Publish
import Utilities
import Visualize
import qualified ReadWrite as RW
import SuppMat
import Text.LaTeX.Base.Render (render)
import qualified Options.Applicative as O
import Path
import Path.IO
import Data.Validation (Validation(..))
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Text.Pretty.Simple as PS
import qualified Data.Graph.Inductive as Gr
import Data.Void
import Control.Monad (zipWithM_, when, unless)
import qualified Text.Megaparsec as M
import Control.Monad.Reader (runReader)
import Paths_dynmod (version)
import Data.Version (showVersion)

main :: IO ()
main = do
    clInput <- O.execParser opts
    mFilePath <- resolveFile' $ T.unpack $ fileN clInput
    (_, ext) <- splitExtension mFilePath
    fileclInput <- RW.readFile mFilePath
    let strFileName = toFilePath mFilePath
    case ext of
        ".dmms" ->
            let pOut = M.runParser modelFileParse strFileName fileclInput in
            workDMMS mFilePath clInput pOut
        _ -> fail $ "Not a dmms file: " <> ext
 
workDMMS :: Path Abs File
         -> Input
         -> Either
                (M.ParseErrorBundle T.Text Void)
                (FileFormatVersion, (DMModel, CitationDictionary))
         -> IO ()
workDMMS _ _ (Left err) = PS.pPrint (M.errorBundlePretty err)
workDMMS f options (Right parsed) = do
    let dmModel = (fst . snd) parsed
        citeDict = (snd . snd) parsed
        fileVersion = fst parsed
        layers = modelLayers dmModel
        tts = layerTTs <$> layers
        bNPs = mkBooleanNet dmModel
    putStrLn "Good parse"
    when ((gmlWrite options) && (updateGMLN options /= ""))
        (fail "-g will overwrite the existing gml file!")
    when (pubWarn options)
        (pubWWrite f (dmModel, citeDict))
    when (coordColors options)
        (graphDetailWrite f dmModel)
    when (suppPDF options)
        (writeSupp f (snd parsed))
    when (gmlWrite options)
        (writeGML f dmModel)
    when (updateGMLN options /= "") $ do
        let gFileText = updateGMLN options
        gFilePath <- resolveFile' $ T.unpack gFileText
        (_, ext) <- splitExtension gFilePath
        case ext of
            ".gml" -> do
                let strGMLFName = toFilePath gFilePath
                gmlFileContent <- RW.readFile gFilePath
                let gmlPOut = M.runParser gmlParse strGMLFName gmlFileContent
                updateDMMS f parsed gmlPOut
            _ -> fail $ "Not a gml file: " <> ext
    when (parseTest options) $ do
        pTest <- parseRelFile "test/parseTest.dmms"
        let modelRender = renderDMMS fileVersion dmModel citeDict
        RW.writeFile pTest modelRender
--         pTest <- parseRelFile "test/parseTest.hs"
--         RW.writeFile pTest $ LT.toStrict $ PS.pShowNoColor $ dmModel
    unless (noTTWrite options)
        (ttWrite f tts bNPs)


-- Write TT & BooleanNet files to disk, as extracted from a dmms file, in a
-- directory with the dmms' name. 
ttWrite :: Path Abs File -> ModelTTFiles -> [(ModelName, BooleanNet)] -> IO ()
ttWrite mFilePath fs bNetPs = do
    (pathNoExt, _) <- splitExtension mFilePath
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
    (fName, _) <- splitExtension $ filename file
    let dir = parent file
        fWarningStr = fNameStr ++ "_warnings"
        fNameStr = fromRelFile fName
    warningFileName <- parseRelFile fWarningStr
    let warningFile = dir </> warningFileName
    warningFileNameWExt <- addExtension ".txt" warningFile
    let warnings =  mkPublish x
        prettyWarnings = prettyPublish warnings
    putStrLn $ case warnings of
        Failure _ -> "See Metadata Warnings"
        Success _ -> "No Warnings"
    RW.writeFile warningFileNameWExt prettyWarnings

-- Write DMNode coordinates & colors to a file
graphDetailWrite :: Path Abs File -> DMModel -> IO ()
graphDetailWrite file dmM = do
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
 
writeSupp :: Path Abs File -> (DMModel, CitationDictionary) -> IO ()
writeSupp f (dmM, cd) = do
    let texText = render $ runReader sM_LaTeX dmM
    let bibText = mkBibFile cd
    (mFileNameNoExt, _) <- splitExtension $ filename f
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

writeGML :: Path Abs File -> DMModel -> IO ()
writeGML f dnM = do
    let gmlText = (renderGML . toGML) dnM
    (gFileNameNoExt, _) <- splitExtension f
    gFileName <- addExtension ".gml" gFileNameNoExt
    RW.writeFile gFileName gmlText

writeDMMS :: Path Abs File
          -> FileFormatVersion
          -> DMModel
          -> CitationDictionary
          -> IO ()
writeDMMS f ver dmm cDict = do
    let dmmsText = renderDMMS ver dmm cDict
    RW.writeFile f dmmsText

updateDMMS :: Path Abs File
           -> (FileFormatVersion, (DMModel, CitationDictionary))
           -> Either (M.ParseErrorBundle T.Text Void) GML
           -> IO ()
updateDMMS _ _ (Left err)= PS.pPrint (M.errorBundlePretty err)
updateDMMS f (dmmsVer, (dmm, cd)) (Right gml) = case gmlValidate gml of
    Failure errs -> fail $ "Invalid GML:\n" <>
            (LT.unpack . PS.pShowNoColor) errs
    Success vGML -> case updateDMModel dmm vGML of
        Failure updateErrs -> fail $ "Failed Update:\n" <>
            (LT.unpack . PS.pShowNoColor) updateErrs
        Success updatedDMM -> do
            putStrLn "Good update"
            let dirPath = parent f
                fNamePath = filename f
            (fNamePathNoExt, ext) <- splitExtension fNamePath
            let uFName = (toFilePath fNamePathNoExt) ++ "_GML_UPDATE"
            uFNamePathNoExt <- parseRelFile uFName
            uFNamePath <- addExtension ext uFNamePathNoExt
            let uFPath = dirPath </> uFNamePath
            writeDMMS uFPath dmmsVer updatedDMM cd
            

-- Option Parsing
data Input = Input
    { fileN       :: T.Text
    , pubWarn     :: Bool
    , coordColors :: Bool
    , suppPDF     :: Bool
    , gmlWrite    :: Bool
    , updateGMLN  :: T.Text
    , noTTWrite   :: Bool
    , parseTest   :: Bool
    } deriving (Eq, Show)

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
        ( O.long "coord_colors"
       <> O.short 'c'
       <> O.help "Whether to write node coordinates & colors to \
            \<FILE>_graph_details.txt")
    <*> O.switch
        ( O.long "supplementary"
       <> O.short 's'
       <> O.help "Whether to write a supplementary publication of the dmms file\
            \ to PDF. ")
    <*> O.switch
        ( O.long "gml"
       <> O.short 'g'
       <> O.help "Whether to write out a simple GML file of the dmms file"
        )
    <*> O.strOption
        ( O.long "update"
       <> O.short 'u'
       <> O.value ""
       <> O.metavar "SOURCE_FILE"
       <> O.help "Whether to update the parsed dmms file with color and \
            \coordinate information from a specified GML file. The networks \
            \in the two files must match. "
        )
    <*> O.switch
        ( O.long "no_ttwrite"
       <> O.short 't'
       <> O.help "Whether to refrain from writing out tt files. "
        )
    -- Whether to write a pShowNoColor of the DMMS we've parsed.
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

opts :: O.ParserInfo Input
opts = O.info (O.helper <*> versionOpt <*> input)
    ( O.fullDesc
   <> O.progDesc "Parse a dmms file and output a Truth Table directory. \
   \Options: write publication level warnings to a file, write node coordinates\
   \ and color to a file, write a supplementary publication of the rmms file to\
   \ PDF, update a dmms file from a gml file, refrain from writing out tt files\ 
   \."
   <> O.header "dynmod - processing dmms files")


