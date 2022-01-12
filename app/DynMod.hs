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
import Compare
import Paths_dynmod (version)
import Text.LaTeX.Base.Render (render)
import qualified Options.Applicative as O
import Path
import Path.IO
import Data.Validation (Validation(..))
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Text.Pretty.Simple as PS
import qualified Data.Graph.Inductive as Gr
import qualified Text.Megaparsec as M
import Data.Void
import Control.Applicative ((<|>))
import Control.Monad (zipWithM_, when)
import Control.Monad.Reader (runReader)
import GHC.Conc (par, pseq)
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
            case activity clInput of 
                Update upd -> updateDMMS mFilePath fileclInput upd
                Compare compareText -> do
                    cFilePath <- resolveFile' $ T.unpack $ compareText
                    (_, cExt) <- splitExtension cFilePath
                    compFileContent <- RW.readFile cFilePath
                    let compFileName = toFilePath cFilePath
                        lComp = M.runParser modelFileParse
                                            strFileName
                                            fileclInput
                        rComp = M.runParser modelFileParse
                                            compFileName
                                            compFileContent
                    case cExt of 
                        ".dmms" ->
                            case lComp `par` (rComp `pseq` (lComp, rComp)) of
                                (Left err1, Left err2) -> do
                                    PS.pPrint (M.errorBundlePretty err1)
                                    PS.pPrint (M.errorBundlePretty err2)
                                (Left err, _) ->
                                    PS.pPrint (M.errorBundlePretty err)
                                (_, Left err) ->
                                    PS.pPrint (M.errorBundlePretty err)
                                (Right leftP, Right rightP) -> 
                                    dmmsCompare mFilePath
                                                cFilePath ((fst . snd) leftP)
                                                          ((fst . snd) rightP)
                        _ ->
                            fail $ "Not a dmms file: " <> toFilePath cFilePath
                Procedure ps -> let
                    pOut = M.runParser modelFileParse strFileName fileclInput in
                    workDMMS mFilePath ps pOut
        _ -> fail $ "Not a dmms file: " <> toFilePath mFilePath

updateDMMS :: Path Abs File
           -> T.Text
           -> Updates
           -> IO ()
updateDMMS f dmmsContent u = updateDMMS' u where
  updateDMMS' (GML gFileText) = do
    gFilePath <- resolveFile' $ T.unpack gFileText
    (_, gExt) <- splitExtension gFilePath
    let strFileName = toFilePath f
        dirPath = parent f
        fNamePath = filename f
    (fNamePathNoExt, ext) <- splitExtension fNamePath
    case gExt of
      ".gml" -> do
        let strGMLFName = toFilePath gFilePath
        gmlFileContent <- RW.readFile gFilePath
        case M.runParser gmlParse strGMLFName gmlFileContent of
          (Left gErr) -> PS.pPrint (M.errorBundlePretty gErr)
          (Right gml) -> case gmlValidate gml of
            Failure errs -> fail $ "Invalid GML:\n" <>
              (LT.unpack . PS.pShowNoColor) errs
            Success vGML -> case M.runParser modelFileParse strFileName
                                                            dmmsContent of
              (Left dErr) -> PS.pPrint (M.errorBundlePretty dErr)
              (Right (dmmsVer, (dmm, cd))) -> do
                putStrLn "Good parse"
                case updateDMModel dmm vGML of
                  Failure updateErrs -> fail $ "Failed Update:\n" <>
                    (LT.unpack . PS.pShowNoColor) updateErrs
                  Success gUpdatedDMM -> do
                    let gUFName = (toFilePath fNamePathNoExt) ++ "_GML_UPDATE"
                    gUFNamePathNoExt <- parseRelFile gUFName
                    gUFNamePath <- addExtension ext gUFNamePathNoExt
                    let gUFPath = dirPath </> gUFNamePath
                    writeDMMS gUFPath dmmsVer gUpdatedDMM cd
      _ -> fail $ "Not a gml file: " <> ext
  updateDMMS' (PT pb) = do
    let strFileName = toFilePath f
        dirPath = parent f
        fNamePath = filename f
    (fNamePathNoExt, ext) <- splitExtension fNamePath
    case M.runParser modelFileParse strFileName dmmsContent of
      (Left dErr) -> PS.pPrint (M.errorBundlePretty dErr)
      (Right (dmmsVer, (dmm, cd))) -> do
        when pb $ do
          let tPurgedDMMSText = purgeTableRenderDMMS dmmsVer dmm cd
              tPFName = (toFilePath fNamePathNoExt) ++ "_Table_Purge_UPDATE"
          tPFNamePathNoExt <- parseRelFile tPFName
          tPFNamePath <- addExtension ext tPFNamePathNoExt
          let tPFPath = dirPath </> tPFNamePath
          RW.writeFile tPFPath tPurgedDMMSText

workDMMS :: Path Abs File
         -> Procedures
         -> Either
                (M.ParseErrorBundle T.Text Void)
                (FileFormatVersion, (DMModel, CitationDictionary))
         -> IO ()
workDMMS _ _ (Left err) = PS.pPrint (M.errorBundlePretty err)
workDMMS f options (Right parsed) = do
    let dmModel = (fst . snd) parsed
        citeDict = (snd . snd) parsed
        fileVersion = fst parsed
    putStrLn "Good parse"
    when (pubWarn options)
        (pubWWrite f (dmModel, citeDict))
    when (coordColors options)
        (graphDetailWrite f dmModel)
    when (suppPDF options)
        (writeSupp f (snd parsed))
    when (gmlWrite options)
        (writeGML f dmModel)
    when (parseTest options) $ do
        pTest <- parseRelFile "test/parseTest.dmms"
        let modelRender = renderDMMS fileVersion dmModel citeDict
        RW.writeFile pTest modelRender
--         pTest <- parseRelFile "test/parseTest.hs"
--         RW.writeFile pTest $ LT.toStrict $ PS.pShowNoColor $ dmModel
    when (ttWrite options)
        (ttFWrite f (layerTTs <$> (modelLayers dmModel)) (mkBooleanNet dmModel))


-- Write TT & BooleanNet files to disk, as extracted from a dmms file, in a
-- directory with the dmms' name. 
ttFWrite :: Path Abs File -> ModelTTFiles -> [(ModelName, BooleanNet)] -> IO ()
ttFWrite mFilePath fs bNetPs = do
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


-- Compare two DMMS files
dmmsCompare :: Path Abs File
            -> Path Abs File
            -> DMModel
            -> DMModel
            -> IO ()
dmmsCompare lF rF lDMM rDMM = do
    putStrLn "Good parses"
    (lPathNoExt, _) <- splitExtension lF
    (rPathNoExt, _) <- splitExtension rF
-- Write the compare text to the directory of the right-hand file, since by
-- convention this is "new" model being compared to the "old" left-hand file.
    let compDir = parent rF
        dmDiffValidation = dmMCompare lDMM rDMM
        lDMMSName = T.pack $ fromRelFile $ filename lPathNoExt
        rDMMSName = T.pack $ fromRelFile $ filename rPathNoExt
        diffName = T.unpack $ lDMMSName <> "__" <> rDMMSName <> "_diff"
    diffRelName <- parseRelFile diffName
    diffPath <- addExtension ".txt" (compDir </> diffRelName)
    case dmDiffValidation of
        Failure compErr -> fail $ show compErr
        Success dmDiff -> writeCompare diffPath lDMMSName rDMMSName dmDiff

-- Write a DMModelDiff to disk
writeCompare :: Path Abs File -> ModelName -> ModelName -> DMModelDiff -> IO()
writeCompare f lDMMSName rDMMSName diff = do
    let diffText = renderDMMSDiff lDMMSName rDMMSName diff
    RW.writeFile f diffText

-- Option Parsing
data Input = Input {
      fileN    :: T.Text
    , activity :: Activity
    } deriving (Eq, Show)
    
data Activity = Update Updates
              | Compare CompareDMMS
              | Procedure Procedures
              deriving (Eq, Show)

data Updates = GML UpdateGMLN
             | PT PurgeTables
             deriving (Eq, Show)
type UpdateGMLN = T.Text
type PurgeTables = Bool

type CompareDMMS = T.Text

data Procedures = Procedures {
      pubWarn     :: Bool
    , coordColors :: Bool
    , suppPDF     :: Bool
    , gmlWrite    :: Bool
    , ttWrite     :: Bool
    , parseTest   :: Bool
    } deriving (Eq, Show)

input :: O.Parser Input
input = Input <$> O.strArgument
                  (O.metavar "FILE"
                  <> O.help "Path to a dmms file to parse")
              <*> activityParser

activityParser :: O.Parser Activity
activityParser = procedureParser <|> updateParser <|> compareParser

updateParser :: O.Parser Activity
updateParser = Update <$> (gmlUpdateParser <|> tablePurgeParser)

gmlUpdateParser :: O.Parser Updates
gmlUpdateParser = GML
    <$> O.strOption
        ( O.long "update"
       <> O.short 'u'
       <> O.metavar "GML_FILE"
       <> O.help "Whether to update the parsed dmms file with color and \
            \coordinate information from a specified GML file. The networks \
            \in the two files must match. The update is written to \
            \<FILE>_GML_UPDATE.dmms. "
        )

tablePurgeParser :: O.Parser Updates
tablePurgeParser = PT
    <$> O.switch
        ( O.long "purge_tables"
       <> O.short 'x'
       <> O.help "Whether to edit the given DMMS file to remove TruthTables \
            \from NodeGates where both DiscreteLogic expressions and \
            \TruthTables exist. "
        )

compareParser :: O.Parser Activity
compareParser = Compare
    <$> O.strOption
        ( O.long "compare"
       <> O.short 'c'
       <> O.metavar "DMMS_FILE"
       <> O.help "Whether to compare one DMMS file with another and write out \
            \the comparison to <File>__<DMMS_FILE>_diff.txt>. "
        )
    

procedureParser :: O.Parser Activity
procedureParser = Procedure <$> (Procedures
    <$> O.switch
        ( O.long "warnings"
       <> O.short 'w'
       <> O.help "Whether to write publication warnings to <FILE>_warnings.txt")
    <*> O.switch
        ( O.long "coord_colors"
       <> O.short 'e'
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
    <*> O.switch
        ( O.long "ttwrite"
       <> O.short 't'
       <> O.help "Whether to write out tt files. "
        )
    -- Whether to write a pShowNoColor of the DMMS we've parsed.
    <*> O.switch
        ( O.long "parsetest"
       <> O.short 'p'
       <> O.help ""
       <> O.hidden
       <> O.internal)
       )

versionOpt :: O.Parser (a -> a)
versionOpt = O.infoOption
                (showVersion version)
                (O.long "version" <> O.help "Show version")

opts :: O.ParserInfo Input
opts = O.info (O.helper <*> versionOpt <*> input)
    ( O.fullDesc
   <> O.progDesc "Parse a dmms file and perform various actions with it. \
   \Options: write publication level warnings to a file, write node coordinates\
   \ and color to a file, write a supplementary publication of the dmms file to\
   \ PDF, update a dmms file from a gml file, compare one dmms file with \
   \another, write out tt files."
   <> O.header "dynmod - processing dmms files")
