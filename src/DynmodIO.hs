{-# LANGUAGE OverloadedStrings #-}


module DynmodIO
    ( experimentDMMS
    , updateDMMS
    , procedureDMMS
    , dmmsCompare
    , figureDMMS
    ) where

import Input
import Types
import Figures
import SuppMat
import Compare
import Render
import Parse
import Visualize
import Utilities
import Properties.Attractors
import Text.LaTeX.Base.Render (render)
import Publish
import qualified ReadWrite as RW
import qualified Data.List.Split as Split
import Path
import Path.IO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import TextShow
import Diagrams.Prelude (mkWidth, Diagram, vsep, frame, hsep)
import Diagrams.Backend.Cairo (B, renderCairo)
import qualified Data.Graph.Inductive as Gr
import qualified Text.Pretty.Simple as PS
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec as M
import qualified Data.Bimap as BM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import Data.Validation (Validation(..))
import System.Random (initStdGen, StdGen, uniform)
import Data.Void
import Control.Monad (void, zipWithM_, when)
import Control.Monad.Reader (runReader)
import Control.Monad.State.Strict (evalState)
import qualified Data.List as L
import qualified Data.Bifunctor as BF
import Data.Maybe (fromJust, fromMaybe)
import Data.Traversable (mapAccumM)

experimentDMMS :: Path Abs File
               -> Experiments
               -> Either
                    (M.ParseErrorBundle T.Text Void)
                    (FileFormatVersion, (DMModel, CitationDictionary))
               -> IO ()
experimentDMMS _ _ (Left err) = PS.pPrint (M.errorBundlePretty err)
experimentDMMS f experiment (Right parsed) = do
    let dmModel = (fst . snd) parsed
        fLayer = fineLayer dmModel
    putStrLn "Good parse"
    mMap <- twoLayerModelMapping $ modelMappings dmModel
    let (dmmsMMap, _) = modelMappingSplit mMap
    case experiment of
        AttractorFind (EParams rN nN nProb) ->
            void $ attFindDMMS f dmmsMMap mEnv
            where mEnv = (fLayer, SamplingParameters rN nN nProb [])
        GridSearch ((EParams rN nN nProb), mult) ->
            gridDMMS f dmmsMMap mult mEnv
            where mEnv = (fLayer, SamplingParameters rN nN nProb [])
        VEX vexText -> do
            vexFilePath <- resolveFile' $ T.unpack $ vexText
            (_, vexFileExt) <- splitExtension vexFilePath
            vexFileContent <- RW.readFile vexFilePath
            let vexFileNameStr = toFilePath vexFilePath
            case vexFileExt of
                ".vex" ->
                    runVEX f vexFilePath dmModel pOut
                    where pOut = M.runParser experimentFileParse
                                             vexFileNameStr
                                             vexFileContent
                _ -> fail $ "Not a vex file: " <> vexFileNameStr
            

attFindDMMS :: Path Abs File
            -> DMMSModelMapping
            -> ModelEnv
            -> IO (HS.HashSet Attractor)
attFindDMMS f mMap (fLayer, sParams) = do
    gen <- initStdGen
    let LayerSpecs lniBMap _ _ _ = layerPrep fLayer
        ins = (inputs . modelGraph) fLayer
        inComsSizeS = (show . length) $ inputCombos ins [] lniBMap
    putStrLn "Starting run. Input nodes:"
    PS.pPrint $ (nodeName . nodeMeta) <<$>> ins
    putStrLn $ "In " ++ inComsSizeS ++ " combinations"
    putStr "With settings: "
    PS.pPrint (randomN sParams, noisyP sParams, noisyN sParams)
    when ((not . null . limitedInputs) sParams) $ do
        putStr "With limited inputs: "
        PS.pPrint $ limitedInputs sParams
    let atts = evalState (attractors (fLayer, sParams)) gen
        attPs = (randomN sParams, noisyN sParams, noisyP sParams)
    writeAttractorBundle f lniBMap mMap attPs Nothing atts
    pure atts

gridDMMS :: Path Abs File -> DMMSModelMapping -> Int -> ModelEnv -> IO ()
gridDMMS f mMap mult mEnv@(fLayer, sParams) = do
    gen <- initStdGen
    let LayerSpecs lniBMap _ _ _ = layerPrep fLayer
        attGrid = evalState (attractorGrid mEnv mult) gen
        doublesGrid = (fromIntegral . HS.size) <<$>> attGrid
        hMapDia = attractorHeatMapDia doublesGrid mult
        allAtts = mconcat $ mconcat <$> attGrid
    (fName, _) <- splitExtension $ filename f
    let fNameString = fromRelFile fName
        hMapFileName = fNameString ++
                          "_att_HM_" ++
                          (show (randomN sParams)) ++ "_" ++
                          (show (noisyN sParams)) ++ "_" ++
                          (show mult) ++ "_" ++
                          (show $ ((round . (1000 *)) (noisyP sParams) :: Int))
    hMapFileNameRel <- parseRelFile hMapFileName
    hMapFileNameRelWExt <- addExtension ".pdf" hMapFileNameRel
    let heatMapFPath = toFilePath hMapFileNameRelWExt
    renderCairo heatMapFPath (mkWidth 1600) hMapDia
    let (rN, nP, nN) = (randomN sParams, noisyN sParams, noisyP sParams)
    writeAttractorBundle f lniBMap mMap (rN, nP, nN) (Just mult) allAtts

-- Write out an AttractorBundle to disk. 
writeAttractorBundle :: Path Abs File
                     -> LayerNameIndexBimap
                     -> DMMSModelMapping
                     -> (Int, Int, Probability)
                     -> Maybe Int
                     -> (HS.HashSet Attractor)
                     -> IO ()
writeAttractorBundle f lniBMap mapping (rN, nN, nP) mMult atts = do
    (fName, _) <- splitExtension $ filename f
    let attVecList = HS.toList atts
        attNumber = length attVecList
        reorderedAttList =
            case traverse (attractorMMReorder mapping lniBMap)
                          attVecList
            of  (Left err) -> fail $ "Attractor reorder failed: " <> (show err)
                (Right r)  -> r
        attLLists = U.toList <<$>> (B.toList <$> reorderedAttList)
        attSizes = L.length <$> attLLists
        flatAttTanspLists = L.transpose (mconcat attLLists)
        switchRows = mappingFormat mapping
        attFile = mkAttTable attSizes switchRows flatAttTanspLists <> "\n\n" <>
            showt attNumber
        fNameString = fromRelFile fName
        attFileNameStem = fNameString ++ "_attractors"
        attFileName = case mMult of 
            Nothing -> attFileNameStem ++ "_a" ++ samplePs
            Just m -> attFileNameStem ++ "_gr" ++ samplePs ++ "_" ++ show m
        samplePs = L.intercalate "_" $ [show rN, show nN, show nP]
    attFileNameRel <- parseRelFile attFileName
    attFileNameRelWExt <- (addExtension ".csv") attFileNameRel
    RW.writeFile attFileNameRelWExt attFile

-- It is already a parser error if a switch has a switch name but no member
-- nodes, so the list in the tuple with each switch name is guaranteed to be
-- non-empty. 
mappingFormat :: DMMSModelMapping -> [T.Text]
mappingFormat ms = concatMap switchFormat ms
    where
        switchFormat (_, []) = error "EmptySwitch"
        switchFormat (sName, (n:ns)) = headR:tailRs
            where
                headR = sName <> ", " <> n <> ", "
                tailRs = (\x -> ", " <> x <> ", ") <$> ns

mkAttTable :: [Int] -> [T.Text] -> [[Int]] -> T.Text
mkAttTable sizes stRs atts = T.intercalate "\n" $ mkAttRow <$> (zip stRs atts)
    where
        mkAttRow (r, intR) = r <> "," <> (spacedRow sizes intR)
        spacedRow ss r = T.intercalate ",," $ T.intercalate "," <$>
            (showt <<$>> Split.splitPlaces ss r)

runVEX :: Path Abs File
       -> Path Abs File
       -> DMModel
       -> Either (M.ParseErrorBundle T.Text Void) VEXInvestigation
       -> IO ()
runVEX _ _ _ (Left err) = PS.pPrint (M.errorBundlePretty err)
runVEX dmmsPath vexPath dmModel (Right (dmmsFilePStr, vexLayerExpSpecs)) = do
    vexDMMSFILEPath <- resolveFile' dmmsFilePStr
    case dmmsPath == vexDMMSFILEPath of
        False -> fail errMessage
            where
                errMessage = "The parsed DMMS file: " <> toFilePath dmmsPath <>
                    "\n  and the DMMSFile from " <>
                    toFilePath (filename vexPath) <> ": " <>
                    toFilePath vexDMMSFILEPath <> "\n  do not match. "
        True -> case mkDMInvestigation dmModel vexLayerExpSpecs of
            Failure errs -> do
                mapM_ putStrLn ((T.unpack . vexErrorPrep) <$> errs)
            Success lExpSpecs -> do
                putStrLn "Good vex validation"
                attSets <-
                    mapM (mkInvestigationAtts dmmsPath dmModel) vexLayerExpSpecs
                putStrLn "Good attractors"
                let cMap = mkColorMap dmModel
                gen <- initStdGen
                putStrLn "Running experiments..."
                layerResultIOs <- runInvestigationIO dmmsPath cMap gen $
                        zip attSets lExpSpecs
--                 let invRs =
--                         runInvestigation cMap gen $ zip attSets lExpSpecs
                let numExp = sum $ (length . experiments) <$> lExpSpecs
                    expTense
                        | numExp == 1 = " experiment. "
                        | otherwise = " experiments. "
--                 PS.pPrint (tmlnLF <$> invRs)
                putStrLn $ "Ran " <> show numExp <> expTense
                let allMarks = fmap fst $
                        concatMap layerExperimentHooksIO layerResultIOs
                putStrLn $ "Experiment marks: " <> show allMarks


-- A simple write to disk of the Diagram Bs for now. 
simpleSCWrite :: Path Abs Dir -> (T.Text, Diagram B) -> IO ()
simpleSCWrite f (fName, dia) = do
    relFileName <- parseRelFile (T.unpack fName)
    relFileNameWExt <- addExtension ".pdf" relFileName
    let absFileNameWExt = (f </> relFileNameWExt) :: Path Abs File
        fPath = toFilePath absFileNameWExt
    ensureDir f
    renderCairo fPath (mkWidth 1600) dia

writeExpBCFig :: Path Abs Dir -> T.Text -> (Barcode, [Diagram B]) -> IO ()
writeExpBCFig dirFull expDetails (bc, expDias) = do
    ensureDir dirFull
    case expDias of
        [] -> do 
            putStrLn "No Timecourse figures for Barcode: "
            PS.pPrint bc
        [expDia] -> do
            let bcPatterns = (mconcat $ barFNPattern <$> bc) :: T.Text
                rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
            relFileName <- parseRelFile rFString
            relFileNameWExt <- addExtension ".pdf" relFileName
            let absFileNameWExt = dirFull </> relFileNameWExt
                fPath = toFilePath absFileNameWExt
            renderCairo fPath (mkWidth 1600) expDia
        expDs -> do
            let bcPatterns = (mconcat $ barFNPattern <$> bc) :: T.Text
                rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
                intBStrs = (("_" <>) . show) <$> [1..L.length expDs]
                rFStrings = (rFString <>) <$> intBStrs
                fStrDiaPairs = zip rFStrings expDs
            mapM_ (writeAttAlteredExpBCFig dirFull) fStrDiaPairs

writeAttAlteredExpBCFig :: Path Abs Dir -> (String, Diagram B) -> IO ()
writeAttAlteredExpBCFig dirFull (rFString, expDia) = do
    relFileName <- parseRelFile rFString
    relFileNameWExt <- addExtension ".pdf" relFileName
    let absFileNameWExt = dirFull </> relFileNameWExt
        fPath = toFilePath absFileNameWExt
    renderCairo fPath (mkWidth 1600) expDia

-- Make the part of the filename for the time series figures associated with a
-- particular Bar in a particular Barcode. If the Bar has matched to a
-- Phenotype, number it out of the total number of Phenotypes associated with
-- that Switch (0-indexed numbering). If not, denote it with an 'm' for "miss".
barFNPattern :: Bar -> T.Text
barFNPattern b = case barPhenotype b of
    Nothing -> "m"
    Just phName -> (T.pack . show . fromJust .
        flip L.elemIndex (phenotypeNames b)) phName
    
        

mkInvestigationAtts :: Path Abs File
                    -> DMModel
                    -> VEXLayerExpSpec
                    -> IO (HS.HashSet Attractor)
mkInvestigationAtts dmmsF dmModel vLExSpec = case layerMatch dmModel vLExSpec of
    Failure errs -> fail $ show errs
    Success ((mM, mL), _) -> case samplingVal mL $ sampling vLExSpec of
        Failure errs -> do
            let errMessage = L.intercalate "\n" $
                                    (T.unpack . vexErrorPrep) <$> errs
            fail errMessage
        Success (SampleOnly sParams) -> do
            gen <- initStdGen
            let mEnv = (mL, sParams)
                atts = evalState (attractors mEnv) gen
                LayerSpecs lniBMap _ _ _ = layerPrep mL
                attPs = (randomN sParams, noisyN sParams, noisyP sParams)
            writeAttractorBundle dmmsF lniBMap dmmsMM attPs Nothing atts
            return atts
        Success (ReadOnly csvfileString) -> do
            (_, _, atts) <- loadAttCSV (dmmsMM, mL) csvfileString
            return atts
        Success (ReadAndSample sParams csvfileString) -> do
            gen <- initStdGen
            let mEnv = (mL, sParams)
                generatedAtts = evalState (attractors mEnv) gen
                LayerSpecs lniBMp _ _ _ = layerPrep mL
                attPs = (randomN sParams, noisyN sParams, noisyP sParams)
            (_, _, loadedAtts) <- loadAttCSV (dmmsMM, mL) csvfileString
            writeAttractorBundle dmmsF lniBMp dmmsMM attPs Nothing generatedAtts
            return $ generatedAtts <> loadedAtts
        where
            dmmsMM = (fst . modelMappingSplit) mM
            

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
              (TL.unpack . PS.pShowNoColor) errs
            Success vGML -> case M.runParser modelFileParse strFileName
                                                            dmmsContent of
              (Left dErr) -> PS.pPrint (M.errorBundlePretty dErr)
              (Right (dmmsVer, (dmm, cd))) -> do
                putStrLn "Good parse"
                case updateDMModel dmm vGML of
                  Failure updateErrs -> fail $ "Failed Update:\n" <>
                    (TL.unpack . PS.pShowNoColor) updateErrs
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

procedureDMMS :: Path Abs File
         -> Procedures
         -> Either
                (M.ParseErrorBundle T.Text Void)
                (FileFormatVersion, (DMModel, CitationDictionary))
         -> IO ()
procedureDMMS _ _ (Left err) = PS.pPrint (M.errorBundlePretty err)
procedureDMMS f options (Right parsed) = do
    let dmModel = (fst . snd) parsed
        citeDict = (snd . snd) parsed
        fileVersion = fst parsed
    putStrLn "Good parse"
    when (pubWarn options)
        (pubWWrite f (dmModel, citeDict))
    when (layerILevels options)
        (inputPP dmModel)
    when (suppPDF options)
        (writeSupp f (snd parsed))
    when (gmlWrite options)
        (writeGML f dmModel)
    when (parseTest options) $ do
        pTest <- parseRelFile "test/parseTest.dmms"
        let modelRender = renderDMMS fileVersion dmModel citeDict
        RW.writeFile pTest modelRender
--         pTest <- parseRelFile "test/parseTest.hs"
--         RW.writeFile pTest $ TL.toStrict $ PS.pShowNoColor $ dmModel
    when (ttWrite options)
        (ttFWrite f
                  (layerTTs <$> (modelLayers dmModel))
                  (mkBooleanNet dmModel)
                  (let dmmsMMaps = ((fst . modelMappingSplit) <$>
                            (modelMappings dmModel))
                       mMs = T.intercalate "\n" <$> (renderDMMSSwitch <<$>>
                            dmmsMMaps)
                       mNs = (modelName . modelMeta) <$> modelLayers dmModel
                   in zip mNs mMs)
        )
    when (coordColors options)
        (graphDetailWrite f dmModel)


-- Write TT & BooleanNet files to disk, as extracted from a dmms file, in a
-- directory with the dmms' name. Also write out the SwitchMappings to the same
-- directory as the dmms file. 
ttFWrite :: Path Abs File
         -> ModelTTFiles
         -> [(ModelName, BooleanNet)]
         -> [(ModelName, T.Text)]
         -> IO ()
ttFWrite mFilePath fs bNetPs mMs = do
    (pathNoExt, _) <- splitExtension mFilePath
    let ttFiles = snd <<$>> (snd <$> fs)
        ttFileNames = (T.unpack . fst) <<$>> (snd <$> fs)
        bNFiles = snd <$> bNetPs
        bNFileNames = (T.unpack . fst) <$> bNetPs
        mMsFiles = snd <$> mMs
        mMsFileNames = (T.unpack . flip T.append "_ModelMapping"  . fst) <$> mMs
    topDir <- parseAbsDir $ fromAbsFile pathNoExt
    ttFileNameRels <- mapM (mapM parseRelFile) ttFileNames
    ttFileNamesWExt <- mapM (mapM (addExtension ".csv")) ttFileNameRels
    bNFileNameRels <- mapM parseRelFile bNFileNames
    bNFileNamesWExt <- mapM (addExtension ".booleannet") bNFileNameRels
    mMsFileNameRels <- mapM parseRelFile mMsFileNames
    mMsFileNamesWExt <- mapM (addExtension ".txt") mMsFileNameRels
    layerDirs <- mapM parseRelDir ((T.unpack . fst) <$> fs)
    let ttDirs = (topDir </>) <$> layerDirs
    mapM_ ensureDir ttDirs
    mapM_ removeDirRecur ttDirs
    mapM_ ensureDir ttDirs
    let ttPaths = zipWith (<$>) ((</>) <$> ttDirs) ttFileNamesWExt
        bNPaths = (topDir </>) <$> bNFileNamesWExt
        mMPaths = mMsFileNamesWExt
    zipWithM_ (zipWithM_ RW.writeFile) ttPaths ttFiles
    zipWithM_ RW.writeFile bNPaths bNFiles
    zipWithM_ RW.writeFile mMPaths mMsFiles

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
        Success _ -> "No Warnings"
        Failure ws
            | all isUnescaped ws -> "Some unescaped _, no other warnings"
            | otherwise          -> "See Metadata Warnings"
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

figureDMMS :: Path Abs File
           -> Figures
           -> Either
                (M.ParseErrorBundle T.Text Void)
                (FileFormatVersion, (DMModel, CitationDictionary))
           -> IO ()
figureDMMS _ _ (Left err) = PS.pPrint (M.errorBundlePretty err)
figureDMMS f attFileName (Right parsed) = do
    putStrLn "Good parse"
    let dmModel = (fst . snd) parsed
        cMap = mkColorMap dmModel
        fLayer = fineLayer dmModel
    mMap <- twoLayerModelMapping $ modelMappings dmModel
    let dmmsMMap = (fst . modelMappingSplit) mMap
    (_, _, atts) <- loadAttCSV (dmmsMMap, fLayer) $ T.unpack attFileName
    putStrLn "Generating Figure"
    fiveDFig <- mkFiveDFigure cMap (mMap, fLayer) atts
    putStrLn "Writing PDF File"
    writeFiveDFig f fiveDFig

-- Read and validate an attractor csv file.
-- Parse attractor csv files correctly!!!!!!!
loadAttCSV :: (DMMSModelMapping, ModelLayer) -> FilePath -> IO (AttractorBundle)
loadAttCSV (dmmsMMap, mL) csvAttFStr = do
    csvAttFilePath <- resolveFile' csvAttFStr
    (_, csvAFExt) <- splitExtension csvAttFilePath
    attFileContent <- RW.readFile csvAttFilePath
    case csvAFExt of
        ".csv" -> do
            let parsedAttBundle = attractorFileParse attFileContent
            case attractorCheck (dmmsMMap, mL) parsedAttBundle of
                Failure errs -> fail $ show errs
                Success atts -> do  
                    let LayerSpecs lniBMap _ _ _ = layerPrep mL
                    return (dmmsMMap, lniBMap, atts)
        _ -> fail $ "not a cvs file: " <> csvAttFStr


-- Interact with the user to determine which, if any, inputs are pinned in the
-- 5-D figure, then generate the PDF. 
mkFiveDFigure :: ColorMap
              -> (ModelMapping, ModelLayer)
              -> HS.HashSet Attractor
              -> IO (Diagram B)
mkFiveDFigure cMap (mMap, fLayer) atts = do
    let LayerSpecs lniBMap _ _ _ = layerPrep fLayer
        ipPtNodes = (inputs . modelGraph) fLayer
        numEnv = L.length ipPtNodes
        inptOpts = inputOptions ipPtNodes
        txtInptOpts = textInputOptions ipPtNodes
    case filter ((/= []) . snd . snd) mMap of
        [] -> do
            fail "There are no SwitchProfiles in the DMMS file, so we \
                \cannot make an environmental input figure. "
        _ -> do
            putStrLn $ show numEnv <> " environmental inputs to plot. "
            case numEnv <= 5 of
                True -> case numEnv == 0 of
                    True -> fail "Fine layer has no environmental inputs!"
                    False -> do
                        return $ attractorESpaceFigure cMap mMap lniBMap atts
                                        (InputBundle ipPtNodes U.empty Nothing)
                False -> do
                    putStrLn $ "There are more than 5 environments. Please \
                        \choose at least " <> (show $ numEnv - 5) <> " to pin:"
                    putStrLn $ (T.unpack txtInptOpts) <> "\n"
                    pins <- getLine
                    let pinsPOut = M.runParser (pinsParse inptOpts) ""
                                                                (T.pack pins)
                    case pinsPOut of
                        (Left err) -> do
                            PS.pPrint (M.errorBundlePretty err)
                            fail "Bad pin parse."
                        (Right parsedPinned) -> do
                            let iB =
                                    mkInputBundle ipPtNodes lniBMap parsedPinned
                            return $
                                attractorESpaceFigure cMap mMap lniBMap atts iB


-- Consume the [[DMNode]] which is the list of environmental inputs, the map
-- between the LayerVec index position and DMNode NodeNames, and the nodes
-- pinned by the user, and produce an InputBundle. This InputBundle will not
-- have a BarcodeFilter
mkInputBundle :: [[DMNode]]
              -> LayerNameIndexBimap
              -> [(NodeName, Int)]
              -> InputBundle
mkInputBundle inputDMNodes lniBMap inputChoices =
    InputBundle freeINs pinnedVec Nothing
    where
        pinnedVec :: FixedVec
        pinnedVec = U.fromList $ (BF.first (lniBMap BM.!)) <$> choiceAssocs
        choiceAssocs :: [(NodeName, NodeState)]
        choiceAssocs = concat $ pickStates inputChoices <$> fixedINs
        (fixedINs, freeINs) = L.partition (splitter inputChoices) inputDMNodes
        splitter iCs nodeStack = any (`elem` (fst <$> iCs)) stackNames
            where
                stackNames = (nodeName . nodeMeta) <$> nodeStack

-- For now, we do all our simulation and figure making on the two finest-grained
-- of any given DMModel. twoLayerModelMapping complains if you pass it a single-
-- layer DMModel, and explains program behavior if you pass it a three or more-
-- layer DMMOdel
twoLayerModelMapping :: [ModelMapping] -> IO ModelMapping
twoLayerModelMapping [] =
    fail "Single layer dmms! No ModelMappings to fingerprint on! "
twoLayerModelMapping [mMap] = return mMap
twoLayerModelMapping mMaps = do
    putStrLn "More than two layers. Using Fine and first LayerBinding. "
    let mm = last mMaps
    return mm

writeFiveDFig :: Path Abs File -> Diagram B -> IO ()
writeFiveDFig f fiveDDia = do
    (fName, _) <- splitExtension $ filename f
    let fNameString = fromRelFile fName
        fiveDFigFileName = fNameString ++ "_Attrs_Env_Space"
    fiveDFigFileNameRel <- parseRelFile fiveDFigFileName
    fiveDFigFileNameRelWExt <- (addExtension ".pdf") fiveDFigFileNameRel
    let fPath = toFilePath fiveDFigFileNameRelWExt
    renderCairo fPath (mkWidth 1600) fiveDDia

-- Pretty print the layers and input levels of a DMModel to terminal. 
inputPP :: DMModel -> IO ()
inputPP dMM = do
    let layers = modelLayers dMM
        layerNames = (modelName . modelMeta) <$> layers
        layerLevels = (textInputOptions . inputs . modelGraph) <$> layers
        merger nN ls = nN <> ": \n" <> ls <> "\n"
        layerTs = zipWith merger layerNames layerLevels
        layerT = (T.unpack . T.intercalate "\n") layerTs
    putStrLn "Model layers and their inputs:"
    putStrLn layerT

--------------------------------------------------------------------------------

-- IO versions of experiment running 


-- Run all the experiments from a DMInvestigation. The paired Attractors must
-- be checked beforehand. 
runInvestigationIO :: Path Abs File
                   -> ColorMap
                   -> StdGen
                   -> [(HS.HashSet Attractor, LayerExpSpec)]
                   -> IO [LayerResultIO]
runInvestigationIO fPath cMap gen attLExpSpecPairs =
    snd <$> mapAccumM (runLayerExperimentsIO fPath cMap) gen attLExpSpecPairs

runLayerExperimentsIO :: Path Abs File
                      -> ColorMap
                      -> StdGen
                      -> (HS.HashSet Attractor, LayerExpSpec)
                      -> IO (StdGen, LayerResultIO)
runLayerExperimentsIO fPath cMap gen (atts, lExpSpec) = do
    let mMap = layerExpMMapping lExpSpec
        mL = layerExpMLayer lExpSpec
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        mIB = invesIBundle lExpSpec
        mAttESpaceFig = attractorESpaceFigure cMap mMap lniBMap atts <$> mIB
    mapM_ (writeFiveDFig fPath) mAttESpaceFig
    let exps = experiments lExpSpec
        runExperimentIOF = runExperimentIO fPath cMap mMap mL atts
    (nGen, eHooks) <- mapAccumM runExperimentIOF gen exps
    let lResult = LayerResultIO mMap mL eHooks
    return (nGen, lResult)        

-- Run a DMExperiment by folding up the InputPulses according to the chosen step
-- style. First filter the attractors available. 
-- Note that when running an experiment, it should be run n times for
-- each attractor in the set, where n is the length of the attractor, starting
-- at the next in the loop each time. (Barcode, RepResults or ScanResult)s are
-- then combined if their Barcodes are identical. 
-- For TimeCourse experiments, 
-- Each element in the [([Timeline], [PulseSpacing])] in the
-- (Barcode, RepResults) is turned into a single PDF, no matter how many
-- Timelines it has. As of 10/24/23, only KDOEAtTransition experiments will have
-- more than one, because only they alter the experiment as a function of the
-- size of the Attractor they start in, but this will change in the future. This
-- is in addition to the fact that (Barcode, RepResults) were combined if their
-- Barcodes were identical. 

-- Run a DMExperiment and write the results to disk
runExperimentIO :: Path Abs File
                -> ColorMap
                -> ModelMapping
                -> ModelLayer
                -> HS.HashSet Attractor
                -> StdGen
                -> (DMExperiment, VEXExperiment)
                -> IO (StdGen, ExperimentHook)
runExperimentIO fPath cMap mM mL attSet gen (ex, vexEx) = case ex of
    TCDMEx tcExp -> do
        let expGen = fromMaybe gen (manualTCPRNGSeed tcExp)
            filteredAtts = tcAttFilter tcExp $ layerBCG <$> attList
            expMeta = tcExpMeta tcExp
            expName = (T.unpack . tcExpName) expMeta
            (markGen, attResults) =
                L.mapAccumL (runTimeCourse phData tcExp) expGen filteredAtts
            (xMark, newGen) = uniform markGen
            dmXOutput = preOutput (ExpOutput vexEx (TCO attResults) xMark)
        putStrLn $ "Running experiment: " <> expName
        fullDir <- mkExpPath fPath (TCEM expMeta) "Results"
        let noDetailsDir = parent fullDir
        ensureDir noDetailsDir
        let fileNameStr = expName <> show xMark
        relFileName <- parseRelFile fileNameStr
        relFileNameWExt <- addExtension ".csv" relFileName
        let absFileNameWExt = noDetailsDir </> relFileNameWExt
        RW.writeFileL absFileNameWExt (renderDMExpOutput dmXOutput)
        putStrLn $ "Generating figures for " <> expName
        mapM_ (tcRunDiaIO fPath cMap mM mL expMeta xMark)
                                                (resCombine attResults)
        return (newGen, (xMark, absFileNameWExt))
    ScDMex scanExp -> do
        let filteredAtts = scAttFilter scanExp $ layerBCG <$> attList
            expMeta = scExpMeta scanExp
            expName = (T.unpack . scExpName) expMeta
            (markGen, attResults) =
                L.mapAccumL (runScan phData scanExp) gen filteredAtts
            (xMark, newGen) = uniform markGen
            dmXOutput = preOutput (ExpOutput vexEx (SCO attResults) xMark)
        putStrLn $ "Running experiment: " <> expName
        fullDir <- mkExpPath fPath (SCEM expMeta) "Results"
        let noDetailsDir = parent fullDir
        ensureDir noDetailsDir
        let fileNameStr = expName <> show xMark
        relFileName <- parseRelFile fileNameStr
        relFileNameWExt <- addExtension ".csv" relFileName
        let absFileNameWExt = noDetailsDir </> relFileNameWExt
        RW.writeFileL absFileNameWExt (renderDMExpOutput dmXOutput)
        putStrLn $ "Generating figures for " <> expName
        mapM_ (scRunDiaIO fPath cMap mM mL expMeta) attResults
        return (newGen, (xMark, absFileNameWExt))
    where
        attList = HS.toList attSet
        phData = (lniBMap, phs)
        phs = concatMap (snd . snd) mM
        LayerSpecs lniBMap _ _ _ = layerPrep mL
        layerBCG = mkBarcode cMap mM lniBMap -- Make (BC, Att) pairs
        preOutput = DMExpOutput lGates lniBMap mM
        lGates = (fmap nodeGate . layerNodes) mL


-- Construct the path to the results or figures of a given DMExperiment. 
mkExpPath :: Path Abs File -> ExpMeta -> String -> IO (Path Abs Dir)
mkExpPath vexFPath dmExpMeta figsOrRess = do
    let dirStem = parent vexFPath
    dirExpWhat <- case figsOrRess of
        "" -> parseRelDir "_EXP"
        ress -> do
            dirExp <- parseRelDir "_EXP"
            dirWhat <- parseRelDir ress
            return $ dirExp </> dirWhat
    dirCat <- case dmExpMeta of
        TCEM tcXMeta -> case (tcExpKind tcXMeta) of
            P1 -> parseRelDir "Pulse1"
            KDOE -> parseRelDir "KD_OE"
            KDOEAtTr -> parseRelDir "KD_OE_At_Transition"
            GenExp -> parseRelDir "General_Time_Series"
        SCEM scXMeta -> case scMetaScanKind scXMeta of
            (MetaEnvSc _ _) -> parseRelDir "EnvScan"
            (MetaKDOESc _ _) -> parseRelDir "KDOE_Scan"
            (MetaEnvKDOEScan _ _ _) -> parseRelDir "Env_KDOE_Scan"
            (MetaTwoDEnvScan _ _ _ _) -> parseRelDir "TwoDEnvScan"
            (MetaThreeDEnvScan _ _ _ _) -> parseRelDir "ThreeDEnvScan"
    dirExpDetails <- case dmExpMeta of
        TCEM tcXMeta -> parseRelDir ((T.unpack . tcExpDetails) tcXMeta)
        SCEM scXMeta -> parseRelDir ((T.unpack . scExpDetails) scXMeta)
    let dirFull = dirStem </> dirExpWhat </> dirCat </> dirExpDetails
    return dirFull

tcRunDiaIO :: Path Abs File
           -> ColorMap
           -> ModelMapping
           -> ModelLayer
           -> TCExpMeta
           -> ExperimentMark
           -> (Barcode, [RepResults])
           -> IO ()
tcRunDiaIO fPath cMap mM mL expMeta xMark (bc, repRs) = do
    let figKs = tcExpFigures expMeta
        stripHt = 2.0 :: Double
        params = (stripHt, 24.0) :: (Double, Double)
        reps = ((fromIntegral . expReps) expMeta) :: Double
        avgRepRs = (fmap . BF.first) (averagedAttResults reps) repRs
        -- Integrate the [[PulseSpacing]] with the RealExpSpreadResults
        avgTmlnPSs = (uncurry zip) <$> avgRepRs
        expDetails = tcExpDetails expMeta
        expName = tcExpName expMeta
        bcPatterns = mconcat $ barFNPattern <$> bc
    dataDir <- mkExpPath fPath (TCEM expMeta) "Results"
    figDir <- mkExpPath fPath (TCEM expMeta) ""
    when (nodeTimeCourse figKs) $ do
        let nodeTCFigs = nodeTCDia cMap mM mL bc params avgTmlnPSs
        dirNTC <- parseRelDir "NodeTC"
        writeExpBCFig (figDir </> dirNTC) expDetails (bc, nodeTCFigs)
    when (phenotypeTimeCourse figKs) $ do
        let phenotypeTCFigs =
                        phenotypeTCDia cMap mM mL bc expMeta stripHt avgTmlnPSs
        dirPHTC <- parseRelDir "PHTC"
        writeExpBCFig (figDir </> dirPHTC) expDetails (bc, phenotypeTCFigs)
    when ((not . null) (nodeAvgBars figKs)) $ do
        let bCHNodeNs = nodeAvgBars figKs
            statRepRVecs :: [[[B.Vector (U.Vector RealNodeState)]]]
            statRepRs :: [[[B.Vector (RealNodeState, StdDev)]]]
            (statRepRVecs, statRepRs) = (unzip . fmap nodeBarChartStats) repRs
            LayerSpecs lniBMap _ _ _ = layerPrep mL
            nmdVF = (fmap . BF.first) (lniBMap BM.!>) . zip [0..] . B.toList
            namedPulseVecs :: [[[[(NodeName, U.Vector RealNodeState)]]]]
            namedPulseVecs = (fmap . fmap . fmap) nmdVF statRepRVecs
            formattedData :: [TL.Text]
            formattedData = renderNBCData <$> namedPulseVecs
            nBChartFigs :: [[Diagram B]]
            nBChartFigs = nBChartDia cMap expMeta bCHNodeNs <<$>> statRepRs
            mergedNBChartFigs = (vsep 5.0) <$> nBChartFigs
            numFigs = length mergedNBChartFigs
            dataFileNStrs =
                (((T.unpack expName <> show xMark <> "_") <>) . show) <$>
                    [1..numFigs]
        dirNBCH <- parseRelDir "NodeBCh"
        ensureDir (dataDir </> dirNBCH)
        dataFileNames <- mapM parseRelFile dataFileNStrs
        relDataFileNamesWExt <- mapM (addExtension ".csv") dataFileNames
        let absDataFileNamesWExt =
                            ((dataDir </> dirNBCH) </>) <$> relDataFileNamesWExt
        mapM_ (uncurry RW.writeFileL) (zip absDataFileNamesWExt formattedData)
        let rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
            rFStrings = (((rFString <> "_" )<>) . show) <$> [1..numFigs]
        relFileNames <- mapM parseRelFile rFStrings
        relFileNamesWExt <- mapM (addExtension ".pdf") relFileNames
        ensureDir (figDir </> dirNBCH)
        let absFileNamesWExt = ((figDir </> dirNBCH) </>) <$> relFileNamesWExt
            fStrDiaPairs = zip absFileNamesWExt mergedNBChartFigs
            renderF (f, d) = renderCairo (toFilePath f) (mkWidth 1600) d
        mapM_ renderF fStrDiaPairs
    when ((not . null) (phenotypeAvgBars figKs)) $ do
        let phCHNodeNs = phenotypeAvgBars figKs
            nonEmptySwPhs = snd <<$>> (nonEmptyPhenotypes mM)
            nonEmptySwPhNs = (fmap . fmap . fmap) phenotypeName nonEmptySwPhs
            allPhNs = concatMap snd nonEmptySwPhNs
            switchMap = HM.fromList nonEmptySwPhNs
            pulseSpcs = (fmap . fmap) snd avgTmlnPSs
            zipZip = (zipWith . zipWith) (,)
            phStatRepRVecs :: [[[HM.HashMap PhenotypeName (U.Vector Double)]]]
            phStatRepRs :: [[[HM.HashMap PhenotypeName (Double, StdDev)]]]
            (phStatRepRVecs, phStatRepRs) =
                                (unzip . fmap (phBarChartStats allPhNs)) repRs
            chartVecsss ::
                [[[(SwitchName, [[(PhenotypeName, U.Vector Double)]])]]]
            chartVecsss = L.transpose $ phBCDataPrep nonEmptySwPhNs
                <<$>> phStatRepRVecs
            formattedData :: [TL.Text]
            formattedData = renderPhBCData <$> chartVecsss
-- Integrate the [[PulseSpacing]] with the Phenotype prevalence averages.
            pulseStatRepRs = zipZip pulseSpcs phStatRepRs
            phBCF = phBChartDia cMap mL switchMap expMeta phCHNodeNs
            phBChartFgs :: [Diagram B]
            phBChartFgs = ((vsep 5.0) . fmap (uncurry phBCF)) <$> pulseStatRepRs
            numFigs = length phBChartFgs
            dataFileNStrs =
                (((T.unpack expName <> show xMark <> "_") <>) . show)
                    <$> [1..numFigs]
        dirPHBCH <- parseRelDir "PhBCh" 
        ensureDir (dataDir </> dirPHBCH)
        dataFileNames <- mapM parseRelFile dataFileNStrs
        relDataFileNamesWExt <- mapM (addExtension ".csv") dataFileNames
        let absDataFileNamesWExt =
                        ((dataDir </> dirPHBCH) </>) <$> relDataFileNamesWExt
        mapM_ (uncurry RW.writeFileL) (zip absDataFileNamesWExt formattedData)
        let rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
            rFStrings = (((rFString <> "_") <>) . show) <$> [1..numFigs]
        relFileNames <- mapM parseRelFile rFStrings
        relFileNamesWExt <- mapM (addExtension ".pdf") relFileNames
        ensureDir (figDir </> dirPHBCH)
        let absFileNamesWExt = ((figDir </> dirPHBCH) </>) <$> relFileNamesWExt
            fStrDiaPairs = zip absFileNamesWExt phBChartFgs
            renderF (f, d) = renderCairo (toFilePath f) (mkWidth 1600) d
        mapM_ renderF fStrDiaPairs

scRunDiaIO :: Path Abs File
           -> ColorMap
           -> ModelMapping
           -> ModelLayer
           -> SCExpMeta
           -> (Barcode, ScanResult)
           -> IO ()
scRunDiaIO fPath cMap mM mL exMeta (bc, scRes) = do
  let
    bcPatternStr = "bc" ++  (T.unpack . mconcat . fmap barFNPattern) bc
    overLayVs = needOverlays exMeta
    LayerSpecs lniBMap _ _ _ = layerPrep mL
    phCMap = mkPhColorMap mM cMap
    switchMap = HM.fromList nonEmptySwPhNs
    nonEmptySwPhNs = (fmap . fmap . fmap) phenotypeName nonEmptySwPhs
    nonEmptySwPhs = snd <<$>> (nonEmptyPhenotypes mM)
  figDir <- mkExpPath fPath (SCEM exMeta) ""
  bcDir <- parseRelDir bcPatternStr
  let
    bcAbsDir = figDir </> bcDir
    baseScDiaF = baseScDia cMap phCMap lniBMap switchMap exMeta
  case scRes of
    (SKREnv scBundle) -> do
      let
        BSFgs stopDFig timeInSwitchFigs avgNValueFigs = baseScDiaF scBundle
        flatFigs = stopDFig : (timeInSwitchFigs <> avgNValueFigs)
      mapM_ (simpleSCWrite bcAbsDir) flatFigs
    (SKRKDOE scBundle) -> do
      let
        BSFgs stopDFig timeInSwitchFigs avgNValueFigs = baseScDiaF scBundle
        flatFigs = stopDFig : (timeInSwitchFigs <> avgNValueFigs)
      mapM_ (simpleSCWrite bcAbsDir) flatFigs
    (SKREnvKDOE scBundles) -> do
      let
        swFig = envKDOESWDia phCMap switchMap exMeta scBundles
        nodeFig = envKDOENodeDia cMap mL exMeta scBundles
        flatFigs = [("switches", swFig), ("nodes", nodeFig)]
      mapM_ (simpleSCWrite bcAbsDir) flatFigs
    (SKRTwoEnvWithoutKDOE scBundles) -> do
      let
        (stopPercentFigs, switchHMFigs, nodeHMFigs) = 
          scHeatMapDias mL overLayVs switchMap exMeta scBundles
        flatFigs = stopPercentFigs <> switchHMFigs <> nodeHMFigs
      mapM_ (simpleSCWrite bcAbsDir) flatFigs
    (SKRTwoEnvWithKDOE scBundless) -> do
--   These L.transposes are so that the KDOE is at the top level, so that we
--   may generate the figures. 
      let
        tpBundless = (L.transpose . fmap L.transpose) scBundless
        hmFigs = scHeatMapDias mL overLayVs switchMap exMeta <$> tpBundless
        (offAxisTitle, offAxisRange) =
          (last . scanXAxisData . scMetaScanKind) exMeta
        offAxisPairs = zip (repeat offAxisTitle) offAxisRange
        map3 f (a, b, c) = (f a, f b, f c)
        figs = unzip3 $ zipWith labelMutantHMDias hmFigs offAxisPairs
        concatF :: [(T.Text, Diagram B)] -> (T.Text, Diagram B)
        concatF [] = (mempty, mempty)
        concatF ps = ((fst . head) ps, (hsep 50 . fmap snd) ps)
        joinedFigs = map3 ((fmap concatF) . L.transpose) figs
        framerF = map3 ((fmap . fmap) (frame 20))
        (stopDFigs, switchHMFigs, nodeHMFigs) = framerF joinedFigs
        flatFigs = stopDFigs <> switchHMFigs <> nodeHMFigs
      mapM_ (simpleSCWrite bcAbsDir) flatFigs
--  Take care to distinguish between when [WildTypeVsMutantAlt] are [] and not. 
    (SKRThreeEnv (scBundless, mScBundless)) -> case mScBundless of
      Just scbs -> do
        let
          (stopPlotFigs, swFigs, nFigs) = scDifferenceHeatMapDia mL overLayVs
                                            switchMap exMeta (scBundless, scbs)
          flatFigs = stopPlotFigs <> swFigs <> nFigs
        mapM_ (simpleSCWrite bcAbsDir) flatFigs
      Nothing -> do
        let
          (stPhFigs, swFigs, nodeFigs) = sc3DHeatMapDia mL overLayVs switchMap
                                                            exMeta scBundless
          flatFigs = stPhFigs <> swFigs <> nodeFigs
        mapM_ (simpleSCWrite bcAbsDir) flatFigs

