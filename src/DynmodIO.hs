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
import qualified Data.Text.Lazy as LT
import Diagrams.Prelude (mkWidth, Diagram)
import Diagrams.Backend.Cairo (B, renderCairo)
import qualified Data.Graph.Inductive as Gr
import qualified Text.Pretty.Simple as PS
import qualified Data.HashSet as HS
import qualified Text.Megaparsec as M
import qualified Data.Bimap as BM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import Data.Validation (Validation(..))
import System.Random (initStdGen)
import Data.Void
import Control.Monad (void, zipWithM_, when)
import Control.Monad.Reader (runReader)
import Control.Monad.State.Strict (evalState)
import qualified Data.List as L
import qualified Data.Bifunctor as BF
import Data.Maybe (fromJust)

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
            tShow attNumber
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
            (tShow <<$>> Split.splitPlaces ss r)

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
                let invRs = runInvestigation cMap gen $ zip attSets lExpSpecs
                    numExp = sum $ (length . layerResultERs) <$> invRs
                    expTense
                        | numExp == 1 = " experiment. "
                        | otherwise = " experiments. "
--                 PS.pPrint (tmlnLF <$> invRs)
                putStrLn $ "Ran " <> show numExp <> expTense
                putStrLn "Generating figures..."
                let lRFigs = zipWith ($) (layerRunFigure cMap <$> attSets) invRs
                writeVexFigs dmmsPath lRFigs

-- tmlnLF :: LayerResult -> [[[Int]]]
-- tmlnLF lR = (fmap tmlnLF' . layerResultERs) lR
-- 
-- tmlnLF' :: ExperimentResult -> [[Int]]
-- tmlnLF' (TCExpRes _) = [[0]]
-- tmlnLF' (ScanExpRes (_, scanRs)) = (tmlnLF'' . snd) <$> scanRs
-- 
-- tmlnLF'' :: ScanResult -> [Int]
-- tmlnLF'' (SKREnv tmlnss) = (sum . (fmap length)) <$> tmlnss
-- tmlnLF'' (SKRKDOE tmlnss) = (sum . (fmap length)) <$> tmlnss
-- tmlnLF'' (SKREnvKDOE tmlnss) = sum <$> sums
--     where sums = (fmap . fmap) (sum . (fmap length)) tmlnss
-- tmlnLF'' (SKRTwoEnvWithoutKDOE tmlnss) = sum <$> sums
--     where sums = (fmap . fmap) (sum . (fmap length)) tmlnss
-- tmlnLF'' (SKRTwoEnvWithKDOE tmlnss) = (fmap sum . (fmap . fmap) sum) sums
--     where sums = (fmap . fmap . fmap) (sum . (fmap length)) tmlnss
-- tmlnLF'' (SKRThreeEnv (tmlnss, _)) = (fmap sum . (fmap . fmap) sum) sums
--     where sums = (fmap . fmap . fmap) (sum . (fmap length)) tmlnss

writeVexFigs :: Path Abs File
             -> [([ExperimentFigures], Maybe (Diagram B))]
             -> IO ()
writeVexFigs fP layerFigs = mapM_ (writeLayerFig fP) layerFigs
    where
        writeLayerFig f (expPairs, m5DDia) = do
            mapM_ (writeExpFigs f) expPairs
            mapM_ (writeFiveDFig f) m5DDia

writeExpFigs :: Path Abs File -> ExperimentFigures -> IO ()
writeExpFigs f (TCExpFigs tcFigs) = writeTCExpFig f tcFigs
writeExpFigs f (ScanExpFigs scFigs) = writeScExpFigs f scFigs

writeScExpFigs :: Path Abs File
               -> (SCExpMeta, [(Barcode, [ScanExpFigure])])
               -> IO ()
writeScExpFigs f (scExpMt, bcExpFigs) = do
    let dirStem = parent f
--         expDetails = scExpDetails scExpMt
        expType = scMetaScanKind scExpMt
        expName = scExpName scExpMt
    dirExp <- parseRelDir "_EXP"
    dirCat <- case expType of
        (MetaEnvSc _ _) -> parseRelDir "EnvScan"
        (MetaKDOESc _ _) -> parseRelDir "KDOE_Scan"
        (MetaEnvKDOEScan _ _ _) -> parseRelDir "Env_KDOE_Scan"
        (MetaTwoDEnvScan _ _ _) -> parseRelDir "TwoDEnvScan"
        (MetaThreeDEnvScan _ _ _) -> parseRelDir "ThreeDEnvScan"
    dirExpName <- parseRelDir (T.unpack expName)
    let dirFull = dirStem </> dirExp </> dirCat </> dirExpName
    mapM_ (writeScanExpFigures dirFull scExpMt) bcExpFigs

-- This is a simple write to disk of the Diagram Bs for now, until I have a
-- better idea of what they look like. 
writeScanExpFigures :: Path Abs Dir
                    -> SCExpMeta
                    -> (Barcode, [ScanExpFigure])
                    -> IO ()
writeScanExpFigures f scExpMt (bc, figs) = do
    let bcPatterns = (mconcat $ barFNPattern <$> bc) :: T.Text
        bcPatternStr = "bc" ++ T.unpack bcPatterns
    bcDir <- parseRelDir bcPatternStr
    let fPath = f </> bcDir
    mapM_ (writeScanExpFigure fPath scExpMt bc) figs

writeScanExpFigure :: Path Abs Dir
                   -> SCExpMeta
                   -> Barcode
                   -> ScanExpFigure
                   -> IO ()
writeScanExpFigure f scExpMt bc fig = case fig of
    EnvScFig bsScFgs -> writeBaseScanFig f scExpMt bc bsScFgs
    KDOEScFig bsScFgs -> writeBaseScanFig f scExpMt bc bsScFgs
    EnvKDOESc envKDOEFig -> simpleSCWrite f scExpMt 0 envKDOEFig
    TwoDEnvScWWOKDOE htmaps ->
        mapM_ (uncurry (simpleSCWrite f scExpMt)) (zip [0..] htmaps)
    ThreeDEnvSc heatmapBlockFigs -> mapM_ (uncurry (simpleSCWrite f scExpMt))
        (zip [0..] heatmapBlockFigs)

simpleSCWrite :: Path Abs Dir -> SCExpMeta -> Int -> Diagram B -> IO ()
simpleSCWrite f _ i dia = do
    relFileName <- parseRelFile (show i)
    relFileNameWExt <- addExtension ".pdf" relFileName
    let absFileNameWExt = (f </> relFileNameWExt) :: Path Abs File
        fPath = toFilePath absFileNameWExt
    ensureDir f
    renderCairo fPath (mkWidth 1600) dia

-- Again, this is a simple write to disk of the Diagram Bs for now. 
writeBaseScanFig :: Path Abs Dir -> SCExpMeta -> Barcode -> BaseScanFigs -> IO ()
writeBaseScanFig f scExpMt _ (BSFgs stFig tisPhsFigs) = do
    let figs = stFig:tisPhsFigs
    mapM_ (uncurry (simpleSCWrite f scExpMt)) (zip [0..(length tisPhsFigs)] figs)

writeTCExpFig :: Path Abs File
              -> (TCExpMeta, [(Barcode, BCExpFigures)])
              -> IO ()
writeTCExpFig f (dmExpMeta, bcExpFigs) = do
    let dirStem = parent f
        expDetails = tcExpDetails dmExpMeta
        expType = tcExpKind dmExpMeta
        expName = tcExpDetails dmExpMeta
    dirExp <- parseRelDir "_EXP"
    dirCat <- case expType of
        P1 -> parseRelDir "Pulse1"
        KDOE -> parseRelDir "KD_OE"
        KDOEAtTr -> parseRelDir "KD_OE_At_Transition"
        GenExp -> parseRelDir "General_Time_Series"
    dirExpDetails <- parseRelDir (T.unpack expName)
    dirPHTC <- parseRelDir "PHTC"
    dirNTC <- parseRelDir "NodeTC"
    dirNBCH <- parseRelDir "NodeBCh"
    dirPHBCH <- parseRelDir "PhBCh"
    let dirFull = dirStem </> dirExp </> dirCat </> dirExpDetails
-- Note that for a given experiment, there are either Just nodeBCTCFigs for
-- every Barcode or Nothings; similarly for Just phenotypeBCTCFigs. So these
-- (traverse . traverse) will not clobber any figures that we want to write out
-- to disk. 
        tcFigs = (traverse . traverse) nodeBCTCFigs bcExpFigs
        phFigs = (traverse . traverse) phenotypeBCTCFigs bcExpFigs
        nBChFig = (traverse . traverse) nodeBCAvgBarFig bcExpFigs
        phBChFig = (traverse . traverse) phenotypeBCAvgBarFigs bcExpFigs
    (mapM_ . mapM_) (writeExpBCFig (dirFull </> dirNTC) expDetails) tcFigs
    (mapM_ . mapM_) (writeExpBCFig (dirFull </> dirPHTC) expDetails) phFigs
    (mapM_ . mapM_) (wrExpBCFigNBC (dirFull </> dirNBCH) expDetails) nBChFig
    (mapM_ . mapM_) (wrExpBCFigPhBC (dirFull </> dirPHBCH) expDetails) phBChFig

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
--         RW.writeFile pTest $ LT.toStrict $ PS.pShowNoColor $ dmModel
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



-- For now we will keep both the averages and the vectors of NodeState values
-- that they came from. They will be written out with the figure, to be
-- available for additional analysis. Eventually this will be replaced by the
-- ability to do statistics across experiments internally. 
wrExpBCFigNBC :: Path Abs Dir
        -> T.Text
        -> (Barcode, [(Diagram B, [[[(NodeName, U.Vector RealNodeState)]]])])
        -> IO ()
wrExpBCFigNBC dirFull expDetails (bc, expDiasWData) = do
    ensureDir dirFull
    case expDiasWData of
        [] -> do 
            putStrLn "No node bar chart figures for Barcode: "
            PS.pPrint bc
        [(expDia, nodeVecs)] -> do
            let bcPatterns = (mconcat $ barFNPattern <$> bc) :: T.Text
                rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
                formattedData = formatNBCData nodeVecs
            relFileName <- parseRelFile rFString
            relFileNameWExt <- addExtension ".pdf" relFileName
            dataFileNameWExt <- addExtension ".csv" relFileName
            let absFileNameWExt = dirFull </> relFileNameWExt
                absDataFileNameWExt = dirFull </> dataFileNameWExt
                fPath = toFilePath absFileNameWExt
            renderCairo fPath (mkWidth 1600) expDia
            RW.writeFile absDataFileNameWExt formattedData
        expDs -> do
            let bcPatterns = (mconcat $ barFNPattern <$> bc) :: T.Text
                rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
                intBStrs = (("_" <>) . show) <$> [1..L.length expDs]
                rFStrings = (rFString <>) <$> intBStrs
                fStrDiaPairs = zip rFStrings expDs
            mapM_ (writeAttAlteredExpBCFigNBC dirFull) fStrDiaPairs

-- Format DMNode time course Timeline data to be machine readable
formatNBCData :: [[[(NodeName, U.Vector RealNodeState)]]] -> T.Text
formatNBCData pairedVecsss = (T.intercalate "\n" . (fmap (uncurry fDataF)))
  (zip ((T.pack . show) <$> [(0 :: Int)..]) (pairedVecsss))
  where
    fDataF i pairedVecss = "Chart " <> i <> ":\n" <> body
      where
        body = (T.intercalate "\n" . (fmap (uncurry fDataF')))
          (zip ((T.pack . show) <$> [(0 :: Int)..]) (pairedVecss))
        fDataF' j pairedVecs = "Pulse " <> j <> ":\n" <> body'
          where
            body' = T.intercalate "\n" $ fDataF'' <$> pairedVecs
            fDataF'' (nN, nVec) = nN <> ", " <> 
              (T.intercalate ", " . fmap (T.pack . show) . U.toList) nVec
 
writeAttAlteredExpBCFigNBC
    :: Path Abs Dir
    -> (String, (Diagram B, [[[(NodeName, U.Vector RealNodeState)]]]))
    -> IO ()
writeAttAlteredExpBCFigNBC dirFull (rFString, (expDia, nodeVecs)) = do
    relFileName <- parseRelFile rFString
    relFileNameWExt <- addExtension ".pdf" relFileName
    dataFileNameWExt <- addExtension ".csv" relFileName
    let absFileNameWExt = dirFull </> relFileNameWExt
        absDataFileNameWExt = dirFull </> dataFileNameWExt
        fPath = toFilePath absFileNameWExt
        formattedData = formatNBCData nodeVecs
    renderCairo fPath (mkWidth 1600) expDia
    RW.writeFile absDataFileNameWExt formattedData
    

-- For now we will keep both the averages and the vectors of Phenotype
-- prevalence that they came from. They will be written out with the figure, to
-- be available for additional analysis. Eventually this will be replaced by the
-- ability to do statistics across experiments internally. 
wrExpBCFigPhBC :: Path Abs Dir
               -> T.Text
               -> (Barcode, [(Diagram B,
                        [[(NodeName, [[(PhenotypeName, U.Vector Double)]])]])])
               -> IO ()
wrExpBCFigPhBC dirFull expDetails (bc, expDiasWData) = do
    ensureDir dirFull
    case expDiasWData of
        [] -> do 
            putStrLn "No Phenotype bar chart figures for Barcode: "
            PS.pPrint bc
        [(expDia, phVecs)] -> do
            let bcPatterns = (mconcat $ barFNPattern <$> bc) :: T.Text
                rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
                formattedData = formatPhBCData phVecs
            relFileName <- parseRelFile rFString
            relFileNameWExt <- addExtension ".pdf" relFileName
            dataFileNameWExt <- addExtension ".csv" relFileName
            let absFileNameWExt = dirFull </> relFileNameWExt
                absDataFileNameWExt = dirFull </> dataFileNameWExt
                fPath = toFilePath absFileNameWExt
            renderCairo fPath (mkWidth 1600) expDia
            RW.writeFile absDataFileNameWExt formattedData
        expDs -> do
            let bcPatterns = (mconcat $ barFNPattern <$> bc) :: T.Text
                rFString = "bc" ++ (T.unpack $ bcPatterns <> "_" <> expDetails)
                intBStrs = (("_" <>) . show) <$> [1..L.length expDs]
                rFStrings = (rFString <>) <$> intBStrs
                fStrDiaPairs = zip rFStrings expDs
            mapM_ (writeAttAlteredExpBCFigPhBC dirFull) fStrDiaPairs

-- Format DMNode time course Phenotype data to be machine readable
formatPhBCData :: [[(NodeName, [[(PhenotypeName, U.Vector Double)]])]] -> T.Text
formatPhBCData pairss = (T.intercalate "\n" . (fmap (uncurry fDataF)))
  (zip ((T.pack . show) <$> [(0 :: Int)..]) (pairss))
  where
    fDataF i pairs = "Chart " <> i <> ":\n" <> body
      where
        body = (T.intercalate "\n" . (fmap (uncurry fDataF')))
            (zip ((T.pack . show) <$> [(0 :: Int)..]) (byPulseData pairs))
        fDataF' j phD = "Pulse " <> j <> ":\n" <> body'
          where
            body' = T.intercalate "\n" $ fDataF'' <$> phD
            fDataF'' (swN, phs) = swN <> ":\n" <> body''
              where
                body'' = T.intercalate "\n" $ fDataF''' <$> phs
                fDataF''' (phN, avgs) = phN <> ", " <> T.intercalate ", "
                       ((fmap (T.pack . show) . U.toList) avgs)
                  

-- Rearrange Phenotype Barchart data so that it is ordered first by Pulse, 
-- rather than Switch. 
byPulseData :: [(NodeName, [[(PhenotypeName, U.Vector Double)]])]
            -> [[(NodeName, [(PhenotypeName, U.Vector Double)])]]
byPulseData pairs = zip swNames <$> phData
    where (swNames, phData) = (fmap L.transpose . unzip) pairs


writeAttAlteredExpBCFigPhBC
  :: Path Abs Dir
  -> (String, (Diagram B, [[(NodeName, [[(PhenotypeName, U.Vector Double)]])]]))
  -> IO ()
writeAttAlteredExpBCFigPhBC dirFull (rFString, (expDia, phVecs)) = do
    relFileName <- parseRelFile rFString
    relFileNameWExt <- addExtension ".pdf" relFileName
    dataFileNameWExt <- addExtension ".csv" relFileName
    let absFileNameWExt = dirFull </> relFileNameWExt
        absDataFileNameWExt = dirFull </> dataFileNameWExt
        fPath = toFilePath absFileNameWExt
        formattedData = formatPhBCData phVecs
    renderCairo fPath (mkWidth 1600) expDia
    RW.writeFile absDataFileNameWExt formattedData 



