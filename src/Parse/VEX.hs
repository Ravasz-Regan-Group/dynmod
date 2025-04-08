{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parse.VEX
    ( experimentFileParse )
    where

import Utilities
import Types.DMModel
import Types.DMInvestigation
import Constants (vexRWS)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LE
import Control.Applicative.Permutations -- from parser-combinators
import Data.Scientific
import Data.Void
import qualified Data.List as L

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = LE.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = LE.skipLineComment "//"
    blockCmnt = LE.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = LE.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = LE.symbol sc

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'brackets' parses something between brackets.

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | 'number' parses an number in scientific format.

number :: Parser Scientific
number = lexeme LE.scientific

-- | 'integer' parses an unsigned Int.

integer :: Parser Int
integer = lexeme LE.decimal

signedInteger :: Parser Int
signedInteger = LE.signed sc integer

-- | 'colon' parses a colon.

colon :: Parser T.Text
colon = symbol ":"

cInt :: Parser Int
cInt = colon >> integer

-- | 'comma' parses a comma.

comma :: Parser T.Text
comma = symbol ","

-- | 'rword' generates a parser for a specified reserved word. 

rword :: T.Text -> Parser ()
rword w = (lexeme . try) (chunk w *> notFollowedBy alphaNumChar)

-- Parse an identifier
identifier :: T.Text -> Parser T.Text
identifier name = lexeme $ rword name >> colon >> (p >>= check)
  where
    p = T.pack <$> ((:) <$> letterChar <*> (many (alphaNumChar <|> char '_')))
    check x = if x `elem` vexRWS
                then fail $ "Keyword " ++ show x ++ " cannot be a "
                    ++ (T.unpack name)
                else return x

multiIdentifier :: T.Text -> Parser [T.Text]
multiIdentifier name = lexeme $ rword name >>
    (try
        (colon >>
            (sepBy1 variable comma)
        )
    )

-- Parse an assignment
variable :: Parser T.Text
variable = (lexeme . try) (p >>= check)
  where
    p = T.pack <$> ((:) <$> letterChar <*> (many (alphaNumChar <|> char '_')))
    check x = if x `elem` vexRWS
                then fail $ "Keyword " ++ show x ++ " cannot be a variable. "
                else return x

boolParse :: Parser Bool
boolParse = (try (True <$ rword "True")) <|> (False <$ rword "False")

boolVariableParse :: T.Text -> Parser Bool
boolVariableParse t = rword t >> colon >> boolParse


-- Parse an identified FilePath
identifiedFilePathParse :: T.Text -> Parser FilePath
identifiedFilePathParse name = lexeme $ rword name >> colon >> (stripF <$> p)
    where
        p = (:) <$> letterChar <*> manyTill printChar eol
        stripF = T.unpack . T.strip . T.pack


-- Parse a VEX file. 
experimentFileParse :: Parser VEXInvestigation
experimentFileParse = sc >> (,)
    <$> (identifiedFilePathParse "DMMSFile")
    <*> (some layerParameterParse >>= layerExpsCheck)
    <*  eof

-- Checks on the validity of the parsed VEXLayerExpSpecs. 
layerExpsCheck :: [VEXLayerExpSpec] -> Parser [VEXLayerExpSpec]
layerExpsCheck lExSpcs = layerDupeCheck lExSpcs

-- Check that no two VEXLayerExpSpecs refer to the same layer. 
layerDupeCheck :: [VEXLayerExpSpec] -> Parser [VEXLayerExpSpec]
layerDupeCheck lExSpcs
    | L.null repeats = return lExSpcs
    | otherwise = fail $ show $ DuplicatedLayerNames repeats
    where repeats =  (repeated . fmap vexLayerName) lExSpcs

-- Parse a VEXLayerExpSpecs
layerParameterParse :: Parser VEXLayerExpSpec
layerParameterParse = between (symbol "LayerParameters{")
                              (symbol "LayerParameters}")
    (runPermutation $
        VEXLayerExpSpec <$> toPermutation (identifier "LayerName")
                        <*> toPermutation samplingParse
                        <*> toPermutationWithDefault Nothing
                            (Just <$> inputSpaceDiagramParse)
                        <*> toPermutation (many experimentParse)
    )

-- Parse a Sampling
samplingParse :: Parser Sampling
samplingParse = between (symbol "Sampling{")
                        (symbol "Sampling}")
    (runPermutation $
        (,,) <$> toPermutationWithDefault Nothing (Just <$> readSParse)
             <*> toPermutationWithDefault Nothing (Just <$> sampleSParse)
             <*> toPermutationWithDefault [] limitedInputsSParse
    ) >>= samplingParameterMerge

samplingParameterMerge :: ( Maybe FilePath
                          , Maybe (Int, Int, Probability)
                          , [(NodeName, [Int])])
                       -> Parser Sampling
samplingParameterMerge (Nothing, Nothing, _) =
    fail "A Sampling{} must have either Read:, or Sample:, or both. "
samplingParameterMerge (Nothing, Just (rN, nN, nP), limitedIs) = return $
    SampleOnly $ SamplingParameters rN nN nP limitedIs
samplingParameterMerge (Just f, Nothing, _) = return $ ReadOnly f
samplingParameterMerge (Just f, Just (rN, nN, nP), limitedIs) = return $
    ReadAndSample (SamplingParameters rN nN nP limitedIs) f

readSParse :: Parser FilePath
readSParse = identifiedFilePathParse "Read"

sampleSParse :: Parser (Int, Int, Probability)
sampleSParse = (rword "Sample" >> colon) >> do
    rSts <- integer
    nSts <- integer
    nProb <- probParse
    return (rSts, nSts, nProb)

-- Parse a probability
probParse :: Parser Probability
probParse = do
    num <- number
    case mkProb (toRealFloat num) of
        Just p -> return p
        Nothing -> fail $ show num ++ " is not between 0 and 1, and so \
            \cannot be a probability"

    

limitedInputsSParse :: Parser [(NodeName, [Int])]
limitedInputsSParse = rword "LimitedInputs" >> colon >>
                        (limitedParse `sepBy1` comma)
    where
        limitedParse :: Parser (NodeName, [Int])
        limitedParse = (,) <$> variable <*> (colon >> intsParse)
        intsParse = parens (integer `sepBy1` comma)
            

-- Parse an ISFSpec. InputSpaceDiagrams are optional, so if this parser
-- succeeds, we wrap the result in a Just. 
inputSpaceDiagramParse :: Parser ISFSpec
inputSpaceDiagramParse = between (symbol "InputSpaceDiagram{")
                                 (symbol "InputSpaceDiagram}")
    (runPermutation $ ISFSpec
         <$> toPermutationWithDefault [] (multiIdentifier "AxesOrder")
         <*> toPermutationWithDefault Nothing (Just <$> barCodeFilterParse)
         <*> toPermutationWithDefault [] (coordParse "PinnedInputs")
                  
    )

-- Parse a BarcodeFilter. BarcodeFilters are optional, so if this parser
-- succeeds, we wrap the result in a Just. 
barCodeFilterParse :: Parser BarcodeFilter
barCodeFilterParse =  onlyBCwAnyParse
                  <|> onlyBCWAllParse
                  <|> excludeBCwAnyParse
                  <|> excludeBCwAllParse

onlyBCwAnyParse :: Parser BarcodeFilter
onlyBCwAnyParse = OnlyBarCodesWithAny <$>
    ((lexeme . try) $ rword "OnlyBarCodesWithAny" >> colon >>
        (sepBy1 phenotypeSetParse comma)
    )

onlyBCWAllParse :: Parser BarcodeFilter
onlyBCWAllParse = OnlyBarCodesWithAny <$>
    ((lexeme . try) $ rword "OnlyBarCodesWithAll" >> colon >>
        (sepBy1 phenotypeSetParse comma)
    )

excludeBCwAnyParse :: Parser BarcodeFilter
excludeBCwAnyParse = ExcludeBarCodesWithAny <$>
    ((lexeme . try) $ rword "ExcludeBarCodesWithAny" >> colon >>
        (sepBy1 phenotypeSetParse comma)
    )

excludeBCwAllParse :: Parser BarcodeFilter
excludeBCwAllParse = ExcludeBarCodesWithAll <$>
    ((lexeme . try) $ rword "ExcludeBarCodesWithAll" >> colon >>
        (sepBy1 phenotypeSetParse comma)
    )

-- Parse a switch set to a particular phenotype
phenotypeSetParse :: Parser (NodeName, PhenotypeName)
phenotypeSetParse = (,) <$> variable <*> (colon >> variable)

-- Parse a VEXTimeCourse. 
experimentParse :: Parser VEXExperiment
experimentParse = (VXTC <$> timecourseParse)
              <|> (TXSC <$> scanParse)

timecourseParse :: Parser VEXTimeCourse
timecourseParse = generalTCParse
              <|> pulse1Parse
              <|> kdoeTCParse
              <|> kdoeAtTransitionParse

-- Parse a general TimeCourse. 
generalTCParse :: Parser VEXTimeCourse
generalTCParse = between (symbol "GeneralExperiment{")
                          (symbol "GeneralExperiment}")
    (runPermutation $
        GeneralTC <$> toPermutation (identifier "ExperimentName")
                  <*> toPermutation startingModelStateParse
                  <*> toPermutation modelStepTypeParse
                  <*> toPermutation (some pulseParse)
                  <*> toPermutationWithDefault 1 expRepsParse
                  <*> toPermutationWithDefault defFigKinds figKindsParse
                  <*> toPermutationWithDefault Nothing manualPRNGParse
        
    )

-- Parse an InitialEnvironment. When we go to run the actual experiment, this
-- will be transformed into a [LayerVec]. 
startingModelStateParse :: Parser InitialEnvironment
startingModelStateParse = between (symbol "StartingModelState{")
                                  (symbol "StartingModelState}")
    (runPermutation $
        InEnv <$> toPermutation (coordParse "InputCoordinate")
              <*> toPermutationWithDefault Nothing
                                   (Just <$> barCodeFilterParse)
    )

modelStepTypeParse :: Parser ExperimentStep
modelStepTypeParse = lexeme $ rword "ModelStepType" >>
    (try
        (colon >>
            (   (SynchronousExpStepper <$ rword "Synchronous")
            <|> (NoisyExpStepper <$> ((rword "Noisy") >> probParse))
            <|> (AsynchronousExpStepper <$ rword "Asynchronous")
            )
        )
    )

pulseParse :: Parser VEXInputPulse
pulseParse = between (symbol "Pulse{") (symbol "Pulse}")
    (runPermutation $
        VEXInPt <$> toPermutation (realCoordParse "InputFix")
                <*> toPermutationWithDefault [] nodeAlterationsParse
                <*> (toPermutation (durationParse "Duration"))
        
    )

coordParse :: T.Text -> Parser [(NodeName, NodeState)]
coordParse rw = rword rw >> colon >>
    (sepBy1 
        ((,) <$> variable <*> cInt)
     comma)

realCoordParse :: T.Text -> Parser [(NodeName, RealNodeState)]
realCoordParse rw = rword rw >> colon >>
    (sepBy1 
        ((,) <$> variable <*> (colon >> (toRealFloat <$> number)))
     comma)

manualScanNameParse :: Parser (Maybe ScanName)
manualScanNameParse = Just <$> (identifier "ScanName")

nodeAlterationsParse :: Parser [NodeAlteration]
nodeAlterationsParse = between (symbol "NodeAlterations{")
                               (symbol "NodeAlterations}")
                               (many nodeAlterationParse)

nodeAlterationParse :: Parser NodeAlteration
nodeAlterationParse = try nodeLockParse <|> gradientNudgeParse

nodeLockParse :: Parser NodeAlteration
nodeLockParse = NodeLock <$> variable
                         <*> cInt
                         <*> (comma >> probParse)

gradientNudgeParse :: Parser NodeAlteration
gradientNudgeParse = GradientNudge <$> variable
                                   <*> (colon >> nDirectionParse)
                                   <*> (comma >> probParse)

nDirectionParse :: Parser NudgeDirection
nDirectionParse = (try (NudgeUp <$ rword "Up")) <|> (NudgeDown <$ rword "Down")

durationParse :: T.Text -> Parser Duration
durationParse tag = UserD <$> ((rword tag) >> colon >> integer)

-- How many times do we run this experiment?
expRepsParse :: Parser ExperimentReps
expRepsParse = rword "SampleSize" >> colon >> integer

-- What kinds of figures do we produce? This parses the kinds of figures
-- that will be made from single experiments, as opposed to a 5-D figure, which
-- you only make one of per ModelMapping. 
figKindsParse :: Parser FigKinds
figKindsParse = between (symbol "Figures{") (symbol "Figures}")
    (try (runPermutation $ FigKinds
        <$> toPermutationWithDefault True nodeTimeCourseParse
        <*> toPermutationWithDefault False phTimeCourseParse
        <*> toPermutationWithDefault [] (multiIdentifier "AvgBarChartNodes")
        <*> toPermutationWithDefault [] (multiIdentifier "AvgBarChartSwitches")
    ))

-- Do we produce a Node time course figure? Default to True. 
nodeTimeCourseParse :: Parser DoNodeTimeCourse
nodeTimeCourseParse = boolVariableParse "NodeTimeCourse"

-- Do we produce a Phenotype time course figure? Default to False. 
phTimeCourseParse :: Parser DoPhenotypeTimeCourse
phTimeCourseParse = boolVariableParse "PhenotypeTimeCourse"

manualPRNGParse :: Parser (Maybe Int)
manualPRNGParse = Just <$> (lexeme $ rword "ManualPRNGSeed" >>
    (try (colon >> signedInteger)))

-- Parse a Pulse1. t_0 and t_end are optional, so they get the default values
-- of: t_0 = 50, t_end = 50
pulse1Parse :: Parser VEXTimeCourse
pulse1Parse = between (symbol "Pulse1{") (symbol "Pulse1}")
    (runPermutation $
        Pulse1 <$> ((,) <$> toPermutationWithDefault (DefaultD 50)
                               (durationParse "t_0")
                        <*> toPermutationWithDefault (DefaultD 50)
                               (durationParse "t_end")
                   )
               <*> toPermutation startingModelStateParse
               <*> toPermutation (durationParse "Duration")
               <*> toPermutation flipParse
               <*> toPermutationWithDefault 1 expRepsParse
               <*> toPermutationWithDefault defFigKinds figKindsParse
               <*> toPermutationWithDefault Nothing manualPRNGParse
        
    )

flipParse :: Parser (NodeName, RealNodeState)
flipParse = rword "FlipTo" >> colon >> (,) <$> variable <*>
            (comma >> (toRealFloat <$> number))

-- Parse a KDOE. t_0 and t_end are optional, so they get the default values
-- of: t_0 = 50, t_end = 50
kdoeTCParse :: Parser VEXTimeCourse
kdoeTCParse = between (symbol "KDOE{") (symbol "KDOE}")
    (runPermutation $
        KnockDOverE <$>
                 ((,) <$> toPermutationWithDefault (DefaultD 50)
                           (durationParse "t_0")
                      <*> toPermutationWithDefault (DefaultD 50)
                           (durationParse "t_end")
                           )
                       <*> toPermutation startingModelStateParse
                       <*> toPermutation (durationParse "Duration")
                       <*> toPermutation nodeAlterationsParse
                       <*> toPermutationWithDefault 1 expRepsParse
                       <*> toPermutationWithDefault defFigKinds figKindsParse
                       <*> toPermutationWithDefault Nothing manualPRNGParse
        
    )

-- Parse a KDOEAtTransition. t_0 and t_end are optional, so they get the default
-- values of: t_0 = 50, t_end = 50
kdoeAtTransitionParse :: Parser VEXTimeCourse
kdoeAtTransitionParse = between (symbol "KDOEAtTransition{")
                                (symbol "KDOEAtTransition}")
    (runPermutation $
        KDOEAtTransition <$>
                 ((,) <$> toPermutationWithDefault (DefaultD 50)
                           (durationParse "t_0")
                      <*> toPermutationWithDefault (DefaultD 50)
                           (durationParse "t_end")
                           )
                       <*> toPermutation startingModelStateParse
                       <*> toPermutation (durationParse "Duration")
                       <*> toPermutation flipParse
                       <*> toPermutation nodeAlterationsParse
                       <*> toPermutationWithDefault 1 expRepsParse
                       <*> toPermutationWithDefault defFigKinds figKindsParse
                       <*> toPermutationWithDefault Nothing manualPRNGParse
        
    )

-- Parse a VEXScan
scanParse :: Parser VEXScan
scanParse = (between (symbol "Scan{") (symbol "Scan}") (
    runPermutation $
        VEXScan <$> toPermutation scanKindParse
                <*> toPermutation startingModelStateParse
                <*> toPermutationWithDefault Nothing manualScanNameParse
                <*> toPermutationWithDefault [] nodeAlterationsParse
                <*> toPermutationWithDefault [] (realCoordParse "InputFix")
                <*> toPermutation maxTParse
                <*> toPermutation relevantTParse
                <*> toPermutation stopPhParse
                <*> toPermutation modelStepTypeParse
                <*> ((,) <$> toPermutationWithDefault []
                                            (multiIdentifier "ScanSwitches")
                         <*> toPermutationWithDefault []
                                            (multiIdentifier "ScanNodes")
                    )
    )) >>= scanCheck

scanKindParse :: Parser ScanKind
scanKindParse = (EnvSc <$> envScanParse)
            <|> (KDOESc <$> kdoeScanParse)
            <|> envKDOEScanParse
            <|> twoDEnvScanParse
            <|> threeDEnvScanParse

envScanParse :: Parser EnvScan
envScanParse = lexeme $ rword "EnvironmentalScan" >> (try (colon >>
    (   (try stepSpecifiedEnvScanParse)
    <|> (try rangedEnvScanParse)
    <|> wholeEnvScanParse
    )))
    where
        stepSpecifiedEnvScanParse = StepSpecESC
            <$> variable
            <*> (colon >> (brackets ((toRealFloat <$> number) `sepBy1` comma)))
        rangedEnvScanParse = RangeESC <$> variable
                                      <*> (colon >> (toRealFloat <$> number))
                                      <*> (comma >> (toRealFloat <$> number))
                                      <*> (comma >> integer)
        wholeEnvScanParse = WholeESC <$> variable <*> cInt

kdoeScanParse :: Parser KDOEScan
kdoeScanParse = lexeme $ rword "KDOEScan" >> (try (colon >>
    (   (try stepSpecifiedKDOEScanParse)
    <|> (try wholeKDOEScanParse)
    <|> rangedKDOEScanParse
    )))
    where
        stepSpecifiedKDOEScanParse = StepSpecKDOESC
            <$> kdoeSCNodesParse
            <*> (colon >> (brackets (probParse `sepBy1` comma)))
        wholeKDOEScanParse = WholeKDOESC <$> kdoeSCNodesParse
                                         <*> cInt
        rangedKDOEScanParse = RangeKDOESC <$> kdoeSCNodesParse
                                          <*> (colon >> probParse)
                                          <*> (comma >> probParse)
                                          <*> (comma >> integer)
        kdoeSCNodesParse = parens (sepBy1 ((,) <$> variable
                                               <*> (colon >> integer)) comma)
    
xAxisParse :: Parser XAxis
xAxisParse = lexeme $ rword "X_Axis" >>
    (colon >>
        (   EnvX <$ rword "EnvX"
        <|> KDOEX <$ rword "KDOEX"
        )
    )

scanCheck :: VEXScan -> Parser VEXScan
scanCheck (VEXScan scKnd inEnv mScanName nAlts iFix maxN relN stopPhs
    exStep plottingNs) =
    VEXScan <$> scanKindCheck nAlts scKnd
            <*> pure inEnv
            <*> pure mScanName
            <*> pure nAlts
            <*> pure iFix
            <*> pure maxN
            <*> pure relN
            <*> pure stopPhs
            <*> pure exStep
            <*> plottingNodeCheck plottingNs
    

scanKindCheck :: [NodeAlteration] -> ScanKind -> Parser ScanKind
scanKindCheck _ x@(EnvSc _) = return x
scanKindCheck nAlts (KDOESc kdoeSc) = KDOESc <$> (kdoeScanCheck nAlts kdoeSc)
scanKindCheck nAlts (EnvKDOEScan envScan kdoeScan xAx) =
    EnvKDOEScan <$> pure envScan
                <*> kdoeScanCheck nAlts kdoeScan
                <*> pure xAx
scanKindCheck nAlts (TwoDEnvScan envScan1 envScan2 overLayVs mKDOEScan)
    | envScansCheck envScan1 envScan2 =
        TwoDEnvScan <$> pure envScan1
                    <*> pure envScan2
                    <*> pure overLayVs
                    <*> traverse (kdoeScanCheck nAlts) mKDOEScan
    | otherwise = fail $ show $
        DuplicateEnvScanNodeInScan (envScanInputName envScan1)
scanKindCheck nAlts
  (ThreeDEnvScan envScan1 envScan2 envScan3 overLayVs vsNAlts) =
    case envScansCheck envScan1 envScan2 of
        False -> fail $ show $ DuplicateEnvScanNodeInScan
            (envScanInputName envScan1)
        True -> case envScansCheck envScan1 envScan3 of
            False -> fail $ show $ DuplicateEnvScanNodeInScan
                (envScanInputName envScan1)
            True -> case envScansCheck envScan2 envScan3 of 
                False -> fail $ show $ DuplicateEnvScanNodeInScan
                    (envScanInputName envScan2)
                True -> ThreeDEnvScan <$> pure envScan1
                                      <*> pure envScan2
                                      <*> pure envScan3
                                      <*> pure overLayVs
                                      <*> nAltsCh
                    where
                        nAltsCh
                            | (not . L.null) sharedNAlts = fail $ show $
                                KDOESharedNodesIn3DVSAndAlts sharedNAlts
                            | otherwise = pure vsNAlts
                                where
                                    sharedNAlts = L.intersect
                                        (nodeAltName <$> nAlts)
                                        (nodeAltName <$> vsNAlts)
    
kdoeScanCheck :: [NodeAlteration] -> KDOEScan -> Parser KDOEScan
kdoeScanCheck nAlts kdoeScan
    | sharedNAlts /= [] = fail $ show $
        KDOESharedNodesInSteppedAndAlts sharedNAlts
    | otherwise = return kdoeScan
    where
        sharedNAlts = L.intersect (nodeAltName <$> nAlts) (fst <$> locknodes)
        locknodes = kdoeScNLocks kdoeScan

envScansCheck :: EnvScan -> EnvScan -> Bool
envScansCheck envScan1 envScan2 = not
    (envScanInputName envScan1 == envScanInputName envScan2)

plottingNodeCheck :: PlottingNodes -> Parser PlottingNodes
plottingNodeCheck (scanSws, scanNds)
    | not ((scanSws == []) && (scanNds == [])) = pure (scanSws, scanNds)
    | otherwise = fail $ show $ EmptyScanSwitchesAndScanNodes

envKDOEScanParse :: Parser ScanKind
envKDOEScanParse = between (symbol "EnvKDOEScan{") (symbol "EnvKDOEScan}") (
    runPermutation $
        EnvKDOEScan <$> toPermutation envScanParse
                    <*> toPermutation kdoeScanParse
                    <*> toPermutation xAxisParse
    )

twoDEnvScanParse :: Parser ScanKind
twoDEnvScanParse = between (symbol "TwoDEnvScan{") (symbol "TwoDEnvScan}") (
    runPermutation $ TwoDEnvScan
        <$> toPermutation envScanParse
        <*> toPermutation envScanParse
        <*> toPermutationWithDefault False valuesOnHeatMapParse
        <*> toPermutationWithDefault Nothing (Just <$> kdoeScanParse)
    )

-- Do we overlay the values of a heat map onto the figure?
valuesOnHeatMapParse :: Parser Bool
valuesOnHeatMapParse = boolVariableParse "ValuesOnHeatMap"

threeDEnvScanParse :: Parser ScanKind
threeDEnvScanParse = between (symbol "ThreeDEnvScan{") (symbol "ThreeDEnvScan}")
    (runPermutation $ ThreeDEnvScan
        <$> toPermutation envScanParse
        <*> toPermutation envScanParse
        <*> toPermutation envScanParse
        <*> toPermutationWithDefault False valuesOnHeatMapParse
        <*> toPermutationWithDefault [] wildTypeVsMutantParse
    )

wildTypeVsMutantParse :: Parser [WildTypeVsMutantAlt]
wildTypeVsMutantParse = lexeme $ rword "WildTypeVsMutant" >>
    (colon >>
        (many (parens nodeAlterationParse))
    )

stopPhParse :: Parser [(NodeName, PhenotypeName)]
stopPhParse = lexeme $ rword "StopPhenotypes" >>
    (try
        (colon >>
            (sepBy ((,) <$> variable <*> (colon >> variable)) comma)
        )
    )

maxTParse :: Parser Int
maxTParse = (lexeme . try) $ rword "Max_T" >> cInt

relevantTParse :: Parser Int
relevantTParse = (lexeme . try) $ rword "Relevant_T" >> cInt

