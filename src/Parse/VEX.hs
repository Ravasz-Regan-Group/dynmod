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
import Data.List.Unique (repeated)
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

-- | 'number' parses an number in scientific format.

number :: Parser Scientific
number = lexeme LE.scientific

-- | 'integer' parses an Int. (Note that this does not parse signs!)

integer :: Parser Int
integer = lexeme LE.decimal


-- | 'colon' parses a colon.

colon :: Parser T.Text
colon = symbol ":"

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

-- Parse an assignment
variable :: Parser T.Text
variable = (lexeme . try) (p >>= check)
  where
    p = T.pack <$> ((:) <$> letterChar <*> (many (alphaNumChar <|> char '_')))
    check x = if x `elem` vexRWS
                then fail $ "Keyword " ++ show x ++ " cannot be a variable. "
                else return x

-- Parse an identified FilePath
identifiedFilePathParse :: T.Text -> Parser FilePath
identifiedFilePathParse name = lexeme $ rword name >> colon >> p
    where
        p = (:) <$> letterChar <*> manyTill printChar eol


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
    ) >>= showHiddenCheck

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
         <$> toPermutationWithDefault [] axesOrderParse
         <*> toPermutationWithDefault Nothing (Just <$> barCodeFilterParse)
         <*> toPermutationWithDefault [] (coordParse "PinnedInputs")
                  
    )

-- Parser the order of axes for the input space diagram. 
axesOrderParse :: Parser [NodeName]
axesOrderParse = lexeme $ rword "AxesOrder" >>
    (try
        (colon >>
            (sepBy1 variable comma
            )
        )
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

-- Parse a VEXExperiment. 
experimentParse :: Parser VEXExperiment
experimentParse =  generalExpParse
               <|> pulse1Parse
               <|> kdoeParse
               <|> kdoeAtTransitionParse

-- Parse a general experiment. 
generalExpParse :: Parser VEXExperiment
generalExpParse = between (symbol "GeneralExperiment{")
                          (symbol "GeneralExperiment}")
    (runPermutation $
        GeneralExp <$> toPermutation (identifier "ExperimentName")
                   <*> toPermutation startingModelStateParse
                   <*> toPermutation modelStepTypeParse
                   <*> toPermutation (some pulseParse)
                   <*> toPermutationWithDefault 1 expRepsParse
                   <*> toPermutation figKindsParse
        
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
              <*> toPermutation showHiddenParse
        
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

showHiddenParse :: Parser (Either Bool BarcodeFilter)
showHiddenParse = rword "ShowHiddenFromSpaceDiagram" >> colon >>
    (Left <$> boolParse)

boolParse :: Parser Bool
boolParse = (try (True <$ rword "True")) <|> (False <$ rword "False")

pulseParse :: Parser VEXInputPulse
pulseParse = between (symbol "Pulse{") (symbol "Pulse}")
    (runPermutation $
        VEXInPt <$> toPermutation (realCoordParse "InputFix")
                <*> toPermutation nodeAlterationsParse
                <*> (toPermutation (durationParse "Duration"))
        
    )

coordParse :: T.Text -> Parser [(NodeName, NodeState)]
coordParse rw = rword rw >> colon >>
    (sepBy1 
        ((,) <$> variable <*> (colon >> integer))
     comma)

realCoordParse :: T.Text -> Parser [(NodeName, RealNodeState)]
realCoordParse rw = rword rw >> colon >>
    (sepBy1 
        ((,) <$> variable <*> (colon >> (toRealFloat <$> number)))
     comma)

nodeAlterationsParse :: Parser [NodeAlteration]
nodeAlterationsParse = between (symbol "NodeAlterations{")
                               (symbol "NodeAlterations}")
                               (many nodeAlterationParse)

nodeAlterationParse :: Parser NodeAlteration
nodeAlterationParse = try nodeLockParse <|> gradientNudgeParse

nodeLockParse :: Parser NodeAlteration
nodeLockParse = NodeLock <$> variable
                         <*> (colon >> integer)
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
figKindsParse = runPermutation $
    FigKinds <$> toPermutationWithDefault True nodeTimeCourseParse
             <*> toPermutationWithDefault False phTimeCourseParse
             <*> toPermutationWithDefault [] nodeAvgBChartParse

-- Do we produce a Node time course figure? Default to True. 
nodeTimeCourseParse :: Parser DoNodeTimeCourse
nodeTimeCourseParse = rword "NodeTimeCourse" >> colon >> boolParse

-- Do we produce a Phenotype time course figure? Default to False. 
phTimeCourseParse :: Parser DoPhenotypeTimeCourse
phTimeCourseParse = rword "PhenotypeTimeCourse" >> colon >> boolParse

-- Which Nodes should we make average value bar charts for? Default to []
nodeAvgBChartParse :: Parser AvgBChartNodes
nodeAvgBChartParse = lexeme $ rword "AvgBarChartNodes" >>
    (try
        (colon >>
            (sepBy1 variable comma
            )
        )
    )

-- Parse a Pulse1. t_0 and t_end are optional, so they get the default values
-- of: t_0 = 50, t_end = 50
pulse1Parse :: Parser VEXExperiment
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
               <*> toPermutation figKindsParse
        
    )

flipParse :: Parser (NodeName, RealNodeState)
flipParse = rword "FlipTo" >> colon >> (,) <$> variable <*>
            (comma >> (toRealFloat <$> number))

-- Parse a KDOE. t_0 and t_end are optional, so they get the default values
-- of: t_0 = 50, t_end = 50
kdoeParse :: Parser VEXExperiment
kdoeParse = between (symbol "KDOE{") (symbol "KDOE}")
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
                       <*> toPermutation figKindsParse
        
    )

-- Parse a KDOEAtTransition. t_0 and t_end are optional, so they get the default
-- values of: t_0 = 50, t_end = 50
kdoeAtTransitionParse :: Parser VEXExperiment
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
                       <*> toPermutation figKindsParse
        
    )

-- In every experiment is a StartingModelState{}, whose
-- ShowHiddenFromSpaceDiagram parameter checks if we should trim Attractors from
-- the experiment's run. We check if that is the case and, if there is an
-- ISFSpec with a BarcodeFilter, we copy it to the
-- InitialEnvironment of each VEXExperiment in the VEXLayerExpSpec. This is a
-- hack, but ¯\_(ツ)_/¯
showHiddenCheck :: VEXLayerExpSpec -> Parser VEXLayerExpSpec
showHiddenCheck vexLExpSpec = case vexISpaceSpec vexLExpSpec of
    Nothing -> return vexLExpSpec
    Just vISFSpec -> case bcFilter vISFSpec of
        Nothing -> return vexLExpSpec
        Just bcF -> return $ vexLExpSpec {vexExperiments = newVexExps}
            where
                newVexExps = insertBCF bcF <$> (vexExperiments vexLExpSpec)

insertBCF :: BarcodeFilter -> VEXExperiment -> VEXExperiment
insertBCF bc (GeneralExp n inEnv es ps reps fs)
    | showHidden inEnv == Left False = GeneralExp n newInEnv es ps reps fs
    | otherwise = GeneralExp n inEnv es ps reps fs
    where newInEnv = inEnv {showHidden = Right bc} 
insertBCF bc (Pulse1 ts inEnv d f rs fs)
    | showHidden inEnv == Left False = Pulse1 ts newInEnv d f rs fs
    | otherwise = Pulse1 ts inEnv d f rs fs
    where newInEnv = inEnv {showHidden = Right bc}
insertBCF bc (KnockDOverE ts inEnv d alts reps fs)
    | showHidden inEnv == Left False = KnockDOverE ts newInEnv d alts reps fs
    | otherwise = KnockDOverE ts inEnv d alts reps fs
    where newInEnv = inEnv {showHidden = Right bc}
insertBCF bc (KDOEAtTransition ts inEnv d f alts rs fs)
    | showHidden inEnv == Left False =
        KDOEAtTransition ts newInEnv d f alts rs fs
    | otherwise = KnockDOverE ts inEnv d alts rs fs
    where newInEnv = inEnv {showHidden = Right bc}

