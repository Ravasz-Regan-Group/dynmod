{-# LANGUAGE OverloadedStrings #-}


module Input where    

import Paths_dynmod (version)
import Parse.UI
import Utilities
import qualified Data.Text as T
import qualified Options.Applicative as O
import qualified Text.Megaparsec as M
import Data.Version (showVersion)
import Control.Applicative ((<|>))

-- Option Parsing
data Input = Input {
      fileN    :: T.Text
    , activity :: Activity
    } deriving (Eq, Show)

data Activity = Update Updates
              | Compare CompareDMMS
              | Procedure Procedures
              | Experiment Experiments
              | Figure Figures
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

data Experiments = GridSearch (EParams, GParam)
                 | AttractorFind EParams
                 | VEX VexFile
                 deriving (Eq, Show)

type VexFile = T.Text

type Figures = T.Text -- The file to get an AttractorBundle from

-- The number of random states per input combination, noisy steps per random
-- state, and the probability that any particular gate will slip on a step. 
-- Eventually these will be folded into the grid option, as the --attractor
-- option will automatically search for attractors depending on how exhaustive
-- you tell it to be, but for now its manual. In the context of the --grid
-- option, the first two are the number of random state and noisy steps per
-- random state slots in the grid, and the multiplier in GParam will determine
-- what the actual numbers of random states and noisy steps are. 
data EParams = EParams {
      randomState :: Int
    , noisySteps :: Int
    , noiseProb :: Probability
    } deriving (Eq, Show)

-- The grid multiplier for the --grid option. U
type GParam = Int



input :: O.Parser Input
input = Input <$> O.strArgument
                  (O.metavar "FILE"
                  <> O.help "Path to a dmms file to parse")
              <*> activityParser

activityParser :: O.Parser Activity
activityParser =  procedureParser
              <|> updateParser
              <|> compareParser
              <|> experimentParser
              <|> figureParser

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
       <> O.short 'i'
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

experimentParser :: O.Parser Activity
experimentParser = Experiment <$> (attFindParser <|> gridParser <|> vexParser)

eParamReader :: O.ReadM Experiments
eParamReader = do
    o <- O.str
    case M.runParser eParamParse "" o of
        (Left epErr) -> do
            fail (M.errorBundlePretty epErr)
        (Right eP) -> return $ AttractorFind $ uncurry3 EParams eP
    

gParamReader :: O.ReadM Experiments
gParamReader = do
    o <- O.str
    case M.runParser gParamParse "" o of
        (Left gpErr) -> do
            fail (M.errorBundlePretty gpErr)
        (Right (eP, gP)) -> return $ GridSearch (uncurry3 EParams eP , gP)

attFindParser :: O.Parser Experiments
attFindParser = O.option eParamReader
                       ( O.long "attractors"
                      <> O.short 'a'
                      <> O.metavar "Int,Int,Double" 
                      <> O.help "Sample the state space of the fine\
                            \ layer of the dmms file in order to find its \
                            \attractors. "
                       )

gridParser :: O.Parser Experiments
gridParser = O.option gParamReader
                       ( O.long "grid"
                      <> O.metavar "Int,Int,Double,Int"
                      <> O.help "Sample the state space of the fine layer of \
                            \the dmms file repeatedly in a grid, and output \
                            \the results as a grid in order to determine how \
                            \quickly its attractors can be found. "
                       )

vexParser :: O.Parser Experiments
vexParser = VEX 
    <$> O.strOption
        ( O.long "experiment"
       <> O.short 'e'
       <> O.metavar "VEX_FILE"
       <> O.help "Whether to read in and run the experiments in a Virtual \
        \Experiment File (.vex) and write out the resulting figures to disk. "
        )

figureParser :: O.Parser Activity
figureParser = Figure <$> (fiveDFigParser)

fiveDFigParser :: O.Parser Figures
fiveDFigParser = O.strOption
                    ( O.long "figure"
                   <> O.short 'f'
                   <> O.metavar "FILE"
                   <> O.help "Create a 5-D figure illustrating which attractors\
                        \ exist at which environmental input values, and which \
                        \ phenotypes map best onto those attractors. "
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
   \another, write out tt files, find attractors and run simulations on the \
   \models of the dmms, create a 5_D figure of the attractors in plotted \
   \against the possible environments."
   <> O.header "dynmod - processing dmms files")
