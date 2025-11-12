-- ADC-IMPLEMENTS: spellcraft-adc-005
-- ADC-IMPLEMENTS: <videomancer-integration-01>
module VHDL.CLI.Options
  ( -- * CLI Options
    CliOptions(..)
  , AnalysisMode(..)
  , OutputFormat(..)
  , parseOptions
  ) where

import Options.Applicative

-- | Analysis mode
-- Contract: spellcraft-adc-010 Section: Tool
data AnalysisMode
  = StandardMode
  | VideomancerMode
    { vmConfigFile :: Maybe FilePath
    , vmReportPath :: Maybe FilePath
    , vmStrictMode :: Bool
    }
  deriving (Show, Eq)

-- | CLI options
-- Contract: spellcraft-adc-005 Section: Interface
data CliOptions = CliOptions
  { optInputFiles :: [FilePath]
  , optVerbose :: Bool
  , optOutputFormat :: OutputFormat
  , optThreshold :: Int
  , optComponentLibrary :: Maybe FilePath
  , optStrictMode :: Bool
  , optAnalysisMode :: AnalysisMode
  } deriving (Show, Eq)

-- | Output formats
data OutputFormat
  = HumanReadable
  | JSON
  | GCC
  deriving (Show, Eq, Enum, Bounded)

-- | Parse CLI options
-- Contract: spellcraft-adc-005 Section: Interface
parseOptions :: IO CliOptions
parseOptions = execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "Craft correct hardware designs - verify physical constraints before synthesis"
     <> header "spellcraft - Safe Programmable Electronics Linting Library & Code Review Analysis Framework Tool" )

cliParser :: Parser CliOptions
cliParser = CliOptions
  <$> some (argument str (metavar "FILES..." <> help "VHDL (.vhd) or Clash (.hs) source files to analyze"))
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output")
  <*> option formatReader
      ( long "format"
     <> short 'f'
     <> metavar "FORMAT"
     <> value HumanReadable
     <> help "Output format: human, json, gcc" )
  <*> option auto
      ( long "threshold"
     <> short 't'
     <> metavar "N"
     <> value 4
     <> help "Combinatorial path complexity threshold (default: 4)" )
  <*> optional (strOption
      ( long "library"
     <> short 'l'
     <> metavar "FILE"
     <> help "Component library file (future feature)" ))
  <*> switch (long "strict" <> help "Treat warnings as errors")
  <*> analysisModeParser

-- | Parse analysis mode (standard or videomancer)
-- Contract: spellcraft-adc-010 Section: Tool
analysisModeParser :: Parser AnalysisMode
analysisModeParser =
  flag' VideomancerMode
    ( long "videomancer"
   <> help "Enable Videomancer mode: video hardware analysis with parameter validation" )
  <*> optional (strOption
      ( long "config"
     <> metavar "FILE"
     <> help "JSON parameter configuration file (for Videomancer mode)" ))
  <*> optional (strOption
      ( long "report"
     <> metavar "FILE"
     <> help "Output report file path (default: stdout)" ))
  <*> pure False  -- vmStrictMode handled by main --strict flag
  <|> pure StandardMode

formatReader :: ReadM OutputFormat
formatReader = eitherReader $ \s -> case s of
  "human" -> Right HumanReadable
  "json" -> Right JSON
  "gcc" -> Right GCC
  _ -> Left $ "Invalid format: " <> s <> ". Use: human, json, or gcc"
