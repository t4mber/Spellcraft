{-# OPTIONS_GHC -Wno-partial-fields #-}
-- | Partial fields are intentional - VideomancerMode has fields that StandardMode doesn't.

-- ADC-IMPLEMENTS: spellcraft-adc-005
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: CLI Flags
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
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: CLI Flags
data CliOptions = CliOptions
  { optInputFiles :: [FilePath]
  , optVerbose :: Bool
  , optOutputFormat :: OutputFormat
  , optThreshold :: Int
  , optComponentLibrary :: Maybe FilePath
  , optStrictMode :: Bool           -- ^ Treat warnings as errors (--strict or --warnings-as-errors)
  , optSuppressWarnings :: Bool     -- ^ Hide warning messages (--suppress-warnings)
  , optAnalysisMode :: AnalysisMode
  } deriving (Show, Eq)

-- | Output formats
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: CLI Flags
data OutputFormat
  = HumanReadable   -- ^ Human-readable colored output
  | JSON            -- ^ JSON format for CI/CD integration
  | GCC             -- ^ GCC-compatible format
  | SARIF           -- ^ SARIF 2.1.0 for IDE/GitHub integration
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
     <> long "output-format"  -- alias for compatibility
     <> metavar "FORMAT"
     <> value HumanReadable
     <> help "Output format: human|text, json, gcc, sarif" )
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
  -- ADC-IMPLEMENTS: spellcraft-adc-014 Section: CLI Flags
  <*> (switch (long "strict" <> help "Treat warnings as errors")
       <|> switch (long "warnings-as-errors" <> help "Treat warnings as errors (alias for --strict)"))
  <*> switch (long "suppress-warnings" <> help "Hide warning messages from output")
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
  "text"  -> Right HumanReadable  -- alias for human
  "json"  -> Right JSON
  "gcc"   -> Right GCC
  "sarif" -> Right SARIF
  _ -> Left $ "Invalid format: " <> s <> ". Use: human, json, gcc, or sarif"
