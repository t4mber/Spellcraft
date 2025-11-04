-- ADC-IMPLEMENTS: spellcraft-adc-005
module VHDL.CLI.Options
  ( -- * CLI Options
    CliOptions(..)
  , OutputFormat(..)
  , parseOptions
  ) where

import Options.Applicative

-- | CLI options
-- Contract: spellcraft-adc-005 Section: Interface
data CliOptions = CliOptions
  { optInputFiles :: [FilePath]
  , optVerbose :: Bool
  , optOutputFormat :: OutputFormat
  , optThreshold :: Int
  , optComponentLibrary :: Maybe FilePath
  , optStrictMode :: Bool
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

formatReader :: ReadM OutputFormat
formatReader = eitherReader $ \s -> case s of
  "human" -> Right HumanReadable
  "json" -> Right JSON
  "gcc" -> Right GCC
  _ -> Left $ "Invalid format: " <> s <> ". Use: human, json, or gcc"
