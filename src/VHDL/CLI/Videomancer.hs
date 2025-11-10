{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- ADC-IMPLEMENTS: <videomancer-cli-01>
-- Purpose: Command-line interface for Videomancer video hardware analysis mode
-- Contract: spellcraft-adc-010
module VHDL.CLI.Videomancer
  ( -- * CLI Types
    VideomancerOptions(..)
  , runVideomancer
  ) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath.Glob (glob)

import VHDL.Parser (parseVHDLFile)
import VHDL.Videomancer.Config (loadProgramConfig, ProgramConfig)
import VHDL.Videomancer.Analysis (analyzeVideoHardware)
import qualified VHDL.Videomancer.Analysis as Analysis
import VHDL.Videomancer.Validation (validateParameterSoundness, ValidationMode(..), ValidationReport(..), ValidationStatus(..))
import VHDL.Videomancer.Report (generateVideomancerReport, renderReport, AnalysisResult(..))

-- | Videomancer CLI options
-- Contract: spellcraft-adc-010 Section: Tool
data VideomancerOptions = VideomancerOptions
  { voInputFiles :: [FilePath]       -- ^ VHDL files to analyze
  , voConfigFile :: Maybe FilePath   -- ^ JSON parameter config
  , voReportPath :: Maybe FilePath   -- ^ Output report path
  , voStrictMode :: Bool             -- ^ Fail on any violation
  , voVerbose :: Bool                -- ^ Verbose output
  } deriving (Show, Eq, Generic)

-- | Run Videomancer analysis
-- Contract: spellcraft-adc-010 Section: Tool
runVideomancer :: VideomancerOptions -> IO ExitCode
runVideomancer opts@VideomancerOptions{..} = do
  when voVerbose $
    putStrLn "🎬 Videomancer: Video Hardware Analysis Mode"

  -- Expand glob patterns in input files
  expandedFiles <- concat <$> mapM glob voInputFiles

  when (null expandedFiles) $ do
    putStrLn "❌ Error: No VHDL files found"
    exitWith (ExitFailure 1)

  when voVerbose $
    putStrLn $ "📁 Found " <> show (length expandedFiles) <> " VHDL files"

  -- Parse VHDL files
  parseResults <- forM expandedFiles $ \file -> do
    when voVerbose $
      putStrLn $ "   Parsing: " <> file
    parseVHDLFile file

  -- Separate successes and failures
  let (errors, designs) = partitionEithers parseResults

  -- Report parse errors
  unless (null errors) $ do
    putStrLn $ "⚠️ Parse errors in " <> show (length errors) <> " files:"
    mapM_ (putStrLn . ("   - " <>) . show) errors

  when (null designs) $ do
    putStrLn "❌ Error: No files parsed successfully"
    exitWith (ExitFailure 1)

  when voVerbose $
    putStrLn $ "✅ Successfully parsed " <> show (length designs) <> " files"

  -- Load parameter config if provided
  maybeConfig <- case voConfigFile of
    Nothing -> pure Nothing
    Just configPath -> do
      when voVerbose $
        putStrLn $ "📋 Loading config: " <> configPath
      result <- loadProgramConfig configPath
      case result of
        Left err -> do
          putStrLn $ "❌ Config load error: " <> show err
          exitWith (ExitFailure 1)
        Right config -> pure (Just config)

  -- Run analysis
  when voVerbose $
    putStrLn "🔍 Running hardware analysis..."

  analysisResultRaw <- analyzeVideoHardware designs maybeConfig

  -- Convert to Report's AnalysisResult type
  let analysisResult = AnalysisResult
        { arViolations = Analysis.arViolations analysisResultRaw
        , arClockSources = Analysis.arClockSources analysisResultRaw
        }

  -- Validate parameters if config provided
  validation <- case maybeConfig of
    Nothing -> do
      when voVerbose $
        putStrLn "⏭️ Skipping parameter validation (no config provided)"
      pure $ Right $ ValidationReport
        { vrStatus = Skipped
        , vrParametersChecked = 0
        , vrConstraintsFound = 0
        , vrViolations = []
        }
    Just config -> do
      when voVerbose $
        putStrLn "✓ Validating parameters..."
      let mode = if voStrictMode then Strict else Permissive
      pure $ validateParameterSoundness config designs mode

  -- Generate report
  report <- generateVideomancerReport designs analysisResult validation

  -- Output report
  let renderedReport = renderReport report
  case voReportPath of
    Nothing -> TIO.putStrLn renderedReport
    Just path -> do
      TIO.writeFile path renderedReport
      when voVerbose $
        putStrLn $ "📄 Report written to: " <> path

  -- Determine exit code
  case validation of
    Left violations | voStrictMode -> do
      putStrLn $ "❌ Validation failed with " <> show (length violations) <> " violations"
      pure $ ExitFailure 1
    Left violations -> do
      putStrLn $ "⚠️ Validation has " <> show (length violations) <> " warnings"
      pure ExitSuccess
    Right _ -> do
      when voVerbose $
        putStrLn "✅ Analysis complete"
      pure ExitSuccess

-- Helper functions

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()

unless :: Bool -> IO () -> IO ()
unless cond = when (not cond)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left  a (ls, rs) = (a:ls, rs)
    right a (ls, rs) = (ls, a:rs)
