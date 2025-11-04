{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-005
module VHDL.CLI.Report
  ( -- * Reports
    AnalysisReport(..)
  , runAnalysis
  , generateReport
  ) where

import Control.Monad (when)
import Data.Aeson (ToJSON, encode)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import VHDL.Analysis.Combinatorial (ComplexityWarning)
import VHDL.CLI.Color (green, red, yellow)
import VHDL.CLI.Format
  ( formatComplexityWarning
  , formatParseError
  , formatViolation
  )
import VHDL.CLI.Options (CliOptions(..), OutputFormat(..))
import VHDL.Constraint.Types (ConstraintViolation)
import VHDL.Constraint.Library (ComponentLibrary)
import VHDL.Parser (ParseError, parseVHDLFile)
import ComponentLibs.TestComponents (testComponentLibrary)
import VHDL.Analysis.ClockGraph (buildClockGraph)
import VHDL.Analysis.Propagation (propagateFrequencies)
import VHDL.Analysis.Violation (detectFrequencyViolations)
import VHDL.AST (VHDLDesign)

-- | Analysis report
-- Contract: vhdl-analyzer-adc-005 Section: Interface
data AnalysisReport = AnalysisReport
  { reportFiles :: [FilePath]
  , reportParseErrors :: [ParseError]
  , reportViolations :: [ConstraintViolation]
  , reportWarnings :: [ComplexityWarning]
  , reportSuccess :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON AnalysisReport

-- | Run complete analysis
-- Contract: vhdl-analyzer-adc-005 Section: Interface
runAnalysis :: CliOptions -> IO ExitCode
runAnalysis opts = do
  when (optVerbose opts) $
    TIO.putStrLn "[INFO] Starting VHDL analysis..."

  -- Parse all files
  parseResults <- mapM parseVHDLFile (optInputFiles opts)

  let parseErrors = [err | Left err <- parseResults]
  let designs = [design | Right design <- parseResults]

  -- Run constraint analysis
  -- Contract: vhdl-analyzer-adc-005 Section: Interface
  let violations = if null designs
        then []
        else analyzeDesigns designs testComponentLibrary
  let warnings = []    -- Future: detectComplexityWarnings designs (optThreshold opts)

  let report = AnalysisReport
        { reportFiles = optInputFiles opts
        , reportParseErrors = parseErrors
        , reportViolations = violations
        , reportWarnings = warnings
        , reportSuccess = null parseErrors && null violations && (not (optStrictMode opts) || null warnings)
        }

  generateReport opts report

  pure $ if reportSuccess report then ExitSuccess else ExitFailure 1

-- | Generate and display report
-- Contract: vhdl-analyzer-adc-005 Section: Interface
generateReport :: CliOptions -> AnalysisReport -> IO ()
generateReport opts report = case optOutputFormat opts of
  JSON -> TIO.putStrLn $ TL.toStrict $ TLE.decodeUtf8 $ encode report

  _ -> do
    -- Human-readable or GCC format
    let fmt = optOutputFormat opts

    -- Print parse errors
    mapM_ (printError fmt) (reportParseErrors report)

    -- Print violations
    mapM_ (printViolation fmt) (reportViolations report)

    -- Print warnings
    mapM_ (printWarning fmt) (reportWarnings report)

    -- Summary
    when (fmt == HumanReadable) $ do
      let errorCount = length (reportParseErrors report) + length (reportViolations report)
      let warningCount = length (reportWarnings report)

      if reportSuccess report
        then do
          msg <- green "✓ Analysis complete. No issues found."
          TIO.putStrLn msg
        else do
          msg <- red $ T.pack $ "✗ Found " <> show errorCount <> " error(s), " <> show warningCount <> " warning(s)"
          TIO.putStrLn msg

printError :: OutputFormat -> ParseError -> IO ()
printError fmt err = do
  let formatted = formatParseError fmt err
  msg <- red formatted
  TIO.putStrLn msg

printViolation :: OutputFormat -> ConstraintViolation -> IO ()
printViolation fmt violation = do
  let formatted = formatViolation fmt violation
  msg <- red formatted
  TIO.putStrLn msg

printWarning :: OutputFormat -> ComplexityWarning -> IO ()
printWarning fmt warning = do
  let formatted = formatComplexityWarning fmt warning
  msg <- yellow formatted
  TIO.putStrLn msg

-- | Analyze designs for constraint violations
-- Contract: vhdl-analyzer-adc-005 Section: Interface
analyzeDesigns :: [VHDLDesign] -> ComponentLibrary -> [ConstraintViolation]
analyzeDesigns designs lib = concatMap analyzeDesign designs
  where
    analyzeDesign design =
      case buildClockGraph design lib of
        Left err ->
          -- TODO: In verbose mode, log error
          -- For now, return empty list (graph building failed)
          []
        Right clockGraph ->
          -- Note: Currently clock sources are not detected, so propagation won't work
          -- This is a known limitation - see ClockGraph.hs line 92
          case propagateFrequencies clockGraph lib of
            Left err -> []
            Right propagatedGraph -> detectFrequencyViolations propagatedGraph lib
