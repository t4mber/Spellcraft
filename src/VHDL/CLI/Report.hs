{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-005
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
-- ADC-IMPLEMENTS: spellcraft-adc-029
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: Report Integration
module VHDL.CLI.Report
  ( -- * Reports
    AnalysisReport(..)
  , runAnalysis
  , generateReport
  ) where

import Control.Monad (when, unless)
import Data.Aeson (ToJSON, encode)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import VHDL.Analysis.Combinatorial (ComplexityWarning)
import VHDL.CLI.Color (green, red, yellow)
import VHDL.CLI.Format
  ( formatComplexityWarning
  , formatParseError
  , formatViolation
  )
import VHDL.CLI.Options (CliOptions(..), OutputFormat(..))
import VHDL.CLI.Export (exportJSON, exportSARIF)
import VHDL.Constraint.Types (ConstraintViolation(..), Severity(..), violationSeverity)
import VHDL.Constraint.Library (ComponentLibrary)
import VHDL.Parser (ParseError, parseVHDLFile)
import ComponentLibs.TestComponents (testComponentLibrary)
import VHDL.Analysis.ClockGraph (buildClockGraph)
import VHDL.Analysis.Propagation (propagateFrequencies)
import VHDL.Analysis.Violation (detectFrequencyViolations)
import VHDL.Analysis.ClashFile (analyzeClashFile, ClashAnalysisResult(..), ClashViolation(..), clashViolationToConstraint)
import VHDL.Analysis.SignalUsage (analyzeSignalUsageWithContext, SignalViolation(..), violationSignal, violationLocation, violationType)
import VHDL.Analysis.MultiFile (buildContext)
import VHDL.Analysis.ControlFlow (analyzeControlFlow, ControlFlowViolation(..), latchSignal, latchLocation, latchDescription)
import VHDL.Analysis.ArithmeticBounds (checkArithmeticBounds, ArithmeticViolation(..))
import VHDL.AST (VHDLDesign, designArchitectures)
import VHDL.SourceLocation (SourceLocation(..))
import System.FilePath (takeExtension)

-- | Analysis report
-- Contract: spellcraft-adc-005 Section: Interface
data AnalysisReport = AnalysisReport
  { reportFiles :: [FilePath]
  , reportParseErrors :: [ParseError]
  , reportViolations :: [ConstraintViolation]
  , reportWarnings :: [ComplexityWarning]
  , reportSuccess :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON AnalysisReport

-- | Run complete analysis
-- Contract: spellcraft-adc-005 Section: Interface
runAnalysis :: CliOptions -> IO ExitCode
runAnalysis opts = do
  when (optVerbose opts) $
    TIO.putStrLn "[INFO] Starting hardware design analysis..."

  -- Separate files by type
  let (vhdlFiles, clashFiles) = partitionFiles (optInputFiles opts)

  when (optVerbose opts) $ do
    putStrLn ("[INFO] Analyzing Files: " ++ show (optInputFiles opts)) >> hFlush stdout
    putStrLn ("[INFO] VHDL files: " ++ show (length vhdlFiles) ++ ", Clash files: " ++ show (length clashFiles)) >> hFlush stdout

  -- Parse VHDL files
  vhdlResults <- mapM parseVHDLFile vhdlFiles
  let parseErrors = [err | Left err <- vhdlResults]
  let designs = [design | Right design <- vhdlResults]

  -- Analyze Clash files
  clashResults <- mapM analyzeClashFile clashFiles
  let clashViolations = concatMap extractClashViolations clashResults

  when (optVerbose opts) $
    putStrLn ("[INFO] Parse results - VHDL errors: " ++ show (length parseErrors) ++
              ", designs: " ++ show (length designs) ++
              ", Clash violations: " ++ show (length clashViolations)) >> hFlush stdout

  -- Run constraint analysis on VHDL designs
  -- Contract: spellcraft-adc-005 Section: Interface
  let vhdlViolations = if null designs
        then []
        else analyzeDesigns designs testComponentLibrary

  let allViolations = vhdlViolations ++ clashViolations
  let warnings = []    -- Future: detectComplexityWarnings designs (optThreshold opts)

  -- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Exit Code Logic
  -- Classify violations by severity
  let errors = filter (\v -> violationSeverity v == SeverityError) allViolations
      warningViolations = filter (\v -> violationSeverity v == SeverityWarning) allViolations

  -- Success criteria:
  -- - No parse errors
  -- - No error-severity violations
  -- - If --strict: also no warning-severity violations
  let hasErrors = not (null parseErrors) || not (null errors)
      hasWarnings = not (null warningViolations) || not (null warnings)
      success = not hasErrors && (not (optStrictMode opts) || not hasWarnings)

  let report = AnalysisReport
        { reportFiles = optInputFiles opts
        , reportParseErrors = parseErrors
        , reportViolations = allViolations
        , reportWarnings = warnings
        , reportSuccess = success
        }

  generateReport opts report

  -- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Exit Code Logic
  -- Exit code 1 only if:
  -- - Parse errors exist, OR
  -- - Error-severity violations exist, OR
  -- - (--strict mode AND warning-severity violations exist)
  pure $ if success then ExitSuccess else ExitFailure 1
  where
    partitionFiles :: [FilePath] -> ([FilePath], [FilePath])
    partitionFiles files =
      let vhdl = filter (\f -> takeExtension f `elem` [".vhd", ".vhdl"]) files
          clash = filter (\f -> takeExtension f `elem` [".hs", ".lhs"]) files
      in (vhdl, clash)

    extractClashViolations :: Either String ClashAnalysisResult -> [ConstraintViolation]
    extractClashViolations (Left err) =
      -- Create a parse-like error for Clash analysis failures
      []  -- For now, silently skip errors
    extractClashViolations (Right result) =
      map clashViolationToConstraint (carViolations result)

-- | Generate and display report
-- Contract: spellcraft-adc-005 Section: Interface
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: Report Generation
generateReport :: CliOptions -> AnalysisReport -> IO ()
generateReport opts report = case optOutputFormat opts of
  JSON -> do
    -- ADC-IMPLEMENTS: spellcraft-adc-031 Section: JSON Export
    now <- getCurrentTime
    let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
    let jsonOutput = exportJSON
          timestamp
          (reportFiles report)
          (reportViolations report)
          (reportParseErrors report)
          (reportSuccess report)
    TIO.putStrLn jsonOutput

  SARIF -> do
    -- ADC-IMPLEMENTS: spellcraft-adc-031 Section: SARIF Export
    let sarifOutput = exportSARIF
          (reportViolations report)
          (reportParseErrors report)
    TIO.putStrLn sarifOutput

  _ -> do
    -- Human-readable or GCC format
    let fmt = optOutputFormat opts
    let suppressWarnings = optSuppressWarnings opts

    -- Print parse errors
    mapM_ (printError fmt) (reportParseErrors report)

    -- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
    -- Print violations with color based on severity
    mapM_ (printViolationBySeverity fmt suppressWarnings) (reportViolations report)

    -- Print warnings (ComplexityWarning type)
    unless suppressWarnings $
      mapM_ (printWarning fmt) (reportWarnings report)

    -- Summary
    -- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
    when (fmt == HumanReadable) $ do
      -- Separate counts for errors and warnings
      let violations = reportViolations report
          errorViolations = filter (\v -> violationSeverity v == SeverityError) violations
          warningViolations = filter (\v -> violationSeverity v == SeverityWarning) violations
          errorCount = length (reportParseErrors report) + length errorViolations
          warningCount = length warningViolations + length (reportWarnings report)

      if reportSuccess report && errorCount == 0 && warningCount == 0
        then do
          msg <- green "Analysis complete. No issues found."
          TIO.putStrLn msg
        else if errorCount == 0 && warningCount > 0
        then do
          -- Only warnings - yellow summary
          msg <- yellow $ T.pack $ "Analysis complete. Found " <> show warningCount <> " warning(s)"
          TIO.putStrLn msg
        else do
          -- Has errors - red summary
          msg <- red $ T.pack $ "Found " <> show errorCount <> " error(s), " <> show warningCount <> " warning(s)"
          TIO.putStrLn msg

printError :: OutputFormat -> ParseError -> IO ()
printError fmt err = do
  let formatted = formatParseError fmt err
  msg <- red formatted
  TIO.putStrLn msg

-- | Print violation with color based on severity
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
printViolationBySeverity :: OutputFormat -> Bool -> ConstraintViolation -> IO ()
printViolationBySeverity fmt suppressWarnings violation = do
  let severity = violationSeverity violation
      formatted = formatViolation fmt violation
  case severity of
    SeverityError -> do
      msg <- red formatted
      TIO.putStrLn msg
    SeverityWarning ->
      unless suppressWarnings $ do
        msg <- yellow formatted
        TIO.putStrLn msg
    SeverityInfo ->
      unless suppressWarnings $ do
        TIO.putStrLn formatted  -- No color for info

-- | Print violation (legacy - uses red for all)
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
-- Contract: spellcraft-adc-005 Section: Interface
-- ADC-IMPLEMENTS: spellcraft-adc-029
-- Now builds multi-file context from all designs for cross-file resolution
analyzeDesigns :: [VHDLDesign] -> ComponentLibrary -> [ConstraintViolation]
analyzeDesigns designs lib =
  -- ADC-029: Build multi-file analysis context from all designs
  let ctx = buildContext designs
  in concatMap (analyzeDesign ctx) designs
  where
    analyzeDesign ctx design =
      -- Signal usage analysis with multi-file context (ADC-012 Priority 1, ADC-029)
      let signalViolations = concatMap (analyzeSignalUsageWithContext ctx) (designArchitectures design)
          signalConstraints = map signalViolationToConstraint signalViolations
          -- Control flow analysis (ADC-012 Priority 2)
          controlViolations = concatMap analyzeControlFlow (designArchitectures design)
          controlConstraints = map controlFlowViolationToConstraint controlViolations
          -- Arithmetic bounds analysis (ADC-012 Priority 3)
          arithmeticViolations = concatMap checkArithmeticBounds (designArchitectures design)
          arithmeticConstraints = map arithmeticViolationToConstraint arithmeticViolations
          -- All ADC-012 violations combined
          allAdc012Constraints = signalConstraints ++ controlConstraints ++ arithmeticConstraints
      in case buildClockGraph design lib of
        Left _err ->
          allAdc012Constraints
        Right clockGraph ->
          case propagateFrequencies clockGraph lib of
            Left _err ->
              allAdc012Constraints
            Right propagatedGraph ->
              let freqViolations = detectFrequencyViolations propagatedGraph lib
              in allAdc012Constraints ++ freqViolations

-- | Convert signal usage violation to constraint violation
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
signalViolationToConstraint :: SignalViolation -> ConstraintViolation
signalViolationToConstraint violation =
  SignalUsageViolation
    { violationSignalName = violationSignal violation
    , violationDescription = violationType violation
    , violationLocation = VHDL.Analysis.SignalUsage.violationLocation violation
    }

-- | Convert control flow violation to constraint violation
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
controlFlowViolationToConstraint :: ControlFlowViolation -> ConstraintViolation
controlFlowViolationToConstraint violation =
  ControlFlowViolation
    { violationSignalName = latchSignal violation
    , violationDescription = latchDescription violation
    , violationLocation = latchLocation violation
    }

-- | Convert arithmetic violation to constraint violation
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
arithmeticViolationToConstraint :: ArithmeticViolation -> ConstraintViolation
arithmeticViolationToConstraint (UnboundedCounter sig loc desc) =
  ArithmeticBoundsViolation
    { violationSignalName = sig
    , violationDescription = desc
    , violationLocation = loc
    }
arithmeticViolationToConstraint (PotentialOverflow sig _ loc _ _ desc) =
  ArithmeticBoundsViolation
    { violationSignalName = sig
    , violationDescription = desc
    , violationLocation = loc
    }
