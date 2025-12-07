{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-005
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
import System.IO (hFlush, stdout, stderr)
import Debug.Trace (trace, traceIO)
import VHDL.Analysis.Combinatorial (ComplexityWarning)
import VHDL.CLI.Color (green, red, yellow)
import VHDL.CLI.Format
  ( formatComplexityWarning
  , formatParseError
  , formatViolation
  )
import VHDL.CLI.Options (CliOptions(..), OutputFormat(..))
import VHDL.Constraint.Types (ConstraintViolation(..))
import VHDL.Constraint.Library (ComponentLibrary)
import VHDL.Parser (ParseError, parseVHDLFile)
import ComponentLibs.TestComponents (testComponentLibrary)
import VHDL.Analysis.ClockGraph (buildClockGraph, ClockGraph(..), cgNodes, cgEdges, cgSources, ClockNode(..), ClockEdge(..), ClockSource(..))
import VHDL.Analysis.Propagation (propagateFrequencies)
import VHDL.Analysis.Violation (detectFrequencyViolations)
import VHDL.Analysis.ClashFile (analyzeClashFile, ClashAnalysisResult(..), ClashViolation(..), clashViolationToConstraint)
import VHDL.Analysis.SignalUsage (analyzeSignalUsage, SignalViolation(..), violationSignal, violationLocation, violationType)
import VHDL.Analysis.ControlFlow (analyzeControlFlow, ControlFlowViolation(..), latchSignal, latchLocation, latchDescription)
import VHDL.Analysis.ArithmeticBounds (checkArithmeticBounds, ArithmeticViolation(..))
import VHDL.AST (VHDLDesign, designArchitectures)
import VHDL.SourceLocation (mkSourceLocation, SourceLocation(..))
import System.FilePath (takeExtension)
import qualified Data.Map.Strict as Map

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
  putStrLn "=== runAnalysis CALLED ===" >> hFlush stdout
  when (optVerbose opts) $
    TIO.putStrLn "[INFO] Starting hardware design analysis..."

  -- Separate files by type
  let (vhdlFiles, clashFiles) = partitionFiles (optInputFiles opts)

  putStrLn ("\n=== Analyzing Files: " ++ show (optInputFiles opts)) >> hFlush stdout
  putStrLn ("VHDL files: " ++ show (length vhdlFiles) ++ ", Clash files: " ++ show (length clashFiles)) >> hFlush stdout

  -- Parse VHDL files
  vhdlResults <- mapM parseVHDLFile vhdlFiles
  let parseErrors = [err | Left err <- vhdlResults]
  let designs = [design | Right design <- vhdlResults]

  -- Analyze Clash files
  clashResults <- mapM analyzeClashFile clashFiles
  let clashViolations = concatMap extractClashViolations clashResults

  putStrLn ("Parse results - VHDL errors: " ++ show (length parseErrors) ++
            ", designs: " ++ show (length designs) ++
            ", Clash violations: " ++ show (length clashViolations)) >> hFlush stdout

  -- Run constraint analysis on VHDL designs
  -- Contract: spellcraft-adc-005 Section: Interface
  let vhdlViolations = if null designs
        then []
        else analyzeDesigns designs testComponentLibrary

  let allViolations = vhdlViolations ++ clashViolations
  let warnings = []    -- Future: detectComplexityWarnings designs (optThreshold opts)

  let report = AnalysisReport
        { reportFiles = optInputFiles opts
        , reportParseErrors = parseErrors
        , reportViolations = allViolations
        , reportWarnings = warnings
        , reportSuccess = null parseErrors && null allViolations && (not (optStrictMode opts) || null warnings)
        }

  generateReport opts report

  pure $ if reportSuccess report then ExitSuccess else ExitFailure 1
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
-- Contract: spellcraft-adc-005 Section: Interface
analyzeDesigns :: [VHDLDesign] -> ComponentLibrary -> [ConstraintViolation]
analyzeDesigns designs lib = concatMap analyzeDesign designs
  where
    analyzeDesign design =
      trace ("\n=== Starting Design Analysis ===") $
      -- Signal usage analysis (ADC-012 Priority 1)
      let signalViolations = concatMap analyzeSignalUsage (designArchitectures design)
          signalConstraints = map signalViolationToConstraint signalViolations
          -- Control flow analysis (ADC-012 Priority 2)
          controlViolations = concatMap analyzeControlFlow (designArchitectures design)
          controlConstraints = map controlFlowViolationToConstraint controlViolations
          -- Arithmetic bounds analysis (ADC-012 Priority 3)
          arithmeticViolations = concatMap checkArithmeticBounds (designArchitectures design)
          arithmeticConstraints = map arithmeticViolationToConstraint arithmeticViolations
          -- All ADC-012 violations combined
          allAdc012Constraints = signalConstraints ++ controlConstraints ++ arithmeticConstraints
      in trace ("\n=== Signal Usage Analysis ===" ++
               "\nSignal violations: " ++ show (length signalViolations) ++
               "\nControl flow violations: " ++ show (length controlViolations) ++
               "\nArithmetic violations: " ++ show (length arithmeticViolations)) $
      case buildClockGraph design lib of
        Left err ->
          trace ("Graph building ERROR: " ++ show err) allAdc012Constraints
        Right clockGraph ->
          let sourceInfo = unlines [show s | s <- cgSources clockGraph]
              edgeInfo = unlines [show e | e <- cgEdges clockGraph]
              nodesBefore = unlines [show (cnSignal n, cnFrequency n) | n <- Map.elems (cgNodes clockGraph)]
          in trace ("\n=== Clock Graph Built ===" ++
                    "\nSources:\n" ++ sourceInfo ++
                    "\nEdges:\n" ++ edgeInfo ++
                    "\nNodes before propagation:\n" ++ nodesBefore) $
          case propagateFrequencies clockGraph lib of
            Left err ->
              trace ("Propagation error: " ++ show err) allAdc012Constraints
            Right propagatedGraph ->
              let nodesAfter = unlines [show (cnSignal n, cnFrequency n) | n <- Map.elems (cgNodes propagatedGraph)]
                  freqViolations = detectFrequencyViolations propagatedGraph lib
                  allViolations = allAdc012Constraints ++ freqViolations
              in trace ("\n=== After Propagation ===" ++
                       "\nNodes:\n" ++ nodesAfter ++
                       "\nViolations: " ++ show allViolations) allViolations

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
