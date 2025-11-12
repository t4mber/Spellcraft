{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- ADC-IMPLEMENTS: <videomancer-report-01>
-- Purpose: Generate comprehensive markdown reports for video hardware analysis
-- Contract: spellcraft-adc-010
module VHDL.Videomancer.Report
  ( -- * Report Types
    VideomancerReport(..)
  , ReportSummary(..)
  , ReportStatus(..)
  , AnalysisResult(..)
    -- * Report Generation
  , generateVideomancerReport
  , renderReport
    -- * Helper Functions
  , generateSummary
  , generateRecommendations
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.List (partition)
import VHDL.AST (VHDLDesign(..))
import VHDL.Analysis.ClockGraph (ClockGraph)
import VHDL.Videomancer.Config (ProgramConfig(..))
import VHDL.Videomancer.Validation
import VHDL.Videomancer.Constraint

-- | Overall report status
data ReportStatus
  = Pass              -- ^ All checks passed
  | PassWithWarnings  -- ^ Passed but has warnings
  | Fail              -- ^ Has errors
  deriving (Show, Eq, Generic)

instance ToJSON ReportStatus

-- | Report summary statistics
data ReportSummary = ReportSummary
  { rsFilesAnalyzed :: Int
  , rsParseSuccessRate :: Double
  , rsFrequencyViolations :: Int
  , rsParameterViolations :: Int
  , rsOverallStatus :: ReportStatus
  } deriving (Show, Eq, Generic)

instance ToJSON ReportSummary

-- | Analysis result placeholder
-- Note: This should eventually be imported from VHDL.Videomancer.Analysis
-- but for now we define a compatible version
data AnalysisResult = AnalysisResult
  { arViolations :: [Text]  -- Simplified for now
  , arClockSources :: Int
  } deriving (Show, Eq, Generic)

-- | Complete Videomancer report
data VideomancerReport = VideomancerReport
  { vrTimestamp :: UTCTime
  , vrSummary :: ReportSummary
  , vrParseResults :: [(FilePath, Bool)]  -- File path and success status
  , vrParameterValidation :: Either [ConstraintViolation] ValidationReport
  , vrRecommendations :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON VideomancerReport

-- | Generate a Videomancer analysis report
-- Contract: spellcraft-adc-010 Section: DataTransform
generateVideomancerReport :: [VHDLDesign]
                          -> AnalysisResult
                          -> Either [ConstraintViolation] ValidationReport
                          -> IO VideomancerReport
generateVideomancerReport designs analysis validation = do
  timestamp <- getCurrentTime

  let parseResults = map (\d -> (designSourceFile d, True)) designs
  let summary = generateSummary designs analysis validation
  let recommendations = generateRecommendations designs validation

  pure $ VideomancerReport
    { vrTimestamp = timestamp
    , vrSummary = summary
    , vrParseResults = parseResults
    , vrParameterValidation = validation
    , vrRecommendations = recommendations
    }

-- | Generate report summary
-- Contract: spellcraft-adc-010 Section: DataTransform
generateSummary :: [VHDLDesign]
                -> AnalysisResult
                -> Either [ConstraintViolation] ValidationReport
                -> ReportSummary
generateSummary designs analysis validation =
  let filesAnalyzed = length designs
      parseSuccessRate = if filesAnalyzed > 0
                           then (fromIntegral filesAnalyzed / fromIntegral filesAnalyzed) * 100
                           else 0
      freqViolations = length (arViolations analysis)
      paramViolations = case validation of
        Left violations -> length violations
        Right report -> length (vrViolations report)

      status = determineOverallStatus designs analysis validation

  in ReportSummary
    { rsFilesAnalyzed = filesAnalyzed
    , rsParseSuccessRate = parseSuccessRate
    , rsFrequencyViolations = freqViolations
    , rsParameterViolations = paramViolations
    , rsOverallStatus = status
    }

-- | Determine overall report status
determineOverallStatus :: [VHDLDesign]
                       -> AnalysisResult
                       -> Either [ConstraintViolation] ValidationReport
                       -> ReportStatus
determineOverallStatus designs analysis validation =
  let hasFreqViolations = not (null (arViolations analysis))
      hasParamErrors = case validation of
        Left violations -> any (\v -> cvSeverity v == Error) violations
        Right _ -> False
      hasWarnings = case validation of
        Left violations -> not (null violations)
        Right report -> not (null (vrViolations report))
  in
    if hasFreqViolations || hasParamErrors
      then Fail
      else if hasWarnings
        then PassWithWarnings
        else Pass

-- | Generate recommendations based on analysis
-- Contract: spellcraft-adc-010 Section: DataTransform
generateRecommendations :: [VHDLDesign]
                        -> Either [ConstraintViolation] ValidationReport
                        -> [Text]
generateRecommendations designs validation =
  let paramRecs = case validation of
        Left violations ->
          let (errors, warnings) = partition (\v -> cvSeverity v == Error) violations
          in
            if not (null errors)
              then ["Fix parameter constraint violations before deployment"]
              else if not (null warnings)
                then ["Review parameter warnings for potential issues"]
                else []
        Right report ->
          if vrStatus report == Valid
            then ["All parameter validations passed"]
            else ["Review parameter validation report for details"]

      parseRecs = if null designs
        then ["No VHDL files were successfully parsed - check parser compatibility"]
        else []
  in
    paramRecs <> parseRecs

-- | Render report as markdown
-- Contract: spellcraft-adc-010 Section: DataTransform
renderReport :: VideomancerReport -> Text
renderReport VideomancerReport{..} =
  let ReportSummary{..} = vrSummary
      timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" vrTimestamp

      statusEmoji = case rsOverallStatus of
        Pass -> "✅"
        PassWithWarnings -> "⚠️"
        Fail -> "❌"

      header = T.unlines
        [ "# Videomancer Analysis Report"
        , ""
        , "**Generated:** " <> T.pack timestamp
        , "**Tool:** Spellcraft v0.3.0 (Videomancer Mode)"
        , "**Analysis Mode:** Video Hardware with Parameter Validation"
        , ""
        , "## Summary"
        , ""
        , "- **Files Analyzed:** " <> T.pack (show rsFilesAnalyzed)
        , "- **Parse Success Rate:** " <> T.pack (show (round rsParseSuccessRate :: Int)) <> "%"
        , "- **Frequency Violations:** " <> T.pack (show rsFrequencyViolations)
        , "- **Parameter Violations:** " <> T.pack (show rsParameterViolations)
        , "- **Overall Status:** " <> statusEmoji <> " " <> T.pack (show rsOverallStatus)
        , ""
        ]

      parseSection = T.unlines
        [ "## Parse Results"
        , ""
        , "### Successfully Parsed (" <> T.pack (show (length vrParseResults)) <> " files)"
        , ""
        ] <> T.unlines (map formatParseResult vrParseResults)

      paramSection = case vrParameterValidation of
        Left violations -> T.unlines
          [ "## Parameter Validation"
          , ""
          , "**Status:** ❌ FAILED"
          , ""
          , "### Violations (" <> T.pack (show (length violations)) <> ")"
          , ""
          ] <> T.unlines (map formatViolation violations)

        Right report -> T.unlines
          [ "## Parameter Validation"
          , ""
          , "**Status:** " <> formatValidationStatus (vrStatus report)
          , "**Parameters Checked:** " <> T.pack (show (vrParametersChecked report))
          , "**Constraints Found:** " <> T.pack (show (vrConstraintsFound report))
          , ""
          ] <> if null (vrViolations report)
                 then "✅ No violations found\n"
                 else T.unlines (map formatViolation (vrViolations report))

      recommendationSection = if null vrRecommendations
        then ""
        else T.unlines
          [ "## Recommendations"
          , ""
          ] <> T.unlines (map (\r -> "- " <> r) vrRecommendations)
  in
    header <> parseSection <> "\n" <> paramSection <> "\n" <> recommendationSection

-- | Format parse result as markdown
formatParseResult :: (FilePath, Bool) -> Text
formatParseResult (path, success) =
  let emoji = if success then "✅" else "❌"
      fileName = T.pack $ reverse $ takeWhile (/= '/') $ reverse path
  in emoji <> " " <> fileName

-- | Format constraint violation as markdown
formatViolation :: ConstraintViolation -> Text
formatViolation violation =
  let severityEmoji = case cvSeverity violation of
        Error -> "❌"
        Warning -> "⚠️"
        Info -> "ℹ️"
  in T.unlines
    [ severityEmoji <> " **" <> cvParameter violation <> "**: " <> cvMessage violation
    , "   - Type: " <> T.pack (show (cvViolationType violation))
    ]

-- | Format validation status
formatValidationStatus :: ValidationStatus -> Text
formatValidationStatus status = case status of
  Valid -> "✅ VALID"
  Invalid -> "❌ INVALID"
  ValidWithWarnings -> "⚠️ VALID (with warnings)"
  Skipped -> "⏭️ SKIPPED"
