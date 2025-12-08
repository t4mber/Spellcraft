{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: Report Export
-- Purpose: Export analysis results in machine-readable formats for CI/CD and IDE integration
-- Contract: spellcraft-adc-031
module VHDL.CLI.Export
  ( -- * Export Functions
    exportJSON
  , exportSARIF
    -- * SARIF Types
  , SARIFReport(..)
  , SARIFRun(..)
  , SARIFTool(..)
  , SARIFToolDriver(..)
  , SARIFResult(..)
  , SARIFLocation(..)
  , SARIFPhysicalLocation(..)
  , SARIFArtifactLocation(..)
  , SARIFRegion(..)
  , SARIFMessage(..)
  , SARIFLevel(..)
    -- * JSON Export Types
  , JSONReport(..)
  , JSONViolation(..)
  , JSONParseError(..)
    -- * Conversion Functions
  , toSARIFLevel
  , violationToSARIF
  , parseErrorToSARIF
  ) where

import Data.Aeson (ToJSON(..), (.=), object, encode)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Data.Maybe (catMaybes)
import VHDL.Constraint.Types (ConstraintViolation(..), Severity(..), violationSeverity)
import VHDL.Parser (ParseError(..))
import VHDL.SourceLocation (SourceLocation(..))

--------------------------------------------------------------------------------
-- JSON Export Types
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: JSON Export
--------------------------------------------------------------------------------

-- | JSON export report structure
data JSONReport = JSONReport
  { jrVersion :: Text                   -- ^ Spellcraft version
  , jrTimestamp :: Text                 -- ^ ISO 8601 timestamp
  , jrFiles :: [Text]                   -- ^ Files analyzed
  , jrSummary :: JSONSummary            -- ^ Summary statistics
  , jrViolations :: [JSONViolation]     -- ^ List of violations
  , jrParseErrors :: [JSONParseError]   -- ^ List of parse errors
  } deriving (Show, Eq, Generic)

instance ToJSON JSONReport where
  toJSON JSONReport{..} = object
    [ "version" .= jrVersion
    , "timestamp" .= jrTimestamp
    , "files" .= jrFiles
    , "summary" .= jrSummary
    , "violations" .= jrViolations
    , "parseErrors" .= jrParseErrors
    ]

-- | Summary statistics for JSON report
data JSONSummary = JSONSummary
  { jsFilesAnalyzed :: Int
  , jsErrorCount :: Int
  , jsWarningCount :: Int
  , jsInfoCount :: Int
  , jsParseErrorCount :: Int
  , jsSuccess :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON JSONSummary

-- | JSON representation of a violation
data JSONViolation = JSONViolation
  { jvFile :: Text                      -- ^ Source file path
  , jvLine :: Int                       -- ^ Line number
  , jvColumn :: Int                     -- ^ Column number
  , jvSeverity :: Text                  -- ^ "error", "warning", or "info"
  , jvType :: Text                      -- ^ Violation type
  , jvMessage :: Text                   -- ^ Human-readable message
  , jvSignal :: Maybe Text              -- ^ Signal name if applicable
  } deriving (Show, Eq, Generic)

instance ToJSON JSONViolation

-- | JSON representation of a parse error
data JSONParseError = JSONParseError
  { jpFile :: Text                      -- ^ Source file path
  , jpLine :: Int                       -- ^ Line number
  , jpColumn :: Int                     -- ^ Column number
  , jpMessage :: Text                   -- ^ Error message
  } deriving (Show, Eq, Generic)

instance ToJSON JSONParseError

--------------------------------------------------------------------------------
-- SARIF Types (Static Analysis Results Interchange Format)
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: SARIF Export
-- Specification: SARIF 2.1.0 (OASIS Standard)
--------------------------------------------------------------------------------

-- | SARIF level (severity)
data SARIFLevel
  = SARIFError
  | SARIFWarning
  | SARIFNote
  | SARIFNone
  deriving (Show, Eq, Generic)

instance ToJSON SARIFLevel where
  toJSON SARIFError = "error"
  toJSON SARIFWarning = "warning"
  toJSON SARIFNote = "note"
  toJSON SARIFNone = "none"

-- | SARIF message
data SARIFMessage = SARIFMessage
  { smText :: Text                      -- ^ Plain text message
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFMessage where
  toJSON SARIFMessage{..} = object ["text" .= smText]

-- | SARIF region (location within a file)
data SARIFRegion = SARIFRegion
  { srStartLine :: Int                  -- ^ Starting line (1-based)
  , srStartColumn :: Int                -- ^ Starting column (1-based)
  , srEndLine :: Maybe Int              -- ^ Ending line (optional)
  , srEndColumn :: Maybe Int            -- ^ Ending column (optional)
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFRegion where
  toJSON SARIFRegion{..} = object $ catMaybes
    [ Just ("startLine" .= srStartLine)
    , Just ("startColumn" .= srStartColumn)
    , ("endLine" .=) <$> srEndLine
    , ("endColumn" .=) <$> srEndColumn
    ]

-- | SARIF artifact location (file path)
data SARIFArtifactLocation = SARIFArtifactLocation
  { salUri :: Text                      -- ^ File URI
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFArtifactLocation where
  toJSON SARIFArtifactLocation{..} = object ["uri" .= salUri]

-- | SARIF physical location
data SARIFPhysicalLocation = SARIFPhysicalLocation
  { splArtifactLocation :: SARIFArtifactLocation
  , splRegion :: SARIFRegion
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFPhysicalLocation where
  toJSON SARIFPhysicalLocation{..} = object
    [ "artifactLocation" .= splArtifactLocation
    , "region" .= splRegion
    ]

-- | SARIF location
data SARIFLocation = SARIFLocation
  { slPhysicalLocation :: SARIFPhysicalLocation
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFLocation where
  toJSON SARIFLocation{..} = object ["physicalLocation" .= slPhysicalLocation]

-- | SARIF result (single finding)
data SARIFResult = SARIFResult
  { srRuleId :: Text                    -- ^ Rule identifier
  , srLevel :: SARIFLevel               -- ^ Severity level
  , srMessage :: SARIFMessage           -- ^ Message
  , srLocations :: [SARIFLocation]      -- ^ Locations
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFResult where
  toJSON SARIFResult{..} = object
    [ "ruleId" .= srRuleId
    , "level" .= srLevel
    , "message" .= srMessage
    , "locations" .= srLocations
    ]

-- | SARIF tool driver
data SARIFToolDriver = SARIFToolDriver
  { stdName :: Text                     -- ^ Tool name
  , stdVersion :: Text                  -- ^ Tool version
  , stdInformationUri :: Text           -- ^ Tool information URL
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFToolDriver where
  toJSON SARIFToolDriver{..} = object
    [ "name" .= stdName
    , "version" .= stdVersion
    , "informationUri" .= stdInformationUri
    ]

-- | SARIF tool
data SARIFTool = SARIFTool
  { stDriver :: SARIFToolDriver
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFTool where
  toJSON SARIFTool{..} = object ["driver" .= stDriver]

-- | SARIF run
data SARIFRun = SARIFRun
  { srunTool :: SARIFTool               -- ^ Tool that produced results
  , srunResults :: [SARIFResult]        -- ^ Analysis results
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFRun where
  toJSON SARIFRun{..} = object
    [ "tool" .= srunTool
    , "results" .= srunResults
    ]

-- | SARIF report (top-level)
data SARIFReport = SARIFReport
  { sarifVersion :: Text                -- ^ SARIF version (2.1.0)
  , sarifSchema :: Text                 -- ^ JSON schema URL
  , sarifRuns :: [SARIFRun]             -- ^ Analysis runs
  } deriving (Show, Eq, Generic)

instance ToJSON SARIFReport where
  toJSON SARIFReport{..} = object
    [ "$schema" .= sarifSchema
    , "version" .= sarifVersion
    , "runs" .= sarifRuns
    ]

--------------------------------------------------------------------------------
-- Export Functions
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: Export Functions
--------------------------------------------------------------------------------

-- | Export analysis results as JSON
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: JSON Export
exportJSON :: Text                      -- ^ Timestamp (ISO 8601)
           -> [FilePath]                -- ^ Files analyzed
           -> [ConstraintViolation]     -- ^ Violations found
           -> [ParseError]              -- ^ Parse errors
           -> Bool                      -- ^ Success status
           -> Text                      -- ^ JSON output
exportJSON timestamp files violations parseErrors success =
  let jsonViolations = map violationToJSON violations
      jsonParseErrors = map parseErrorToJSON parseErrors

      errorCount = length $ filter (\v -> violationSeverity v == SeverityError) violations
      warningCount = length $ filter (\v -> violationSeverity v == SeverityWarning) violations
      infoCount = length $ filter (\v -> violationSeverity v == SeverityInfo) violations

      summary = JSONSummary
        { jsFilesAnalyzed = length files
        , jsErrorCount = errorCount
        , jsWarningCount = warningCount
        , jsInfoCount = infoCount
        , jsParseErrorCount = length parseErrors
        , jsSuccess = success
        }

      report = JSONReport
        { jrVersion = "0.8.0"
        , jrTimestamp = timestamp
        , jrFiles = map T.pack files
        , jrSummary = summary
        , jrViolations = jsonViolations
        , jrParseErrors = jsonParseErrors
        }
  in TL.toStrict $ TLE.decodeUtf8 $ encode report

-- | Export analysis results as SARIF
-- ADC-IMPLEMENTS: spellcraft-adc-031 Section: SARIF Export
exportSARIF :: [ConstraintViolation]    -- ^ Violations found
            -> [ParseError]             -- ^ Parse errors
            -> Text                     -- ^ SARIF output
exportSARIF violations parseErrors =
  let violationResults = map violationToSARIF violations
      parseErrorResults = map parseErrorToSARIF parseErrors

      tool = SARIFTool
        { stDriver = SARIFToolDriver
            { stdName = "Spellcraft"
            , stdVersion = "0.8.0"
            , stdInformationUri = "https://github.com/t4mber/spellcraft"
            }
        }

      run = SARIFRun
        { srunTool = tool
        , srunResults = violationResults ++ parseErrorResults
        }

      report = SARIFReport
        { sarifVersion = "2.1.0"
        , sarifSchema = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"
        , sarifRuns = [run]
        }
  in TL.toStrict $ TLE.decodeUtf8 $ encode report

--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- | Convert severity to SARIF level
toSARIFLevel :: Severity -> SARIFLevel
toSARIFLevel SeverityError = SARIFError
toSARIFLevel SeverityWarning = SARIFWarning
toSARIFLevel SeverityInfo = SARIFNote

-- | Convert severity to text
severityToText :: Severity -> Text
severityToText SeverityError = "error"
severityToText SeverityWarning = "warning"
severityToText SeverityInfo = "info"

-- | Convert violation to JSON format
violationToJSON :: ConstraintViolation -> JSONViolation
violationToJSON violation = case violation of
  FrequencyViolation{..} ->
    JSONViolation
      { jvFile = T.pack $ locFile violationLocation
      , jvLine = locLine violationLocation
      , jvColumn = locColumn violationLocation
      , jvSeverity = severityToText $ violationSeverity violation
      , jvType = "frequency"
      , jvMessage = violationComponent <> " port " <> violationPort <>
                    " receives " <> T.pack (show violationActual) <> " MHz" <>
                    " but maximum is " <> T.pack (show violationMax) <> " MHz"
      , jvSignal = Just violationPort
      }

  GenericRangeViolation{..} ->
    JSONViolation
      { jvFile = T.pack $ locFile violationLocation
      , jvLine = locLine violationLocation
      , jvColumn = locColumn violationLocation
      , jvSeverity = severityToText $ violationSeverity violation
      , jvType = "generic_range"
      , jvMessage = violationComponent <> " generic " <> violationGeneric <>
                    " value out of range"
      , jvSignal = Just violationGeneric
      }

  FanOutViolation{..} ->
    JSONViolation
      { jvFile = T.pack $ locFile violationLocation
      , jvLine = locLine violationLocation
      , jvColumn = locColumn violationLocation
      , jvSeverity = severityToText $ violationSeverity violation
      , jvType = "fan_out"
      , jvMessage = violationComponent <> " port " <> violationPort <>
                    " fan-out " <> T.pack (show violationActualFanOut) <>
                    " exceeds maximum " <> T.pack (show violationMaxFanOut)
      , jvSignal = Just violationPort
      }

  SignalUsageViolation{..} ->
    JSONViolation
      { jvFile = T.pack $ locFile violationLocation
      , jvLine = locLine violationLocation
      , jvColumn = locColumn violationLocation
      , jvSeverity = severityToText $ violationSeverity violation
      , jvType = "signal_usage"
      , jvMessage = violationDescription
      , jvSignal = Just violationSignalName
      }

  ControlFlowViolation{..} ->
    JSONViolation
      { jvFile = T.pack $ locFile violationLocation
      , jvLine = locLine violationLocation
      , jvColumn = locColumn violationLocation
      , jvSeverity = severityToText $ violationSeverity violation
      , jvType = "control_flow"
      , jvMessage = violationDescription
      , jvSignal = Just violationSignalName
      }

  ArithmeticBoundsViolation{..} ->
    JSONViolation
      { jvFile = T.pack $ locFile violationLocation
      , jvLine = locLine violationLocation
      , jvColumn = locColumn violationLocation
      , jvSeverity = severityToText $ violationSeverity violation
      , jvType = "arithmetic_bounds"
      , jvMessage = violationDescription
      , jvSignal = Just violationSignalName
      }

-- | Convert parse error to JSON format
parseErrorToJSON :: ParseError -> JSONParseError
parseErrorToJSON pe =
  JSONParseError
    { jpFile = T.pack $ locFile (parseErrorLocation pe)
    , jpLine = locLine (parseErrorLocation pe)
    , jpColumn = locColumn (parseErrorLocation pe)
    , jpMessage = parseErrorMessage pe
    }

-- | Convert violation to SARIF result
violationToSARIF :: ConstraintViolation -> SARIFResult
violationToSARIF violation =
  let (file, line, col, msg, ruleId) = case violation of
        FrequencyViolation{..} ->
          ( locFile violationLocation
          , locLine violationLocation
          , locColumn violationLocation
          , violationComponent <> " port " <> violationPort <>
            " receives " <> T.pack (show violationActual) <> " MHz" <>
            " but maximum is " <> T.pack (show violationMax) <> " MHz"
          , "SPELL001"  -- Frequency violation
          )

        GenericRangeViolation{..} ->
          ( locFile violationLocation
          , locLine violationLocation
          , locColumn violationLocation
          , violationComponent <> " generic " <> violationGeneric <>
            " value out of range"
          , "SPELL002"  -- Generic range violation
          )

        FanOutViolation{..} ->
          ( locFile violationLocation
          , locLine violationLocation
          , locColumn violationLocation
          , violationComponent <> " port " <> violationPort <>
            " fan-out exceeds maximum"
          , "SPELL003"  -- Fan-out violation
          )

        SignalUsageViolation{..} ->
          ( locFile violationLocation
          , locLine violationLocation
          , locColumn violationLocation
          , violationDescription
          , "SPELL004"  -- Signal usage violation
          )

        ControlFlowViolation{..} ->
          ( locFile violationLocation
          , locLine violationLocation
          , locColumn violationLocation
          , violationDescription
          , "SPELL005"  -- Control flow violation
          )

        ArithmeticBoundsViolation{..} ->
          ( locFile violationLocation
          , locLine violationLocation
          , locColumn violationLocation
          , violationDescription
          , "SPELL006"  -- Arithmetic bounds violation
          )

      location = SARIFLocation
        { slPhysicalLocation = SARIFPhysicalLocation
            { splArtifactLocation = SARIFArtifactLocation { salUri = T.pack file }
            , splRegion = SARIFRegion
                { srStartLine = line
                , srStartColumn = col
                , srEndLine = Nothing
                , srEndColumn = Nothing
                }
            }
        }
  in SARIFResult
    { srRuleId = ruleId
    , srLevel = toSARIFLevel $ violationSeverity violation
    , srMessage = SARIFMessage { smText = msg }
    , srLocations = [location]
    }

-- | Convert parse error to SARIF result
parseErrorToSARIF :: ParseError -> SARIFResult
parseErrorToSARIF pe =
  let loc = parseErrorLocation pe
      location = SARIFLocation
        { slPhysicalLocation = SARIFPhysicalLocation
            { splArtifactLocation = SARIFArtifactLocation { salUri = T.pack (locFile loc) }
            , splRegion = SARIFRegion
                { srStartLine = locLine loc
                , srStartColumn = locColumn loc
                , srEndLine = Nothing
                , srEndColumn = Nothing
                }
            }
        }
  in SARIFResult
    { srRuleId = "SPELL000"  -- Parse error
    , srLevel = SARIFError
    , srMessage = SARIFMessage { smText = parseErrorMessage pe }
    , srLocations = [location]
    }
