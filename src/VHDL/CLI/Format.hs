{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-005
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
module VHDL.CLI.Format
  ( -- * Formatting
    formatViolation
  , formatViolationWithSeverity
  , formatComplexityWarning
  , formatParseError
  , severityLabel
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import VHDL.Analysis.Combinatorial (ComplexityWarning(..), CombinatorialPath(..))
import VHDL.CLI.Options (OutputFormat(..))
import VHDL.Constraint.Types (ConstraintViolation(..), Severity(..), violationSeverity)
import VHDL.Parser (ParseError(..))
import VHDL.SourceLocation (formatLocation)

-- | Get the label for a severity level
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
severityLabel :: Severity -> Text
severityLabel SeverityError = "error"
severityLabel SeverityWarning = "warning"
severityLabel SeverityInfo = "info"

-- | Format constraint violation with automatic severity detection
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
formatViolationWithSeverity :: OutputFormat -> ConstraintViolation -> (Text, Severity)
formatViolationWithSeverity fmt violation =
  let severity = violationSeverity violation
      formatted = formatViolation fmt violation
  in (formatted, severity)

-- | Format constraint violation
-- Contract: spellcraft-adc-005 Section: Interface
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
formatViolation :: OutputFormat -> ConstraintViolation -> Text
formatViolation fmt violation@(FrequencyViolation comp port actual maxFreq loc) =
  let sev = severityLabel (violationSeverity violation)
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: %s: Frequency violation\n  Component '%s' port '%s' receives %.1f MHz\n  but maximum is %.1f MHz"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack comp) (T.unpack port) actual maxFreq
    GCC -> T.pack $ printf "%s: %s: Frequency violation: %s.%s receives %.1f MHz but max is %.1f MHz"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack comp) (T.unpack port) actual maxFreq
    JSON -> T.pack $ printf "{\"type\":\"FrequencyViolation\",\"severity\":\"%s\",\"component\":\"%s\",\"port\":\"%s\",\"actual\":%.1f,\"max\":%.1f}"
      (T.unpack sev) (T.unpack comp) (T.unpack port) actual maxFreq

formatViolation fmt violation@(GenericRangeViolation comp generic val range loc) =
  let sev = severityLabel (violationSeverity violation)
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: %s: Generic range violation\n  Component '%s' generic '%s' value %s\n  is outside allowed range %s"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack comp) (T.unpack generic) (show val) (show range)
    GCC -> T.pack $ printf "%s: %s: Generic range violation: %s.%s = %s (range: %s)"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack comp) (T.unpack generic) (show val) (show range)
    JSON -> T.pack $ printf "{\"type\":\"GenericRangeViolation\",\"severity\":\"%s\",\"component\":\"%s\",\"generic\":\"%s\"}"
      (T.unpack sev) (T.unpack comp) (T.unpack generic)

formatViolation fmt violation@(FanOutViolation comp port actualFanOut maxFanOut loc) =
  let sev = severityLabel (violationSeverity violation)
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: %s: Fan-out violation\n  Component '%s' port '%s' has %d connections\n  but maximum is %d"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack comp) (T.unpack port) actualFanOut maxFanOut
    GCC -> T.pack $ printf "%s: %s: Fan-out violation: %s.%s has %d connections (max: %d)"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack comp) (T.unpack port) actualFanOut maxFanOut
    JSON -> T.pack $ printf "{\"type\":\"FanOutViolation\",\"severity\":\"%s\",\"component\":\"%s\",\"port\":\"%s\",\"actual\":%d,\"max\":%d}"
      (T.unpack sev) (T.unpack comp) (T.unpack port) actualFanOut maxFanOut

-- | Format signal usage violation
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
formatViolation fmt violation@(SignalUsageViolation sigName desc loc) =
  let sev = severityLabel (violationSeverity violation)
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: %s: %s\n  Signal: '%s'"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack desc) (T.unpack sigName)
    GCC -> T.pack $ printf "%s: %s: %s: %s"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack desc) (T.unpack sigName)
    JSON -> T.pack $ printf "{\"type\":\"SignalUsageViolation\",\"severity\":\"%s\",\"signal\":\"%s\",\"description\":\"%s\"}"
      (T.unpack sev) (T.unpack sigName) (T.unpack desc)

-- | Format control flow violation (latch inference)
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
formatViolation fmt violation@(ControlFlowViolation sigName desc loc) =
  let sev = severityLabel (violationSeverity violation)
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: %s: Potential latch inference\n  %s\n  Signal: '%s'"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack desc) (T.unpack sigName)
    GCC -> T.pack $ printf "%s: %s: Latch inference: %s"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack sigName)
    JSON -> T.pack $ printf "{\"type\":\"ControlFlowViolation\",\"severity\":\"%s\",\"signal\":\"%s\",\"description\":\"%s\"}"
      (T.unpack sev) (T.unpack sigName) (T.unpack desc)

-- | Format arithmetic bounds violation (overflow risk)
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Report Updates
formatViolation fmt violation@(ArithmeticBoundsViolation sigName desc loc) =
  let sev = severityLabel (violationSeverity violation)
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: %s: Arithmetic bounds risk\n  %s\n  Signal: '%s'"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack desc) (T.unpack sigName)
    GCC -> T.pack $ printf "%s: %s: Overflow risk: %s"
      (T.unpack $ formatLocation loc) (T.unpack sev) (T.unpack sigName)
    JSON -> T.pack $ printf "{\"type\":\"ArithmeticBoundsViolation\",\"severity\":\"%s\",\"signal\":\"%s\",\"description\":\"%s\"}"
      (T.unpack sev) (T.unpack sigName) (T.unpack desc)

-- | Format complexity warning
-- Contract: spellcraft-adc-005 Section: Interface
formatComplexityWarning :: OutputFormat -> ComplexityWarning -> Text
formatComplexityWarning fmt warning =
  let loc = cpLocation (cwPath warning)
      depth = cpDepth (cwPath warning)
      threshold = cwThreshold warning
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: warning: Combinatorial complexity\n  Process has %d arithmetic operations (threshold: %d)"
      (T.unpack $ formatLocation loc) depth threshold
    GCC -> T.pack $ printf "%s: warning: Combinatorial complexity: %d operations (threshold: %d)"
      (T.unpack $ formatLocation loc) depth threshold
    JSON -> T.pack $ printf "{\"type\":\"ComplexityWarning\",\"depth\":%d,\"threshold\":%d}"
      depth threshold

-- | Format parse error
-- Contract: spellcraft-adc-005 Section: Interface
formatParseError :: OutputFormat -> ParseError -> Text
formatParseError fmt err =
  let loc = parseErrorLocation err
      msg = parseErrorMessage err
  in case fmt of
    HumanReadable -> T.pack $ printf "%s: error: Parse error\n  %s"
      (T.unpack $ formatLocation loc) (T.unpack msg)
    GCC -> T.pack $ printf "%s: error: %s"
      (T.unpack $ formatLocation loc) (T.unpack msg)
    JSON -> T.pack $ printf "{\"type\":\"ParseError\",\"message\":\"%s\"}"
      (T.unpack msg)
