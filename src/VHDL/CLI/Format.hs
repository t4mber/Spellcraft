{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-005
module VHDL.CLI.Format
  ( -- * Formatting
    formatViolation
  , formatComplexityWarning
  , formatParseError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import VHDL.Analysis.Combinatorial (ComplexityWarning(..), CombinatorialPath(..))
import VHDL.CLI.Options (OutputFormat(..))
import VHDL.Constraint.Types (ConstraintViolation(..))
import VHDL.Parser (ParseError(..))
import VHDL.SourceLocation (formatLocation)

-- | Format constraint violation
-- Contract: vhdl-analyzer-adc-005 Section: Interface
formatViolation :: OutputFormat -> ConstraintViolation -> Text
formatViolation fmt (FrequencyViolation comp port actual maxFreq loc) =
  case fmt of
    HumanReadable -> T.pack $ printf "%s: error: Frequency violation\n  Component '%s' port '%s' receives %.1f MHz\n  but maximum is %.1f MHz"
      (T.unpack $ formatLocation loc) (T.unpack comp) (T.unpack port) actual maxFreq
    GCC -> T.pack $ printf "%s: error: Frequency violation: %s.%s receives %.1f MHz but max is %.1f MHz"
      (T.unpack $ formatLocation loc) (T.unpack comp) (T.unpack port) actual maxFreq
    JSON -> T.pack $ printf "{\"type\":\"FrequencyViolation\",\"component\":\"%s\",\"port\":\"%s\",\"actual\":%.1f,\"max\":%.1f}"
      (T.unpack comp) (T.unpack port) actual maxFreq

formatViolation fmt (GenericRangeViolation comp generic val range loc) =
  case fmt of
    HumanReadable -> T.pack $ printf "%s: error: Generic range violation\n  Component '%s' generic '%s' value %s\n  is outside allowed range %s"
      (T.unpack $ formatLocation loc) (T.unpack comp) (T.unpack generic) (show val) (show range)
    GCC -> T.pack $ printf "%s: error: Generic range violation: %s.%s = %s (range: %s)"
      (T.unpack $ formatLocation loc) (T.unpack comp) (T.unpack generic) (show val) (show range)
    JSON -> T.pack $ printf "{\"type\":\"GenericRangeViolation\",\"component\":\"%s\",\"generic\":\"%s\"}"
      (T.unpack comp) (T.unpack generic)

formatViolation fmt (FanOutViolation comp port actualFanOut maxFanOut loc) =
  case fmt of
    HumanReadable -> T.pack $ printf "%s: error: Fan-out violation\n  Component '%s' port '%s' has %d connections\n  but maximum is %d"
      (T.unpack $ formatLocation loc) (T.unpack comp) (T.unpack port) actualFanOut maxFanOut
    GCC -> T.pack $ printf "%s: error: Fan-out violation: %s.%s has %d connections (max: %d)"
      (T.unpack $ formatLocation loc) (T.unpack comp) (T.unpack port) actualFanOut maxFanOut
    JSON -> T.pack $ printf "{\"type\":\"FanOutViolation\",\"component\":\"%s\",\"port\":\"%s\",\"actual\":%d,\"max\":%d}"
      (T.unpack comp) (T.unpack port) actualFanOut maxFanOut

-- | Format complexity warning
-- Contract: vhdl-analyzer-adc-005 Section: Interface
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
-- Contract: vhdl-analyzer-adc-005 Section: Interface
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
