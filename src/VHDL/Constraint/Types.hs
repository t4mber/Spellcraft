{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- | Partial fields are intentional - ConstraintViolation constructors have different fields.

-- ADC-IMPLEMENTS: spellcraft-adc-002
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Core Types
module VHDL.Constraint.Types
  ( -- * Component Specifications
    ComponentSpec(..)
  , GenericConstraint(..)
  , GenericType(..)
  , Range(..)
  , PortConstraint(..)
    -- * Severity (ADC-014)
  , Severity(..)
  , violationSeverity
    -- * Violations
  , ConstraintViolation(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified Data.Text as Data.Text
import GHC.Generics (Generic)
import VHDL.AST (Identifier, PortDirection, Value)
import VHDL.SourceLocation (SourceLocation)

-- | Component specification with constraints
-- Contract: spellcraft-adc-002 Section: Interface
data ComponentSpec = ComponentSpec
  { compSpecName :: Text
  , compSpecGenerics :: [GenericConstraint]
  , compSpecPorts :: [PortConstraint]
  } deriving (Show, Eq, Generic)

instance ToJSON ComponentSpec

-- | Generic parameter constraint
data GenericConstraint = GenericConstraint
  { genConstraintName :: Identifier
  , genConstraintType :: GenericType
  , genConstraintRange :: Maybe Range
  } deriving (Show, Eq, Generic)

instance ToJSON GenericConstraint

-- | Generic parameter type
data GenericType
  = IntegerType
  | RealType
  | StringType
  deriving (Show, Eq, Generic)

instance ToJSON GenericType

-- | Value range constraint
data Range
  = IntRange Integer Integer  -- min, max
  | RealRange Double Double
  deriving (Show, Eq, Generic)

instance ToJSON Range

-- | Port constraint including max frequency
data PortConstraint = PortConstraint
  { portConstraintName :: Identifier
  , portConstraintDirection :: PortDirection
  , portConstraintMaxFreq :: Maybe Double  -- in MHz
  , portConstraintFanOut :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON PortConstraint

-- | Severity levels for violations
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Core Types
-- Errors: Critical issues that must be fixed (exit code 1)
-- Warnings: Non-critical issues (exit code 0 unless --strict)
-- Info: Informational suggestions (never affects exit code)
data Severity
  = SeverityError    -- ^ Critical violations (undriven signals, frequency violations)
  | SeverityWarning  -- ^ Non-critical issues (unused signals, latch inference)
  | SeverityInfo     -- ^ Informational suggestions
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Severity

-- | Constraint violations
-- Contract: spellcraft-adc-002 Section: Interface
data ConstraintViolation
  = FrequencyViolation
      { violationComponent :: Text
      , violationPort :: Identifier
      , violationActual :: Double  -- MHz
      , violationMax :: Double     -- MHz
      , violationLocation :: SourceLocation
      }
  | GenericRangeViolation
      { violationComponent :: Text
      , violationGeneric :: Identifier
      , violationValue :: Value
      , violationRange :: Range
      , violationLocation :: SourceLocation
      }
  | FanOutViolation
      { violationComponent :: Text
      , violationPort :: Identifier
      , violationActualFanOut :: Int
      , violationMaxFanOut :: Int
      , violationLocation :: SourceLocation
      }
  | SignalUsageViolation
      { violationSignalName :: Text
      , violationDescription :: Text
      , violationLocation :: SourceLocation
      }
  -- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
  | ControlFlowViolation
      { violationSignalName :: Text
      , violationDescription :: Text
      , violationLocation :: SourceLocation
      }
  -- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
  | ArithmeticBoundsViolation
      { violationSignalName :: Text
      , violationDescription :: Text
      , violationLocation :: SourceLocation
      }
  deriving (Show, Eq, Generic)

instance ToJSON ConstraintViolation

-- | Determine severity of a constraint violation
-- ADC-IMPLEMENTS: spellcraft-adc-014 Section: Severity Classifications
-- Errors: Undriven signals, frequency violations (critical issues)
-- Warnings: Unused signals, latch inference, unbounded counters (non-critical)
violationSeverity :: ConstraintViolation -> Severity
-- Frequency violations are errors (can cause hardware failure)
violationSeverity (FrequencyViolation {}) = SeverityError
-- Generic range violations are errors (invalid configuration)
violationSeverity (GenericRangeViolation {}) = SeverityError
-- Fan-out violations are errors (can cause signal integrity issues)
violationSeverity (FanOutViolation {}) = SeverityError
-- Signal usage violations depend on the type
violationSeverity (SignalUsageViolation _ desc _)
  -- Undriven signals are errors (no source at all - will be undefined)
  | "undriven" `isInfixOfText` desc = SeverityError
  -- Unused signals are warnings (written but not read - wasteful but not fatal)
  | "unused" `isInfixOfText` desc = SeverityWarning
  | otherwise = SeverityWarning
-- Control flow violations (latch inference) are warnings
violationSeverity (ControlFlowViolation {}) = SeverityWarning
-- Arithmetic bounds violations (overflow risk) are warnings
violationSeverity (ArithmeticBoundsViolation {}) = SeverityWarning

-- | Case-insensitive infix check for Text
isInfixOfText :: Text -> Text -> Bool
isInfixOfText needle haystack =
  let lowerNeedle = Data.Text.toLower needle
      lowerHaystack = Data.Text.toLower haystack
  in lowerNeedle `Data.Text.isInfixOf` lowerHaystack
