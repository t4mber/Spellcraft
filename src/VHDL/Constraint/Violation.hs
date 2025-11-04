-- ADC-IMPLEMENTS: vhdl-analyzer-adc-002
module VHDL.Constraint.Violation
  ( -- * Violation Checking
    checkGenericValue
  , checkPortFrequency
  ) where

import Data.Text (Text)
import VHDL.AST (Value(..), Identifier)
import VHDL.Constraint.Types
  ( ConstraintViolation(..)
  , GenericConstraint(..)
  , GenericType(..)
  , Range(..)
  , PortConstraint(..)
  )
import VHDL.SourceLocation (SourceLocation, mkSourceLocation)

-- | Check if a generic value satisfies its constraint
-- Contract: vhdl-analyzer-adc-002 Section: Interface
checkGenericValue
  :: GenericConstraint
  -> Value
  -> SourceLocation
  -> Text
  -> Maybe ConstraintViolation
checkGenericValue constraint val loc compName =
  case (genConstraintRange constraint, val) of
    (Just (IntRange minVal maxVal), IntValue v) ->
      if v < minVal || v > maxVal
        then Just $ GenericRangeViolation
          { violationComponent = compName
          , violationGeneric = genConstraintName constraint
          , violationValue = val
          , violationRange = IntRange minVal maxVal
          , violationLocation = loc
          }
        else Nothing
    (Just (RealRange minVal maxVal), RealValue v) ->
      if v < minVal || v > maxVal
        then Just $ GenericRangeViolation
          { violationComponent = compName
          , violationGeneric = genConstraintName constraint
          , violationValue = val
          , violationRange = RealRange minVal maxVal
          , violationLocation = loc
          }
        else Nothing
    _ -> Nothing

-- | Check if a port frequency satisfies its constraint
-- Contract: vhdl-analyzer-adc-002 Section: Interface
checkPortFrequency
  :: PortConstraint
  -> Double  -- Actual frequency in MHz
  -> SourceLocation
  -> Text  -- Component name
  -> Maybe ConstraintViolation
checkPortFrequency constraint actualFreq loc compName =
  case portConstraintMaxFreq constraint of
    Just maxFreq ->
      if actualFreq > maxFreq
        then Just $ FrequencyViolation
          { violationComponent = compName
          , violationPort = portConstraintName constraint
          , violationActual = actualFreq
          , violationMax = maxFreq
          , violationLocation = loc
          }
        else Nothing
    Nothing -> Nothing
