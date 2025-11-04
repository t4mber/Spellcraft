{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: spellcraft-adc-002
module VHDL.Constraint.Types
  ( -- * Component Specifications
    ComponentSpec(..)
  , GenericConstraint(..)
  , GenericType(..)
  , Range(..)
  , PortConstraint(..)
    -- * Violations
  , ConstraintViolation(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
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
  deriving (Show, Eq, Generic)

instance ToJSON ConstraintViolation
