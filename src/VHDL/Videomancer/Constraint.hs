{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: <parameter-constraint-01>
-- Purpose: Represent hardware constraints extracted from VHDL generics
-- Contract: spellcraft-adc-010
module VHDL.Videomancer.Constraint
  ( -- * Constraint Types
    ConstraintSource(..)
  , ParameterConstraint(..)
  , ConstraintViolation(..)
  , ViolationType(..)
  , Severity(..)
    -- * Extraction Functions
  , extractConstraintsFromVHDL
  , extractFromEntity
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import VHDL.AST
import VHDL.SourceLocation
import VHDL.Videomancer.Config (ParameterConfig)

-- | Source of a parameter constraint
data ConstraintSource
  = FromGeneric Text SourceLocation  -- ^ From VHDL generic declaration
  | FromComment Text SourceLocation  -- ^ From VHDL comment annotation
  | Inferred SourceLocation           -- ^ Inferred from signal widths
  deriving (Show, Eq, Generic)

instance ToJSON ConstraintSource

-- | Hardware constraint from VHDL
data ParameterConstraint = ParameterConstraint
  { pcName :: Text                    -- ^ Parameter name from VHDL
  , pcSource :: ConstraintSource      -- ^ Where constraint came from
  , pcMinValue :: Maybe Double        -- ^ Minimum value
  , pcMaxValue :: Maybe Double        -- ^ Maximum value
  , pcDataType :: Maybe Text          -- ^ VHDL type name
  , pcBitWidth :: Maybe Int           -- ^ Signal bit width
  } deriving (Show, Eq, Generic)

instance ToJSON ParameterConstraint

-- | Type of constraint violation
data ViolationType
  = MinValueMismatch        -- ^ JSON min doesn't match VHDL constraint
  | MaxValueMismatch        -- ^ JSON max doesn't match VHDL constraint
  | TypeMismatch            -- ^ Type incompatibility
  | MissingParameter        -- ^ Parameter in VHDL but not in JSON
  | ExtraParameter          -- ^ Parameter in JSON but not in VHDL
  deriving (Show, Eq, Generic)

instance ToJSON ViolationType

-- | Severity of violation
data Severity
  = Error     -- ^ Critical error, invalid configuration
  | Warning   -- ^ Warning, may work but inconsistent
  | Info      -- ^ Informational only
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Severity

-- | Constraint violation
data ConstraintViolation = ConstraintViolation
  { cvParameter :: Text                  -- ^ Parameter name
  , cvExpected :: Maybe ParameterConstraint  -- ^ Expected constraint (if any)
  , cvActual :: ParameterConfig          -- ^ Actual config from JSON
  , cvViolationType :: ViolationType     -- ^ Type of violation
  , cvSeverity :: Severity               -- ^ Severity level
  , cvMessage :: Text                    -- ^ Human-readable message
  } deriving (Show, Eq, Generic)

instance ToJSON ConstraintViolation

-- | Extract parameter constraints from VHDL designs
-- Contract: spellcraft-adc-010 Section: Algorithm
extractConstraintsFromVHDL :: [VHDLDesign] -> [ParameterConstraint]
extractConstraintsFromVHDL designs =
  concatMap extractFromDesign designs

-- | Extract constraints from a single design
extractFromDesign :: VHDLDesign -> [ParameterConstraint]
extractFromDesign design =
  concatMap extractFromEntity (designEntities design)

-- | Extract constraints from entity generic declarations
-- Contract: spellcraft-adc-010 Section: Algorithm
extractFromEntity :: Entity -> [ParameterConstraint]
extractFromEntity (Entity _name generics _ports loc) =
  map (extractFromGeneric loc) generics

-- | Extract constraint from a generic declaration
extractFromGeneric :: SourceLocation -> GenericDecl -> ParameterConstraint
extractFromGeneric entityLoc (GenericDecl name gtype defaultVal) =
  createConstraint gtype defaultVal entityLoc name

-- | Create a parameter constraint from generic info
createConstraint :: Text  -- Type name
                 -> Maybe Value
                 -> SourceLocation
                 -> Text  -- Generic name
                 -> ParameterConstraint
createConstraint gtype maybeDefault loc name =
  ParameterConstraint
    { pcName = name
    , pcSource = FromGeneric name loc
    , pcMinValue = extractMinFromType gtype maybeDefault
    , pcMaxValue = extractMaxFromType gtype maybeDefault
    , pcDataType = Just gtype
    , pcBitWidth = extractBitWidth gtype
    }

-- | Extract minimum value from VHDL type name
extractMinFromType :: Text -> Maybe Value -> Maybe Double
extractMinFromType _typename _defaultVal =
  -- For now, we can't extract range info from type names alone
  -- This would require a type database or parsing type definitions
  Nothing

-- | Extract maximum value from VHDL type name
extractMaxFromType :: Text -> Maybe Value -> Maybe Double
extractMaxFromType _typename _defaultVal =
  -- For now, we can't extract range info from type names alone
  -- This would require a type database or parsing type definitions
  Nothing

-- | Extract bit width from VHDL type name
extractBitWidth :: Text -> Maybe Int
extractBitWidth _typename =
  -- Conservative: can't determine bit width from type name alone
  Nothing
