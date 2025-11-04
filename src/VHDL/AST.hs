{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-001
module VHDL.AST
  ( -- * Design
    VHDLDesign(..)
    -- * Entity
  , Entity(..)
  , GenericDecl(..)
  , PortDecl(..)
  , PortDirection(..)
    -- * Architecture
  , Architecture(..)
  , ComponentInst(..)
    -- * Types
  , Identifier
  , TypeName
  , SignalName
  , Value(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import VHDL.SourceLocation (SourceLocation)

-- | A complete VHDL design (entities and architectures)
-- Contract: vhdl-analyzer-adc-001 Section: Interface
data VHDLDesign = VHDLDesign
  { designEntities :: [Entity]
  , designArchitectures :: [Architecture]
  , designSourceFile :: FilePath
  } deriving (Show, Eq, Generic)

instance ToJSON VHDLDesign

-- | Entity declaration with generics and ports
data Entity = Entity
  { entityName :: Identifier
  , entityGenerics :: [GenericDecl]
  , entityPorts :: [PortDecl]
  , entityLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON Entity

-- | Generic parameter declaration
data GenericDecl = GenericDecl
  { genericName :: Identifier
  , genericType :: TypeName
  , genericDefault :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON GenericDecl

-- | Port declaration
data PortDecl = PortDecl
  { portName :: Identifier
  , portDirection :: PortDirection
  , portType :: TypeName
  } deriving (Show, Eq, Generic)

instance ToJSON PortDecl

-- | Port direction
data PortDirection
  = Input
  | Output
  | InOut
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON PortDirection

-- | Architecture with component instantiations
data Architecture = Architecture
  { archName :: Identifier
  , archEntityName :: Identifier
  , archComponents :: [ComponentInst]
  , archLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON Architecture

-- | Component instantiation with generic and port maps
data ComponentInst = ComponentInst
  { compInstName :: Identifier
  , compComponentName :: Identifier
  , compGenericMap :: [(Identifier, Value)]
  , compPortMap :: [(Identifier, SignalName)]
  , compLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON ComponentInst

-- | Type aliases
type Identifier = Text
type TypeName = Text
type SignalName = Text

-- | Values that can appear in generic maps
data Value
  = IntValue Integer
  | RealValue Double
  | StringValue Text
  | IdentifierValue Identifier
  deriving (Show, Eq, Generic)

instance ToJSON Value
