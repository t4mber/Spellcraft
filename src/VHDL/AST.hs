{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: spellcraft-adc-001
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- ADC-IMPLEMENTS: spellcraft-adc-012
module VHDL.AST
  ( -- * Design
    VHDLDesign(..)
    -- * Library and Use Clauses
  , LibraryDeclaration(..)
  , UseClause(..)
    -- * Entity
  , Entity(..)
  , GenericDecl(..)
  , PortDecl(..)
  , PortDirection(..)
    -- * Architecture
  , Architecture(..)
  , SignalDecl(..)
  , ArchStatement(..)
  , Statement(..)
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

-- | Library declaration (e.g., "library work;")
-- Contract: spellcraft-adc-008 Section: Interface
data LibraryDeclaration = LibraryDeclaration
  { libName :: Text
  , libSourceLoc :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON LibraryDeclaration

-- | Use clause (e.g., "use work.all;")
-- Contract: spellcraft-adc-008 Section: Interface
data UseClause = UseClause
  { useLibrary :: Text
  , usePackage :: Text
  , useSourceLoc :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON UseClause

-- | A complete VHDL design (entities and architectures)
-- Contract: spellcraft-adc-001 Section: Interface
-- Enhanced: spellcraft-adc-008 Section: Interface (added libraries and uses)
data VHDLDesign = VHDLDesign
  { designLibraries :: [LibraryDeclaration]
  , designUses :: [UseClause]
  , designEntities :: [Entity]
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

-- | Signal declaration (e.g., "signal s_data : std_logic;")
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
data SignalDecl = SignalDecl
  { sigDeclName :: Identifier
  , sigDeclType :: TypeName
  , sigDeclLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON SignalDecl

-- | Architecture-level statement (processes, concurrent, component instantiations)
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
data ArchStatement
  = ProcessStmt
      { procName :: Maybe Identifier
      , procSensitivity :: [SignalName]
      , procStatements :: [Statement]
      , procLocation :: SourceLocation
      }
  | ConcurrentAssignment
      { concTarget :: SignalName
      , concSource :: SignalName
      , concLocation :: SourceLocation
      }
  | ComponentInstStmt ComponentInst
  deriving (Show, Eq, Generic)

instance ToJSON ArchStatement

-- | Sequential statement (inside processes)
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
data Statement
  = SignalAssignment
      { stmtTarget :: SignalName
      , stmtSource :: SignalName
      , stmtLocation :: SourceLocation
      }
  | IfStatement
      { stmtCondition :: SignalName
      , stmtThen :: [Statement]
      , stmtElse :: [Statement]
      , stmtLocation :: SourceLocation
      }
  deriving (Show, Eq, Generic)

instance ToJSON Statement

-- | Architecture with signals, processes, and component instantiations
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
data Architecture = Architecture
  { archName :: Identifier
  , archEntityName :: Identifier
  , archSignals :: [SignalDecl]
  , archStatements :: [ArchStatement]
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
