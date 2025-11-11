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
    -- * Expressions
  , Expression(..)
  , BinaryOp(..)
  , UnaryOp(..)
  , SliceDirection(..)
  , Literal(..)
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
-- ADC-IMPLEMENTS: spellcraft-adc-018
data SignalDecl = SignalDecl
  { sigDeclName :: Identifier
  , sigDeclType :: TypeName
  , sigDeclInitValue :: Maybe Expression  -- Optional initialization value
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
      , concExpr :: Expression  -- Changed from SignalName per ADC-013
      , concLocation :: SourceLocation
      }
  | ComponentInstStmt ComponentInst
  deriving (Show, Eq, Generic)

instance ToJSON ArchStatement

-- | Sequential statement (inside processes)
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
-- Enhanced: spellcraft-adc-013 Section: AST Extensions
data Statement
  = SignalAssignment
      { stmtTarget :: SignalName
      , stmtExpr :: Expression  -- Changed from SignalName per ADC-013
      , stmtLocation :: SourceLocation
      }
  | VariableAssignment
      { stmtVarTarget :: Identifier
      , stmtVarExpr :: Expression
      , stmtLocation :: SourceLocation
      }
  | IfStatement
      { stmtCondition :: Expression  -- Changed from SignalName per ADC-013
      , stmtThen :: [Statement]
      , stmtElsifs :: [(Expression, [Statement])]  -- Added per ADC-013
      , stmtElse :: [Statement]
      , stmtLocation :: SourceLocation
      }
  | CaseStatement
      { stmtCaseExpr :: Expression
      , stmtWhenClauses :: [(Expression, [Statement])]
      , stmtLocation :: SourceLocation
      }
  | LoopStatement
      { stmtLoopVar :: Maybe Identifier
      , stmtLoopRange :: Maybe (Expression, Expression)
      , stmtLoopBody :: [Statement]
      , stmtLocation :: SourceLocation
      }
  | WaitStatement
      { stmtWaitExpr :: Maybe Expression
      , stmtLocation :: SourceLocation
      }
  | NullStatement
      { stmtLocation :: SourceLocation
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

-- | VHDL Expressions (according to ADC-013, ADC-016, ADC-017)
-- Contract: spellcraft-adc-013 Section: AST Extensions
-- Contract: spellcraft-adc-016 Section: AST Extensions
-- Contract: spellcraft-adc-017 Section: AST Extensions
data Expression
  = IdentifierExpr Identifier
  | LiteralExpr Literal
  | BinaryExpr BinaryOp Expression Expression
  | UnaryExpr UnaryOp Expression
  | FunctionCall Identifier [Expression]
  | IndexedName Expression Expression
  | Aggregate [Expression]
  -- ADC-IMPLEMENTS: spellcraft-adc-016
  -- Attribute access: signal'event, arr'length, type'image(value)
  | AttributeExpr Expression Identifier [Expression]
  -- ADC-IMPLEMENTS: spellcraft-adc-017
  -- Slice/range indexing: signal(7 downto 0), arr(0 to 7)
  | SliceExpr Expression Expression Expression SliceDirection
  deriving (Show, Eq, Generic)

instance ToJSON Expression

-- | Binary operators
data BinaryOp
  = Add | Sub | Mul | Div | Mod | Rem
  | And | Or | Xor | Nand | Nor
  | Eq | NEq | Lt | Gt | LEq | GEq
  | Concat
  deriving (Show, Eq, Generic)

instance ToJSON BinaryOp

-- | Unary operators
data UnaryOp
  = Not | Negate
  deriving (Show, Eq, Generic)

instance ToJSON UnaryOp

-- ADC-IMPLEMENTS: spellcraft-adc-017
-- | Slice direction for range indexing
data SliceDirection
  = DownTo  -- signal(7 downto 0)
  | To      -- signal(0 to 7)
  deriving (Show, Eq, Generic)

instance ToJSON SliceDirection

-- | Literal values in expressions
data Literal
  = IntLiteral Integer
  | RealLiteral Double
  | StringLiteral Text
  | CharLiteral Char
  | BitLiteral Bool
  deriving (Show, Eq, Generic)

instance ToJSON Literal
