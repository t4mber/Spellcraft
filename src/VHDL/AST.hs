{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- | Partial fields are intentional in this module - sum types like Expression,
-- | Statement, and ArchStatement have constructors with different record fields.

-- ADC-IMPLEMENTS: spellcraft-adc-001
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- ADC-IMPLEMENTS: spellcraft-adc-012
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- ADC-IMPLEMENTS: spellcraft-adc-029
module VHDL.AST
  ( -- * Design
    VHDLDesign(..)
    -- * Multi-File Analysis Context (ADC-029)
  , AnalysisContext(..)
  , emptyContext
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
    -- * Generate Statements (ADC-028)
  , GenerateStatement(..)
  , GenerateScheme(..)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
-- Enhanced: spellcraft-adc-022 Section: Indexed Assignments
-- Enhanced: spellcraft-adc-028 Section: Generate Statements
data ArchStatement
  = ProcessStmt
      { procName :: Maybe Identifier
      , procSensitivity :: [SignalName]
      , procStatements :: [Statement]
      , procLocation :: SourceLocation
      }
  | ConcurrentAssignment
      { concTarget :: Expression  -- Changed from SignalName to Expression (ADC-022)
      , concExpr :: Expression  -- Changed from SignalName per ADC-013
      , concLocation :: SourceLocation
      }
  | ComponentInstStmt ComponentInst
  -- ADC-IMPLEMENTS: spellcraft-adc-028
  | GenerateStmt GenerateStatement
  deriving (Show, Eq, Generic)

instance ToJSON ArchStatement

-- | Sequential statement (inside processes)
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
-- Enhanced: spellcraft-adc-013 Section: AST Extensions
-- Enhanced: spellcraft-adc-022 Section: Indexed Assignments
data Statement
  = SignalAssignment
      { stmtTarget :: Expression  -- Changed from SignalName to Expression (ADC-022)
      , stmtExpr :: Expression  -- Changed from SignalName per ADC-013
      , stmtLocation :: SourceLocation
      }
  | VariableAssignment
      { stmtVarTarget :: Expression  -- Changed from Identifier to Expression (ADC-022)
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
      , stmtLoopRange :: Maybe (Expression, Expression, SliceDirection)  -- ADC-027: Added direction for downto support
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
-- ADC-IMPLEMENTS: spellcraft-adc-020
-- ADC-IMPLEMENTS: spellcraft-adc-021
-- ADC-IMPLEMENTS: spellcraft-adc-027
data ComponentInst = ComponentInst
  { compInstName :: Identifier
  , compComponentName :: Identifier
  , compGenericMap :: [(Identifier, Expression)]  -- Changed from Value to Expression (ADC-020)
  , compPortMap :: [(Expression, Expression)]     -- ADC-027: Changed from Identifier to Expression to support indexed formals (a(0) => sig)
  , compLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON ComponentInst

-- | Generate statement (for-generate and if-generate)
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- IEEE 1076-2008 Section 11.8
-- Patterns:
--   gen_label: for i in 0 to N-1 generate ... end generate;
--   gen_label: if CONDITION generate ... end generate;
data GenerateStatement = GenerateStatement
  { genLabel :: Identifier
  , genScheme :: GenerateScheme
  , genStatements :: [ArchStatement]
  , genLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON GenerateStatement

-- | Generate scheme (for-generate or if-generate)
-- ADC-IMPLEMENTS: spellcraft-adc-028
data GenerateScheme
  = ForGenerate Identifier Expression Expression SliceDirection
    -- ^ for i in start to/downto end
  | IfGenerate Expression
    -- ^ if condition
  deriving (Show, Eq, Generic)

instance ToJSON GenerateScheme

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

-- | VHDL Expressions (according to ADC-013, ADC-016, ADC-017, ADC-026)
-- Contract: spellcraft-adc-013 Section: AST Extensions
-- Contract: spellcraft-adc-016 Section: AST Extensions
-- Contract: spellcraft-adc-017 Section: AST Extensions
-- Contract: spellcraft-adc-026 Section: AST Extensions
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
  -- ADC-IMPLEMENTS: spellcraft-adc-026
  -- Selected name (record field access): signal.field, record.field1.field2
  | SelectedName Expression Identifier
  -- ADC-IMPLEMENTS: spellcraft-adc-027
  -- Conditional expression: value when condition else default
  -- Used in concurrent conditional signal assignments
  | ConditionalExpr Expression Expression Expression  -- value, condition, else_value
  deriving (Show, Eq, Generic)

instance ToJSON Expression

-- | Binary operators
-- ADC-IMPLEMENTS: spellcraft-adc-019
data BinaryOp
  = Add | Sub | Mul | Div | Mod | Rem | Pow  -- Pow = ** exponentiation
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
-- ADC-IMPLEMENTS: spellcraft-adc-008
data Literal
  = IntLiteral Integer
  | RealLiteral Double
  | StringLiteral Text
  | CharLiteral Char
  | BitLiteral Bool
  | BasedLiteral Text  -- VHDL based literals: x"BEEF", b"1010", o"777"
  deriving (Show, Eq, Generic)

instance ToJSON Literal

-- =============================================================================
-- ADC-029: Multi-File Analysis Context
-- =============================================================================

-- | Multi-file analysis context for resolving cross-file references
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- When analyzing multiple VHDL files together, this context provides
-- information about entities and architectures defined in other files,
-- allowing signal usage analysis to correctly identify component outputs.
data AnalysisContext = AnalysisContext
  { ctxEntities :: Map Identifier Entity
    -- ^ All known entities indexed by name (without library prefix)
  , ctxArchitectures :: Map (Identifier, Identifier) Architecture
    -- ^ Architectures indexed by (archName, entityName)
  , ctxDesigns :: [VHDLDesign]
    -- ^ Original parsed designs for reference
  , ctxSourceFiles :: [FilePath]
    -- ^ Source files included in context
  } deriving (Show, Eq)

-- | Empty context for single-file analysis or as starting point
-- ADC-IMPLEMENTS: spellcraft-adc-029
emptyContext :: AnalysisContext
emptyContext = AnalysisContext
  { ctxEntities = Map.empty
  , ctxArchitectures = Map.empty
  , ctxDesigns = []
  , ctxSourceFiles = []
  }
