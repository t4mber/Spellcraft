{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-004
module VHDL.Analysis.Process
  ( -- * Process AST
    Process(..)
  , Statement(..)
  ) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import VHDL.AST (Identifier, SignalName)
import VHDL.Analysis.Expression (Expression)
import VHDL.SourceLocation (SourceLocation)

-- | VHDL process
-- Contract: vhdl-analyzer-adc-004 Section: Interface
data Process = Process
  { procName :: Maybe Identifier
  , procSensitivity :: [SignalName]
  , procStatements :: [Statement]
  , procLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON Process

-- | VHDL statements
data Statement
  = Assignment
      { assignTarget :: Identifier
      , assignExpr :: Expression
      , assignLocation :: SourceLocation
      }
  | IfStatement
      { ifCondition :: Expression
      , ifThen :: [Statement]
      , ifElse :: [Statement]
      , ifLocation :: SourceLocation
      }
  | ClockedAssignment
      { clockedTarget :: Identifier
      , clockedExpr :: Expression
      , clockedLocation :: SourceLocation
      }
  deriving (Show, Eq, Generic)

instance ToJSON Statement
