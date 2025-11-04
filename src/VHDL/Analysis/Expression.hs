{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: spellcraft-adc-004
module VHDL.Analysis.Expression
  ( -- * Expressions
    Expression(..)
  , BinOp(..)
  , UnOp(..)
    -- * Utilities
  , isArithmeticOp
  , calculatePathDepth
  ) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import VHDL.AST (Identifier, Value)

-- | VHDL expressions
-- Contract: spellcraft-adc-004 Section: Interface
data Expression
  = Literal Value
  | Variable Identifier
  | BinaryOp BinOp Expression Expression
  | UnaryOp UnOp Expression
  | FunctionCall Identifier [Expression]
  deriving (Show, Eq, Generic)

instance ToJSON Expression

-- | Binary operators
data BinOp
  = Add | Sub | Mul | Div | Mod
  | And | Or | Xor
  | Eq | Neq | Lt | Le | Gt | Ge
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON BinOp

-- | Unary operators
data UnOp
  = Negate
  | Not
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON UnOp

-- | Check if an operator is arithmetic
-- Contract: spellcraft-adc-004 Section: Interface
isArithmeticOp :: BinOp -> Bool
isArithmeticOp op = op `elem` [Add, Sub, Mul, Div, Mod]

-- | Calculate combinatorial path depth
-- Contract: spellcraft-adc-004 Section: Interface
calculatePathDepth :: Expression -> Int
calculatePathDepth (Literal _) = 0
calculatePathDepth (Variable _) = 0
calculatePathDepth (UnaryOp _ e) = calculatePathDepth e
calculatePathDepth (FunctionCall _ args) = maximum (0 : map calculatePathDepth args)
calculatePathDepth (BinaryOp op left right)
  | isArithmeticOp op = 1 + max (calculatePathDepth left) (calculatePathDepth right)
  | otherwise = max (calculatePathDepth left) (calculatePathDepth right)
