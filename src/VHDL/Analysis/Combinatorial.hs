{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: spellcraft-adc-004
module VHDL.Analysis.Combinatorial
  ( -- * Complexity Analysis
    CombinatorialPath(..)
  , ComplexityWarning(..)
  , analyzeProcessComplexity
  , findCombinatorialPaths
  , analyzeArchitectureComplexity
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import VHDL.AST (Architecture(..), Identifier)
import VHDL.Analysis.Expression (BinOp, Expression(..), calculatePathDepth, isArithmeticOp)
import VHDL.Analysis.Process (Process(..), Statement(..))
import VHDL.SourceLocation (SourceLocation)

-- | Combinatorial path in an expression
-- Contract: spellcraft-adc-004 Section: Interface
data CombinatorialPath = CombinatorialPath
  { cpExpression :: Expression
  , cpDepth :: Int
  , cpOperations :: [BinOp]
  , cpLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON CombinatorialPath

-- | Complexity warning
data ComplexityWarning = ComplexityWarning
  { cwProcess :: Maybe Identifier
  , cwPath :: CombinatorialPath
  , cwThreshold :: Int
  , cwMessage :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ComplexityWarning

-- | Analyze process for complexity warnings
-- Contract: spellcraft-adc-004 Section: Interface
analyzeProcessComplexity
  :: Process
  -> Int  -- Threshold
  -> [ComplexityWarning]
analyzeProcessComplexity proc threshold =
  let paths = concatMap extractPathsFromStatement (procStatements proc)
      violations = filter (\p -> cpDepth p > threshold) paths
  in map (mkWarning (procName proc) threshold) violations

-- | Find all combinatorial paths in an expression
-- Contract: spellcraft-adc-004 Section: Interface
findCombinatorialPaths :: Expression -> [CombinatorialPath]
findCombinatorialPaths expr =
  let depth = calculatePathDepth expr
      ops = extractOperations expr
      loc = undefined  -- Would need location tracking in Expression
  in if depth > 0
     then [CombinatorialPath expr depth ops (error "Location not tracked in prototype")]
     else []

-- | Analyze architecture for complexity
-- Contract: spellcraft-adc-004 Section: Interface
analyzeArchitectureComplexity
  :: Architecture
  -> Int  -- Threshold
  -> [ComplexityWarning]
analyzeArchitectureComplexity _arch _threshold = []
  -- Prototype: would extract processes from architecture

-- Extract operations from expression
extractOperations :: Expression -> [BinOp]
extractOperations (BinaryOp op left right) =
  if isArithmeticOp op
    then op : extractOperations left ++ extractOperations right
    else extractOperations left ++ extractOperations right
extractOperations (UnaryOp _ e) = extractOperations e
extractOperations (FunctionCall _ args) = concatMap extractOperations args
extractOperations _ = []

-- Extract paths from statement
extractPathsFromStatement :: Statement -> [CombinatorialPath]
extractPathsFromStatement (Assignment _ expr loc) =
  let depth = calculatePathDepth expr
      ops = extractOperations expr
  in if depth > 0
     then [CombinatorialPath expr depth ops loc]
     else []
extractPathsFromStatement (IfStatement _ thenStmts elseStmts _) =
  concatMap extractPathsFromStatement (thenStmts ++ elseStmts)
extractPathsFromStatement (ClockedAssignment _ _ _) = []

-- Create warning from path
mkWarning :: Maybe Identifier -> Int -> CombinatorialPath -> ComplexityWarning
mkWarning procName threshold path = ComplexityWarning
  { cwProcess = procName
  , cwPath = path
  , cwThreshold = threshold
  , cwMessage = T.pack $ "Combinatorial path has " <> show (cpDepth path)
                      <> " arithmetic operations (threshold: " <> show threshold <> ")"
  }
