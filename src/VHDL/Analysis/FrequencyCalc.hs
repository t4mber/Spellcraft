-- ADC-IMPLEMENTS: spellcraft-adc-003
module VHDL.Analysis.FrequencyCalc
  ( calculateOutputFrequency
  ) where

import Data.List (find)
import qualified Data.Text as T
import VHDL.AST (Identifier, Value(..), Expression(..), Literal(..))
import VHDL.Analysis.ClockGraph (AnalysisError(..))
import VHDL.SourceLocation (mkSourceLocation)
import VHDL.Constraint.Types (ComponentSpec(..))

-- | Calculate output frequency for a component given its generics and input frequency
-- Contract: spellcraft-adc-003 Section: Interface
-- ADC-IMPLEMENTS: spellcraft-adc-020
-- Convert Expression to Value for frequency calculation (simple literals only)
exprToValue :: Expression -> Maybe Value
exprToValue (LiteralExpr (IntLiteral i)) = Just (IntValue i)
exprToValue (LiteralExpr (RealLiteral r)) = Just (RealValue r)
exprToValue (LiteralExpr (StringLiteral s)) = Just (StringValue s)
exprToValue (IdentifierExpr ident) = Just (IdentifierValue ident)
exprToValue _ = Nothing  -- Complex expressions not yet supported

calculateOutputFrequency
  :: ComponentSpec
  -> [(Identifier, Expression)]  -- Generic map (changed from Value)
  -> Double  -- Input frequency (MHz)
  -> Either AnalysisError Double
calculateOutputFrequency spec generics inputFreq =
  -- Convert expressions to values (filter out complex ones)
  let valueGenerics = [(name, val) | (name, expr) <- generics, Just val <- [exprToValue expr]]
  in case compSpecName spec of
    "PLL_1" -> calculatePLLOutput valueGenerics inputFreq
    "YPbPr_Encoder_A" -> pure inputFreq  -- Pass-through
    _ -> pure inputFreq  -- Default: pass-through

-- | Calculate PLL output frequency based on MULT_FACTOR
calculatePLLOutput :: [(Identifier, Value)] -> Double -> Either AnalysisError Double
calculatePLLOutput generics inputFreq =
  case find (\(name, _) -> name == "MULT_FACTOR") generics of
    Nothing -> Left $ MissingGeneric "PLL_1" "MULT_FACTOR" (mkSourceLocation "unknown" 0 0)
    Just (_, RealValue factor) -> pure (inputFreq * factor)
    Just (_, IntValue factor) -> pure (inputFreq * fromIntegral factor)
    Just (_, val) -> Left $ InvalidGenericValue "PLL_1" (T.pack $ show val) (mkSourceLocation "unknown" 0 0)
