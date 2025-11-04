-- ADC-IMPLEMENTS: vhdl-analyzer-adc-003
module VHDL.Analysis.FrequencyCalc
  ( calculateOutputFrequency
  ) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import VHDL.AST (Identifier, Value(..))
import VHDL.Analysis.ClockGraph (AnalysisError(..))
import VHDL.Constraint.Types (ComponentSpec(..), GenericConstraint(..))
import VHDL.SourceLocation (mkSourceLocation)

-- | Calculate output frequency for a component given its generics and input frequency
-- Contract: vhdl-analyzer-adc-003 Section: Interface
calculateOutputFrequency
  :: ComponentSpec
  -> [(Identifier, Value)]  -- Generic map
  -> Double  -- Input frequency (MHz)
  -> Either AnalysisError Double
calculateOutputFrequency spec generics inputFreq =
  case compSpecName spec of
    "PLL_1" -> calculatePLLOutput generics inputFreq
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
