-- ADC-IMPLEMENTS: vhdl-analyzer-adc-002
module VHDL.Constraint.Units
  ( -- * Frequency Units
    FreqUnit(..)
  , Frequency(..)
  , convertToMHz
  , mkFrequency
  , compareFrequencies
  ) where

-- | Frequency units
-- Contract: vhdl-analyzer-adc-002 Section: Interface
data FreqUnit
  = Hz
  | KHz
  | MHz
  | GHz
  deriving (Show, Eq, Enum, Bounded)

-- | Frequency value with unit
data Frequency = Frequency
  { freqValue :: Double
  , freqUnit :: FreqUnit
  } deriving (Show, Eq)

-- | Convert frequency to MHz for comparison
-- Contract: vhdl-analyzer-adc-002 Section: Interface
convertToMHz :: FreqUnit -> Double -> Double
convertToMHz Hz n = n / 1_000_000
convertToMHz KHz n = n / 1_000
convertToMHz MHz n = n
convertToMHz GHz n = n * 1_000

-- | Create a frequency value
mkFrequency :: Double -> FreqUnit -> Frequency
mkFrequency val unit = Frequency
  { freqValue = val
  , freqUnit = unit
  }

-- | Compare two frequencies (returns MHz values)
compareFrequencies :: Frequency -> Frequency -> Ordering
compareFrequencies f1 f2 =
  compare (convertToMHz (freqUnit f1) (freqValue f1))
          (convertToMHz (freqUnit f2) (freqValue f2))
