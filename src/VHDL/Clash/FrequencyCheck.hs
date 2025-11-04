{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-006
module VHDL.Clash.FrequencyCheck
  ( -- * Type-Level Frequency Constraints
    CheckMaxFreq
  , CheckMinFreq
  , CheckFreqRange
    -- * Frequency Comparison
  , FreqLTE
  , FreqGTE
  , FreqEQ
    -- * Connection Functions
  , connectPLL
  , connectEncoder
  , connectClockDivider
    -- * Validation Functions
  , validateFrequency
  , FrequencyCheckResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (Nat, KnownNat)
import GHC.TypeNats (Div, type (<=?))
import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import Data.Type.Bool (type (&&))
import VHDL.Clash.Types
  ( FreqMHz
  , FreqMult
  , FreqDiv
  , ClockDomain(..)
  , HWSignal(..)
  , PLL(..)
  , Encoder(..)
  , mkHWSignal
  , natToInteger
  )
import VHDL.Constraint.Types (ConstraintViolation(..))
import VHDL.SourceLocation (SourceLocation, mkSourceLocation)

-- | Type-level constraint: actual frequency must be <= max frequency
-- Contract: vhdl-analyzer-adc-006 Section: Type-Level Constraint Checking
type family CheckMaxFreq (actual :: FreqMHz) (max :: FreqMHz) :: Constraint where
  CheckMaxFreq actual max = (actual <=? max) ~ 'True

-- | Type-level constraint: actual frequency must be >= min frequency
type family CheckMinFreq (actual :: FreqMHz) (min :: FreqMHz) :: Constraint where
  CheckMinFreq actual min = (min <=? actual) ~ 'True

-- | Type-level constraint: frequency must be within range
type family CheckFreqRange (actual :: FreqMHz) (min :: FreqMHz) (max :: FreqMHz) :: Constraint where
  CheckFreqRange actual min max = (CheckMinFreq actual min, CheckMaxFreq actual max)

-- | Type-level frequency comparison: less than or equal
type family FreqLTE (f1 :: FreqMHz) (f2 :: FreqMHz) :: Bool where
  FreqLTE f1 f2 = f1 <=? f2

-- | Type-level frequency comparison: greater than or equal
type family FreqGTE (f1 :: FreqMHz) (f2 :: FreqMHz) :: Bool where
  FreqGTE f1 f2 = f2 <=? f1

-- | Type-level frequency comparison: equal
type family FreqEQ (f1 :: FreqMHz) (f2 :: FreqMHz) :: Bool where
  FreqEQ f1 f2 = (f1 <=? f2) && (f2 <=? f1)

-- | Result of frequency validation
data FrequencyCheckResult
  = FrequencyOK
  | FrequencyTooHigh
    { actualFreq :: Integer
    , maxFreq :: Integer
    }
  | FrequencyTooLow
    { actualFreq :: Integer
    , minFreq :: Integer
    }
  deriving (Show, Eq)

-- | Safe PLL connection with type-level frequency checking
-- The output frequency must equal input frequency times the multiplication factor
-- Contract: vhdl-analyzer-adc-006 Section: Safe Connection Functions
connectPLL :: forall inFreq factor outFreq a.
              (KnownNat inFreq, KnownNat factor, KnownNat outFreq,
               outFreq ~ FreqMult inFreq factor)
           => PLL inFreq factor
           -> HWSignal inFreq a
           -> HWSignal outFreq a
connectPLL pll inputSignal = HWSignal
  { hwSignalName = hwSignalName inputSignal <> "_pll_out"
  , hwSignalDomain = ClockDomain
      { domainName = domainName (hwSignalDomain inputSignal) <> "_pll"
      , domainFreqMHz = natToInteger @outFreq
      }
  , hwSignalMetadata = Just $ T.pack $ "PLL multiplied by " ++ show (natToInteger @factor)
  }

-- | Safe encoder connection with frequency constraint checking
-- Returns Either to indicate constraint violation at runtime
-- Contract: vhdl-analyzer-adc-006 Section: Safe Connection Functions
connectEncoder :: forall freq maxFreq a.
                  (KnownNat freq, KnownNat maxFreq, CheckMaxFreq freq maxFreq)
               => Encoder maxFreq
               -> HWSignal freq a
               -> Either ConstraintViolation (HWSignal freq a)
connectEncoder encoder signal =
  let actual = natToInteger @freq
      max' = natToInteger @maxFreq
  in if actual <= max'
     then Right $ signal
       { hwSignalMetadata = Just $ T.pack $ "Encoded by " ++ T.unpack (encoderName encoder)
       }
     else Left $ FrequencyViolation
       { violationComponent = encoderName encoder
       , violationPort = hwSignalName signal
       , violationActual = fromIntegral actual
       , violationMax = fromIntegral max'
       , violationLocation = mkSourceLocation "<type-level-check>" 0 0
       }

-- | Safe clock divider connection
-- Divides the frequency by a compile-time constant
connectClockDivider :: forall inFreq divisor outFreq a.
                       (KnownNat inFreq, KnownNat divisor, KnownNat outFreq,
                        outFreq ~ FreqDiv inFreq divisor)
                    => Integer  -- divisor value (for runtime info)
                    -> HWSignal inFreq a
                    -> HWSignal outFreq a
connectClockDivider divisor inputSignal = HWSignal
  { hwSignalName = hwSignalName inputSignal <> "_div"
  , hwSignalDomain = ClockDomain
      { domainName = domainName (hwSignalDomain inputSignal) <> "_divided"
      , domainFreqMHz = natToInteger @outFreq
      }
  , hwSignalMetadata = Just $ T.pack $ "Divided by " ++ show divisor
  }

-- | Runtime validation of frequency against maximum
-- Used when type-level checking is not available
validateFrequency :: forall freq maxFreq.
                     (KnownNat freq, KnownNat maxFreq)
                  => Proxy freq
                  -> Proxy maxFreq
                  -> FrequencyCheckResult
validateFrequency _ _ =
  let actual = natToInteger @freq
      max' = natToInteger @maxFreq
  in if actual <= max'
     then FrequencyOK
     else FrequencyTooHigh
       { actualFreq = actual
       , maxFreq = max'
       }
