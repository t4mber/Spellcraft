{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

-- ADC-IMPLEMENTS: spellcraft-adc-006
module VHDL.Clash.Types
  ( -- * Type-Level Frequency Types
    FreqMHz
  , FreqMult
  , FreqDiv
    -- * Clock Domain Types
  , ClockDomain(..)
  , mkClockDomain
    -- * Hardware Signal Types
  , HWSignal(..)
  , mkHWSignal
    -- * PLL Types
  , PLL(..)
  , mkPLL
    -- * Encoder Types
  , Encoder(..)
  , mkEncoder
    -- * Type-Level Utilities
  , natToInteger
  ) where

import Data.Text (Text)
import GHC.TypeLits (Nat, KnownNat, natVal)
import GHC.TypeNats (Div)
import Data.Proxy (Proxy(..))
import qualified GHC.TypeNats as TN

-- | Frequency in MHz as type-level natural
-- Contract: spellcraft-adc-006 Section: Type-Level Hardware Types
type FreqMHz = Nat

-- | Type family for frequency multiplication
-- Used for PLL frequency scaling
-- Contract: spellcraft-adc-006 Section: Type Families
-- Note: Uses GHC.TypeNats built-in multiplication
type FreqMult f m = f TN.* m

-- | Type family for frequency division
-- Used for clock dividers
-- Contract: spellcraft-adc-006 Section: Type Families
type family FreqDiv (f :: FreqMHz) (d :: Nat) :: FreqMHz where
  FreqDiv f d = Div f d

-- | Clock domain with frequency constraint
-- The frequency is encoded at the type level for compile-time checking
-- Contract: spellcraft-adc-006 Section: Clock Domain Types
data ClockDomain (freq :: FreqMHz) = ClockDomain
  { domainName :: Text
  , domainFreqMHz :: Integer
  } deriving (Show, Eq)

-- | Smart constructor for ClockDomain
-- Automatically extracts the type-level frequency
mkClockDomain :: forall freq. KnownNat freq => Text -> ClockDomain freq
mkClockDomain name = ClockDomain
  { domainName = name
  , domainFreqMHz = natVal (Proxy :: Proxy freq)
  }

-- | Clash Signal with frequency-tagged domain
-- Combines a signal name with its clock domain
-- Contract: spellcraft-adc-006 Section: Hardware Signal Types
data HWSignal (freq :: FreqMHz) a = HWSignal
  { hwSignalName :: Text
  , hwSignalDomain :: ClockDomain freq
  , hwSignalMetadata :: Maybe Text
  } deriving (Show, Eq)

-- | Smart constructor for HWSignal
mkHWSignal :: forall freq a. Text
           -> ClockDomain freq
           -> HWSignal freq a
mkHWSignal name domain = HWSignal
  { hwSignalName = name
  , hwSignalDomain = domain
  , hwSignalMetadata = Nothing
  }

-- | Phase-Locked Loop (PLL) component
-- Takes an input frequency and multiplies it by a factor
-- Contract: spellcraft-adc-006 Section: PLL Types
data PLL (inFreq :: FreqMHz) (factor :: Nat) = PLL
  { pllName :: Text
  , pllInputFreq :: Integer
  , pllMultiplier :: Integer
  , pllOutputFreq :: Integer
  } deriving (Show, Eq)

-- | Smart constructor for PLL
-- Validates that input and output frequencies are correctly related
-- Note: outFreq type parameter is intentionally used for compile-time frequency verification
mkPLL :: forall inFreq factor outFreq.
         (KnownNat inFreq, KnownNat factor, KnownNat outFreq,
          outFreq ~ FreqMult inFreq factor)
      => Text
      -> PLL inFreq factor
mkPLL name = PLL
  { pllName = name
  , pllInputFreq = natVal (Proxy :: Proxy inFreq)
  , pllMultiplier = natVal (Proxy :: Proxy factor)
  , pllOutputFreq = natVal (Proxy :: Proxy outFreq)
  }

-- | Encoder component with maximum frequency constraint
-- Contract: spellcraft-adc-006 Section: Encoder Types
data Encoder (maxFreq :: FreqMHz) = Encoder
  { encoderName :: Text
  , encoderMaxFreqMHz :: Integer
  , encoderBitWidth :: Int
  } deriving (Show, Eq)

-- | Smart constructor for Encoder
mkEncoder :: forall maxFreq. KnownNat maxFreq
          => Text
          -> Int
          -> Encoder maxFreq
mkEncoder name bitWidth = Encoder
  { encoderName = name
  , encoderMaxFreqMHz = natVal (Proxy :: Proxy maxFreq)
  , encoderBitWidth = bitWidth
  }

-- | Utility to convert type-level Nat to runtime Integer
natToInteger :: forall n. KnownNat n => Integer
natToInteger = natVal (Proxy :: Proxy n)
