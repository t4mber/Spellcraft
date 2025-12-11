{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- ADC-IMPLEMENTS: spellcraft-adc-006
module VHDL.Clash.Domains
  ( -- * Clock Domain Management
    DomainRegistry
  , emptyRegistry
  , registerDomain
  , lookupDomain
  , listDomains
    -- * Domain Relationships
  , DomainRelation(..)
  , relateDomains
  , checkDomainCompatibility
    -- * Domain Crossing
  , CrossingStrategy(..)
  , DomainCrossing(..)
  , createCrossing
  , validateCrossing
    -- * Predefined Domains
  , System
  , Fast
  , Slow
  , mkSystemDomain
  , mkFastDomain
  , mkSlowDomain
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import VHDL.Clash.Types
  ( FreqMHz
  , ClockDomain(..)
  , mkClockDomain
  )

-- | Registry for managing clock domains
-- Maps domain names to their frequency information
-- Contract: spellcraft-adc-006 Section: Clock Domain Management
newtype DomainRegistry = DomainRegistry
  { unRegistry :: Map Text (Text, Integer)  -- name -> (description, freq)
  } deriving (Show, Eq)

-- | Create an empty domain registry
emptyRegistry :: DomainRegistry
emptyRegistry = DomainRegistry Map.empty

-- | Register a clock domain in the registry
-- Note: freq type parameter used for type applications in tests
registerDomain :: forall freq. Text  -- domain name
               -> Text  -- description
               -> ClockDomain freq
               -> DomainRegistry
               -> DomainRegistry
registerDomain name desc domain (DomainRegistry reg) =
  DomainRegistry $ Map.insert name (desc, domainFreqMHz domain) reg

-- | Look up a domain by name
lookupDomain :: Text -> DomainRegistry -> Maybe (Text, Integer)
lookupDomain name (DomainRegistry reg) = Map.lookup name reg

-- | List all registered domains
listDomains :: DomainRegistry -> [(Text, Text, Integer)]
listDomains (DomainRegistry reg) =
  [(name, desc, freq) | (name, (desc, freq)) <- Map.toList reg]

-- | Relationship between two clock domains
-- Contract: spellcraft-adc-006 Section: Domain Relationships
data DomainRelation
  = Synchronous     -- Same frequency, phase-aligned
  | Rational        -- Frequencies are rational multiples
  | Asynchronous    -- Completely unrelated frequencies
  deriving (Show, Eq)

-- | Determine the relationship between two clock domains
relateDomains :: ClockDomain freq1
              -> ClockDomain freq2
              -> DomainRelation
relateDomains domain1 domain2 =
  let f1 = domainFreqMHz domain1
      f2 = domainFreqMHz domain2
  in if f1 == f2
     then Synchronous
     else if isRationalMultiple f1 f2
          then Rational
          else Asynchronous

-- | Check if two frequencies are rational multiples
isRationalMultiple :: Integer -> Integer -> Bool
isRationalMultiple f1 f2 =
  let g = gcd f1 f2
  in g > 1

-- | Check if two domains can be safely connected
checkDomainCompatibility :: ClockDomain freq1
                         -> ClockDomain freq2
                         -> (Bool, Text)
checkDomainCompatibility domain1 domain2 =
  case relateDomains domain1 domain2 of
    Synchronous ->
      (True, "Domains are synchronous - direct connection safe")
    Rational ->
      (True, "Domains have rational frequency relationship - crossing required")
    Asynchronous ->
      (False, "Domains are asynchronous - careful synchronization required")

-- | Clock domain crossing strategy
-- Contract: spellcraft-adc-006 Section: Domain Crossing
data CrossingStrategy
  = DirectCrossing      -- For synchronous domains
  | FIFOCrossing Int    -- FIFO-based crossing with depth
  | HandshakeCrossing   -- Request-acknowledge protocol
  deriving (Show, Eq)

-- | Clock domain crossing specification
data DomainCrossing (srcFreq :: FreqMHz) (dstFreq :: FreqMHz) = DomainCrossing
  { crossingName :: Text
  , crossingSource :: ClockDomain srcFreq
  , crossingDestination :: ClockDomain dstFreq
  , crossingStrategy :: CrossingStrategy
  , crossingVerified :: Bool
  } deriving (Show, Eq)

-- | Create a domain crossing
createCrossing :: Text
               -> ClockDomain srcFreq
               -> ClockDomain dstFreq
               -> CrossingStrategy
               -> DomainCrossing srcFreq dstFreq
createCrossing name srcDomain dstDomain strategy = DomainCrossing
  { crossingName = name
  , crossingSource = srcDomain
  , crossingDestination = dstDomain
  , crossingStrategy = strategy
  , crossingVerified = False  -- Must be verified separately
  }

-- | Validate a domain crossing
validateCrossing :: DomainCrossing srcFreq dstFreq
                 -> Either Text (DomainCrossing srcFreq dstFreq)
validateCrossing crossing =
  let (compatible, msg) = checkDomainCompatibility
                           (crossingSource crossing)
                           (crossingDestination crossing)
      relation = relateDomains (crossingSource crossing) (crossingDestination crossing)
  in case (compatible, crossingStrategy crossing, relation) of
    (True, DirectCrossing, Synchronous) ->
      Right $ crossing { crossingVerified = True }
    (True, FIFOCrossing _, Rational) ->
      Right $ crossing { crossingVerified = True }
    (True, HandshakeCrossing, _) ->
      Right $ crossing { crossingVerified = True }
    (False, _, _) ->
      Left $ "Invalid crossing: " <> msg
    _ ->
      Left $ "Strategy " <> T.pack (show $ crossingStrategy crossing)
          <> " incompatible with " <> T.pack (show relation) <> " domains"

-- | Predefined standard clock domain types
-- Contract: spellcraft-adc-006 Section: Predefined Domains

-- | System clock domain (100 MHz)
type System = 100

-- | Fast clock domain (400 MHz)
type Fast = 400

-- | Slow clock domain (25 MHz)
type Slow = 25

-- | Create a system clock domain (100 MHz)
mkSystemDomain :: ClockDomain System
mkSystemDomain = mkClockDomain @System "system"

-- | Create a fast clock domain (400 MHz)
mkFastDomain :: ClockDomain Fast
mkFastDomain = mkClockDomain @Fast "fast"

-- | Create a slow clock domain (25 MHz)
mkSlowDomain :: ClockDomain Slow
mkSlowDomain = mkClockDomain @Slow "slow"
