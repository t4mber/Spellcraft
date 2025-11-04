{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-006
module VHDL.Clash.Constraints
  ( -- * Type-Level Constraint Checking
    HardwareConstraint
  , FrequencyConstraint
  , PowerConstraint
    -- * Constraint Types
  , ConstraintCheck(..)
  , ConstraintResult(..)
    -- * Constraint Validation
  , validateHardwareConstraints
  , checkFrequencyConstraint
  , checkPowerConstraint
    -- * Constraint Combinators
  , (&&&)
  , (|||)
  , satisfiesAll
  , satisfiesAny
    -- * Integration with Existing Types
  , toViolation
  , fromComponentSpec
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (Nat, KnownNat)
import GHC.TypeNats (type (<=?))
import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import VHDL.Clash.Types
  ( FreqMHz
  , ClockDomain(..)
  , HWSignal(..)
  , Encoder(..)
  , natToInteger
  )
import VHDL.Clash.FrequencyCheck
  ( CheckMaxFreq
  , CheckMinFreq
  , FrequencyCheckResult(..)
  )
import VHDL.Constraint.Types
  ( ComponentSpec(..)
  , PortConstraint(..)
  , ConstraintViolation(..)
  )
import VHDL.SourceLocation (SourceLocation, mkSourceLocation)

-- | General hardware constraint kind
-- Contract: vhdl-analyzer-adc-006 Section: Type-Level Constraint Checking
type HardwareConstraint = Constraint

-- | Frequency constraint: signal frequency must be within bounds
type FrequencyConstraint (freq :: FreqMHz) (min :: FreqMHz) (max :: FreqMHz) =
  (CheckMinFreq freq min, CheckMaxFreq freq max)

-- | Power constraint: frequency must be below power-limited maximum
-- Power typically scales with frequency, so this enforces max frequency
type PowerConstraint (freq :: FreqMHz) (maxFreq :: FreqMHz) =
  CheckMaxFreq freq maxFreq

-- | Runtime constraint check result
-- Contract: vhdl-analyzer-adc-006 Section: Constraint Validation
data ConstraintCheck
  = FrequencyCheck
    { checkName :: Text
    , checkActual :: Integer
    , checkMin :: Maybe Integer
    , checkMax :: Maybe Integer
    }
  | PowerCheck
    { checkName :: Text
    , checkActual :: Integer
    , checkMaxPower :: Integer
    }
  | CustomCheck
    { checkName :: Text
    , checkDescription :: Text
    }
  deriving (Show, Eq)

-- | Result of constraint validation
data ConstraintResult
  = ConstraintSatisfied
    { resultCheck :: ConstraintCheck
    }
  | ConstraintViolated
    { resultCheck :: ConstraintCheck
    , resultReason :: Text
    }
  deriving (Show, Eq)

-- | Validate all hardware constraints for a signal
-- Contract: vhdl-analyzer-adc-006 Section: Constraint Validation
validateHardwareConstraints :: forall freq a.
                               KnownNat freq
                            => HWSignal freq a
                            -> [ConstraintCheck]
                            -> [ConstraintResult]
validateHardwareConstraints signal checks =
  map (validateSingleCheck signal) checks

-- | Validate a single constraint check
validateSingleCheck :: forall freq a.
                       KnownNat freq
                    => HWSignal freq a
                    -> ConstraintCheck
                    -> ConstraintResult
validateSingleCheck signal check =
  let actualFreq = natToInteger @freq
  in case check of
    FrequencyCheck name actual minM maxM ->
      if actualFreq /= actual
      then ConstraintViolated check "Frequency mismatch"
      else case (minM, maxM) of
        (Just minF, Just maxF) ->
          if actualFreq >= minF && actualFreq <= maxF
          then ConstraintSatisfied check
          else ConstraintViolated check $
            "Frequency " <> T.pack (show actualFreq)
            <> " MHz outside range [" <> T.pack (show minF)
            <> ", " <> T.pack (show maxF) <> "] MHz"
        (Just minF, Nothing) ->
          if actualFreq >= minF
          then ConstraintSatisfied check
          else ConstraintViolated check $
            "Frequency " <> T.pack (show actualFreq)
            <> " MHz below minimum " <> T.pack (show minF) <> " MHz"
        (Nothing, Just maxF) ->
          if actualFreq <= maxF
          then ConstraintSatisfied check
          else ConstraintViolated check $
            "Frequency " <> T.pack (show actualFreq)
            <> " MHz above maximum " <> T.pack (show maxF) <> " MHz"
        (Nothing, Nothing) ->
          ConstraintSatisfied check
    PowerCheck name actual maxPower ->
      if actualFreq <= maxPower
      then ConstraintSatisfied check
      else ConstraintViolated check $
        "Frequency " <> T.pack (show actualFreq)
        <> " MHz exceeds power limit " <> T.pack (show maxPower) <> " MHz"
    CustomCheck _ _ ->
      ConstraintSatisfied check  -- Custom checks require external validation

-- | Check frequency constraint for a signal
checkFrequencyConstraint :: forall freq minFreq maxFreq a.
                            (KnownNat freq, KnownNat minFreq, KnownNat maxFreq,
                             FrequencyConstraint freq minFreq maxFreq)
                         => HWSignal freq a
                         -> Either ConstraintViolation ()
checkFrequencyConstraint signal =
  let actual = natToInteger @freq
      minF = natToInteger @minFreq
      maxF = natToInteger @maxFreq
  in if actual >= minF && actual <= maxF
     then Right ()
     else Left $ FrequencyViolation
       { violationComponent = T.pack "HWSignal"
       , violationPort = hwSignalName signal
       , violationActual = fromIntegral actual
       , violationMax = fromIntegral maxF
       , violationLocation = mkSourceLocation "<type-level-check>" 0 0
       }

-- | Check power constraint for a signal
checkPowerConstraint :: forall freq maxFreq a.
                        (KnownNat freq, KnownNat maxFreq,
                         PowerConstraint freq maxFreq)
                     => HWSignal freq a
                     -> Either ConstraintViolation ()
checkPowerConstraint signal =
  let actual = natToInteger @freq
      maxF = natToInteger @maxFreq
  in if actual <= maxF
     then Right ()
     else Left $ FrequencyViolation
       { violationComponent = T.pack "PowerCheck"
       , violationPort = hwSignalName signal
       , violationActual = fromIntegral actual
       , violationMax = fromIntegral maxF
       , violationLocation = mkSourceLocation "<type-level-check>" 0 0
       }

-- | Constraint combinator: both constraints must be satisfied (AND)
(&&&) :: ConstraintResult -> ConstraintResult -> ConstraintResult
(&&&) (ConstraintSatisfied _) (ConstraintSatisfied c2) = ConstraintSatisfied c2
(&&&) (ConstraintViolated c1 r1) _ = ConstraintViolated c1 r1
(&&&) _ (ConstraintViolated c2 r2) = ConstraintViolated c2 r2

-- | Constraint combinator: at least one constraint must be satisfied (OR)
(|||) :: ConstraintResult -> ConstraintResult -> ConstraintResult
(|||) (ConstraintSatisfied c1) _ = ConstraintSatisfied c1
(|||) _ (ConstraintSatisfied c2) = ConstraintSatisfied c2
(|||) (ConstraintViolated c1 r1) (ConstraintViolated _ r2) =
  ConstraintViolated c1 (r1 <> " AND " <> r2)

-- | Check if all constraints are satisfied
satisfiesAll :: [ConstraintResult] -> Bool
satisfiesAll = all isSatisfied
  where
    isSatisfied (ConstraintSatisfied _) = True
    isSatisfied _ = False

-- | Check if any constraint is satisfied
satisfiesAny :: [ConstraintResult] -> Bool
satisfiesAny = any isSatisfied
  where
    isSatisfied (ConstraintSatisfied _) = True
    isSatisfied _ = False

-- | Convert a ConstraintResult to a ConstraintViolation
-- Contract: vhdl-analyzer-adc-006 Section: Integration
toViolation :: ConstraintResult -> Maybe ConstraintViolation
toViolation (ConstraintSatisfied _) = Nothing
toViolation (ConstraintViolated check reason) =
  Just $ case check of
    FrequencyCheck name actual _ (Just maxF) ->
      FrequencyViolation
        { violationComponent = name
        , violationPort = T.pack "unknown"
        , violationActual = fromIntegral actual
        , violationMax = fromIntegral maxF
        , violationLocation = mkSourceLocation "<constraint-check>" 0 0
        }
    PowerCheck name actual maxPower ->
      FrequencyViolation
        { violationComponent = name
        , violationPort = T.pack "power"
        , violationActual = fromIntegral actual
        , violationMax = fromIntegral maxPower
        , violationLocation = mkSourceLocation "<power-check>" 0 0
        }
    _ ->
      FrequencyViolation
        { violationComponent = checkName check
        , violationPort = T.pack "unknown"
        , violationActual = 0
        , violationMax = 0
        , violationLocation = mkSourceLocation "<custom-check>" 0 0
        }

-- | Create constraint checks from a ComponentSpec
-- Contract: vhdl-analyzer-adc-006 Section: Integration
fromComponentSpec :: ComponentSpec -> [ConstraintCheck]
fromComponentSpec spec =
  let portChecks = map portToCheck (compSpecPorts spec)
  in portChecks
  where
    portToCheck :: PortConstraint -> ConstraintCheck
    portToCheck portConstraint =
      case portConstraintMaxFreq portConstraint of
        Just maxFreq ->
          FrequencyCheck
            { checkName = compSpecName spec
            , checkActual = 0  -- Must be filled at runtime
            , checkMin = Nothing
            , checkMax = Just (round maxFreq)
            }
        Nothing ->
          CustomCheck
            { checkName = compSpecName spec
            , checkDescription = "No frequency constraint"
            }
