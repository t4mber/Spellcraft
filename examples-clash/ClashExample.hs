{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- ADC-IMPLEMENTS: vdhl-analyzer-adc-006
-- Example demonstrating Clash type-level hardware constraint modeling

module ClashExample where

import VDHL.Clash.Types
  ( FreqMHz
  , ClockDomain
  , HWSignal
  , PLL
  , Encoder
  , mkClockDomain
  , mkHWSignal
  , mkPLL
  , mkEncoder
  )
import VDHL.Clash.FrequencyCheck
  ( connectPLL
  , connectEncoder
  , validateFrequency
  , FrequencyCheckResult(..)
  )
import VDHL.Clash.Domains
  ( System
  , Fast
  , Slow
  , mkSystemDomain
  , mkFastDomain
  , mkSlowDomain
  , DomainCrossing
  , CrossingStrategy(..)
  , createCrossing
  , validateCrossing
  , relateDomains
  )
import VDHL.Clash.Constraints
  ( ConstraintCheck(..)
  , validateHardwareConstraints
  , checkFrequencyConstraint
  , checkPowerConstraint
  , (&&&)
  , satisfiesAll
  )
import Data.Proxy (Proxy(..))

-- | Example 1: Type-safe PLL connection
-- A 50 MHz input clock is multiplied by 4 to produce 200 MHz
example1_pllConnection :: IO ()
example1_pllConnection = do
  putStrLn "Example 1: Type-safe PLL connection"
  putStrLn "===================================="

  -- Create a 50 MHz clock domain
  let inputDomain = mkClockDomain @50 "input_clk"

  -- Create an input signal
  let inputSignal = mkHWSignal @50 "clk_in" inputDomain

  -- Create a PLL that multiplies by 4 (50 * 4 = 200)
  let pll = mkPLL @50 @4 @200 "main_pll"

  -- Connect the PLL - this is type-safe!
  -- The output frequency is enforced to be 200 MHz at compile time
  let outputSignal = connectPLL pll inputSignal

  putStrLn $ "Input frequency: " ++ show (domainFreqMHz $ hwSignalDomain inputSignal) ++ " MHz"
  putStrLn $ "Output frequency: " ++ show (domainFreqMHz $ hwSignalDomain outputSignal) ++ " MHz"
  putStrLn ""

-- | Example 2: Encoder with frequency constraint checking
-- Demonstrates both compile-time and runtime checking
example2_encoderConstraint :: IO ()
example2_encoderConstraint = do
  putStrLn "Example 2: Encoder with frequency constraint"
  putStrLn "============================================="

  -- Create a 100 MHz signal (system clock)
  let systemClk = mkSystemDomain
  let signal = mkHWSignal @100 "data_signal" systemClk

  -- Create an encoder with max frequency of 150 MHz
  let encoder = mkEncoder @150 "video_encoder" 8

  -- Try to connect - this will succeed because 100 <= 150
  case connectEncoder encoder signal of
    Right result -> putStrLn "Connection successful: frequency constraint satisfied"
    Left violation -> putStrLn $ "Connection failed: " ++ show violation

  putStrLn ""

-- | Example 3: Clock domain crossing
-- Demonstrates domain relationship checking
example3_domainCrossing :: IO ()
example3_domainCrossing = do
  putStrLn "Example 3: Clock domain crossing"
  putStrLn "================================="

  -- Create different clock domains
  let slowDomain = mkSlowDomain    -- 25 MHz
  let systemDomain = mkSystemDomain -- 100 MHz
  let fastDomain = mkFastDomain    -- 400 MHz

  -- Check relationship between slow and system domains
  let relation1 = relateDomains slowDomain systemDomain
  putStrLn $ "Relation (25 MHz -> 100 MHz): " ++ show relation1

  -- Create a crossing from slow to system
  let crossing = createCrossing "slow_to_system" slowDomain systemDomain (FIFOCrossing 16)

  -- Validate the crossing
  case validateCrossing crossing of
    Right validCrossing -> putStrLn "Domain crossing validated successfully"
    Left err -> putStrLn $ "Domain crossing validation failed: " ++ show err

  putStrLn ""

-- | Example 4: Constraint validation
-- Shows how to validate multiple constraints
example4_constraintValidation :: IO ()
example4_constraintValidation = do
  putStrLn "Example 4: Constraint validation"
  putStrLn "================================="

  -- Create a 200 MHz signal
  let fastDomain = mkClockDomain @200 "fast_clk"
  let signal = mkHWSignal @200 "high_speed_signal" fastDomain

  -- Define constraint checks
  let checks =
        [ FrequencyCheck "freq_check" 200 (Just 100) (Just 300)
        , PowerCheck "power_check" 200 250
        ]

  -- Validate all constraints
  let results = validateHardwareConstraints signal checks

  putStrLn "Constraint validation results:"
  mapM_ (\result -> putStrLn $ "  " ++ show result) results

  if satisfiesAll results
    then putStrLn "All constraints satisfied!"
    else putStrLn "Some constraints violated!"

  putStrLn ""

-- | Example 5: Type-level frequency arithmetic
-- Demonstrates frequency multiplication and division at the type level
example5_frequencyArithmetic :: IO ()
example5_frequencyArithmetic = do
  putStrLn "Example 5: Type-level frequency arithmetic"
  putStrLn "==========================================="

  -- 50 MHz base clock
  let baseDomain = mkClockDomain @50 "base"
  let baseSignal = mkHWSignal @50 "base_clk" baseDomain

  -- Create a PLL that multiplies by 8 (50 * 8 = 400)
  let pll1 = mkPLL @50 @8 @400 "pll_x8"
  let signal400 = connectPLL pll1 baseSignal

  putStrLn $ "Base frequency: 50 MHz"
  putStrLn $ "After PLL x8: " ++ show (domainFreqMHz $ hwSignalDomain signal400) ++ " MHz"
  putStrLn ""

-- | Example 6: Integration with existing VDHL constraint system
-- Shows how Clash types integrate with VDHL.Constraint.Types
example6_integration :: IO ()
example6_integration = do
  putStrLn "Example 6: Integration with VDHL constraints"
  putStrLn "============================================="

  -- Create a signal with frequency constraint
  let domain = mkClockDomain @100 "system"
  let signal = mkHWSignal @100 "data_bus" domain

  -- Type-level constraint checking (enforced at compile time)
  -- This would fail to compile if the frequency was wrong:
  -- checkFrequencyConstraint @100 @50 @150 signal

  case checkFrequencyConstraint @100 @50 @150 signal of
    Right () -> putStrLn "Type-level frequency constraint satisfied (50 <= 100 <= 150)"
    Left violation -> putStrLn $ "Violation: " ++ show violation

  putStrLn ""

-- | Main example runner
main :: IO ()
main = do
  putStrLn "\n=========================================="
  putStrLn "VDHL Clash Integration Examples"
  putStrLn "ADC Contract: vdhl-analyzer-adc-006"
  putStrLn "==========================================\n"

  example1_pllConnection
  example2_encoderConstraint
  example3_domainCrossing
  example4_constraintValidation
  example5_frequencyArithmetic
  example6_integration

  putStrLn "=========================================="
  putStrLn "All examples completed!"
  putStrLn "=========================================="

-- | Compile-time examples that demonstrate type safety
-- These would fail to compile if the constraints are violated

-- This compiles: 100 MHz is within bounds [50, 150]
typeCheck1 :: HWSignal 100 Int -> Either a ()
typeCheck1 = checkFrequencyConstraint @100 @50 @150

-- This would NOT compile: 200 MHz exceeds maximum 150
-- Uncomment to see the type error:
-- typeCheck2 :: HWSignal 200 Int -> Either a ()
-- typeCheck2 = checkFrequencyConstraint @200 @50 @150

-- This compiles: PLL output is correctly calculated (50 * 4 = 200)
typeCheck3 :: PLL 50 4
typeCheck3 = mkPLL @50 @4 @200 "pll"

-- This would NOT compile: output frequency mismatch (50 * 4 != 150)
-- Uncomment to see the type error:
-- typeCheck4 :: PLL 50 4
-- typeCheck4 = mkPLL @50 @4 @150 "pll"
