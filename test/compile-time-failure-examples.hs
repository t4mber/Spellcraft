{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Compile-Time Type Checking Demonstration
-- These examples SHOULD FAIL at compile time when uncommented
-- This demonstrates that the type system catches constraint violations

module CompileTimeFailureExamples where

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck
import VHDL.Clash.Constraints

-- Example 1: PLL with incorrect output frequency
-- This WILL NOT compile because 50 * 4 ≠ 180
{-
badPLL :: PLL 50 4
badPLL = mkPLL @50 @4 @180 "BadPLL"

-- Expected error:
-- Couldn't match type '200' with '180'
-- arising from constraint: 180 ~ FreqMult 50 4
-}

-- Example 2: Encoder connection exceeding maximum frequency
-- This WILL NOT compile because 208 > 165
{-
badEncoderConnection :: Encoder 165 -> HWSignal 208 () -> Either ConstraintViolation (HWSignal 208 ())
badEncoderConnection = connectEncoder

-- Expected error:
-- Couldn't match type ''False' with ''True'
-- arising from constraint: CheckMaxFreq 208 165
-}

-- Example 3: Frequency constraint violation
-- This WILL NOT compile because 200 is not in range [50, 150]
{-
badFrequencyConstraint :: HWSignal 200 () -> Either ConstraintViolation ()
badFrequencyConstraint = checkFrequencyConstraint @200 @50 @150

-- Expected error:
-- Couldn't match type ''False' with ''True'
-- arising from constraint: FrequencyConstraint 200 50 150
-}

-- Example 4: Clock divider with incorrect result
-- This WILL NOT compile because 100 / 4 ≠ 50
{-
badDivider :: HWSignal 100 () -> HWSignal 50 ()
badDivider inputSig = connectClockDivider @100 @4 @50 4 inputSig

-- Expected error:
-- Couldn't match type '25' with '50'
-- arising from constraint: 50 ~ FreqDiv 100 4
-}

-- Example 5: Power constraint violation
-- This WILL NOT compile because 300 > 200
{-
badPowerConstraint :: HWSignal 300 () -> Either ConstraintViolation ()
badPowerConstraint = checkPowerConstraint @300 @200

-- Expected error:
-- Couldn't match type ''False' with ''True'
-- arising from constraint: PowerConstraint 300 200
-}

-- Example 6: Valid connections (these SHOULD compile)
goodPLL :: PLL 50 4
goodPLL = mkPLL @50 @4 @200 "GoodPLL"

goodEncoderConnection :: Encoder 200 -> HWSignal 150 () -> Either ConstraintViolation (HWSignal 150 ())
goodEncoderConnection = connectEncoder

goodFrequencyConstraint :: HWSignal 100 () -> Either ConstraintViolation ()
goodFrequencyConstraint = checkFrequencyConstraint @100 @50 @150

goodDivider :: HWSignal 100 () -> HWSignal 25 ()
goodDivider inputSig = connectClockDivider @100 @4 @25 4 inputSig

-- Demonstration: Uncommenting the bad* functions above will cause
-- compilation to fail with type errors, proving that the type system
-- is enforcing hardware constraints at compile time!
