{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example 2: Multiple PLL Cascading Violation (Clash)
-- Corresponds to: examples-vhdl/02_multiple_pll_cascading.vhd
--
-- Design: sys_clk (25 MHz) → PLL1 (×4) → 100 MHz → PLL2 (×3) → 300 MHz → Encoder (max 165 MHz)
-- Expected: Type error - 300 MHz > 165 MHz
--
-- This SHOULD fail at compile time when we try to connect the 300 MHz signal
-- to the 165 MHz encoder.

module Example02_MultiplePLLCascading where

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck

-- | Clock domain frequencies
type SystemClockFreq = 25   -- 25 MHz system clock
type PLL1Factor = 4          -- First PLL multiplies by 4
type Stage1Freq = 100        -- 25 * 4 = 100 MHz
type PLL2Factor = 3          -- Second PLL multiplies by 3
type Stage2Freq = 300        -- 100 * 3 = 300 MHz
type EncoderMaxFreq = 165    -- Encoder maximum

-- | System clock (25 MHz)
sysClockDomain :: ClockDomain SystemClockFreq
sysClockDomain = mkClockDomain @SystemClockFreq "sys_clk"

sysClock :: HWSignal SystemClockFreq ()
sysClock = mkHWSignal "sys_clk" sysClockDomain

-- | First PLL: 25 MHz × 4 = 100 MHz
pll1 :: PLL SystemClockFreq PLL1Factor
pll1 = mkPLL @SystemClockFreq @PLL1Factor @Stage1Freq "PLL_1"

stage1Clock :: HWSignal Stage1Freq ()
stage1Clock = connectPLL pll1 sysClock

-- | Second PLL: 100 MHz × 3 = 300 MHz
pll2 :: PLL Stage1Freq PLL2Factor
pll2 = mkPLL @Stage1Freq @PLL2Factor @Stage2Freq "PLL_2"

stage2Clock :: HWSignal Stage2Freq ()
stage2Clock = connectPLL pll2 stage1Clock

-- | Encoder with 165 MHz maximum
encoder :: Encoder EncoderMaxFreq
encoder = mkEncoder @EncoderMaxFreq "YPbPr_Encoder_A" 8

-- | THIS SHOULD FAIL: 300 MHz > 165 MHz
-- Uncomment to see the type error:
{-
violationDesign :: Either ConstraintViolation (HWSignal Stage2Freq ())
violationDesign = connectEncoder encoder stage2Clock

-- Expected error:
-- Couldn't match type 'False' with 'True'
--   arising from a use of 'connectEncoder'
-- The type constraint (CheckMaxFreq 300 165) cannot be satisfied
-- because (300 <=? 165) ~ 'False', but we need 'True'
-}

-- | Demonstrate the violation explicitly with concrete types
-- Removed type signature to avoid ConstraintViolation import
explicitViolation = connectEncoder encoder stage2Clock
-- ^ This WILL fail to compile: CheckMaxFreq 300 165 = False
