{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example 1: PLL Frequency Violation (Clash)
-- Corresponds to: examples-vhdl/01_pll_frequency_violation.vhd
--
-- Design: pixel_clk (50 MHz) → PLL (×4.16) → 208 MHz → Encoder (max 165 MHz)
-- Expected: Type error when uncommenting violationDesign
--
-- This demonstrates compile-time detection of the frequency violation
-- that the VHDL analyzer should detect at runtime.

module Example01_PLLViolation where

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck
import VHDL.Clash.Domains

-- | Define the clock domains
type PixelClockFreq = 50   -- 50 MHz input
type PLLMultiplier = 4     -- PLL multiplies by ~4 (we'll use 4 for simplicity, real is 4.16)
type HighClockFreq = 200   -- 50 * 4 = 200 MHz (208 in real VHDL)
type EncoderMaxFreq = 165  -- Encoder maximum frequency

-- | Create the pixel clock domain
pixelDomain :: ClockDomain PixelClockFreq
pixelDomain = mkClockDomain @PixelClockFreq "pixel_clk"

-- | Create a 50 MHz pixel clock signal
pixelClock :: HWSignal PixelClockFreq ()
pixelClock = mkHWSignal "pixel_clk" pixelDomain

-- | PLL component that multiplies frequency by 4
pll :: PLL PixelClockFreq PLLMultiplier
pll = mkPLL @PixelClockFreq @PLLMultiplier @HighClockFreq "PLL_1"

-- | High-speed clock after PLL
highClock :: HWSignal HighClockFreq ()
highClock = connectPLL pll pixelClock

-- | YPbPr Encoder with 165 MHz maximum
encoder :: Encoder EncoderMaxFreq
encoder = mkEncoder @EncoderMaxFreq "YPbPr_Encoder_A" 8

-- | VALID DESIGN (type checks because 200 > 165 but we catch at runtime)
-- This demonstrates the limitation: type checker can't always catch this
-- because CheckMaxFreq is checked at connection time
--
-- To trigger the type error, we need to use connectEncoder directly:
--
-- violationDesign :: Either ConstraintViolation (HWSignal HighClockFreq ())
-- violationDesign = connectEncoder encoder highClock
--
-- Expected error: Couldn't match type 'False' with 'True'
--   arising from: CheckMaxFreq 200 165

-- | Let's make it fail explicitly with a bad frequency
-- Removed type signature to avoid ConstraintViolation import
badDesign =
  let domain208 = mkClockDomain @208 "high_clk"
      signal208 = mkHWSignal @208 "high_clk_out" domain208
      encoder165 = mkEncoder @165 "YPbPr_Encoder" 8
  in connectEncoder encoder165 signal208
  -- ^ This WILL fail to compile: CheckMaxFreq 208 165 = False

-- Uncomment to see the type error:
{-
testViolation :: Either ConstraintViolation (HWSignal 208 ())
testViolation = connectEncoder (mkEncoder @165 "Enc" 8)
                                (mkHWSignal "sig" (mkClockDomain @208 "clk") :: HWSignal 208 ())
-- ERROR: Couldn't match type 'False' with 'True'
-}
