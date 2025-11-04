{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Example 3: Valid Design (Clash)
-- Corresponds to: examples-vhdl/03_valid_design.vhd
--
-- Design: pixel_clk (50 MHz) → PLL (×2) → 100 MHz → Encoder (max 165 MHz)
-- Expected: Compiles successfully - 100 MHz < 165 MHz
--
-- This demonstrates a VALID design that type-checks correctly.

module Example03_ValidDesign where

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck

-- | Clock domain frequencies
type PixelClockFreq = 50    -- 50 MHz input
type PLLMultiplier = 2      -- PLL multiplies by 2
type OutputFreq = 100       -- 50 * 2 = 100 MHz
type EncoderMaxFreq = 165   -- Encoder maximum

-- | Create pixel clock
pixelDomain :: ClockDomain PixelClockFreq
pixelDomain = mkClockDomain @PixelClockFreq "pixel_clk"

pixelClock :: HWSignal PixelClockFreq ()
pixelClock = mkHWSignal "pixel_clk" pixelDomain

-- | PLL: 50 MHz × 2 = 100 MHz
pll :: PLL PixelClockFreq PLLMultiplier
pll = mkPLL @PixelClockFreq @PLLMultiplier @OutputFreq "PLL_1"

outputClock :: HWSignal OutputFreq ()
outputClock = connectPLL pll pixelClock

-- | Encoder with 165 MHz maximum
encoder :: Encoder EncoderMaxFreq
encoder = mkEncoder @EncoderMaxFreq "YPbPr_Encoder_A" 8

-- | VALID DESIGN: This compiles successfully
-- 100 MHz < 165 MHz, so the constraint is satisfied
validDesign :: Either ConstraintViolation (HWSignal OutputFreq ())
validDesign = connectEncoder encoder outputClock
-- ^ Type-checks successfully: CheckMaxFreq 100 165 = True

-- | We can also verify the output is indeed 100 MHz
verifyFrequency :: Double
verifyFrequency = domainFreqMHz (hwSignalDomain outputClock)  -- Returns 100.0

-- | Show that the design parameters are within bounds
designSummary :: String
designSummary = unlines
  [ "Valid Video Processing Design"
  , "=============================="
  , "Input:  " ++ show (domainFreqMHz pixelDomain) ++ " MHz"
  , "PLL:    ×" ++ show (pllMultiplier pll)
  , "Output: " ++ show verifyFrequency ++ " MHz"
  , "Encoder Max: " ++ show (encoderMaxFreqMHz encoder) ++ " MHz"
  , ""
  , "✓ Output frequency (" ++ show verifyFrequency ++ " MHz) is within"
  , "  encoder maximum (" ++ show (encoderMaxFreqMHz encoder) ++ " MHz)"
  ]
