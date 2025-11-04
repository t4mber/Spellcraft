{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Validation tests for Clash examples
-- Tests that Clash examples demonstrate proper type-level checking
-- Contrasts with VHDL examples that the runtime analyzer should catch

module VHDL.Clash.ExampleValidationSpec (spec) where

import Test.Hspec
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat)

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck
import VHDL.Clash.Domains
import VHDL.Clash.Constraints

spec :: Spec
spec = do
  describe "Clash Example Validation" $ do

    describe "Example 01: PLL Violation (examples-clash/01_pll_violation.hs)" $ do
      it "should create 50 MHz pixel clock" $ do
        let domain = mkClockDomain @50 "pixel_clk"
        domainFreqMHz domain `shouldBe` 50

      it "should multiply 50 MHz by 4 to get 200 MHz" $ do
        let domain50 = mkClockDomain @50 "pixel"
            signal50 = mkHWSignal @50 "pixel_clk" domain50
            pll = mkPLL @50 @4 @200 "PLL_1"
            signal200 = connectPLL pll signal50
        domainFreqMHz (hwSignalDomain signal200) `shouldBe` 200

      it "should detect that 208 MHz > 165 MHz (at runtime)" $ do
        let result = validateFrequency (Proxy @208) (Proxy @165)
        case result of
          FrequencyTooHigh actual maxF -> do
            actual `shouldBe` 208
            maxF `shouldBe` 165
          _ -> expectationFailure "Expected FrequencyTooHigh"

    describe "Example 02: Multiple PLL Cascading (examples-clash/02_multiple_pll_cascading.hs)" $ do
      it "should cascade PLLs correctly: 25 → 100 → 300 MHz" $ do
        -- 25 MHz system clock
        let domain25 = mkClockDomain @25 "sys"
            signal25 = mkHWSignal @25 "sys_clk" domain25

        -- First PLL: 25 * 4 = 100
        let pll1 = mkPLL @25 @4 @100 "PLL_1"
            signal100 = connectPLL pll1 signal25
        domainFreqMHz (hwSignalDomain signal100) `shouldBe` 100

        -- Second PLL: 100 * 3 = 300
        let pll2 = mkPLL @100 @3 @300 "PLL_2"
            signal300 = connectPLL pll2 signal100
        domainFreqMHz (hwSignalDomain signal300) `shouldBe` 300

      it "should detect that 300 MHz > 165 MHz" $ do
        let result = validateFrequency (Proxy @300) (Proxy @165)
        case result of
          FrequencyTooHigh actual maxF -> do
            actual `shouldBe` 300
            maxF `shouldBe` 165
          _ -> expectationFailure "Expected FrequencyTooHigh"

    describe "Example 03: Valid Design (examples-clash/03_valid_design.hs)" $ do
      it "should multiply 50 MHz by 2 to get 100 MHz" $ do
        let domain50 = mkClockDomain @50 "pixel"
            signal50 = mkHWSignal @50 "pixel_clk" domain50
            pll = mkPLL @50 @2 @100 "PLL_1"
            signal100 = connectPLL pll signal50
        domainFreqMHz (hwSignalDomain signal100) `shouldBe` 100

      it "should validate that 100 MHz < 165 MHz" $ do
        let result = validateFrequency (Proxy @100) (Proxy @165)
        result `shouldBe` FrequencyOK

      it "should successfully connect to encoder" $ do
        let domain = mkClockDomain @100 "output"
            signal = mkHWSignal @100 "output_clk" domain
            encoder = mkEncoder @165 "YPbPr_Encoder_A" 8
            result = connectEncoder encoder signal
        result `shouldSatisfy` isRight

    describe "Type-Level Arithmetic Verification" $ do
      it "should verify PLL multiplication at type level" $ do
        -- Type-level arithmetic: 50 * 4 should equal 200
        let pll = mkPLL @50 @4 @200 "TestPLL"
        pllOutputFreq pll `shouldBe` 200

      it "should verify cascaded multiplication: 25 * 4 * 3 = 300" $ do
        -- First multiplication
        let pll1 = mkPLL @25 @4 @100 "PLL1"
        pllOutputFreq pll1 `shouldBe` 100

        -- Second multiplication
        let pll2 = mkPLL @100 @3 @300 "PLL2"
        pllOutputFreq pll2 `shouldBe` 300

-- Helper
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
