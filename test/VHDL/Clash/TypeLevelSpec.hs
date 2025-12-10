{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Evaluation Test Suite for ADC-006 Clash Integration
-- Tests type-level frequency checking and constraint validation
module VHDL.Clash.TypeLevelSpec (spec) where

import Test.Hspec
import Data.Proxy (Proxy(..))

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck
import VHDL.Clash.Domains
import VHDL.Clash.Constraints

spec :: Spec
spec = do
  describe "ADC-006 Type-Level Frequency Checking" $ do

    describe "Scenario 1: Valid PLL Connection (50 MHz * 4 = 200 MHz)" $ do
      it "should successfully connect PLL with correct frequencies" $ do
        let inputDomain = mkClockDomain @50 "pixel_clk"
            inputSignal = mkHWSignal @50 "input" inputDomain
            pll = mkPLL @50 @4 @200 "PLL_1"
            outputSignal = connectPLL pll inputSignal

        domainFreqMHz (hwSignalDomain outputSignal) `shouldBe` 200
        hwSignalName outputSignal `shouldBe` "input_pll_out"

    describe "Scenario 2: Encoder Frequency Constraint (100 MHz <= 150 MHz)" $ do
      it "should accept signal within encoder maximum frequency" $ do
        let domain = mkClockDomain @100 "test_clk"
            signal = mkHWSignal @100 "test_signal" domain
            encoder = mkEncoder @150 "TestEncoder" 8
            result = connectEncoder encoder signal

        result `shouldSatisfy` isRight

      it "should validate frequency against maximum" $ do
        let result = validateFrequency (Proxy @100) (Proxy @150)
        result `shouldBe` FrequencyOK

    describe "Scenario 3: Clock Domain Crossing (25 MHz -> 100 MHz)" $ do
      it "should detect rational frequency relationship" $ do
        let srcDomain = mkClockDomain @25 "slow"
            dstDomain = mkClockDomain @100 "fast"
            relation = relateDomains srcDomain dstDomain

        relation `shouldBe` Rational

      it "should validate FIFO crossing for rational domains" $ do
        let srcDomain = mkClockDomain @25 "slow"
            dstDomain = mkClockDomain @100 "fast"
            crossing = createCrossing "slow_to_fast" srcDomain dstDomain (FIFOCrossing 16)
            result = validateCrossing crossing

        result `shouldSatisfy` isRight

    describe "Scenario 4: Synchronous Domain Detection (100 MHz == 100 MHz)" $ do
      it "should detect synchronous domains with same frequency" $ do
        let domain1 = mkClockDomain @100 "clk_a"
            domain2 = mkClockDomain @100 "clk_b"
            relation = relateDomains domain1 domain2

        relation `shouldBe` Synchronous

      it "should allow direct crossing for synchronous domains" $ do
        let domain1 = mkClockDomain @100 "clk_a"
            domain2 = mkClockDomain @100 "clk_b"
            crossing = createCrossing "direct" domain1 domain2 DirectCrossing
            result = validateCrossing crossing

        result `shouldSatisfy` isRight

    describe "Scenario 5: Clock Divider (100 MHz / 4 = 25 MHz)" $ do
      it "should correctly divide clock frequency" $ do
        let inputDomain = mkClockDomain @100 "fast_clk"
            inputSignal = mkHWSignal @100 "fast" inputDomain
            outputSignal = connectClockDivider @100 @4 @25 4 inputSignal

        domainFreqMHz (hwSignalDomain outputSignal) `shouldBe` 25

    describe "Scenario 6: Domain Registry Management" $ do
      it "should register and lookup domains" $ do
        let domain = mkClockDomain @200 "pll_out"
            registry = registerDomain @200 "pll_out" "PLL output domain" domain emptyRegistry
            result = lookupDomain "pll_out" registry

        result `shouldBe` Just ("PLL output domain", 200)

      it "should list all registered domains" $ do
        let registry = emptyRegistry
            registry1 = registerDomain @100 "sys" "System" (mkClockDomain @100 "sys") registry
            registry2 = registerDomain @400 "fast" "Fast" (mkClockDomain @400 "fast") registry1
            domains = listDomains registry2

        length domains `shouldBe` 2

    describe "Scenario 7: Constraint Validation with Multiple Checks" $ do
      it "should validate hardware constraints on a signal" $ do
        let domain = mkClockDomain @100 "test"
            signal = mkHWSignal @100 "test_sig" domain
            checks = [ FrequencyCheck "test1" 100 (Just 50) (Just 150)
                     , PowerCheck "test2" 100 200
                     ]
            results = validateHardwareConstraints signal checks

        length results `shouldBe` 2
        all isSatisfiedResult results `shouldBe` True

      it "should detect constraint violations" $ do
        let domain = mkClockDomain @200 "high_freq"
            signal = mkHWSignal @200 "high_sig" domain
            checks = [ FrequencyCheck "test" 200 Nothing (Just 150) ]
            results = validateHardwareConstraints signal checks

        any isViolatedResult results `shouldBe` True

    describe "Scenario 8: Predefined Standard Domains" $ do
      it "should create system domain at 100 MHz" $ do
        let domain = mkSystemDomain
        domainFreqMHz domain `shouldBe` 100

      it "should create fast domain at 400 MHz" $ do
        let domain = mkFastDomain
        domainFreqMHz domain `shouldBe` 400

      it "should create slow domain at 25 MHz" $ do
        let domain = mkSlowDomain
        domainFreqMHz domain `shouldBe` 25

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isSatisfiedResult :: ConstraintResult -> Bool
isSatisfiedResult (ConstraintSatisfied _) = True
isSatisfiedResult _ = False

isViolatedResult :: ConstraintResult -> Bool
isViolatedResult (ConstraintViolated _ _) = True
isViolatedResult _ = False
