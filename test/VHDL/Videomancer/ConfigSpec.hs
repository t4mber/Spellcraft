{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: <videomancer-config-01>
-- Test suite for Videomancer configuration loading and validation
-- Contract: spellcraft-adc-010

module VHDL.Videomancer.ConfigSpec (spec) where

import Test.Hspec
import VHDL.Videomancer.Config
import Data.Either (isLeft, isRight)

spec :: Spec
spec = do
  describe "Videomancer Config Loading" $ do
    it "loads Lumarian configuration successfully" $ do
      result <- loadProgramConfig "test/fixtures/videomancer/test-lumarian.json"
      result `shouldSatisfy` isRight
      case result of
        Right config -> do
          progName config `shouldBe` "Lumarian"
          length (progParameters config) `shouldBe` 12
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ show err

    it "loads Mirrorbound configuration successfully" $ do
      result <- loadProgramConfig "test/fixtures/videomancer/test-mirrorbound.json"
      result `shouldSatisfy` isRight
      case result of
        Right config -> do
          progName config `shouldBe` "Mirrorbound"
          length (progParameters config) `shouldBe` 12
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ show err

  describe "Parameter Validation" $ do
    describe "RANGE parameters" $ do
      it "accepts valid RANGE parameter with min/max" $ do
        let param = ParameterConfig
              { paramRef = "P1"
              , paramName = "Contrast"
              , paramType = RangeParameter
              , paramFloatPrecision = Just 1
              , paramMin = Just 0
              , paramMax = Just 200
              , paramSuffix = Just "%"
              , paramLabels = Nothing
              }
        validateParameter param `shouldBe` Right ()

      it "rejects RANGE parameter without min" $ do
        let param = ParameterConfig
              { paramRef = "P1"
              , paramName = "Test"
              , paramType = RangeParameter
              , paramFloatPrecision = Just 1
              , paramMin = Nothing
              , paramMax = Just 100
              , paramSuffix = Just "%"
              , paramLabels = Nothing
              }
        validateParameter param `shouldSatisfy` isLeft

      it "rejects RANGE parameter with min >= max" $ do
        let param = ParameterConfig
              { paramRef = "P1"
              , paramName = "Test"
              , paramType = RangeParameter
              , paramFloatPrecision = Just 1
              , paramMin = Just 100
              , paramMax = Just 50
              , paramSuffix = Just "%"
              , paramLabels = Nothing
              }
        validateParameter param `shouldSatisfy` isLeft

    describe "BOOLEAN parameters" $ do
      it "accepts valid BOOLEAN parameter with 2 labels" $ do
        let param = ParameterConfig
              { paramRef = "P7"
              , paramName = "Luma Invert"
              , paramType = BooleanParameter
              , paramFloatPrecision = Nothing
              , paramMin = Nothing
              , paramMax = Nothing
              , paramSuffix = Nothing
              , paramLabels = Just ["On", "Off"]
              }
        validateParameter param `shouldBe` Right ()

      it "rejects BOOLEAN parameter with 1 label" $ do
        let param = ParameterConfig
              { paramRef = "P7"
              , paramName = "Test"
              , paramType = BooleanParameter
              , paramFloatPrecision = Nothing
              , paramMin = Nothing
              , paramMax = Nothing
              , paramSuffix = Nothing
              , paramLabels = Just ["On"]
              }
        validateParameter param `shouldSatisfy` isLeft

      it "rejects BOOLEAN parameter with no labels" $ do
        let param = ParameterConfig
              { paramRef = "P7"
              , paramName = "Test"
              , paramType = BooleanParameter
              , paramFloatPrecision = Nothing
              , paramMin = Nothing
              , paramMax = Nothing
              , paramSuffix = Nothing
              , paramLabels = Nothing
              }
        validateParameter param `shouldSatisfy` isLeft
