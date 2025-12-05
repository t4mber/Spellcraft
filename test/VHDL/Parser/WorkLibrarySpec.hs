-- ADC-IMPLEMENTS: spellcraft-adc-008
module VHDL.Parser.WorkLibrarySpec (spec) where

import Test.Hspec
import VHDL.Parser
import VHDL.AST
import Data.Either (isRight)

spec :: Spec
spec = do
  describe "Work Library Support" $ do
    it "parses library work declaration" $ do
      result <- parseVHDLFile "test/fixtures/work_library.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designLibraries design) `shouldBe` 1
          libName (head $ designLibraries design) `shouldBe` "work"
        Left _ -> expectationFailure "Expected Right but got Left"

    it "parses use work.all clause" $ do
      result <- parseVHDLFile "test/fixtures/work_library.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designUses design) `shouldBe` 1
          useLibrary (head $ designUses design) `shouldBe` "work"
          usePackage (head $ designUses design) `shouldBe` "all"
        Left _ -> expectationFailure "Expected Right but got Left"

    it "parses complete design with entities" $ do
      result <- parseVHDLFile "test/fixtures/work_library.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designEntities design) `shouldBe` 1
          entityName (head $ designEntities design) `shouldBe` "test_entity"
        Left _ -> expectationFailure "Expected Right but got Left"

  describe "Multi-Signal Declaration Support (ADC-008)" $ do
    it "parses multi-signal declarations" $ do
      result <- parseVHDLFile "test/fixtures/multi_signal_decl.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designArchitectures design) `shouldBe` 1
          -- Should find 7 signals: single_sig, sig_a, sig_b, sig_c, data_r, data_g, data_b
          length (archSignals (head $ designArchitectures design)) `shouldBe` 7
        Left _ -> expectationFailure "Expected Right but got Left"

    it "parses based literals (hex)" $ do
      result <- parseVHDLFile "test/fixtures/test_hex_init.vhd"
      result `shouldSatisfy` isRight

  describe "Codeglow Pattern Parsing (ADC-008)" $ do
    it "parses codeglow-style VHDL with multi-signal, hex init, and functions" $ do
      result <- parseVHDLFile "test/fixtures/codeglow_pattern.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designEntities design) `shouldBe` 1
          entityName (head $ designEntities design) `shouldBe` "codeglow_pattern"
          length (designArchitectures design) `shouldBe` 1
          -- Should find 5 signals: s1_r, s1_g, s1_b, phase_acc, lfsr_state
          length (archSignals (head $ designArchitectures design)) `shouldBe` 5
        Left err -> expectationFailure $ "Parse failed: " ++ show err
