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
          case designLibraries design of
            (lib:_) -> libName lib `shouldBe` "work"
            [] -> expectationFailure "Expected at least one library"
        Left _ -> expectationFailure "Expected Right but got Left"

    it "parses use work.all clause" $ do
      result <- parseVHDLFile "test/fixtures/work_library.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designUses design) `shouldBe` 1
          case designUses design of
            (use:_) -> do
              useLibrary use `shouldBe` "work"
              usePackage use `shouldBe` "all"
            [] -> expectationFailure "Expected at least one use clause"
        Left _ -> expectationFailure "Expected Right but got Left"

    it "parses complete design with entities" $ do
      result <- parseVHDLFile "test/fixtures/work_library.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designEntities design) `shouldBe` 1
          case designEntities design of
            (entity:_) -> entityName entity `shouldBe` "test_entity"
            [] -> expectationFailure "Expected at least one entity"
        Left _ -> expectationFailure "Expected Right but got Left"

  describe "Multi-Signal Declaration Support (ADC-008)" $ do
    it "parses multi-signal declarations" $ do
      result <- parseVHDLFile "test/fixtures/multi_signal_decl.vhd"
      result `shouldSatisfy` isRight
      case result of
        Right design -> do
          length (designArchitectures design) `shouldBe` 1
          -- Should find 7 signals: single_sig, sig_a, sig_b, sig_c, data_r, data_g, data_b
          case designArchitectures design of
            (arch:_) -> length (archSignals arch) `shouldBe` 7
            [] -> expectationFailure "Expected at least one architecture"
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
          case designEntities design of
            (entity:_) -> entityName entity `shouldBe` "codeglow_pattern"
            [] -> expectationFailure "Expected at least one entity"
          length (designArchitectures design) `shouldBe` 1
          -- Should find 5 signals: s1_r, s1_g, s1_b, phase_acc, lfsr_state
          case designArchitectures design of
            (arch:_) -> length (archSignals arch) `shouldBe` 5
            [] -> expectationFailure "Expected at least one architecture"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
