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
