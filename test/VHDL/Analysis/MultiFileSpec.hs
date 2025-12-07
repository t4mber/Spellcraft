-- ADC-IMPLEMENTS: spellcraft-adc-029
-- | Tests for multi-file analysis context building and entity resolution
module VHDL.Analysis.MultiFileSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import VHDL.AST
import VHDL.Analysis.MultiFile
import VHDL.Parser (parseVHDLFile)
import VHDL.SourceLocation (mkSourceLocation)

-- | Helper to create a minimal entity for testing
mkTestEntity :: T.Text -> [PortDecl] -> Entity
mkTestEntity name ports = Entity
  { entityName = name
  , entityGenerics = []
  , entityPorts = ports
  , entityLocation = mkSourceLocation "test.vhd" 1 1
  }

-- | Helper to create a port declaration
mkPort :: T.Text -> PortDirection -> PortDecl
mkPort name dir = PortDecl
  { portName = name
  , portDirection = dir
  , portType = "std_logic"
  }

-- | Helper to create a minimal design with an entity
mkTestDesign :: Entity -> VHDLDesign
mkTestDesign ent = VHDLDesign
  { designLibraries = []
  , designUses = []
  , designEntities = [ent]
  , designArchitectures = []
  , designSourceFile = "test.vhd"
  }

-- | Helper to create a component instantiation for testing
mkTestComponentInst :: T.Text -> T.Text -> [(T.Text, T.Text)] -> ComponentInst
mkTestComponentInst instName compName portMappings = ComponentInst
  { compInstName = instName
  , compComponentName = compName
  , compGenericMap = []
  , compPortMap = [(IdentifierExpr formal, IdentifierExpr actual) | (formal, actual) <- portMappings]
  , compLocation = mkSourceLocation "test.vhd" 10 1
  }

spec :: Spec
spec = do
  describe "buildContext" $ do
    it "builds empty context from empty list" $ do
      let ctx = buildContext []
      Map.size (ctxEntities ctx) `shouldBe` 0
      Map.size (ctxArchitectures ctx) `shouldBe` 0
      ctxDesigns ctx `shouldBe` []

    it "indexes entity by name" $ do
      let entity = mkTestEntity "test_entity" [mkPort "clk" Input, mkPort "result" Output]
      let design = mkTestDesign entity
      let ctx = buildContext [design]
      Map.size (ctxEntities ctx) `shouldBe` 1
      Map.member "test_entity" (ctxEntities ctx) `shouldBe` True

    it "indexes multiple entities from multiple designs" $ do
      let ent1 = mkTestEntity "entity_a" [mkPort "a" Input]
      let ent2 = mkTestEntity "entity_b" [mkPort "b" Output]
      let design1 = mkTestDesign ent1
      let design2 = mkTestDesign ent2
      let ctx = buildContext [design1, design2]
      Map.size (ctxEntities ctx) `shouldBe` 2
      Map.member "entity_a" (ctxEntities ctx) `shouldBe` True
      Map.member "entity_b" (ctxEntities ctx) `shouldBe` True

  describe "lookupEntity" $ do
    it "finds entity by bare name" $ do
      let entity = mkTestEntity "alu" [mkPort "result" Output]
      let ctx = buildContext [mkTestDesign entity]
      lookupEntity ctx "alu" `shouldSatisfy` isJust

    it "finds entity with work. prefix stripped" $ do
      let entity = mkTestEntity "contrast_u" [mkPort "result" Output]
      let ctx = buildContext [mkTestDesign entity]
      lookupEntity ctx "work.contrast_u" `shouldSatisfy` isJust

    it "returns Nothing for unknown entity" $ do
      let ctx = buildContext []
      lookupEntity ctx "nonexistent" `shouldSatisfy` isNothing

    it "handles qualified names with multiple dots" $ do
      -- Should strip library prefix: "work.pkg.entity" -> "pkg.entity"
      -- In practice, we strip everything before the first dot
      let entity = mkTestEntity "component" [mkPort "x" Input]
      let ctx = buildContext [mkTestDesign entity]
      -- "lib.component" should resolve to "component"
      lookupEntity ctx "lib.component" `shouldSatisfy` isJust

  describe "getPortDirection" $ do
    it "returns Input for input port" $ do
      let entity = mkTestEntity "test_ent" [mkPort "clk" Input, mkPort "out" Output]
      let ctx = buildContext [mkTestDesign entity]
      getPortDirection ctx "test_ent" "clk" `shouldBe` Just Input

    it "returns Output for output port" $ do
      let entity = mkTestEntity "test_ent" [mkPort "clk" Input, mkPort "result" Output]
      let ctx = buildContext [mkTestDesign entity]
      getPortDirection ctx "test_ent" "result" `shouldBe` Just Output

    it "returns InOut for bidirectional port" $ do
      let entity = mkTestEntity "test_ent" [mkPort "data" InOut]
      let ctx = buildContext [mkTestDesign entity]
      getPortDirection ctx "test_ent" "data" `shouldBe` Just InOut

    it "returns Nothing for unknown port" $ do
      let entity = mkTestEntity "test_ent" [mkPort "clk" Input]
      let ctx = buildContext [mkTestDesign entity]
      getPortDirection ctx "test_ent" "nonexistent" `shouldBe` Nothing

  describe "isOutputPort" $ do
    it "returns True for Output direction" $ do
      let entity = mkTestEntity "alu" [mkPort "result" Output]
      let ctx = buildContext [mkTestDesign entity]
      isOutputPort ctx "alu" "result" `shouldBe` True

    it "returns True for InOut direction" $ do
      let entity = mkTestEntity "mem" [mkPort "data" InOut]
      let ctx = buildContext [mkTestDesign entity]
      isOutputPort ctx "mem" "data" `shouldBe` True

    it "returns False for Input direction" $ do
      let entity = mkTestEntity "alu" [mkPort "clk" Input]
      let ctx = buildContext [mkTestDesign entity]
      isOutputPort ctx "alu" "clk" `shouldBe` False

    it "returns False for unknown entity" $ do
      let ctx = buildContext []
      isOutputPort ctx "unknown" "port" `shouldBe` False

  describe "getComponentOutputPorts" $ do
    it "returns output port mappings for known entity" $ do
      let entity = mkTestEntity "level6_alu"
            [ mkPort "clk" Input
            , mkPort "enable" Input
            , mkPort "result" Output
            , mkPort "valid" Output
            ]
      let ctx = buildContext [mkTestDesign entity]
      let inst = mkTestComponentInst "alu_inst" "level6_alu"
            [ ("clk", "clk")
            , ("enable", "en")
            , ("result", "alu_result")
            , ("valid", "alu_valid")
            ]
      let outputs = getComponentOutputPorts ctx inst
      length outputs `shouldBe` 2
      outputs `shouldContain` [("result", "alu_result")]
      outputs `shouldContain` [("valid", "alu_valid")]

    it "returns empty list for unknown entity" $ do
      let ctx = buildContext []
      let inst = mkTestComponentInst "inst" "unknown_entity" [("a", "sig")]
      getComponentOutputPorts ctx inst `shouldBe` []

    it "handles entity with no output ports" $ do
      let entity = mkTestEntity "input_only" [mkPort "clk" Input, mkPort "data" Input]
      let ctx = buildContext [mkTestDesign entity]
      let inst = mkTestComponentInst "inst" "input_only" [("clk", "c"), ("data", "d")]
      getComponentOutputPorts ctx inst `shouldBe` []

  describe "isKnownComponentOutput" $ do
    it "returns True when signal is driven by component output" $ do
      let entity = mkTestEntity "alu"
            [ mkPort "result" Output
            , mkPort "valid" Output
            ]
      let ctx = buildContext [mkTestDesign entity]
      let inst = mkTestComponentInst "alu_inst" "alu"
            [ ("result", "my_result")
            , ("valid", "my_valid")
            ]
      isKnownComponentOutput ctx inst "my_result" `shouldBe` True
      isKnownComponentOutput ctx inst "my_valid" `shouldBe` True

    it "returns False for input port mapping" $ do
      let entity = mkTestEntity "alu"
            [ mkPort "clk" Input
            , mkPort "result" Output
            ]
      let ctx = buildContext [mkTestDesign entity]
      let inst = mkTestComponentInst "alu_inst" "alu"
            [ ("clk", "sys_clk")
            , ("result", "out_sig")
            ]
      isKnownComponentOutput ctx inst "sys_clk" `shouldBe` False

    it "returns False for unknown component" $ do
      let ctx = buildContext []
      let inst = mkTestComponentInst "inst" "unknown" [("out", "sig")]
      isKnownComponentOutput ctx inst "sig" `shouldBe` False

  describe "mergeContexts" $ do
    it "merges entities from both contexts" $ do
      let ent1 = mkTestEntity "entity_a" []
      let ent2 = mkTestEntity "entity_b" []
      let ctx1 = buildContext [mkTestDesign ent1]
      let ctx2 = buildContext [mkTestDesign ent2]
      let merged = mergeContexts ctx1 ctx2
      Map.size (ctxEntities merged) `shouldBe` 2
      Map.member "entity_a" (ctxEntities merged) `shouldBe` True
      Map.member "entity_b" (ctxEntities merged) `shouldBe` True

    it "second context wins on conflict" $ do
      let ent1 = mkTestEntity "shared" [mkPort "a" Input]
      let ent2 = mkTestEntity "shared" [mkPort "b" Output]
      let ctx1 = buildContext [mkTestDesign ent1]
      let ctx2 = buildContext [mkTestDesign ent2]
      let merged = mergeContexts ctx1 ctx2
      -- Second context wins, so ports should be from ent2
      case lookupEntity merged "shared" of
        Just ent -> length (entityPorts ent) `shouldBe` 1
        Nothing -> expectationFailure "Entity should exist"

  describe "Integration with real VHDL files" $ do
    it "builds context from KAOS Level 6 component files" $ do
      resultA <- parseVHDLFile "contrib/lzx-kaos-levels/multi-file/level6_component_a.vhd"
      resultB <- parseVHDLFile "contrib/lzx-kaos-levels/multi-file/level6_component_b.vhd"
      case (resultA, resultB) of
        (Right designA, Right designB) -> do
          let ctx = buildContext [designA, designB]
          -- Should have both entities
          Map.member "level6_alu" (ctxEntities ctx) `shouldBe` True
          Map.member "level6_register" (ctxEntities ctx) `shouldBe` True
          -- ALU should have 'result' and 'valid' as outputs
          isOutputPort ctx "level6_alu" "result" `shouldBe` True
          isOutputPort ctx "level6_alu" "valid" `shouldBe` True
          isOutputPort ctx "level6_alu" "clk" `shouldBe` False
          -- Register should have 'q' as output
          isOutputPort ctx "level6_register" "q" `shouldBe` True
          isOutputPort ctx "level6_register" "d" `shouldBe` False
        _ -> expectationFailure "Failed to parse KAOS Level 6 component files"
