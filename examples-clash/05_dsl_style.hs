{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Example 5: DSL-Style VHDL in Clash
-- This demonstrates a domain-specific language (DSL) approach to make
-- Clash code read almost exactly like VHDL structural descriptions.
--
-- The goal is to write code that hardware engineers familiar with VHDL
-- can immediately understand and work with.

module Example05_DSLStyle where

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck
import Data.Proxy (Proxy(..))

-- ============================================================================
-- DSL OPERATORS (Make it read like VHDL)
-- ============================================================================

-- Port map operator: signal <=: port
-- Simulates VHDL: clk_out => high_clk
infix 5 <=>
(<=>) :: a -> a -> a
port <=> signal = signal  -- In real impl, would build port map

-- Generic map operator: generic :=> value
-- Simulates VHDL: CLK_IN_FREQ => 50.0
infix 6 :=>
data GenericAssignment = String :=> Double
  deriving (Show)

-- ============================================================================
-- VHDL-LIKE ENTITY DECLARATION
-- ============================================================================

-- Instead of Haskell data types, use builder pattern:

data Entity freq = Entity
  { entityName :: String
  , ports :: [Port]
  , signals :: [Signal]
  , components :: [Component]
  }

data Port = Port
  { portName :: String
  , portMode :: PortMode
  , portType :: String
  }

data PortMode = In | Out | InOut
  deriving (Show)

data Signal = Signal
  { signalName :: String
  , signalType :: String
  }
  deriving (Show)

data Component = Component
  { componentType :: String
  , instanceName :: String
  , genericMap :: [GenericAssignment]
  , portMap :: [(String, String)]
  }
  deriving (Show)

-- ============================================================================
-- EXAMPLE 1: PLL VIOLATION (DSL Style)
-- ============================================================================

-- VHDL:
--   architecture structural of Video_Processor is
--     signal high_clk : std_logic;
--   begin
--     pll_inst : PLL_1
--       generic map (CLK_IN_FREQ => 50.0, CLK_OUT_FREQ => 200.0)
--       port map (clk_in => pixel_clk, clk_out => high_clk);
--   end architecture;

-- Clash DSL:
video_processor_dsl :: Entity 50
video_processor_dsl = Entity
  { entityName = "Video_Processor"
  , ports =
      [ Port "pixel_clk" In "std_logic"
      , Port "video_out" Out "std_logic_vector(23 downto 0)"
      ]
  , signals =
      [ Signal "high_clk" "std_logic"
      ]
  , components =
      [ Component
          { componentType = "PLL_1"
          , instanceName = "pll_inst"
          , genericMap =
              [ "CLK_IN_FREQ" :=> 50.0
              , "CLK_OUT_FREQ" :=> 200.0
              , "MULTIPLY_BY" :=> 4.0
              ]
          , portMap =
              [ ("clk_in", "pixel_clk")
              , ("clk_out", "high_clk")
              ]
          }
      , Component
          { componentType = "YPbPr_Encoder_A"
          , instanceName = "encoder_inst"
          , genericMap =
              [ "BIT_DEPTH" :=> 8.0
              ]
          , portMap =
              [ ("pixel_clk", "high_clk")
              , ("video_out", "video_out")
              ]
          }
      ]
  }

-- ============================================================================
-- BUILDER PATTERN (Fluent API)
-- ============================================================================

-- Create a fluent API that reads like VHDL:

class EntityBuilder a where
  entity :: String -> a
  port :: String -> PortMode -> String -> a
  signal :: String -> String -> a
  component :: String -> String -> a
  generic :: String -> Double -> a
  mapPort :: String -> String -> a

-- Example usage:
-- architecture_structural =
--   entity "Video_Processor"
--   `port` "pixel_clk" In "std_logic"
--   `signal` "high_clk" "std_logic"
--   `component` "PLL_1" "pll_inst"
--   `generic` "CLK_IN_FREQ" 50.0
--   `mapPort` "clk_in" "pixel_clk"

-- ============================================================================
-- QUASI-QUOTER APPROACH (Most VHDL-like)
-- ============================================================================

-- With QuasiQuotes, we could write actual VHDL syntax:
--
-- [vhdl|
--   architecture structural of Video_Processor is
--     signal high_clk : std_logic;
--   begin
--     pll_inst : PLL_1
--       generic map (CLK_IN_FREQ => 50.0)
--       port map (clk_in => pixel_clk, clk_out => high_clk);
--   end architecture;
-- |]
--
-- This would be parsed at compile time and generate Clash code!

-- ============================================================================
-- PROCESS-STYLE SYNTAX (Using do-notation)
-- ============================================================================

-- VHDL processes can be simulated with do-notation:

-- VHDL:
--   process(clk, rst)
--   begin
--     if rising_edge(clk) then
--       if rst = '1' then
--         counter <= 0;
--       else
--         counter <= counter + 1;
--       end if;
--     end if;
--   end process;

-- Clash equivalent with do-notation:
sequentialProcess :: HWSignal freq () -> IO ()
sequentialProcess clk = do
  -- Wait for rising edge
  let rising_edge = clk  -- Simplified

  -- Check reset
  -- if rst == high then ...

  return ()

-- ============================================================================
-- GENERATE STATEMENT SYNTAX
-- ============================================================================

-- VHDL generate statements for creating multiple instances:

-- VHDL:
--   gen_plls : for i in 0 to 3 generate
--     pll_inst : PLL_1
--       port map (...);
--   end generate;

-- Clash equivalent:
generatePLLs :: [HWSignal 50 ()] -> [HWSignal 200 ()]
generatePLLs inputs =
  [ connectPLL (mkPLL @50 @4 @200 ("PLL_" ++ show i)) input
  | (i, input) <- zip [0..3] inputs
  ]

-- ============================================================================
-- CONFIGURATION-STYLE TYPE-LEVEL PROGRAMMING
-- ============================================================================

-- Use type-level literals to make configuration explicit:

data Config (inFreq :: Nat) (outFreq :: Nat) (maxFreq :: Nat) = Config
  { inputFreq :: Proxy inFreq
  , outputFreq :: Proxy outFreq
  , maximumFreq :: Proxy maxFreq
  }

-- Create configuration
config_pll_violation :: Config 50 200 165
config_pll_violation = Config
  { inputFreq = Proxy @50
  , outputFreq = Proxy @200
  , maximumFreq = Proxy @165
  }

-- ============================================================================
-- COMPLETE EXAMPLE: Side-by-side VHDL and Clash
-- ============================================================================

{-

VHDL:
------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Video_Processor is
  port (
    pixel_clk : in  std_logic;
    video_out : out std_logic_vector(23 downto 0)
  );
end entity;

architecture structural of Video_Processor is
  signal high_clk : std_logic;

  component PLL_1 is
    generic (
      CLK_IN_FREQ  : real;
      CLK_OUT_FREQ : real
    );
    port (
      clk_in  : in  std_logic;
      clk_out : out std_logic
    );
  end component;

begin
  pll_inst : PLL_1
    generic map (
      CLK_IN_FREQ  => 50.0,
      CLK_OUT_FREQ => 200.0
    )
    port map (
      clk_in  => pixel_clk,
      clk_out => high_clk
    );

  encoder_inst : YPbPr_Encoder_A
    generic map (
      BIT_DEPTH => 8
    )
    port map (
      pixel_clk => high_clk,
      video_out => video_out
    );
end architecture;

-}

-- Clash DSL (matches VHDL structure):
------

library_ieee = True  -- Marker for human readers

entity_video_processor = "Video_Processor"

port_pixel_clk = Port "pixel_clk" In "std_logic"
port_video_out = Port "video_out" Out "std_logic_vector(23 downto 0)"

architecture_structural = Entity
  { entityName = "Video_Processor"
  , ports = [port_pixel_clk, port_video_out]
  , signals = [signal_high_clk]
  , components = [component_pll_inst, component_encoder_inst]
  }
  where
    signal_high_clk = Signal "high_clk" "std_logic"

    component_pll_inst = Component
      { componentType = "PLL_1"
      , instanceName = "pll_inst"
      , genericMap =
          [ "CLK_IN_FREQ" :=> 50.0
          , "CLK_OUT_FREQ" :=> 200.0
          ]
      , portMap =
          [ ("clk_in", "pixel_clk")
          , ("clk_out", "high_clk")
          ]
      }

    component_encoder_inst = Component
      { componentType = "YPbPr_Encoder_A"
      , instanceName = "encoder_inst"
      , genericMap =
          [ "BIT_DEPTH" :=> 8.0
          ]
      , portMap =
          [ ("pixel_clk", "high_clk")
          , ("video_out", "video_out")
          ]
      }

-- ============================================================================
-- ACTUAL EXECUTION (Converting DSL to Type-Checked Clash)
-- ============================================================================

-- The DSL description above is just for readability.
-- We need to execute it with type-level checks:

executeDesign :: HWSignal 50 () -> Either ConstraintViolation (HWSignal 200 ())
executeDesign pixel_clk_input = do
  -- Execute PLL component
  let high_clk = connectPLL pll pixel_clk_input
      where
        pll = mkPLL @50 @4 @200 "pll_inst"

  -- Execute encoder component
  encoder_output <- connectEncoder encoder high_clk
    where
      encoder = mkEncoder @165 "encoder_inst" 8

  return encoder_output

-- ============================================================================
-- DEMONSTRATION
-- ============================================================================

demoVHDLStyle :: String
demoVHDLStyle = unlines
  [ "DSL-Style VHDL in Clash"
  , "======================="
  , ""
  , "Entity: " ++ entityName architecture_structural
  , "Ports: " ++ show (length (ports architecture_structural))
  , "Signals: " ++ show (length (signals architecture_structural))
  , "Components: " ++ show (length (components architecture_structural))
  , ""
  , "Component Instances:"
  ] ++ concatMap showComponent (components architecture_structural)
  where
    showComponent Component{..} = unlines
      [ "  - " ++ instanceName ++ " : " ++ componentType
      , "    Generic Map: " ++ show genericMap
      , "    Port Map: " ++ show portMap
      ]

-- Usage:
-- putStrLn demoVHDLStyle
