{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Example 4: VHDL-Style Syntax in Clash
-- This example shows how to write Clash code that looks more like VHDL
-- using Haskell syntax extensions and clever naming conventions.
--
-- Compare this to the VHDL version: examples-vhdl/01_pll_frequency_violation.vhd

module Example04_VHDLStyleSyntax where

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck
import qualified Data.Map as Map

-- ============================================================================
-- ENTITY DECLARATION (VHDL-style using data types)
-- ============================================================================

-- In VHDL:
--   entity Video_Processor is
--     port (
--       pixel_clk : in  std_logic;
--       video_out : out std_logic_vector(23 downto 0)
--     );
--   end entity;

data EntityPorts freq = EntityPorts
  { pixel_clk :: HWSignal freq ()
  , video_out :: HWSignal freq ()
  }

-- ============================================================================
-- ARCHITECTURE DECLARATION (VHDL-style using where clauses)
-- ============================================================================

-- In VHDL:
--   architecture structural of Video_Processor is
--     signal high_clk : std_logic;
--     signal stage1_clk : std_logic;
--   begin
--     ...
--   end architecture;

videoProcessor :: EntityPorts 50 -> EntityPorts 200
videoProcessor ports =
  EntityPorts { pixel_clk = high_clk_signal, video_out = video_output }
  where
    -- Signal declarations (like VHDL signals)
    high_clk_signal = pll_1.clk_out
    video_output = encoder_inst.video_out

    -- Component instantiation: PLL_1
    pll_1 = ComponentPLL
      { component_name = "PLL_1"
      , clk_in = ports.pixel_clk
      , clk_out = connectPLL pll_spec ports.pixel_clk
      }
      where
        pll_spec = mkPLL @50 @4 @200 "PLL_1"

    -- Component instantiation: YPbPr_Encoder
    encoder_inst = ComponentEncoder
      { component_name = "YPbPr_Encoder_A"
      , pixel_clk = high_clk_signal
      , video_out = high_clk_signal  -- Simplified
      }

-- ============================================================================
-- GENERIC MAP / PORT MAP SYNTAX (Using record syntax)
-- ============================================================================

-- In VHDL:
--   pll_inst : PLL_1
--     generic map (
--       CLK_IN_FREQ  => 50.0,
--       CLK_OUT_FREQ => 200.0,
--       MULTIPLY_BY  => 4
--     )
--     port map (
--       clk_in  => pixel_clk,
--       clk_out => high_clk
--     );

data GenericMap = GenericMap
  { clk_in_freq  :: Double
  , clk_out_freq :: Double
  , multiply_by  :: Int
  }

data PortMap freq_in freq_out = PortMap
  { clk_in  :: HWSignal freq_in ()
  , clk_out :: HWSignal freq_out ()
  }

-- Component instance with generic map and port map
pll_instance
  :: GenericMap
  -> PortMap 50 200
  -> PortMap 50 200
pll_instance GenericMap{..} PortMap{..} =
  PortMap
    { clk_in  = clk_in
    , clk_out = connectPLL pll clk_in
    }
  where
    pll = mkPLL @50 @4 @200 "PLL_1"

-- ============================================================================
-- COMPLETE VHDL-STYLE DESIGN
-- ============================================================================

-- VHDL equivalent:
--
-- architecture structural of Video_Processor is
--   signal high_clk : std_logic;
-- begin
--   pll_inst : PLL_1
--     generic map (CLK_IN_FREQ => 50.0, CLK_OUT_FREQ => 200.0, MULTIPLY_BY => 4)
--     port map (clk_in => pixel_clk, clk_out => high_clk);
--
--   encoder_inst : YPbPr_Encoder_A
--     generic map (BIT_DEPTH => 8)
--     port map (pixel_clk => high_clk, video_out => video_out);
-- end architecture;

videoProcessorStructural :: HWSignal 50 () -> HWSignal 200 ()
videoProcessorStructural pixel_clk_input =
  video_out_signal
  where
    -- Signal declarations
    high_clk :: HWSignal 200 ()
    high_clk = pll_inst_output

    video_out_signal :: HWSignal 200 ()
    video_out_signal = encoder_inst_output

    -- Component instantiation: pll_inst
    pll_inst_output = connectPLL pll pixel_clk_input
      where
        pll = mkPLL @50 @4 @200 "PLL_1"

    -- Component instantiation: encoder_inst
    encoder_inst_output = case connectEncoder encoder high_clk of
      Right sig -> sig
      Left err -> high_clk  -- Simplified error handling
      where
        encoder = mkEncoder @165 "YPbPr_Encoder_A" 8

-- ============================================================================
-- BLOCK SYNTAX (Using do-notation for sequential style)
-- ============================================================================

-- VHDL uses begin...end blocks. We can simulate with do-notation:

videoProcessorBlock :: HWSignal 50 () -> Either ConstraintViolation (HWSignal 200 ())
videoProcessorBlock input = do
  -- Signal declarations and assignments
  let pll_component = mkPLL @50 @4 @200 "PLL_1"
  let high_clk = connectPLL pll_component input

  let encoder_component = mkEncoder @165 "YPbPr_Encoder_A" 8
  encoder_output <- connectEncoder encoder_component high_clk

  return encoder_output

-- ============================================================================
-- CONFIGURATION-STYLE SYNTAX (Using type applications)
-- ============================================================================

-- Make it read like VHDL configuration:

type Configuration_PLL_Frequencies =
  '[ '("pixel_clk_freq", 50)
   , '("high_clk_freq", 200)
   , '("encoder_max_freq", 165)
   ]

-- ============================================================================
-- COMPARISON: VHDL vs Clash
-- ============================================================================

{- VHDL VERSION:

architecture structural of Video_Processor is
  signal high_clk : std_logic;
begin
  pll_inst : PLL_1
    generic map (
      CLK_IN_FREQ  => 50.0,
      CLK_OUT_FREQ => 200.0,
      MULTIPLY_BY  => 4,
      DIVIDE_BY    => 1
    )
    port map (
      clk_in  => pixel_clk,
      clk_out => high_clk,
      locked  => open
    );

  encoder_inst : YPbPr_Encoder_A
    generic map (
      BIT_DEPTH => 8
    )
    port map (
      pixel_clk => high_clk,
      video_in  => video_data,
      video_out => video_out
    );
end architecture;

-}

-- CLASH VERSION (VHDL-style):

architecture_structural input_ports = output_ports
  where
    -- Signal declarations
    high_clk = pll_inst.clk_out

    -- Component instantiations
    pll_inst = ComponentPLL
      { clk_in  = input_ports.pixel_clk
      , clk_out = connectPLL pll_component (input_ports.pixel_clk)
      , locked  = ()  -- simplified
      }
      where
        pll_component = mkPLL @50 @4 @200 "PLL_1"

    encoder_inst = ComponentEncoder
      { pixel_clk = high_clk
      , video_out = case connectEncoder enc high_clk of
                      Right sig -> sig
                      Left _ -> high_clk
      }
      where
        enc = mkEncoder @165 "YPbPr_Encoder_A" 8

    output_ports = EntityPorts
      { pixel_clk = high_clk
      , video_out = encoder_inst.video_out
      }

-- ============================================================================
-- HELPER DATA TYPES (To make syntax more VHDL-like)
-- ============================================================================

data ComponentPLL freq_in freq_out = ComponentPLL
  { component_name :: String
  , clk_in  :: HWSignal freq_in ()
  , clk_out :: HWSignal freq_out ()
  , locked  :: ()
  }

data ComponentEncoder freq = ComponentEncoder
  { component_name :: String
  , pixel_clk :: HWSignal freq ()
  , video_out :: HWSignal freq ()
  }

-- ============================================================================
-- DESIGN DEMONSTRATION
-- ============================================================================

-- Create input ports
createInputPorts :: EntityPorts 50
createInputPorts = EntityPorts
  { pixel_clk = mkHWSignal @50 "pixel_clk" (mkClockDomain @50 "pixel_clk")
  , video_out = mkHWSignal @50 "video_in" (mkClockDomain @50 "pixel_clk")
  }

-- Instantiate the design
designInstance :: EntityPorts 200
designInstance = videoProcessor createInputPorts

-- Demonstrate VHDL-style naming
demo_structural :: String
demo_structural = unlines
  [ "-- VHDL-Style Clash Design"
  , "-- ======================="
  , ""
  , "entity: Video_Processor"
  , "architecture: structural"
  , ""
  , "Components instantiated:"
  , "  - pll_inst (PLL_1): 50 MHz → 200 MHz"
  , "  - encoder_inst (YPbPr_Encoder_A): max 165 MHz"
  , ""
  , "Expected: Type error if encoder receives 200 MHz"
  ]
