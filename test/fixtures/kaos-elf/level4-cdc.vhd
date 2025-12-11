--------------------------------------------------------------------------------
--  File:        level4-cdc.vhd
--  KAOS ELF:    Level 4 - The Dimensional Rift
--  Description: Clock Domain Crossing violation - signals crossing clock
--               domains without proper synchronization
--------------------------------------------------------------------------------
-- CHAOS-MONKEY: level4-cdc
-- SUBTLETY: 4 (Very Subtle)
-- VIOLATION: Signals crossing between clock domains without synchronizers
--
-- In the realm of digital design, clock domains are like separate dimensions.
-- When a signal passes from one domain to another without proper synchronization,
-- it creates a rift - metastability that corrupts data unpredictably.
--
-- This violation can cause:
--   - Metastability (signal oscillates between 0 and 1)
--   - Data corruption (wrong values captured)
--   - Timing violations that only manifest under specific conditions
--   - Silicon failures that pass simulation but fail on hardware
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity level4_cdc_rift is
  port (
    -- Domain A: Fast clock (200 MHz)
    clk_fast     : in  std_logic;
    rst_fast     : in  std_logic;
    data_in      : in  std_logic_vector(7 downto 0);
    valid_in     : in  std_logic;

    -- Domain B: Slow clock (50 MHz)
    clk_slow     : in  std_logic;
    rst_slow     : in  std_logic;
    data_out     : out std_logic_vector(7 downto 0);
    valid_out    : out std_logic
  );
end level4_cdc_rift;

architecture rtl of level4_cdc_rift is
  -- Signals in fast clock domain
  signal s_data_captured    : std_logic_vector(7 downto 0);
  signal s_valid_captured   : std_logic;

  -- CHAOS-MONKEY: level4-cdc
  -- THE DIMENSIONAL RIFT: These signals are registered in the fast domain
  -- but read directly in the slow domain without synchronization!
  signal s_data_crossing    : std_logic_vector(7 downto 0);
  signal s_valid_crossing   : std_logic;

  -- Signals in slow clock domain (properly registered in this domain)
  signal s_data_slow        : std_logic_vector(7 downto 0);
  signal s_valid_slow       : std_logic;

begin

  ----------------------------------------------------------------------------
  -- Fast Clock Domain (200 MHz)
  -- Captures incoming data and prepares for crossing
  ----------------------------------------------------------------------------
  process(clk_fast, rst_fast)
  begin
    if rst_fast = '1' then
      s_data_captured  <= (others => '0');
      s_valid_captured <= '0';
      s_data_crossing  <= (others => '0');
      s_valid_crossing <= '0';
    elsif rising_edge(clk_fast) then
      -- Capture input data
      if valid_in = '1' then
        s_data_captured <= data_in;
        s_valid_captured <= '1';
      end if;

      -- Transfer to crossing signals (still in fast domain)
      s_data_crossing  <= s_data_captured;
      s_valid_crossing <= s_valid_captured;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- Slow Clock Domain (50 MHz)
  -- THE VIOLATION: Reading signals from fast domain without synchronizers!
  --
  -- A proper CDC would use:
  --   1. A 2-FF synchronizer for single-bit signals (like s_valid_crossing)
  --   2. Gray code encoding or a FIFO for multi-bit signals (like s_data_crossing)
  --   3. Handshaking protocols for reliable data transfer
  --
  -- Instead, we directly sample the fast-domain signals, creating metastability risk!
  ----------------------------------------------------------------------------
  process(clk_slow, rst_slow)
  begin
    if rst_slow = '1' then
      s_data_slow  <= (others => '0');
      s_valid_slow <= '0';
    elsif rising_edge(clk_slow) then
      -- VIOLATION: Direct sampling of signals from different clock domain!
      -- No synchronization = metastability = unreliable operation
      s_data_slow  <= s_data_crossing;   -- CDC violation on 8-bit bus
      s_valid_slow <= s_valid_crossing;  -- CDC violation on control signal
    end if;
  end process;

  -- Output assignment
  data_out  <= s_data_slow;
  valid_out <= s_valid_slow;

end architecture;

--------------------------------------------------------------------------------
-- The Proper Incantation (Synchronizer Pattern)
--
-- For reference, a proper 2-FF synchronizer for single-bit signals:
--
--   signal sync_ff1, sync_ff2 : std_logic;
--
--   process(clk_slow)
--   begin
--     if rising_edge(clk_slow) then
--       sync_ff1 <= async_signal;  -- First FF (may go metastable)
--       sync_ff2 <= sync_ff1;      -- Second FF (stable)
--     end if;
--   end process;
--
-- For multi-bit signals, use:
--   - Gray code counters (for FIFO pointers)
--   - Async FIFO with dual-clock design
--   - Handshaking with proper pulse synchronizers
--
--------------------------------------------------------------------------------
