-- Test fixture for multi-signal declarations
-- ADC-IMPLEMENTS: spellcraft-adc-008

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multi_signal_test is
  port (
    clk : in std_logic;
    data_out : out std_logic_vector(7 downto 0)
  );
end entity multi_signal_test;

architecture rtl of multi_signal_test is
  -- Single signal declaration (existing support)
  signal single_sig : std_logic;

  -- Multi-signal declaration (new support needed)
  signal sig_a, sig_b, sig_c : std_logic;
  signal data_r, data_g, data_b : unsigned(7 downto 0);

begin
  data_out <= (others => '0');
end architecture rtl;
