-- Minimal test: signal with hex literal initializer
-- ADC-IMPLEMENTS: spellcraft-adc-008

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity hex_test is
  port (
    clk : in std_logic;
    data_out : out unsigned(15 downto 0)
  );
end hex_test;

architecture rtl of hex_test is
  -- Signal with hex literal initializer
  signal lfsr_state : unsigned(15 downto 0) := x"BEEF";

begin
  data_out <= lfsr_state;
end architecture rtl;
