-- ADC-028: Test generate statement parsing
-- Tests: for-generate and if-generate

library ieee;
use ieee.std_logic_1164.all;

entity generate_test is
  generic (
    N : integer := 4;
    USE_FEATURE : boolean := true
  );
  port (
    clk : in std_logic;
    data_in : in std_logic_vector(N-1 downto 0);
    data_out : out std_logic_vector(N-1 downto 0)
  );
end entity generate_test;

architecture rtl of generate_test is
  signal internal : std_logic_vector(N-1 downto 0);
begin
  -- For-generate statement with 'to'
  gen_up: for i in 0 to N-1 generate
    data_out(i) <= data_in(i);
  end generate gen_up;

  -- For-generate statement with 'downto'
  gen_down: for i in N-1 downto 0 generate
    internal(i) <= data_in(i);
  end generate;

  -- If-generate statement
  gen_feature: if USE_FEATURE generate
    data_out <= internal;
  end generate gen_feature;

end architecture rtl;
