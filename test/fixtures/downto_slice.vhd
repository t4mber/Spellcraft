library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity downto_test is
  port (
    clk : in std_logic;
    cutoff : in std_logic_vector(15 downto 0);
    result : out unsigned(7 downto 0)
  );
end downto_test;

architecture rtl of downto_test is
  constant C_FRAC_BITS : integer := 8;
  signal s_cutoff : unsigned(7 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      -- Test: indexed with downto range
      s_cutoff <= unsigned(std_logic_vector(cutoff(C_FRAC_BITS - 1 downto 0)));
    end if;
  end process;

  result <= s_cutoff;
end architecture;
