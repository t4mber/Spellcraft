library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity power_paren_test is
  port (
    clk : in std_logic;
    result : out signed(11 downto 0)
  );
end power_paren_test;

architecture rtl of power_paren_test is
  constant C_FRAC_BITS : integer := 8;
  constant C_PROC_WIDTH : integer := 12;
  signal s_temp : signed(11 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= to_signed(2 ** (C_FRAC_BITS - 1), C_PROC_WIDTH);
    end if;
  end process;
  result <= s_temp;
end architecture;
