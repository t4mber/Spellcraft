library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity power_test is
  port (
    clk : in std_logic;
    result : out integer
  );
end power_test;

architecture rtl of power_test is
  signal s_temp : integer;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= 2 ** 4;
    end if;
  end process;
  result <= s_temp;
end architecture;
