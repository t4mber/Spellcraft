library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity complex_expr_test is
  port (
    clk : in std_logic;
    a : in unsigned(7 downto 0);
    result : out signed(11 downto 0)
  );
end complex_expr_test;

architecture rtl of complex_expr_test is
  signal s_temp : signed(11 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= resize(signed('0' & std_logic_vector(a)), 12);
    end if;
  end process;

  result <= s_temp;
end architecture;
