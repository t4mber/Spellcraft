library ieee;
use ieee.std_logic_1164.all;

entity test_attr_expr is
  port (
    clk : in std_logic;
    result : out integer
  );
end test_attr_expr;

architecture rtl of test_attr_expr is
  signal s_vec : std_logic_vector(7 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      result <= s_vec'length;
    end if;
  end process;
end architecture;
