library ieee;
use ieee.std_logic_1164.all;

entity test_var_assign is
  port (
    clk : in std_logic;
    result : out integer
  );
end test_var_assign;

architecture rtl of test_var_assign is
begin
  process(clk)
  variable v_temp : integer;
  begin
    if rising_edge(clk) then
      v_temp := 42;
      result <= v_temp;
    end if;
  end process;
end architecture;
