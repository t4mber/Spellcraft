library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_process_var is
  port (
    clk : in std_logic;
    result : out integer
  );
end test_process_var;

architecture rtl of test_process_var is
begin
  process(clk)
    variable v_temp : integer := 0;
  begin
    if rising_edge(clk) then
      v_temp := v_temp + 1;
      result <= v_temp;
    end if;
  end process;
end architecture;
