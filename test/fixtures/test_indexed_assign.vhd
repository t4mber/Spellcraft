library ieee;
use ieee.std_logic_1164.all;

entity test_indexed is
  port (
    clk : in std_logic;
    a : in std_logic
  );
end test_indexed;

architecture rtl of test_indexed is
  type t_array is array (0 to 3) of std_logic;
  signal s_array : t_array;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_array(0) <= a;
    end if;
  end process;
end architecture;
