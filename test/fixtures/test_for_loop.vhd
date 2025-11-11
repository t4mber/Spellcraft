library ieee;
use ieee.std_logic_1164.all;

entity test_for_loop is
  port (
    clk : in std_logic
  );
end test_for_loop;

architecture rtl of test_for_loop is
  type t_array is array (0 to 3) of std_logic;
  signal s_array : t_array;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      for i in 0 to 3 loop
        s_array(i) <= '0';
      end loop;
    end if;
  end process;
end architecture;
