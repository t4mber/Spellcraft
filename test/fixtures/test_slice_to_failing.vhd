library ieee;
use ieee.std_logic_1164.all;

entity test_slice_to_fixed is
  port (
    clk : in std_logic
  );
end test_slice_to_fixed;

architecture rtl of test_slice_to_fixed is
  type t_array is array (0 to 7) of std_logic;
  signal s_arr : t_array;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_arr <= s_arr(0 to 6) & '0';
    end if;
  end process;
end architecture;
