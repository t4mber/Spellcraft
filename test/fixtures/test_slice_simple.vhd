library ieee;
use ieee.std_logic_1164.all;

entity test_slice_simple is
  port (
    clk : in std_logic
  );
end test_slice_simple;

architecture rtl of test_slice_simple is
  signal s_vec : std_logic_vector(7 downto 0);
  signal s_result : std_logic_vector(6 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_result <= s_vec(6 downto 0);
    end if;
  end process;
end architecture;
