library ieee;
use ieee.std_logic_1164.all;

entity concat_op_test is
  port (
    clk : in std_logic;
    a : in std_logic_vector(7 downto 0);
    result : out std_logic_vector(8 downto 0)
  );
end concat_op_test;

architecture rtl of concat_op_test is
  signal s_temp : std_logic_vector(8 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= '0' & a;
    end if;
  end process;

  result <= s_temp;
end architecture;
