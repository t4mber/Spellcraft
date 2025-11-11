library ieee;
use ieee.std_logic_1164.all;

entity attribute_test is
  port (
    clk : in std_logic;
    a : in std_logic_vector(7 downto 0);
    result : out integer
  );
end attribute_test;

architecture rtl of attribute_test is
  signal s_temp : integer;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= a'length;
    end if;
  end process;

  result <= s_temp;
end architecture;
