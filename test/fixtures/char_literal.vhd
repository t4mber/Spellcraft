library ieee;
use ieee.std_logic_1164.all;

entity char_literal_test is
  port (
    clk : in std_logic;
    result : out std_logic
  );
end char_literal_test;

architecture rtl of char_literal_test is
begin
  process(clk)
  begin
    if rising_edge(clk) then
      result <= '0';
    end if;
  end process;
end architecture;
