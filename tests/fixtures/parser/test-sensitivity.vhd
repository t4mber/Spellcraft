library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity;

architecture rtl of test is
  signal s : std_logic;
begin
  process(s)
  begin
    s <= '1';
  end process;
end architecture;