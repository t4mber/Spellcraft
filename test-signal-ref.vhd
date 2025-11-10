library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity;

architecture rtl of test is
  signal s : std_logic;
  signal t : std_logic;
begin
  process
  begin
    s <= t;
    wait;
  end process;
end architecture;