library ieee;
use ieee.std_logic_1164.all;

entity test is
  port (
    data_in : in std_logic
  );
end entity;

architecture rtl of test is
  signal s : std_logic;
begin
  process(s)
  begin
    s <= data_in;
  end process;
end architecture;