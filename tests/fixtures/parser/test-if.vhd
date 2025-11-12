library ieee;
use ieee.std_logic_1164.all;

entity test is
  port (
    clk : in std_logic;
    data_in : in std_logic
  );
end entity;

architecture rtl of test is
  signal s : std_logic;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s <= data_in;
    end if;
  end process;
end architecture;
