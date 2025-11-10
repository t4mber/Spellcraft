library ieee;
use ieee.std_logic_1164.all;

entity simple_test is
  port (
    clk : in std_logic;
    data_out : out std_logic
  );
end simple_test;

architecture rtl of simple_test is
  signal internal_signal : std_logic;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      internal_signal <= '1';
    end if;
  end process;

  data_out <= internal_signal;
end architecture;
