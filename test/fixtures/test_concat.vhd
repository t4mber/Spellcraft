library ieee;
use ieee.std_logic_1164.all;

entity test_concat is
  port (
    clk : in std_logic;
    a : in std_logic;
    b : in std_logic;
    result : out std_logic_vector(1 downto 0)
  );
end test_concat;

architecture rtl of test_concat is
begin
  process(clk)
  begin
    if rising_edge(clk) then
      result <= a & b;
    end if;
  end process;
end architecture;
