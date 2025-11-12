library ieee;
use ieee.std_logic_1164.all;

entity constant_test is
  generic (G_WIDTH : integer := 8);
  port (
    clk : in std_logic;
    result : out std_logic
  );
end constant_test;

architecture rtl of constant_test is
  constant C_PROC_WIDTH : integer := G_WIDTH + 2;
  constant C_FRAC_BITS : integer := G_WIDTH;
  signal s_temp : std_logic;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= '0';
    end if;
  end process;
  result <= s_temp;
end architecture;
