library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity constant_test is
  generic (G_WIDTH : integer := 10);
  port (
    clk : in std_logic;
    a : in unsigned(7 downto 0);
    result : out signed(11 downto 0)
  );
end constant_test;

architecture rtl of constant_test is
  constant C_WIDTH : integer := G_WIDTH + 2;
  signal s_temp : signed(11 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= resize(signed('0' & std_logic_vector(a)), C_WIDTH);
    end if;
  end process;

  result <= s_temp;
end architecture;
