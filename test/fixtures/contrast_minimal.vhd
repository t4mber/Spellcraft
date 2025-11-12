library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity contrast_u is
  generic (G_WIDTH : integer := 8);
  port (
    clk         : in  std_logic;
    enable      : in  std_logic;
    a           : in  unsigned(G_WIDTH - 1 downto 0);
    contrast    : in  unsigned(G_WIDTH - 1 downto 0);
    brightness  : in  unsigned(G_WIDTH - 1 downto 0);
    result      : out unsigned(G_WIDTH - 1 downto 0);
    valid       : out std_logic
  );
end contrast_u;

architecture rtl of contrast_u is
  constant C_PROC_WIDTH    : integer := G_WIDTH + 2;
  constant C_FRAC_BITS     : integer := G_WIDTH;
  signal s_input : signed(C_PROC_WIDTH - 1 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_input <= (others => '0');
    end if;
  end process;
end architecture;
