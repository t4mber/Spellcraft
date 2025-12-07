--------------------------------------------------------------------------------
--  File:        level6_component_b.vhd
--  Description: Register component for KAOS Level 6 multi-file test
--
--  KAOS-ELF: Level 6 - Cross-File Violations
--  This file defines a register entity with output port 'q' that drives
--  a signal in the top-level module.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity level6_register is
  generic (
    G_WIDTH : integer := 8
  );
  port (
    clk     : in  std_logic;
    rst     : in  std_logic;
    load    : in  std_logic;
    d       : in  unsigned(G_WIDTH - 1 downto 0);
    -- OUTPUT PORT: Drives signal in instantiating module
    q       : out unsigned(G_WIDTH - 1 downto 0)
  );
end level6_register;

architecture rtl of level6_register is
  signal s_reg : unsigned(G_WIDTH - 1 downto 0);
begin

  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        s_reg <= (others => '0');
      elsif load = '1' then
        s_reg <= d;
      end if;
    end if;
  end process;

  q <= s_reg;

end architecture;
