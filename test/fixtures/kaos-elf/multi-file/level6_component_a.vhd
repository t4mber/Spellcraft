--------------------------------------------------------------------------------
--  File:        level6_component_a.vhd
--  Description: ALU component for KAOS Level 6 multi-file test
--
--  KAOS-ELF: Level 6 - Cross-File Violations
--  This file defines an ALU entity with output ports that drive signals
--  in the top-level module. Without multi-file analysis, these outputs
--  appear as undriven signals in the instantiating module.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity level6_alu is
  generic (
    G_WIDTH : integer := 8
  );
  port (
    clk     : in  std_logic;
    enable  : in  std_logic;
    a       : in  unsigned(G_WIDTH - 1 downto 0);
    b       : in  unsigned(G_WIDTH - 1 downto 0);
    op      : in  std_logic_vector(1 downto 0);
    -- OUTPUT PORTS: These drive signals in instantiating modules
    result  : out unsigned(G_WIDTH - 1 downto 0);
    valid   : out std_logic
  );
end level6_alu;

architecture rtl of level6_alu is
  signal s_result : unsigned(G_WIDTH - 1 downto 0);
  signal s_valid  : std_logic;
begin

  process(clk)
  begin
    if rising_edge(clk) then
      if enable = '1' then
        case op is
          when "00" =>
            s_result <= a + b;
          when "01" =>
            s_result <= a - b;
          when "10" =>
            s_result <= a and b;
          when others =>
            s_result <= a or b;
        end case;
        s_valid <= '1';
      else
        s_valid <= '0';
      end if;
    end if;
  end process;

  result <= s_result;
  valid  <= s_valid;

end architecture;
