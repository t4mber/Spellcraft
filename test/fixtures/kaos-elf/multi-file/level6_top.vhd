--------------------------------------------------------------------------------
--  File:        level6_top.vhd
--  Description: Top-level module for KAOS Level 6 multi-file test
--
--  KAOS-ELF: Level 6 - Cross-File Violations
--
--  This module instantiates components from other files. When analyzed alone,
--  signals driven by component output ports appear "undriven" (false positives).
--  When analyzed with component files, these false positives are eliminated.
--
--  EXPECTED VIOLATIONS (Multi-File Analysis):
--    - orphan_signal: Declared but never assigned (truly undriven)
--    - dead_signal: Assigned but never read (truly unused)
--
--  FALSE POSITIVES (Single-File Analysis):
--    - alu_result: Appears undriven, but driven by level6_alu.result
--    - alu_valid: Appears undriven, but driven by level6_alu.valid
--    - reg_q: Appears undriven, but driven by level6_register.q
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity level6_top is
  generic (
    G_WIDTH : integer := 8
  );
  port (
    clk         : in  std_logic;
    rst         : in  std_logic;
    enable      : in  std_logic;
    input_a     : in  unsigned(G_WIDTH - 1 downto 0);
    input_b     : in  unsigned(G_WIDTH - 1 downto 0);
    op_select   : in  std_logic_vector(1 downto 0);
    output_data : out unsigned(G_WIDTH - 1 downto 0);
    output_valid: out std_logic
  );
end level6_top;

architecture rtl of level6_top is
  -- Signals driven by component output ports
  -- With multi-file analysis: correctly identified as driven
  -- With single-file analysis: incorrectly flagged as undriven
  signal alu_result : unsigned(G_WIDTH - 1 downto 0);
  signal alu_valid  : std_logic;
  signal reg_q      : unsigned(G_WIDTH - 1 downto 0);

  -- CHAOS-MONKEY: level6-multifile-undriven
  -- SUBTLETY: 3 (Moderate - requires multi-file context)
  -- VIOLATION: Signal declared but genuinely never assigned
  signal orphan_signal : std_logic;

  -- CHAOS-MONKEY: level6-multifile-unused
  -- SUBTLETY: 2 (Moderate)
  -- VIOLATION: Signal assigned but genuinely never read
  signal dead_signal : unsigned(G_WIDTH - 1 downto 0);

begin

  -- Instantiate ALU component
  -- Output ports 'result' and 'valid' drive alu_result and alu_valid
  alu_inst : entity work.level6_alu
    generic map (
      G_WIDTH => G_WIDTH
    )
    port map (
      clk     => clk,
      enable  => enable,
      a       => input_a,
      b       => input_b,
      op      => op_select,
      result  => alu_result,  -- Driven by component output
      valid   => alu_valid    -- Driven by component output
    );

  -- Instantiate register component
  -- Output port 'q' drives reg_q
  reg_inst : entity work.level6_register
    generic map (
      G_WIDTH => G_WIDTH
    )
    port map (
      clk     => clk,
      rst     => rst,
      load    => alu_valid,
      d       => alu_result,
      q       => reg_q        -- Driven by component output
    );

  -- Connect outputs using component-driven signals
  -- This proves the signals are actually used (read)
  output_data  <= reg_q;      -- Uses reg_q (read)
  output_valid <= alu_valid;  -- Uses alu_valid (read)

  -- Assign dead_signal but never read it
  -- This is a genuine "unused signal" violation
  process(clk)
  begin
    if rising_edge(clk) then
      dead_signal <= alu_result;  -- Written but never read anywhere
    end if;
  end process;

  -- Note: orphan_signal is declared but never assigned anywhere
  -- This is a genuine "undriven signal" violation

end architecture;
