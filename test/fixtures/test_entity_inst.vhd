library ieee;
use ieee.std_logic_1164.all;

entity test_entity_inst is
  port (
    clk : in std_logic
  );
end test_entity_inst;

architecture rtl of test_entity_inst is
begin
  multiplier_inst : entity work.diff_multiplier_s
    generic map (
      G_WIDTH => 8
    )
    port map (
      clk => clk
    );
end architecture;
