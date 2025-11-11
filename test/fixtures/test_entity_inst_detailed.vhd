library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_entity_inst is
    port (
        clk : in std_logic
    );
end test_entity_inst;

architecture rtl of test_entity_inst is
    signal s_data : std_logic_vector(7 downto 0);
    signal s_result : std_logic_vector(7 downto 0);
begin
    -- Test entity instantiation with library prefix
    inst1 : entity work.my_component
        generic map (
            G_WIDTH => 8
        )
        port map (
            clk => clk,
            data => s_data,
            result => s_result
        );
end architecture;
