library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_multiline_portmap is
    port (
        clk : in std_logic;
        data_in_avid : in std_logic
    );
end test_multiline_portmap;

architecture rtl of test_multiline_portmap is
    signal contrast_val : unsigned(7 downto 0);
    signal y_contrast_out : unsigned(7 downto 0);
    signal y_contrast_valid : std_logic;
begin
    y_contrast_inst : entity work.contrast_u
        generic map (G_WIDTH => 8)
        port map (
            clk        => clk,
            enable     => data_in_avid,
            contrast   => contrast_val,
            result     => y_contrast_out,
            valid      => y_contrast_valid
        );
end architecture;
