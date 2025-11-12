library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_portmap_record_field is
    port (
        clk : in std_logic
    );
end test_portmap_record_field;

architecture rtl of test_portmap_record_field is
    -- Define a record type
    type t_data is record
        y : unsigned(7 downto 0);
        avid : std_logic;
    end record;

    signal data_in : t_data;
    signal y_contrast_out : unsigned(7 downto 0);
    signal y_contrast_valid : std_logic;
    signal contrast_val : unsigned(7 downto 0);
begin
    -- Component instantiation with record field access in port map
    y_contrast_inst : entity work.contrast_u
        generic map (G_WIDTH => 8)
        port map (
            clk        => clk,
            enable     => data_in.avid,
            a          => unsigned(data_in.y),
            contrast   => contrast_val,
            result     => y_contrast_out,
            valid      => y_contrast_valid
        );
end architecture;
