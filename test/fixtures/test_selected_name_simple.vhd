library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_selected_name_simple is
    port (clk : in std_logic);
end test_selected_name_simple;

architecture rtl of test_selected_name_simple is
    type t_data is record
        value : unsigned(7 downto 0);
        valid : std_logic;
    end record;

    signal data : t_data;
    signal result : unsigned(7 downto 0);
    signal flag : std_logic;
begin
    -- Simple field access
    result <= data.value;
    flag <= data.valid;
end architecture;
