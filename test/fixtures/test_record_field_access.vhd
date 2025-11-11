library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_record_field_access is
    port (
        clk : in std_logic
    );
end test_record_field_access;

architecture rtl of test_record_field_access is
    signal data_in_avid : std_logic;
    signal result : std_logic;

    type t_data is record
        avid : std_logic;
    end record;

    signal data_in : t_data;
begin
    -- Simple assignment first
    result <= data_in_avid;

    -- Record field access
    result <= data_in.avid;
end architecture;
