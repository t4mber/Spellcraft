library ieee;
use ieee.std_logic_1164.all;

entity test_record_type is
    port (
        clk : in std_logic
    );
end test_record_type;

architecture rtl of test_record_type is
    type t_data is record
        avid : std_logic;
    end record;
begin
end architecture;
