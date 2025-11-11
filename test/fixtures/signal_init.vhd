library ieee;
use ieee.std_logic_1164.all;

entity signal_init_test is
  port (
    data_out : out std_logic
  );
end signal_init_test;

architecture rtl of signal_init_test is
  signal s_temp : std_logic := '0';
begin
  data_out <= s_temp;
end architecture;
