library ieee;
use ieee.std_logic_1164.all;

entity simple_slice is
  port (
    data_in : in std_logic_vector(7 downto 0);
    data_out : out std_logic_vector(3 downto 0)
  );
end simple_slice;

architecture rtl of simple_slice is
begin
  data_out <= data_in(3 downto 0);
end architecture;
