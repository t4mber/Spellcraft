library ieee;
use ieee.std_logic_1164.all;

entity test_generic is
  generic (
    G_WIDTH : integer := 8
  );
  port (
    data_in : in std_logic;
    data_out : out std_logic
  );
end test_generic;

architecture rtl of test_generic is
begin
  data_out <= data_in;
end architecture;
