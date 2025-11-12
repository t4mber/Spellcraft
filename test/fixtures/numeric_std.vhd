library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity simple_test is
  generic (
    G_WIDTH : integer := 8
  );
  port (clk : in std_logic);
end entity;

architecture rtl of simple_test is
begin
end architecture;
