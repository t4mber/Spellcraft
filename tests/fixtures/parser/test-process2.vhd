library ieee;
use ieee.std_logic_1164.all;

entity test_entity is
  port (
    clk : in std_logic;
    input : in std_logic;
    output : out std_logic
  );
end entity;

architecture rtl of test_entity is
  signal s_data : std_logic;
  signal s_temp : std_logic;
begin
  process(clk)
  begin
    s_data <= input;
    s_temp <= s_data;
    output <= s_temp;
  end process;
end architecture;