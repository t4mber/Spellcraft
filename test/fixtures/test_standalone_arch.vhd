library ieee;
use ieee.std_logic_1164.all;

-- Standalone architecture without entity in same file
-- Entity 'external_entity' must be defined elsewhere
architecture test_arch of external_entity is
  signal s_data : std_logic;
begin
  s_data <= '0';
end architecture;
