-- ADC-028: Test conditional signal assignment parsing
-- Tests: output <= a when sel = '1' else b;

library ieee;
use ieee.std_logic_1164.all;

entity conditional_test is
  port (
    sel : in std_logic;
    a : in std_logic;
    b : in std_logic;
    c : in std_logic;
    output1 : out std_logic;
    output2 : out std_logic
  );
end entity conditional_test;

architecture rtl of conditional_test is
begin
  -- Simple conditional assignment
  output1 <= a when sel = '1' else b;

  -- Chained conditional assignment
  output2 <= a when sel = '0' else
             b when sel = '1' else
             c;
end architecture rtl;
