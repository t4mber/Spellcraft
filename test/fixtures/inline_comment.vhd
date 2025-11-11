library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inline_comment_test is
  port (
    clk : in std_logic;
    result : out signed(11 downto 0)
  );
end inline_comment_test;

architecture rtl of inline_comment_test is
  signal s_temp : signed(11 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      s_temp <= to_signed(42, 12); -- This is a comment
    end if;
  end process;
  result <= s_temp;
end architecture;
