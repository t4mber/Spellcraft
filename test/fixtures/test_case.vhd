library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_case is
  port (
    clk : in std_logic
  );
end test_case;

architecture rtl of test_case is
  signal s_mode : unsigned(2 downto 0);
  signal s_data : signed(7 downto 0);
begin
  process(clk)
  begin
    if rising_edge(clk) then
      case s_mode is
        when "000" =>
          s_data <= to_signed(0, 8);
        when "001" =>
          s_data <= to_signed(1, 8);
        when "010" =>
          s_data <= to_signed(2, 8);
        when others =>
          s_data <= to_signed(-1, 8);
      end case;
    end if;
  end process;
end architecture;
