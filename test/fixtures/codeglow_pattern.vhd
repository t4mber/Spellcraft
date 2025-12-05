-- Test fixture replicating codeglow patterns
-- ADC-IMPLEMENTS: spellcraft-adc-008

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity codeglow_pattern is
  generic (
    G_WIDTH : integer := 10
  );
  port (
    clk        : in  std_logic;
    enable     : in  std_logic;
    data_out   : out unsigned(G_WIDTH - 1 downto 0)
  );
end codeglow_pattern;

architecture rtl of codeglow_pattern is
  -- Multi-signal declarations (like color_gradient_mapper.vhd)
  signal s1_r, s1_g, s1_b : unsigned(G_WIDTH - 1 downto 0);

  -- Signal with aggregate initializer
  signal phase_acc : unsigned(17 downto 0) := (others => '0');

  -- Signal with hex literal initializer (like shimmer_modulator.vhd)
  signal lfsr_state : unsigned(15 downto 0) := x"BEEF";

  -- Function declaration in architecture
  function add_one(x : unsigned(G_WIDTH - 1 downto 0)) return unsigned is
    variable result : unsigned(G_WIDTH - 1 downto 0);
  begin
    result := x + 1;
    return result;
  end function;

begin
  process(clk)
  begin
    if rising_edge(clk) then
      if enable = '1' then
        s1_r <= add_one(s1_r);
        s1_g <= s1_g + 1;
        s1_b <= s1_b + 1;
        phase_acc <= phase_acc + 1;
        lfsr_state <= lfsr_state(14 downto 0) & lfsr_state(15);
      end if;
    end if;
  end process;

  data_out <= s1_r;
end architecture rtl;
