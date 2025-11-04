-- Example 2: Multiple PLL Cascading Violation
-- Demonstrates cascaded PLLs creating excessive frequency

entity clock_multiplier is
  port (
    sys_clk : in std_logic;  -- 25 MHz system clock
    ultra_fast_clk : out std_logic
  );
end entity clock_multiplier;

architecture rtl of clock_multiplier is
  signal clk_stage1 : std_logic;
  signal clk_stage2 : std_logic;
begin

  -- First PLL: 25 MHz × 4 = 100 MHz
  pll1_inst : component PLL_1
    generic map (
      MULT_FACTOR => 4.0
    )
    port map (
      clk_in => sys_clk,
      clk_out => clk_stage1
    );

  -- Second PLL: 100 MHz × 3 = 300 MHz
  pll2_inst : component PLL_1
    generic map (
      MULT_FACTOR => 3.0
    )
    port map (
      clk_in => clk_stage1,
      clk_out => clk_stage2
    );

  -- VIOLATION: Encoder receives 300 MHz, max is 165 MHz!
  encoder_inst : component YPbPr_Encoder_A
    port map (
      pixel_clk => clk_stage2,  -- ERROR: 300 MHz > 165 MHz
      video_out => ultra_fast_clk
    );

end architecture rtl;
