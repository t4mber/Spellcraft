-- Example 4: Boundary Case Violation
-- Just barely exceeds the limit by 0.1 MHz

entity boundary_test is
  port (
    clk_in : in std_logic;  -- 50 MHz
    data_out : out std_logic_vector(7 downto 0)
  );
end entity boundary_test;

architecture rtl of boundary_test is
  signal clk_165_1 : std_logic;
begin

  -- PLL: 50 MHz × 3.302 = 165.1 MHz (just over limit!)
  pll_inst : component PLL_1
    generic map (
      MULT_FACTOR => 3.302
    )
    port map (
      clk_in => clk_in,
      clk_out => clk_165_1
    );

  -- VIOLATION: 165.1 MHz > 165.0 MHz (even 0.1 MHz over is a violation)
  encoder_inst : component YPbPr_Encoder_A
    port map (
      pixel_clk => clk_165_1,  -- ERROR: 165.1 MHz > 165 MHz
      video_out => data_out
    );

end architecture rtl;
