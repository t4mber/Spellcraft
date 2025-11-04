entity top is
  port (
    pixel_clk : in std_logic
  );
end entity top;

architecture rtl of top is
  signal high_clk : std_logic;
begin
  pll_inst : component PLL_1
    generic map (
      MULT_FACTOR => 4.16
    )
    port map (
      clk_in => pixel_clk,
      clk_out => high_clk
    );

  encoder_inst : component YPbPr_Encoder_A
    port map (
      pixel_clk => high_clk
    );
end architecture rtl;
