-- Example VDHL design with frequency violation (PRD test case)
-- This demonstrates the 208 MHz > 165 MHz violation

entity top is
  port (
    pixel_clk : in std_logic
  );
end entity top;

architecture rtl of top is
  signal high_clk : std_logic;
begin
  -- PLL multiplies 50 MHz by 4.16 = 208 MHz
  pll_inst : component PLL_1
    generic map (
      MULT_FACTOR => 4.16
    )
    port map (
      clk_in => pixel_clk,
      clk_out => high_clk
    );

  -- YPbPr encoder can only handle 165 MHz max
  -- This should trigger a frequency violation!
  encoder_inst : component YPbPr_Encoder_A
    port map (
      pixel_clk => high_clk
    );
end architecture rtl;
