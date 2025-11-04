entity video_pipeline is
  port (
    pixel_clk : in std_logic;
    video_out : out std_logic_vector(23 downto 0)
  );
end entity video_pipeline;

architecture rtl of video_pipeline is
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
      pixel_clk => high_clk,
      video_out => video_out
    );

end architecture rtl;
