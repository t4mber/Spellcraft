entity safe_video_pipeline is
  port (
    pixel_clk : in std_logic;
    video_out : out std_logic_vector(23 downto 0)
  );
end entity safe_video_pipeline;

architecture rtl of safe_video_pipeline is
  signal scaled_clk : std_logic;
begin

  pll_inst : component PLL_1
    generic map (
      MULT_FACTOR => 3.0
    )
    port map (
      clk_in => pixel_clk,
      clk_out => scaled_clk
    );

  encoder_inst : component YPbPr_Encoder_A
    port map (
      pixel_clk => scaled_clk,
      video_out => video_out
    );

end architecture rtl;
