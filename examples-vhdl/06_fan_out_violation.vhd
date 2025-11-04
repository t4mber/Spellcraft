-- Example 6: Fan-Out Violation
-- A single PLL output drives too many encoder instances

entity multi_encoder_system is
  port (
    sys_clk : in std_logic;  -- 50 MHz
    video_out1 : out std_logic_vector(23 downto 0);
    video_out2 : out std_logic_vector(23 downto 0);
    video_out3 : out std_logic_vector(23 downto 0);
    video_out4 : out std_logic_vector(23 downto 0);
    video_out5 : out std_logic_vector(23 downto 0);
    video_out6 : out std_logic_vector(23 downto 0);
    video_out7 : out std_logic_vector(23 downto 0);
    video_out8 : out std_logic_vector(23 downto 0);
    video_out9 : out std_logic_vector(23 downto 0);
    video_out10 : out std_logic_vector(23 downto 0);
    video_out11 : out std_logic_vector(23 downto 0)
  );
end entity multi_encoder_system;

architecture rtl of multi_encoder_system is
  signal shared_clk : std_logic;
begin

  pll_inst : component PLL_1
    generic map (
      MULT_FACTOR => 3.0
    )
    port map (
      clk_in => sys_clk,
      clk_out => shared_clk  -- This output has max fan-out of 10
    );

  -- VIOLATION: 11 instances connected to shared_clk, but max fan-out is 10
  enc1 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out1);
  enc2 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out2);
  enc3 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out3);
  enc4 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out4);
  enc5 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out5);
  enc6 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out6);
  enc7 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out7);
  enc8 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out8);
  enc9 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out9);
  enc10 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out10);
  enc11 : component YPbPr_Encoder_A port map (pixel_clk => shared_clk, video_out => video_out11);
  -- ERROR: 11 loads > 10 max fan-out

end architecture rtl;
