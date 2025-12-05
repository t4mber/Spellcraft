--------------------------------------------------------------------------------
--  File:        codeglow.vhd
--  Project:     Codeglow - Star Glow Effect Generator
--  Description: Top-level module implementing a Trapcode Starglow-inspired
--               effect for FPGA video processing. Creates multicolored
--               star-shaped glows with configurable rays, shimmer, and
--               gradient mapping.
--
--  Author:      t4mber
--  Date:        2025-11-14
--  Version:     1.0.0
--
--  Features:
--    - Configurable star ray count (2-16 points)
--    - Multicolored gradient mapping (warm/cool/rainbow/custom)
--    - Temporal shimmer effects (sine/triangle/pulse/random)
--    - Adjustable ray length and width
--    - Real-time video rate processing
--
--  Pipeline Latency: ~8-10 clock cycles
--  Resource Usage: Optimized for iCE40 HX / ECP5
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity codeglow is
  generic (
    G_WIDTH       : integer := 10;  -- Video bit width (10-bit typical)
    G_COORD_WIDTH : integer := 12   -- Coordinate space (4096x4096 max)
  );
  port (
    -- System
    clk           : in  std_logic;
    reset         : in  std_logic;
    enable        : in  std_logic;

    -- Video input (luminance + coordinates)
    pixel_luma_in : in  unsigned(G_WIDTH - 1 downto 0);
    x_coord       : in  unsigned(G_COORD_WIDTH - 1 downto 0);
    y_coord       : in  unsigned(G_COORD_WIDTH - 1 downto 0);

    -- Star ray controls
    num_rays      : in  unsigned(3 downto 0);              -- 2-16 rays
    ray_length    : in  unsigned(G_WIDTH - 1 downto 0);    -- Streak length
    ray_width     : in  unsigned(G_WIDTH - 1 downto 0);    -- Streak width/softness

    -- Color controls
    color_mode    : in  unsigned(1 downto 0);              -- Gradient type
    saturation    : in  unsigned(G_WIDTH - 1 downto 0);    -- Color intensity
    color_stop0_r : in  unsigned(G_WIDTH - 1 downto 0);    -- Custom color 1
    color_stop0_g : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop0_b : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop1_r : in  unsigned(G_WIDTH - 1 downto 0);    -- Custom color 2
    color_stop1_g : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop1_b : in  unsigned(G_WIDTH - 1 downto 0);

    -- Shimmer/animation controls
    shimmer_rate  : in  unsigned(G_WIDTH - 1 downto 0);    -- Animation speed
    shimmer_depth : in  unsigned(G_WIDTH - 1 downto 0);    -- Modulation amount
    shimmer_mode  : in  unsigned(1 downto 0);              -- Waveform type

    -- RGB output
    red_out       : out unsigned(G_WIDTH - 1 downto 0);
    green_out     : out unsigned(G_WIDTH - 1 downto 0);
    blue_out      : out unsigned(G_WIDTH - 1 downto 0);
    valid_out     : out std_logic
  );
end codeglow;

architecture rtl of codeglow is

  -- Inter-stage signals
  signal ray_intensity     : unsigned(G_WIDTH - 1 downto 0);
  signal ray_valid         : std_logic;

  signal shimmer_intensity : unsigned(G_WIDTH - 1 downto 0);
  signal shimmer_valid     : std_logic;

  signal gradient_r        : unsigned(G_WIDTH - 1 downto 0);
  signal gradient_g        : unsigned(G_WIDTH - 1 downto 0);
  signal gradient_b        : unsigned(G_WIDTH - 1 downto 0);
  signal gradient_valid    : std_logic;

begin

  -- Component 1: Star Ray Generator
  -- Analyzes pixel position and creates directional star pattern
  star_rays : entity work.star_ray_generator
    generic map (
      G_WIDTH       => G_WIDTH,
      G_COORD_WIDTH => G_COORD_WIDTH
    )
    port map (
      clk           => clk,
      enable        => enable,
      pixel_in      => pixel_luma_in,
      x_pos         => x_coord,
      y_pos         => y_coord,
      num_rays      => num_rays,
      ray_length    => ray_length,
      ray_width     => ray_width,
      ray_intensity => ray_intensity,
      valid         => ray_valid
    );

  -- Component 2: Shimmer Modulator
  -- Adds temporal animation and sparkle effects
  shimmer : entity work.shimmer_modulator
    generic map (
      G_WIDTH => G_WIDTH
    )
    port map (
      clk           => clk,
      enable        => ray_valid,
      reset         => reset,
      intensity_in  => ray_intensity,
      shimmer_rate  => shimmer_rate,
      shimmer_depth => shimmer_depth,
      shimmer_mode  => shimmer_mode,
      intensity_out => shimmer_intensity,
      valid         => shimmer_valid
    );

  -- Component 3: Color Gradient Mapper
  -- Maps intensity to RGB color gradient
  color_mapper : entity work.color_gradient_mapper
    generic map (
      G_WIDTH => G_WIDTH
    )
    port map (
      clk           => clk,
      enable        => shimmer_valid,
      intensity     => shimmer_intensity,
      color_mode    => color_mode,
      saturation    => saturation,
      color_stop0_r => color_stop0_r,
      color_stop0_g => color_stop0_g,
      color_stop0_b => color_stop0_b,
      color_stop1_r => color_stop1_r,
      color_stop1_g => color_stop1_g,
      color_stop1_b => color_stop1_b,
      red_out       => gradient_r,
      green_out     => gradient_g,
      blue_out      => gradient_b,
      valid         => gradient_valid
    );

  -- Output assignment
  red_out   <= gradient_r;
  green_out <= gradient_g;
  blue_out  <= gradient_b;
  valid_out <= gradient_valid;

end architecture rtl;
