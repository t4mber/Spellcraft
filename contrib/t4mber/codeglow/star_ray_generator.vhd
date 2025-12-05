--------------------------------------------------------------------------------
--  File:        star_ray_generator.vhd
--  Project:     Codeglow - Star Glow Effect Generator
--  Description: Generates directional star rays with configurable point count
--               and length. Creates the characteristic multi-pointed star
--               pattern by synthesizing radial streaks.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity star_ray_generator is
  generic (
    G_WIDTH       : integer := 10;  -- Video signal bit width
    G_COORD_WIDTH : integer := 12   -- Coordinate space for rays
  );
  port (
    clk           : in  std_logic;
    enable        : in  std_logic;

    -- Input luminance and position
    pixel_in      : in  unsigned(G_WIDTH - 1 downto 0);
    x_pos         : in  unsigned(G_COORD_WIDTH - 1 downto 0);
    y_pos         : in  unsigned(G_COORD_WIDTH - 1 downto 0);

    -- Control parameters
    num_rays      : in  unsigned(3 downto 0);  -- 2-16 rays (4 bits)
    ray_length    : in  unsigned(G_WIDTH - 1 downto 0);  -- Length control
    ray_width     : in  unsigned(G_WIDTH - 1 downto 0);  -- Width control

    -- Output
    ray_intensity : out unsigned(G_WIDTH - 1 downto 0);
    valid         : out std_logic
  );
end star_ray_generator;

architecture rtl of star_ray_generator is

  -- Pipeline stages
  signal s1_pixel      : unsigned(G_WIDTH - 1 downto 0);
  signal s1_x          : unsigned(G_COORD_WIDTH - 1 downto 0);
  signal s1_y          : unsigned(G_COORD_WIDTH - 1 downto 0);
  signal s1_num_rays   : unsigned(3 downto 0);
  signal s1_length     : unsigned(G_WIDTH - 1 downto 0);
  signal s1_width      : unsigned(G_WIDTH - 1 downto 0);
  signal s1_valid      : std_logic;

  -- Polar coordinate approximation (stage 2)
  signal s2_angle_approx : unsigned(7 downto 0);  -- 0-255 for 0-360 degrees
  signal s2_radius       : unsigned(G_COORD_WIDTH - 1 downto 0);
  signal s2_pixel        : unsigned(G_WIDTH - 1 downto 0);
  signal s2_num_rays     : unsigned(3 downto 0);
  signal s2_length       : unsigned(G_WIDTH - 1 downto 0);
  signal s2_width        : unsigned(G_WIDTH - 1 downto 0);
  signal s2_valid        : std_logic;

  -- Ray modulation (stage 3)
  signal s3_ray_mod    : unsigned(G_WIDTH - 1 downto 0);
  signal s3_pixel      : unsigned(G_WIDTH - 1 downto 0);
  signal s3_valid      : std_logic;

  -- Fast approximation: atan2-like using octant logic
  function approx_angle(x, y : unsigned) return unsigned is
    variable abs_x, abs_y : unsigned(G_COORD_WIDTH - 1 downto 0);
    variable octant : unsigned(2 downto 0);
    variable ratio : unsigned(7 downto 0);
  begin
    -- Simple 8-octant approximation
    abs_x := x;
    abs_y := y;

    -- Determine octant based on x/y signs and magnitudes
    if x >= y then
      if y >= (x / 2) then
        octant := "001";  -- ~45 degrees
      else
        octant := "000";  -- ~0 degrees
      end if;
    else
      if x >= (y / 2) then
        octant := "010";  -- ~90-45 degrees
      else
        octant := "011";  -- ~90 degrees
      end if;
    end if;

    -- Convert octant to angle (0-255 range)
    return shift_left(octant, 5);  -- * 32
  end function;

begin

  -- Stage 1: Input registration
  process(clk)
  begin
    if rising_edge(clk) then
      if enable = '1' then
        s1_pixel    <= pixel_in;
        s1_x        <= x_pos;
        s1_y        <= y_pos;
        s1_num_rays <= num_rays;
        s1_length   <= ray_length;
        s1_width    <= ray_width;
        s1_valid    <= '1';
      else
        s1_valid    <= '0';
      end if;
    end if;
  end process;

  -- Stage 2: Polar coordinate approximation
  process(clk)
    variable angle_temp : unsigned(7 downto 0);
    variable radius_temp : unsigned(G_COORD_WIDTH - 1 downto 0);
  begin
    if rising_edge(clk) then
      if s1_valid = '1' then
        -- Fast angle approximation
        angle_temp := approx_angle(s1_x, s1_y);
        s2_angle_approx <= angle_temp;

        -- Simple radius approximation: max(|x|, |y|) + min(|x|, |y|)/2
        if s1_x > s1_y then
          radius_temp := s1_x + shift_right(s1_y, 1);
        else
          radius_temp := s1_y + shift_right(s1_x, 1);
        end if;
        s2_radius <= radius_temp;

        s2_pixel    <= s1_pixel;
        s2_num_rays <= s1_num_rays;
        s2_length   <= s1_length;
        s2_width    <= s1_width;
        s2_valid    <= '1';
      else
        s2_valid <= '0';
      end if;
    end if;
  end process;

  -- Stage 3: Ray modulation
  -- Generate star pattern: modulate intensity based on angle
  process(clk)
    variable ray_angle : unsigned(7 downto 0);
    variable angle_mod : unsigned(7 downto 0);
    variable ray_contribution : unsigned(G_WIDTH - 1 downto 0);
    variable distance_atten : unsigned(G_WIDTH - 1 downto 0);
  begin
    if rising_edge(clk) then
      if s2_valid = '1' then
        -- Calculate which ray we're on (angle modulo ray spacing)
        -- For N rays: ray_spacing = 256 / N
        if s2_num_rays > 0 then
          angle_mod := resize(s2_angle_approx mod (256 / resize(s2_num_rays, 8)), 8);
        else
          angle_mod := s2_angle_approx;
        end if;

        -- Ray intensity: peak at aligned angles, fade between
        -- Use triangular wave: intensity = max(0, width - |angle_mod - 128|)
        if angle_mod < 128 then
          ray_contribution := resize(s2_width - resize(angle_mod, G_WIDTH), G_WIDTH);
        else
          ray_contribution := resize(s2_width - resize(255 - angle_mod, G_WIDTH), G_WIDTH);
        end if;

        -- Distance attenuation: fade with radius / length
        if s2_radius < resize(s2_length, G_COORD_WIDTH) then
          distance_atten := resize(s2_length - resize(s2_radius, G_WIDTH), G_WIDTH);
        else
          distance_atten := (others => '0');
        end if;

        -- Combine ray pattern with distance falloff
        s3_ray_mod <= shift_right(ray_contribution * distance_atten, G_WIDTH);
        s3_pixel   <= s2_pixel;
        s3_valid   <= '1';
      else
        s3_valid <= '0';
      end if;
    end if;
  end process;

  -- Stage 4: Output (multiply pixel intensity by ray modulation)
  process(clk)
    variable result_temp : unsigned(G_WIDTH * 2 - 1 downto 0);
  begin
    if rising_edge(clk) then
      if s3_valid = '1' then
        -- Multiply and normalize
        result_temp := s3_pixel * s3_ray_mod;
        ray_intensity <= resize(shift_right(result_temp, G_WIDTH), G_WIDTH);
        valid <= '1';
      else
        valid <= '0';
      end if;
    end if;
  end process;

end architecture rtl;
