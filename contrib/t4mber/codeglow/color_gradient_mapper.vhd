--------------------------------------------------------------------------------
--  File:        color_gradient_mapper.vhd
--  Project:     Codeglow - Star Glow Effect Generator
--  Description: Maps glow intensity to RGB color gradient with configurable
--               color stops. Supports both monochromatic and multicolored
--               gradients for magical/realistic effects.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity color_gradient_mapper is
  generic (
    G_WIDTH : integer := 10  -- Bit width for color channels
  );
  port (
    clk            : in  std_logic;
    enable         : in  std_logic;

    -- Input intensity (0 = dark, max = bright)
    intensity      : in  unsigned(G_WIDTH - 1 downto 0);

    -- Gradient control
    color_mode     : in  unsigned(1 downto 0);  -- 00=warm, 01=cool, 10=rainbow, 11=custom
    saturation     : in  unsigned(G_WIDTH - 1 downto 0);  -- Color intensity

    -- Custom color stops (for mode 11)
    color_stop0_r  : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop0_g  : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop0_b  : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop1_r  : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop1_g  : in  unsigned(G_WIDTH - 1 downto 0);
    color_stop1_b  : in  unsigned(G_WIDTH - 1 downto 0);

    -- RGB Output
    red_out        : out unsigned(G_WIDTH - 1 downto 0);
    green_out      : out unsigned(G_WIDTH - 1 downto 0);
    blue_out       : out unsigned(G_WIDTH - 1 downto 0);
    valid          : out std_logic
  );
end color_gradient_mapper;

architecture rtl of color_gradient_mapper is

  -- Pipeline registers
  signal s1_intensity  : unsigned(G_WIDTH - 1 downto 0);
  signal s1_mode       : unsigned(1 downto 0);
  signal s1_saturation : unsigned(G_WIDTH - 1 downto 0);
  signal s1_c0_r, s1_c0_g, s1_c0_b : unsigned(G_WIDTH - 1 downto 0);
  signal s1_c1_r, s1_c1_g, s1_c1_b : unsigned(G_WIDTH - 1 downto 0);
  signal s1_valid      : std_logic;

  -- Gradient calculation (stage 2)
  signal s2_r_base     : unsigned(G_WIDTH - 1 downto 0);
  signal s2_g_base     : unsigned(G_WIDTH - 1 downto 0);
  signal s2_b_base     : unsigned(G_WIDTH - 1 downto 0);
  signal s2_intensity  : unsigned(G_WIDTH - 1 downto 0);
  signal s2_saturation : unsigned(G_WIDTH - 1 downto 0);
  signal s2_valid      : std_logic;

  -- Constants for predefined gradients
  constant MAX_VAL : unsigned(G_WIDTH - 1 downto 0) := (others => '1');
  constant MID_VAL : unsigned(G_WIDTH - 1 downto 0) := '0' & (G_WIDTH - 2 downto 0 => '1');
  constant ZERO    : unsigned(G_WIDTH - 1 downto 0) := (others => '0');

begin

  -- Stage 1: Input registration and gradient selection
  process(clk)
    variable r_temp, g_temp, b_temp : unsigned(G_WIDTH - 1 downto 0);
    variable intensity_scaled : unsigned(G_WIDTH - 1 downto 0);
  begin
    if rising_edge(clk) then
      if enable = '1' then
        s1_intensity  <= intensity;
        s1_mode       <= color_mode;
        s1_saturation <= saturation;
        s1_c0_r       <= color_stop0_r;
        s1_c0_g       <= color_stop0_g;
        s1_c0_b       <= color_stop0_b;
        s1_c1_r       <= color_stop1_r;
        s1_c1_g       <= color_stop1_g;
        s1_c1_b       <= color_stop1_b;
        s1_valid      <= '1';
      else
        s1_valid <= '0';
      end if;
    end if;
  end process;

  -- Stage 2: Generate base gradient colors
  process(clk)
    variable r_temp, g_temp, b_temp : unsigned(G_WIDTH - 1 downto 0);
    variable inv_intensity : unsigned(G_WIDTH - 1 downto 0);
  begin
    if rising_edge(clk) then
      if s1_valid = '1' then
        inv_intensity := MAX_VAL - s1_intensity;

        case s1_mode is
          when "00" =>  -- Warm gradient: Black -> Orange -> Yellow -> White
            if s1_intensity < MID_VAL then
              -- Black to orange phase
              r_temp := shift_left(s1_intensity, 1);  -- Red grows fast
              g_temp := shift_right(s1_intensity, 1);  -- Green grows slower
              b_temp := ZERO;                          -- No blue
            else
              -- Orange to white phase
              r_temp := MAX_VAL;
              g_temp := shift_left(s1_intensity - MID_VAL, 1);
              b_temp := shift_right(s1_intensity - MID_VAL, 2);
            end if;

          when "01" =>  -- Cool gradient: Black -> Blue -> Cyan -> White
            if s1_intensity < MID_VAL then
              -- Black to blue phase
              r_temp := ZERO;
              g_temp := shift_right(s1_intensity, 2);
              b_temp := shift_left(s1_intensity, 1);
            else
              -- Blue to white phase
              r_temp := shift_right(s1_intensity - MID_VAL, 1);
              g_temp := shift_left(s1_intensity - MID_VAL, 1);
              b_temp := MAX_VAL;
            end if;

          when "10" =>  -- Rainbow gradient: spectral colors
            -- Divide intensity into 3 regions: R->G, G->B, B->R
            if s1_intensity < resize(MAX_VAL / 3, G_WIDTH) then
              -- Red to Green
              r_temp := MAX_VAL - (s1_intensity * 3);
              g_temp := s1_intensity * 3;
              b_temp := ZERO;
            elsif s1_intensity < resize((MAX_VAL * 2) / 3, G_WIDTH) then
              -- Green to Blue
              r_temp := ZERO;
              g_temp := MAX_VAL - ((s1_intensity - resize(MAX_VAL / 3, G_WIDTH)) * 3);
              b_temp := (s1_intensity - resize(MAX_VAL / 3, G_WIDTH)) * 3;
            else
              -- Blue to Red
              r_temp := (s1_intensity - resize((MAX_VAL * 2) / 3, G_WIDTH)) * 3;
              g_temp := ZERO;
              b_temp := MAX_VAL - ((s1_intensity - resize((MAX_VAL * 2) / 3, G_WIDTH)) * 3);
            end if;

          when "11" =>  -- Custom: interpolate between two color stops
            -- Linear interpolation: color = c0 + (c1 - c0) * intensity
            r_temp := s1_c0_r + resize(shift_right((s1_c1_r - s1_c0_r) * s1_intensity, G_WIDTH), G_WIDTH);
            g_temp := s1_c0_g + resize(shift_right((s1_c1_g - s1_c0_g) * s1_intensity, G_WIDTH), G_WIDTH);
            b_temp := s1_c0_b + resize(shift_right((s1_c1_b - s1_c0_b) * s1_intensity, G_WIDTH), G_WIDTH);

          when others =>
            r_temp := s1_intensity;
            g_temp := s1_intensity;
            b_temp := s1_intensity;
        end case;

        s2_r_base     <= r_temp;
        s2_g_base     <= g_temp;
        s2_b_base     <= b_temp;
        s2_intensity  <= s1_intensity;
        s2_saturation <= s1_saturation;
        s2_valid      <= '1';
      else
        s2_valid <= '0';
      end if;
    end if;
  end process;

  -- Stage 3: Apply saturation and output
  process(clk)
    variable gray       : unsigned(G_WIDTH - 1 downto 0);
    variable r_out, g_out, b_out : unsigned(G_WIDTH - 1 downto 0);
    variable r_mul, g_mul, b_mul : unsigned(G_WIDTH * 2 - 1 downto 0);
  begin
    if rising_edge(clk) then
      if s2_valid = '1' then
        -- Calculate grayscale value for desaturation
        -- gray = 0.299*R + 0.587*G + 0.114*B
        -- Approximation: gray ≈ (R + G + B) / 3
        gray := resize(shift_right(resize(s2_r_base, G_WIDTH + 2) +
                                     resize(s2_g_base, G_WIDTH + 2) +
                                     resize(s2_b_base, G_WIDTH + 2), 2), G_WIDTH);

        -- Apply saturation: output = gray + (color - gray) * saturation
        -- Simplification: output = gray * (1 - saturation) + color * saturation
        r_mul := (gray * (MAX_VAL - s2_saturation)) + (s2_r_base * s2_saturation);
        g_mul := (gray * (MAX_VAL - s2_saturation)) + (s2_g_base * s2_saturation);
        b_mul := (gray * (MAX_VAL - s2_saturation)) + (s2_b_base * s2_saturation);

        -- Normalize (divide by MAX_VAL)
        red_out   <= resize(shift_right(r_mul, G_WIDTH), G_WIDTH);
        green_out <= resize(shift_right(g_mul, G_WIDTH), G_WIDTH);
        blue_out  <= resize(shift_right(b_mul, G_WIDTH), G_WIDTH);
        valid     <= '1';
      else
        valid <= '0';
      end if;
    end if;
  end process;

end architecture rtl;
