--------------------------------------------------------------------------------
--  File:        shimmer_modulator.vhd
--  Project:     Codeglow - Star Glow Effect Generator
--  Description: Temporal modulation for creating shimmer, twinkle, and
--               animation effects. Uses low-frequency oscillators (LFOs)
--               to animate glow intensity over time.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity shimmer_modulator is
  generic (
    G_WIDTH : integer := 10  -- Bit width for modulation values
  );
  port (
    clk            : in  std_logic;
    enable         : in  std_logic;
    reset          : in  std_logic;

    -- Input intensity
    intensity_in   : in  unsigned(G_WIDTH - 1 downto 0);

    -- Shimmer control
    shimmer_rate   : in  unsigned(G_WIDTH - 1 downto 0);  -- LFO frequency
    shimmer_depth  : in  unsigned(G_WIDTH - 1 downto 0);  -- Modulation amount
    shimmer_mode   : in  unsigned(1 downto 0);  -- 00=sine, 01=triangle, 10=pulse, 11=random

    -- Output modulated intensity
    intensity_out  : out unsigned(G_WIDTH - 1 downto 0);
    valid          : out std_logic
  );
end shimmer_modulator;

architecture rtl of shimmer_modulator is

  -- LFO phase accumulator
  signal phase_acc       : unsigned(G_WIDTH + 7 downto 0) := (others => '0');
  signal phase           : unsigned(7 downto 0);  -- 0-255 phase

  -- LFSR for pseudo-random shimmer
  signal lfsr_state      : unsigned(15 downto 0) := x"BEEF";  -- Seed
  signal random_val      : unsigned(G_WIDTH - 1 downto 0);

  -- Pipeline stages
  signal s1_intensity    : unsigned(G_WIDTH - 1 downto 0);
  signal s1_depth        : unsigned(G_WIDTH - 1 downto 0);
  signal s1_mode         : unsigned(1 downto 0);
  signal s1_lfo          : unsigned(G_WIDTH - 1 downto 0);
  signal s1_valid        : std_logic;

  signal s2_modulated    : unsigned(G_WIDTH - 1 downto 0);
  signal s2_valid        : std_logic;

  -- Generate sine-like approximation from phase
  function phase_to_sine(phase_in : unsigned(7 downto 0)) return unsigned is
    variable result : unsigned(G_WIDTH - 1 downto 0);
    variable phase_shifted : unsigned(7 downto 0);
  begin
    -- Use triangular approximation for sine
    -- 0-127: rising (0 to MAX)
    -- 128-255: falling (MAX to 0)
    if phase_in < 128 then
      result := resize(shift_left(resize(phase_in, G_WIDTH), G_WIDTH - 7), G_WIDTH);
    else
      result := resize(shift_left(resize(255 - phase_in, G_WIDTH), G_WIDTH - 7), G_WIDTH);
    end if;
    return result;
  end function;

  -- Generate triangle wave from phase
  function phase_to_triangle(phase_in : unsigned(7 downto 0)) return unsigned is
    variable result : unsigned(G_WIDTH - 1 downto 0);
  begin
    if phase_in < 128 then
      result := resize(shift_left(resize(phase_in, G_WIDTH), G_WIDTH - 7), G_WIDTH);
    else
      result := resize(shift_left(resize(255 - phase_in, G_WIDTH), G_WIDTH - 7), G_WIDTH);
    end if;
    return result;
  end function;

  -- Generate pulse wave from phase
  function phase_to_pulse(phase_in : unsigned(7 downto 0)) return unsigned is
    variable result : unsigned(G_WIDTH - 1 downto 0);
    constant MAX_VAL : unsigned(G_WIDTH - 1 downto 0) := (others => '1');
  begin
    if phase_in < 128 then
      result := MAX_VAL;
    else
      result := (others => '0');
    end if;
    return result;
  end function;

begin

  -- Phase accumulator process (always running when enabled)
  process(clk)
    variable rate_scaled : unsigned(G_WIDTH + 7 downto 0);
  begin
    if rising_edge(clk) then
      if reset = '1' then
        phase_acc <= (others => '0');
      elsif enable = '1' then
        -- Accumulate phase based on shimmer rate
        rate_scaled := resize(shimmer_rate, phase_acc'length);
        phase_acc <= phase_acc + rate_scaled;
      end if;
    end if;
  end process;

  -- Extract upper bits as phase (0-255)
  phase <= phase_acc(phase_acc'high downto phase_acc'high - 7);

  -- LFSR for random modulation
  process(clk)
    variable feedback : std_logic;
  begin
    if rising_edge(clk) then
      if reset = '1' then
        lfsr_state <= x"BEEF";
      elsif enable = '1' then
        -- 16-bit LFSR with taps at 16, 15, 13, 4
        feedback := lfsr_state(15) xor lfsr_state(14) xor lfsr_state(12) xor lfsr_state(3);
        lfsr_state <= lfsr_state(14 downto 0) & feedback;
      end if;
    end if;
  end process;

  -- Extract random value from LFSR
  random_val <= resize(lfsr_state(G_WIDTH - 1 downto 0), G_WIDTH);

  -- Stage 1: Generate LFO waveform
  process(clk)
    variable lfo_temp : unsigned(G_WIDTH - 1 downto 0);
  begin
    if rising_edge(clk) then
      if enable = '1' then
        s1_intensity <= intensity_in;
        s1_depth     <= shimmer_depth;
        s1_mode      <= shimmer_mode;

        -- Generate waveform based on mode
        case shimmer_mode is
          when "00" =>  -- Sine
            lfo_temp := phase_to_sine(phase);
          when "01" =>  -- Triangle
            lfo_temp := phase_to_triangle(phase);
          when "10" =>  -- Pulse
            lfo_temp := phase_to_pulse(phase);
          when "11" =>  -- Random
            lfo_temp := random_val;
          when others =>
            lfo_temp := phase_to_sine(phase);
        end case;

        s1_lfo   <= lfo_temp;
        s1_valid <= '1';
      else
        s1_valid <= '0';
      end if;
    end if;
  end process;

  -- Stage 2: Apply modulation to intensity
  process(clk)
    variable depth_scaled  : unsigned(G_WIDTH - 1 downto 0);
    variable lfo_scaled    : unsigned(G_WIDTH - 1 downto 0);
    variable mod_amount    : unsigned(G_WIDTH * 2 - 1 downto 0);
    variable result_temp   : unsigned(G_WIDTH * 2 - 1 downto 0);
    constant MAX_VAL       : unsigned(G_WIDTH - 1 downto 0) := (others => '1');
    constant MID_VAL       : unsigned(G_WIDTH - 1 downto 0) := '0' & (G_WIDTH - 2 downto 0 => '1');
  begin
    if rising_edge(clk) then
      if s1_valid = '1' then
        -- Calculate modulation: intensity * (1 + (lfo - 0.5) * depth)
        -- Rewrite as: intensity + intensity * (lfo - 0.5) * depth

        -- LFO centered around 0.5 (MID_VAL)
        if s1_lfo >= MID_VAL then
          lfo_scaled := s1_lfo - MID_VAL;
        else
          lfo_scaled := MID_VAL - s1_lfo;
        end if;

        -- mod_amount = intensity * lfo_scaled * depth / MAX^2
        mod_amount := s1_intensity * lfo_scaled;
        mod_amount := shift_right(mod_amount, G_WIDTH);  -- Normalize once
        mod_amount := mod_amount * s1_depth;
        mod_amount := shift_right(mod_amount, G_WIDTH);  -- Normalize again

        -- Apply modulation (add or subtract based on LFO)
        if s1_lfo >= MID_VAL then
          result_temp := s1_intensity + resize(mod_amount, G_WIDTH);
        else
          if s1_intensity >= resize(mod_amount, G_WIDTH) then
            result_temp := s1_intensity - resize(mod_amount, G_WIDTH);
          else
            result_temp := (others => '0');
          end if;
        end if;

        -- Clamp to valid range
        if result_temp > MAX_VAL then
          s2_modulated <= MAX_VAL;
        else
          s2_modulated <= resize(result_temp, G_WIDTH);
        end if;

        s2_valid <= '1';
      else
        s2_valid <= '0';
      end if;
    end if;
  end process;

  -- Output assignment
  intensity_out <= s2_modulated;
  valid <= s2_valid;

end architecture rtl;
