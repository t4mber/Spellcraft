-- luma_proc.vhd
-- Luma Processor
-- Author: Lars
-- Latency: 9 + (DATAWIDTH / 2) clock cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;

entity luma_proc is
  port (
    clk         : in  std_logic;
    brightness  : in  std_logic_vector;
    contrast    : in  std_logic_vector;
    luma_in     : in  std_logic_vector;
    luma_out    : out std_logic_vector
  );
end entity luma_proc;

architecture rtl of luma_proc is
  constant C_DATA_WIDTH      : integer := luma_in'length;
  constant C_FRAC_MULT       : integer := 2 ** C_DATA_WIDTH;
  constant C_FRAC_MIDPOINT   : integer := C_FRAC_MULT / 2;
  constant C_MATH_WIDTH      : integer := C_DATA_WIDTH + 4;
  constant C_MULT_WIDTH      : integer := C_MATH_WIDTH * 2;
  constant C_OUTPUT_MIN      : integer := 0;
  constant C_OUTPUT_MAX      : integer := (2 ** C_DATA_WIDTH) - 1;

  signal s_luma, s_contrast, s_brightness : signed(C_MATH_WIDTH - 1 downto 0);
  signal s_mult_input_limited : signed(C_MATH_WIDTH - 1 downto 0);
  signal s_mult_result        : signed(C_MULT_WIDTH - 1 downto 0);
  signal s_mult_scaled        : signed(C_MULT_WIDTH - 1 downto 0);
  signal s_mult_limited       : signed(C_MATH_WIDTH - 1 downto 0);
  signal s_sum                : signed(C_MATH_WIDTH - 1 downto 0);
  signal s_sum_limited        : signed(C_MATH_WIDTH - 1 downto 0);

begin
  --

  -- Input conversion and scaling
  process(clk)
  begin
    if rising_edge(clk) then
      s_luma       <= resize(signed('0' & luma_in), s_luma'length);
      s_contrast   <= resize(signed('0' & contrast & '0'), s_contrast'length);
      s_brightness <= resize(signed('0' & brightness & "00"), s_brightness'length)
                      - to_signed(C_FRAC_MULT * 2, s_brightness'length);
    end if;
  end process;

  -- Subtract gray level from input
  offset1 : entity offset
    generic map (
      G_OFFSET => -C_FRAC_MIDPOINT
    )
    port map (
      clk    => clk,
      a      => s_luma,
      result => s_mult_input_limited
    );

  -- Multiply by contrast
  multiply1 : entity multiply
    port map (
      clk    => clk,
      x      => s_mult_input_limited,
      y      => s_contrast,
      result => s_mult_result
    );

  -- Fractional reduction after multiplication
  scale1 : entity scale
    generic map (
      G_SCALE => -C_DATA_WIDTH
    )
    port map (
      clk    => clk,
      a      => s_mult_result,
      result => s_mult_scaled
    );

  -- Add gray level back
  offset2 : entity offset
    generic map (
      G_OFFSET => C_FRAC_MIDPOINT
    )
    port map (
      clk    => clk,
      a      => s_mult_scaled,
      result => s_mult_limited
    );

  -- Add brightness
  add1 : entity add
    port map (
      clk    => clk,
      a      => s_mult_limited,
      b      => s_brightness,
      result => s_sum
    );

  -- Clamp output to valid range
  clamp1 : entity clamp
    generic map (
      G_MIN => C_OUTPUT_MIN,
      G_MAX => C_OUTPUT_MAX
    )
    port map (
      clk    => clk,
      a      => s_sum,
      result => s_sum_limited
    );

  -- Output assignment
  luma_out <= std_logic_vector(s_sum_limited(C_DATA_WIDTH - 1 downto 0));

end architecture rtl;
