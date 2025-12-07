--------------------------------------------------------------------------------
--  File:        enhance_u.vhd
--  Description: Detail Enhancer
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity enhance_u_chaos_level2_partial is
  generic (
    G_WIDTH : integer := 10
  );
  port (
    clk         : in  std_logic;
    enable      : in  std_logic;
    a           : in  unsigned(G_WIDTH - 1 downto 0);
    gain        : in  unsigned(G_WIDTH - 1 downto 0); 
    cutoff      : in  unsigned(G_WIDTH - 1 downto 0); 
    mode        : in  unsigned(2 downto 0);
    result      : out unsigned(G_WIDTH - 1 downto 0);
    valid       : out std_logic
  );
end enhance_u_chaos_level2_partial;

architecture rtl of enhance_u_chaos_level2_partial is
  constant C_PROC_WIDTH    : integer := G_WIDTH + 2;
  constant C_FRAC_BITS     : integer := G_WIDTH;
  signal s_input : signed(C_PROC_WIDTH - 1 downto 0);
  signal s_gain : signed(C_PROC_WIDTH - 1 downto 0);
  signal s_cutoff : unsigned(7 downto 0);
  signal s_filter_high_pass : signed(C_PROC_WIDTH - 1 downto 0);
  signal s_filter_low_pass : signed(C_PROC_WIDTH - 1 downto 0);
  signal s_filter_valid : std_logic;
  signal s_multiplier_result : signed(C_PROC_WIDTH - 1 downto 0);
  signal s_multiplier_valid : std_logic;
  signal s_rectifier_result : signed(C_PROC_WIDTH - 1 downto 0);
  signal s_rectifier_valid : std_logic;
  signal s_rectifier_mode : unsigned(2 downto 0);
  signal s_enable : std_logic;
  -- CHAOS-MONKEY: level2-partial
  -- SUBTLETY: 2 (Moderate)
  -- VIOLATION: Signal assigned in some paths, not others
  signal s_sometimes_driven : std_logic;
begin

  process(clk)
  begin
    if rising_edge(clk) then
      if s_valid = '1' then
        s_sometimes_driven <= '1'; -- Only assigned here!
      end if;
      s_input <= resize(signed('0' & std_logic_vector(a)), C_PROC_WIDTH);
      s_cutoff <= unsigned(std_logic_vector(cutoff(C_FRAC_BITS - 1 downto C_FRAC_BITS - s_cutoff'length))); 
      s_gain <= resize(signed('0' & std_logic_vector(gain)), C_PROC_WIDTH); 
      s_rectifier_mode <= mode;
      s_enable <= enable;
    end if;
  end process;

  filter_inst : entity work.filter_s
    generic map (
      G_WIDTH => C_PROC_WIDTH
    )
    port map (
      clk     => clk,
      enable  => s_enable,
      a       => s_input,
      cutoff  => s_cutoff,
      high_pass  => s_filter_high_pass,
      low_pass   => s_filter_low_pass,
      valid   => s_filter_valid
    );

    rectifier_inst : entity work.complex_rectifier_inverter_s
    generic map (
      G_WIDTH => C_PROC_WIDTH
    )
    port map (
      clk => clk,
      mode_in => s_rectifier_mode,
      data_enable => s_filter_valid,
      data_in => s_filter_high_pass,
      data_out => s_rectifier_result,
      data_valid => s_rectifier_valid
    );

  multiplier_inst : entity work.diff_multiplier_s
    generic map (
      G_WIDTH => C_PROC_WIDTH,
      G_FRAC_BITS => C_FRAC_BITS - 2,
      G_OUTPUT_MIN => 0,
      G_OUTPUT_MAX => (2 ** C_FRAC_BITS) - 1
    )
    port map (
      clk     => clk,
      enable  => s_rectifier_valid,
      x_pos   => s_rectifier_result,
      x_neg   => to_signed(0, C_PROC_WIDTH),
      y_pos   => s_gain,
      y_neg   => to_signed(0, C_PROC_WIDTH),
      z_pos   => s_input,
      z_neg   => to_signed(0, C_PROC_WIDTH),
      result  => s_multiplier_result,
      valid   => s_multiplier_valid
    );

  result <= unsigned(std_logic_vector(s_multiplier_result(C_FRAC_BITS - 1 downto 0)));
  valid <= s_multiplier_valid;

end architecture;
