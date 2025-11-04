-- Example 5: Generic Parameter Out of Range
-- PLL multiplication factor exceeds allowed range

entity bad_pll_config is
  port (
    clk_in : in std_logic;
    clk_out : out std_logic
  );
end entity bad_pll_config;

architecture rtl of bad_pll_config is
begin

  -- VIOLATION: MULT_FACTOR = 12.5 exceeds max of 10.0
  -- (PLL_1 constraint: MULT_FACTOR range 1.0 to 10.0)
  pll_inst : component PLL_1
    generic map (
      MULT_FACTOR => 12.5  -- ERROR: > 10.0 max
    )
    port map (
      clk_in => clk_in,
      clk_out => clk_out
    );

end architecture rtl;
