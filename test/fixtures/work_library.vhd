library work;
use work.all;

entity test_entity is
  port (
    clk : in std_logic;
    data_out : out std_logic_vector(7 downto 0)
  );
end entity test_entity;

architecture rtl of test_entity is
begin
  data_out <= (others => '0');
end architecture rtl;
