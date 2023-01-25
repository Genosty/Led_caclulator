library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity Counter is
  port (clk    : in    std_logic;
           rst_n : in    std_logic;
           led : inout std_logic;
		   led_prob : inout std_logic
           );
end Counter;

architecture Behavioral of Counter is
  signal scounter   : std_logic_vector(4 downto 0);
  signal clkcounter : integer := 0;
  
begin
  process(clk, rst_n)
  begin
    if (rst_n = '1') then
      scounter   <= (others => '0');
      led        <= '0';
	  led_prob   <= '0';
      clkcounter <= 0;
    elsif (clk'event and clk = '1') then
      if (clkcounter = 50000000) then 
 --    if (clkcounter = 1125) then  -- for simulation	  
        clkcounter <= 0;
        led        <= (not(led));		--when the button is active, led is off
        led_prob   <= led;
	  else
        clkcounter <= clkcounter + 1;
      end if;
  end if;
end process;

end Behavioral;