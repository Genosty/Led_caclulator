library ieee;
use ieee.std_logic_1164.all;
package constants is

	--constant c_number_of_leds : integer := 4;

	--constant c_max_count : integer := 2 ** c_number_of_leds - 1;
	
	--constant c_pll_counter : integer := 100000000;--2500000 -- pll is running at 5Mhz
	-- synthesis translate_off
	--/ 500000
	-- synthesis translate_on
	
	
	constant c_avs_data_width : integer := 32; -- I added this cuz 32 will be more than enough for the LEDs
	constant c_avs_address_width : integer := 8;-- this one has to be 8 cuz somewhere in the code its already initiated with x00 
	-- each digit of the hexa decimal represent 4 binary bits
	--type t_counter_state is (idle, increasing, decreasing);
	
end package constants;