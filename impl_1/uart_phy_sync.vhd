--------------------------------------------------------------------------------
-- Copyright (c) 2018, 
--------------------------------------------------------------------------------
-- Title        : Synchronization and debouncing
-- Filename     : uart_phy_sync.vhd
-- Contents     : VHDL entity architecture
--
--------------------------------------------------------------------------------
-- Author(s)    : Ronald Grootelaar
--------------------------------------------------------------------------------
-- Purpose      : Synchronize and debounce external inputs
--------------------------------------------------------------------------------
-- Comment      : 
--
--------------------------------------------------------------------------------
-- Assumptions  : 
--
--------------------------------------------------------------------------------
-- Limitations  : 
--------------------------------------------------------------------------------
-- Known errors : 
--
--------------------------------------------------------------------------------
-- Specification Reference:
--
--------------------------------------------------------------------------------
-- Revision History:
-- Date         Author             Comment
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_phy_sync is
	generic(
		NR_SIGNALS           : positive;
		NR_SYNC_STAGES       : positive  := 3;
		NR_DEBOUNCE_CYCLES   : positive  := 10;
		DEFAULT_OUTPUT_STATE : std_logic := '0'
	);
	port(
		clk             : in  std_logic;
		reset           : in  std_logic;
		dig_in          : in  std_logic_vector(NR_SIGNALS - 1 downto 0);
		dig_out         : out std_logic_vector(NR_SIGNALS - 1 downto 0);
		test_sel_sensor : in  std_logic_vector(NR_SIGNALS - 1 downto 0);
		test_dig_in     : in  std_logic_vector(NR_SIGNALS - 1 downto 0)
	);
end uart_phy_sync;

architecture rtl of uart_phy_sync is
	subtype t_sync_stage is std_logic_vector(NR_SYNC_STAGES - 1 downto 0);
	type t_sync_stages is array (integer range <>) of t_sync_stage;
	subtype t_debounce_counter is natural range 0 to NR_DEBOUNCE_CYCLES - 1;
	type t_debounce_counters is array (integer range <>) of t_debounce_counter;

	signal sync_stages       : t_sync_stages(NR_SIGNALS - 1 downto 0);
	signal dig_in_sync_prev  : std_logic_vector(NR_SIGNALS - 1 downto 0);
	signal debounce_counters : t_debounce_counters(NR_SIGNALS - 1 downto 0) := (others => NR_DEBOUNCE_CYCLES - 1);
	signal dig_out_i         : std_logic_vector(NR_SIGNALS - 1 downto 0);

begin
	g_dig_out : for i in dig_out'range generate
		dig_out(i) <= dig_out_i(i) when test_sel_sensor(i) = '0' else test_dig_in(i);
	end generate;

	p_sync_input : process(clk, reset)
	begin
		if (reset = '1') then
			sync_stages <= (others => (others => DEFAULT_OUTPUT_STATE));
		elsif rising_edge(clk) then
			for i in sync_stages'range loop
				sync_stages(i) <= sync_stages(i)(t_sync_stage'left - 1 downto 0) & dig_in(i);
			end loop;
		end if;
	end process p_sync_input;

	p_debounce_input : process(clk, reset)
	begin
		if (reset = '1') then
			debounce_counters <= (others => NR_DEBOUNCE_CYCLES - 1);
			dig_out_i         <= (others => DEFAULT_OUTPUT_STATE);
			dig_in_sync_prev  <= (others => DEFAULT_OUTPUT_STATE);
		elsif rising_edge(clk) then
			for i in debounce_counters'range loop
				dig_in_sync_prev(i) <= sync_stages(i)(t_sync_stage'left);
				if (dig_in_sync_prev(i) = sync_stages(i)(t_sync_stage'left)) then
					if (debounce_counters(i) = 0) then
						dig_out_i(i) <= sync_stages(i)(t_sync_stage'left);
					else
						debounce_counters(i) <= debounce_counters(i) - 1;
					end if;
				else
					debounce_counters(i) <= NR_DEBOUNCE_CYCLES - 1;
				end if;
			end loop;
		end if;
	end process p_debounce_input;

end rtl;
