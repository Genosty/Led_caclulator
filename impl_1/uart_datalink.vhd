--------------------------------------------------------------------------------
-- Copyright (c) 2018
--------------------------------------------------------------------------------
-- Title        : UART datalink layer
-- Filename     : uart_datalink.vhd
-- Contents     : VHDL entity architecture
--
--------------------------------------------------------------------------------
-- Author(s)    : Ronald Grootelaar
--------------------------------------------------------------------------------
-- Purpose      : UART datalink layer implementation
--------------------------------------------------------------------------------
-- Comment      : Rx message for READ  <r><A><CR>
--                Rx message for WRITE <w><A><D><CR>
--                <A> is address to read or write, same width as avm_address
--                <D> is data to write, same width as avm_writedata
--				  <CR> is Carriage Return character (0xD)
--
--                Tx response for READ <D><CR>
--                Tx response for WRITE <D><CR>
--------------------------------------------------------------------------------
-- Assumptions  : Rx message size > Tx message size
--                No buffering nor flow control required
--------------------------------------------------------------------------------
-- Limitations  : No checksum/CRC
--------------------------------------------------------------------------------
-- Known errors :
--
--------------------------------------------------------------------------------
-- Specification Reference
--
--------------------------------------------------------------------------------
-- Revision History:
-- Date         Author             Comment
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_pkg.all;

entity uart_datalink is
	generic(
		SYSTEM_CLK_FREQ_Hz : integer := 100e6
	);
	port(
		clk                : in  std_logic;
		reset              : in  std_logic;
		-- UART receive stream
		m_axis_rx_tdata    : in  std_logic_vector(7 downto 0); -- Incoming byte
		m_axis_rx_tuser    : in  std_logic_vector(1 downto 0); -- PHY errors
		m_axis_rx_tvalid   : in  std_logic; -- Data valid strobe
		m_axis_rx_tready   : out std_logic;
		-- UART transmit stream
		m_axis_tx_tdata    : out std_logic_vector(7 downto 0); -- Outgoing byte
		m_axis_tx_tvalid   : out std_logic;
		m_axis_tx_tready   : in  std_logic; -- tready used to control byte output timing
		-- Avalon master
		avm_address        : out std_logic_vector(31 downto 0);
		avm_write          : out std_logic;
		avm_writedata      : out std_logic_vector(31 downto 0);
		avm_read           : out std_logic;
		avm_waitrequest    : in  std_logic;
		avm_readdata       : in  std_logic_vector(31 downto 0);
		avm_readdata_valid : in  std_logic
	);
end uart_datalink;

architecture rtl of uart_datalink is

	constant CMD_READ_INDEX  : natural  := avm_address'length / 4;
	constant CMD_WRITE_INDEX : natural  := CMD_READ_INDEX + avm_writedata'length / 4;
	constant MAX_CMD_LENGTH  : positive := CMD_WRITE_INDEX + 1;
	constant RESPONSE_LENGTH : positive := avm_writedata'length / 4 + 1;
	constant AVM_TIMEOUT     : positive := SYSTEM_CLK_FREQ_Hz / (1 sec / 1 us);

	type t_command_type is (COMMAND_UNKNOWN, COMMAND_READ, COMMAND_WRITE);
	type t_command is record
		command_type  : t_command_type;
		command_data  : std_logic_vector(avm_readdata'range);
		command_error : boolean;
		timeout       : boolean;
	end record t_command;
	type t_cmd_state is (s_cmd_idle, s_cmd_read, s_cmd_handle, s_cmd_busy, s_cmd_response);
	type t_tx_state is (s_tx_idle, s_tx_start, s_tx_next);

	signal avm_writedata_i     : std_logic_vector(avm_writedata'range);
	signal cmd_state           : t_cmd_state;
	signal tx_state            : t_tx_state;
	signal avm_timeout_counter : natural range 0 to AVM_TIMEOUT - 1;
	signal uart_tx_byte_count  : natural range 0 to RESPONSE_LENGTH;
	signal rx_phy_error        : std_logic_vector(m_axis_rx_tuser'range);
	signal rx_data             : t_byte_stream(MAX_CMD_LENGTH - 1 downto 0);
	signal rx_data_i           : t_byte_stream(rx_data'range);
	signal rx_data_valid       : std_logic;
	signal command             : t_command;
	signal tx_data             : t_byte_stream(RESPONSE_LENGTH - 1 downto 0);
	signal tx_data_i           : t_byte_stream(tx_data'range);
	signal tx_data_valid       : std_logic;

	function get_value(constant rx_data : t_byte_stream) return std_logic_vector is
		constant BITS_PER_NIBBLE : positive                                                        := 4;
		variable bin_value       : std_logic_vector(BITS_PER_NIBBLE * rx_data'length - 1 downto 0) := (others => '0');
	begin
		for i in rx_data'high downto rx_data'low loop
			bin_value := bin_value(bin_value'left - BITS_PER_NIBBLE downto 0) & ascii2val(rx_data(i));
		end loop;
		return bin_value;
	end function;

	function get_command_type(constant rx_data : t_byte_stream) return t_command_type is
		variable command_type : t_command_type := COMMAND_UNKNOWN;
	begin

		if (rx_data(CMD_READ_INDEX) = char2ascii('r')) then
			command_type := COMMAND_READ;
			for i in 0 to CMD_READ_INDEX - 1 loop
				if (not ascii_is_value(rx_data(i))) then
					command_type := COMMAND_UNKNOWN;
				end if;
			end loop;
		elsif (rx_data(CMD_WRITE_INDEX) = char2ascii('w')) then
			command_type := COMMAND_WRITE;
			for i in 0 to CMD_WRITE_INDEX - 1 loop
				if (not ascii_is_value(rx_data(i))) then
					command_type := COMMAND_UNKNOWN;
				end if;
			end loop;
		end if;
		return command_type;
	end function;

begin
	-- Transmit byte stream

	process(reset, clk) is
	begin
		if (reset = '1') then
			uart_tx_byte_count <= 0;
			m_axis_tx_tdata    <= (others => '0');
			m_axis_tx_tvalid   <= '0';
      tx_data_i          <= (others => t_byte'(others => '-'));
			tx_state           <= s_tx_idle;
		elsif (rising_edge(clk)) then
			case tx_state is
				when s_tx_idle =>
					if (tx_data_valid = '1') then
						tx_data_i <= tx_data;
						tx_state  <= s_tx_start;
					end if;

				when s_tx_start =>
					m_axis_tx_tdata    <= tx_data_i(tx_data_i'left);
					tx_data_i          <= tx_data_i(tx_data_i'left - 1 downto 0) & x"00";
					m_axis_tx_tvalid   <= '1';
					uart_tx_byte_count <= 1;
					tx_state           <= s_tx_next;

				when s_tx_next =>
					if (m_axis_tx_tready = '1') then
						-- Byte accepted
						if (uart_tx_byte_count = tx_data_i'length) then
							-- Ready
							m_axis_tx_tvalid <= '0';
							tx_state         <= s_tx_idle;
						else
							-- Next byte
							m_axis_tx_tdata    <= tx_data_i(tx_data_i'left);
							tx_data_i          <= tx_data_i(tx_data_i'left - 1 downto 0) & x"00";
							uart_tx_byte_count <= uart_tx_byte_count + 1;
						end if;
					end if;
			end case;
		end if;
	end process;

	-- Store and process command
	avm_writedata <= avm_writedata_i;

	process(clk, reset) is
	begin
		if (reset = '1') then
			tx_data             <= (others => t_byte'(others => '-'));
			tx_data_valid       <= '0';
      rx_data_i           <= (others => t_byte'(others => '-'));
      command             <= (
                               command_type  => COMMAND_UNKNOWN,
                               command_data  => (others => '-'),
                               command_error => false,
                               timeout       => false
                             );
			avm_writedata_i     <= (others => '0');
			avm_read            <= '0';
			avm_write           <= '0';
			avm_address         <= (others => '0');
			avm_timeout_counter <= 0;
			cmd_state <= s_cmd_idle;
		elsif (rising_edge(clk)) then
			tx_data_valid <= '0';
			case cmd_state is
				when s_cmd_idle =>
					if (rx_data_valid = '1') then
						-- Store data
						rx_data_i <= rx_data;
						cmd_state <= s_cmd_read;
					end if;

				when s_cmd_read =>
					command.command_type  <= get_command_type(rx_data_i);
					command.command_error <= false;
					command.timeout       <= false;
					cmd_state             <= s_cmd_handle;

				when s_cmd_handle =>
					avm_timeout_counter <= AVM_TIMEOUT - 1;
					if (command.command_type = COMMAND_READ) then
						avm_address <= get_value(rx_data_i(CMD_READ_INDEX - 1 downto 0));
						avm_read    <= '1';
						cmd_state   <= s_cmd_busy;
					elsif (command.command_type = COMMAND_WRITE) then
						avm_address     <= get_value(rx_data_i(CMD_WRITE_INDEX - 1 downto CMD_WRITE_INDEX - avm_address'length / 4));
						avm_write       <= '1';
						avm_writedata_i <= get_value(rx_data_i(CMD_WRITE_INDEX - avm_address'length / 4 - 1 downto 0));
						cmd_state       <= s_cmd_busy;
					else
						command.command_error <= true;
						cmd_state             <= s_cmd_response;
					end if;

				when s_cmd_busy =>
					if (avm_timeout_counter = 0) then
						avm_read        <= '0';
						avm_write       <= '0';
						command.timeout <= true;
						cmd_state       <= s_cmd_response;
					else
						avm_timeout_counter <= avm_timeout_counter - 1;

						if (avm_waitrequest = '0') then
							-- Avalon request accepted by slave
							avm_read  <= '0';
							avm_write <= '0';
						end if;

						if (avm_readdata_valid = '1' and command.command_type = COMMAND_READ) then
							avm_read             <= '0';
							command.command_data <= avm_readdata;
							cmd_state            <= s_cmd_response;
						end if;
						if (avm_waitrequest = '0' and command.command_type = COMMAND_WRITE) then
							command.command_data <= avm_writedata_i;
							cmd_state            <= s_cmd_response;
						end if;
					end if;
				when s_cmd_response =>
					if (command.command_error) then
						tx_data(tx_data'left downto 1) <= char2ascii("E01-CMD ");
					elsif (command.timeout) then
						tx_data(tx_data'left downto 1) <= char2ascii("E02-BUSY");
					else
						tx_data(tx_data'left downto 1) <= val2ascii(command.command_data);
					end if;
					tx_data(0) <= char2ascii(CR);
					tx_data_valid <= '1';
					cmd_state <= s_cmd_idle;
			end case;
		end if;
	end process;

	m_axis_rx_tready <= '1';            -- always ready for data

	process(clk, reset) is
	begin
		if (reset = '1') then
			rx_phy_error  <= (others => '0');
			rx_data       <= (others => (others => '0'));
			rx_data_valid <= '0';
		elsif (rising_edge(clk)) then
			rx_data_valid <= '0';
			if (m_axis_rx_tvalid = '1') then
				rx_phy_error <= rx_phy_error or m_axis_rx_tuser;
				-- Check for Carriage return character
				if (m_axis_rx_tdata = char2ascii(CR)) then
					if (rx_phy_error = "00") then
						-- Accept packets without PHY errors (frame and parity)
						rx_data_valid <= '1';
					end if;
					rx_phy_error <= (others => '0');
				else
					rx_data <= rx_data(rx_data'left - 1 downto 0) & m_axis_rx_tdata;
				end if;
			end if;
		end if;
	end process;

end rtl;
