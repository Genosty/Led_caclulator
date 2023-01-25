--------------------------------------------------------------------------------
-- Copyright (c) 2018, 3T 
--------------------------------------------------------------------------------
-- Title        : UART
-- Filename     : uart.vhd
-- Contents     : VHDL entity architecture
--
--------------------------------------------------------------------------------
-- Author(s)    : Ronald Grootelaar
--------------------------------------------------------------------------------
-- Purpose      : UART datalink and application layer
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

entity uart is
	generic(
		SYSTEM_CLK_FREQ_Hz : integer := 100e6;
		USE_PARITY         : integer := 0;
		ODD_PARITY         : integer := 0;
		BAUDRATE           : integer := 115200
	);
	port(
		clk                : in  std_logic;
		reset              : in  std_logic;
		-- UART
		tx                 : out std_logic;
		cts                : in  std_logic;
		rx                 : in  std_logic;
		rts                : out std_logic;
		-- Avalon master
		avm_address        : out std_logic_vector(31 downto 0);
		avm_write          : out std_logic;
		avm_writedata      : out std_logic_vector(31 downto 0);
		avm_read           : out std_logic;
		avm_waitrequest    : in  std_logic;
		avm_readdata       : in  std_logic_vector(31 downto 0);
		avm_readdata_valid : in  std_logic
	);
end uart;

architecture rtl of uart is
	constant START_BIT_LEVEL : std_logic := '0';

	signal m_axis_rx_tdata  : std_logic_vector(7 downto 0);
	signal m_axis_rx_tuser  : std_logic_vector(1 downto 0);
	signal m_axis_rx_tvalid : std_logic;
	signal m_axis_rx_tready : std_logic;
	signal m_axis_tx_tdata  : std_logic_vector(7 downto 0);
	signal m_axis_tx_tvalid : std_logic;
	signal m_axis_tx_tready : std_logic;

	component uart_datalink is
		generic(
			SYSTEM_CLK_FREQ_Hz : integer := 100e6
		);
		port(
			clk                : in  std_logic;
			reset              : in  std_logic;
			-- UART receive stream
			m_axis_rx_tdata    : in  std_logic_vector(7 downto 0); -- Incoming data byte
			m_axis_rx_tuser    : in  std_logic_vector(1 downto 0); -- PHY errors
			m_axis_rx_tvalid   : in  std_logic; -- Data valid strobe
			m_axis_rx_tready   : out std_logic;
			-- UART transmit stream
			m_axis_tx_tdata    : out std_logic_vector(7 downto 0); -- Outgoing data byte
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
	end component;

	component uart_phy is
		generic(
			SYSTEM_CLK_FREQ_Hz : integer   := 100e6;
			USE_PARITY         : integer   := 0;
			ODD_PARITY         : integer   := 0;
			BAUDRATE           : integer   := 115200;
			START_BIT_LEVEL    : std_logic := '0'
		);
		port(
			clk              : in  std_logic;
			reset            : in  std_logic;
			-- hardware interface
			tx               : out std_logic;
			cts              : in  std_logic;
			rx               : in  std_logic;
			rts              : out std_logic;
			-- UART receive stream
			m_axis_rx_tdata  : out std_logic_vector(7 downto 0); -- Incoming data byte
			m_axis_rx_tuser  : out std_logic_vector(1 downto 0); -- Errors
			m_axis_rx_tvalid : out std_logic; -- Data valid strobe
			m_axis_rx_tready : in  std_logic; -- No buffering: tready deassertion results in an overflow error
			-- UART transmit stream
			m_axis_tx_tdata  : in  std_logic_vector(7 downto 0); -- Outgoing data byte
			m_axis_tx_tvalid : in  std_logic;
			m_axis_tx_tready : out std_logic -- tready used to control byte output timing and signal CTS         
		);
	end component;

begin

	i_uart_datalink : uart_datalink
		generic map(
			SYSTEM_CLK_FREQ_Hz => SYSTEM_CLK_FREQ_Hz
		)
		port map(
			clk                => clk,
			reset              => reset,
			m_axis_rx_tdata    => m_axis_rx_tdata,
			m_axis_rx_tuser    => m_axis_rx_tuser,
			m_axis_rx_tvalid   => m_axis_rx_tvalid,
			m_axis_rx_tready   => m_axis_rx_tready,
			m_axis_tx_tdata    => m_axis_tx_tdata,
			m_axis_tx_tvalid   => m_axis_tx_tvalid,
			m_axis_tx_tready   => m_axis_tx_tready,
			avm_address        => avm_address,
			avm_write          => avm_write,
			avm_writedata      => avm_writedata,
			avm_read           => avm_read,
			avm_waitrequest    => avm_waitrequest,
			avm_readdata       => avm_readdata,
			avm_readdata_valid => avm_readdata_valid
		);

	i_uart_phy : uart_phy
		generic map(
			SYSTEM_CLK_FREQ_Hz => SYSTEM_CLK_FREQ_Hz,
			USE_PARITY         => USE_PARITY,
			ODD_PARITY         => ODD_PARITY,
			BAUDRATE           => BAUDRATE,
			START_BIT_LEVEL    => START_BIT_LEVEL
		)
		port map(
			clk              => clk,
			reset            => reset,
			tx               => tx,
			cts              => cts,
			rx               => rx,
			rts              => rts,
			m_axis_rx_tdata  => m_axis_rx_tdata,
			m_axis_rx_tuser  => m_axis_rx_tuser,
			m_axis_rx_tvalid => m_axis_rx_tvalid,
			m_axis_rx_tready => m_axis_rx_tready,    
			m_axis_tx_tdata  => m_axis_tx_tdata,
			m_axis_tx_tvalid => m_axis_tx_tvalid,
			m_axis_tx_tready => m_axis_tx_tready
		);

end rtl;
