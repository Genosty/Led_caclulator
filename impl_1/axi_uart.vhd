--------------------------------------------------------------------------------
-- Copyright (c) 2020, 3T 
--------------------------------------------------------------------------------
-- Title        : AXI UART
-- Filename     : axi_uart.vhd
-- Contents     : VHDL entity architecture
--
--------------------------------------------------------------------------------
-- Author(s)    : Ronald Grootelaar
--------------------------------------------------------------------------------
-- Purpose      : UART with AXI master to read/write registers
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

entity axi_uart is
	generic(
		SYSTEM_CLK_FREQ_Hz : integer   := 100e6;
		USE_PARITY         : integer   := 0;
		ODD_PARITY         : integer   := 0;
		BAUDRATE           : integer   := 115200;
		START_BIT_LEVEL    : std_logic := '0'
	);
	port(
		-- AXI master
		m_axi_aclk    : in  std_logic;
		m_axi_aresetn : in  std_logic;
		m_axi_awvalid : out std_logic;
		m_axi_awready : in  std_logic;
		m_axi_awaddr  : out std_logic_vector(31 downto 0);
		m_axi_awprot  : out std_logic_vector(2 downto 0);
		m_axi_wvalid  : out std_logic;
		m_axi_wready  : in  std_logic;
		m_axi_wdata   : out std_logic_vector(31 downto 0);
		m_axi_wstrb   : out std_logic_vector(3 downto 0);
		m_axi_bvalid  : in  std_logic;
		m_axi_bready  : out std_logic;
		m_axi_bresp   : in  std_logic_vector(1 downto 0);
		m_axi_arvalid : out std_logic;
		m_axi_arready : in  std_logic;
		m_axi_araddr  : out std_logic_vector(31 downto 0);
		m_axi_arprot  : out std_logic_vector(2 downto 0);
		m_axi_rvalid  : in  std_logic;
		m_axi_rready  : out std_logic;
		m_axi_rdata   : in  std_logic_vector(31 downto 0);
		m_axi_rresp   : in  std_logic_vector(1 downto 0);
		-- UART
		tx            : out std_logic;
		cts           : in  std_logic;
		rx            : in  std_logic;
		rts           : out std_logic
	);
end axi_uart;

architecture rtl of axi_uart is
	constant AXI_TIMEOUT_VALUE : positive := SYSTEM_CLK_FREQ_Hz / (1 sec / 1 us);

	type t_axi_state is (s_axi_idle, s_axi_read, s_axi_write, s_axi_wait_wready, s_axi_wait_awready, s_axi_ready);

	signal reset                     : std_logic;
	signal axi_state                 : t_axi_state;
	signal avm_address               : std_logic_vector(31 downto 0);
	signal avm_write                 : std_logic;
	signal avm_writedata             : std_logic_vector(31 downto 0);
	signal avm_read                  : std_logic;
	signal avm_waitrequest           : std_logic;
	signal avm_readdata              : std_logic_vector(31 downto 0);
	signal avm_readdata_valid        : std_logic;
	signal start_axi_timeout_counter : std_logic;
	signal stop_axi_timeout_counter  : std_logic;	
	signal axi_timeout               : boolean;
	signal axi_timeout_counter       : natural range 0 to AXI_TIMEOUT_VALUE;

	component uart is
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
	end component;

begin
	reset <= not m_axi_aresetn;

	m_axi_awprot <= "010";
	m_axi_wstrb  <= "1111";
	m_axi_bready <= '1';
	m_axi_arprot <= "010";
	m_axi_rready <= '1';

	p_m_axi : process(m_axi_aclk, m_axi_aresetn)
	begin
		if (m_axi_aresetn = '0') then
			avm_waitrequest           <= '1';
			avm_readdata              <= (others => '-');
			avm_readdata_valid        <= '0';
			m_axi_arvalid             <= '0';
			m_axi_awvalid             <= '0';
			m_axi_wvalid              <= '0';
			m_axi_araddr              <= (others => '-');
			m_axi_awaddr              <= (others => '-');
			start_axi_timeout_counter <= '0';
			stop_axi_timeout_counter  <= '0';
			axi_state                 <= s_axi_idle;

		elsif rising_edge(m_axi_aclk) then
			start_axi_timeout_counter <= '0';
			stop_axi_timeout_counter  <= '0';
			avm_readdata_valid        <= '0';

			case axi_state is
				when s_axi_idle =>
					if (avm_read = '1') then
						m_axi_araddr              <= avm_address;
						m_axi_arvalid             <= '1';
						avm_waitrequest           <= '1'; -- Hold internal bus
						start_axi_timeout_counter <= '1';
						axi_state                 <= s_axi_read;
					elsif (avm_write = '1') then
						m_axi_awaddr              <= avm_address;
						m_axi_awvalid             <= '1';
						m_axi_wvalid              <= '1';
						m_axi_wdata               <= avm_writedata;
						avm_waitrequest           <= '1'; -- Hold internal bus
						start_axi_timeout_counter <= '1';
						axi_state                 <= s_axi_write;
					end if;

				when s_axi_read =>
					if (axi_timeout) then
						m_axi_arvalid <= '0';
						axi_state     <= s_axi_ready;
					else
						if (m_axi_arready = '1') then
							m_axi_arvalid <= '0';
						end if;
						if (m_axi_rvalid = '1') then
							avm_readdata       <= m_axi_rdata;
							avm_readdata_valid <= '1';
							axi_state          <= s_axi_ready;
						end if;
					end if;

				when s_axi_write =>
					if (axi_timeout) then
						m_axi_awvalid <= '0';
						m_axi_wvalid  <= '0';
						axi_state     <= s_axi_idle;
					elsif (m_axi_awready = '1' and m_axi_wready = '1') then
						m_axi_awvalid <= '0';
						m_axi_wvalid <= '0';
						axi_state     <= s_axi_ready;
					elsif (m_axi_awready = '1') then
						m_axi_awvalid <= '0';
						axi_state     <= s_axi_wait_wready;
					elsif (m_axi_wready = '1') then
						m_axi_wvalid <= '0';
						axi_state    <= s_axi_wait_awready;
					end if;

				when s_axi_wait_wready =>
					if (m_axi_wready = '1' or axi_timeout) then
						m_axi_wvalid <= '0';
						axi_state    <= s_axi_ready;
					end if;

				when s_axi_wait_awready =>
					if (m_axi_awready = '1' or axi_timeout) then
						m_axi_awvalid <= '0';
						axi_state     <= s_axi_ready;
					end if;

				when s_axi_ready =>
					avm_waitrequest <= '0';
					stop_axi_timeout_counter <= '1';
					axi_state       <= s_axi_idle;
			end case;
		end if;
	end process;

	axi_timeout <= true when (axi_timeout_counter = 1) else false;

	p_axi_timeout : process(m_axi_aclk, m_axi_aresetn)
	begin
		if (m_axi_aresetn = '0') then
			axi_timeout_counter <= 0;
		elsif rising_edge(m_axi_aclk) then
			if (start_axi_timeout_counter = '1') then
				axi_timeout_counter <= AXI_TIMEOUT_VALUE;
			elsif (stop_axi_timeout_counter = '1') then
				axi_timeout_counter <= 0;
			else
				if (axi_timeout_counter = 0) then
				-- Wait for start request
				else
					axi_timeout_counter <= axi_timeout_counter - 1;
				end if;
			end if;
		end if;
	end process;

	i_uart : uart
		generic map(
			SYSTEM_CLK_FREQ_Hz => SYSTEM_CLK_FREQ_Hz,
			USE_PARITY         => USE_PARITY,
			ODD_PARITY         => ODD_PARITY,
			BAUDRATE           => BAUDRATE
		)
		port map(
			clk                => m_axi_aclk,
			reset              => reset,
			tx                 => tx,
			cts                => cts,
			rx                 => rx,
			rts                => rts,
			avm_address        => avm_address,
			avm_write          => avm_write,
			avm_writedata      => avm_writedata,
			avm_read           => avm_read,
			avm_waitrequest    => avm_waitrequest,
			avm_readdata       => avm_readdata,
			avm_readdata_valid => avm_readdata_valid
		);

end rtl;
