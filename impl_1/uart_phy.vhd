--------------------------------------------------------------------------------
-- Copyright (c) 2018
--------------------------------------------------------------------------------
-- Title        : UART physical layer
-- Filename     : uart_phy.vhd
-- Contents     : VHDL entity architecture
--
--------------------------------------------------------------------------------
-- Author(s)    : Ronald Grootelaar
--------------------------------------------------------------------------------
-- Purpose      : UART physical layer implementation  
--------------------------------------------------------------------------------
-- Comment      : Universal asynchronous receiver-transmitter
--                AXI4-Streaming application interface for asynchronous serial data transfer.                
--                BAUDRATE and PARITY settings are defined compile time with generics.
--                The digital input filter depth is set to 1/4 of the bit period.
--                Frame and parity errors are reported through the AXI TUSER sideband signals
--                TUSER(0) = FRAME error
--                TUSER(1) = PARITY error
--                The RX AXI stream can connect directly to a FIFO by connecting 
--                TDATA and TUSER signals to the FIFO data input and TVALID to the FIFO write strobe 
--------------------------------------------------------------------------------
-- Assumptions  : No buffers required. 
--                The FPGA application shall accept all incoming data.
--                The RTS (Request To Send) ouput is set to '1' (fixed). 
--                However, the link partner may deassert CTS (Clear To Send) to pauze
--                dataflow from FPGA (AXI TREADY backpressure is used to stop the dataflow 
--                from FPGA application
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

entity uart_phy is
    generic(
        SYSTEM_CLK_FREQ_Hz   : integer   := 100e6;
        USE_PARITY           : integer   := 0;
        ODD_PARITY           : integer   := 0;
        BAUDRATE             : integer   := 115200;
        START_BIT_LEVEL      : std_logic := '0';
        RTS_CTS_ACTIVE_LEVEL : std_logic := '0'
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
        m_axis_tx_tready : out std_logic -- tready used to control byte output timing and CTS signal         
    );
end uart_phy;

architecture rtl of uart_phy is
    constant START_BIT          : std_logic := START_BIT_LEVEL;
    constant STOP_BIT           : std_logic := not START_BIT;
    constant BIT_PERIOD         : positive  := SYSTEM_CLK_FREQ_Hz / BAUDRATE;
    constant HALF_BIT_PERIOD    : positive  := BIT_PERIOD / 2;
    constant INPUT_FILTER_DEPTH : positive  := BIT_PERIOD / 8;
    constant NUM_SYNC_STAGES    : positive  := 3;
    constant FRAME_ERROR_INDEX  : integer   := 0;
    constant PARITY_ERROR_INDEX : integer   := 1;

    type t_rx_state is (s_idle, s_start, s_data, s_parity, s_stop);
    type t_tx_state is (s_idle, s_start, s_data, s_parity, s_stop, s_ready);

    signal rx_state       : t_rx_state;
    signal tx_state       : t_tx_state;
    signal rx_data_valid  : std_logic;
    signal rx_bit_counter : natural range 0 to BIT_PERIOD - 1;
    signal rx_bit_index   : natural range m_axis_rx_tdata'range := 0;
    signal rx_sync        : std_logic;
    signal rx_sync_c      : std_logic;
    signal parity         : std_logic;
    signal calc_parity    : std_logic;
    signal tx_data        : std_logic_vector(7 downto 0);
    signal tx_bit_counter : natural range 0 to BIT_PERIOD - 1;
    signal tx_bit_index   : natural range m_axis_rx_tdata'range := 0;
    signal cts_sync       : std_logic;

begin
    assert not (rx_data_valid = '1' and m_axis_rx_tready = '0') report "Backpressure not supported on RX path" severity failure;
    m_axis_rx_tvalid <= rx_data_valid;

    rts <= RTS_CTS_ACTIVE_LEVEL;                         -- Always ready for data

    p_rx_state : process(reset, clk)
    begin
        if (reset = '1') then
            rx_bit_counter  <= 0;
            rx_bit_index    <= 0;
            rx_data_valid   <= '0';
            m_axis_rx_tdata <= (others => '0');
            m_axis_rx_tuser <= (others => '0');
            rx_sync_c       <= not START_BIT_LEVEL;
            parity          <= '0';
            rx_state        <= s_idle;
        elsif rising_edge(clk) then
            rx_data_valid <= '0';
            case rx_state is
                when s_idle =>
                    rx_sync_c <= rx_sync;
                    if (rx_sync_c /= START_BIT and rx_sync = START_BIT) then
                        -- Start bit detected, start sampling in the middle of the bit period
                        m_axis_rx_tuser <= (others => '0');
                        rx_bit_counter  <= HALF_BIT_PERIOD - 1;
                        rx_state        <= s_start;
                    end if;

                when s_start =>
                    if (rx_bit_counter = 0) then
                        rx_bit_counter <= BIT_PERIOD - 1;
                        if (rx_sync /= START_BIT) then
                            -- Frame error
                            m_axis_rx_tuser(FRAME_ERROR_INDEX) <= '1';
                        end if;
                        rx_bit_index   <= 0;
                        rx_state       <= s_data;
                    else
                        rx_bit_counter <= rx_bit_counter - 1;
                    end if;
                    parity <= '0';

                when s_data =>
                    if (rx_bit_counter = 0) then
                        rx_bit_counter                <= BIT_PERIOD - 1;
                        -- Read LSB first
                        m_axis_rx_tdata(rx_bit_index) <= rx_sync;
                        parity                        <= parity xor rx_sync;
                        if (rx_bit_index = m_axis_rx_tdata'left) then
                            if (USE_PARITY = 0) then
                                rx_state <= s_stop;
                            else
                                rx_state <= s_parity;
                            end if;
                        else
                            rx_bit_index <= rx_bit_index + 1;
                        end if;
                    else
                        rx_bit_counter <= rx_bit_counter - 1;
                    end if;

                when s_parity =>
                    if (rx_bit_counter = 0) then
                        rx_bit_counter <= BIT_PERIOD - 1;
                        if (ODD_PARITY = 0) then
                            -- Even parity
                            m_axis_rx_tuser(PARITY_ERROR_INDEX) <= parity xor rx_sync;
                        else
                            -- Odd parity
                            m_axis_rx_tuser(PARITY_ERROR_INDEX) <= not (parity xor rx_sync);
                        end if;
                        rx_state       <= s_stop;
                    else
                        rx_bit_counter <= rx_bit_counter - 1;
                    end if;

                when s_stop =>
                    if (rx_bit_counter = 0) then
                        if (rx_sync /= STOP_BIT) then
                            m_axis_rx_tuser(FRAME_ERROR_INDEX) <= '1';
                        end if;
                        rx_data_valid <= '1';
                        rx_state <= s_idle;
                    else
                        rx_bit_counter <= rx_bit_counter - 1;
                    end if;
            end case;
        end if;
    end process p_rx_state;

    p_tx_state : process(reset, clk)
    begin
        if (reset = '1') then
            tx               <= STOP_BIT;
            m_axis_tx_tready <= '0';
            tx_data          <= (others => '0');
            tx_bit_counter   <= 0;
            tx_bit_index     <= 0;
            calc_parity      <= '0';
            tx_state         <= s_idle;
        elsif rising_edge(clk) then

            case tx_state is
                when s_idle =>
                    if (cts_sync /= RTS_CTS_ACTIVE_LEVEL) then
                        -- Not clear to send
                        m_axis_tx_tready <= '0';
                    else
                        -- Clear to send
                        m_axis_tx_tready <= '1';
                        if (m_axis_tx_tvalid = '1') then
                            m_axis_tx_tready <= '0';
                            -- Store data byte
                            tx_data          <= m_axis_tx_tdata;
                            tx_state         <= s_start;
                        end if;
                    end if;

                when s_start =>
                    tx             <= START_BIT;
                    tx_bit_counter <= BIT_PERIOD - 1;
                    tx_bit_index   <= 0;
                    calc_parity    <= '0';
                    tx_state       <= s_data;

                when s_data =>
                    if (tx_bit_counter = 0) then
                        tx_bit_counter <= BIT_PERIOD - 1;
                        -- Output data LSB first
                        tx             <= tx_data(tx_bit_index);
                        calc_parity    <= calc_parity xor tx_data(tx_bit_index);
                        if (tx_bit_index = m_axis_tx_tdata'left) then
                            if (USE_PARITY = 0) then
                                tx_state <= s_stop;
                            else
                                tx_state <= s_parity;
                            end if;
                        else
                            tx_bit_index <= tx_bit_index + 1;
                        end if;
                    else
                        tx_bit_counter <= tx_bit_counter - 1;
                    end if;

                when s_parity =>
                    if (tx_bit_counter = 0) then
                        tx_bit_counter <= BIT_PERIOD - 1;
                        -- Output parity
                        if (ODD_PARITY = 0) then
                            -- Even parity
                            tx <= calc_parity;
                        else
                            -- Odd parity
                            tx <= not calc_parity;
                        end if;
                        tx_state       <= s_stop;
                    else
                        tx_bit_counter <= tx_bit_counter - 1;
                    end if;

                when s_stop =>
                    if (tx_bit_counter = 0) then
                        tx_bit_counter <= BIT_PERIOD - 1;
                        -- Output stop bit
                        tx             <= STOP_BIT;
                        tx_state       <= s_ready;
                    else
                        tx_bit_counter <= tx_bit_counter - 1;
                    end if;

                when s_ready =>
                    if (tx_bit_counter = 0) then
                        m_axis_tx_tready <= '1';
                        tx_state         <= s_idle;
                    else
                        tx_bit_counter <= tx_bit_counter - 1;
                    end if;

            end case;
        end if;
    end process p_tx_state;

    i_sync_debounce_rx : entity work.uart_phy_sync
        generic map(
            NR_SIGNALS           => 1,
            NR_SYNC_STAGES       => NUM_SYNC_STAGES,
            NR_DEBOUNCE_CYCLES   => INPUT_FILTER_DEPTH,
            DEFAULT_OUTPUT_STATE => STOP_BIT
        )
        port map(
            clk                => clk,
            reset              => reset,
            dig_in(0)          => rx,
            dig_out(0)         => rx_sync,
            test_sel_sensor(0) => '0',
            test_dig_in(0)     => '0'
        );

    i_sync_debounce_cts : entity work.uart_phy_sync
        generic map(
            NR_SIGNALS           => 1,
            NR_SYNC_STAGES       => NUM_SYNC_STAGES,
            NR_DEBOUNCE_CYCLES   => INPUT_FILTER_DEPTH,
            DEFAULT_OUTPUT_STATE => '0'
        )
        port map(
            clk                => clk,
            reset              => reset,
            dig_in(0)          => cts,
            dig_out(0)         => cts_sync,
            test_sel_sensor(0) => '0',
            test_dig_in(0)     => '0'
        );
end rtl;
