library ieee;
library lib_uart;
library lib_ip_calculator;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use lib_uart.all;

entity top_uart_calculator is
    port(
        clk_in  : in std_logic;
        reset_n : in std_logic;--(should be active low )
        
        rx: in std_logic;
        --cts: in std_logic;
        tx: out std_logic;
        --rts: out std_logic;
        
        led_n : out std_logic;
		
		counter_led : inout std_logic;
		counter_led_prob : inout std_logic
        
    );
end entity top_uart_calculator;

architecture RTL of top_uart_calculator is
    
    signal clk                : std_logic;
    signal locked_sig         : std_logic;
    signal rst                : std_logic;
    signal rst_in             : std_logic;
	
	--signal clk_in			: std_logic;
    
    signal    av_address        :   std_logic_vector(31 downto 0);
    signal    av_write          :   std_logic;
    signal    av_writedata      :   std_logic_vector(31 downto 0);
    signal    av_read           :   std_logic;
    signal    av_waitrequest    :   std_logic;
    signal    av_readdata       :   std_logic_vector(31 downto 0);
    signal    av_readdata_valid :   std_logic;
    signal    led               :   std_logic;
	
	
	
	component osc_0 is
    port(
        hf_out_en_i: in std_logic;
        hf_clk_out_o: out std_logic
    );
end component;
component pll_block_100mhz is
    port(
        clki_i: in std_logic;
        rstn_i: in std_logic;
        clkop_o: out std_logic;
        lock_o: out std_logic
    );
end component;

	component Counter is port(
    clk : in    std_logic;
    rst_n : in    std_logic;
    led : inout std_logic;
	led_prob : inout std_logic
    );
	end component;

begin

	Counter_inst : Counter
    port map(
      clk => clk,
      rst_n => rst,
      led => counter_led,
	  led_prob => open
      );  
    
	osc_0_inst : osc_0 port map(
    hf_out_en_i=> '1',
    hf_clk_out_o=> counter_led_prob --150mhz
);

	pll_block_100mhz_inst: pll_block_100mhz port map(
    clki_i=>  clk_in,
    rstn_i=>  reset_n,
    clkop_o=> clk,--100mhz
    lock_o=>  locked_sig
);

    
    
    rst               <= not locked_sig;


calculator_top_cmp : entity work.calculator_top(RTL)
    port map (
        clk_in => clk,
        rst  => rst,
        --Avalon
        avs_address => av_address(7 downto 0),
        avs_read  => av_read,
        avs_write => av_write,
        avs_writedata => av_writedata,
        avs_readdata => av_readdata,
        avs_readdata_valid => av_readdata_valid,
        avs_waitrequest => av_waitrequest,
        led => led
        
        
    );
    
uart_cmp : entity work.uart(rtl)
    generic map(
        SYSTEM_CLK_FREQ_Hz => 12e6,
        USE_PARITY => 0,
        ODD_PARITY => 0,
        BAUDRATE => 115200
    )
    port map(
        clk     =>    clk,          
        reset   => rst,          
        -- UART
        tx  => tx,              
        cts => '0',             
        rx  => rx,               
        rts => open,               
        -- Avalon master
        avm_address  => av_address,  -- check this later    
        avm_write    => av_write,      
        avm_writedata => av_writedata,     
        avm_read      => av_read,      
        avm_waitrequest  => av_waitrequest,  
        avm_readdata      => av_readdata, 
        avm_readdata_valid => av_readdata_valid
        
    );
    
    led_n <= not led;
	--counter_led_prob <= clk;
end architecture RTL;
