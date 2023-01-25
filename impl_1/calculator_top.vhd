library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.constants.all;

entity calculator_top is
    port(
        clk_in : in std_logic;
        rst : in std_logic;
        
        
        avs_address        : in  std_logic_vector(c_avs_address_width - 1 downto 0);
        avs_read           : in  std_logic;
        avs_write          : in  std_logic;
        avs_writedata      : in  std_logic_vector(c_avs_data_width - 1 downto 0);
        avs_readdata       : out std_logic_vector(c_avs_data_width - 1 downto 0);
        avs_readdata_valid : out std_logic;
        avs_waitrequest    : out std_logic;
        
        led : out std_logic
    );
end entity calculator_top;

architecture RTL of calculator_top is
    
        signal sig1 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
        signal sig2 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
        signal sig3 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
        signal sig4 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
        signal sig5 :  std_logic_vector(c_avs_data_width - 1 downto 0);
        signal led_sig : std_logic;
    
begin
    led <= led_sig;

led_calculator_cmp : entity work.led_calculator(behavioural)
    port map(
        clk_in => clk_in,
        rst_in => rst,
        in1 => sig1,
        in2 => sig2,
        in3 => sig3,
        in4 => sig4,
        in5 => sig5,
        led => led_sig
    );
    
    calculator_reg_cmp : entity work.calculator_reg(RTL)
    port map(
        clk                => clk_in,
        rst            => rst,
        avs_address        => avs_address,
        avs_read           => avs_read,
        avs_write          => avs_write,
        avs_writedata      => avs_writedata,
        avs_readdata       => avs_readdata,
        avs_readdata_valid => avs_readdata_valid,
        avs_waitrequest    => avs_waitrequest,
        
        out1 => sig1,
        out2 => sig2,
        out3 => sig3,
        out4 => sig4,
        out5 => sig5,
        led_data => led_sig
        
    );
    
    
end architecture RTL;