library ieee;
library lib_ip_calculator;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use lib_ip_calculator.all;
use work.constants.all;

entity led_calculator is
    port(
        clk_in : in std_logic;
        rst_in : in std_logic;
        in1 : in std_logic_vector(c_avs_data_width/2 -1 downto 0);
        in2 : in std_logic_vector(c_avs_data_width/2 -1 downto 0);
        in3 : in std_logic_vector(c_avs_data_width/2 -1 downto 0);
        in4 : in std_logic_vector(c_avs_data_width/2 -1 downto 0);
        in5 : in std_logic_vector(c_avs_data_width  - 1 downto 0);
        
        led : out std_logic
        
    );
end entity led_calculator;

architecture behavioural of led_calculator is
    
  
    signal in1_reg            :std_logic_vector(c_avs_data_width/2 -1 downto 0);
    signal in2_reg            :std_logic_vector(c_avs_data_width/2 -1 downto 0);
    signal in3_reg            :std_logic_vector(c_avs_data_width/2 -1 downto 0);
    signal in4_reg            :std_logic_vector(c_avs_data_width/2 -1 downto 0);
    signal in5_reg            :std_logic_vector(c_avs_data_width - 1 downto 0);
    --signal sum                :std_logic_vector();
    
begin


    
    
    process (clk_in, rst_in)
        variable mult0,mult1,sum : std_logic_vector(31 downto 0);
    begin
        if(rst_in = '1') then
            in1_reg <= (others => '0');
            in2_reg <= (others => '0');
            in3_reg <= (others => '0');
            in4_reg <= (others => '0');
            in5_reg <= (others => '0');
        elsif(rising_edge(clk_in)) then
            in1_reg <= in1;
            in2_reg <= in2;
            in3_reg <= in3;
            in4_reg <= in4;
            in5_reg <= in5;
            
            mult0 := std_logic_vector(unsigned(in1_reg) * unsigned(in2_reg));
            mult1 := std_logic_vector(unsigned(in3_reg) * unsigned(in4_reg)); 
            sum   := std_logic_vector(unsigned(mult0)   + unsigned(mult1)); 
            
            if(sum > in5_reg)
                then 
                led <= '1';
            else 
                led <= '0';
                
                end if;
        else
            --
            end if;
      
    end process;
    
    
    
    
    
end architecture behavioural;
