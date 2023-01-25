
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.constants.all;

entity calculator_reg is
    port( 
        clk                : in  std_logic;
        rst            : in  std_logic; -- active low
        
        out1               : out std_logic_vector(c_avs_data_width/2 -1 downto 0);
        out2               : out std_logic_vector(c_avs_data_width/2 -1 downto 0);
        out3               : out std_logic_vector(c_avs_data_width/2 -1 downto 0);
        out4               : out std_logic_vector(c_avs_data_width/2 -1 downto 0);
        out5               : out std_logic_vector(c_avs_data_width -  1 downto 0);
       

        avs_address        : in  std_logic_vector(c_avs_address_width - 1 downto 0);
        avs_read           : in  std_logic;
        avs_write          : in  std_logic;
        avs_writedata      : in  std_logic_vector(c_avs_data_width - 1 downto 0);
        avs_readdata       : out std_logic_vector(c_avs_data_width - 1 downto 0);
        avs_readdata_valid : out std_logic;
        avs_waitrequest    : out std_logic;
        
        led_data               : in std_logic
       
       
    );
end entity calculator_reg;

architecture RTL of calculator_reg is
    
    constant c_address_in1   : std_logic_vector(c_avs_address_width - 1 downto 0) := x"00";
    constant c_address_in2   : std_logic_vector(c_avs_address_width - 1 downto 0) := x"04";
    constant c_address_in3   : std_logic_vector(c_avs_address_width - 1 downto 0) := x"08";
    constant c_address_in4   : std_logic_vector(c_avs_address_width - 1 downto 0) := x"0c";
    constant c_address_in5   : std_logic_vector(c_avs_address_width - 1 downto 0) := x"10";
    constant c_address_led   : std_logic_vector(c_avs_address_width - 1 downto 0) := x"14";
     
     signal output1 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
     signal output2 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
     signal output3 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
     signal output4 :  std_logic_vector(c_avs_data_width/2 -1 downto 0);
     signal output5 :  std_logic_vector(c_avs_data_width  - 1 downto 0);
     signal led_sig :  std_logic;
begin
           
    out1 <= output1;
    out2 <= output2;
    out3 <= output3;
    out4 <= output4;
    out5 <= output5;
    led_sig <= led_data;
            
        avs_waitrequest <= '0';

    clocking : process(clk, rst)
        
    begin
        
        if (rst = '1') then
            avs_readdata       <= (others => '0');
            avs_readdata_valid <= '0';
            avs_readdata       <= (others => '0');
            
            output1    <= (others => '0');
            output2    <= (others => '0');
            output3    <= (others => '0');
            output4    <= (others => '0');
            output5    <= (others => '0');
            
        elsif rising_edge(clk) then

            avs_readdata_valid <= '0'; 
            avs_readdata       <= (others => '0');

            if (avs_read = '1') then
                -- avalon read cycle
                avs_readdata_valid <= '1';
                if    (avs_address = c_address_in1) then
                    avs_readdata(output1'range) <= output1;
                elsif (avs_address = c_address_in2) then
                    avs_readdata(output2'range) <= output2;
                elsif (avs_address = c_address_in3) then
                    avs_readdata(output3'range) <= output3;
                elsif (avs_address = c_address_in4) then
                    avs_readdata(output4'range) <= output4;
                elsif (avs_address = c_address_in5) then
                    avs_readdata(output5'range) <= output5;
                elsif (avs_address = c_address_led) then
                    avs_readdata(0) <= led_sig;
                end if;
            end if;

            if (avs_write = '1') then
                -- Avalon write cycle
                if (avs_address = c_address_in1) then
                    output1 <= avs_writedata(output1'range);
                elsif (avs_address = c_address_in2) then
                    output2 <= avs_writedata(output2'range);
                elsif (avs_address = c_address_in3) then
                    output3 <= avs_writedata(output3'range);
                elsif (avs_address = c_address_in4) then
                    output4 <= avs_writedata(output4'range);
                elsif (avs_address = c_address_in5) then
                    output5 <= avs_writedata(output5'range);
                end if;

            end if;
        end if;
    end process;
    
    
    
end architecture RTL;
