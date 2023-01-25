# Led_caclulator
The VHDL files that have been used for the logic circuit in the FPGA vendor comparison.
top_uart_caclulator.vhd is the topmost level.
This module was made and tested on the lattice croslink-NX40 on the Lattice Radiant.
A 100MHz pll have been used and instantiated in the code. The Ip should ben added from the lattice ip catalog.
Same thing goes for the oscilator, and the input of the pll should be added aording to the oscilator. 
The can be used on other FPGAs from different vendors. Only the pll and oschilator must be changed.
