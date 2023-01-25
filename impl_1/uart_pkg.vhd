--------------------------------------------------------------------------------
-- Copyright (c) 2018
--------------------------------------------------------------------------------
-- Title        : UART package
-- Filename     : uart_pkg.vhd
-- Contents     : VHDL package
--
--------------------------------------------------------------------------------
-- Author(s)    : Ronald Grootelaar
--------------------------------------------------------------------------------
-- Purpose      : UART data conversion functions
--------------------------------------------------------------------------------
-- Comment      :
--
--------------------------------------------------------------------------------
-- Assumptions  :
--
--------------------------------------------------------------------------------
-- Limitations  :
--
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

package uart_pkg is
	type t_characters is array (integer range <>) of character;
	subtype t_byte is std_logic_vector(7 downto 0);
	
	type t_byte_stream is array (integer range <>) of t_byte;

	function char2ascii(char : character) return std_logic_vector;
	function char2ascii(char : t_characters) return t_byte_stream;
	function ascii2char(char : std_logic_vector) return character;
	function ascii2val(byte : std_logic_vector(7 downto 0)) return std_logic_vector;
	function ascii_is_value(byte : std_logic_vector(7 downto 0)) return boolean;
	function val2ascii(value : std_logic_vector) return t_byte_stream;

end uart_pkg;

package body uart_pkg is

	function char2ascii(char : character) return std_logic_vector is
		variable r : std_logic_vector(7 downto 0);
	begin
		r := std_logic_vector(to_unsigned(character'pos(char), 8));
		return r;
	end function;

	function char2ascii(char : t_characters) return t_byte_stream is
		variable r : t_byte_stream(char'range);
	begin
		for i in r'range loop
			r(i) := char2ascii(char(i));
		end loop;
		return r;
	end function;

	function ascii2char(char : std_logic_vector) return character is
	begin
		return character'val(to_integer(unsigned(char)));
	end function;

	function ascii2val(byte : std_logic_vector(7 downto 0)) return std_logic_vector is
		variable r : std_logic_vector(3 downto 0);
	begin
		case byte is
			when std_logic_vector(to_unsigned(character'pos('0'), 8)) => r := x"0";
			when std_logic_vector(to_unsigned(character'pos('1'), 8)) => r := x"1";
			when std_logic_vector(to_unsigned(character'pos('2'), 8)) => r := x"2";
			when std_logic_vector(to_unsigned(character'pos('3'), 8)) => r := x"3";
			when std_logic_vector(to_unsigned(character'pos('4'), 8)) => r := x"4";
			when std_logic_vector(to_unsigned(character'pos('5'), 8)) => r := x"5";
			when std_logic_vector(to_unsigned(character'pos('6'), 8)) => r := x"6";
			when std_logic_vector(to_unsigned(character'pos('7'), 8)) => r := x"7";
			when std_logic_vector(to_unsigned(character'pos('8'), 8)) => r := x"8";
			when std_logic_vector(to_unsigned(character'pos('9'), 8)) => r := x"9";
			when std_logic_vector(to_unsigned(character'pos('a'), 8)) => r := x"a";
			when std_logic_vector(to_unsigned(character'pos('b'), 8)) => r := x"b";
			when std_logic_vector(to_unsigned(character'pos('c'), 8)) => r := x"c";
			when std_logic_vector(to_unsigned(character'pos('d'), 8)) => r := x"d";
			when std_logic_vector(to_unsigned(character'pos('e'), 8)) => r := x"e";
			when std_logic_vector(to_unsigned(character'pos('f'), 8)) => r := x"f";
			when others                                               => r := x"0";
		end case;
		return r;
	end function;

	function nibble2ascii(byte : std_logic_vector(3 downto 0)) return std_logic_vector is
		variable r : std_logic_vector(7 downto 0);
		-- variable error : std_logic ;

	begin
		case byte is
			when x"0"   => r := std_logic_vector(to_unsigned(character'pos('0'), 8));
			when x"1"   => r := std_logic_vector(to_unsigned(character'pos('1'), 8));
			when x"2"   => r := std_logic_vector(to_unsigned(character'pos('2'), 8));
			when x"3"   => r := std_logic_vector(to_unsigned(character'pos('3'), 8));
			when x"4"   => r := std_logic_vector(to_unsigned(character'pos('4'), 8));
			when x"5"   => r := std_logic_vector(to_unsigned(character'pos('5'), 8));
			when x"6"   => r := std_logic_vector(to_unsigned(character'pos('6'), 8));
			when x"7"   => r := std_logic_vector(to_unsigned(character'pos('7'), 8));
			when x"8"   => r := std_logic_vector(to_unsigned(character'pos('8'), 8));
			when x"9"   => r := std_logic_vector(to_unsigned(character'pos('9'), 8));
			when x"a"   => r := std_logic_vector(to_unsigned(character'pos('a'), 8));
			when x"b"   => r := std_logic_vector(to_unsigned(character'pos('b'), 8));
			when x"c"   => r := std_logic_vector(to_unsigned(character'pos('c'), 8));
			when x"d"   => r := std_logic_vector(to_unsigned(character'pos('d'), 8));
			when x"e"   => r := std_logic_vector(to_unsigned(character'pos('e'), 8));
			when x"f"   => r := std_logic_vector(to_unsigned(character'pos('f'), 8));
			when others => r := x"00";
		end case;
		return r;
	end function;
	
	function ascii_is_value(byte : std_logic_vector(7 downto 0)) return boolean is
		variable r : boolean;
	begin
		case byte is
			when std_logic_vector(to_unsigned(character'pos('0'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('1'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('2'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('3'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('4'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('5'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('6'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('7'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('8'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('9'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('a'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('b'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('c'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('d'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('e'), 8)) => r := true;
			when std_logic_vector(to_unsigned(character'pos('f'), 8)) => r := true;
			when others                                               => r := false;
		end case;
		return r;
	end function;

	function val2ascii(value : std_logic_vector) return t_byte_stream is
		variable nibble : std_logic_vector(3 downto 0);
		variable r      : t_byte_stream(value'length / nibble'length - 1 downto 0);
	begin
		for i in r'range loop
			nibble := value(i * nibble'length + nibble'length - 1 downto i * nibble'length);
			r(i)   := nibble2ascii(nibble);
		end loop;
		return r;
	end function;


end uart_pkg;
