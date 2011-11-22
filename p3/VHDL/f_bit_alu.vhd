----------------------------------------------------------------------------------
-- Company: Chemnitz University of Technology
-- Engineer: oere
-- 
-- Create Date:    12:53:42 09/06/2010 
-- Design Name: 
-- Module Name:    f_bit_alu - a 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: Behavioral modeling of an alu.
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity f_bit_alu is
    Port ( a : in  STD_LOGIC_VECTOR (3 downto 0);
           b : in  STD_LOGIC_VECTOR (3 downto 0);
           cin : in  STD_LOGIC;
           fn_select : in  STD_LOGIC_VECTOR (2 downto 0);
           s : out  STD_LOGIC_VECTOR (3 downto 0);
           cout : out  STD_LOGIC);
end f_bit_alu;

architecture a of f_bit_alu is

constant ALU_ADD: std_logic_vector(2 downto 0) := "000";
constant ALU_AND: std_logic_vector(2 downto 0) := "001";
constant ALU_SLL: std_logic_vector(2 downto 0) := "010";

signal temp_s, temp_a, temp_b, temp_cin: std_logic_vector(4 downto 0);

begin

	temp_a <= '0' & a;	-- Erweitern auf 5 Bit und 5. Bit auf 0 setzen
	temp_b <= '0' & b;   -- (z.B. aus "1011" wird "01011")
	temp_cin <= "0000" & cin;
	
p0:	process(temp_a, temp_b, temp_cin, fn_select)
		begin
			case fn_select is
				when ALU_ADD =>
					temp_s <= temp_a + temp_b + temp_cin;
				when ALU_AND =>
					temp_s <= temp_a and temp_b;
				when ALU_SLL =>
					temp_s(4) <= temp_a(3);
					temp_s(3) <= temp_a(2);
					temp_s(2) <= temp_a(1);
					temp_s(1) <= temp_a(0);
					temp_s(0) <= '0';
				when others =>
					temp_s <= "00000";
			end case;
		end process p0;
		
cout <= temp_s(4);
s <= temp_s(3 downto 0);
end a;
