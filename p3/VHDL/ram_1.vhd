----------------------------------------------------------------------------------
-- Company: Chemnitz University of Technology
-- Engineer: oere
-- 
-- Create Date:    13:09:13 09/06/2010 
-- Design Name: 
-- Module Name:    ram_1 - a_behav 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: Flipflop implementation of a RAM.
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
use IEEE.STD_LOGIC_ARITH.ALL;

entity ram_1 is
    Port ( clk : in  STD_LOGIC;
           we : in  STD_LOGIC;
           address : in  STD_LOGIC_VECTOR (2 downto 0);
           write_data : in  STD_LOGIC_VECTOR (7 downto 0);
           read_data : out  STD_LOGIC_VECTOR (7 downto 0));
end ram_1;

architecture a_behav of ram_1 is

		-- Einen entsprechenden Typ fuer den Speicher anlegen
		type memory_t is array(0 to 7) of std_logic_vector(7 downto 0);
		-- Ein Signal mit diesem Typ anlegen (der eigentliche Speicher)
		signal memory: memory_t;

begin
	-- Auslesen des RAMs
	-- read_data folgt address ohne jeglichen Takt
	-- --> Lesen ist asynchron
	read_data <= memory(CONV_INTEGER(address));
	
p0:	process(clk)
	begin
		if clk'event and clk = '1' then
			-- geschrieben wird mit der steigenden Taktflanke
			-- --> Schreiben ist synchron
			if we = '1' then
				memory(CONV_INTEGER(address)) <= write_data;
			end if;
		end if;
	end process p0;
end a_behav;
