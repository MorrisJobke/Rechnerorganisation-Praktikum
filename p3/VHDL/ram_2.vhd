----------------------------------------------------------------------------------
-- Company: Chemnitz University of Technology
-- Engineer: oere
-- 
-- Create Date:    14:36:40 09/06/2010 
-- Design Name: 
-- Module Name:    ram_2 - b_behav 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: Block RAM implementation of 8 x 8 bits RAM.
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

library UNISIM;
use UNISIM.VComponents.all;

entity ram_2 is
    Port ( clk : in  STD_LOGIC;
           we : in  STD_LOGIC;
           address : in  STD_LOGIC_VECTOR (3 downto 0);
           write_data : in  STD_LOGIC_VECTOR (7 downto 0);
           read_data : out  STD_LOGIC_VECTOR (7 downto 0));
end ram_2;

architecture b_behav of ram_2 is

signal addr: std_logic_vector(10 downto 0);
signal dop: std_logic_vector(0 downto 0); -- Parity output ist ein Vektor
                                          -- Warnung, weil nicht weiter verwendet
begin	
	addr <= "0000000" & address; -- Erweiterung auf 11-bit

   -- RAMB16_S9: 2k x 8 + 1 Parity bit Single-Port RAM
   --            Spartan-3E
   -- Xilinx HDL Language Template, version 12.2

   RAMB16_S9_inst : RAMB16_S9
   generic map (
      INIT => X"000", --  Value of output RAM registers at startup
      SRVAL => X"000", --  Ouput value upon SSR assertion
      WRITE_MODE => "WRITE_FIRST" --  WRITE_FIRST, READ_FIRST or NO_CHANGE
		-- Zeilen
		-- INIT_00 bis INIT_3F, sowie 
		-- INITP_00 bis INITP_3F entfernt, weil default '0'
   )
   port map (
      DO => read_data,      -- 8-bit Data Output
      DOP => dop,           -- 1-bit parity Output
      ADDR => addr,         -- 11-bit Address Input
      CLK => clk,           -- Clock
      DI => write_data,     -- 8-bit Data Input
      DIP => "0",           -- 1-bit parity Input
      EN => '1',            -- RAM Enable Input
      SSR => '0',           -- Synchronous Set/Reset Input
      WE => we              -- Write Enable Input
   );

   -- End of RAMB16_S9_inst instantiation
end b_behav;
