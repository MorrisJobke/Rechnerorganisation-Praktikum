-------------------------------------------------------------------------------
-- Title      : mctable
-- Project    : CPU-4
-------------------------------------------------------------------------------
-- File       : mctable.vhd
-- Author     : René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2010-09-14
-- Last update: 2010-09-15
-- Platform   : Xilinx Spartan-3E
-------------------------------------------------------------------------------
-- Description: - Memory 16x6 bit for the microcode table of the 4-bit CPU
-------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY mctable IS

  PORT (
    address  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    data_out : OUT STD_LOGIC_VECTOR(5 DOWNTO 0));
    
END mctable;

ARCHITECTURE A_behav OF mctable IS

  TYPE rom_type IS ARRAY(0 TO 15) OF STD_LOGIC_VECTOR(5 DOWNTO 0);
  CONSTANT rom : rom_type := (
    -----------------------------------
    --         Code  % Address: Command
    B"000000", --0-- % - unused -     %
    B"000000", --1-- % - unused -     %
    B"000010", --2-- % 02: CCF        %
    B"000010", --3-- % 02: SCF        %
    B"000100", --4-- % 04: JPZ MEM    %
    B"000100", --5-- % 04: JPC MEM    %
    B"000000", --6-- % - unused -     %
    B"000000", --7-- % - unused -     %
    B"001110", --8-- % 0E: LD (MEM)   %
    B"100010", --9-- % 22: - unused - %
    B"011000", --A-- % 18: STO (MEM)  %
    B"101100", --B-- % 2C: - unused - %
    B"000011", --C-- % 03: ADC        %
    B"000011", --D-- % 03: NOT        %
    B"000000", --E-- % - unused -     %
    B"000011"  --F-- % 03: RRC        %
    -----------------------------------
  );

BEGIN

  data_out <= rom(CONV_INTEGER(address));

END A_behav;
