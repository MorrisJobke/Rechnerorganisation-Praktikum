-------------------------------------------------------------------------------
-- Title      : io_unit 
-- Project    : CPU-4
-------------------------------------------------------------------------------
-- File       : io_unit.vhd
-- Author     : Sven Lasch, René Oertel
-- Company    : TU-Chemnitz
-- Created    : 2002-02-10
-- Last update: 2010-09-15
-- Platform   : Digilent Nexys 2
-------------------------------------------------------------------------------
-- Description: IO unit for 4-bit CPU
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY io_unit IS

  PORT (
    clk       : IN  STD_LOGIC;
    reset     : IN  STD_LOGIC;
    step      : IN  STD_LOGIC;
    wr        : IN  STD_LOGIC;
    ioreq     : IN  STD_LOGIC;
    addr      : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_in   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    preset    : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
    preset_en : IN  STD_LOGIC;
    reg2_out  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    reg3_out  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    data_out  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
    
END io_unit;

ARCHITECTURE A_behav OF io_unit IS

BEGIN

  -- Insert modifications for IO unit here!
  
END A_behav;
