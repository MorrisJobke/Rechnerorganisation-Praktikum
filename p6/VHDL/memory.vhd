-------------------------------------------------------------------------------
-- Title      : memory
-- Project    : CPU-4
-------------------------------------------------------------------------------
-- File       : memory.vhd
-- Author     : René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2010-09-14
-- Last update: 2010-09-15
-- Platform   : Xilinx Spartan-3E
-------------------------------------------------------------------------------
-- Description: - Memory 256x4 bit using Spartan-3E Block RAM (4Kx4)
-------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY UNISIM;
USE UNISIM.VComponents.ALL;

ENTITY memory IS

  PORT ( 
    clk      : IN  STD_LOGIC;
    we       : IN  STD_LOGIC;
    address  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
    data_in  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    data_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
     
END memory;

ARCHITECTURE A_behav OF memory IS

  SIGNAL addr : STD_LOGIC_VECTOR(11 DOWNTO 0);

BEGIN

  addr <= "0000" & address;

  -- RAMB16_S4: 4k x 4 Single-Port RAM
  --            Spartan-3E
  -- Xilinx HDL Language Template, version 12.2

  RAMB16_S4_inst : RAMB16_S4
  generic map (
    INIT => X"0", --  Value of output RAM registers at startup
    SRVAL => X"0", --  Ouput value upon SSR assertion
    WRITE_MODE => "WRITE_FIRST", --  WRITE_FIRST, READ_FIRST or NO_CHANGE
    -- The following INIT_xx declarations specify the initial contents of the RAM
    -- INIT_00: 255 to 0
    --                          3               2               1               0  
    --           FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210FEDCBA9876543210
    INIT_00 => X"0000000000000000000000000000000000000000000000000000FFAFEAFD8FC8")   
    -- Progam: 
    -- 00: 8 C F -- LD $FC (IO_0)
    -- 03: 8 D F -- LD $FD (IO_1)
    -- 06: A E F -- STO $FE (IO_2)
    -- 09: A F F -- STO $FF (IO_3)
    --
  port map (
    DO => data_out, -- 4-bit Data Output
    ADDR => addr,   -- 12-bit Address Input
    CLK => clk,     -- Clock
    DI => data_in,  -- 4-bit Data Input
    EN => '1',      -- RAM Enable Input
    SSR => '0',     -- Synchronous Set/Reset Input
    WE => we        -- Write Enable Input
  );

  -- End of RAMB16_S4_inst instantiation

END A_behav;
