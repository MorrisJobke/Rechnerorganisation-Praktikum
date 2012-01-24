-------------------------------------------------------------------------------
-- Title      : cpu4_top_level 
-- Project    : CPU-4 
-------------------------------------------------------------------------------
-- File       : cpu4_top_level.vhd
-- Author     : Sven Lasch, René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2002-02-10
-- Last update: 2010-09-15
-- Platform   : Digilent Nexys 2
-------------------------------------------------------------------------------
-- Description: 4-Bit CPU with clock generator, memory and IO unit   
--              
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY cpu4_top_level IS

  PORT (
    nexys2_clk       : IN  STD_LOGIC;
    nexys2_btn0      : IN  STD_LOGIC;
    nexys2_btn1      : IN  STD_LOGIC;
    nexys2_btn2      : IN  STD_LOGIC;
    nexys2_btn3      : IN  STD_LOGIC;
    nexys2_sw0       : IN  STD_LOGIC;
    nexys2_sw1       : IN  STD_LOGIC;
    nexys2_sw2       : IN  STD_LOGIC;
    nexys2_sw3       : IN  STD_LOGIC;
    nexys2_sw4       : IN  STD_LOGIC;
    nexys2_sw5       : IN  STD_LOGIC;
    nexys2_sw6       : IN  STD_LOGIC;
    nexys2_sw7       : IN  STD_LOGIC;
    nexys2_ld0       : OUT STD_LOGIC;
    nexys2_ld1       : OUT STD_LOGIC;
    nexys2_ld2       : OUT STD_LOGIC;
    nexys2_ld3       : OUT STD_LOGIC;
    nexys2_ld4       : OUT STD_LOGIC;
    nexys2_ld5       : OUT STD_LOGIC;
    nexys2_ld6       : OUT STD_LOGIC;
    nexys2_ld7       : OUT STD_LOGIC;
    nexys2_disp1_ca  : OUT STD_LOGIC;
    nexys2_disp1_cb  : OUT STD_LOGIC;
    nexys2_disp1_cc  : OUT STD_LOGIC;
    nexys2_disp1_cd  : OUT STD_LOGIC;
    nexys2_disp1_ce  : OUT STD_LOGIC;
    nexys2_disp1_cf  : OUT STD_LOGIC;
    nexys2_disp1_cg  : OUT STD_LOGIC;
    nexys2_disp1_dp  : OUT STD_LOGIC;
    nexys2_disp1_an0 : OUT STD_LOGIC;
    nexys2_disp1_an1 : OUT STD_LOGIC;
    nexys2_disp1_an2 : OUT STD_LOGIC;
    nexys2_disp1_an3 : OUT STD_LOGIC);
    
END cpu4_top_level;

ARCHITECTURE A_structure OF cpu4_top_level IS

  SIGNAL clk        : STD_LOGIC; -- external clock
  SIGNAL button     : STD_LOGIC; -- button 0
  SIGNAL reset      : STD_LOGIC; -- button 1
  SIGNAL preset_en  : STD_LOGIC; -- button 2
  SIGNAL outsel     : STD_LOGIC; -- button 3
  SIGNAL step       : STD_LOGIC;
  SIGNAL we         : STD_LOGIC;
  SIGNAL rd         : STD_LOGIC;
  SIGNAL wr         : STD_LOGIC;
  SIGNAL mreq       : STD_LOGIC;
  SIGNAL ioreq      : STD_LOGIC;
  SIGNAL addr       : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL data       : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL data_cpu   : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL data_mem   : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL data_io    : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL ip         : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL ar         : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL br         : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL aa         : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL bb         : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL carry_flag : STD_LOGIC;
  SIGNAL zero_flag  : STD_LOGIC;
  SIGNAL preset_io  : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL io_reg2    : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL io_reg3    : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL segs       : STD_LOGIC_VECTOR(7 DOWNTO 0); -- segment vector
  SIGNAL ans        : STD_LOGIC_VECTOR(3 DOWNTO 0); -- anode vector
  SIGNAL state      : STD_LOGIC_VECTOR(5 DOWNTO 0);

  COMPONENT single_step
    PORT (
      clk    : IN  STD_LOGIC;
      reset  : IN  STD_LOGIC;
      button : IN  STD_LOGIC;
      step   : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT cpu4
    PORT (
      clk       : IN  STD_LOGIC;
      reset     : IN  STD_LOGIC;
      step      : IN  STD_LOGIC;
      rd        : OUT STD_LOGIC;
      wr        : OUT STD_LOGIC;
      mreq      : OUT STD_LOGIC;
      ioreq     : OUT STD_LOGIC;
      addr      : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      state     : OUT STD_LOGIC_VECTOR(5 DOWNTO 0);
      data_in   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      data_out  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      ip_out    : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      ar_out    : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      br_out    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      a_out     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      b_out     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      carry_out : OUT STD_LOGIC;
      zero_out  : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT io_unit
    PORT (
      clk         : IN  STD_LOGIC;
      reset       : IN  STD_LOGIC;
      step        : IN  STD_LOGIC;
      wr          : IN  STD_LOGIC;
      ioreq       : IN  STD_LOGIC;
      addr        : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_in     : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      preset      : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      preset_en   : IN  STD_LOGIC;
      reg2_out    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      reg3_out    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      data_out    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;
  
  COMPONENT memory
    PORT ( 
      clk      : IN  STD_LOGIC;
      we       : IN  STD_LOGIC;
      address  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      data_in  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;
  
  COMPONENT ssd_cpu_4bit
    PORT (
      clk    : IN  STD_LOGIC;
      rd     : IN  STD_LOGIC;
      wr     : IN  STD_LOGIC;
      carry  : IN  STD_LOGIC;
      zero   : IN  STD_LOGIC;
      mreq   : IN  STD_LOGIC;
      ioreq  : IN  STD_LOGIC;
      ar_in  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      a_in   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      b_in   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);    
      state  : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);
      ip_in  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      outsel : IN  STD_LOGIC;
      segs   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      ans    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;  
  
BEGIN

  -- External clock
  clk <= nexys2_clk;
  
  -- Button for manual 'clock'
  button <= nexys2_btn0;
  
  -- Button for reset
  reset <= nexys2_btn1;
  
  -- Button for IO preset
  preset_en <= nexys2_btn2;
  
  -- Button for output select
  outsel <= nexys2_btn3;

  -- Switches for IO configuration
  preset_io(0) <= nexys2_sw0;
  preset_io(1) <= nexys2_sw1;
  preset_io(2) <= nexys2_sw2;
  preset_io(3) <= nexys2_sw3;
  preset_io(4) <= nexys2_sw4;
  preset_io(5) <= nexys2_sw5;
  preset_io(6) <= nexys2_sw6;
  preset_io(7) <= nexys2_sw7;
  
  -- LEDs for memory content display
  -- mux1: 
  nexys2_ld0 <= io_reg2(0) WHEN outsel = '1' ELSE br(0);
  nexys2_ld1 <= io_reg2(1) WHEN outsel = '1' ELSE br(1);
  nexys2_ld2 <= io_reg2(2) WHEN outsel = '1' ELSE br(2);
  nexys2_ld3 <= io_reg2(3) WHEN outsel = '1' ELSE br(3);
  nexys2_ld4 <= io_reg3(0) WHEN outsel = '1' ELSE data(0);
  nexys2_ld5 <= io_reg3(1) WHEN outsel = '1' ELSE data(1);
  nexys2_ld6 <= io_reg3(2) WHEN outsel = '1' ELSE data(2);
  nexys2_ld7 <= io_reg3(3) WHEN outsel = '1' ELSE data(3);
  
  -- Segment vectors for display (low active on Nexys 2)
  nexys2_disp1_ca <= NOT(segs(0));
  nexys2_disp1_cb <= NOT(segs(1));
  nexys2_disp1_cc <= NOT(segs(2));
  nexys2_disp1_cd <= NOT(segs(3));
  nexys2_disp1_ce <= NOT(segs(4));
  nexys2_disp1_cf <= NOT(segs(5));
  nexys2_disp1_cg <= NOT(segs(6));
  nexys2_disp1_dp <= NOT(segs(7));
  
  -- Anode vectors for display (low active on Nexys 2)
  nexys2_disp1_an0 <= NOT(ans(0));
  nexys2_disp1_an1 <= NOT(ans(1));
  nexys2_disp1_an2 <= NOT(ans(2));
  nexys2_disp1_an3 <= NOT(ans(3));
  
  -- Write-enable only when step = '1', i.e.
  -- input data is set at the end of the write command
  we <= wr AND mreq AND step;
  
  -- Select data for CPU input
  -- mux2:
  data <= data_io WHEN ioreq = '1' ELSE data_mem;

  I_SINGLE_STEP : single_step
    PORT MAP (
      clk    => clk,
      reset  => reset,
      button => button,
      step   => step);

  I_CPU4 : cpu4
    PORT MAP (
      clk       => clk,
      reset     => reset,
      step      => step,
      data_in   => data,
      rd        => rd,
      wr        => wr,
      mreq      => mreq,
      ioreq     => ioreq,
      addr      => addr,
      state     => state,
      data_out  => data_cpu,
      ip_out    => ip,
      ar_out    => ar,
      br_out    => br,
      a_out     => aa,
      b_out     => bb,
      carry_out => carry_flag,
      zero_out  => zero_flag);

  I_IO_UNIT : io_unit
    PORT MAP (
      clk       => clk,
      reset     => reset,
      step      => step,
      wr        => wr,
      ioreq     => ioreq,
      addr      => addr(1 DOWNTO 0),
      data_in   => data_cpu,
      preset    => preset_io,
      preset_en => preset_en,
      reg2_out  => io_reg2,
      reg3_out  => io_reg3,
      data_out  => data_io);

  -- Memory 256 x 4 bit
  I_MEMORY : memory
    PORT MAP (
      clk      => clk,
      we       => we,
      address  => addr,
      data_in  => data_cpu,
      data_out => data_mem);

  I_ssd : ssd_cpu_4bit
    PORT MAP (
      clk    => clk,
      rd     => rd,
      wr     => wr,
      carry  => carry_flag,
      zero   => zero_flag,
      mreq   => mreq,
      ioreq  => ioreq,
      ar_in  => ar,
      a_in   => aa,
      b_in   => bb, 
      state  => state,
      ip_in  => ip,
      outsel => outsel,
      segs   => segs,
      ans    => ans);

END A_structure;
