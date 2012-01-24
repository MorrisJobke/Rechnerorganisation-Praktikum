-------------------------------------------------------------------------------
-- Title      : ssd_cpu_4bit
-- Project    : CPU-4
-------------------------------------------------------------------------------
-- File       : ssd_cpu_4bit.vhd
-- Author     : René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2010-09-14
-- Last update: 2010-09-14
-- Platform   : Digilent Nexys 2
-------------------------------------------------------------------------------
-- Description: Seven segment customized for our 4-bit CPU 
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY ssd_cpu_4bit IS

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
  
END ssd_cpu_4bit;

ARCHITECTURE A_behav OF ssd_cpu_4bit IS

  SIGNAL cnt       : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL input_int : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL input_an0 : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL input_an1 : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL input_an2 : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL input_an3 : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL input_dp0 : STD_LOGIC;
  SIGNAL input_dp1 : STD_LOGIC;
  SIGNAL input_dp2 : STD_LOGIC;
  SIGNAL input_dp3 : STD_LOGIC;
  SIGNAL dp_int    : STD_LOGIC;
  SIGNAL segs_int  : STD_LOGIC_VECTOR (6 DOWNTO 0);

BEGIN
  
  -- AN0: B or lower STATE
  input_an0 <= b_in WHEN outsel = '0' ELSE state(3 DOWNTO 0);
  -- DP0: WR 
  input_dp0 <= wr WHEN outsel = '0' ELSE '0';

  -- AN1: A or upper STATE             
  input_an1 <= a_in WHEN outsel = '0' ELSE "00" & state(5 DOWNTO 4);
  -- DP1: RD 
  input_dp1 <= rd WHEN outsel = '0' ELSE '0';

  -- AN2: Lower AR or lower IP                
  input_an2 <= ar_in(3 DOWNTO 0) WHEN outsel = '0' ELSE ip_in(3 DOWNTO 0);
  -- DP2: ZERO or IOREQ 
  input_dp2 <= zero WHEN outsel = '0' ELSE ioreq;

  -- AN3: Upper AR or upper IP               
  input_an3 <= ar_in(7 DOWNTO 4) WHEN outsel = '0' ELSE ip_in(7 DOWNTO 4);
  -- DP3: CARRY or MREQ  
  input_dp3 <= carry WHEN outsel = '0' ELSE mreq;               
               
  -- input_int is changed with cnt below
  WITH input_int(3 DOWNTO 0) SELECT segs_int <=  -- 4 bit binary to seven segment hexadecimal    
    "0111111" WHEN "0000", -- 0
    "0000110" WHEN "0001", -- 1
    "1011011" WHEN "0010", -- 2
    "1001111" WHEN "0011", -- 3
    "1100110" WHEN "0100", -- 4
    "1101101" WHEN "0101", -- 5
    "1111101" WHEN "0110", -- 6
    "0000111" WHEN "0111", -- 7
    "1111111" WHEN "1000", -- 8
    "1101111" WHEN "1001", -- 9
    "1110111" WHEN "1010", -- A
    "1111100" WHEN "1011", -- B
    "0111001" WHEN "1100", -- C
    "1011110" WHEN "1101", -- D
    "1111001" WHEN "1110", -- E
    "1110001" WHEN "1111", -- F
    "0000000" WHEN OTHERS; -- NONE
  
  P_cnt : PROCESS (clk)
  BEGIN
	 IF clk'event AND clk = '1' THEN
      cnt <= cnt + 1;
    END IF;
  END PROCESS;  
  
  -- cnt(16 downto 15) alters with 50 MHz / 2^15 ~= 1526Hz 
  WITH cnt(16 DOWNTO 15) SELECT input_int <= -- which input
    input_an0 WHEN "00",
    input_an1 WHEN "01",
    input_an2 WHEN "10",
    input_an3 WHEN "11",
    "1111" WHEN OTHERS;   -- all segments
    
  WITH cnt(16 DOWNTO 15) SELECT dp_int <= -- which decimal point
    input_dp0 WHEN "00",
    input_dp1 WHEN "01",
    input_dp2 WHEN "10",
    input_dp3 WHEN "11",
    '0' WHEN OTHERS;      -- DP off
    
  WITH cnt(16 DOWNTO 15) SELECT ans <= -- which anode
	 "0001" WHEN "00",   -- AN0
	 "0010" WHEN "01",   -- AN1
	 "0100" WHEN "10",   -- AN2
	 "1000" WHEN "11",   -- AN3
	 "0001" WHEN OTHERS; -- AN0
    
  segs <= dp_int & segs_int; 
   
END A_behav;
