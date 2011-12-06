-------------------------------------------------------------------------------
-- Title      : cpu 2bit top-level
-- Project    : cpu 2bit
-------------------------------------------------------------------------------
-- File       : cpu_2bit_top_level.vhd
-- Author     : Sven Lasch, René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2001-11-08
-- Last update: 2010-09-09
-- Platform   : ANY
-------------------------------------------------------------------------------
-- Description: default (exercise)
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author      Description
-- 2001-11-08  1.0      Sven Lasch  Created
-- 2010-09-09  2.0      René Oertel Modified for Digilent Nexys 2
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Entity ip_counter
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY ip_counter IS

  PORT (
    clk    : IN  STD_LOGIC;
    reset  : IN  STD_LOGIC;
    step   : IN  STD_LOGIC;
    ip_ld  : IN  STD_LOGIC;
    ip_cnt : IN  STD_LOGIC;
    ip_in  : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    ip_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));

END ip_counter;

ARCHITECTURE A_behav OF ip_counter IS

  --
  -- Hier sind von Ihnen Ergänzungen zu machen.
  --

END A_behav;

-------------------------------------------------------------------------------
-- Entity register_2bit
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY register_2bit IS

  PORT (
    clk     : IN  STD_LOGIC;
    reset   : IN  STD_LOGIC;
    step    : IN  STD_LOGIC;
    reg_en  : IN  STD_LOGIC;
    reg_in  : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    reg_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));

END register_2bit;

ARCHITECTURE A_behav OF register_2bit IS

  --
  -- Hier sind von Ihnen Ergänzungen zu machen.
  --

END A_behav;

-------------------------------------------------------------------------------
-- Entity mux
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY mux IS

  PORT (
    sel : IN  STD_LOGIC;
    a   : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    b   : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    y   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));

END mux;

ARCHITECTURE A_behav OF mux IS

  --
  -- Hier sind von Ihnen Ergänzungen zu machen.
  --

END A_behav;

-------------------------------------------------------------------------------
-- Entity alu
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY alu IS

  PORT (
    a : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    b : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    y : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));

END alu;

ARCHITECTURE A_behav OF alu IS

  --
  -- Hier sind von Ihnen Ergänzungen zu machen.
  --

END A_behav;

-------------------------------------------------------------------------------
-- Entity steuerwerk
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY steuerwerk IS

  PORT (
    clk    : IN  STD_LOGIC;
    reset  : IN  STD_LOGIC;
    step   : IN  STD_LOGIC;
    br     : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    state  : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    br_en  : OUT STD_LOGIC;
    ar_en  : OUT STD_LOGIC;
    aa_en  : OUT STD_LOGIC;
    ip_ld  : OUT STD_LOGIC;
    ip_cnt : OUT STD_LOGIC;
    rd     : OUT STD_LOGIC;
    wr     : OUT STD_LOGIC;
    m1_s   : OUT STD_LOGIC;
    m2_s   : OUT STD_LOGIC;
    m3_s   : OUT STD_LOGIC;
    m4_s   : OUT STD_LOGIC);

END steuerwerk;

ARCHITECTURE A_behav OF steuerwerk IS

  --
  -- Hier sind von Ihnen Ergänzungen zu machen.
  --

END A_behav;

-------------------------------------------------------------------------------
-- CPU-2 Top Level
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY cpu_2bit IS

  PORT (
    clk      : IN  STD_LOGIC;
    reset    : IN  STD_LOGIC;
    step     : IN  STD_LOGIC;
    rd       : OUT STD_LOGIC;
    wr       : OUT STD_LOGIC;
    addr     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    state    : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    ar_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    br_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    ip_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    aa_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    bb_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_in  : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));

END cpu_2bit;

ARCHITECTURE A_structure OF cpu_2bit IS

  --
  -- Hier sind von Ihnen Ergänzungen zu machen.
  --

END A_structure;

-------------------------------------------------------------------------------
-- Hier folgen die zum Test der CPU notwendigen Module. Diese sind bereits
-- vollständig implementiert und sollten daher nicht verändert werden.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- memory
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY memory IS

  PORT (
    clk          : IN  STD_LOGIC;
    step         : IN  STD_LOGIC;
    addr         : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_in      : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_out     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    preset_data  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
    data_display : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    preset       : IN  STD_LOGIC;
    rd           : IN  STD_LOGIC;
    wr           : IN  STD_LOGIC);

END memory;

ARCHITECTURE A_behav OF memory IS

  TYPE mem_type IS ARRAY (0 TO 3) OF STD_LOGIC_VECTOR(1 DOWNTO 0);

  SIGNAL memory_array : mem_type;

BEGIN

  P_memory : PROCESS (clk, preset, preset_data)
  BEGIN
    IF preset = '1' THEN
      memory_array(0)                    <= preset_data(1 DOWNTO 0);
      memory_array(1)                    <= preset_data(3 DOWNTO 2);
      memory_array(2)                    <= preset_data(5 DOWNTO 4);
      memory_array(3)                    <= preset_data(7 DOWNTO 6);
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF wr = '1' THEN
          memory_array(conv_integer(addr)) <= data_in;
        END IF;
      ELSIF rd = '1' THEN
          data_out <= memory_array(conv_integer(addr));
      END IF;
    END IF;
  END PROCESS P_memory;

  data_display(1 DOWNTO 0) <= memory_array(0);
  data_display(3 DOWNTO 2) <= memory_array(1);
  data_display(5 DOWNTO 4) <= memory_array(2);
  data_display(7 DOWNTO 6) <= memory_array(3);

END A_behav;

-------------------------------------------------------------------------------
-- single step state maschine
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY single_step IS

  PORT (
    clk    : IN  STD_LOGIC;
    reset  : IN  STD_LOGIC;
    button : IN  STD_LOGIC;
    step   : OUT STD_LOGIC);

END single_step;

ARCHITECTURE A_behav OF single_step IS

  CONSTANT DELAY : NATURAL RANGE 0 TO 131071 := 131071;

  TYPE states IS (high, low, step_out, cnt_reset);
  SIGNAL current_state : states;
  SIGNAL next_state    : states;
  SIGNAL counter       : INTEGER RANGE 0 TO 131071;
  SIGNAL timeout       : STD_LOGIC;
  SIGNAL cnt_clear     : STD_LOGIC;
  SIGNAL cnt_count     : STD_LOGIC;

BEGIN

  P_step_state : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      current_state <= high;
    ELSIF clk'EVENT AND clk = '1' THEN
      current_state <= next_state;
    END IF;
  END PROCESS P_step_state;

  P_step_trans_out : PROCESS (button, current_state, timeout)
  BEGIN
    CASE current_state IS
      WHEN high      =>
        IF timeout = '1' AND button = '1' THEN
          next_state <= cnt_reset;
        ELSE
          next_state <= high;
        END IF;
      WHEN low       =>
        IF timeout = '1' AND button = '0' THEN
          next_state <= step_out;
        ELSE
          next_state <= low;
        END IF;
      WHEN step_out  =>
        next_state   <= high;
      WHEN cnt_reset =>
        next_state   <= low;
      WHEN OTHERS    => NULL;
    END CASE;
  END PROCESS P_step_trans_out;

  step      <= '1' WHEN current_state = step_out                                  ELSE '0';
  cnt_clear <= '1' WHEN (current_state = step_out) OR (current_state = cnt_reset) ELSE '0';
  cnt_count <= '1' WHEN (current_state = high) OR (current_state = low)           ELSE '0';

  P_timeout : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      counter     <= 0;
    ELSIF clk'EVENT AND clk = '1' THEN
      IF cnt_clear = '1' THEN
        counter   <= 0;
      ELSIF cnt_count = '1' THEN
        IF counter < delay THEN
          counter <= counter + 1;
        END IF;
      END IF;
    END IF;
  END PROCESS P_timeout;

  timeout <= '1' WHEN counter = DELAY ELSE '0';

END A_behav;

-------------------------------------------------------------------------------
-- ssd_cpu_2bit
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY ssd_cpu_2bit IS

  PORT (
    clk   : IN  STD_LOGIC;
    rd    : IN  STD_LOGIC;
    wr    : IN  STD_LOGIC;
    ar_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    br_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    aa_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    bb_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);    
    state : IN  STD_LOGIC_VECTOR(2 DOWNTO 0);
    ip_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    segs  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    ans   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  
END ssd_cpu_2bit;

ARCHITECTURE A_behav OF ssd_cpu_2bit IS

  SIGNAL cnt      : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL segs_an0 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL segs_an1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL segs_an2 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL segs_an3 : STD_LOGIC_VECTOR (7 DOWNTO 0);

BEGIN
    
  WITH ip_in SELECT segs_an0 <=  -- 2 bit binary to seven segment hexadecimal
    wr & "0111111" WHEN "00", -- 0
    wr & "0000110" WHEN "01", -- 1
    wr & "1011011" WHEN "10", -- 2
    wr & "1001111" WHEN "11", -- 3
    wr & "0000000" WHEN OTHERS; -- none
    
  WITH state SELECT segs_an1 <= -- 2 bit binary to seven segment to bar
    rd & "0001000" WHEN "001", -- F1 -> segment cd (_)
    rd & "1000000" WHEN "010", -- F2 -> segment cg (-)
    rd & "0000001" WHEN "100", -- F3 -> segment ca (^-)
    rd & "0000000" WHEN OTHERS;

  -- segments dp,cg  cf         ce         cd    cc         cb         ca  
  segs_an2 <= "00" & ar_in(1) & bb_in(1) & '0' & bb_in(0) & ar_in(0) & '0';
  segs_an3 <= "00" & br_in(1) & aa_in(1) & '0' & aa_in(0) & br_in(0) & '0';
  
  P_cnt : PROCESS (clk)
  BEGIN
	 IF clk'event AND clk = '1' THEN
      cnt <= cnt + 1;
    END IF;
  END PROCESS;  
  
  -- cnt(16 downto 15) alters with 50 MHz / 2^15 ~= 1526Hz 
  WITH cnt(16 DOWNTO 15) SELECT ans <= -- which anode
	 "0001" WHEN "00",   -- AN0
	 "0010" WHEN "01",   -- AN1
	 "0100" WHEN "10",   -- AN2
	 "1000" WHEN "11",   -- AN3
	 "0001" WHEN OTHERS; -- AN0
    
  WITH cnt(16 DOWNTO 15) SELECT segs <= -- which segment vector
    segs_an0 WHEN "00",     -- ip, wr
    segs_an1 WHEN "01",     -- state, rd
    segs_an2 WHEN "10",     -- ar, bb
    segs_an3 WHEN "11",     -- br, aa
    "11111111" WHEN OTHERS; -- all segments
   
END A_behav;

-------------------------------------------------------------------------------
-- top_level_entity
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY cpu_2bit_top_level IS

  PORT (
    nexys2_clk       : IN  STD_LOGIC;
    nexys2_btn1      : IN  STD_LOGIC;
    nexys2_btn0      : IN  STD_LOGIC;
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

END cpu_2bit_top_level;

ARCHITECTURE A_structure OF cpu_2bit_top_level IS

  SIGNAL preset_data  : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL addr         : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL data_in      : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL data_out     : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL rd           : STD_LOGIC;
  SIGNAL wr           : STD_LOGIC;
  SIGNAL step         : STD_LOGIC;
  SIGNAL state        : STD_LOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL ar_out       : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL br_out       : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL ip_out       : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL aa_out       : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL bb_out       : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL data_display : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL clk          : STD_LOGIC; -- external clock
  SIGNAL reset        : STD_LOGIC; -- button 1
  SIGNAL button       : STD_LOGIC; -- button 0
  SIGNAL segs         : STD_LOGIC_VECTOR(7 DOWNTO 0); -- segment vector
  SIGNAL ans          : STD_LOGIC_VECTOR(3 DOWNTO 0); -- anode vector

  COMPONENT single_step
    PORT (
      clk    : IN  STD_LOGIC;
      reset  : IN  STD_LOGIC;
      button : IN  STD_LOGIC;
      step   : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT cpu_2bit
    PORT (
      clk      : IN  STD_LOGIC;
      reset    : IN  STD_LOGIC;
      step     : IN  STD_LOGIC;
      rd       : OUT STD_LOGIC;
      wr       : OUT STD_LOGIC;
      addr     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      state    : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
      ar_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      br_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      ip_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      aa_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      bb_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_in  : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));
  END COMPONENT;

  COMPONENT memory
    PORT (
      clk          : IN  STD_LOGIC;
      step         : IN  STD_LOGIC;
      addr         : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_in      : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_out     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      preset_data  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      data_display : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      preset       : IN  STD_LOGIC;
      rd           : IN  STD_LOGIC;
      wr           : IN  STD_LOGIC);
  END COMPONENT;
  
  COMPONENT ssd_cpu_2bit
    PORT (
      clk   : IN STD_LOGIC;
      rd    : IN STD_LOGIC;
      wr    : IN STD_LOGIC;
      ar_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      br_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      aa_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      bb_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);    
      state : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
      ip_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      segs  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      ans   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;    

BEGIN

  -- External clock
  clk <= nexys2_clk;
  
  -- Button for reset
  reset <= nexys2_btn1;
  
  -- Button for manual 'clock'
  button <= nexys2_btn0;
  
  -- Switches for memory configuration
  preset_data(1) <= nexys2_sw1;
  preset_data(0) <= nexys2_sw0;
  preset_data(3) <= nexys2_sw3;
  preset_data(2) <= nexys2_sw2;
  preset_data(5) <= nexys2_sw5;
  preset_data(4) <= nexys2_sw4;
  preset_data(7) <= nexys2_sw7;
  preset_data(6) <= nexys2_sw6;
  
  -- LEDs for memory content display
  nexys2_ld1 <= data_display(1);
  nexys2_ld0 <= data_display(0);
  nexys2_ld3 <= data_display(3);
  nexys2_ld2 <= data_display(2);
  nexys2_ld5 <= data_display(5);
  nexys2_ld4 <= data_display(4);
  nexys2_ld7 <= data_display(7);
  nexys2_ld6 <= data_display(6);

  -- Segment vectors for display (low active)
  nexys2_disp1_ca <= NOT(segs(0));
  nexys2_disp1_cb <= NOT(segs(1));
  nexys2_disp1_cc <= NOT(segs(2));
  nexys2_disp1_cd <= NOT(segs(3));
  nexys2_disp1_ce <= NOT(segs(4));
  nexys2_disp1_cf <= NOT(segs(5));
  nexys2_disp1_cg <= NOT(segs(6));
  nexys2_disp1_dp <= NOT(segs(7));
  
  -- Anode vectors for display (low active)
  nexys2_disp1_an0 <= NOT(ans(0));
  nexys2_disp1_an1 <= NOT(ans(1));
  nexys2_disp1_an2 <= NOT(ans(2));
  nexys2_disp1_an3 <= NOT(ans(3));
  
  I_single_step : single_step
    PORT MAP (
      clk    => clk,
      reset  => reset,
      button => button,
      step   => step);

  I_cpu : cpu_2bit
    PORT MAP (
      clk      => clk,
      reset    => reset,
      step     => step,
      rd       => rd,
      wr       => wr,
      addr     => addr,
      state    => state,
      ar_out   => ar_out,
      br_out   => br_out,
      ip_out   => ip_out,
      aa_out   => aa_out,
      bb_out   => bb_out,
      data_in  => data_in,
      data_out => data_out);
      
  I_memory : memory
    PORT MAP (
      clk          => clk,
      step         => step,
      addr         => addr,
      data_in      => data_out,
      data_out     => data_in,
      preset_data  => preset_data,
      data_display => data_display,
      preset       => reset,
      rd           => rd,
      wr           => wr);

  I_ssd : ssd_cpu_2bit
    PORT MAP (
      clk   => clk,
      rd    => rd,
      wr    => wr,
      ar_in => ar_out,
      br_in => br_out,
      aa_in => aa_out,
      bb_in => bb_out, 
      state => state,
      ip_in => ip_out,
      segs  => segs,
      ans   => ans);

END A_structure;
