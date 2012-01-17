-------------------------------------------------------------------------------
-- Title      : cpu 2bit top-level
-- Project    : cpu 2bit
-------------------------------------------------------------------------------
-- File       : cpu_2bit_top_level.vhd
-- Author     : Sven Lasch, René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2001-11-08
-- Last update: 2010-09-13
-- Platform   : ANY
-------------------------------------------------------------------------------
-- Description: 2 bit CPU with 4 bit address extension 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author      Description
-- 2001-11-08  1.0      Sven Lasch  Created
-- 2010-09-13  2.0      René Oertel Modified for Digilent Nexys 2
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Entity ip_counter 4-bit
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
    ip_in  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    ip_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));

END ip_counter;

ARCHITECTURE A_behav OF ip_counter IS

  SIGNAL ip : STD_LOGIC_VECTOR(3 DOWNTO 0);

BEGIN

  P_ip : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      ip <= (OTHERS => '0');
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF ip_ld = '1' THEN
          ip <= ip_in;
        ELSIF ip_cnt = '1' THEN
          ip <= ip + 1;
        END IF;
      END IF;
    END IF;
  END PROCESS P_ip;
  ip_out <= ip;

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

  SIGNAL reg : STD_LOGIC_VECTOR(1 DOWNTO 0);

BEGIN

  P_reg : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      reg <= (OTHERS => '0');
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF reg_en = '1' THEN
          reg <= reg_in;
        END IF;
      END IF;
    END IF;
  END PROCESS P_reg;
  reg_out <= reg;

END A_behav;

-------------------------------------------------------------------------------
-- Entity register_4bit
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY register_4bit IS

  PORT (
    clk     : IN  STD_LOGIC;
    reset   : IN  STD_LOGIC;
    step    : IN  STD_LOGIC;
    reg_en  : IN  STD_LOGIC;
    reg_in  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    reg_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));

END register_4bit;

ARCHITECTURE A_behav OF register_4bit IS

  SIGNAL reg : STD_LOGIC_VECTOR(3 DOWNTO 0);

BEGIN

  P_reg : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      reg <= (OTHERS => '0');
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF reg_en = '1' THEN
          reg <= reg_in;
        END IF;
      END IF;
    END IF;
  END PROCESS P_reg;
  reg_out <= reg;

END A_behav;

-------------------------------------------------------------------------------
-- Entity mux_2bit
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY mux_2bit IS

  PORT (
    sel : IN  STD_LOGIC;
    a   : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    b   : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    y   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));

END mux_2bit;

ARCHITECTURE A_behav OF mux_2bit IS

BEGIN

  y <= a WHEN sel = '0' ELSE b;

END A_behav;

-------------------------------------------------------------------------------
-- Entity mux_4bit
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY mux_4bit IS

  PORT (
    sel : IN  STD_LOGIC;
    a   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    b   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    y   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));

END mux_4bit;

ARCHITECTURE A_behav OF mux_4bit IS

BEGIN

  y <= a WHEN sel = '0' ELSE b;

END A_behav;

-------------------------------------------------------------------------------
-- Entity alu
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY alu IS

  PORT (
    a : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    b : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    y : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));

END alu;

ARCHITECTURE A_behav OF alu IS

BEGIN

  y <= a + b;

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
    state  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    br_en  : OUT STD_LOGIC;
    arl_en : OUT STD_LOGIC;
    arh_en : OUT STD_LOGIC;
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

  SIGNAL f       : STD_LOGIC_VECTOR(4 DOWNTO 1);
  SIGNAL modres  : STD_LOGIC;
  SIGNAL ip_cnt1 : STD_LOGIC;
  SIGNAL ip_cnt2 : STD_LOGIC;

BEGIN

  P_counter : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      f     <= "0001";
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF modres = '1' THEN
          f <= "0001";
        ELSE
          f <= f(3 DOWNTO 1) & f(4);
        END IF;
      END IF;
    END IF;
  END PROCESS P_counter;

  state <= f;

  br_en   <= f(1);
  arl_en  <= ((f(2) AND NOT br(1) AND br(0)) OR (f(2) AND br(1) AND NOT br(0)) OR (f(2) AND NOT br(1) AND NOT br(0)));
  arh_en  <= ((f(3) AND NOT br(1) AND br(0)) OR (f(3) AND br(1) AND NOT br(0)) OR (f(3) AND NOT br(1) AND NOT br(0)));
  aa_en   <= ((f(2) AND br(1) AND br(0)) OR (f(4) AND NOT br(1) AND br(0)));
  ip_ld   <= (f(4) AND NOT br(1) AND NOT br(0));
  ip_cnt1 <= (f(2) AND NOT br(1) AND br(0)) OR (f(2) AND br(1) AND NOT br(0)) OR (f(2) AND NOT br(1) AND NOT br(0));
  ip_cnt2 <= (f(3) AND NOT br(1) AND br(0)) OR (f(3) AND br(1) AND NOT br(0)) OR (f(3) AND NOT br(1) AND NOT br(0));
  ip_cnt  <= f(1) OR ip_cnt1 OR ip_cnt2;

  rd     <= (f(1) OR f(2) OR f(3) OR (f(4) AND NOT br(1) AND br(0)));
  wr     <= (f(4) AND br(1) AND NOT br(0));
  modres <= (f(2) AND br(1) AND br(0));
  m1_s   <= f(4);
  m2_s   <= f(4);
  m3_s   <= f(4);
  m4_s   <= f(4);
  
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
    addr     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    state    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    ar_out   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    br_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    ip_out   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    aa_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    bb_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_in  : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));

END cpu_2bit;

ARCHITECTURE A_structure OF cpu_2bit IS

  SIGNAL ar      : STD_LOGIC_VECTOR(3 DOWNTO 0);  -- address register
  SIGNAL br      : STD_LOGIC_VECTOR(1 DOWNTO 0);  -- instruction register
  SIGNAL ip      : STD_LOGIC_VECTOR(3 DOWNTO 0);  -- instruction pointer
  SIGNAL aa_in   : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL aa      : STD_LOGIC_VECTOR(1 DOWNTO 0);  -- accumulator
  SIGNAL bb      : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL alu_in1 : STD_LOGIC_VECTOR(3 DOWNTO 0); 
  SIGNAL alu_in2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL alu_out : STD_LOGIC_VECTOR(3 DOWNTO 0);
  
  SIGNAL m3_ain : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL m4_ain : STD_LOGIC_VECTOR(3 DOWNTO 0);
 
  SIGNAL ip_cnt : STD_LOGIC;
  SIGNAL ip_ld  : STD_LOGIC;
  SIGNAL arl_en : STD_LOGIC;
  SIGNAL arh_en : STD_LOGIC;
  SIGNAL br_en  : STD_LOGIC;
  SIGNAL aa_en  : STD_LOGIC;
  SIGNAL m1_s   : STD_LOGIC;
  SIGNAL m2_s   : STD_LOGIC;
  SIGNAL m3_s   : STD_LOGIC;
  SIGNAL m4_s   : STD_LOGIC;

  COMPONENT ip_counter
    PORT (
      clk    : IN  STD_LOGIC;
      reset  : IN  STD_LOGIC;
      step   : IN  STD_LOGIC;
      ip_ld  : IN  STD_LOGIC;
      ip_cnt : IN  STD_LOGIC;
      ip_in  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      ip_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;

  COMPONENT register_2bit
    PORT (
      clk     : IN  STD_LOGIC;
      reset   : IN  STD_LOGIC;
      step    : IN  STD_LOGIC;
      reg_en  : IN  STD_LOGIC;
      reg_in  : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      reg_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));
  END COMPONENT;

  COMPONENT register_4bit
    PORT (
      clk     : IN  STD_LOGIC;
      reset   : IN  STD_LOGIC;
      step    : IN  STD_LOGIC;
      reg_en  : IN  STD_LOGIC;
      reg_in  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      reg_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;

  COMPONENT mux_2bit
    PORT (
      sel : IN  STD_LOGIC;
      a   : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      b   : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      y   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));
  END COMPONENT;

  COMPONENT mux_4bit
    PORT (
      sel : IN  STD_LOGIC;
      a   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      b   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      y   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;

  COMPONENT alu
    PORT (
      a : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      b : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      y : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;

  COMPONENT steuerwerk
    PORT (
      clk    : IN  STD_LOGIC;
      reset  : IN  STD_LOGIC;
      step   : IN  STD_LOGIC;
      br     : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      state  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      br_en  : OUT STD_LOGIC;
      arl_en : OUT STD_LOGIC;
      arh_en : OUT STD_LOGIC;
      aa_en  : OUT STD_LOGIC;
      ip_ld  : OUT STD_LOGIC;
      ip_cnt : OUT STD_LOGIC;
      rd     : OUT STD_LOGIC;
      wr     : OUT STD_LOGIC;
      m1_s   : OUT STD_LOGIC;
      m2_s   : OUT STD_LOGIC;
      m3_s   : OUT STD_LOGIC;
      m4_s   : OUT STD_LOGIC);
  END COMPONENT;

BEGIN

  ar_out   <= ar;
  br_out   <= br;
  ip_out   <= ip;
  aa_out   <= aa;
  bb_out   <= bb;
  data_out <= aa;

  -- IP: IP-Counter
  IP1 : ip_counter
    PORT MAP (
      clk    => clk,
      reset  => reset,
      step   => step,
      ip_ld  => ip_ld,
      ip_cnt => ip_cnt,
      ip_in  => alu_out,
      ip_out => ip);

  -- BR: Instruction register
  BR1 : register_2bit
    PORT MAP (
      clk     => clk,
      reset   => reset,
      step    => step,
      reg_en  => br_en,
      reg_in  => data_in,
      reg_out => br);

  -- AR: Address register 2x2bit
  ARL : register_2bit
    PORT MAP (
      clk     => clk,
      reset   => reset,
      step    => step,
      reg_en  => arl_en,
      reg_in  => data_in,
      reg_out => ar(1 DOWNTO 0));
      
  ARH : register_2bit
    PORT MAP (
      clk     => clk,
      reset   => reset,
      step    => step,
      reg_en  => arh_en,
      reg_in  => data_in,
      reg_out => ar(3 DOWNTO 2));

  -- AA: Accumulator
  AA1 : register_2bit
    PORT MAP (
      clk     => clk,
      reset   => reset,
      step    => step,
      reg_en  => aa_en,
      reg_in  => aa_in,
      reg_out => aa);

  -- M1: Multiplexer IP, AR
  M1 : mux_4bit
    PORT MAP (
      sel => m1_s,
      a   => ip,
      b   => ar,
      y   => addr);

  -- M2: Multiplexer ALU, Data_in
  M2 : mux_2bit
    PORT MAP (
      sel => m2_s,
      a   => alu_out(1 DOWNTO 0),
      b   => data_in,
      y   => aa_in);

  -- M3: Multiplexer AA, AR
  m3_ain <= ("00" & aa);
  M3 : mux_4bit
    PORT MAP (
      sel => m3_s,
      a   => m3_ain,
      b   => ar,
      y   => alu_in1);

  bb <= "01";

  -- M4: Multiplexer BB, IP
  m4_ain <= ("00" & bb);
  M4 : mux_4bit
    PORT MAP (
      sel => m4_s,
      a   => m4_ain,
      b   => ip,
      y   => alu_in2);

  ALU1 : alu
    PORT MAP (
      a => alu_in1,
      b => alu_in2,
      y => alu_out);

  SW1 : steuerwerk
    PORT MAP (
      clk    => clk,
      reset  => reset,
      step   => step,
      br     => br,
      state  => state,
      br_en  => br_en,
      arl_en => arl_en,
      arh_en => arh_en,
      aa_en  => aa_en,
      ip_ld  => ip_ld,
      ip_cnt => ip_cnt,
      rd     => rd,
      wr     => wr,
      m1_s   => m1_s,
      m2_s   => m2_s,
      m3_s   => m3_s,
      m4_s   => m4_s);

END A_structure;

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
    addr         : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    data_in      : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_out     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_display : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    preset       : IN  STD_LOGIC;
    rd           : IN  STD_LOGIC;
    wr           : IN  STD_LOGIC);

END memory;

ARCHITECTURE A_behav OF memory IS

  TYPE mem_type IS ARRAY (0 TO 15) OF STD_LOGIC_VECTOR(1 DOWNTO 0);

  SIGNAL memory_array : mem_type;

BEGIN

  P_memory : PROCESS (clk, preset)
  BEGIN
    IF preset = '1' THEN
 
      -- Speichervoreinstellung wird hier vorgenommen. Durch Drücken
      -- des Tasters BTN1 werden die folgenden Werte in den
      -- Hauptspeicher der CPU geladen.

      memory_array(0)  <= "01";         -- READ $E (14)
      memory_array(1)  <= "10";
      memory_array(2)  <= "11";
      memory_array(3)  <= "11";         -- ADD
      memory_array(4)  <= "11";         -- ADD
      memory_array(5)  <= "11";         -- ADD
      memory_array(6)  <= "11";         -- ADD
      memory_array(7)  <= "11";         -- ADD
      memory_array(8)  <= "11";         -- ADD
      memory_array(9)  <= "11";         -- ADD
      memory_array(10) <= "10";         -- WRITE $0
      memory_array(11) <= "00";
      memory_array(12) <= "00";
      memory_array(13) <= "00";         -- JUMP $3
      memory_array(14) <= "11";
      memory_array(15) <= "00";

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
    ar_in : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    br_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    aa_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    bb_in : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);    
    state : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    ip_in : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    segs  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    ans   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  
END ssd_cpu_2bit;

ARCHITECTURE A_behav OF ssd_cpu_2bit IS

  SIGNAL cnt      : STD_LOGIC_VECTOR (16 DOWNTO 0);
  SIGNAL segs_an0 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL segs_an1 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL segs_an2 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL segs_an3 : STD_LOGIC_VECTOR (7 DOWNTO 0);
  SIGNAL statedis : STD_LOGIC_VECTOR (1 DOWNTO 0);
  SIGNAL data_int : STD_LOGIC_VECTOR (3 DOWNTO 0);
  SIGNAL segs_int : STD_LOGIC_VECTOR (6 DOWNTO 0);

BEGIN
  
  -- data_int is changed with cnt below
  WITH data_int SELECT segs_int <=  -- 4 bit binary to seven segment hexadecimal    
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

  -- segs_int changes with cnt/data_int
  segs_an0 <= wr & segs_int; -- wr, IP (4 bit)
  segs_an1 <= rd & segs_int; -- rd, AR (4 bit)
    
  WITH state SELECT statedis <= -- 4 bit 1-of-4 to 2 bit binary
    "00" WHEN "0001", -- F1
    "01" WHEN "0010", -- F2
    "10" WHEN "0100", -- F3
    "11" WHEN "1000", -- F4
    "00" WHEN OTHERS;

  -- segments dp,cg  cf            ce         cd    cc         cb            ca  
  segs_an2 <= "00" & br_in(1)    & bb_in(1) & '0' & bb_in(0) & br_in(0)    & '0';
  segs_an3 <= "00" & statedis(1) & aa_in(1) & '0' & aa_in(0) & statedis(0) & '0';
  
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
    segs_an1 WHEN "01",     -- ar, rd
    segs_an2 WHEN "10",     -- br, bb
    segs_an3 WHEN "11",     -- state, aa
    "11111111" WHEN OTHERS; -- all segments
   
  WITH cnt(16 DOWNTO 15) SELECT data_int <= -- which source for hexadecimal
    ip_in WHEN "00", -- ip
    ar_in WHEN "01", -- ar
    "0000" WHEN OTHERS;

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

  SIGNAL addr         : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL data_in      : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL data_out     : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL rd           : STD_LOGIC;
  SIGNAL wr           : STD_LOGIC;
  SIGNAL step         : STD_LOGIC;
  SIGNAL state        : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL ar_out       : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL br_out       : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL ip_out       : STD_LOGIC_VECTOR(3 DOWNTO 0);
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
      addr     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      state    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      ar_out   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      br_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      ip_out   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      aa_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      bb_out   : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_in  : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0));
  END COMPONENT;

  COMPONENT memory
    PORT (
      clk          : IN  STD_LOGIC;
      step         : IN  STD_LOGIC;
      addr         : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      data_in      : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      data_out     : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
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
      ar_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
      br_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      aa_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
      bb_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);    
      state : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
      ip_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
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
