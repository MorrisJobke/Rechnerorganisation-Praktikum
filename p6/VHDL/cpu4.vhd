-------------------------------------------------------------------------------
-- Title      : cpu4 
-- Project    : CPU-4
-------------------------------------------------------------------------------
-- File       : cpu4.vhd
-- Author     : Sven Lasch, René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2002-02-10
-- Last update: 2010-09-15
-- Platform   : All
-------------------------------------------------------------------------------
-- Description: - 4-Bit CPU with reduced instruction set
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Address register 8-bit
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_arith.ALL;

ENTITY ar8 IS

  PORT (
    clk   : IN  STD_LOGIC;
    reset : IN  STD_LOGIC;
    step  : IN  STD_LOGIC;
    d     : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    enl   : IN  STD_LOGIC;
    enh   : IN  STD_LOGIC;
    q     : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));

END ar8;

ARCHITECTURE A_behav OF ar8 IS

BEGIN

  P_ar8 : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      q <= "00000000";
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF enl = '1' THEN
          q(3 DOWNTO 0) <= d;
        ELSIF enh = '1' THEN
          q(7 DOWNTO 4) <= d;
        END IF;
      END IF;
    END IF;
  END PROCESS P_ar8;

END A_behav;

-------------------------------------------------------------------------------
-- ip-counter 8-bit
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY ipcount8 IS

  PORT (
    clk   : IN  STD_LOGIC;
    reset : IN  STD_LOGIC;
    step  : IN  STD_LOGIC;
    d     : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
    cnt   : IN  STD_LOGIC;
    ld    : IN  STD_LOGIC;
    q     : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));  

END ipcount8;

ARCHITECTURE A_behav OF ipcount8 IS
  SIGNAL qint_s : STD_LOGIC_VECTOR(7 DOWNTO 0);

BEGIN

  P_ipcount8 : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      qint_s <= "00000000";
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF ld = '1' THEN
          qint_s <= d;
        ELSIF cnt = '1' THEN
          qint_s <= qint_s + 1;
        END IF;
      END IF;
    END IF;
  END PROCESS P_ipcount8;
  q <= qint_s;
END A_behav;

-------------------------------------------------------------------------------
-- Register for carry and zero flags 
-- 
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY reg1 IS

  PORT (
    clk   : IN  STD_LOGIC;
    reset : IN  STD_LOGIC;
    step  : IN  STD_LOGIC;
    d     : IN  STD_LOGIC;
    en    : IN  STD_LOGIC;
    q     : OUT STD_LOGIC);

END reg1;

ARCHITECTURE A_behav OF reg1 IS

BEGIN

  P_reg1 : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      q <= '0';
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF en = '1' THEN
          q <= d;
        END IF;
      END IF;
    END IF;
  END PROCESS P_reg1;

END A_behav;

-------------------------------------------------------------------------------
-- 4-bit register for several applications (A, B, BR)
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY reg4 IS

  PORT (
    clk   : IN  STD_LOGIC;
    reset : IN  STD_LOGIC;
    step  : IN  STD_LOGIC;
    d     : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    en    : IN  STD_LOGIC;
    q     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));

END reg4;

ARCHITECTURE A_behav OF reg4 IS

BEGIN

  P_reg4 : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      q <= "0000";
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        IF en = '1' THEN
          q <= d;
        END IF;
      END IF;
    END IF;
  END PROCESS P_reg4;

END A_behav;

-------------------------------------------------------------------------------
-- 4-bit multiplexer 3 to 1
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY mux34 IS

  PORT (
    a : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    b : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    c : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    s : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
    z : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));

END mux34;

ARCHITECTURE A_behav OF mux34 IS

BEGIN

  WITH s SELECT z <=
    a      WHEN "00",
    b      WHEN "01",
    c      WHEN "10",
    "0000" WHEN OTHERS;
  
END A_behav;

-------------------------------------------------------------------------------
-- multiplexer 2 to 1 [8-Bit]
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY mux28 IS

  PORT (
    a : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
    b : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
    s : IN  STD_LOGIC;
    z : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));

END mux28;

ARCHITECTURE A_behav OF mux28 IS

BEGIN

  WITH s SELECT z <=
    a WHEN '0',
    b WHEN OTHERS;
    
END A_behav;

-------------------------------------------------------------------------------
-- ALU 
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;
USE IEEE.std_logic_arith.ALL;

ENTITY alu IS
  PORT (
    a    : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);  -- Operand A
    b    : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);  -- Operand B
    op   : IN  STD_LOGIC_VECTOR(2 DOWNTO 0);  -- operation code
    cin  : IN  STD_LOGIC;                     -- carry in from register
    alu  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);  -- alu output
    c_en : OUT STD_LOGIC;                     -- carry enable
    z_en : OUT STD_LOGIC;                     -- zero enable
    cout : OUT STD_LOGIC;                     -- carry out
    zout : OUT STD_LOGIC);                    -- zero out
END alu;

ARCHITECTURE A_behav OF alu IS

BEGIN

  P_alu : PROCESS (a, b, cin, op)
    VARIABLE aluint_v : STD_LOGIC_VECTOR(4 DOWNTO 0);
    
  BEGIN
    CASE op IS
      -- operation ADC 
      WHEN "000" =>
        aluint_v := ('0'&a) + ('0'&b) + ("0000"&cin);
        cout     <= aluint_v(4);
        c_en     <= '1';
        z_en     <= '1';
        IF aluint_v(3 DOWNTO 0) = "0000" THEN
          zout <= '1';
        ELSE
          zout <= '0';
        END IF;

        -- operation NOT
      WHEN "001" =>
        aluint_v := '0'& NOT a;
        cout     <= '0';
        c_en     <= '0';
        z_en     <= '1';
        IF aluint_v = "0000" THEN
          zout <= '1';
        ELSE
          zout <= '0';
        END IF;

        -- operation CCF
      WHEN "010" =>
        aluint_v := "00000";
        cout     <= '0';
        c_en     <= '1';
        zout     <= '0';
        z_en     <= '0';

        -- operation RRC
      WHEN "011" =>
        aluint_v(4) := '0';
        aluint_v(3) := cin;
        aluint_v(2) := a(3);
        aluint_v(1) := a(2);
        aluint_v(0) := a(1);
        cout        <= a(0);
        c_en        <= '1';
        z_en        <= '1';
        IF aluint_v(3 DOWNTO 0) = "0000" THEN
          zout <= '1';
        ELSE
          zout <= '0';
        END IF;

        -- operation SCF
      WHEN "111" =>
        aluint_v := "00000";
        cout     <= '1';
        c_en     <= '1';
        zout     <= '0';
        z_en     <= '0';
        
      WHEN OTHERS =>
        aluint_v := "00000";
        cout     <= '0';
        zout     <= '0';
        c_en     <= '0';
        z_en     <= '0';
    END CASE;
    alu <= aluint_v(3 DOWNTO 0);
  END PROCESS P_alu;

END A_behav;

-------------------------------------------------------------------------------
-- Microcode control unit
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;
USE IEEE.std_logic_arith.ALL;

ENTITY sequencer IS

  PORT (
    clk     : IN  STD_LOGIC;
    reset   : IN  STD_LOGIC;
    step    : IN  STD_LOGIC;
    next_mc : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);
    br_in   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    q       : OUT STD_LOGIC_VECTOR(5 DOWNTO 0));

END sequencer;

ARCHITECTURE A_behav OF sequencer IS

  SIGNAL mc_table : STD_LOGIC_VECTOR(5 DOWNTO 0);
  SIGNAL q_int    : STD_LOGIC_VECTOR(5 DOWNTO 0);
  
  
  COMPONENT mctable 
  PORT (
    address  : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    data_out : OUT STD_LOGIC_VECTOR(5 DOWNTO 0));
  END COMPONENT;
    
BEGIN

  I_MCTABLE : mctable
    PORT MAP (
      address => br_in,
      data_out => mc_table);      

  P_state : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      q_int <= (OTHERS => '0');
    ELSIF clk'EVENT AND clk = '1' THEN
      IF step = '1' THEN
        -- After microprogram address '000001'
        -- the microcode table is used for the next microcode address
        IF q_int = "000001" THEN
          q_int <= mc_table;          
        ELSE
          q_int <= next_mc;
        END IF;
      END IF;
    END IF;
  END PROCESS P_state;
  q <= q_int;

END A_behav;

-------------------------------------------------------------------------------
-- Microcode-ROM
--
LIBRARY IEEE;
USE ieee.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;
USE ieee.std_logic_arith.ALL;


ENTITY kombi IS

  PORT (
    br      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);  -- instruction opcode
    z       : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);  -- sequence counter state
    zf      : IN  STD_LOGIC;            -- Zero-Flag
    cf      : IN  STD_LOGIC;            -- Carry-Flag
    op      : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);  -- Opcode Alu
    mreq    : OUT STD_LOGIC;            -- memory request
    ioreq   : OUT STD_LOGIC;            -- ioreq
    wr      : OUT STD_LOGIC;            -- write
    a_en    : OUT STD_LOGIC;            -- accu enable
    arl_en  : OUT STD_LOGIC;
    arh_en  : OUT STD_LOGIC;
    b_en    : OUT STD_LOGIC;            -- register 'b' enable
    br_en   : OUT STD_LOGIC;            -- opcode register enable
    ip_cnt  : OUT STD_LOGIC;            -- instruction pointer count up
    ip_ld   : OUT STD_LOGIC;            -- instruction pointer load
    m1_s    : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    m2_s    : OUT STD_LOGIC;
    next_mc : OUT STD_LOGIC_VECTOR(5 DOWNTO 0));

END kombi;

ARCHITECTURE A_behav OF kombi IS

  SIGNAL microcode : STD_LOGIC_VECTOR(18 DOWNTO 0);
  SIGNAL next_mc_s : STD_LOGIC_VECTOR(5 DOWNTO 0);
  
  COMPONENT microcode_rom
  PORT (
    address  : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);
    data_out : OUT STD_LOGIC_VECTOR(18 DOWNTO 0));
  END COMPONENT;

BEGIN

  I_MICROCODE : microcode_rom
    PORT MAP (
      address => z,
      data_out => microcode);

  next_mc_s <= microcode(18 DOWNTO 13);
  wr        <= microcode(12);
  mreq      <= microcode(11);
  ioreq     <= microcode(10);
  br_en     <= microcode(9);
  arl_en    <= microcode(8);
  arh_en    <= microcode(7);
  a_en      <= microcode(6);
  b_en      <= microcode(5);
  ip_cnt    <= microcode(4);
  ip_ld     <= microcode(3);
  m1_s      <= microcode(2 DOWNTO 1);
  m2_s      <= microcode(0);

  P_op : PROCESS (br, z)
  BEGIN
    IF z = "000010" OR z = "000011" THEN
      CASE br IS
        WHEN "1100" =>
          op <= "000";                  -- opcode adc
        WHEN "1101" =>
          op <= "001";                  -- opcode not
        WHEN "1111" =>
          op <= "011";                  -- opcode rrc
        WHEN "0011" =>
          op <= "111";                  -- opcode scf
        WHEN "0010" =>
          op <= "010";                  -- opcode ccf
        WHEN OTHERS =>
          op <= "100";
      END CASE;
    ELSE
      op <= "100";
    END IF;
  END PROCESS P_op;

  -- If C='1' or Z='1' then the next microcode address
  -- is modified for JPC and JPZ instructions
  -- It is inflexible and ugly, but ...
  
  P_next_mc : PROCESS (br, cf, next_mc_s, z, zf)
    VARIABLE next_mc_v : STD_LOGIC_VECTOR(5 DOWNTO 0);
  BEGIN
    next_mc_v := next_mc_s;
    IF z = "000100" THEN
      IF (br = "0101" AND cf = '1') OR (br = "0100" AND zf = '1') THEN
        -- If condition is true, go on with microcode address 7
        next_mc_v := "000111";  -- jpc c/z = 1
      END IF;
    END IF;
    next_mc <= next_mc_v;
  END PROCESS P_next_mc;

END A_behav;

-------------------------------------------------------------------------------
-- top level entity 
--
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY cpu4 IS

  PORT (
    clk       : IN  STD_LOGIC;
    reset     : IN  STD_LOGIC;
    step      : IN  STD_LOGIC;
    data_in   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
    rd        : OUT STD_LOGIC;
    wr        : OUT STD_LOGIC;
    mreq      : OUT STD_LOGIC;
    ioreq     : OUT STD_LOGIC;
    state     : OUT STD_LOGIC_VECTOR(5 DOWNTO 0);
    addr      : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    data_out  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    ip_out    : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    ar_out    : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    br_out    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    a_out    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    b_out    : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    carry_out : OUT STD_LOGIC;
    zero_out  : OUT STD_LOGIC);
    
END cpu4;

ARCHITECTURE A_structure OF cpu4 IS

  SIGNAL current_mc : STD_LOGIC_VECTOR(5 DOWNTO 0);
  SIGNAL m1         : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL br         : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL alu_out    : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL ar         : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL ip         : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL a          : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL b          : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL op         : STD_LOGIC_VECTOR(2 DOWNTO 0);

  SIGNAL br_en    : STD_LOGIC;
  SIGNAL ip_cnt   : STD_LOGIC;
  SIGNAL ip_ld    : STD_LOGIC;
  SIGNAL a_en     : STD_LOGIC;
  SIGNAL b_en     : STD_LOGIC;
  SIGNAL arl_en   : STD_LOGIC;
  SIGNAL arh_en   : STD_LOGIC;
  SIGNAL carry_en : STD_LOGIC;
  SIGNAL zero_en  : STD_LOGIC;
  SIGNAL m1_s     : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL m2_s     : STD_LOGIC;
  SIGNAL c_out    : STD_LOGIC;
  SIGNAL z_out    : STD_LOGIC;
  SIGNAL carry    : STD_LOGIC;
  SIGNAL zero     : STD_LOGIC;
  SIGNAL wr_nrd   : STD_LOGIC;          -- rd and wr signals
  SIGNAL next_mc  : STD_LOGIC_VECTOR(5 DOWNTO 0);

  COMPONENT ar8
    PORT (
      clk   : IN  STD_LOGIC;
      reset : IN  STD_LOGIC;
      step  : IN  STD_LOGIC;
      d     : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      enl   : IN  STD_LOGIC;
      enh   : IN  STD_LOGIC;
      q     : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
  END COMPONENT;

  COMPONENT ipcount8
    PORT (
      clk   : IN  STD_LOGIC;
      reset : IN  STD_LOGIC;
      step  : IN  STD_LOGIC;
      d     : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      cnt   : IN  STD_LOGIC;
      ld    : IN  STD_LOGIC;
      q     : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
  END COMPONENT;

  COMPONENT reg1
    PORT (
      clk   : IN  STD_LOGIC;
      reset : IN  STD_LOGIC;
      step  : IN  STD_LOGIC;
      d     : IN  STD_LOGIC;
      en    : IN  STD_LOGIC;
      q     : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT reg4
    PORT (
      clk   : IN  STD_LOGIC;
      reset : IN  STD_LOGIC;
      step  : IN  STD_LOGIC;
      d     : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      en    : IN  STD_LOGIC;
      q     : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;

  COMPONENT mux34
    PORT (
      a : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      b : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      c : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      s : IN  STD_LOGIC_VECTOR(1 DOWNTO 0);
      z : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
  END COMPONENT;

  COMPONENT mux28
    PORT (
      a : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      b : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      s : IN  STD_LOGIC;
      z : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)); 
  END COMPONENT;

  COMPONENT alu
    PORT (
      a    : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      b    : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      op   : IN  STD_LOGIC_VECTOR(2 DOWNTO 0);
      cin  : IN  STD_LOGIC;
      alu  : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
      c_en : OUT STD_LOGIC;
      z_en : OUT STD_LOGIC;
      cout : OUT STD_LOGIC;
      zout : OUT STD_LOGIC);
  END COMPONENT;

  COMPONENT kombi
    PORT (
      br      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      z       : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);
      zf      : IN  STD_LOGIC;
      cf      : IN  STD_LOGIC;
      op      : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
      mreq    : OUT STD_LOGIC;
      ioreq   : OUT STD_LOGIC;
      wr      : OUT STD_LOGIC;
      a_en    : OUT STD_LOGIC;
      arl_en  : OUT STD_LOGIC;
      arh_en  : OUT STD_LOGIC;
      b_en    : OUT STD_LOGIC;
      br_en   : OUT STD_LOGIC;
      ip_cnt  : OUT STD_LOGIC;
      ip_ld   : OUT STD_LOGIC;
      m1_s    : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
      m2_s    : OUT STD_LOGIC;
      next_mc : OUT STD_LOGIC_VECTOR(5 DOWNTO 0));
  END COMPONENT;

  COMPONENT sequencer
    PORT (
      clk     : IN  STD_LOGIC;
      reset   : IN  STD_LOGIC;
      step    : IN  STD_LOGIC;
      next_mc : IN  STD_LOGIC_VECTOR(5 DOWNTO 0);
      br_in   : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      q       : OUT STD_LOGIC_VECTOR(5 DOWNTO 0));
  END COMPONENT;

BEGIN

  rd <= NOT wr_nrd;
  wr <= wr_nrd;

  ip_out    <= ip;
  ar_out    <= ar;
  br_out    <= br;
  a_out     <= a;
  b_out     <= b;
  carry_out <= carry;
  zero_out  <= zero;
  data_out  <= a;
  state     <= current_mc;

  -- BR: Instruction register
  I_BR : reg4
    PORT MAP (
      clk   => clk,
      reset => reset,
      step  => step,
      d     => data_in,
      en    => br_en,
      q     => br);

  -- AR: Address register
  I_AR : ar8
    PORT MAP (
      clk   => clk,
      reset => reset,
      step  => step,
      d     => data_in,
      enl   => arl_en,
      enh   => arh_en,
      q     => ar);

  -- IP: IP-Counter
  I_IP : ipcount8
    PORT MAP (
      clk   => clk,
      reset => reset,
      step  => step,
      d     => ar,
      cnt   => ip_cnt,
      ld    => ip_ld,
      q     => ip);

  -- Multiplexer 1
  I_M1 : mux34
    PORT MAP (
      a => alu_out,
      b => data_in,
      c => b,
      s => m1_s,
      z => m1);

  -- Multiplexer 2
  I_M2 : mux28
    PORT MAP (
      a => ar,
      b => ip,
      s => m2_s,
      z => addr);

  -- Register AA
  I_REGA : reg4
    PORT MAP (
      clk   => clk,
      reset => reset,
      step  => step,
      d     => m1,
      en    => a_en,
      q     => a);

  -- Register BB
  I_REGB : reg4
    PORT MAP (
      clk   => clk,
      reset => reset,
      step  => step,
      d     => a,
      en    => b_en,
      q     => b);

  -- Carry-Flag  
  I_CARRY : reg1
    PORT MAP (
      clk   => clk,
      reset => reset,
      step  => step,
      d     => c_out,
      en    => carry_en,
      q     => carry);

  -- Zero-Flag [z]
  I_ZERO : reg1
    PORT MAP (
      clk   => clk,
      reset => reset,
      step  => step,
      d     => z_out,
      en    => zero_en,
      q     => zero);

  -- Arithmetic-Logical-Unit [alu_komb] ---
  I_ALU : alu
    PORT MAP (
      a    => a,
      b    => b,
      op   => op,
      cin  => carry,
      alu  => alu_out,
      c_en => carry_en,
      z_en => zero_en,
      cout => c_out,
      zout => z_out);

  -- Counter [control unit] ---
  I_SEQUENCER : sequencer
    PORT MAP (
      clk     => clk,
      reset   => reset,
      step    => step,
      next_mc => next_mc,
      br_in   => data_in,
      q       => current_mc);

  -- Control unit combinatorial part ---
  I_KOMBI : kombi
    PORT MAP (
      br      => br,
      z       => current_mc,
      zf      => zero,
      cf      => carry,
      op      => op,
      mreq    => mreq,
      ioreq   => ioreq,
      wr      => wr_nrd,
      a_en    => a_en,
      arl_en  => arl_en,
      arh_en  => arh_en,
      b_en    => b_en,
      br_en   => br_en,
      ip_cnt  => ip_cnt,
      ip_ld   => ip_ld,
      m1_s    => m1_s,
      m2_s    => m2_s,
      next_mc => next_mc);

END A_structure;
