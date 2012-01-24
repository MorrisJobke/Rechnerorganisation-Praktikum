-------------------------------------------------------------------------------
-- Title      : single_step  
-- Project    : CPU-4
-------------------------------------------------------------------------------
-- File       : single_step.vhd
-- Author     : Sven Lasch, René Oertel
-- Company    : Chemnitz University of Technology
-- Created    : 2002-02-10
-- Last update: 2010-09-15
-- Platform   : Digilent Nexys 2
-------------------------------------------------------------------------------
-- Description: single step with extension
--              - short push of the button -> 1 clock forward
--              - long push -> automatic clock generation (3 Hz)
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

  CONSTANT FREERUN_DELAY : NATURAL RANGE 0 TO 2**23-1 := 2**23-1;
  CONSTANT TOGGLE_DELAY  : NATURAL RANGE 0 TO 2**23-1 := 2**18-1;

  TYPE states IS (high, low, step_out, cnt_reset1, cnt_reset2, freerun);

  SIGNAL current_state  : states;
  SIGNAL next_state     : states;
  SIGNAL counter        : INTEGER RANGE 0 TO 2**23-1;
  SIGNAL cnt_clear      : STD_LOGIC;

BEGIN
  P_step_state : PROCESS (clk, reset) 
  BEGIN
    IF reset = '1' THEN
      current_state <= HIGH;
    ELSIF clk'EVENT AND clk = '1' THEN
      current_state <= next_state;
    END IF;
  END PROCESS P_step_state;

  P_step_trans_out : PROCESS (button, counter, current_state) 
  BEGIN
    CASE current_state IS
      WHEN high =>
        IF button = '1' AND counter > TOGGLE_DELAY THEN
          next_state <= cnt_reset1;
        ELSE
          next_state <= high;
        END IF;
      WHEN cnt_reset1 =>
        next_state <= low;
      WHEN low =>
        IF button = '1' AND counter = FREERUN_DELAY THEN
          next_state <= freerun;
        ELSIF button = '0' AND counter > TOGGLE_DELAY THEN
          next_state <= step_out;
        ELSE
          next_state <= low;
        END IF;
      WHEN step_out =>
        next_state <= high;
      WHEN freerun =>
        IF button = '0' THEN
          next_state <= cnt_reset2;
        ELSE
          next_state <= freerun;
        END IF;
      WHEN cnt_reset2 =>
        next_state  <= high;
      WHEN OTHERS => NULL;
    END CASE;
  END PROCESS P_step_trans_out;

  P_timeout : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      counter <= 0;
    ELSIF clk'EVENT AND clk = '1' THEN
      IF cnt_clear = '1' THEN
        counter <= 0;
      ELSE
        CASE current_state IS
          WHEN HIGH | LOW =>
            IF counter < FREERUN_DELAY THEN
              counter <= counter + 1;
            END IF;
          WHEN FREERUN =>
            IF counter < FREERUN_DELAY THEN
              counter <= counter + 1;
            ELSE
              counter <= 0;
            END IF;
          WHEN OTHERS => NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS P_timeout;

  step      <= '1' WHEN (current_state = step_out) OR (counter = FREERUN_DELAY AND current_state = freerun) ELSE '0';
  cnt_clear <= '1' WHEN (current_state = step_out) OR (current_state = cnt_reset1)
               OR (current_state = cnt_reset2) ELSE '0';

END A_behav;

