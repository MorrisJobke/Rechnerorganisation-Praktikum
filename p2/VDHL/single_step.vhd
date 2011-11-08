-------------------------------------------------------------------------------
-- single step state machine
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY single_step IS
  PORT (
    clk    : IN  STD_LOGIC;
    reset  : IN  STD_LOGIC;
    button : IN  STD_LOGIC;
    step   : OUT STD_LOGIC);

END single_step;

ARCHITECTURE A_behav OF single_step IS

  -- Vereinbarung eines Aufzaehlungstyps fur 4 
  -- verschiedene Zustaende des Automaten ...
  TYPE   states IS (high, low, step_out, cnt_reset);
  -- ... und Vereinbarung entsprechender Signale
  -- von diesem Typ.
  SIGNAL current_state : states;
  SIGNAL next_state    : states;

  -- Der Zaehler 
  -- Natuerliche Zahlen von 0 - 262143
  -- => daraus wird ein 18 Bit Zaehler
  -- 5 ms Verzoegerung bei 50 MHz Taktgenerator
  SIGNAL counter       : NATURAL RANGE 0 TO 262143;
 
  -- Bis wohin soll der Zaehler zaehlen?
  -- (als Konstantenvereinbarung)
  -- Damit laesst sich die Entprellzeit variieren. 
  CONSTANT DELAY : NATURAL := 262143;

  -- Diverse interne Steuersignale
  SIGNAL timeout       : STD_LOGIC;
  SIGNAL cnt_clear     : STD_LOGIC;
  SIGNAL cnt_count     : STD_LOGIC;
  
BEGIN

  -- Zustandsuebernahme (mit jeder steigenden Taktflanke
  -- sowie asynchronen Reset)
  P_step_state : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      current_state <= high;
    ELSIF clk'EVENT AND clk = '1' THEN -- statt "clk'EVENT AND clk = '1'" kann man auch "rising_edge(clk)"(bzw" falling_edge(clk)") schreiben
      current_state <= next_state;
    END IF;
  END PROCESS P_step_state;

  -- Zustandsueberfuehrung
  P_step_trans_out : PROCESS (button, current_state, timeout)
  BEGIN
    CASE current_state IS
      WHEN high =>
        IF timeout = '1' AND button = '1' THEN
          next_state <= cnt_reset;
        ELSE
          next_state <= high;
        END IF;
      WHEN low =>
        IF timeout = '1' AND button = '0' THEN
          next_state <= step_out;
        ELSE
          next_state <= low;
        END IF;
      WHEN step_out =>
        next_state <= high;
      WHEN cnt_reset =>
        next_state <= low;
      WHEN OTHERS => NULL;
    END CASE;
  END PROCESS P_step_trans_out;

  -- Defacto die Ausgangsfunktionen des Automaten
  -- (hier nicht in einen Prozess verpackt - koennte man jedoch tun)
  step      <= '1' WHEN current_state = step_out ELSE '0';
  cnt_clear <= '1' WHEN (current_state = step_out) OR (current_state = cnt_reset) ELSE '0';
  cnt_count <= '1' WHEN (current_state = high) OR (current_state = low) ELSE '0';

  -- Der Zaehler fuer die Entprellzeit
  P_timeout : PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      counter <= 0;
    ELSIF clk'EVENT AND clk = '1' THEN
      IF cnt_clear = '1' THEN
        counter <= 0;
      ELSIF cnt_count = '1' THEN
        IF counter < DELAY THEN
          counter <= counter + 1;
        END IF;
      END IF;
    END IF;
  END PROCESS P_timeout;

  -- Signal zum Signalisieren, wenn der gewaehlte 
  -- (durch die Konstante DELAY) Zaehlerstand erreicht
  -- wurde 
  timeout <= '1' WHEN counter = DELAY ELSE '0';

END A_behav;
