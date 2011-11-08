-- brems_tb.vhdl - Testbench für das Bremssystem
-- RS18042002

entity brems_tb is
end;

architecture a of brems_tb is
    component bremssystem is
        port
        (
            bremsen, blockiert, clk, einschalten: in bit;
            abs_led, bremse_aktiv: out bit
        );
    end component bremssystem;
    
    for all: bremssystem use entity work.bremssystem(arch_brems);
    
    signal brems, blk, clk, einsch, abs_out, bremse_out: bit;
begin
p1: process
    begin    
        clk <= not clk;            
        wait for 10 ns;
    end process p1;
    
p2: process
    begin
        brems <= '0';
        blk <= '0';
        einsch <= '1';
        
        wait for 40 ns;            
         
        brems <= '1';
        blk <= '0';
        einsch <= '0';
        
        wait for 40 ns;           
        
        brems <= '1';
        blk <= '1';
        
        wait for 40 ns; 
                              
        brems <= '1';
        blk <= '0';
        
        wait for 40 ns;
                 
        brems <= '1';
        blk <= '1';
        
        wait for 40 ns;
                       
        brems <= '1';
        blk <= '1';             
        
        wait for 40 ns;
        
        brems <= '0';
        blk <= '1';                       
        
        wait for 40 ns;
        wait;
    end process p2;     
   
u1: bremssystem port map(brems, blk, clk, einsch, abs_out, bremse_out);
end;
