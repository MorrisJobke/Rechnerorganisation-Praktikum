-- bremssystem.vhdl - Beispiel 1, RO-Praktikum 2
-- RS18042002
-- Letzte Änderung: oere20101125

entity bremssystem is
    port
    (
        bremsen, blockiert, clk, einschalten: in bit;
        abs_led, bremse_aktiv: out bit
    );
end;

architecture arch_brems of bremssystem is
    
    -- Definition der 3 Zustaende
    constant ZUSTAND1: bit_vector(1 downto 0) := "00";
    constant ZUSTAND2: bit_vector(1 downto 0) := "10";
    constant ZUSTAND3: bit_vector(1 downto 0) := "01";
    
    signal akt_zustand, naechst_zustand: bit_vector(1 downto 0);
    
begin

f:  process(akt_zustand, bremsen, blockiert)        -- Ueberfuehrungsfunktion
    begin
        case akt_zustand is
            when ZUSTAND1 => 
                if bremsen = '1' then
                    naechst_zustand <= ZUSTAND2;
					 else
                    naechst_zustand <= ZUSTAND1;	
                end if;
            when ZUSTAND2 =>                 
                if bremsen = '0' then
                    naechst_zustand <= ZUSTAND1;
                elsif bremsen = '1' and blockiert = '0' then
                    naechst_zustand <= ZUSTAND2;
                elsif bremsen = '1' and blockiert = '1' then
                    naechst_zustand <= ZUSTAND3;
                end if;
            when ZUSTAND3 =>
                if bremsen = '0' then
                    naechst_zustand <= ZUSTAND1;
                elsif bremsen = '1' and blockiert = '1' then
                    naechst_zustand <= ZUSTAND3; 
                elsif bremsen = '1' and blockiert = '0' then
                    naechst_zustand <= ZUSTAND2;
                end if;
            when others =>                          -- Reset (Anti-Latch)
                    naechst_zustand <= ZUSTAND1;
        end case;
    end process f;
    
g:  process(akt_zustand)                            -- Ergebnisfunktion        
    begin
        abs_led <= akt_zustand(0);
        bremse_aktiv <= akt_zustand(1);
    end process g;
    
zw: process(clk, einschalten)                       -- Taktung, Reset (asynchron)
    begin
        if einschalten = '1' then
            akt_zustand <= ZUSTAND1;
        elsif clk'event and clk = '1' then
            akt_zustand <= naechst_zustand;
        end if;
    end process zw;

end architecture arch_brems;
