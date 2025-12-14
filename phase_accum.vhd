-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Phase_Accumulator is
    Generic(
		fcw      	: in std_logic_vector(8 downto 0) := "000000001" -- Frequency control word
    );
    Port(
		enable		: in std_logic;
        clk      	: in std_logic;
        reset    	: in std_logic;
        phase_out	: out std_logic_vector(8 downto 0) := "000000000";  -- Accumulated phase output
		ready		: buffer std_logic := '0'
    );
end Phase_Accumulator;

architecture Behavioural of Phase_Accumulator is
	-- Signals
    signal current_phase 	: unsigned(8 downto 0) := "000000000";
	
-- Begin processes
begin
    process(clk, reset) -- need clock divider to run at the sample rate we want
    begin
		if reset = '1' then
			current_phase <= (others => '0');
			ready <= '0';
        elsif rising_edge(clk) then
			if enable = '1' and ready = '0' then
				current_phase <= current_phase + unsigned(fcw); -- add fcw to current count
				ready <= '1';
			elsif enable = '0' then
				ready <= '0';
			else
				null;
			end if;
        end if;
    end process;

    phase_out <= std_logic_vector(current_phase);

end Behavioural;