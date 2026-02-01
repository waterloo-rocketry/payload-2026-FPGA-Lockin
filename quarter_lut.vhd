-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity NCO is
	Generic(
		bits		: integer := 8;
		n_samples	: integer := 512 - 1;
		fcw			: std_logic_vector(8 downto 0) := "000000001"
	);
	Port(
		enable		: in std_logic;											-- Enable flag to avoid overwriting current value
		addr_i		: in std_logic_vector(8 downto 0);						-- Address of memory, used to tell when to switch quadrants
        clk     	: in std_logic;											-- Clock
        reset   	: in std_logic;											-- Reset
        addr_sin	: out std_logic_vector(8 downto 0) := "000000000";		-- New address of memory, transformed and for sin
		neg_sin		: out std_logic := '0';									-- If sin sample should be flipped negative
		addr_cos	: out std_logic_vector(8 downto 0) := "000000000";		-- New address of memory, transformed and for cos
		neg_cos		: out std_logic := '0';									-- If cos sample should be flipped negative
		out_ready	: out std_logic := '0'									-- Flag for done
    );
end NCO;

architecture Behavioural of NCO is
	signal quadrant : std_logic_vector(1 downto 0) := "00";

--
--	BEGIN PROCESS
--
begin
	-- Get sample from LUT
	process(clk, reset)
		-- Index variable from address
		variable idx : integer := 0;
		variable count : std_logic := '0';
	begin
		if rising_edge(clk) then
			if reset = '1' then
				quadrant <= "00";
				neg_cos <= '0';
				neg_sin <= '0';
				addr_sin <= (others => '0');
				addr_cos <= (others => '0');
				out_ready <= '0';
				idx := 0;
			elsif enable = '1' then
				idx := to_integer(unsigned(addr_i));
				
				-- if addr + fcw > sample then it overflows next run (this is a problem)
				if idx = 0 then -- still wrong
					if count = '0' then
						quadrant <= std_logic_vector(unsigned(quadrant) + 1);
						count := '1';
					end if;
				else
					count := '1';
				end if;
				
				if count = '1' then
				-- Quarter look up table, exploits symmetries to save memory
				case quadrant is
					when "00" =>
						neg_sin <= '0';
						addr_sin <= std_logic_vector(to_unsigned(idx, addr_sin'length));
						neg_cos <= '0';
						addr_cos <= std_logic_vector(to_unsigned(n_samples - idx, addr_sin'length));
						out_ready <= '1';
					when "01" =>
						neg_sin <= '0';
						addr_sin <= std_logic_vector(to_unsigned(n_samples - idx, addr_sin'length));
						neg_cos <= '1';
						addr_cos <= std_logic_vector(to_unsigned(idx, addr_sin'length));
						out_ready <= '1';
					when "10" =>
						neg_sin <= '1';
						addr_sin <= std_logic_vector(to_unsigned(idx, addr_sin'length));
						neg_cos <= '1';
						addr_cos <= std_logic_vector(to_unsigned(n_samples - idx, addr_sin'length));
						out_ready <= '1';
					when "11" =>
						neg_sin <= '1';
						addr_sin <= std_logic_vector(to_unsigned(n_samples - idx , addr_sin'length));
						neg_cos <= '0';
						addr_cos <= std_logic_vector(to_unsigned(idx, addr_sin'length));
						out_ready <= '1';
					when others =>
						neg_cos <= '0';
						neg_sin <= '0';
						addr_sin <= (others => '0');
						addr_cos <= (others => '0');
						out_ready <= '0';
				end case;
				end if;
			else
				out_ready <= '0';
				count := '0';
			end if;
		end if;
	end process;
end Behavioural;