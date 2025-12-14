-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ==============================
-- ==    Entity Declaration    ==
-- ==============================
entity tb_CIC_Filter is
    -- None
end tb_CIC_Filter;

architecture test of tb_CIC_Filter is
	component CIC_Filter
		Port(
			enable		: in std_logic := '0';
			reset		: in std_logic := '0';
			clk 		: in std_logic := '0';
			sample_in 	: in std_logic_vector(31 downto 0);
			data_out 	: out std_logic_vector(31 downto 0);
			out_ready	: out std_logic
		);
	end component;
	
	signal enable : std_logic := '0';
	signal reset : std_logic := '0';
	signal clk : std_logic;
	signal sample_in : std_logic_vector(31 downto 0);
	signal data_out : std_logic_vector(31 downto 0);
	signal out_ready : std_logic;
	
	constant clk_period : time := 20 ns;
begin
	-- Instantiate the Unit Under Test (UUT)
    uut: CIC_Filter
        Port map (
            enable => enable,
			reset => reset,
			clk => clk,
			sample_in => sample_in,
			data_out => data_out,
			out_ready => out_ready
			);
			
	CLOCK_GEN_PROC: process
    begin
        loop
            clk <= '1';
            wait for CLK_PERIOD/2;
            clk <= '0';
            wait for CLK_PERIOD/2;
        end loop;
    end process CLOCK_GEN_PROC;
	
	-- Stimulus Process
    stim_proc: process
    begin
		sample_in <= "00000000100010101110100110001011";
		wait for 40 ns;
		
        enable <= '1';
        wait until out_ready = '1';
		enable <= '1';
        wait;
    end process stim_proc;

end test;