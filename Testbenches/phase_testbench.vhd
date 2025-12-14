-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_phase_accum is
    -- Not needed for tb
end tb_phase_accum;

architecture test of tb_phase_accum is
    component Phase_Accumulator
		Generic(
			fcw      	: in std_logic_vector(8 downto 0) := "000000001"
		);
		Port(
			enable			: in std_logic := '0';
			clk      	: in std_logic := '0';
			reset    	: in std_logic := '0';
			phase_out	: out std_logic_vector(8 downto 0);  -- Accumulated phase output
			ready		: out std_logic
		);
	end component;
	
	signal enable : std_logic := '0';
	signal reset : std_logic := '0';
	signal clk : std_logic;
	signal phase_out : std_logic_vector(8 downto 0);
	signal ready : std_logic;
	
	constant clk_period : time := 20 ns;
begin
	-- Instantiate the Unit Under Test (UUT)
    uut: Phase_Accumulator
		Generic map(
			fcw => "000000001"
		)
        Port map (
            enable => enable,
			clk => clk,
			reset => reset,
			phase_out => phase_out,
			ready => ready
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
		enable <= '1';
        wait until ready = '1';
		enable <= '0';
		wait for 40 ns;
		
		enable <= '1';
        wait until ready = '1';
		enable <= '0';
		wait for 40 ns;
		
		enable <= '1';
        wait until ready = '1';
		enable <= '0';
		wait for 40 ns;
		
		enable <= '1';
        wait until ready = '1';
		enable <= '0';
		wait for 40 ns;
		
        wait;
    end process stim_proc;
end test;