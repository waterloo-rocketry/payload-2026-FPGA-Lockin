-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ==============================
-- ==    Entity Declaration    ==
-- ==============================
entity tb_NCO is
    -- None
end tb_NCO;

architecture test of tb_NCO is
	component NCO is
		Generic(
			bits		: integer := 8;
			n_samples	: integer := 512 - 1;
			fcw			: std_logic_vector(8 downto 0) := "000000001"
		);
		Port(
			enable		: in std_logic;
			clk     	: in std_logic;
			reset   	: in std_logic;	
			addr_i		: in std_logic_vector(8 downto 0);
			addr_sin	: out std_logic_vector(8 downto 0) := "000000000";
			neg_sin		: out std_logic := '0';	
			addr_cos	: out std_logic_vector(8 downto 0) := "000000000";
			neg_cos		: out std_logic := '0';
			out_ready	: out std_logic := '0'
		);
	end component;
	
	signal enable : std_logic := '0';
	signal reset : std_logic := '0';
	signal clk : std_logic := '0';
	signal addr_i : std_logic_vector(8 downto 0) := "111111100";
    signal addr_sin	: std_logic_vector(8 downto 0);
	signal neg_sin : std_logic;	
	signal addr_cos	: std_logic_vector(8 downto 0);
	signal neg_cos : std_logic;
	signal out_ready : std_logic;
	
	constant clk_period : time := 20 ns;
begin
	uut: NCO
		Generic map(
			bits => 8,
			n_samples => 512 - 1,
			fcw	=> "000000001"
		)
        Port map(
            enable => en,
			clk => clk,
			reset => reset,	
			addr_i => addr_i,
			addr_sin => addr_sin,
			neg_sin	=> neg_sin,
			addr_cos => addr_cos,
			neg_cos	=> neg_cos,
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
		loop
            addr_i <= std_logic_vector(unsigned(addr_i) + 1);
			wait for 40 ns;
		
			enable <= '1';
			wait until out_ready = '1';
			enable <= '0';
			wait for 40 ns;
        end loop;
		
		
        wait;
    end process stim_proc;
end test;