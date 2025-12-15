-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_lockin is
    -- Not needed for tb
end tb_lockin;

architecture test of tb_lockin is
    component LockinAmp is
		Port(
			-- ADC/DAC SPI ports
			miso_an				: in std_logic;
			mosi_an 			: out std_logic;
			sclk_an 			: out std_logic;
			ss_an 				: out std_logic_vector(1 downto 0);
			-- MCU/SDROM SPI ports
			miso_mem			: in std_logic;
			mosi_mem 			: out std_logic;
			sclk_mem			: out std_logic;
			ss_mem				: out std_logic_vector(1 downto 0);
			
			--clk					: in std_logic;
			
			-- reset port
			reset				: in std_logic
		);
    end component;

    -- Signals
    --signal clk 		: std_logic;
    signal miso_an	: std_logic;
    signal mosi_an 	: std_logic;
	signal sclk_an 	: std_logic;
	signal ss_an 	: std_logic_vector(1 downto 0);
	signal miso_mem	: std_logic;
    signal mosi_mem	: std_logic;
	signal sclk_mem : std_logic;
	signal ss_mem 	: std_logic_vector(1 downto 0);
	signal reset : std_logic := '0';
	
	constant clk_period : time := 20 ns;

begin
    -- Instantiate the Unit Under Test (UUT)
    uut: LockinAmp
        Port map (
			--clk => clk,
			miso_an => '1',
			mosi_an => mosi_an,
			sclk_an => sclk_an,
			ss_an => ss_an,
			miso_mem => '1',
			mosi_mem => mosi_mem,
			sclk_mem => sclk_mem,
			ss_mem => ss_mem,
			reset => reset
			);
			
	--CLOCK_GEN_PROC: process
    --begin
    --   loop
    --        clk <= '1';
    --        wait for CLK_PERIOD/2;
    --        clk <= '0';
    --        wait for CLK_PERIOD/2;
    --    end loop;
    --end process CLOCK_GEN_PROC;

    -- Stimulus Process
    stim_proc: process
    begin

        -- End of test
        wait;
    end process stim_proc;

end test;