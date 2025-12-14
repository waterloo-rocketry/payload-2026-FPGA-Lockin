-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_spi_master is
    -- Not needed for tb
end tb_spi_master;

architecture test of tb_spi_master is
    component spi_master
		Generic(
			CPOL				: in std_logic := '0';
			n_bytes				: in integer := 2;
			clk_div				: in unsigned(3 downto 0) := "0010"
		);
        Port(
			clk					: in std_logic;
		
			-- SPI ports
			miso				: in std_logic;
			mosi 				: out std_logic;
			sclk 				: out std_logic;
			ss	 				: out std_logic_vector(1 downto 0);
			
			-- Control ports
			ss_ctrl				: in std_logic_vector(1 downto 0);
			tx_data				: in std_logic_vector(15 downto 0);
			rx_data				: out std_logic_vector(15 downto 0);
			done				: out std_logic := '0';

			-- reset port
			reset				: in std_logic := '0'
	);
    end component;

    -- Signals
    signal clk : std_logic;
    signal miso : std_logic;
    signal mosi : std_logic;
	signal sclk : std_logic;
	signal ss 	: std_logic_vector(1 downto 0);
	signal ss_ctrl : std_logic_vector(1 downto 0) := "11";
	signal tx_data : std_logic_vector(15 downto 0) := "0001100000011000";
	signal rx_data : std_logic_vector(15 downto 0);
	signal done : std_logic;
	signal reset : std_logic;
	
	constant clk_period : time := 20 ns;

begin
    -- Instantiate the Unit Under Test (UUT)
    uut: spi_master
        Port map (
            clk => clk,
			miso => miso,
			mosi => mosi,
			sclk => sclk,
			ss => ss,
			ss_ctrl => ss_ctrl,
			tx_data => tx_data,
			rx_data => rx_data,
			done => done,
			reset => reset
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
		ss_ctrl <= "11";
		wait for 40 ns;
		
        -- SS test 1:
        miso <= '1';
		ss_ctrl <= "10";
        wait until done = '1';
		ss_ctrl <= "11";
		wait for 40 ns;

        -- SS test 1:
		miso <= '0';
        ss_ctrl <= "01";
        wait until done = '1';
		ss_ctrl <= "11";
		wait for 40 ns;

        -- SS test 1:
        ss_ctrl <= "00";
        wait for 40 ns;

        -- SS test 1:
        ss_ctrl <= "11";
        wait for 40 ns;

        -- End of test
        wait;
    end process stim_proc;

end test;