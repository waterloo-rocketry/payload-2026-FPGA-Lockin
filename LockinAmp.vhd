-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Library for ICE40 primatives
library ice40up;
use ice40up.all;

-- ==============================
-- ==    Entity Declaration    ==
-- ==============================
entity LockinAmp is
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
		-- clock port (for testing)
		--clk					: in std_logic;
		-- reset port
		reset				: in std_logic
		-- LED ports
		--LED0				: out std_logic;
		--LED1				: out std_logic;
		--LED2				: out std_logic
	);
end LockinAmp;

architecture Structural of LockinAmp is
	-- ==============================
	-- ==   Signal Declarations   ==
	-- ==============================
	
	-- Clock (signal instead of pin since using on chip osc)
	signal clk			: std_logic;
	
	-- State machine
	type machine is(
		wait_smp, start_acc, start_nco, read_sin, read_cos, send_dac, read_adc, mix, filter, export_i, export_q
	);
	signal state    	: machine := start_acc;
	
	--constant clk_freq 			: integer := 48;
	
	-- Phase Accumulator
	signal acc_en		: std_logic := '0';
	signal acc_phase	: std_logic_vector(8 downto 0);
	signal acc_ready	: std_logic;
	
	-- NCO
	signal nco_en		: std_logic := '0';
	signal lut_addr		: std_logic_vector(8 downto 0);
	signal sin_addr		: std_logic_vector(8 downto 0);
	signal cos_addr		: std_logic_vector(8 downto 0);
	signal sin_neg		: std_logic;
	signal cos_neg		: std_logic;
	signal nco_ready	: std_logic;
	
	-- Sin Filter
	signal sin_in		: std_logic_vector(15 downto 0); -- actually 8 bits
	signal sin_en		: std_logic;
	signal sin_mod		: std_logic_vector(23 downto 0);
	signal i_data		: std_logic_vector(31 downto 0);
	signal i_ready		: std_logic;
	
	-- Cos Filter
	signal cos_in		: std_logic_vector(15 downto 0); -- actually 8 bits
	signal cos_en		: std_logic;
	signal cos_mod		: std_logic_vector(23 downto 0);
	signal q_data		: std_logic_vector(31 downto 0);
	signal q_ready		: std_logic;
	
	-- Memory
	signal mem_addr		: std_logic_vector(8 downto 0);
	signal mem_do		: std_logic_vector(15 downto 0);
	
	-- SPI analog
	signal ss_ctrl_an	: std_logic_vector(1 downto 0) := "11";
	signal tx_data_an	: std_logic_vector(15 downto 0);
	signal rx_data_an	: std_logic_vector(15 downto 0);
	signal done_an		: std_logic;
	
	signal sample_in	: std_logic_vector(15 downto 0);
	
	-- SPI logging
	signal ss_ctrl_mem	: std_logic_vector(1 downto 0) := "11";
	signal tx_data_mem	: std_logic_vector(31 downto 0);
	signal rx_data_mem	: std_logic_vector(31 downto 0);
	signal done_mem		: std_logic;
	
	
	-- ==============================
	-- ==  Component Declarations  ==
	-- ==============================
	
	-- High frequency oscillator (TRIM warnings from this)
	-- to avoid using multiple clock domains we should just use counters and dividers.
	component HSOSC
        Generic(
            CLKHF_DIV			: string := "0b00" 	-- "0b00" for 48MHz, "0b01" for 24MHz, "0b10" for 12MHz, "0b11" for 6MHz
        );
        Port(
            CLKHFPU 			: in std_logic;  	-- Power, active high
            CLKHFEN 			: in std_logic;  	-- Enable, active high
            CLKHF  	 			: out std_logic  	-- Output clock signal
        );
    end component;
	
	-- SPI master. uses a soft core for 16+ bit reads
	-- One is dedicated to read and writing to analog, 
	-- the other for reading flash and writing to mcu
	component spi_master is
		Generic(
			CPOL				: in std_logic := '0';
			n_bytes				: in integer := 2;
			clk_div				: in unsigned(3 downto 0) := "0010"
		);
		Port(
			clk					: in std_logic;
		
			-- SPI ports
			miso				: in std_logic;
			mosi 				: out std_logic := 'Z';
			sclk 				: out std_logic := CPOL;
			ss	 				: out std_logic_vector(1 downto 0);
		
			-- Control ports
			ss_ctrl				: in std_logic_vector(1 downto 0);
			tx_data				: in std_logic_vector(((8*n_bytes) - 1) downto 0);
			rx_data				: out std_logic_vector(((8*n_bytes) - 1) downto 0) := (others => '0');
			done				: out std_logic := '0';

			-- reset port
			reset				: in std_logic
		);
	end component;
	
	
	-- Lowpass filters
	component CIC_Filter is
		Port(
			enable				: in std_logic;
			reset				: in std_logic;
			clk 				: in std_logic;
			sample_in 			: in std_logic_vector(23 downto 0);
			data_out 			: out std_logic_vector(31 downto 0) := (others => '0');
			out_ready			: out std_logic := '0'
		);
	end component;
	
	-- BRAM ROM with LUT
	component lut_rom is
    Port(
        rd_clk_i				: in std_logic;
        rst_i					: in std_logic;
        rd_en_i					: in std_logic;
        rd_clk_en_i				: in std_logic;
        rd_addr_i				: in std_logic_vector(8 downto 0);
        rd_data_o				: out std_logic_vector(15 downto 0)
    );
	end component;
	
	-- Phase accumulator for NCO
	component Phase_Accumulator is
		Generic(
			fcw      			: in std_logic_vector(8 downto 0) := "000000001" -- Frequency control word
		);
		Port(
			enable				: in std_logic;
			clk      			: in std_logic;
			reset    			: in std_logic;
			phase_out			: out std_logic_vector(8 downto 0) := "000000000";  -- Accumulated phase output
			ready				: out std_logic := '0'
		);
	end component;
	
	-- Quarter-LUT-based NCO
	component NCO is
		Generic(
		bits				: integer := 8;
		n_samples			: integer := 512 - 1;
		fcw					: std_logic_vector(8 downto 0) := "000000001"
	);
	Port(
		enable				: in std_logic;											-- Enable flag to avoid overwriting current value
		addr_i				: in std_logic_vector(8 downto 0);						-- Address of memory, used to tell when to switch quadrants
        clk     			: in std_logic;											-- Clock
        reset   			: in std_logic;											-- Reset
        addr_sin			: out std_logic_vector(8 downto 0) := "000000000";		-- New address of memory, transformed and for sin
		neg_sin				: out std_logic := '0';									-- If sin sample should be flipped negative
		addr_cos			: out std_logic_vector(8 downto 0) := "000000000";		-- New address of memory, transformed and for cos
		neg_cos				: out std_logic := '0';									-- If cos sample should be flipped negative
		out_ready			: out std_logic := '0'									-- Flag for done
	);
	end component;

-- ============================
-- ==  	Begin Processes	     ==
-- ============================
begin
	-- ==============================
	-- ==  Instantiate Components  ==
	-- ==============================
	
	-- Instantiate high frequency oscillator (~48 MHz)
    HOSC : HSOSC
       Port map(CLKHFPU => '1', CLKHFEN => '1', CLKHF => clk);
	
	-- Instantiate phase accumulator
	accum : Phase_Accumulator
        Port map(enable => acc_en, clk => clk, reset => reset, phase_out => acc_phase, ready => acc_ready);
	
	-- Instantiate sine NCO
	lut_nco: NCO
		Port map(enable => nco_en, addr_i => lut_addr, clk => clk, reset => reset, addr_sin => sin_addr, neg_sin => sin_neg, 
			addr_cos => cos_addr, neg_cos => cos_neg, out_ready => nco_ready);
		
	-- Instantiate lowpass filter for I
	sin_cic: CIC_Filter
		Port map(enable => sin_en, reset => reset, sample_in => sin_mod, data_out => i_data, clk => clk, out_ready => i_ready);
		
	-- Instantiate lowpass filter for Q
	cos_cic: CIC_Filter
		Port map(enable => cos_en, reset => reset, sample_in => cos_mod, data_out => q_data, clk => clk, out_ready => q_ready);
	
	-- Instantiate LUT ROM
	LUT: lut_rom 
		Port map(rd_clk_i => clk, rst_i => reset, rd_en_i => '1', rd_clk_en_i => '1', rd_addr_i => mem_addr, rd_data_o => mem_do);
		
	-- Instantiate SPI for sampling ADC
	spi_an: spi_master
		Generic map(CPOL => '0', n_bytes => 2, clk_div => "0010")
		Port map(clk => clk, miso => miso_an, mosi => mosi_an, sclk => sclk_an, ss => ss_an, ss_ctrl => ss_ctrl_an, tx_data => tx_data_an, 
			rx_data => rx_data_an, done => done_an, reset => reset);

	-- Instantiate SPI for output and configuration
	spi_mem: spi_master
		Generic map(CPOL => '0', n_bytes => 4, clk_div => "0010")
		Port map(clk => clk, miso => miso_mem, mosi => mosi_mem, sclk => sclk_mem, ss => ss_mem, ss_ctrl => ss_ctrl_mem, tx_data => tx_data_mem,
			rx_data => rx_data_mem, done => done_mem, reset => reset);
	
	process(clk, reset)
		variable counter 		: integer := 0;
		variable wait_counter	: integer := 0;
	begin
		-- ==============================
		-- ==       State Machine      ==
		-- ==============================
		-- can be parallelized in the future
		if(reset = '1') then
			state <= start_acc;
		elsif(rising_edge(clk)) then
			-- ==============================
			-- ==          Reset           ==
			-- ==============================
			case state is
				-- ==============================
				-- ==      Wait for sample     ==
				-- ==============================
				when wait_smp =>
					-- 1kHz ish read rate
					if wait_counter = 48000 then
						state <= start_acc;
						wait_counter := 0;
					else
						-- increment counter
						wait_counter := wait_counter + 1;
					end if;
				
				-- ==============================
				-- ==      Start Accum         ==
				-- ==============================
				when start_acc =>
					acc_en <= '1';
					
					-- Turn off phase accumulator when done
					if acc_ready = '1' then
						acc_en <= '0';
						-- use phase to find sine wave sample in the ram look up table
						lut_addr <= acc_phase;
						state <= start_nco;
					end if;
					
					
				-- ==============================
				-- ==        Start NCO         ==
				-- ==============================
				when start_nco =>
					nco_en <= '1';
					
					-- Turn off oscillator when sample is done
					if nco_ready = '1' then
						nco_en <= '0';
						-- use corrected address from nco to find what sample to use for sine
						mem_addr <= sin_addr;
						state <= read_sin;
					end if;
					
				-- ==============================
				-- ==         Read Sin         ==
				-- ==============================
				when read_sin => -- 2 clock cycles to read?
					-- done state. set up to read cosine
					if counter = 2 then
						counter := 0;
						state <= read_cos;
					else
						-- if not done, increment counter to know when done.
						mem_addr <= sin_addr;
						counter := counter + 1;
					end if;
					
				-- ==============================
				-- ==         Read Cos         ==
				-- ==============================
				when read_cos =>
					-- done state, set up tx data for dac send
					if counter = 2 then
						counter := 0;
						tx_data_an <= sin_in;
						state <= send_dac;
					else
						-- read the sin on the first iteration of this loop.
						if sin_neg = '1' then
							sin_in <= std_logic_vector(-signed(mem_do));
						else
							sin_in <= mem_do;
						end if;
						
						-- start cos read
						mem_addr <= cos_addr;
						counter := counter + 1;
					end if;
        
				-- ==============================
				-- ==        Bias Out          ==
				-- ==============================
				when send_dac =>
					-- read the cosine on the first iteration of this loop.
					if cos_neg = '1' then
						cos_in <= std_logic_vector(-signed(mem_do));
					else
						cos_in <= mem_do;
					end if;
					
					-- pull chip select to dac low
					ss_ctrl_an <= "10";
					
					-- when done sending to dac
					if done_an = '1' then
						-- reset chip select
						ss_ctrl_an <= "11";
						-- set up adc sample command
						tx_data_an <= "0000000000000000"; -- adc sample command
						state <= read_adc;
					end if;
						
					
				-- ==============================
				-- ==        Sample In         ==
				-- ==============================
				when read_adc =>
					-- pull adc chip select low
					ss_ctrl_an <= "01";
					
					-- when done reading
					if done_an = '1' then
						-- reset chip select
						ss_ctrl_an <= "11";
						-- save read data
						sample_in <= rx_data_an;
						state <= mix;
					end if;
					
					
				-- ==============================
				-- ==          Mix             ==
				-- ==============================
				when mix =>
					-- multiply by cos and sin to start demodulation
					sin_mod <= std_logic_vector(signed(sample_in) * resize(signed(sin_in), 8));
					cos_mod <= std_logic_vector(signed(sample_in) * resize(signed(cos_in), 8));
					
					state <= filter;
					
				-- ==============================
				-- ==         Filter           ==
				-- ==============================
				when filter =>
					sin_en <= '1';
					cos_en <= '1';
					
					-- read lowpassed modulated sin
					if i_ready = '1' then
						sin_en <= '0';
						counter := counter + 1;
					end if;
					
					-- read lowpassed modulated cos
					if q_ready = '1' then
						cos_en <= '0';
						counter := counter + 1;
					end if;
					
					-- set up for exporting i and q
					if counter = 2 then
						tx_data_mem <= i_data;
						state <= export_i;
					end if;
				
				-- ==============================
				-- ==         CORDIC?          ==
				-- ==============================
	
				-- convert x and y to r and theta?
					
				-- ==============================
				-- ==         Sin Out          ==
				-- ==============================
				when export_i =>
					ss_ctrl_mem <= "10";
					
					if done_mem = '1' then
						ss_ctrl_mem <= "11";
						tx_data_mem <= q_data;
						state <= export_q;
					end if;
        
				when export_q =>
					ss_ctrl_mem <= "10";
					
					if done_mem = '1' then
						ss_ctrl_mem <= "11";
						state <= wait_smp;
					end if;
					
				
				-- ==============================
				-- ==         Default          ==
				-- ==============================
				--default to start state
				when others => 
					state <= start_acc;
			end case;
		end if;
	end process;
end Structural;
	
	