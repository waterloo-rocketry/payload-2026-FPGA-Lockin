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
		sclk_an 			: buffer std_logic;
		ss_an 				: buffer std_logic_vector(1 downto 0);
		-- MCU/SDROM SPI ports
		miso_mem			: in std_logic;
		mosi_mem 			: out std_logic;
		sclk_mem			: buffer std_logic;
		ss_mem				: buffer std_logic_vector(1 downto 0);
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
	signal clk					: std_logic;
	
	-- State machine
	type machine is(
		start_lut, start_nco, read_sin, read_cos,
		start_dac, pause_dac, ready_dac, send_data_dac, read_data_adc, 
		mix, filter, pause_export_sin, ready_export_sin, export_sin,
		pause_export_cos, ready_export_cos, export_cos
	);
	signal state         		: machine := start_lut;
	
	-- Memory
	signal addr_mem				: std_logic_vector(8 downto 0) := "000000000";
	signal do_mem				: std_logic_vector(15 downto 0);
	
	
	-- SPI in
	constant clk_freq			: integer := 48;
	signal spi_ena_an			: std_logic := '0';
	signal spi_tx_data_an		: std_logic_vector(15 downto 0);
	signal spi_busy_an			: std_logic;
	signal spi_rx_data_an		: std_logic_vector(15 downto 0);
	signal spi_busy_prev_an		: std_logic;
	signal adc_sample			: std_logic_vector(15 downto 0);
	signal slv_addr_an			: integer; -- slave address, 0 for dac, 1 for adc
	
	-- NCO
	constant fcw				: std_logic_vector(7 downto 0) := "00000001";
	signal acc_en				: std_logic := '0';
	signal acc_ready			: std_logic := '0';
	signal acc_phase			: std_logic_vector(8 downto 0) := "000000000";
	signal lut_addr				: std_logic_vector(8 downto 0) := "000000000";
	signal nco_en 				: std_logic := '0';
	signal addr_sin				: std_logic_vector(8 downto 0) := "000000000";
	signal neg_sin 				: std_logic := '0';
	signal addr_cos				: std_logic_vector(8 downto 0) := "000000000";
	signal neg_cos 				: std_logic := '0';
	signal nco_ready			: std_logic := '0';
	signal sin_in				: std_logic_vector(15 downto 0);
	signal cos_in				: std_logic_vector(15 downto 0);
	
	-- Input to filters 
	signal sin_en				: std_logic := '0';
	signal cos_en				: std_logic := '0';
	signal sin_mod_data			: std_logic_vector(31 downto 0);
	signal cos_mod_data			: std_logic_vector(31 downto 0);
	
	-- Filter out
	signal i_data				: std_logic_vector(31 downto 0);
	signal q_data				: std_logic_vector(31 downto 0);
	signal i_out				: std_logic_vector(31 downto 0);
	signal q_out				: std_logic_vector(31 downto 0);
	signal write_ready_sin		: std_logic := '0';
	signal write_ready_cos		: std_logic := '0';
	
	-- CORDIC? we can probably post process from just x and y
	
	-- SPI out
	signal spi_ena_mem			: std_logic := '0';
	signal spi_tx_data_mem		: std_logic_vector(31 downto 0);
	signal spi_busy_mem			: std_logic;
	signal spi_rx_data_mem		: std_logic_vector(31 downto 0);
	signal spi_busy_prev_mem	: std_logic;
	signal slv_addr_mem			: integer; -- slave address, 0 for flash, 1 for mcu
	
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
	
	-- SPI master from IP. uses a soft core for 16+ bit reads
	-- One is dedicated to read and writing to analog, 
	-- the other for reading flash and writing to mcu
	component spi_master is
		Generic(
			slaves				: integer := 2;		--number of spi slaves
			d_width				: integer := 16 	--data bus width
		); 
		Port(
			clock   			: in std_logic;									--system clock
			reset_n 			: in std_logic;									--asynchronous reset
			enable  			: in std_logic;									--initiate transaction
			cpol    			: in std_logic;									--spi clock polarity
			cpha    			: in std_logic;									--spi clock phase
			cont    			: in std_logic;									--continuous mode command
			clk_div 			: in integer;									--system clock cycles per 1/2 period of sclk
			addr    			: in integer;									--address of slave
			tx_data 			: in std_logic_vector(d_width-1 downto 0);		--data to transmit
			miso    			: in std_logic;									--master in, slave out
			sclk    			: buffer std_logic;                             --spi clock
			ss_n    			: buffer std_logic_vector(slaves-1 downto 0);   --slave select
			mosi    			: out std_logic;								--master out, slave in
			busy    			: out std_logic;								--busy / data ready signal
			rx_data 			: out std_logic_vector(d_width-1 downto 0)		--data received
		); 
	end component;
	
	-- Lowpass filters
	component CIC_Filter is
		Port(
			enable				: in std_logic;
			reset				: in std_logic;
			clk 				: in std_logic;
			sample_in 			: in std_logic_vector(31 downto 0);
			data_out 			: out std_logic_vector(31 downto 0);
			out_ready			: out std_logic
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
			fcw					: std_logic_vector(8 downto 0) := "000000001"
		);
		Port(
			en					: in std_logic;
			clk      			: in std_logic;
			reset    			: in std_logic;
			phase_out			: out std_logic_vector(8 downto 0);  -- Accumulated phase output
			ready				: out std_logic
		);
	end component;
	
	-- Quarter-LUT-based NCO
	component NCO is
		Generic(
			bits		: integer := 8;
			n_samples	: integer := (2**8) - 1;
			fcw			: std_logic_vector(8 downto 0) := "000000001"
		);
		Port(
			en			: in std_logic;							-- Enable flag to avoid overwriting current value
			addr_i		: in std_logic_vector(8 downto 0);		-- Address of memory, used to tell when to switch quadrants
			clk     	: in std_logic;							-- Clock
			reset   	: in std_logic;							-- Reset
			addr_sin	: out std_logic_vector(8 downto 0);	-- New address of memory, transformed and for sin
			neg_sin		: out std_logic;						-- If sin sample should be flipped negative
			addr_cos	: out std_logic_vector(8 downto 0);	-- New address of memory, transformed and for cos
			neg_cos		: out std_logic;						-- If cos sample should be flipped negative
			out_ready	: out std_logic							-- Flag for done
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
        Port map(en => acc_en, clk => clk, reset => reset, phase_out => acc_phase, ready => acc_ready);
	
	-- Instantiate sine NCO
	lut_nco: NCO
		Port map(en => nco_en, addr_i => lut_addr, clk => clk, reset => reset, addr_sin => addr_sin, neg_sin => neg_sin, 
			addr_cos => addr_cos, neg_cos => neg_cos, out_ready => nco_ready);
		
	-- Instantiate lowpass filter for I
	sin_cic: CIC_Filter
		Port map(enable => sin_en, reset => reset, sample_in => sin_mod_data, data_out => i_data, clk => clk, out_ready => write_ready_sin);
		
	-- Instantiate lowpass filter for Q
	cos_cic: CIC_Filter
		Port map(enable => cos_en, reset => reset, sample_in => cos_mod_data, data_out => q_data, clk => clk, out_ready => write_ready_cos);
	
	-- Instantiate LUT ROM
	LUT: lut_rom 
		Port map(
			rd_clk_i => clk,
			rst_i => reset,
			rd_en_i => '1',
			rd_clk_en_i => '1',
			rd_addr_i => addr_mem,
			rd_data_o => do_mem
		);
		
	-- Instantiate SPI for sampling ADC, change to soft core
	SPI_an: spi_master
		Generic map(slaves => 2, d_width => 16)
		Port map(clock => clk, reset_n => reset, enable => spi_ena_an, cpol => '1', cpha => '1',
			cont => '0', clk_div => clk_freq/10, addr => slv_addr_an, tx_data => spi_tx_data_an, miso => miso_an,
			sclk => sclk_an, ss_n => ss_an, mosi => mosi_an, busy => spi_busy_an, rx_data => spi_rx_data_an);

	-- Instantiate SPI for output and configuration
	SPI_mem: spi_master
		Generic map(slaves => 2, d_width => 32)
		Port map(clock => clk, reset_n => reset, enable => spi_ena_mem, cpol => '1', cpha => '1',
			cont => '0', clk_div => clk_freq/10, addr => slv_addr_mem, tx_data => spi_tx_data_mem, miso => miso_mem,
			sclk => sclk_mem, ss_n => ss_mem, mosi => mosi_mem, busy => spi_busy_mem, rx_data => spi_rx_data_mem);
	
	process(clk, reset) --update times and stuff. maybe also need to configure the dac and adc here or if non volatile do with python, add nco in states
		variable count_an 				: integer range 0 to clk_freq*100; 	--counter
		variable count_mem 				: integer range 0 to clk_freq*100; 	--counter
		variable current_write			: integer := 0;
		variable spi_busy_int_an 		: std_logic; -- had to add to avoid pissing off synthesizer
		variable spi_busy_int_mem 		: std_logic; -- had to add to avoid pissing off synthesizer
	begin
		-- ==============================
		-- ==       State Machine      ==
		-- ==============================
		
		-- ==============================
		-- ==          Reset           ==
		-- ==============================
		if(reset = '1') then
			-- SPI to analog
			spi_ena_an <= '0';							--clear spi component enable
			spi_tx_data_an <= (others => '0');			--clear spi component transmit data
			spi_busy_int_an := '1';						--indication component is unavailable
			
			-- SPI to memory
			spi_ena_mem <= '0';							--clear spi component enable
			spi_tx_data_mem <= (others => '0');			--clear spi component transmit data
			spi_busy_int_mem := '1';					--indication component is unavailable
			
			state <= start_lut;                     	--restart state machine
		
		elsif(rising_edge(clk)) then 					--fix times for this clock
			spi_busy_prev_an <= spi_busy_an;			--collect previous spi_busy
			spi_busy_prev_mem <= spi_busy_mem;			--collect previous spi_busy
				
			case state is
				when start_lut =>
					acc_en <= '1';
					state <= start_nco;
					
				when start_nco =>
					if acc_ready = '1' then
						acc_en <= '0';
						lut_addr <= acc_phase;
						nco_en <= '1';
						state <= read_sin;
					end if;
					
				-- ==============================
				-- ==         Read LUT         ==
				-- ==============================
				when read_sin =>
					if nco_ready = '1' then
						addr_mem <= addr_sin;
						nco_en <= '0';
						
						-- add count wait
						if(count_mem < clk_freq*100) then    --100us not yet reached
							count_mem := count_mem + 1;       --increment counter
						else                             	--100us reached
							count_mem := 0;  
						
							if neg_sin = '0' then
								sin_in <= std_logic_vector(-signed(do_mem));
							else
								sin_in <= do_mem;
							end if;
						end if;
						state <= read_cos;
					end if;
						
				when read_cos =>
					addr_mem <= addr_cos;
					
					if(count_mem < clk_freq*100) then    --100us not yet reached
						count_mem := count_mem + 1;       --increment counter
					else                             	--100us reached
						count_mem := 0;  
				
						if neg_cos = '0' then 
							cos_in <= do_mem;
						else
							cos_in <= std_logic_vector(-signed(do_mem));
						end if;
					end if;

					state <= start_dac;
						
				-- ==============================
				-- ==    Start up and Wait     ==
				-- ==============================
				--entry state, give dac 100us to power up before communicating
				when start_dac => -- stuck
					spi_busy_int_an := '1';             --component is busy, dac not yet available
					if(count_an < clk_freq) then    --100us not yet reached
						count_an := count_an + 1;       --increment counter
					else                             	--100us reached
						count_an := 0;                  --clear counter
						state <= pause_dac;             --advance to configure the dac
					end if;

				--pauses 20ns between spi transactions
				when pause_dac =>
					if(count_an < clk_freq/50) then   	--less than 20ns
						count_an := count_an + 1;		--increment counters
					else                           		--20ns has elapsed
						count_an := 0;                  --clear counter
						spi_busy_int_an := '0';         --indicate component is ready for a transaction
						slv_addr_an <= 0;				--indicate dac address
						spi_ena_an <= '1';
						state <= ready_dac;             --advance to ready state 
					end if;
        
				-- ==============================
				-- ==        Bias Out          ==
				-- ==============================
				when ready_dac => -- stuck?
					if(spi_ena_an = '1') then      		--transaction to dac requested
						spi_tx_data_an <= sin_in;       --latch in data stream to send
						spi_busy_int_an := '0';         --indicate transaction is in progress
						state <= send_data_dac;         --advance to sending transaction
					end if;

				--performs spi transaction to dac  
				when send_data_dac =>
					if(spi_busy_an = '0' and spi_busy_prev_an = '0' and spi_busy_int_an = '0') then  	--transaction not started
						spi_ena_an <= '1';                                 								--enable spi transaction
					elsif(spi_busy_an = '1' or spi_busy_int_an = '1') then                       		--transaction underway
						spi_ena_an <= '0';																--clear enable                            
					else                                             									--transaction complete
						slv_addr_an <= 1;																--indicate adc address
						state <= read_data_adc;															--go to read adc
					end if;
					
				-- ==============================
				-- ==        Sample In         ==
				-- ==============================
				when read_data_adc =>
					if(spi_busy_an = '0' and spi_busy_int_an = '0') then		--serial transaction with adc not in process
						if(count_an < clk_freq/20-2) then						--wait at least 50ns between serial transactions
							count_an := count_an + 1;							--increment clock counter
							spi_ena_an <= '0';									--do not enable serial transaction
						else													--50ns wait time met
							spi_ena_an <= '1';									--enable next serial transaction to get data
						end if;
					else														--serial transaction with adc in process
						count_an := 0;											--clear clock counter
						spi_ena_an <= '0';										--clear enable signal for next transaction
						adc_sample <= spi_rx_data_an(15 downto 0);				--assign channel 0 adc data bits to output
						state <= mix;											--return to beginning of loop							
					end if;
					
				-- ==============================
				-- ==          Mix             ==
				-- ==============================
				when mix =>
					sin_mod_data <= std_logic_vector(signed(sin_in) * signed(adc_sample));
					cos_mod_data <= std_logic_vector(signed(cos_in) * signed(adc_sample));
					sin_en <= '1';
					cos_en <= '1';
					state <= filter;
					
				-- ==============================
				-- ==         Filter           ==
				-- ==============================
				when filter =>
					if write_ready_sin = '1' and write_ready_cos = '1' then
						sin_en <= '0';
						cos_en <= '0';
						i_out <= i_data;
						q_out <= q_data;
						state <= pause_export_sin;
					end if;
				
				-- ==============================
				-- ==         CORDIC?          ==
				-- ==============================
	
				-- convert x and y to r and theta?
					
				-- ==============================
				-- ==         Sin Out          ==
				-- ==============================
				when pause_export_sin =>
					if(count_mem < clk_freq/50) then   	--less than 20ns
						count_mem := count_mem + 1;		--increment counter
					else                           		--20ns has elapsed
						count_mem := 0;                  --clear counter
						spi_busy_int_mem := '0';         --indicate component is ready for a transaction
						slv_addr_mem <= 1;				--indicate dac address
						state <= ready_export_sin;             --advance to ready state 
					end if;
        
				when ready_export_sin =>
					if(spi_ena_mem = '1') then      		--transaction to mcu requested
						spi_tx_data_mem <= i_out;       --latch in data stream to send
						spi_busy_int_mem := '0';         --indicate transaction is in progress
						state <= export_sin;         --advance to sending transaction
					end if;

				--performs spi transaction to mcu  
				when export_sin =>
					if(spi_busy_mem = '0' and spi_busy_prev_mem = '0' and spi_busy_int_mem = '0') then  	--transaction not started
						spi_ena_mem <= '1';                                 								--enable spi transaction
					elsif(spi_busy_mem = '1' or spi_busy_int_mem = '1') then                       		--transaction underway
						spi_ena_mem <= '0';																--clear enable                            
					else                                             									--transaction complete
						state <= pause_export_cos;															--go to read adc
					end if;
						
				-- ==============================
				-- ==         Cos Out          ==
				-- ==============================
				when pause_export_cos =>
					if(count_mem < clk_freq/50) then   	--less than 20ns
						count_mem := count_mem + 1;		--increment counter
					else                           		--20ns has elapsed
						count_mem := 0;                  --clear counter
						spi_busy_int_mem := '0';         --indicate component is ready for a transaction
						slv_addr_mem <= 0;				--indicate dac address
						state <= ready_export_cos;             --advance to ready state 
					end if;
						
				when ready_export_cos =>
					if(spi_ena_mem = '1') then      		--transaction to dac requested
						spi_tx_data_mem <= q_out;       --latch in data stream to send
						spi_busy_int_mem := '0';         --indicate transaction is in progress
						state <= export_cos;         --advance to sending transaction
					end if;

				--performs spi transaction to dac  
				when export_cos =>
					if(spi_busy_mem = '0' and spi_busy_prev_mem = '0' and spi_busy_int_mem = '0') then  	--transaction not started
						spi_ena_mem <= '1';                                 								--enable spi transaction
					elsif(spi_busy_mem = '1' or spi_busy_int_mem = '1') then                       		--transaction underway
						spi_ena_mem <= '0';																--clear enable                            
					else                                             									--transaction complete
						acc_en <= '1';
						state <= read_sin;
					end if;
					
				
				-- ==============================
				-- ==         Default          ==
				-- ==============================
				--default to start state
				when others => 
					state <= start_dac;
			end case;
		end if;
	end process;
end Structural;
	
	