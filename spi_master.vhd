-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ==============================
-- ==    Entity Declaration    ==
-- ==============================
entity spi_master is
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
		sclk 				: buffer std_logic := CPOL;
		ss	 				: out std_logic_vector(1 downto 0) := "11";
		
		-- Control ports
		ss_ctrl				: in std_logic_vector(1 downto 0);
		tx_data				: in std_logic_vector(((8*n_bytes) - 1) downto 0);
		rx_data				: out std_logic_vector(((8*n_bytes) - 1) downto 0) := (others => '0');
		done				: buffer std_logic := '0';

		-- reset port
		reset				: in std_logic
	);
end spi_master;

architecture Behavioural of spi_master is
	signal count : unsigned(7 downto 0) := (others => '0');
	constant max : integer := (8*n_bytes);
	signal soft_reset : std_logic := '0';
	
begin
	-- Clock divider
	process(clk, reset)
		variable clk_count : unsigned(1 downto 0) := "00";
	begin
		if reset = '1' then
			done <= '0';
			ss <= "11";
			count <= (others => '0');
			clk_count := "00";
		
		elsif rising_edge(clk) then
			ss <= ss_ctrl;
			
			if to_integer(count) = max then
				done <= '1';
				count <= (others => '0');
				sclk <= CPOL;
				clk_count := "00";
				--ss <= "11";
			else
				done <= '0';
		
				case ss_ctrl is
					when "10" | "01" =>
						soft_reset <= '0';
					
						-- SCLK Generator
						clk_count := clk_count + 1;
			
						if clk_count = clk_div then
							count <= count + 1;
							sclk <= not CPOL;
							clk_count := "00";
						else
							sclk <= CPOL;
						end if;
					
					when others =>
						sclk <= CPOL;
						--ss <= "11";
						clk_count := "00";
						count <= (others => '0');
						soft_reset <= '1';
				
				end case;
			end if;	
		end if;
	end process;

	-- Send and receive
	process(clk, sclk, reset, soft_reset)
	begin
		if reset = '1' then
			rx_data <= (others => '0');
			mosi <= 'Z';
		elsif soft_reset = '1' then
			rx_data <= (others => '0');
			mosi <= 'Z';
		elsif rising_edge(sclk) then
			case ss_ctrl is
				when "10" | "01" =>
					if done = '0' then
						-- MOSI/MISO Data handling
						mosi <= tx_data(to_integer(count) - 1);
						rx_data(to_integer(count)- 1) <= miso;
					end if;
                
				when others =>
					mosi <= 'Z';
					rx_data <= (others => '0');
                
			end case;
		end if;
	end process;
end Behavioural;