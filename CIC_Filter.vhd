-- Library and Use statements for IEEE packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ==============================
-- ==    Entity Declaration    ==
-- ==============================
entity CIC_Filter is
    Port(
		enable		: in std_logic;
		reset		: in std_logic;
        clk 		: in std_logic;
        sample_in 	: in std_logic_vector(23 downto 0);
        data_out 	: out std_logic_vector(31 downto 0) := (others => '0');
		out_ready	: out std_logic := '0'
    );
end CIC_Filter;

architecture Behavioural of CIC_Filter is
	type delay_array_t     is array (0 to 31) of signed(23 downto 0);
	--type sum32_array_t     is array (0 to 31) of signed(31 downto 0);
	type sum16_array_t     is array (0 to 15) of signed(24 downto 0); 
    type sum8_array_t      is array (0 to 7)  of signed(25 downto 0); 
    type sum4_array_t      is array (0 to 3)  of signed(26 downto 0); 
    type sum2_array_t      is array (0 to 1)  of signed(27 downto 0); 
	
	signal delay_line : delay_array_t := (others => (others => '0'));
	--signal sum_stage0 : sum32_array_t := (others => (others => '0'));
	signal sum_stage1 : sum16_array_t := (others => (others => '0'));
    signal sum_stage2 : sum8_array_t := (others => (others => '0'));
    signal sum_stage3 : sum4_array_t := (others => (others => '0'));
    signal sum_stage4 : sum2_array_t := (others => (others => '0'));

begin
	-- ==============================
	-- ==        Delay Line        ==
	-- ==============================
	process(clk, enable, reset)
    begin
		if reset = '1' then
			delay_line <= (others => (others => '0'));
        elsif rising_edge(clk) and enable = '1' then
            for i in 31 downto 1 loop
                delay_line(i) <= delay_line(i - 1);
            end loop;
            delay_line(0) <= signed(sample_in);
        end if;
    end process;
	
	-- ==============================
	-- ==       32 Sum Line        ==
	-- ==============================
	--process(clk, reset)
    --begin
    --    if reset = '1' then
	--		sum_stage0 <= (others => (others => '0'));
	--	elsif rising_edge(clk) and enable = '1' then
    --        for i in 0 to 31 loop
    --            sum_stage0(i) <= resize(delay_line(2*i) + delay_line(2*i + 1), 32);
    --        end loop;
    --    end if;
    --end process;
	
	-- ==============================
	-- ==       16 Sum Line        ==
	-- ==============================
	process(clk, enable, reset)
    begin
        if reset = '1' then
			sum_stage1 <= (others => (others => '0'));
		elsif rising_edge(clk) and enable = '1' then
            for i in 0 to 15 loop
                --sum_stage1(i) <= resize(sum_stage0(2*i) + sum_stage0(2*i + 1), 32);
				sum_stage1(i) <= resize(delay_line(2*i) + delay_line(2*i + 1), 25);
            end loop;
        end if;
    end process;
	
	-- ==============================
	-- ==        8 Sum Line        ==
	-- ==============================
	process(clk, enable, reset)
    begin
        if reset = '1' then
			sum_stage2 <= (others => (others => '0'));
		elsif rising_edge(clk) and enable = '1' then
            for i in 0 to 7 loop
                sum_stage2(i) <= resize(sum_stage1(2*i) + sum_stage1(2*i + 1), 26);
            end loop;
        end if;
    end process;
	
	-- ==============================
	-- ==        4 Sum Line        ==
	-- ==============================
	process(clk, enable, reset)
    begin
        if reset = '1' then
			sum_stage3 <= (others => (others => '0'));
		elsif rising_edge(clk) and enable = '1' then
            for i in 0 to 3 loop
                sum_stage3(i) <= resize(sum_stage2(2*i) + sum_stage2(2*i + 1), 27);
            end loop;
        end if;
    end process;
	
	-- ==============================
	-- ==        2 Sum Line        ==
	-- ==============================
	process(clk, enable, reset)
    begin
        if reset = '1' then
			sum_stage4 <= (others => (others => '0'));
		elsif rising_edge(clk) and enable = '1' then
            for i in 0 to 1 loop
                sum_stage4(i) <= resize(sum_stage3(2*i) + sum_stage3(2*i + 1), 28);
            end loop;
        end if;
    end process;
	
	-- ==============================
	-- ==         Data Out         ==
	-- ==============================
	process(clk, enable, reset)
    begin
        if reset = '1' then
			data_out <= (others => '0');
		elsif rising_edge(clk) then
			if enable = '1' then
				data_out <= std_logic_vector(resize(sum_stage4(0) + sum_stage4(1)/ 32, 32));
				out_ready <= '1';
			else
				out_ready <= '0';
			end if;
        end if;
    end process;
end Behavioural;