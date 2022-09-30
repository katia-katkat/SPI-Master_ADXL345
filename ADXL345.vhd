--------------------------------------------------------------------------------
--
--   FileName:         pmod_accelerometer_adxl345.vhd
--   Dependencies:     spi_master.vhd
--   Design Software:  Quartus Prime Version 17.0.0 Build 595 SJ Lite Edition
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 09/19/2019 Scott Larson
--     Initial Public Release
--   Version 1.1 07/29/2022 Scott Larson
--     Fixed potential intermittent problem coming out of reset
--    
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY pmod_accelerometer_adxl345 IS
  GENERIC(
    clk_freq   : INTEGER := 50;              --system clock frequency in MHz
    data_rate  : STD_LOGIC_VECTOR := "0100"; --data rate code to configure the accelerometer
    data_range : STD_LOGIC_VECTOR := "00");  --data range code to configure the accelerometer
  PORT(
    clk            : IN      STD_LOGIC;                      --system clock
    reset_n        : IN      STD_LOGIC;                      --active low asynchronous reset
    miso           : IN      STD_LOGIC;                      --SPI bus: master in, slave out
    sclk           : out  STD_LOGIC;                      --SPI bus: serial clock
    cs           : BUFFER  STD_LOGIC;   --SPI bus: slave select
    mosi           : OUT     STD_LOGIC;                      --SPI bus: master out, slave in
    acceleration_x : BUFFER     STD_LOGIC_VECTOR(15 DOWNTO 0);  --x-axis acceleration data
	 x : OUT     STD_LOGIC_VECTOR(15 DOWNTO 0);  --x-axis acceleration data
    acceleration_y : OUT     STD_LOGIC_VECTOR(15 DOWNTO 0);  --y-axis acceleration data
    acceleration_z : OUT     STD_LOGIC_VECTOR(15 DOWNTO 0)); --z-axis acceleration data
END pmod_accelerometer_adxl345;

ARCHITECTURE behavior OF pmod_accelerometer_adxl345 IS
  TYPE machine IS(start, pause, configure, read_data, output_result); --needed states
  SIGNAL state              : machine := start;                       --state machine
  SIGNAL parameter          : INTEGER RANGE 0 TO 3;                   --parameter being configured
  SIGNAL parameter_addr     : STD_LOGIC_VECTOR(5 DOWNTO 0);           --register address of configuration parameter
  SIGNAL parameter_data     : STD_LOGIC_VECTOR(3 DOWNTO 0);           --value of configuration parameter
  SIGNAL spi_busy_prev      : STD_LOGIC;                              --previous value of the SPI component's busy signal
  SIGNAL spi_busy           : STD_LOGIC;                              --busy signal from SPI component
  SIGNAL spi_ena            : STD_LOGIC;                              --enable for SPI component
  SIGNAL mode           : STD_LOGIC;                              --continuous mode signal for SPI component
  SIGNAL spi_tx_data        : STD_LOGIC_VECTOR(7 DOWNTO 0);           --transmit data for SPI component
  SIGNAL spi_rx_data        : STD_LOGIC_VECTOR(7 DOWNTO 0);           --received data from SPI component
  SIGNAL acceleration_x_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          --internal x-axis acceleration data buffer
  SIGNAL acceleration_y_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          --internal y-axis acceleration data buffer
  SIGNAL acceleration_z_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          --internal z-axis acceleration data buffer
  signal clk_counter : natural range 0 to 2500000 := 0;

  --declare SPI Master component
   COMPONENT spi  IS
  PORT(
				clk , reset : in std_logic ;
				din : in std_logic_vector (7 downto 0); --i_mosi_byte
				dvsr : in std_logic_vector (15 downto 0);
				start : in std_logic ;
				cpol , cpha : in std_logic ;
				dout : out std_logic_vector (7 downto 0); -- o_miso 
				busy : out std_logic ;				
				cs : buffer std_logic ;
				mode : in      STD_LOGIC;                      --SPI bus: master in, slave out
				sclk : out std_logic ;
				miso : in std_logic ; 
				mosi : out std_logic);
  END COMPONENT spi;
BEGIN

  --instantiate the SPI Master component
 --instantiate the SPI Master component
  spi_master_0:  spi
    PORT MAP(
			  clk => clk, 
			  reset => reset_n, 
			  start => spi_ena, 
			  cpol => '1', 
			  cpha => '1',
           dvsr => "0000000000001010",
			  din => spi_tx_data,
			  miso => miso,
           sclk => sclk, 
			  cs => cs,
			  mode => mode,
			  mosi => mosi, 
			  busy => spi_busy, 
			  dout => spi_rx_data
			  );
  PROCESS(clk)
    VARIABLE count : INTEGER := 0; --universal counter
  BEGIN
    IF(reset_n = '1') THEN              --reset activated
      spi_busy_prev <= '0';               --clear previous value of SPI component's busy signal
      spi_ena <= '0';                     --clear SPI component enable
      mode <= '0';                    --clear SPI component continuous mode signal
      spi_tx_data <= (OTHERS => '0');     --clear SPI component transmit data
      acceleration_x <= (OTHERS => '0');  --clear x-axis acceleration data
      acceleration_y <= (OTHERS => '0');  --clear y-axis acceleration data
      acceleration_z <= (OTHERS => '0');  --clear z-axis acceleration data
      state <= start;                     --restart state machine
    ELSIF(clk'EVENT AND clk = '1') THEN --rising edge of system clock
      CASE state IS                       --state machine

        --entry state
        WHEN start =>
          count := 0;      --clear universal counter
          parameter <= 0;  --clear parameter indicator
          state <= pause;
          
        --pauses 200ns between SPI transactions and selects SPI transaction
        WHEN pause =>
          IF(spi_busy = '0') THEN                --SPI component not busy
            IF(count < clk_freq/5) THEN            --less than 200ns
              count := count + 1;                    --increment counter
              state <= pause;                        --remain in pause state
            ELSE                                   --200ns has elapsed
              count := 0;                            --clear counter
              CASE parameter IS                      --select SPI transaction
                WHEN 0 =>                              --SPI transaction to set range
                  parameter <= parameter + 1;            --increment parameter for next transaction
                  parameter_addr <= "110001";            --register address with range setting
                  parameter_data <= "10" & data_range;   --data to set specified range
                  state <= configure;                    --proceed to SPI transaction
                WHEN 1 =>                             --SPI transaction to set data rate
                  parameter <= parameter + 1;            --increment parameter for next transaction
                  parameter_addr <= "101100";            --register address with data rate setting
                  parameter_data <= data_rate;           --code to set specified data rate
                  state <= configure;                    --proceed to SPI transaction
                WHEN 2 =>                             --SPI transaction to enable measuring
                  parameter <= parameter + 1;            --increment parameter for next transaction
                  parameter_addr <= "101101";            --register address with enable measurement setting
                  parameter_data <= "1000";              --data to enable measurement
                  state <= configure;                    --proceed to SPI transaction
                WHEN 3 =>                             --SPI transaction to read data
                  state <= read_data;                    --proceed to SPI transaction
                WHEN OTHERS => NULL;
              END CASE;        
            END IF;
          END IF;

        --performs SPI transactions that write to configuration registers  
        WHEN configure =>
          spi_busy_prev <= spi_busy;                      --capture the value of the previous spi busy signal
          IF(spi_busy_prev = '1' AND spi_busy = '0') THEN --spi busy just went low
            count := count + 1;                             --counts times busy goes from high to low during transaction
          END IF;
          CASE count IS                                   --number of times busy has gone from high to low
            WHEN 0 =>                                       --no busy deassertions
              IF(spi_busy = '0') THEN                         --transaction not started
                mode <= '1';                                --set to continuous mode
                spi_ena <= '1';                                 --enable SPI transaction
                spi_tx_data <= "00" & parameter_addr;           --first information to send
              ELSE                                            --transaction has started
                spi_tx_data <= "0000" & parameter_data;         --second information to send (first has been latched in)
              END IF;
            WHEN 1 =>                                       --first busy deassertion
              mode <= '0';                                --clear continous mode to end transaction
              spi_ena <= '0';                                 --clear SPI transaction enable
              count := 0;                                     --clear universal counter
              state <= pause;                                 --return to pause state
            WHEN OTHERS => NULL;
          END CASE;

        --performs SPI transactions that read acceleration data registers  
        WHEN read_data =>
          spi_busy_prev <= spi_busy;                        --capture the value of the previous spi busy signal
          IF(spi_busy_prev = '1' AND spi_busy = '0') THEN   --spi busy just went low
            count := count + 1;                               --counts the times busy goes from high to low during transaction
          END IF;          
          CASE count IS                                     --number of times busy has gone from high to low
            WHEN 0 =>                                         --no busy deassertions
              IF(spi_busy = '0') THEN                           --transaction not started
                mode <= '1';                                  --set to continuous mode
                spi_ena <= '1';                                   --enable SPI transaction
                spi_tx_data <= "11110010";                        --first information to send
              ELSE                                              --transaction has started
                spi_tx_data <= "00000000";                        --second information to send (first has been latched in)              
              END IF;
            WHEN 2 =>                                         --2nd busy deassertion
              acceleration_x_int(7 DOWNTO 0) <= spi_rx_data;    --latch in first received acceleration data
            WHEN 3 =>                                         --3rd busy deassertion
              acceleration_x_int(15 DOWNTO 8) <= spi_rx_data;   --latch in second received acceleration data
            WHEN 4 =>                                         --4th busy deassertion
              acceleration_y_int(7 DOWNTO 0) <= spi_rx_data;    --latch in third received acceleration data
            WHEN 5 =>                                         --5th busy deassertion
              acceleration_y_int(15 DOWNTO 8) <= spi_rx_data;   --latch in fourth received acceleration data
            WHEN 6 =>                                         --6th busy deassertion
              mode <= '0';                                  --clear continuous mode to end transaction
              spi_ena <= '0';                                   --clear SPI transaction enable
              acceleration_z_int(7 DOWNTO 0) <= spi_rx_data;    --latch in fifth received acceleration data
            WHEN 7 =>                                         --7th busy deassertion
              acceleration_z_int(15 DOWNTO 8) <= spi_rx_data;   --latch in sixth received acceleration data
              count := 0;                                       --clear universal counter
              state <= output_result;                           --proceed to output result state
            WHEN OTHERS => NULL;
          END CASE;
  
        --outputs acceleration data
        WHEN output_result =>
            acceleration_x <= acceleration_x_int;  --output x-axis data
            acceleration_y <= acceleration_y_int;  --output y-axis data
            acceleration_z <= acceleration_z_int;  --output z-axis data
            state <= pause;                        --return to pause state
        
        --default to start state
        WHEN OTHERS => 
          state <= start;

      END CASE;      
    END IF;
  END PROCESS;
   process(clk) 
  begin 
   if rising_edge(clk) then
      if clk_counter = 2500000 then  
        x <= acceleration_x;
        clk_counter  <= 0;
      else
        clk_counter <= clk_counter + 1;
      end if;
    end if;
	 end process;
END behavior;
