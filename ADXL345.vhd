
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
    miso           : in      STD_LOGIC;                      --SPI bus: master in, slave out
	 miso_s           : out      STD_LOGIC;                      --SPI bus: master in, slave out
    sclk           : out  STD_LOGIC;                      --SPI bus: serial clock
    mosi           : OUT     STD_LOGIC;                      --SPI bus: master out, slave in
	 cs           : OUT     STD_LOGIC;                      --SPI bus: master out, slave in
    acceleration_x : OUT     STD_LOGIC_VECTOR(15 DOWNTO 0);  --x-axis acceleration data
    acceleration_y : OUT     STD_LOGIC_VECTOR(15 DOWNTO 0);  --y-axis acceleration data
    acceleration_z : OUT     STD_LOGIC_VECTOR(15 DOWNTO 0)); --z-axis acceleration data
END pmod_accelerometer_adxl345;

	ARCHITECTURE behavior OF pmod_accelerometer_adxl345 IS
  TYPE machine IS(start, configure, write_data ,read_data,output_result); --needed states
  SIGNAL state              : machine := start;                       --state machine
  SIGNAL parameter          : INTEGER RANGE 0 TO 3;                   --parameter being configured
  SIGNAL parameter_addr     : STD_LOGIC_VECTOR(7 DOWNTO 0);           --register address of configuration parameter
  SIGNAL parameter_data     : STD_LOGIC_VECTOR(3 DOWNTO 0);           --value of configuration parameter
  SIGNAL spi_busy_prev      : STD_LOGIC;                              --previous value of the SPI component's busy signal
  SIGNAL spi_busy           : STD_LOGIC;                              --busy signal from SPI component
  SIGNAL spi_ena            : STD_LOGIC;                              --enable for SPI component
  SIGNAL spi_tx_data        : STD_LOGIC_VECTOR(7 DOWNTO 0);           --transmit data for SPI component
  SIGNAL spi_rx_data        : STD_LOGIC_VECTOR(7 DOWNTO 0);           --received data from SPI component
  SIGNAL acceleration_x_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          --internal x-axis acceleration data buffer
  SIGNAL acceleration_y_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          --internal y-axis acceleration data buffer
  SIGNAL acceleration_z_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          --internal z-axis acceleration data buffer
  signal count_signal : integer := 0;
  signal clk_counter : natural range 0 to 50000000 := 0;


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
				cs : out std_logic ;
				miso_s           : out      STD_LOGIC;                      --SPI bus: master in, slave out
				sclk : out std_logic ;
				miso : in std_logic ; 
				mosi : out std_logic);
  END COMPONENT spi;

BEGIN

  --instantiate the SPI Master component
  spi_master_0:  spi
    PORT MAP(
			  clk => clk, 
			  reset => reset_n, 
			  start => spi_ena, 
			  cpol => '0', 
			  cpha => '0',
           dvsr => "0000000000000100",
			  din => spi_tx_data,
			  miso => miso,
			  miso_s => miso_s,
           sclk => sclk, 
			  cs => cs,
			  mosi => mosi, 
			  busy => spi_busy, 
			  dout => spi_rx_data
			  );

  PROCESS(clk,reset_n)
    VARIABLE count : INTEGER := 0; --universal counter
  BEGIN
    IF(reset_n = '1') THEN              --reset activated
      spi_busy_prev <= '0';               --clear previous value of SPI component's busy signal
      spi_ena <= '0';                     --clear SPI component enable
      spi_tx_data <= (OTHERS => '0');     --clear SPI component transmit data
      acceleration_x <= (OTHERS => '0');  --clear x-axis acceleration data
      acceleration_y <= (OTHERS => '0');  --clear y-axis acceleration data
      acceleration_z <= (OTHERS => '0');  --clear z-axis acceleration data
		acceleration_z <= (OTHERS => '0');  --clear z-axis acceleration data
		parameter_data <= (OTHERS => '0');  --clear data paramerter
		parameter_addr <= (OTHERS => '0');  --clear z-axis acceleration dat
      state <= start;                     --restart state machine
    ELSIF(clk'EVENT AND clk = '1') THEN --rising edge of system clock
      CASE state IS                       --state machine

        --entry state
        WHEN start =>
          state <= configure;
			 parameter <= 0;
          
        --pauses 200ns between SPI transactions and selects SPI transaction
        WHEN configure =>
		    spi_busy_prev <= spi_busy;                      --capture the value of the previous spi busy signal
          --IF(spi_busy_prev = '1' AND spi_busy = '0') THEN --spi busy just went low
           -- parameter <= parameter + 1;                             --counts times busy goes from high to low during transaction
				--end if;
              CASE parameter IS                      --select SPI transaction
                WHEN 0 =>                              --SPI transaction to set range
                  parameter_addr <= x"31";            --register address with range setting
                  parameter_data <= "10" & data_range;   --data to set specified range
                  state <= write_data;                    --proceed to SPI transaction
                WHEN 1 =>                             --SPI transaction to set data rate
                  parameter_addr <= x"2C";            --register address with data rate setting
                  parameter_data <= data_rate;           --code to set specified data rate
                  state <= write_data;                    --proceed to SPI transaction
                WHEN 2 =>                             --SPI transaction to enable measuring
                  parameter_addr <= x"2D";            --register address with enable measurement setting
                  parameter_data <= "1000";              --data to enable measurement
                  state <= write_data;                    --proceed to SPI transaction
                WHEN 3 =>                             --SPI transaction to read data
                  state <= read_data;       						--proceed to SPI transaction
						
                WHEN OTHERS => NULL;
              END CASE;        


        --performs SPI transactions that write to configuration registers  
        WHEN write_data =>
          spi_busy_prev <= spi_busy;                      --capture the value of the previous spi busy signal
          IF(spi_busy_prev = '0' AND spi_busy = '1') THEN --spi busy just went low
            count := count + 1;                             --counts times busy goes from high to low during transaction
			END IF;
          CASE count IS                                   --number of times busy has gone from high to low
            WHEN 0 =>                                       --no busy deassertions
                spi_ena <= '1';                                 --enable SPI transaction
                spi_tx_data <= parameter_addr;           --first information to send
					 
            WHEN 1 =>    
				    spi_ena <= '1';          --clear SPI transaction enable
                spi_tx_data <= "0000" & parameter_data;         --second information to send (first has been latched in)
					
				WHEN 2 => 
					 state <= configure;
					 count := 0;
					 parameter <= parameter +1;
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
                spi_ena <= '1';                                   --enable SPI transaction
                spi_tx_data <= "11110010";                        --first information to send
              ELSE                                              --transaction has started
                spi_tx_data <= "00000000";                        --second information to send (first has been latched in)              
              END IF;
            WHEN 1 =>                                         --2nd busy deassertion
              acceleration_x_int(7 DOWNTO 0) <= spi_rx_data;    --latch in first received acceleration data
            WHEN 2 =>                                         --3rd busy deassertion
              acceleration_x_int(15 DOWNTO 8) <= spi_rx_data;   --latch in second received acceleration data
            WHEN 3 =>                                         --4th busy deassertion
              acceleration_y_int(7 DOWNTO 0) <= spi_rx_data;    --latch in third received acceleration data
            WHEN 4 =>                                         --5th busy deassertion
              acceleration_y_int(15 DOWNTO 8) <= spi_rx_data;   --latch in fourth received acceleration data
            WHEN 5 =>                                         --6th busy deassertion
              spi_ena <= '0';                                   --clear SPI transaction enable
              acceleration_z_int(7 DOWNTO 0) <= spi_rx_data;    --latch in fifth received acceleration data
            WHEN 6 =>                                         --7th busy deassertion
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

            state <= configure;                        
        
        --default to start state
        WHEN OTHERS => 
          state <= start;		

      END CASE;      
    END IF;
	   count_signal <= count;
  END PROCESS;
END behavior;
