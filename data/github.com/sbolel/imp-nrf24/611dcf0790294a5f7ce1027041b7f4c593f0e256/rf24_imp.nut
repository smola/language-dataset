 /* Imp library for nRF24L01+ 2.4Ghz transceiver
  *
  * Author:      Sinan Bolel
  * Company:     SwipeSense Inc.
  * Email:       sinan@swipesense.com
  * Repository:  git@github.com:sbolel/nrf24_imp.git
  * License:     MIT
  * Version:     1.2.1
  * Last Update: 10/01/2012
  */


// RF24 ADDRESSES AND INSTRUCTIONS

// Memory Map
const CONFIG        = 0x00;
const EN_AA         = 0x01;
const EN_RXADDR     = 0x02;
const SETUP_AW      = 0x03;
const SETUP_RETR    = 0x04;
const RF_CH         = 0x05;
const RF_SETUP      = 0x06;
const STATUS        = 0x07;
const OBSERVE_TX    = 0x08;
const CD            = 0x09;
const RX_ADDR_P0    = 0x0A;
const RX_ADDR_P1    = 0x0B;
const RX_ADDR_P2    = 0x0C;
const RX_ADDR_P3    = 0x0D;
const RX_ADDR_P4    = 0x0E;
const RX_ADDR_P5    = 0x0F;
const TX_ADDR       = 0x10;
const RX_PW_P0      = 0x11;
const RX_PW_P1      = 0x12;
const RX_PW_P2      = 0x13;
const RX_PW_P3      = 0x14;
const RX_PW_P4      = 0x15;
const RX_PW_P5      = 0x16;
const FIFO_STATUS   = 0x17;

// Bit Mnemonics
const MASK_RX_DR    = 6;
const MASK_TX_DS    = 5;
const MASK_MAX_RT   = 4;
const EN_CRC        = 3;
const CRCO          = 2;
const PWR_UP        = 1;
const PRIM_RX       = 0;
const ENAA_P5       = 5;
const ENAA_P4       = 4;
const ENAA_P3       = 3;
const ENAA_P2       = 2;
const ENAA_P1       = 1;
const ENAA_P0       = 0;
const ERX_P5        = 5;
const ERX_P4        = 4;
const ERX_P3        = 3;
const ERX_P2        = 2;
const ERX_P1        = 1;
const ERX_P0        = 0;
const AW            = 0;
const ARD           = 4;
const ARC           = 0;
const PLL_LOCK      = 4;
const RF_DR         = 3;
const RF_PWR        = 1;
const LNA_HCURR     = 0;
const RX_DR         = 6;
const TX_DS         = 5;
const MAX_RT        = 4;
const RX_P_NO       = 1;
const TX_FULL       = 0;
const PLOS_CNT      = 4;
const ARC_CNT       = 0;
const TX_REUSE      = 6;
const FIFO_FULL     = 5;
const TX_EMPTY      = 4;
const RX_FULL       = 1;
const RX_EMPTY      = 0;

// Instruction Mnemonics
const R_REGISTER    = 0x00;
const W_REGISTER    = 0x20;
const REGISTER_MASK = 0x1F;
const R_RX_PAYLOAD  = 0x61;
const W_TX_PAYLOAD  = 0xA0;
const FLUSH_TX      = 0xE1;
const FLUSH_RX      = 0xE2;
const REUSE_TX_PL   = 0xE3;
const NOP           = 0xFF;

// Pipe Addresses for RX & TX
const pipes0 = "\xE1\xF0\xF0\xF0\xF0";
const pipes1 = "\xD2\xF0\xF0\xF0\xF0";

// RF24 Methods
function _BV(x) { return (1<<(x)); }

/* Pin Schema for IMP <--> nRF24L01
 *
 * nRF24  |  Imp  | Function
 * -------|-------|-----------------------
 * CE    ->   2   |  Controles RX/TX
 * CSN   ->   5   |  Chip select not
 * SCK   ->   1   |  SPI Clock
 * MOSI  ->   8   |  Master-out-slave-in
 * MISO  ->   9   |  Master-in-slave-out
 */

// RF24 class (nRF24L01 transciever)
class RF24 {
  ce_pin = null;
  cs_pin = null;
  so_pin = null;
  spiSetup_flags = null;
  spiSetup_clock = null;
  mode = null;
  power = null;
  channel = null;
  dataRate = null;
  payloadSize = null;
  localAddress = null;
  remoteAddress = null;

  constructor(clock)
  {
    mode           = 0;                 // MD  = 0b0000
    power          = 3;                 // PWR = 0b0011
    channel        = 111;               // CH  = 0b01101111
    dataRate       = 1;                 // DR  = 0b0001 (Unimplemented)
    payloadSize    = 0;                 // PS  = 0b0000 (Unimplemented)
    localAddress   = 0;                 // LA  = 0b0000 (Unimplemented)
    remoteAddress  = 0;                 // RA  = 0b0000 (Unimplemented)
    spiSetup_flags = 0;                 // SPI Flags = 0b0000 (Master)
    spiSetup_clock = clock;             // SPI Clock Speed = input
    pin_setup();                        // Configure Imp pins (for CSN & CE)
    spi_setup();                        // Configure Imp SPI_189 communication
    write_register(SETUP_RETR,(0x4<<ARD)|(0xF<<ARC));           // Set 1500uS timeouts
    write_register(STATUS,_BV(RX_DR)|_BV(TX_DS)|_BV(MAX_RT) );  // Reset Status
    set_channel(76);                    // Set RF channel to 76
    flush_rx();                         // Flush RX buffer
    flush_tx();                         // Flush TX buffer
  }

  // RF24 CSN functions (active low)
  function csn(state) {                 
    switch(state) {                     // Check state input
      case 0: cs_pin.write(0);          // Select Chip   (SPI comm enabled)
      case 1: cs_pin.write(1);          // Deselect Chip (SPI comm disabled)
    }
  }
  // RF24 CE Functions (RF24 RX mode)
  function ce(state) {
    switch(state) {                     // Check state input
      case 1: ce_pin.write(1);          // Enable Chip  (radio listen enabled)
      case 0: ce_pin.write(0);          // Disable Chip (radio listen disabled)
    }
  }

  // Configure SPI Comm (pins 189)
  function spi_setup() {
    hardware.configure(SPI_189);
    return hardware.spi189.configure(0,spiSetup_clock);
  }

  // Setup imp pins
  function pin_setup() {
    ce_pin = hardware.pin2;             // Chip Enable - Active high (While RX)
    cs_pin = hardware.pin5;             // Chip Select Not - Active low (SPI)
    // Configure CSN pin on Imp
    if(cs_pin != null) {
      cs_pin.configure(DIGITAL_OUT);    // CSN pin = digital out
      csn(1);                           // Chip deselect
    }
    // Configure CE pin on Imp 
    if(ce_pin != null) {                
      ce_pin.configure(DIGITAL_OUT);    // CE pin = digital out
      ce(0);                            // Stop radio
    }
  }

  // Read a reagister
  function read_register(regAddr) {
    csn(0);                                           // Chip select
    hardware.spi189.write(format("%c", regAddr));     // Send read instruction
    hardware.spi189.read(1);                          // Read a byte
    hardware.spi189.write("\xFF");                    // Send dummy byte
    hardware.spi189.read(1);                          // Read a byte
    hardware.spi189.write("\xFF");                    // Send dummy byte
    hardware.spi189.read(1);                          // Read a byte
    hardware.spi189.write("\xFF");                    // Send dummy byte
    hardware.spi189.read(1);                          // Read a byte
    hardware.spi189.write("\xFF");                    // Send dummy byte
    hardware.spi189.read(1);                          // Read a byte
    hardware.spi189.write("\xFF");                    // Send dummy byte
    local result = hardware.spi189.read(1)[0];        // Read the actual result
    csn(1);                                           // Chip deselect
    return result;
  }

  // Write data to a register
  function write_register(regAddr, data) {
    local address = regAddr | 0x20;                   // Addr = Read + Address
    csn(0);                                           // Chip select
    hardware.spi189.write(format("%c",address));      // Write the address
    hardware.spi189.write(format("%c",data));         // Write the data
    csn(1);                                           // Chip deselect
  }

  // Send instruction byte
  function instructByte(instruction) {
    csn(0);                                           // Chip select
    hardware.spi189.write(format("%c",instruction));  // Flush RX buffer
    csn(1);                                           // Chip deselect
  }

  function pwrDn() { write_register(CONFIG,read_register(CONFIG) & ~_BV(PWR_UP)); }
  function pwrUp() { write_register(CONFIG,read_register(CONFIG) | _BV(PWR_UP)); }

  function flush_rx() { instructByte(FLUSH_RX); }             // Flush RX buffer
  function flush_tx() { instructByte(FLUSH_TX); }             // Flush RX buffer

  function set_channel(chn) { write_register(RF_CH,chn); }    // Set RF channel

  function get_status() { instructByte(NOP); }                // Read STATUS
  function print_status(status) {                             // Log status to server
    server.log(("STATUS\t\t = 0x%02x RX_DR=%x TX_DS=%x MAX_RT=%x RX_P_NO=%x TX_FULL=%x\r\n"),
           status,
           (status & _BV(RX_DR))?1:0,
           (status & _BV(TX_DS))?1:0,
           (status & _BV(MAX_RT))?1:0,
           ((status >> RX_P_NO) & B111),
           (status & _BV(TX_FULL))?1:0
          );
  }

  /* Configuration arrays for pipe constants (From Arduino RF24 lib)
   *
   *    static const uint8_t child_pipe[] PROGMEM =
   *    { RX_ADDR_P0, RX_ADDR_P1, RX_ADDR_P2, RX_ADDR_P3, RX_ADDR_P4, RX_ADDR_P5 };
   *    static const uint8_t child_payload_size[] PROGMEM =
   *    { RX_PW_P0, RX_PW_P1, RX_PW_P2, RX_PW_P3, RX_PW_P4, RX_PW_P5 };
   *    static const uint8_t child_pipe_enable[] PROGMEM =
   *    { ERX_P0, ERX_P1, ERX_P2, ERX_P3, ERX_P4, ERX_P5 };
   */

  function openWritingPipe() {
    /* Notes:
     *  - AVR 8-bit uC's store this LSB first
     *  - NRF24L01(+) expects it LSB first as well
     *  - Writing pipe: pipes[1]                
     */
    write_register(RX_ADDR_P0, pipes1);
    write_register(TX_ADDR, pipes1);
    write_register(RX_PW_P0,"\x20");            // max_payload_size = 32;
  }

  function openReadingPipe() {
    /* Notes:
     * - radio.openReadingPipe(1,pipes[0]);
     * - child = 1, pipe = 0
    */
    write_register(RX_ADDR_P1, pipes0);         // pipes0 : 0xE1F0F0F0F0
    write_register(RX_PW_P1, "\x20");
    write_register(EN_RXADDR, read_register(EN_RXADDR) | _BV(ERX_P1));
  }

  function stopListening() {
    ce(0);                    // Stop listening on Radio
    flush_tx();               // Flush buffers
    flush_rx();               // Flush buffers
  }

  function startListening() {
    write_register(CONFIG, read_register(CONFIG) | _BV(PWR_UP) | _BV(PRIM_RX));
    write_register(STATUS, _BV(RX_DR) | _BV(TX_DS) | _BV(MAX_RT) );
    /* [TODO] (incomplete, not sure if needed)
     * if (pipe0_reading_address)    // Restore the pipe0 adddress, if exists
     *   write_register(RX_ADDR_P0, reinterpret_cast<const uint8_t*>(&pipe0_reading_address), 5);
     */
    flush_rx();               // Flush buffers
    flush_tx();               // Flush buffers
    ce(1);                    // Start listening on Radio
    imp.sleep(0.00013);       // wait for the radio to start (only 130us needed)
  }

  function startRadio() {
    openWritingPipe();        // Configure sending address
    openReadingPipe();        // Configure receiving address
    startListening();         // Initiate listening
  }

/* TODO (incomplete)
 *
 *  uint8_t RF24::read_payload(void* buf, uint8_t len) {
 *    local status;
 *    local* current = reinterpret_cast<uint8_t*>(buf);
 *
 *    uint8_t data_len = min(len,payload_size);
 *    uint8_t blank_len = dynamic_payloads_enabled ? 0 : payload_size - data_len;
 *
 *    //printf("[Reading %u bytes %u blanks]",data_len,blank_len);
 *
 *    RF24_select();
 *    status = SPI.transfer( R_RX_PAYLOAD );
 *    while ( data_len-- )
 *      *current++ = SPI.transfer(0xff);
 *    while ( blank_len-- )
 *      SPI.transfer(0xff);
 *    csn(HIGH);
 *
 *    return status;
 *  }
 *
 *  function RF24::read( void* buf, uint8_t len )
 *  {
 *    // Fetch the payload
 *    read_payload( buf, len );
 *
 *    // was this the last of the data available?
 *    return read_register(FIFO_STATUS) & _BV(RX_EMPTY);
 *  }
 *
 */

}

// IMP code
function imp_connectionStatistics()
{
  local signal = format("Signal Strenght: %d dBm", imp.rssi());
  server.show(signal);
}

// Configure Imp
server.log("Imp starting...");
imp.configure("Imp RF24", [], []);
imp_connectionStatistics();

// Declare radio object
radio <- RF24(100);
server.log("01) RF24 Initialization Success " + radio + "");

// Start RF24 radio
radio.startListening();
imp.sleep(1);
server.log("02) Radio Started ");
