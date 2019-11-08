program_name='amx-controlports-api'

#if_not_defined __AMX_CONTROLPORTS_API__
#define __AMX_CONTROLPORTS_API__


define_constant

/*
 * --------------------
 * Commands
 * --------------------
 */

// controller LED commands
char CONTROLLER_COMMAND_ENABLE_LEDS[]     = 'LED-EN'
char CONTROLLER_COMMAND_DISABLE_LEDS[]    = 'LED-DIS'
// serial commands
char SERIAL_COMMAND_BAUD_CONFIGURE[]                        = 'SET BAUD '
char SERIAL_COMMAND_BAUD_CONFIGURE_TEMPORARY[]              = 'TSET BAUD '
char SERIAL_COMMAND_BAUD_REQUEST[]                          = 'GET BAUD'
char SERIAL_COMMAND_BAUD_RESPONSE[]                         = 'PORT '
char SERIAL_COMMAND_HARDWARE_HANDSHAKING_OFF[]              = 'HSOFF'
char SERIAL_COMMAND_HARDWARE_HANDSHAKING_ON[]               = 'HSON'
char SERIAL_COMMAND_SOFTWARE_HANDSHAKING_OFF[]              = 'XOFF'
char SERIAL_COMMAND_SOFTWARE_HANDSHAKING_ON[]               = 'XON'
char SERIAL_COMMAND_CLEAR_RECEIVE_BUFFER[]                  = 'RXCLR'
char SERIAL_COMMAND_CLEAR_TRANSMIT_BUFFER[]                 = 'TXCLR'
char SERIAL_COMMAND_RECEIVED_DATA_PROCESS[]                 = 'RXON'
char SERIAL_COMMAND_RECEIVED_DATA_IGNORE[]                  = 'RXOFF'
char SERIAL_COMMAND_TRANSMIT_CHARACTER_DELAY_MICROSECONDS[] = 'CHARD-'
char SERIAL_COMMAND_TRANSMIT_CHARACTER_DELAY_MILLISECONDS[] = 'CHARDM-'
char SERIAL_COMMAND_CTS_REPORTING_ENABLE[]                  = 'CTSPSH'
char SERIAL_COMMAND_CTS_REPORTING_DISABLE[]                 = 'CTSPSH OFF'
// IR commands
char IR_COMMAND_CARRIER_ON[]                                           = 'CARON'
char IR_COMMAND_CARRIER_OFF[]                                          = 'CAROFF'
char IR_COMMAND_CHANNEL_SET_CHANNEL[]                                  = 'CH'
char IR_COMMAND_HALT_AND_CLEAR_ALL_ACTIVE_AND_BUFFERED_IR_SEND_PULSE[] = 'CP'
char IR_COMMAND_DURATION_OFF_TIME_BETWEEN_PULSES[]                     = 'CTOF'
char IR_COMMAND_DURATION_ON_TIME_PULSE[]                               = 'CTON'
char IR_COMMAND_BAUD_REQUEST[]                                         = 'GET BAUD'
char IR_COMMAND_BAUD_OR_MODE_CARRIER_AND_IO_LINK_RESPONSE[]            = 'PORT '
char IR_COMMAND_MODE_REQUEST[]                                         = 'GET MODE'
char IR_COMMAND_HALT_AND_CLEAR_ALL_ACTIVE_AND_BUFFERED_IR[]            = 'IROFF'
char IR_COMMAND_BAUD_CONFIGURE[]                                       = 'SET BAUD '
char IR_COMMAND_IO_LINK[]                                              = 'SET IO LINK '
char IR_COMMAND_MODE[]                                                 = 'SET MODE '
char IR_COMMAND_SINGLE_IR_PULSE[]                                      = 'SP'
char IR_COMMAND_CHANGE_IR_PATTERN[]                                    = 'XCHM-'
char IR_COMMAND_TRANSMIT_CODE_USING_PATTERN[]                          = 'XCH-'
char IR_COMMAND_DISABLE_IO_LINK_POWER_SETTINGS[]                       = 'POD'
char IR_COMMAND_TURN_OFF_DEVICE_BASED_ON_IO_LINK_STATUS[]              = 'POF'
char IR_COMMAND_TURN_ON_DEVICE_BASED_ON_IO_LINK_STATUS[]               = 'PON'
char IR_COMMAND_DURATION_ON_TIME_POWER_PULSES[]                        = 'PTON'
char IR_COMMAND_DURATION_OFF_TIME_BETWEEN_POWER_PULSES[]               = 'PTOF'

char IO_COMMAND_INPUT_ACTIVE_STATE_REQUEST[]   = 'GET INPUT '
char IO_COMMAND_INPUT_ACTIVE_STATE_CONFIGURE[] = 'SET INPUT '
char IO_COMMAND_INPUT_STATE_RESPONSE[]  = 'INPUT'

define_constant

integer SERIAL_PORT_CTS_CHANNEL = 255

/*
 * --------------------
 * Command Parameters
 * --------------------
 */


char SERIAL_BAUD_RATE_115200[]  = '115200'
char SERIAL_BAUD_RATE_76800[]   = '76800'
char SERIAL_BAUD_RATE_57600[]   = '57600'
char SERIAL_BAUD_RATE_38400[]   = '38400'
char SERIAL_BAUD_RATE_19200[]   = '19200'
char SERIAL_BAUD_RATE_9600[]    = '9600'
char SERIAL_BAUD_RATE_4800[]    = '4800'
char SERIAL_BAUD_RATE_2400[]    = '2400'
char SERIAL_BAUD_RATE_1200[]    = '1200'
char SERIAL_BAUD_RATE_600[]     = '600'
char SERIAL_BAUD_RATE_300[]     = '300'
char SERIAL_BAUD_RATE_150[]     = '150'

char SERIAL_BAUD_PARITY_NONE[]  = 'N'
char SERIAL_BAUD_PARITY_ODD[]   = 'O'
char SERIAL_BAUD_PARITY_EVEN[]  = 'E'
char SERIAL_BAUD_PARITY_MARK[]  = 'M'
char SERIAL_BAUD_PARITY_SPACE[] = 'S'

char SERIAL_DATA_BITS_8[]   = '8'
char SERIAL_DATA_BITS_9[]   = '9' // only valid with no parity and 1 stop bit

char SERIAL_STOP_BITS_1[]   = '1'
char SERIAL_STOP_BITS_2[]   = '2'

char SERIAL_485_ENABLE[]    = '485 ENABLE'
char SERIAL_485_DISABLE[]   = '485 DISABLE'
char SERIAL_485_ENABLED[]   = '485 ENABLED'
char SERIAL_485_DISABLED[]  = '485 DISABLED'
char SERIAL_485_IGNORE[]    = ''

char IO_STATE_HIGH[]    = 'HIGH'
char IO_STATE_LOW[]     = 'LOW'

char IR_BAUD_RATE_19200[]   = '19200'
char IR_BAUD_RATE_9600[]    = '9600'
char IR_BAUD_RATE_4800[]    = '4800'
char IR_BAUD_RATE_2400[]    = '2400'
char IR_BAUD_RATE_1200[]    = '1200'

char IR_PARITY_NONE[]  = 'N'
char IR_PARITY_ODD[]   = 'O'
char IR_PARITY_EVEN[]  = 'E'
char IR_PARITY_MARK[]  = 'M'
char IR_PARITY_SPACE[] = 'S'

char IR_DATA_BITS_7[]   = '7'
char IR_DATA_BITS_8[]   = '8'

char IR_STOP_BITS_1[]   = '1'
char IR_STOP_BITS_2[]   = '2'

char IR_MODE_SERIAL[]   = 'SERIAL'
char IR_MODE_DATA[]     = 'DATA'
char IR_MODE_IR[]       = 'IR'

char IR_PATTERN_MODE_0[] = '0'   // Example: [x][x]<x><enter> (3 transmitted as 3-enter, 34 transmitted as 3-4-enter, 345 transmitted as 3-4-5-enter)
char IR_PATTERN_MODE_1[] = '1'   // Example: <x><x><x><enter> (3 transmitted as 0-0-3-enter, 34 transmitted as 0-3-4-enter, 345 transmitted as 3-4-5-enter)
char IR_PATTERN_MODE_2[] = '2'   // Example: <x><x><x> (3 transmitted as 0-0-3, 34 transmitted as 0-3-4, 345 transmitted as 3-4-5)
char IR_PATTERN_MODE_3[] = '3'   // Example: [[100][100]....]<x><x> (3 transmitted as 0-3, 34 transmitted as 3-4, 345 transmitted as 100-100-3-4-5)
char IR_PATTERN_MODE_4[] = '4'   // Sends same sequence as 'CH' command. Note: only use mode 4 with channels 0..199.
char IR_PATTERN_MODE_5[] = '5'   // <x><x><x><x><enter> (3 transmitted as 0-0-0-3-enter, 34 transmitted as 0-0-3-4-enter, 345 transmitted as 0-3-4-5-enter, 3456 transmitted as 3-4-5-6-enter)
char IR_PATTERN_MODE_6[] = '6'   // <x><x><x><x> (3 transmitted as 0-0-0-3, 34 transmitted as 0-0-3-4, 345 transmitted as 0-3-4-5, 3456 transmitted as 3-4-5-6)



char IR_CARRIER_ENABLED[]  = 'CARRIER'
char IR_CARRIER_DISABLED[] = 'NO CARRIER'

char IO_ACTIVE_STATE_LOW[]  = 'ACTIVE LOW'
char IO_ACTIVE_STATE_HIGH[] = 'ACTIVE HIGH'


#end_if