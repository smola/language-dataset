PROGRAM_NAME='prime-connman-server'

#IF_NOT_DEFINED __PRIME_CONNMAN_SERVER
	#DEFINE __PRIME_CONNMAN_SERVER

(***********************************************************)
(*  FILE CREATED ON: 01/23/2016  AT: 21:43:16              *)
(***********************************************************)
(***********************************************************)
(***********************************************************)
(*  FILE_LAST_MODIFIED_ON: 05/19/2016  AT: 17:39:08        *)
(***********************************************************)
(*  FILE REVISION: Rev 2                                   *)
(*  REVISION DATE: 04/15/2016  AT: 11:01:40                *)
(*                                                         *)
(*  COMMENTS:                                              *)
(*  Corrected debug message function naming                *)
(*                                                         *)
(***********************************************************)
(* System Type : NetLinx                                   *)
(***********************************************************)
(* REV HISTORY:                                            *)
(***********************************************************)
(*
	$History: $
*)

(*

	prime-connman-server
	NetLinx IP Listening Socket Manager
	
	Author: niek.groot@amxaustralia.com.au
	No rights or warranties implied.
	
	
	Usage:

	#DEFINE CONNMAN_SERVER_SET_MAX_LEN_BUFFER_IN  1500                          // Maximum size of the inbound buffer. Default: 2048.
	#INCLUDE 'prime-connman-server'                                             // Use connman_server_setProperty() to initialise configuration parameters

	connman_server_open()                                                       // Open the listening socket
	connman_server_close()                                                      // Close the listening socket


	Response:
	
	Strings received from the server are buffered in the 
	connman_server_buffer.in[CONNMAN_SERVER_MAX_LEN_BUFFER_IN] variable.

	
	Configuration:
	
	connman_server_setProperty('PORT', '<port>')                                // Listening port.
	connman_server_setProperty('PROTOCOL', '<IP_TCP|IP_UDP>')                   // Transport protocol using NetLinx transport protocol constants.
	connman_server_setProperty('AUTO_REOPEN', '<true|false>')                   // Automatically re-open the listening socket if it is closed. Default: true.
	connman_server_setProperty('OPEN_DELAY', '<msec>')                          // Delay between attempts to open the listening socket. Default: 5000ms.
	connman_server_reinitialize()                                               // Re-initialise and open the listening socket if AUTO_REOPEN
*)

(***********************************************************)
(*               CONSTANT DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_CONSTANT

#IF_DEFINED CONNMAN_SERVER_SET_MAX_LEN_BUFFER_IN
	integer         CONNMAN_SERVER_MAX_LEN_BUFFER_IN        = CONNMAN_SERVER_SET_MAX_LEN_BUFFER_IN;
#ELSE
	integer         CONNMAN_SERVER_MAX_LEN_BUFFER_IN        =  2048;
#END_IF

long                CONNMAN_SERVER_OPEN_TL                  =  1111;

integer             CONNMAN_SERVER_STATUS_CLOSED            =     0;
integer             CONNMAN_SERVER_STATUS_OPENING           =     1;
integer             CONNMAN_SERVER_STATUS_OPEN              =     2;

#IF_NOT_DEFINED __IP_PROTOCOL_STRINGS
	#DEFINE __IP_PROTOCOL_STRINGS

char                IP_PROTOCOL_STRINGS[3][8] = {
						'TCP',
						'UDP',
						'UDP_2WAY'
					}

#END_IF

(***********************************************************)
(*              DATA TYPE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_TYPE

structure _connman_server_config {
	integer         protocol;
	integer         port;
}

structure _connman_server_buffer {
	char        in[CONNMAN_MAX_LEN_BUFFER_IN];
}

(***********************************************************)
(*               VARIABLE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_VARIABLE

volatile integer        connman_server_auto_reopen          = true;

volatile _connman_server_config connman_server_config;
volatile _connman_server_buffer connman_server_buffer;

volatile long           connman_server_open_times[]         = { 0, 5000 }
volatile integer        connman_server_open_status          = CONNMAN_SERVER_STATUS_CLOSED;

(***********************************************************)
(*                     INCLUDES GO BELOW                   *)
(***********************************************************)

INCLUDE 'prime-debug';

(***********************************************************)
(*        SUBROUTINE/FUNCTION DEFINITIONS GO BELOW         *)
(***********************************************************)

define_function integer connman_server_setProperty(char key[], char value[]) {
	if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_setProperty', '(', key, ', ', value, ')'");
	
	switch (key) {
		case 'PORT': {
			connman_server_config.port = atoi(value);
		}
		case 'PROTOCOL': {
			connman_server_config.protocol = atoi(value);
		}
		case 'OPEN_DELAY': {
			connman_server_open_times[2] = atoi(value);
		}
		case 'AUTO_REOPEN': {
			connman_server_auto_reopen = stringtobool(value);
		}
		default: {
			return false;
		}
	}
	
	if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_setProperty', '() ', 'returning true'");
	return true;
}

define_function connman_server_reinitialize() {
	if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_reinitialize', '()'");
	
	if (connman_server_open_status == CONNMAN_SERVER_STATUS_CLOSED) {
		if (connman_server_auto_reopen) connman_server_open();
	} else {
		connman_server_close(); // connman_server_open() will be called automatically by offline event
	}
}

define_function connman_server_open() {
	if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_open', '()'");
	
	if (!connman_server_config.port) {
		if (AMX_INFO <= get_log_level()) debug(AMX_INFO, "'connman_server_open() ', 'listening port not configured'");
	}
	
	switch (connman_server_open_status) {
		case CONNMAN_SERVER_STATUS_CLOSED: {
			connman_server_open_status = CONNMAN_SERVER_STATUS_OPENING;
			
			if (AMX_INFO <= get_log_level()) debug(AMX_INFO, "'connman_server_open() ', 'opening listening socket on port ', itoa(connman_server_config.port), ' ', IP_PROTOCOL_STRINGS[connman_server_config.protocol]");
			ip_server_open(connman_server_device.port, connman_server_config.port, connman_server_config.protocol);
		}
		case CONNMAN_SERVER_STATUS_OPENING: {
			// Already opening socket
		}
		case CONNMAN_SERVER_STATUS_OPEN: {
			// Socket is already open
		}
		default: {
			if (AMX_ERROR <= get_log_level()) debug(AMX_ERROR, "'connman_server_open', '() ', 'unexpected socket status!'");
		}
	}
}

define_function connman_server_close() {
	if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_disconnect', '()'");
	
	switch (connman_server_open_status) {
		case CONNMAN_SERVER_STATUS_CLOSED: {
			if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_disconnect', '() ', 'listening socket already closed'");
		}
		case CONNMAN_SERVER_STATUS_OPENING: {
			if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_disconnect', '() ', 'cancelled attempt to open listening socket'");
			
			if (timeline_active(CONNMAN_SERVER_OPEN_TL)) timeline_kill(CONNMAN_SERVER_OPEN_TL);
			connman_server_open_status = CONNMAN_SERVER_STATUS_CLOSED;
		}
		case CONNMAN_SERVER_STATUS_OPEN: {
			if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'connman_server_disconnect', '() ', 'closing listening socket...'");
			
			ip_server_close(connman_server_device.port);
		}
	}
}

(***********************************************************)
(*                STARTUP CODE GOES BELOW                  *)
(***********************************************************)
DEFINE_START

create_buffer connman_server_device, connman_server_buffer.in;

(***********************************************************)
(*                THE EVENTS GO BELOW                      *)
(***********************************************************)
DEFINE_EVENT

data_event[connman_server_device] {
	online: {
		connman_server_open_status = CONNMAN_SERVER_STATUS_OPEN;
		
		if (AMX_INFO <= get_log_level()) debug(AMX_INFO, "'listening socket opened on port ', itoa(connman_server_config.port), ' ', IP_PROTOCOL_STRINGS[connman_server_config.protocol]");
	}
	offline: {
		connman_server_open_status = CONNMAN_SERVER_STATUS_CLOSED;
		
		if (AMX_INFO <= get_log_level()) debug(AMX_INFO, "'listening socket on port ', itoa(connman_server_config.port), ' ', IP_PROTOCOL_STRINGS[connman_server_config.protocol], ' closed'");
		
		if (connman_server_auto_reopen) connman_server_open();
	}
	onerror: {
		if (AMX_ERROR <= get_log_level()) debug(AMX_ERROR, "'cculd not open listening socket on port ', itoa(connman_server_config.port), ' ', IP_PROTOCOL_STRINGS[connman_server_config.protocol], ' (Error ', itoa(data.number), ' ', ip_error_desc(type_cast(data.number)), ')'");
		
		if (connman_server_open_status == CONNMAN_SERVER_STATUS_OPENING) {
			timeline_create(CONNMAN_SERVER_OPEN_TL, connman_server_open_times, length_array(connman_server_open_times), TIMELINE_RELATIVE, TIMELINE_ONCE);
		}
	}
	string: {
		if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'string received from ', data.sourceip, ' on port ', itoa(connman_server_config.port), ' ', IP_PROTOCOL_STRINGS[connman_server_config.protocol], ' (', itoa(length_string(data.text)), ' bytes): ', data.text");
	}
}

timeline_event[CONNMAN_SERVER_OPEN_TL] {
	switch(timeline.sequence) {
		case 1: {
			if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'CONNMAN_SERVER_OPEN_TL', '(', itoa(timeline.repetition), ', ', itoa(timeline.sequence), ') ', 'opening listening socket in ', ftoa(connman_server_open_times[2] / 1000), ' seconds...'");
		}
		case 2: {
			if (connman_server_open_status != CONNMAN_SERVER_OPEN_STATUS) {
				if (AMX_DEBUG <= get_log_level()) debug(AMX_DEBUG, "'CONNMAN_SERVER_OPEN_TL', '(', itoa(timeline.repetition), ', ', itoa(timeline.sequence), ') ', 'opening listening socket on port ', itoa(connman_server_config.port), ' ', IP_PROTOCOL_STRINGS[connman_server_config.protocol]");
				ip_server_open(connman_server_device.port, connman_server_config.port, connman_server_config.protocol);
			}
		}
	}
}

#END_IF

