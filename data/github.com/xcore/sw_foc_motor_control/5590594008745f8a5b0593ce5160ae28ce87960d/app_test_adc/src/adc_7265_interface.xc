/**
 * The copyrights, all other intellectual and industrial
 * property rights are retained by XMOS and/or its licensors.
 * Terms and conditions covering the use of this code can
 * be found in the Xmos End User License Agreement.
 *
 * Copyright XMOS Ltd 2013
 *
 * In the case where this code is a modification of existing code
 * under a separate license, the separate license terms are shown
 * below. The modifications to the code are still covered by the
 * copyright notice above.
 **/

#include "adc_7265_interface.h"

/*****************************************************************************/
static void configure_test_ports( // Configure all ADC data ports
	out buffered port:32 pb32_tst_data[NUM_ADC_DATA_PORTS], // Array of 32-bit buffered ADC data ports
	in port p1_tst_ready,	 // 1-bit port used to as ready signal for p32_adc_data ports and ADC chip
	in port p1_tst_sclk,  // 1-bit port used for serial clock
	clock tst_xclk // Internal XMOS clock
)
{
	int port_cnt; // port counter


	/* p1_tst_sclk & p1_tst_ready, are used as the clock & ready signals respectively for controlling the following 2 functions:-
		(1) Reading the Digital data from the AD7265 into an XMOS buffered 1-bit port
		(2) Initiating an Analogue-to-digital conversion on the AD7265 chip.

		The xclk is configured in the the ADC module (ADC_7265.xc)
		For (1), Referring to XMOS XS1 library documentation ...
		By default, the ports are read on the rising edge of the clock, and when the ready signal is high.

		For (2), Referring to the  AD7265 data-sheet ...
		p1_tst_ready is used to control CSi (Chip Select Inverted)
		When signal CSi falls, ( 1 -> 0 ) A2D conversion starts. When CSi rises ( 0 -> 1 ), conversion halts.
    The digital outputs are tri-state when CSi is high (1).
	*/

	configure_clock_src( tst_xclk ,p1_tst_sclk ); // Uses serial clock on input port as clock

	configure_in_port( p1_tst_ready ,tst_xclk ); // Set initial value of port to 0 ( NOT ready )

	set_port_inv( p1_tst_ready ); // Invert p1_tst_ready as AD7265 has an active low

	// For each port, configure to read into buffer when using the serial clock
	for (port_cnt=0; port_cnt<NUM_ADC_DATA_PORTS; port_cnt++)
	{
		configure_out_port_strobed_slave( pb32_tst_data[port_cnt] ,p1_tst_ready ,tst_xclk ,0x0 );
	} // for port_cnt

	start_clock( tst_xclk ); // Start the test clock
} // configure_test_ports
/*****************************************************************************/
void adc_7265_interface( // Generate ADC Test data for all motors
	chanend c_adc_trig[], // Array of channels for sending PWM-to_ADC trigger pulse
	streaming chanend c_gen, // Channel for communication with Test_Generator core
	buffered out port:32 pb32_tst_data[],	// Array of ADC data ports for transmitting raw ADC values (NB For Phase_A and Phase_B)
	in port p1_tst_ready, // 1-bit port used for ready signal
	in port p1_tst_sclk,  // 1-bit port used for serial clock
	clock tst_xclk // Internal XMOS clock
)
{
	int port_cnt; // port counter
	int motor_id; // Motor Identifier
	ADC_TYP adc_val; // ADC value
	unsigned out_val;


	configure_test_ports( pb32_tst_data ,p1_tst_ready ,p1_tst_sclk ,tst_xclk );

	c_gen :> motor_id; // Get identifier for motor under test

	// loop forever
	while(1)
	{
		// For each port, configure to read into buffer when using the serial clock
		for (port_cnt=0; port_cnt<NUM_ADC_DATA_PORTS; port_cnt++)
		{
			c_gen :> adc_val; // Receive a standardised ADC value (Currently 24-bit)
			out_val = bitrev((unsigned)adc_val); // Reverse bit-order as ADC_7265 transmits MSB first
			out_val >>= SHIFT_BITS; // Shift active bits to LSB end of integer (as XMOS port transmits LSB first)

			// Place ADC-bits on output ready to send
			partout( pb32_tst_data[port_cnt] ,ADC_TOTAL_BITS ,out_val ); // NB Partial Output (ADC_TOTAL_BITS <= 32)
		} // for port_cnt

		// Send synchronisation token to correct motor to initiate ADC sampling
		outct( c_adc_trig[motor_id] ,XS1_CT_END );

		// Wait for ready signal signalling ADC started
		p1_tst_ready when pinseq(0) :> void;

		// NB raw ADC values on output ports are now clocked out

		// Wait for ready signal signalling ADC finished
		p1_tst_ready when pinseq(1) :> void;
	} // while(1)

} // adc_7265_interface
/*****************************************************************************/
