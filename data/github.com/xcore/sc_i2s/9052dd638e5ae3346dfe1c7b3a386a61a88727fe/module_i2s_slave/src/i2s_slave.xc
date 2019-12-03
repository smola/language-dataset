// Copyright (c) 2011, XMOS Ltd., All rights reserved
// This software is freely distributable under a derivative of the
// University of Illinois/NCSA Open Source License posted in
// LICENSE.txt and at <http://github.xcore.com/>

///////////////////////////////////////////////////////////////////////////////
//
// Multichannel I2S_SLAVE slave receiver-transmitter

#include <xs1.h>
#include <xclib.h>
#include "i2s_slave.h"

#pragma unsafe arrays
void i2s_slave_loop(in buffered port:32 din[], out buffered port:32 dout[], streaming chanend c_in, streaming chanend c_out, in port wck)
{
  int lr = 0;
	unsigned frame_counter = 0;

  while (1) {
    int t;

    // wait for WCK edge
    // timestamp this edge
    wck when pinsneq(lr) :> lr @ t;

    // set time for audio data input
    // split SETPT from IN using asm
    // basically a split transaction to allow multichannel timed input
    // input is always "up to" given time
    // I2S_SLAVE sample starts at t + 1, so capture "up to" t + 1 + 23
#pragma loop unroll
    for (int i = 0; i < I2S_SLAVE_NUM_OUT; i++) {
      asm("setpt res[%0], %1" :: "r"(din[i]), "r"(t + 24));
    }

    // output audio data
    // output is always "starting from" given time
    // next I2S_SLAVE sample starts at t + 33, so output at that time
    // must do partial output of 24 bits
    // full 32-bit output would span entire cycle not allowing consecutive SETPT's
#pragma loop unroll
    for (int i = 0; i < I2S_SLAVE_NUM_OUT; i++) {
      signed x = 0;
      c_out :> x;
      partout_timed(dout[i],24,bitrev(x),(t + 33));
    }

    // input audio data
    // port will capture I2S MSb at t + 1 and LSb at t + 24
    // bits 0..7 are older than I2S MSb and hence discard them
    // compiler would insert SETC FULL on DIN input, because it doesn't know about inline SETPT above
    // hence we need inline IN too
#pragma loop unroll
    for (int i = 0; i < I2S_SLAVE_NUM_IN; i++) {
      signed x;
			asm("in %0, res[%1]" : "=r"(x)  : "r"(din[i]));
      c_in <: bitrev(x) << 8;
    }

    frame_counter++;
  }
}

void i2s_slave(struct i2s_slave &r_i2s_slave, streaming chanend c_in, streaming chanend c_out)
{
  // clock block clocked off external BCK
  set_clock_src(r_i2s_slave.cb, r_i2s_slave.bck);

  // WCK and all data ports clocked off BCK
  set_port_clock(r_i2s_slave.wck, r_i2s_slave.cb);
  for (int i = 0; i < I2S_SLAVE_NUM_IN; i++)
  {
    set_port_clock(r_i2s_slave.din[i], r_i2s_slave.cb);
  }
  for (int i = 0; i < I2S_SLAVE_NUM_OUT; i++)
  {
    set_port_clock(r_i2s_slave.dout[i], r_i2s_slave.cb);
  }

  // start clock block after configuration
  start_clock(r_i2s_slave.cb);

  // fast mode - instructions repeatedly issued instead of paused
  set_thread_fast_mode_on();

  i2s_slave_loop(r_i2s_slave.din, r_i2s_slave.dout, c_in, c_out, r_i2s_slave.wck);

  set_thread_fast_mode_off();
}
