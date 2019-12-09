// Copyright (c) 2015, XMOS Ltd, All rights reserved
#include "i2c.h"
#include "xs1.h"
#include "debug_print.h"

enum i2c_slave_state {
  WAITING_FOR_START_OR_STOP,
  READING_ADDR,
  ACK_ADDR,
  MASTER_WRITE,
  MASTER_READ
};

[[combinable]]
void i2c_slave(client i2c_slave_callback_if i,
               port p_scl, port p_sda,
               uint8_t device_addr)
{
  enum i2c_slave_state state = WAITING_FOR_START_OR_STOP;
  int sda_val = 0;
  int scl_val;
  int bitnum = 0;
  int data;
  int rw = 0;
  int stop_bit_check = 0;
  int ignore_stop_bit = 1;
  p_sda when pinseq(1) :> void;
  while (1) {
    select {
    case i.shutdown():
      return;
    case state != WAITING_FOR_START_OR_STOP => p_scl when pinseq(scl_val) :> void:
      switch (state) {
      case READING_ADDR:
        // If clock has gone low, wait for it to go high before doing anything
        if (scl_val == 0) {
          scl_val = 1;
          break;
        }
        int bit;
        p_sda :> bit;
        if (bitnum < 7) {
          data = (data << 1) | bit;
          bitnum++;
          scl_val = 0;
          break;
        }
        // We have gathered the whole device address sent by the master.
        if (data != device_addr) {
          // No match, just wait for the next start bit.
          state = WAITING_FOR_START_OR_STOP;
          sda_val = 0;
          break;;
        }
        state = ACK_ADDR;
        scl_val = 0;
        rw = bit;
      break;
      case ACK_ADDR:
        int ack;
        p_scl <: 0;
        // Callback to the application to determine whether to ACK
        // or NACK the address.
        if (rw) {
          i.start_write_request();
          ack = i.ack_write_request();
        } else {
          i.start_read_request();
          ack = i.ack_read_request();
        }
        ignore_stop_bit = 0;
        if (ack == I2C_SLAVE_NACK) {
          p_sda :> void;
          state = WAITING_FOR_START_OR_STOP;
          sda_val = 0;
        } else if (rw) {
          p_sda <: 0;
          state = MASTER_WRITE;
          data = 0;
          scl_val = 1;
          bitnum = 0;
        } else {
          p_sda <: 0;
          state = MASTER_READ;
          scl_val = 1;
          bitnum = 0;
        }
        p_scl :> void;
      break;
      case MASTER_READ:
        if (scl_val == 1 && bitnum == 9) {
          int bit;
          p_sda :> bit;
          if (bit) {
            // Master has NACKed so the transaction is finished
            state = WAITING_FOR_START_OR_STOP;
            sda_val = 0;
          } else {
            bitnum = 0;
            scl_val = 0;
          }
        } else if (scl_val == 1) {
          scl_val = 0;
        } else {
          if (bitnum < 8) {
            if (bitnum == 0) {
              p_scl <: 0;
              i.start_master_read();
              data = i.master_requires_data();
              p_scl :> void;
            }
            int bit = data >> 7;
            p_sda <: data >> 7;
            data <<= 1;
          } else {
            p_sda :> void;
          }
          bitnum++;
          scl_val = 1;
        }
        break;
      case MASTER_WRITE:
        if (scl_val == 1) {
          int bit;
          if (bitnum == 0) {
            scl_val = 0;
          } else if (bitnum == 9) {
            state = WAITING_FOR_START_OR_STOP;
            p_sda :> sda_val;
            sda_val = 1-sda_val;
          } else {
            p_sda :> bit;
            data = (data << 1) | bit;
            if (bit == 0) {
              sda_val = 1;
              stop_bit_check = 1;
            }
          }
          scl_val = 0;
          bitnum++;
        } else if (bitnum == 9) {
          p_scl <: 0;
          stop_bit_check = 0;
          i.start_master_write();
          int ack = i.master_sent_data(data);
          if (ack == I2C_SLAVE_NACK) {
            p_sda :> void;
          } else {
            p_sda <: 0;
            data = 0;
            bitnum = 0;
          }
          scl_val = 1;
          p_scl :> void;
        } else {
          stop_bit_check = 0;
          p_sda :> void;
          scl_val = 1;
        }
        break;
      }
      break;
    case (state == WAITING_FOR_START_OR_STOP) || stop_bit_check =>
            p_sda when pinseq(sda_val) :> void:
      //      debug_printf("%d\n", sda_val);
      if (sda_val == 1) {
        // SDA has transitioned from low to high, if SCL is high
        // then it is a stop bit.
        int val;
        p_scl :> val;
        if (val) {
          if (!ignore_stop_bit)
            i.stop_bit();
          state = WAITING_FOR_START_OR_STOP;
          ignore_stop_bit = 1;
          sda_val = 0;
        } else {
          sda_val = 0;
        }
      } else {
        // SDA has transitioned from high to low, if SCL is high
        // then it is a start bit.
        int val;
        p_scl :> val;
        if (val == 1) {
          state = READING_ADDR;
          bitnum = 0;
          data = 0;
          scl_val = 0;
          stop_bit_check = 0;
        } else {
          sda_val = 1;
        }
      }
      break;
    }
  }
}