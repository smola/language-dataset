//::declaration
#include <stdio.h>
#include <xs1.h>
#include "i2c.h"

struct r_i2c i2c = {XS1_PORT_4F};
port gpio = XS1_PORT_4E;
//::

//::main program

int main(void)
{
  unsigned char data[1];
  int x;
  int i;

  i2c_master_init(i2c);

  gpio <: 0xF;

  data[0] = 0xFF;
  for(int j = 0; j < 1000000; j++) {
      for(i = 0x90; i < 0x9E; i += 2) {
          if (i == 0x94) i = 0x9C;
          data[0] = ~i;
          x = i2c_master_write_reg(i, 7, data, 1, i2c);
      }
      for(i = 0x90; i < 0x9E; i += 2) {
          if (i == 0x94) i = 0x9C;
          data[0] = i;
          x = i2c_master_write_reg(i, 8, data, 1, i2c);
      }
  }
  return 0;
}
//::
