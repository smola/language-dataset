/**
 * The copyrights, all other intellectual and industrial
 * property rights are retained by XMOS and/or its licensors.
 * Terms and conditions covering the use of this code can
 * be found in the Xmos End User License Agreement.
 *
 * Copyright XMOS Ltd 2012
 *
 * In the case where this code is a modification of existing code
 * under a separate license, the separate license terms are shown
 * below. The modifications to the code are still covered by the
 * copyright notice above.
 *
 **/

/*---------------------------------------------------------------------------
 include files
 ---------------------------------------------------------------------------*/
#include <xccompat.h>

/*---------------------------------------------------------------------------
 Send data to CANOpen stack from the application
 ---------------------------------------------------------------------------*/
void canopen_client_send_data_to_stack(streaming chanend c_application,
                                               unsigned char pdo_number,
                                               unsigned char data_length,
                                               unsigned char data[])
{
  char counter = 0;
  c_application<: pdo_number;
  c_application<: data_length;
  while(counter != data_length)
  {
    c_application<: data[(int)counter];
    counter++;
  }
}

/*---------------------------------------------------------------------------
 Receive application Data from the CANOpen stack
 ---------------------------------------------------------------------------*/
void canopen_client_receive_data_from_stack(streaming chanend c_application,
                                                    REFERENCE_PARAM(unsigned char, data_length),
                                                    NULLABLE_ARRAY_OF(unsigned char, data))
{
  char counter = 0;
  c_application:> data_length;
  while(counter != data_length)
  {
    c_application:> data[(int)counter];
    counter++;
  }
}
