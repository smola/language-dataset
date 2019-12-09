#include "jtag_otp_access.h"
#include "jtag.h"
#include <xclib.h>

/* Test port mode register. */

#define MODE_SELECT_SHIFT 0
#define MODE_SELECT_SIZE 1
#define MODE_SELECT_MASK (((1 << MODE_SELECT_SIZE) - 1) << MODE_SELECT_SHIFT)

#define MODE_SELECT_ENABLE_VAL 1
#define MODE_SELECT_DISABLE_VAL 0

#define MODE_SELECT_ENABLE (MODE_SELECT_ENABLE_VAL << MODE_SELECT_SHIFT)
#define MODE_SELECT_DISABLE (MODE_SELECT_DISABLE_VAL << MODE_SELECT_SHIFT)

#define TSO_SELECTOR_SHIFT 1
#define TSO_SELECTOR_SIZE 2
#define TSO_SELECTOR_MASK (((1 << TSO_SELECTOR_SIZE) - 1) << TSO_SELECTOR_SHIFT)

#define TSO_SELECTOR_STATUS_VAL 0
#define TSO_SELECTOR_SO_VAL 1
#define TSO_SELECTOR_SR_VAL 2

#define TSO_SELECTOR_STATUS (TSO_SELECTOR_STATUS_VAL << TSO_SELECTOR_SHIFT)
#define TSO_SELECTOR_SO (TSO_SELECTOR_SO_VAL << TSO_SELECTOR_SHIFT)
#define TSO_SELECTOR_SR (TSO_SELECTOR_SR_VAL << TSO_SELECTOR_SHIFT)

#define BURST_MODE_SHIFT 3
#define BURST_MODE_SIZE 2
#define BURST_MODE_MASK (((1 << BURST_MODE_SIZE) - 1) << BURST_MODE_SHIFT)

#define BURST_MODE_NONE_VAL 0

#define BURST_MODE_NONE (BURST_MODE_NONE_VAL << BURST_MODE_SHIFT)

#define AUX_UPDATE_SHIFT 8
#define AUX_UPDATE_SIZE 1
#define AUX_UPDATE_MASK (((1 << AUX_UPDATE_SIZE) - 1) << AUX_UPDATE_SHIFT)

#define AUX_UPDATE_ENABLE_VAL 1
#define AUX_UPDATE_DISABLE_VAL 0

#define AUX_UPDATE_ENABLE (AUX_UPDATE_ENABLE_VAL << AUX_UPDATE_SHIFT)
#define AUX_UPDATE_DISABLE (AUX_UPDATE_DISABLE_VAL << AUX_UPDATE_SHIFT)

#define MACRO_SELECT_SHIFT 9
#define MACRO_SELECT_SIZE 1
#define MACRO_SELECT_MASK (((1 << MACRO_SELECT_SIZE) - 1) << MACRO_SELECT_SHIFT)

#define MACRO_SELECT_ENABLE_VAL 1
#define MACRO_SELECT_DISABLE_VAL 0

#define MACRO_SELECT_ENABLE (MACRO_SELECT_ENABLE_VAL << MACRO_SELECT_SHIFT)
#define MACRO_SELECT_DISABLE (MACRO_SELECT_DISABLE_VAL << MACRO_SELECT_SHIFT)

#define MODE_REGISTER_LEN 6

/* Test port command register. */
enum {
  TP_IDLE = 0x0,
  TP_DIRECT = 0x1,
  TP_SHIFT = 0x2,
  TP_UPDATE_MODE = 0x3,
  TP_CAPTURE = 0x4,
  TP_ROTATE = 0x5,
  TP_UPDATE_CMD = 0x6,
  TP_INC_ADDR = 0x7
};
#define CMD_REGISTER_LEN 6

/* Macro command register. */
#define COMP_SHIFT 0
#define PCH_SHIFT 1
#define PGM_SHIFT 2
#define READ_SHIFT 3
#define WRITE_SHIFT 4
#define RESET_SHIFT 5

#define COMP (1 << COMP_SHIFT)
#define PCH (1 << PCH_SHIFT)
#define PGM (1 << PGM_SHIFT)
#define READ (1 << READ_SHIFT)
#define WRITE (1 << WRITE_SHIFT)
#define RESET (1 << RESET_SHIFT)

#define DONTCARE 0

//#define ENABLE_DEBUG
#ifdef ENABLE_DEBUG
#include <stdio.h>
#define DEBUG(x) x
#else
#define DEBUG(x)
#endif

/* Test port commands. */
static inline void update_command(int chipmodule)
{
  DEBUG(printf("update_command()\n");)
  jtag_module_otp_write_test_port_cmd(chipmodule, TP_UPDATE_CMD);
}

static inline void update_mode(int chipmodule)
{
  DEBUG(printf("update_mode()\n");)
  jtag_module_otp_write_test_port_cmd(chipmodule, TP_UPDATE_MODE);
}

static unsigned direct_access(int chipmodule, unsigned data)
{
  DEBUG(printf("direct_access()\n");)
  jtag_module_otp_write_test_port_cmd(chipmodule, TP_DIRECT);
  return jtag_module_otp_shift_data(chipmodule, data);
}

static inline void idle(int chipmodule)
{
  DEBUG(printf("idle()\n");)
  jtag_module_otp_write_test_port_cmd(chipmodule, TP_IDLE);
}

static unsigned shift(int chipmodule, unsigned data)
{
  DEBUG(printf("shift() data=0x%x\n", data);)
  jtag_module_otp_write_test_port_cmd(chipmodule, TP_SHIFT);
  data = jtag_module_otp_shift_data(chipmodule, bitrev(data));
  return bitrev(data);
}

/* Helper functions. */
static void write_mode(int chipmodule, unsigned value, unsigned address)
{
  DEBUG(printf("write_mode() value=0x%x address=0x%x\n", value, address);)
  unsigned data = value | (address << MODE_REGISTER_LEN);
  shift(chipmodule, data);
  update_mode(chipmodule);
}

static void write_command(int chipmodule, unsigned command, unsigned address)
{
  DEBUG(printf("write_command() command=0x%x address=0x%x\n", command, address);)
  unsigned data = command | (address << CMD_REGISTER_LEN);
  shift(chipmodule, data);
  update_command(chipmodule);
}

/* OTP functions */
unsigned jtag_otp_read_word(int chipmodule, unsigned address)
{
  DEBUG(printf("jtag_otp_read_word() address=0x%x\n", address);)
  write_mode(chipmodule, MODE_SELECT_DISABLE | TSO_SELECTOR_SO |
                         BURST_MODE_NONE | MACRO_SELECT_ENABLE, 0);
  write_command(chipmodule, READ, address);
  idle(chipmodule);
  return direct_access(chipmodule, DONTCARE);
}
