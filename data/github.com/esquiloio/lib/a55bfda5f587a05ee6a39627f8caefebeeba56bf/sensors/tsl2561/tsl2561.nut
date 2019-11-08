/////////////////////////////////////////////////////////////////////////////
// TAOS TLS2561 light sensor class
//
// This work is released under the Creative Commons Zero (CC0) license.
// See http://creativecommons.org/publicdomain/zero/1.0/
//
/////////////////////////////////////////////////////////////////////////////
//
// Example usage (works with Grove digital light sensor):
//
// lightSensor <- TSL2561(I2C(0));
//
// while(true) {
//    print(format("lux: %0.2f\n", lightSensor.getLux()));
//    delay(500);
// }
//

// I2C address options
const TSL2561_ADDR_LOW   = 0x29;
const TSL2561_ADDR_FLOAT = 0x39;
const TSL2561_ADDR_HIGH  = 0x49;

// Lux calculations differ slightly based on the package type
const TSL2561_PACKAGE_T_FN_CL = 0;
const TSL2561_PACKAGE_CS      = 1;

// Integration time settings
const TSL2561_INTEGRATIONTIME_13MS  = 0x00;
const TSL2561_INTEGRATIONTIME_101MS = 0x01;
const TSL2561_INTEGRATIONTIME_402MS = 0x02;

// Gain settings
const TSL2561_GAIN_1X  = 0x00;
const TSL2561_GAIN_16X = 0x10;

const TSL2561_WORD_BIT    = 0x20;
const TSL2561_COMMAND_BIT = 0x80;
    
const TSL2561_REGISTER_CONTROL    = 0x00;
const TSL2561_REGISTER_TIMING     = 0x01;
const TSL2561_REGISTER_ID         = 0x0A;
const TSL2561_REGISTER_CHAN0_LOW  = 0x0C;
const TSL2561_REGISTER_CHAN1_LOW  = 0x0E;

const TSL2561_CONTROL_POWEROFF = 0x00;
const TSL2561_CONTROL_POWERON  = 0x03;

//
// TSL2561 light sensor class
//
class TSL2561
{
    _i2c             = null;
    _package         = TSL2561_PACKAGE_T_FN_CL;
    _integrationTime = TSL2561_INTEGRATIONTIME_402MS;
    _gain            = TSL2561_GAIN_1X;
}

// Private

//
// Power on the device
//
function TSL2561::_enable()
{
  _i2c.write8(TSL2561_COMMAND_BIT | TSL2561_REGISTER_CONTROL,
              TSL2561_CONTROL_POWERON);
}

//
// Turn the device off to save power
//
function TSL2561::_disable()
{
  _i2c.write8(TSL2561_COMMAND_BIT | TSL2561_REGISTER_CONTROL,
              TSL2561_CONTROL_POWEROFF);
}

//
// The data registers are little-endian. Read a word and return
// the byte-swapped (i.e. big-endian) value.
//
function TSL2561::_read16ByteSwap(command)
{
    local word = _i2c.read16(command);
    
    local lowByte = word & 0xff;
    local highByte = (word & 0xff00) >> 8;
    
    return (lowByte << 8) | highByte; 
}

//
// Read both ADC channel data registers, scale them based on the
// integration time and gain settings, and return them in an
// array. The scaling is done based on a 402ms integration time
// and 16x gain.
//
function TSL2561::_getData()
{
    const TSL2561_DELAY_INTTIME_13MS  = 15;
	const TSL2561_DELAY_INTTIME_101MS = 120;
	const TSL2561_DELAY_INTTIME_402MS = 450;
    
    // Turn the device on
    _enable();

    // Wait for ADC to complete
    switch (_integrationTime)
    {
        case TSL2561_INTEGRATIONTIME_13MS:
            delay(TSL2561_DELAY_INTTIME_13MS);
            break;
        case TSL2561_INTEGRATIONTIME_101MS:
            delay(TSL2561_DELAY_INTTIME_101MS);
            break;
        default:
            delay(TSL2561_DELAY_INTTIME_402MS);
            break;
    }
    
    // ADC channels
    local channel = array(2);

    // Reads a two byte value from channel 0 (visible + infrared)
    channel[0] = _read16ByteSwap(
        TSL2561_COMMAND_BIT | TSL2561_WORD_BIT | TSL2561_REGISTER_CHAN0_LOW);

    // Reads a two byte value from channel 1 (infrared)
    channel[1] = _read16ByteSwap(
        TSL2561_COMMAND_BIT | TSL2561_WORD_BIT | TSL2561_REGISTER_CHAN1_LOW);
    
    // Convert the values to floating-point
    channel[0] = channel[0].tofloat();
    channel[1] = channel[1].tofloat();
    
    // Normalize the values based on a 16x gain
    if (_gain == TSL2561_GAIN_1X) {
        channel[0] *= 16.0;
        channel[1] *= 16.0;
    }
    
    // Normalize the values based on a 402ms integration time
    local integrationTimeScale;
	switch (_integrationTime)
    {
        case TSL2561_INTEGRATIONTIME_13MS:
            integrationTimeScale = 322.0/11.0;
            break;
        case TSL2561_INTEGRATIONTIME_101MS:
            integrationTimeScale = 322.0/81.0;
            break;
        default:
            integrationTimeScale = 1.0;
            break;
    }
    channel[0] *= integrationTimeScale;
    channel[1] *= integrationTimeScale;

    // Turn the device off to save power
    _disable();
    
    // Return the scaled channel values
    return channel;
}

// Public

//
// Create an instance of the light sensor on the I2C bus
// passed in, at the address and with the package type
// specified.
//
// Arguments:
//  - i2c:     I2C instance of the bus the sensor is
//             connected to.
//  - addr:    I2C address of the sensor. This depends on
//             how the chip is strapped. Valid values:
//             * TSL2561_ADDR_LOW (default)
//             * TSL2561_ADDR_FLOAT
//             * TSL2561_ADDR_HIGH
//  - package: The package type of the chip used.
//             Valid values:   
//             * TSL2561_PACKAGE_T_FN_CL (default)
//             * TSL2561_PACKAGE_CS 
// 
function TSL2561::constructor(i2c, 
                              addr=TSL2561_ADDR_LOW,
                              package=TSL2561_PACKAGE_T_FN_CL)
{
    _i2c     = i2c;
    _i2c.address(addr);
    _package = package;
    
    // Make sure the TSL2561 is there
    _enable();
    if (!(_i2c.read8(TSL2561_REGISTER_ID) & 0x0A))
        throw("TSL2561 device not found");
    _disable();

    // Set defaults
    setIntegrationTime(TSL2561_INTEGRATIONTIME_402MS);
    setGain(TSL2561_GAIN_1X);
}

//
// Sample the ADC registers and calculate the standard SI
// lux equivalent from them. The formulas are straight from
// the datasheet.
//
function TSL2561::getLux()
{
    local channel = _getData();
    local ratio   = channel[1] / channel[0];
    local lux     = 0.0;
    
    if (_package == TSL2561_PACKAGE_T_FN_CL) {
        if (ratio <= 0.50)
            lux = 0.0304 * channel[0] - 0.062 * channel[0] * pow(ratio, 1.4);
        else if (ratio <= 0.61)
            lux = 0.0224 * channel[0] - 0.031 * channel[1];
        else if (ratio <= 0.80)
            lux = 0.0128 * channel[0] - 0.0153 * channel[1];
        else if (ratio <= 1.30)
            lux = 0.00146 * channel[0] - 0.00112 * channel[1];
    } else {
        if (ratio <= 0.52)
            lux = 0.0315 * channel[0] - 0.0593 * channel[0] * pow(ratio, 1.4);
        else if (ratio <= 0.65)
            lux = 0.0229 * channel[0] - 0.0291 * channel[1];
        else if (ratio <= 0.80)
            lux = 0.0157 * channel[0] - 0.0180 * channel[1];
        else if (ratio <= 1.30)
            lux = 0.00338 * channel[0] - 0.00260 * channel[1];
    }
    
    return lux;
}

//
// Set the integration time to one of these values:
// * TSL2561_INTEGRATIONTIME_13MS   (13 ms)
// * TSL2561_INTEGRATIONTIME_101MS  (101 ms)
// * TSL2561_INTEGRATIONTIME_402MS  (402 ms)
//
// Note the longer the integration time, the more precision,
// but also more delay.
//
function TSL2561::setIntegrationTime(integrationTime)
{
    _enable();
    _i2c.write8(TSL2561_COMMAND_BIT | TSL2561_REGISTER_TIMING,
                integrationTime | _gain);
    _disable();
    _integrationTime = integrationTime;
}

//
// Set the gain to one of these values:
// * TSL2561_GAIN_1X   (no gain)
// * TSL2561_GAIN_16X  (signal * 16)
// 
function TSL2561::setGain(gain)
{
    _enable();
    _i2c.write8(TSL2561_COMMAND_BIT | TSL2561_REGISTER_TIMING,
                _integrationTime | gain);
    _disable();
    _gain = gain;
}

