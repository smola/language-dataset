// TempBug Example Device Code

/* GLOBALS and CONSTANTS -----------------------------------------------------*/

// all calculations are done in Kelvin
// these are constants for this particular thermistor; if using a different one,
// check your datasheet
const b_therm = 3988;
const t0_therm = 298.15;
const WAKEINTERVAL_MIN = 2; // interval between wake-and-reads in minutes

/* CLASS AND GLOBAL FUNCTION DEFINITIONS -------------------------------------*/
/* report firmware version*/

function start_version()
{
    server.log("Device started");
    if ("getsoftwareversion" in imp)
    {
        server.log("Device firmware version: " + imp.getsoftwareversion());
    }
    else
    {
        server.log("Sorry - my firmware doesn't include imp.getsoftwareversion() yet.");
    }
}

start_version();

/*
 * simple NTC thermistor
 *
 * Assumes thermistor is the high side of a resistive divider unless otherwise specified in constructor.
 * Low-side resistor is of the same nominal resistance as the thermistor
 */
class thermistor {

    // thermistor constants are shown on your thermistor datasheet
    // beta value (for the temp range your device will operate in)
    b_therm = null;
    t0_therm = null;
    // analog input pin
    p_therm = null;
    points_per_read = null;

    high_side_therm = null;

    constructor(pin, b, t0, points = 10, _high_side_therm = true) {
        this.p_therm = pin;
        this.p_therm.configure(ANALOG_IN);

        // force all of these values to floats in case they come in as integers
        this.b_therm = b * 1.0;
        this.t0_therm = t0 * 1.0;
        this.points_per_read = points * 1.0;
        this.high_side_therm = _high_side_therm;
    }

    // read thermistor in Kelvin
    function read() {
        local vrat_raw = 0;
        for (local i = 0; i < points_per_read; i++) {
            vrat_raw += p_therm.read();
            imp.sleep(0.001); // sleep to allow thermistor pin to recharge
        }
        local v_rat = vrat_raw / (points_per_read * 65535.0);

        local ln_therm = 0;
        if (high_side_therm) {
            ln_therm = math.log(v_rat / (1.0 - v_rat));
        } else {
            ln_therm = math.log((1.0 - v_rat) / v_rat);
        }

        return (t0_therm * b_therm) / (b_therm - t0_therm * ln_therm);
    }

    // read thermistor in Celsius
    function read_c() {
        return this.read() - 273.15;
    }

    // read thermistor in Fahrenheit
    function read_f() {
        return ((this.read() - 273.15) * 9.0 / 5.0 + 32.0);
    }
}

/* REGISTER AGENT CALLBACKS --------------------------------------------------*/

/* RUNTIME BEGINS HERE -------------------------------------------------------*/

// Configure Pins
// pin 8 is driven high to turn off temp monitor (saves power) or low to read
therm_en_l <- hardware.pin8;
therm_en_l.configure(DIGITAL_OUT);
therm_en_l.write(1);
// pin 9 is the middle of the voltage divider formed by the NTC - read the analog voltage to determine temperature
temp_sns <- hardware.pin9;

// instantiate our thermistor class
myThermistor <- thermistor(temp_sns, b_therm, t0_therm, 10, false);

therm_en_l.write(0);
imp.sleep(0.001);
local id = hardware.getdeviceid();
local datapoint = {
    "id" : id,
    "temp" : format("%.2f",myThermistor.read_f()),
}
agent.send("data",datapoint);
therm_en_l.write(1);

//Sleep for 15 minutes and 1 second, minus the time past the 0:15
//so we wake up near each 15 minute mark (prevents drifting on slow DHCP)
imp.onidle( function() {
    //server.sleepfor(1 + 15 - (time() % (WAKEINTERVAL_MIN*15)));
    server.sleepfor(1 + WAKEINTERVAL_MIN*60 - (time() % (WAKEINTERVAL_MIN*60)));
});

// full firmware is reloaded and run from the top on each wake cycle, so no need to construct a loop
