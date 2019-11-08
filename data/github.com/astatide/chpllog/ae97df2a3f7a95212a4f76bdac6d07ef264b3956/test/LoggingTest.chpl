config const testParam: bool = true;

use Logging;

if testParam {

  var log = new owned Logging.Logger();
  log.currentDebugLevel = 0;
  var chmain = new Logging.logHeader();
  chmain.header = 'stdout';

  log.header('Test of logging infrastructure', chmain);
  log.debug('Starting %i tasks'.format(6), chmain);

  forall v in 1..6 {
    var ch = new Logging.logHeader();
    ch.id = v : string; // Cast it to a string
    ch.header = 'V%i'.format(v); // now, any subsequent calls with this ch will be written to V%s.log
    log.header('TESTING TASK ID:', v : string, ch);
    log.log('Hello, world, from log file V%i!'.format(v), ch);
    mainFunction(ch);
    log.critical('END - Normally, only use me for failure events.', ch);
  }

  log.log('Ending!', chmain);

  // We can also use this to set a stack trace.  We can pass around ch and add
  // strings to it to let us know what function is calling.

  proc mainFunction(in ch: Logging.logHeader) throws {
    ch += 'mainFunction';
    log.log('Calling!', ch);
    secondaryFunction(ch);
    log.log('Calling!', ch);

  }

  proc secondaryFunction(in ch: Logging.logHeader) {
    ch += 'secondaryFunction';
    log.log('Calling!', ch);

  }

}
