Module: tracing-core
Synopsis: time-related utilities
Author: Bruce Mitchener, Jr.
Copyright: See LICENSE file in this distribution.

define class <duration> (<object>)
  constant slot duration-seconds :: <integer>,
    required-init-keyword: seconds:;
  constant slot duration-microseconds :: <integer>,
    required-init-keyword: microseconds:;
end class <duration>;

define class <timestamp> (<object>)
  constant slot timestamp-days :: <integer> = 0,
    init-keyword: days:;
  constant slot timestamp-seconds :: <integer>,
    required-init-keyword: seconds:;
  constant slot timestamp-microseconds :: <integer>,
    required-init-keyword: microseconds:;
end class <timestamp>;

define function get-current-timestamp ()
  let (ms, days) = current-timestamp();
  let (seconds, milliseconds) = floor/(ms, 1000);
  make(<timestamp>, days: days, seconds: seconds, microseconds: milliseconds * 1000)
end function get-current-timestamp;

define function add-duration-to-timestamp
    (timestamp :: <timestamp>, seconds :: <integer>, microseconds :: <integer>)
 => (timestamp :: <timestamp>)
  let days = timestamp-days(timestamp);
  let seconds = timestamp-seconds(timestamp) + seconds;
  let microseconds = timestamp-microseconds(timestamp) + microseconds;
  if (microseconds >= 1000000)
    seconds := seconds + 1;
    microseconds := microseconds - 1000000;
  end if;
  if (seconds >= 86400)
    days := days + 1;
    seconds := seconds - 86400;
  end if;
  make(<timestamp>, days: days, seconds: seconds, microseconds: microseconds)
end function add-duration-to-timestamp;
