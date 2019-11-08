module: csv
synopsis: Routines for reading CSV files.
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define class <csv-reader> (<collection>)
  slot %csv-reader-rows :: <sequence>,
    init-value: #[];
  constant slot csv-reader-stream :: <stream>,
    required-init-keyword: stream:;
  constant slot csv-reader-dialect :: <csv-dialect>,
    required-init-keyword: dialect:;
end;

//
// ITERATION PROTOCOL
//

define inline function csv-reader-size
    (csv-reader :: <csv-reader>)
 => (reader-size :: <integer>)
 0 // TODO: return the file length
end function;

define inline function csv-reader-next-state
    (csv-reader :: <csv-reader>, state :: <integer>)
 => (state :: <integer>)
  state + 1
end function;

define inline function csv-reader-finished-state?
    (csv-reader :: <csv-reader>, state :: <integer>, limit :: <integer>)
 => (finished? :: <boolean>)
  ignore(limit);
  stream-at-end?(csv-reader.csv-reader-stream)
end function;

define inline function csv-reader-current-key
    (csv-reader :: <csv-reader>, state :: <integer>) => (key :: <integer>)
  state
end function;

define inline method element
    (csv-reader :: <csv-reader>, state :: <integer>, #key default)
 => (row :: <object>)
  let stream = csv-reader-stream(csv-reader);
  let delimiter = csv-reader.csv-reader-dialect.csv-delimiter;
  let current-rows = size(csv-reader.%csv-reader-rows);
  for (row from current-rows to state)
    let line = read-line(stream);
    let new-value = split(line, delimiter);
    csv-reader.%csv-reader-rows := add!(csv-reader.%csv-reader-rows,
                                         new-value);
  end for;
  csv-reader.%csv-reader-rows[state]
end method;

define inline function csv-reader-current-element-setter
    (new-value, csv-reader :: <csv-reader>, state :: <integer>)
 => (will-never-return :: <bottom>)
  error(make(<error>,
             format-string: "CSV Reader %= is immutable",
             format-arguments: csv-reader))
end function;

define inline function csv-reader-copy-state
    (csv-reader :: <csv-reader>, state :: <object>) => (result :: <object>)
  state
end function;

define inline method forward-iteration-protocol (csv-reader :: <csv-reader>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(0,
         csv-reader-size(csv-reader),
         csv-reader-next-state,
         csv-reader-finished-state?,
         csv-reader-current-key,
         element,
         csv-reader-current-element-setter,
         csv-reader-copy-state)
end method forward-iteration-protocol;
