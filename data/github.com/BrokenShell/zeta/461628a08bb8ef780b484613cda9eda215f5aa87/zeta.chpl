/*
Title:      Zeta Function Approximation in Chapel
Author:     Broken
Date:       April 21, 2017
File Name:  zeta.chpl
Language:   Chapel 1.15.0
Compile:    chpl zeta.chpl -o zeta
Call:       ./zeta --S=3
Output:     Zeta of 3.0 = 1.2020569031503209878053439751966...
Param --S:  Expects a whole number greater than one. Optional, default is 2
Runtime:    Approximately 10 seconds or less on an average PC
*/

// Command Line Argument --S
config const S: real = 2.0;

proc zeta(s) {
  var (d, i, C) = (0.0, 1.0, 299792458);
  while (i < C) do (d, i) = (d + 1 / i ** s, i + 1);
  return d;
}

proc main() {
  if S > 1 {
    writeln("zeta(%i) = %0.52r".format(S, zeta(S)));
  } else {
    // should raise value error or 
    writeln("By definition zeta(1) is undefined.");
    writeln("Param --S must be a whole number greater than one.");
  }
}
