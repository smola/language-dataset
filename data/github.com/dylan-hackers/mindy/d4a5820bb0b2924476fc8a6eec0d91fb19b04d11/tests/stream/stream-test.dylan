module: stream-test
author: Ben Folk-Williams, Nick Kramer
common-dylan-spec-modification: Doug Auclair
synopsis: Test for the streams library.
copyright: see below

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
//
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
//
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
//
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
//
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation.
//
//======================================================================

define variable has-errors = #f;

define method run-several-tests (test-name :: <string>,
                                 test :: <function>)
 => ();
  format("%s ... ", test-name);
  let temp-has-errors = has-errors;
  has-errors := #f;
  test();
  if (has-errors == #f)
    format("ok.\n");
  end if;
  has-errors := temp-has-errors | has-errors;
end method run-several-tests;

define method run-test (input, expected-result, test-name :: <string>)
 => passed? :: <boolean>;
  if (input ~= expected-result)
    has-errors := #t;
    format("Failed!\n", test-name);
    format("     Got %=\n", input);
    format("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method write-test ();
  let s = make(<byte-string-stream>, contents: make(<byte-string>),
               direction: #"output");
  write(s, "foo");
  run-test(s.stream-contents, "foo", "write test");
  new-line(s);
  #if (newlines-are-CRLF)
     run-test(s.stream-contents, "\r\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\r\n", "write-line test");
  #else
     run-test(s.stream-contents, "\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\n", "write-line test");
  #endif
  write-element(s, 'P');
  write(s, "oop");
  run-test(s.stream-contents, "Poop", "write-element test");
  close(s);
end method write-test;

define method read-test ();
  let s = make(<byte-string-stream>, contents: "Hi Joe.\nFoo");
  run-test(read-line(s), "Hi Joe.", "read-line test");
  run-test(peek(s), 'F', " peek test");
  run-test(read-element(s), 'F', "read-element test");
  run-test(read(s, 2), "oo", "read test");
  close(s);
end method read-test;

define method buffered-write-test ();
  // Except for the call to make, this is exactly the same as write-test.
  // Make edits to both.
  let s = make(<file-stream>, direction: #"output", locator: ".delete.me~");
  // Included ~ in the file name so that 'make clean' will zap it.

  write(s, "foo");
  run-test(s.stream-contents, "foo", "write test");
  new-line(s);
  #if (newlines-are-CRLF)
     run-test(s.stream-contents, "\r\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\r\n", "write-line test");
  #else
     run-test(s.stream-contents, "\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\n", "write-line test");
  #endif
  write-element(s, 'P');
  write(s, "oop");
  run-test(s.stream-contents, "Poop", "write-element test");
  close(s);
end method buffered-write-test;

define method buffered-read-test ();
  let s1 = make(<file-stream>, direction: #"output", locator: ".delete.me~");
  // Included ~ in the file name so that 'make clean' will zap it.

  write(s1, "Hi Joe.");
  new-line(s1);
  write(s1, "Foo");
  // s1.stream-position := #"start";
  close(s1);
  let s = make(<file-stream>, direction: #"input", locator: ".delete.me~");
  // The rest is identical to read-test. Make edits to both.
  run-test(read-line(s), "Hi Joe.", "read-line test");
  run-test(peek(s), 'F', " peek test");
  run-test(read-element(s), 'F', "read-element test");
  run-test(read(s, 2), "oo", "read test");
  close(s);
end method buffered-read-test;

define function make-foo-file(file-name :: <string>, direction :: <symbol>)
 => f :: <file-stream>;
  let out = make(<file-stream>, direction: #"output", locator: file-name);
  write(out, "foo");
  close(out);
  make(<file-stream>, direction: direction, locator: file-name, if-exists: #"append");
end function make-foo-file;

define function clearing-helper(test-name :: <string>, clear? :: <boolean>,
                                direction :: <symbol>) => ()
  let in = make-foo-file(".contents.txt~", direction);
  let full-test-name = concatenate(test-name, if(clear?) "" else "-no" end if,
                                   "-clear test, ");
  run-test(stream-contents(in, clear-contents?: clear?), "foo",
           concatenate(full-test-name, "first read"));
  close(in);
  // run the test again to test the contents of the file
  let new-in = make(<file-stream>, direction: direction,
                    locator: ".contents.txt~", if-exists: #"append");
  let new-contents = if(clear? & direction ~== #"input") "" else "foo" end if;
  run-test(stream-contents(new-in, clear-contents?: clear?), new-contents,
           concatenate(full-test-name, "second read"));
  close(new-in);
end function clearing-helper;

define method main (argv0, #rest ignored)
  let crry = method(str, bool, sym)
               method() clearing-helper(str, bool, sym) end
             end;
  format("\nRegression test for the streams library.\n\n");
  run-several-tests("Writing", write-test);
  run-several-tests("Reading", read-test);
  run-several-tests("Buffered writing", buffered-write-test);
  run-several-tests("Buffered reading", buffered-read-test);
  run-several-tests("File stream-contents via #\"input\", no clearing",
                    crry("stream-contents-input", #f, #"input"));
  run-several-tests("File stream-contents via #\"input\", clearing",
                    crry("stream-contents-input", #t, #"input"));
  run-several-tests("File stream-contents via #\"input-output\", no clearing",
                    crry("stream-contents-input-output", #f, #"input-output"));
  run-several-tests("File stream-contents via #\"input-output\", clearing",
                    crry("stream-contents-input-output", #t, #"input-output"));
  run-several-tests("File stream-contents via #\"output\", no clearing",
                    crry("stream-contents-output", #f, #"output"));
  run-several-tests("File stream-contents via #\"output\", clearing",
                    crry("stream-contents-output", #t, #"output"));

  run-several-tests("Buffered reading", buffered-read-test);
  run-several-tests("Buffered reading", buffered-read-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All streams tests pass.\n");
  end if;
  exit(exit-code: if (has-errors) 1 else 0 end);
end method main;
