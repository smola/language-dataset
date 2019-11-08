module: dylan-user
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define library gir-generate-c-ffi
  use common-dylan;
  use collections;
  use c-ffi;
  use gobject-introspection;
  use command-line-parser;
  use io;
  use strings;
  use system;

  export gir-generate-c-ffi;
end library;

define module gir-generate-c-ffi
  use common-dylan, exclude: { format-to-string };
  use collections;
  use c-ffi;
  use gobject-introspection;
  use command-line-parser;
  use streams;
  use file-system;
  use format;
  use format-out;
  use locators;
  use standard-io;
  use strings;

end module;
