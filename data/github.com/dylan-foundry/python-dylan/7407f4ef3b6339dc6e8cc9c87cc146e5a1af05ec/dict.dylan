module: python-dylan
synopsis: Python types support.
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define inline function py-dict-new ()
  wrap-raw-py-object
    (%call-c-function("PyDict_New")
       () => (result :: <raw-py-object>)
       ()
     end)
end;

define inline function py-dict-check (value :: <py-object>)
  primitive-raw-as-boolean(
    %call-c-function("dylan_PyDict_Check")
      (value :: <raw-py-object>) => (check? :: <raw-c-signed-int>)
      (as-raw-py-object(value))
    end)
end;
