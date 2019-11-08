(*
Module: Bacula
  Parses: /etc/bacula/*.conf

Author: Domen Kožar <domen@dev.si>

About: Reference
  This lens tries to be ...

About: License
  This file is licenced under the LGPL v2+, like the rest of Augeas.

About: Lens Usage
   See <lns>.

About: Configuration files
   This lens applies to /etc/bacula/*.conf.

About: Examples
   The <test_bacula.aug> file contains various examples and tests.
*)


module Bacula =
   autoload xfm

   let indent = Util.del_opt_ws "\t"
   let equal = del /[ \t]*=[ \t]*/ " = "
   let key_name = /[a-zA-Z][a-zA-Z ]+[a-zA-Z]/

   let val_quote = [square /"/ (store /([^"#]|\\")+/)]
   let val = [label "" . store /[^}"#\n\t; ]([^}"#\n;]*[^}"#\n\t; ])?/]

   let keyvalue = key key_name . equal . (val|val_quote)
   let include = label "@include" . Util.del_str "@" . store /[^# \t\n@};]+/

   let semicolon = del /([ \t]*;)?/ ""
   let eol = del /[ \t]*(;|(#[ \t]*)?\n)/ "\n"
   let comment_or_eol = Util.comment_eol | eol
   let comment_or_semicolon = Util.comment_eol | semicolon

   let line (sto:lens) = [ sto . comment_or_eol ]
   let line_noeol (sto:lens) = [ sto . comment_or_semicolon ]

   let rec block =
        let entry = Util.empty | (indent . (line keyvalue | line include | block))
     in let entry_noindent = line keyvalue | line include | block
     in let entry_noindent_noeol = line_noeol keyvalue | line_noeol include | block
     in let entry_noeol = indent . entry_noindent_noeol
     in [ label "@block" . store /[a-zA-Z]+/
        . Build.block_generic
            entry                      (* entry *)
            entry_noindent             (* entry_noindent *)
            entry_noeol                (* entry_noeol *)
            entry_noindent_noeol       (* entry_noindent_noeol *) 
            Util.comment               (* comment *)
            Util.comment_noindent      (* comment_noindent *)
            /[ \t\n]*\{[ \t\n]*/       (* ldelim_re *)
            Build.block_rdelim_re      (* rdelim_re *) 
            " {\n\t"                   (* ldelim_default *) 
            Build.block_rdelim_default (* rdelim_default *) 
        ]

   let statement = Util.indent . (line keyvalue | line include | block)

   let lns = (statement|Util.empty|Util.comment)*

   let filter = incl "/etc/bacula/*.conf"
              . Util.stdexcl

   let xfm = transform lns filter
  
   (* TODO: put tests *)

   test (Bacula.line keyvalue) get "Name = kaki-sd\n" =
      {"Name" 
        {"" = "kaki-sd"}
      }

   test (Bacula.line include) get "@/etc/foo.conf\n" =
      {"@include" = "/etc/foo.conf"}

   test (Bacula.line keyvalue) get "Name = kaki-sd;" =
      {"Name" 
        {"" = "kaki-sd"}
      }

   test (Bacula.line include) get "@foobar  ;" =
      {"@include" = "foobar"}

   test Bacula.lns get "Storage {\n   Name = kaki-sd\n}" =
      {"@block" = "Storage"
        {"Name" 
          {"" = "kaki-sd"}
        }
      }

   (* value can have quotes *)
   test Bacula.lns get "Storage {\n   Name = \"kaki-sd\"\n}" =
      {"@block" = "Storage"
        {"Name" 
          {"\"" = "kaki-sd"}
        }
      }

   (* whitespace in key *)
   test Bacula.lns get "Storage {\n   Pid Directory = kaki sd\n}" =
      {"@block" = "Storage"
         {"Pid Directory"
          {"" = "kaki sd"}
         }
      }

   (* one char value *)
   test Bacula.lns get "Storage {\n   Maximum Concurrent Jobs = 1\n}" =
      {"@block" = "Storage"
         {"Maximum Concurrent Jobs"
          {"" = "1"}
         }
      }

   (* semicolon *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd;\n}" =
      {"@block" = "Storage"
        {"Name" 
          {"" = "kaki-sd"}
        }
      }

   (* inline comment *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd         # just a comment\n}" =
      {"@block" = "Storage"
        {"Name" 
          { "" = "kaki-sd" }
          { "#comment" = "just a comment"}
        }
      }

   (* comment as part of directive *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd\n # just a comment\n}" =
      {"@block" = "Storage"
        {"Name" 
          { "" = "kaki-sd" }
        }
        { "#comment" = "just a comment"}
      }

   (* comment after } *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd\n}\n # just a comment\n" =
      {"@block" = "Storage"
        {"Name" 
          { "" = "kaki-sd" }
        }
      }
      { }
      { "#comment" = "just a comment"}

   (* multiple values *)
   test Bacula.lns get "Storage {\n  Name = kaki-sd\nFoo = moo\n}" =
      {"@block" = "Storage"
        {"Name" 
          { "" = "kaki-sd" }
        }
        {"Foo" 
          { "" = "moo" }
        }
      }

   (* toplevel key/value for include files *)
   test Bacula.lns get "Name = kaki-sd\nFoo = moo\n" =
      {"Name" 
        { "" = "kaki-sd" }
      }
      {"Foo" 
        { "" = "moo" }
      }

   (* escaping quotes in value *)
   test Bacula.lns get "Storage {\nName = \"foo \\" bar\"\n}" =
      {"@block" = "Storage"
        {"Name" 
          { "\"" = "foo \\" bar" }
        }
      }

   (* newline comment *)
   test Bacula.lns get "Storage {\n  Name = kaki-sd\n# just a comment\n}" =
      {"@block" = "Storage"
        {"Name" 
          { "" = "kaki-sd" }
        }
        {"#comment" = "just a comment" }
      }

   (* include statements *)
   test Bacula.lns get "Storage {\n  @/etc/foo.conf\n}" =
      {"@block" = "Storage"
         {"@include" = "/etc/foo.conf"}
      }

   test Bacula.lns get "Storage {\n   Name = kaki-sd}" =
      {"@block" = "Storage"
        {"Name" 
          { "" = "kaki-sd" }
        }
      }

   test Bacula.lns get "FileSet { Include { signature = SHA1 } }" =
   { "@block" = "FileSet"
       { "@block" = "Include"
         { "signature" 
           { "" = "SHA1" }
         }
       }
   }
   
   test Bacula.lns get "FileSet {
  Name = \"DefaultSet\"
  Include {
    Options {
      signature = SHA1
      noatime = yes
    }
    File = /etc
  }
}" =
      {"@block" = "FileSet"
         { "Name" 
           { "\"" = "DefaultSet" }
         }
         {"@block" = "Include"
            {"@block" = "Options"
              { "signature" 
                { "" = "SHA1" }
              }
              { "noatime" 
                { "" = "yes" }
              }
            }
            { }
            { "File" 
              { "" = "/etc" }
            }
         }
      }

   (* include top level statements *)
   test Bacula.lns get "@/etc/foo.conf\n" =
      {"@include" = "/etc/foo.conf"}


   (* Blocks can follow each other without \n *)
   test Bacula.lns get "Storage{Name = kaki sd}Storage{Name = kaki-sd}" =
   { "@block" = "Storage"
        {"Name" 
          { "" = "kaki sd" }
        }
   }
   { "@block" = "Storage"
        {"Name" 
          { "" = "kaki-sd" }
        }
   }

   (* recursive directives *)
   test Bacula.lns get "FileSet { Include { signature = SHA1 } }" =
   { "@block" = "FileSet"
       { "@block" = "Include"
         { "signature" 
           { "" = "SHA1" }
         }
       }
   }
