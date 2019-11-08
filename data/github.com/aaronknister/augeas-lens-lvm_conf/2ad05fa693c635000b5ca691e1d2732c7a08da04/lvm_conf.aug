module Lvm_conf =
   autoload xfm

   let empty = Util.empty
   let eol = Util.eol
   let comment = Util.comment
   let indent = del /[ \t]*/ ""
   let tab = del /[ \t]*/ "\t"
   let eq = del /=/ "="

   let id = /[a-zA-Z][a-zA-Z0-9_]+/

   let type_int = /-?[0-9]+/
   let type_float = /-?[0-9]+\.[0-9]+/
   let type_string = /"[^"]*"/

   let type_value = ( type_int | type_float | type_string )

   let nl = del /\n/ "\n"
   let opt_nl = del /\n*/ "\n"

   let type=
     [ tab . key id . indent . eq . indent . store type_value ]

   let array_value = 
     [ label "value" . store type_value ]

   let array_entry_last = 
     indent . array_value . del /[, ]?/ " "

   let array_entry_first = 
     indent . array_value . Util.del_str "," . opt_nl

   let type_array = 
     [ tab . key  id . indent . eq . indent . Util.del_str "[" . ( array_entry_first* . array_entry_last ) . Util.del_str "]" ]

   let section =  [ indent . key id . indent . del /\{/ "{" . ( empty | comment | ( type | type_array ) . eol )* . del /[ \t]*\}/ "}" ]

   let main = ( empty | comment )

   let lns = ( main | section )*

   let filter = incl "/etc/lvm/lvm.conf" 

   let xfm = transform lns filter
