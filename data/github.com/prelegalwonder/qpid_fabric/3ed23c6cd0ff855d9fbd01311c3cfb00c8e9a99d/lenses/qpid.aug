(*
Module: Qpid
  Parses simple key = value conffiles

  Author: Raphael Pinson <raphink@gmail.com>

  About: License
     This file is licenced under the LGPLv2+, like the rest of Augeas.

     About: Lens Usage
        To be documented

About: Examples
   The <Test_Simplevars> file contains various examples and tests.
   *)

module Qpid =

autoload xfm

(* View: entry *)
let entry = Build.key_value_line Rx.word Sep.equal
                                 (store Rx.space_in)

 (* View: lns *)
 let lns = (Util.empty | Util.comment | entry)*

 (* Variable: filter *)
 let filter = incl "/etc/qpidd.conf"
            . incl "/etc/qpid/qpidc.conf"

  let xfm = transform lns filter
