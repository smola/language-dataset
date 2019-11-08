 ''
 '
Copyright 1992-2016 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'
[ 
"prefileIn" self] value


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         crc32 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( |
             {} = 'Comment: Inspired by https://github.com/cristianav/PyCRC/
Ported by Bystroushaak.\x7fModuleInfo: Creator: globals crc32.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Stream methods\x7fModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         append: data = ( |
             ooff.
            | 
            ooff: '00ff' hexAsInteger asInt32.

            self crc32_table == nil
              ifTrue: [ self crc32_table: computeCRC32Table ].

            data do: [| :c. tmp. |
              tmp: working_register ^^ (c asByte).
              working_register: (self crc32_table at: (tmp && ooff)) ^^ (working_register >> 8).
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: crc32 InitialContents: FollowSlot\x7fVisibility: private'
        
         computeCRC32Table = ( |
             out.
            | 
            out: list copy.

            256 do: [| :i. crc. |
              crc: i.

              8 do: [
                (crc && 1) != 0
                  ifTrue: [crc: crc32_constant ^^ (crc >> 1)]
                  False: [crc: crc >> 1].
              ].

              out add: crc asInt64.
            ].

            ^out).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         computeCRC: data = ( |
             crc_table.
             crc_value.
             ooff.
            | 
            crc_table: computeCRC32Table.
            crc_value: ffffffff.
            ooff: '00ff' hexAsInteger asInt32.

            data do: [| :c. tmp. |
              tmp: crc_value ^^ (c asByte).
              crc_value: (crc_table at: (tmp && ooff)) ^^ (crc_value >> 8).
            ].

            ^(crc_value ^^ ffffffff) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         copy = ( |
            | 
            parent.copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (\'edb88320\' hexAsInteger asInt64.)\x7fVisibility: private'
        
         crc32_constant = 'edb88320' hexAsInteger asInt64.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (nil.)\x7fVisibility: private'
        
         crc32_table.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (\'ffffffff\' hexAsInteger asInt64.)\x7fVisibility: private'
        
         ffffffff = 'ffffffff' hexAsInteger asInt64.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (traits clonable.)'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Stream methods\x7fModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         reset = ( |
            | 
            working_register: self ffffffff.
            crc32_table: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Stream methods\x7fModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         result = ( |
            | 
            ^(working_register ^^ ffffffff) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         test = ( |
             crc32.
            | 

            testTable.
            assert: (computeCRC: 'xerexe') Equals: ('5275d1c7' hexAsInteger).

            crc32: self copy.
            crc32 reset.
            crc32 append: 'xe'.
            crc32 append: 're'.
            crc32 append: 'xe'.
            assert: (crc32 result) Equals: ('5275d1c7' hexAsInteger).

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         testTable = ( |
             table.
            | 
            table: self computeCRC32Table.

            assert: table first Equals: 0.
            assert: (table at: 1) Equals: ('77073096' hexAsInteger).
            assert: (table at: 2) Equals: ('ee0e612c' hexAsInteger).
            assert: (table at: 135) Equals: ('73dc1683' hexAsInteger). 
            assert: (table size) Equals: 256.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (tests suite)'
        
         tests* = bootstrap stub -> 'globals' -> 'tests' -> 'suite' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (\'ffffffff\' hexAsInteger asInt64.)\x7fVisibility: private'
        
         working_register <- 'ffffffff' hexAsInteger asInt64.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         crc32 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules crc32.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: InitializeToExpression: (\'30.21.0\')\x7fVisibility: public'
        
         revision <- '30.21.0'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: crc32 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules crc32 postFileIn
