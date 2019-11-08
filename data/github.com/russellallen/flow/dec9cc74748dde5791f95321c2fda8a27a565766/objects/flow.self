 '0.0.3'
 '
Copyright 1992-2014 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'
["preFileIn" self] value


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'defaultBehavior' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         wrapFlow = ( |
            | flow inject copyOnObject: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'defaultBehavior' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         |= f = ( |
            | wrapFlow |= f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'defaultBehavior' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         |> f = ( |
            | (|= f) flush).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: libraries\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         flow = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: structural\x7fComment: Neither writes nor 
reads will succeed\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         blocking = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow blocking.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'Comment: We loop on ourselves by
default otherwise where 
would the turtle stack end?\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copy = ( |
             n.
            | n: clone. n in: n. n out: n. n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'Comment: Search for head 
can\'t go past here\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         heads = ( |
            | 
            list copy add: out).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         core = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits core.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            blk value: 'ATEND').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'Comment: Search for tail -
can\'t go past here.\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         tails = ( |
            | list copy add: in).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            blk value: 'ATEND').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         extract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'extract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow extract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: structural\x7fComment: /dev/null :)
I emit nils and 
eat writes\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         nil = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow nil.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow nil)'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         extract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits extract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (nil)'
        
         wrapped.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: generic\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         filter = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'filter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow filter.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         block.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (list copy)'
        
         buffer <- list copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         filter = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'filter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits filter.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'filter' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: generic\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         gather = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'gather' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow gather.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (nil)'
        
         block.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (list copy)'
        
         buffer <- list copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         gather = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'gather' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits gather.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'gather' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         inject = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'inject' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow inject.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         inject = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits inject.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (nil)'
        
         wrapped.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         interfaceTesting = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow interfaceTesting.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         check = ( |
             m.
             s.
            | 
            m: list copy.
            s: browse sendersOf: 'childResponsibility'.
            s do: [|:e| '*' print.
             "(e holder reflectee printString, ' > ',
             e key printString) printLine. "
             m addAll: checkChildren: e].
            m asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         checkChildren: e = ( |
             k.
             m.
             r.
             x.
            | 
            x: h copy.
            m: list copy.
            k: e key.
            r: e holder reflectee.
            (browse descendantsOf: r) do: [|:d| '-' print.
              (browse descendantsOf: d) isEmpty ifFalse: [ '-' print.
              "(' - ', d printString) printLine."
              (d lookupKey: k) first = e
                ifTrue: [m add: 
                  (((h copy d: d) p: e holder reflectee) k: k)  ]]].
            m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         h = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> 'h' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow interfaceTesting h.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> 'h' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         d.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> 'h' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         k.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> 'h' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         p.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> 'h' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'interfaceTesting' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: generic\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         map = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'map' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow map.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         block.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         map = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'map' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits map.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'map' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( | {
         'Comment: Search for head 
can\'t go past here\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         heads = ( |
            | 
            list copy add: out).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( | {
         'Comment: Search for tail -
can\'t go past here.\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         tails = ( |
            | list copy add: in).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'nil' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: generic\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         reduce = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'reduce' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow reduce.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (nil)'
        
         block.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         in <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (flow blocking )'
        
         out <- bootstrap stub -> 'globals' -> 'flow' -> 'blocking' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         reduce = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits reduce.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (nil)'
        
         reduction.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         tests = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow tests.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         block = ( |
             block.
             w.
            | 
            block: flow blocking  copy.
            w: 'Hello' |= block |= list copy writing.
            [w flush contents size = 0] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         combination = ( |
             myPipeline.
            | 

            myPipeline: 
                  'This is a bit of text for us to play with'  
               |= (flow map copyOn: [|:c| c capitalize])
               |= (flow filter copyOn: [|:c| c isVowel])
               |= (flow gather copyOn: [|:c| (c & c) asList])
               |= '' writing.

            myPipeline flush.
            [myPipeline contents = 'IIIIAAIIOOEEOOUUOOAAII'] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyString = ( |
             w.
            | 
            w: 'hello' |= '' writing.
            [w flush contents = 'hello'] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         filterString = ( |
             w.
            | 
            w: 'hello' 
              |= (flow filter copyOn: [|:c| 'l' = c])
              |= '' writing.
            [w flush contents = 'll'] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         injectObject = ( |
            | 
            [(2 |= flow extract single copy) flush contents = 2] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         mapString = ( |
             w.
            | 
            w: 'hello' 
              |= (flow map copyOn: [|:c| c capitalize])
              |= '' writing.
            [w flush contents = 'HELLO'] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         nil = ( |
             n.
             source.
             w.
            | 
            n: flow nil copy.
            source: 'Hello'.
            w: source |= n |= list copy writing.
            [w pull contents first isNil] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readFile = ( |
             f.
             s.
            | 
            f: os_file openForReading: '/etc/nanorc'.
            s: f |= '' writing.
            s flush.
            f close.
            [s contents = 'set nowrap\n'] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         sum = ( |
             source.
             sum.
             w.
            | 
            sum: flow reduce copyOn: [|:e. :s| s + e].
            source: (1 & 2 & 3 & 4) asVector.
            w: source |= sum |= flow extract single copy.
            [w flush contents = 10] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot\x7fVisibility: public'
        
         testAll = ( |
            | 
            copyString filterString mapString
            sum block nil readFile writeFile
            combination testConcat testInsertFlush
            testHeadTail).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         testConcat = ( |
            | 
            [('<' |= 'h1' |= '>' |= '' writing) flush contents = '<h1>'] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         testHeadTail = ( |
             h.
             t.
            | 
            h: flow map copyOn: [|:c| c capitalize].
            t:   h
              |= (flow filter copyOn: [|:c| c isVowel not])
              |= '' writing.
            [t head = h] assert.
            [h tail = t] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         testInsertFlush = ( |
             in.
             out.
            | 
             halt.
            out: 
                 (flow map copyOn: [|:c| c capitalize])
              |= (flow filter copyOn: [|:c| c isVowel not])
              |= '' writing.
            in: out head.
            in <| 'Hello '.
            'World!' |> in. 
            [out contents = 'HLL WRLD!'] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         writeFile = ( |
             i.
             o.
             s.
            | 
            o: os_file openForWriting: '/tmp/selftest'.
            s: 'Hello, World!' |= o writing.
            s flush.
            o close.
            o: os_file openForReading: '/tmp/selftest'.
            i: o contents.
            o close.
            ['Hello, World!' = i] assert. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> () From: ( | {
         'Category: shared\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         traits = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: composing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         <| f = ( |
            | (f wrapFlow |= self) flush).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copy = ( |
             n.
            | 
            n: resend.copy. 
                 flow blocking copy 
              |= n
              |= flow blocking copy.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: moving through pipeline\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         flush = ( |
            | 
            [|:exit| write: (in readIfFail: exit) IfFail: exit] loopExit. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: traversing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         head = ( |
            | 
            headIfAmbigous: [error: 'Ambigous Flow Heads']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: traversing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         headIfAmbigous: blk = ( |
             h.
            | h: heads. h size = 1 ifTrue: [h first] False: [^ blk value: h]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: traversing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         heads = ( |
            | list copy addAll: in heads).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: moving through pipeline\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         pull = ( |
            | pullIfFail: [^self]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: moving through pipeline\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         pullIfFail: blk = ( |
            | write: (in readIfFail: [|:e| ^ blk value: e]) IfFail: [|:e| ^ blk value: e]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: moving through pipeline\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         pullUpTo: n = ( |
            | n do: [pullIfFail: [^ self]]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            childResponsibility).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: traversing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         tail = ( |
            | 
            tailIfAmbigous: [error: 'Ambigous Flow Tails']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: traversing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         tailIfAmbigous: blk = ( |
             t.
            | t: tails. t size = 1 ifTrue: [t first] False: [^ blk value: t]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: traversing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         tails = ( |
            | 
            list copy addAll: out tails).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: composing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         wrapFlow = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            childResponsibility).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> () From: ( | {
         'Category: composing\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         |= s = ( |
             f.
            | f: s wrapFlow. out: f. f in: self. f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         adaptors = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits extract adaptors.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         collection = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'collection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits extract adaptors collection.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         collection <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         contents = ( |
            | collection).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: o = ( |
            | copy collection: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            collection add: obj. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         file = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'file' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits extract adaptors file.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         contents = ( |
            | file).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: f = ( |
            | copy file: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (os_file deadCopy)'
        
         file <- os_file deadCopy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | file write: obj IfFail: [|:e| ^ blk value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         single = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'single' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits extract adaptors single.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'single' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         contents = ( |
            | object).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'single' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copy = ( |
            | 
            resend.copy object: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'single' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (nil)'
        
         object.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'single' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'single' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | object: obj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         string = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'string' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits extract adaptors string.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'string' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         contents = ( |
            | string).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'string' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: s = ( |
            | copy string: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'string' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'string' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (\'\')'
        
         string <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> 'adaptors' -> 'string' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: char IfFail: blk = ( |
            | string: string, char).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         contents = ( |
            | wrapped contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'Comment: We need to be able to write downstream
without being blocked.\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copy = ( |
             n.
            | n: resend.copy. n |= flow nil copy. n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOnCollection: c = ( |
            | copy wrapped: adaptors collection copyOn: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOnFile: f = ( |
            | copy wrapped: adaptors file copyOn: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOnString: s = ( |
            | copy wrapped: adaptors string copyOn: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
             i.
            | 
            i: in readIfFail: [|:e| ^ blk value: e].
            wrapped write: i IfFail: [|:e| ^ blk value: e].
            i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         single = ( |
            | copy wrapped: adaptors single copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'extract' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            out write: obj IfFail: [|:e| ^ blk value: e].
            wrapped write: obj IfFail: [|:e| ^ blk value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: blk = ( |
            | 
            (copy block: blk) buffer: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         filter: o = ( |
            | 
            block value: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
             o.
            | 
            o: in readIfFail: [^ blk value]. 
            (filter: o) ifTrue: o False: [readIfFail: blk]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'filter' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | (filter: obj) ifTrue: [out write: obj IfFail: [|:e| ^ blk value: e]]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: blk = ( |
            | 
            (copy block: blk) buffer: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         gather: o = ( |
            | block value: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            buffer isEmpty 
              ifTrue: [
                buffer addAll: gather: (in readIfFail: [^ blk value: 'ATEND']).
                readIfFail: blk]
               False: [buffer removeFirst]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'gather' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            (gather: obj) do: [|:e| out write: e IfFail: [|:e| ^ blk value: e]]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         adaptors = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits inject adaptors.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         collection = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'collection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits inject adaptors collection.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         collection <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: o = ( |
            | copy collection: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (0)'
        
         index <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'collection' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            index >= collection size
             ifTrue: [^ blk value: 'ATEND']
              False: [|e| e: collection at: index.
                          index: index + 1.
                          e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         file = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'file' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits inject adaptors file.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: f = ( |
            | copy file: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (os_file deadCopy)'
        
         file <- os_file deadCopy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'file' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            file readMin: 1 Max: 1 IfFail: [|:e| ^ blk value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         object = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'object' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits inject adaptors object.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'object' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: o = ( |
            | copy object: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'object' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (nil)'
        
         object.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'object' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'object' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readFlag <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> 'adaptors' -> 'object' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            readFlag ifTrue: [^ blk value: 'ATEND']
             False: [readFlag: true. object]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOnCollection: c = ( |
            | copy wrapped: adaptors collection copyOn: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOnFile: f = ( |
            | copy wrapped: adaptors file copyOn: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOnObject: o = ( |
            | copy wrapped: adaptors object copyOn: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            in readIfFail: [
              wrapped readIfFail: [|:e| ^ blk value: e]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'inject' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            out write: obj IfFail: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: blk = ( |
            | 
            copy block: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         map: o = ( |
            | block value: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | map: in readIfFail: [^ blk value: 'ATEND']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | 
            out write: (map: obj) IfFail: [|:e| ^ blk value: e]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         pipeable = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'pipeable' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits pipeable.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         copyOn: blk = ( |
            | copy block: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         finishValue = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> 'finishValue' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals flow traits reduce finishValue.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> 'finishValue' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'core' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         readIfFail: blk = ( |
            | 
            finishValue == reduction
              ifTrue: [^ blk value: 'ATEND'].
            [|:exit| reduce: in readIfFail: exit] loopExit.
            [reduction] onReturn: [reduction: finishValue]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         reduce: o = ( |
            | 
            reduction isNil
              ifTrue: [reduction: o]
               False: [reduction: block value: o 
                                         With: reduction]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'flow' -> 'traits' -> 'reduce' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         write: obj IfFail: blk = ( |
            | reduce: obj. in atEnd ifTrue: [out write: reduction IfFail: [|:e| ^ blk value: e]]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         flow = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             bootstrap remove: 'tree' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules flow.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames tree.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         directory <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (\'Experiments in Streams\')'
        
         myComment <- 'Experiments in Streams'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (\'0.0.3\')\x7fVisibility: public'
        
         revision <- '0.0.3'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'flow' -> () From: ( | {
         'ModuleInfo: Module: flow InitialContents: InitializeToExpression: (\'github.com/russellallen/flow\')\x7fVisibility: public'
        
         tree <- 'github.com/russellallen/flow'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'abstractFile' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         wrapFlow = ( |
            | flow inject copyOnFile: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'abstractFile' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         writing = ( |
            | 
            flow extract copyOnFile: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'collection' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         wrapFlow = ( |
            | flow inject copyOnCollection: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'collection' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         writing = ( |
            | flow extract copyOnCollection: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'string' -> () From: ( | {
         'Category: flow\x7fModuleInfo: Module: flow InitialContents: FollowSlot'
        
         writing = ( |
            | 
            flow extract copyOnString: self).
        } | ) 



 '-- Side effects'

 globals modules flow postFileIn
