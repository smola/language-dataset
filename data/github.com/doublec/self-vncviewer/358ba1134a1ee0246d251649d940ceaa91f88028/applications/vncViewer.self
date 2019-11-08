 '$Revision:$'
 '
Copyright 1992-2014 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'
["preFileIn" self] value


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         vncViewer = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vncViewer.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         vncViewer <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         attachMorph = ( |
            | 
            morph: vncMorph copyWith: serverData.
            (message copy receiver: self Selector: 'processServerMessages') fork.
            morph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         bitsPerPixel = ( |
            | serverData format bitsPerPixel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: public'
        
         connect = ( |
            | 
            socket: os_file openTCPHost: '127.0.0.1' Port: 5901.
            doHandshake.
            doSecurity.
            doClientInit.
            serverData: readServerInit.
            sendFrameBufferUpdateRequest: false X: 0 Y: 0 
                                   Width: serverData width Height: serverData height).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: public'
        
         disconnect = ( |
            | 
            socket close.
            socket: nil.
            serverData: nil.
            morph: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         doBell = ( |
            | 
            'bell' printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         doClientInit = ( |
            | 
            socket write: 1 asCharacter).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         doFrameBufferUpdate = ( |
             numRects.
             rects.
            | 
            readUInt: 8.
            numRects: readUInt: 16.
            rects: vector copySize: numRects FillingWith: nil.
            numRects do: [
              | :i. r |
              r: vncRect copy.
              r readFrom: self.
              rects at: i Put: r.
              ('rect x: ',(r x asString)
                   ,'y: ',(r y asString)
                   ,'width: ',(r width asString)
                   ,'height: ',(r height asString)
              )     printLine.
            ].
            (morph isNotNil) ifTrue: [
              morph addRects: rects.
              morph changed.
            ].
            'doFrameBufferUpdate' printLine.
            sendFrameBufferUpdateRequest: true X: 0 Y: 0 
                                   Width: serverData width Height: serverData height.
            rects).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: public'
        
         doHandshake = ( |
             version.
            | 
            version: readHandshake.
            (version >= 3.3) ifTrue: [
              writeHandshake
            ] False: [
              disconnect
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         doSecurity = ( |
             s.
            | 
            s: readSecurityType.
            s = 0 ifTrue: [ ^invalidSecurity ].
            s = 1 ifTrue: [ ^noSecurity ].
            s = 2 ifTrue: [ ^vncSecurity ].
            unknownSecurity).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil.)'
        
         morph.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         noSecurity = ( |
            | 
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         pixelFormat = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer pixelFormat.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         bigEndian.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         bitsPerPixel.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         blueMax.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         blueShift.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         depth.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         greenMax.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         greenShift.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer pixelFormat parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readFrom: client = ( |
            | 
            bitsPerPixel: client readUInt: 8.
            depth: client readUInt: 8.
            bigEndian: (client readUInt: 8) = 0 ifTrue: false False: true.
            trueColor: (client readUInt: 8) = 0 ifTrue: false False: true.
            redMax: client readUInt: 16.
            greenMax: client readUInt: 16.
            blueMax: client readUInt: 16.
            redShift: client readUInt: 8.
            greenShift: client readUInt: 8.
            blueShift: client readUInt: 8.
            client socket readCount: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         redMax.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         redShift.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'pixelFormat' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         trueColor.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         processServerMessages = ( |
            | 
            readServerMessage.
            processServerMessages).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: public'
        
         readHandshake = ( |
             line.
            | 
            line: socket readLine.
            ('RFB ' isPrefixOf: line) ifTrue: [ |xxx. yyy |
              xxx: (line copyFrom: 4 Size: 3) asInteger.
              yyy: (line copyFrom: 8 Size: 3) asInteger.
              (xxx + (yyy / 10.0))
            ] False: [ 
              0
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readSInt: bits = ( |
            | 
            socket readBigEndianIntegerOfByteCount: bits / 8
                   Signed: true
                   IfFail: [ |:e| error: 'readSInt failed' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readSecurityResult = ( |
            | 
            socket readBigEndianIntegerOfByteCount: 4
                   Signed: false
                   IfFail: [ |:e| error: 'Error reading security result' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readSecurityType = ( |
            | 
            socket readBigEndianIntegerOfByteCount: 4
                   Signed: false
                   IfFail: [|:e| error: 'Error reading security type']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readServerInit = ( |
             r.
            | 
            r: serverInit copy.
            r readFrom: self.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readServerMessage = ( |
             type.
            | 
            type: readUInt: 8.
            (type = 0) ifTrue: [ ^doFrameBufferUpdate ].
            (type = 1) ifTrue: [ ^doSetColourMapEntries ].
            (type = 2) ifTrue: [ ^doBell ].
            (type = 3) ifTrue: [ ^doServerCutText ].
            doUnknownServerMessage).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readUInt: bits = ( |
            | 
            socket readBigEndianIntegerOfByteCount: bits / 8
                   Signed: false
                   IfFail: [ |:e| error: 'readUInt failed' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         sendFrameBufferUpdateRequest: incremental X: x Y: y Width: w Height: h = ( |
            | 
            writeUInt: 8 Value: 3.
            writeUInt: 8 Value: (incremental ifTrue: 1 False: 0).
            writeUInt: 16 Value: x.
            writeUInt: 16 Value: y.
            writeUInt: 16 Value: w.
            writeUInt: 16 Value: h).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         sendKeyEvent: key Down: down = ( |
            | 
            writeUInt: 8 Value: 4.
            writeUInt: 8 Value: (down ifTrue: 1 False: 0).
            writeUInt: 16 Value: 0.
            writeUInt: 32 Value: key).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil.)'
        
         serverData.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         serverInit = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer serverInit.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         format.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         height.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         name.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         nameLen.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer serverInit parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readFrom: client = ( |
            | 
            width: client readUInt: 16.
            height: client readUInt: 16.
            format: client pixelFormat copy.
            format readFrom: client.
            nameLen: client readUInt: 32.
            name: client socket readCount: nameLen).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'serverInit' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         width.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil.)\x7fVisibility: private'
        
         socket.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         vncMorph = bootstrap define: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals morph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer vncMorph.

CopyDowns:
globals morph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil.)'
        
         cachedCanvas.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer vncMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         addRects: v = ( |
            | 
            rectLock protect: [
              vncRect: vncRect,v 
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         baseDrawOn: aCanvas = ( |
             h.
             w.
            | 
            rectLock protect: [
              cachedCanvas isNil ifFalse: [
                aCanvas pastePixmap: cachedCanvas At: baseBounds origin.
              ]
            ].

            rectLock protect: [
              updating ifTrue: [ ^self ].
              vncRect isEmpty ifTrue: [ ^self ].

              updating: true.

              cachedCanvas isNil ifTrue: [
                w: baseBounds width min: (vncRect at: 0) width.
                h: baseBounds height min: (vncRect at: 0) height.
                cachedCanvas: pixmapCanvas copyForSameScreenAs: aCanvas winCanvas Width: w Height: h.
              ].

              (message copy receiver: self Selector: 'updateCachedCanvas') fork.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         copyWith: serverData = ( |
             m.
            | 
            m: copy.
            m rectLock: lock copy.
            m vncRect: vector copy.
            m setWidth: serverData width Height: serverData height.
            m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         morphTypeName = 'vncMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'morph' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         updateCachedCanvas = ( |
             h.
             r.
             w.
            | 
            rectLock protect: [
              cachedCanvas isNil ifTrue: [ updating: false. ^self ].
              vncRect isEmpty    ifTrue: [ updating: false. ^self].
              r: vncRect at: 0.
              vncRect: vncRect copyFrom: 1.
            ].

            w: baseBounds width min: r width.
            h: baseBounds height min: r height.

            r y upTo: h By: 1 Do: [ |:y|
             rectLock protect: [
              r x upTo: w By: 1 Do: [ |:x. c. index. |
                index: ((y * r width) + x) * 4.

                c: paint copyRed: (r data at: index + 2) asByte / 255.0
                         Green: (r data at: index + 1) asByte / 255.0
                         Blue: (r data at: index) asByte / 255.0.
                cachedCanvas point: x@y Color: c
              ].
             ].
            ].
            rectLock protect: [  
              updating: false
            ].

            changed.
            updateCachedCanvas).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         prototype = bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         rectLock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (false.)'
        
         updating <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncMorph' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil.)'
        
         vncRect.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         vncRect = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer vncRect.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: InitializeToExpression: (nil)'
        
         data.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         encoding.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         height.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals vncViewer vncRect parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readFrom: client = ( |
            | 
            x: client readUInt: 16.
            y: client readUInt: 16.
            width: client readUInt: 16.
            height: client readUInt: 16.
            encoding: client readSInt: 32.
            (encoding = 0) ifTrue: [ ^readRaw: client ].
            (encoding = 1) ifTrue: [ ^readCopyRect: client ].
            (encoding = 5) ifTrue: [ ^readHextile: client ].
            (encoding = 16) ifTrue: [ ^readZRLE: client ].
            readUnknownEncoding: client).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         readRaw: client = ( |
            | 
            data: client socket readCount: width * height * ((client bitsPerPixel) / 8)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         width.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         x.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> 'vncRect' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         y.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot\x7fVisibility: public'
        
         writeHandshake = ( |
            | 
            socket writeLine: 'RFB 003.003').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'vncViewer' -> () From: ( | {
         'ModuleInfo: Module: vncViewer InitialContents: FollowSlot'
        
         writeUInt: bits Value: v = ( |
            | 
            socket writeBigEndianInteger: v
                   ByteCount: bits / 8
                   Signed: false
                   IfFail: [ |:e| error: 'writeUInt failed' ]).
        } | ) 



 '-- Side effects'

 globals modules vncViewer postFileIn
