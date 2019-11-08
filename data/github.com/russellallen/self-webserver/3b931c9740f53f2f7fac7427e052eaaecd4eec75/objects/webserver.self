 '0.3.0'
 '
Copyright 2015-2016 AUTHORS.
See the LICENSE file for license information and AUTHORS for authors.
'
[ 
modules allCore version >= (modules init moduleVersion copyOn: '30.8.0-prerelease2') 
    ifFalse: [^ error: 'Need more modern Self']] value


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         webserver = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'copyright' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'preFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             bootstrap remove: 'tree' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules webserver.

CopyDowns:
globals modules init. copy 
SlotsToOmit: copyright directory fileInTimeString myComment postFileIn preFileIn revision subpartNames tree.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'Category: state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'
Copyright 2015-2016 AUTHORS.
See the LICENSE file for license information and AUTHORS for authors.
\')\x7fVisibility: public'
        
         copyright <- '
Copyright 2015-2016 AUTHORS.
See the LICENSE file for license information and AUTHORS for authors.
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            webserver server registerForAutomaticStartup).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         preFileIn = ( |
            | 
            modules allCore version >= (modules init moduleVersion copyOn: '30.8.0-prerelease2') 
                ifFalse: [^ error: 'Need more modern Self']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'0.3.0\')\x7fVisibility: public'
        
         revision <- '0.3.0'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'lobbyBrowserServlet
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'org_selflanguage_webserver\')'
        
         tree <- 'org_selflanguage_webserver'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         webserver = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         cookie = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver cookie.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( | {
         'Comment: Domain for which this cookie applies\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         domain <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( | {
         'Comment: Time at which this cookie expires\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (time copy)'
        
         expires <- time copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (false)'
        
         httponly <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver cookie p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         copy = ( |
            | resend.copy values: dictionary copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFromHTTPEncoding: s IfFail: fb = ( |
            | copy fromHTTPEncoding: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         copyName: n Values: v = ( |
            | (copy name: n) values: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         example1 = 'NID=67=a_KLyZ5guE9dR6aI7FmyEgLUrqQuI5nxZv9MUr8ko0RFh5UfDa-KppUFoW1mCfGEknA52RIbZJD5bvnnNyFAKyouQa1M7eny0vgYfxzro7KBnbjVDJ2O1Uh_kj60pMje; expires=Thu, 16-Apr-2015 10:50:04 GMT; path=/; domain=.google.com; HttpOnly'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         example2 = 'name=Nicholas; expires=Sat, 02 May 2009 23:38:25 GMT'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         fromHTTPEncoding: s IfFail: fb = ( |
            | 
             (s asTokensSeparatedByCharactersIn: ';') do: [|:t. kv. k|
               kv: t asTokensSeparatedByCharactersIn: '='.
               k: kv first shrinkwrapped.
               case
                 if: [k = 'HttpOnly'] Then: [httponly: true]
                 If: [k = 'secure'  ] Then: [secure: true]
                 If: [k = 'domain'  ] Then: [domain: kv at: 1]
                 If: [k = 'path'    ] Then: [path: kv at: 1]
                 If: [k = 'expires' ] Then: [expires:
                        (time copyFromString: ((kv at: 1) copyFrom: 0 
                                                              UpTo: (kv at: 1) size - 4)
                                      IfFail: [^ fb value])]
               Else: [
                   "Should be key=value, if not reject cookie"
                   kv size != 2 ifTrue: [^ fb value].
                   values at: k Put: (kv at: 1)]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         testRoundTripOn: c = ( |
            | (copyFromHTTPEncoding: c IfFail: [^ false])
            toHTTPEncoding = c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         toHTTPEncoding = ( |
             s <- ''.
            | 
            values do: [|:v. :k|
              s: s, '; ', k, '=', v].
            s size > 0 ifTrue: [
               s: s copyFrom: 2 UpTo: s size].
            expires != time ifTrue: [
              s: s, '; expires=', expires httpTimeStringGMT].
            domain != '' ifTrue: [
              s: s, '; domain=', domain].
            path != '' ifTrue: [
              s: s, '; path=', path].
            secure ifTrue: [s: s, '; secure'].
            httponly ifTrue: [s: s, '; HttpOnly'].
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( | {
         'Comment: This cookie\'s path\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         path <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( | {
         'Comment: Whether I am secure\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (false)'
        
         secure <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'cookie' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (dictionary copyRemoveAll)'
        
         values <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: servlets\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         defaultServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'defaultServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver defaultServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'defaultServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: con = ( |
            | 
            con res contents: 'Not configured, using default Servlet.'.
            con).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'defaultServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: servlets\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         exampleServlets = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         fileServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets fileServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'~/Sites/\')'
        
         baseDirectory <- '~/Sites/'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         canonicalizeUrl: aPath = ( |
             path.
            | 
            aPath size = 0 ifTrue: [ ^aPath ].
            path: aPath asTokensSeparatedByCharactersIn: '/'.

            "Remove occurences of ./ from path"
            path: path filterBy: [ |:s| s != '.' ].
            path isEmpty ifTrue: [ ^'' ].

            "Remove leading ../ from path"
            [ path isEmpty not && [ path first = '..' ] ] whileTrue: [
              path removeFirst
            ].

            "Process other ../ in path"
            path rep doLinks: [ |:lnk|
              lnk value = '..' ifTrue: [
                lnk prev remove.
                path size: path size pred.
                lnk remove.
                path size: path size pred.
              ].
            ].

            "Get string from path list"
            path isEmpty ifTrue: [ ^'' ].
            path: path reduceWith: [|:a. :b| a,'/',b ] IfSingleton: [ |:a| a ].
            (aPath at: 0) = '/' ifTrue: [ path: '/',path ].
            path).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: u = ( |
             f.
             fn.
             path.
             r.
            | 
            fn: u req url. 
            (fn last = '/') ifTrue: [fn: fn, 'index.html'].
            path: baseDirectory, fn.
            path: os_file expand: path.
            path: canonicalizeUrl: path.
            ((os_file expand: baseDirectory) isPrefixOf: path) ifFalse: [
              r: webserver response copy.
              r contents: 'Not Found:', fn.
              r statusCode: '404 Not Found'.
              ^r
            ].
            f: os_file deadCopy openForReading: path IfFail: [ 
              r: webserver response copy.
              r contents: 'Not Found:', fn.
              r statusCode: '404 Not Found'.
              ^r
            ].
            u res contents: f contents.
            u res guessContentTypeFromName: fn.
            u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         staticSiteServer = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets staticSiteServer.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( | {
         'Category: private\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         baseDir <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( | {
         'Category: private\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (dictionary copyRemoveAll)'
        
         cache <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets staticSiteServer p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         filesInDir: d = ( |
            | (os outputOfCommand: ('find ', d, ' -type f') Delay: 100 IfFail: '') asTokensSeparatedByWhiteSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: u = ( |
             fn.
             r.
            | 
            fn: u req url. 
            (fn last = '/') ifTrue: [fn: fn, 'index.html'].
            u res contents: cache at: fn IfAbsent: ['<html><body>Not Found</body></html>'].
            u res guessContentTypeFromName: fn.
            u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         loadTreeFrom: dir = ( |
            | 
            dir first == '/' ifFalse: [error: 'Directory name must be full'].
             baseDir: dir. reloadTree. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         reloadTree = ( |
            | 
            cache: dictionary copyRemoveAll.
            (filesInDir: baseDir) do: [|:fn. f. s|
              f: os_file openForReading: fn.
              s: f contents.
              f close.
              cache at: (fn copyFrom: baseDir size UpTo: fn size)
                   Put: s].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         httpconnection = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'httpconnection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver httpconnection.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'httpconnection' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'httpconnection' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         req.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'httpconnection' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         res.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: servlets\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         prototypeServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'prototypeServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver prototypeServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: servlets\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         servletTraits = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'servletTraits' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver servletTraits.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'prototypeServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'webserver' -> 'servletTraits' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         request = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver request.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'Category: private\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         headerFields <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         method <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver request p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: multipart\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         bodyPartsIfFail: blk = ( |
             b.
             d.
             t.
            | 
            b: '--', multipartBoundaryIfFail: [^ blk value].
            t: rawBody asTokensSeparatedBySubstring: b.
            t: t copyFrom: 1 UpTo: t size - 1.
            d: dictionary copyRemoveAll.
            t do: [|:e. x|
              x: e breakOnFirstSubstring: '\r\n\r\n'.
              d at: (x at: 0) shrinkwrapped Put: (x at: 1) shrinkwrapped ].
            d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: cookies\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         cookies = ( |
            | 
            (headerFieldsAt: 'Cookie') mapBy: [|:f| 
               webserver cookie 
                    copyFromHTTPEncoding: f y 
                                  IfFail: [webserver cookie copy]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         copy = ( |
            | resend.copy headerFields: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: multipart\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         fileUploadHack = ( |
             b.
             d.
             file.
             name.
             t.
            | 
            b: '--', multipartBoundaryIfFail: [^ blk value].
            t: rawBody asTokensSeparatedBySubstring: b.
            t: t copyFrom: 1 UpTo: t size - 1.
            file: (t first breakOnFirstSubstring: '\r\n\r\n') second shrinkwrapped.
            name: (t second breakOnFirstSubstring: '\r\n\r\n') second shrinkwrapped.
            ((| name. file. p* = traits clonable |) copy
               name: name)
               file: file).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: raw header fields\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         headerFieldsAt: k = ( |
            | 
            headerFields filterBy: [|:e| e x = k] Into: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: construction\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         headerFieldsAt: k Put: v = ( |
            | 
            headerFields add: (k @ v). self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: method\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         ifGET: gb IfPOST: pb Else: e = ( |
            | 
            case 
              if: [ method = 'GET'  ] Then: [ gb value: self ]
              If: [ method = 'POST' ] Then: [ pb value: self ]
              Else: [ e value: self ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: multipart\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         isMultipartFormData = ( |
            | 
            (headerFieldsAt: 'Content-Type') size > 0 ifFalse: [^ false].
            ((headerFieldsAt: 'Content-Type') first y copyFrom: 0 UpTo: 19)
              = 'multipart/form-data').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: multipart\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         multipartBoundaryIfFail: blk = ( |
             i.
            | 
            isMultipartFormData ifFalse: [^ blk value: 'Not form data'].
            i: (headerFieldsAt: 'Content-Type') first y.
            i copyFrom: 30 UpTo: i size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: url\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         path = ( |
            | 
            url findSubstring: '?'
            IfPresent: [|:i | url copyFrom: 0 UpTo: i]
             IfAbsent: [url]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: url\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         pathList = ( |
            | path asTokensSeparatedByCharactersIn: '/').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: url\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         queryString = ( |
            | 
            url findSubstring: '?'
            IfPresent: [|:i | url copyFrom: i + 1 UpTo: url size]
             IfAbsent: ['']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: construction\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         readBody: io IfFail: blk = ( |
             l.
            | 
            l: headerFieldsAt: 'Content-Length'.
            l isEmpty ifFalse: [
              rawBody: (io readCount: l first y asInteger IfFail: [^ blk value]) shrinkwrapped].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: construction\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         readHeader: io IfFail: blk = ( |
             line.
             methodLine.
             readLog.
             req.
            | 
            readLog: [|l| l: (io readLineIfFail: [^ blk value]) shrinkwrapped. 
               log info: l For: 'webserver'. l].
            readMethodLine: readLog value IfFail: [^ blk value].
            [line: readLog value. line size > 0] whileTrue: [
              readHeaderLine: line IfFail: [^ blk value]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: construction\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         readHeaderLine: line IfFail: blk = ( |
            | 
            line findSubstring: ':'
                     IfPresent: [|:i| headerFieldsAt: (line copyFrom: 0 UpTo: i) shrinkwrapped
                                                 Put: (line copyFrom: i + 1 UpTo: line size) shrinkwrapped ]
                      IfAbsent: [^ blk value]. "Malformed header"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: construction\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         readMethodLine: line IfFail: blk = ( |
             methodLine.
            | 
            methodLine: line asTokensSeparatedByWhiteSpace. 
            method: (methodLine at: 0 IfAbsent: [^ blk value]).
            url:    (methodLine at: 1 IfAbsent: [^ blk value]).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: variables\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         unescape: x = ( |
             a.
             b.
             r.
            | 
            r: x.
            b: '  !#$&\'()*+,/:;=?@[]'.
            a: ('+' & '%20' & '%21' & '%23' & '%24' & '%26' & '%27' & '%28' & '%29' & '%2A' & '%2B' & '%2C' & '%2F' & '%3A' & '%3B' & '%3D' & '%3F' & '%40' & '%5B' & '%5D') asVector.
            0 to: b size - 1 Do: [|:i|
              r: r replace: (a at: i) With: (b at: i)].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'Category: variables\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         variableAt: x IfFail: blk = ( |
             pairs.
             u.
            | 
            ifGET: [u: queryString. u = '' ifTrue: [^ blk value: 'No Variables Found']]
            IfPOST: ["Can't yet handle multipart" 
              ((headerFieldsAt: 'Content-Type') size > 0) && [
               (headerFieldsAt: 'Content-Type') first y = 'application/x-www-form-urlencoded']
               ifFalse: [^ blk value: 'Wrong content type']
               u: rawBody]
            Else: [^ blk value: 'Unknown Request Method'].

              pairs: u asTokensSeparatedByCharactersIn: '&'.
              pairs do: [|:p. splitPair |
               splitPair: p asTokensSeparatedByCharactersIn: '='.
               splitPair size = 2 ifFalse: [^ blk value: 'Badly formed key value pair']. 
               (unescape: splitPair first) = x ifTrue: [^ unescape: splitPair at: 1]].
              blk value: 'Variable not found').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'Category: private\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         rawBody <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         url <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         response = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver response.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'text/html; charset=UTF-8\')'
        
         contentType <- 'text/html; charset=UTF-8'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         contents <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         location <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver response p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         contentsLength = ( |
            | 
            contents size asString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         copy = ( |
            | resend.copy rawHeaderFields: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         guessContentTypeFromName: n = ( |
             x.
            | 
            x: (n asTokensSeparatedByCharactersIn: '.') last.
            x = 'html' ifTrue: [contentType: 'text/html; charset=UTF-8'     ].
            x = 'js'   ifTrue: [contentType: 'application/ecmascript'       ].
            x = 'css'  ifTrue: [contentType: 'text/css'                     ].
            x = 'gif'  ifTrue: [contentType: 'image/gif'                    ].
            x = 'jpg'  ifTrue: [contentType: 'image/jpeg'                   ].
            x = 'png'  ifTrue: [contentType: 'image/png'                    ].
            x = 'pdf'  ifTrue: [contentType: 'application/pdf'              ].
            x = 'gz'   ifTrue: [contentType: 'application/x-compressed'     ].
            x = 'zip'  ifTrue: [contentType: 'application/zip'              ].
            x = 'dmg'  ifTrue: [contentType: 'application/x-apple-diskimage'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         write: s = ( |
            | contents: contents, s. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         writeBodyOn: io IfFail: blk = ( |
            | 
            io write: contents IfFail: [^ blk value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         writeHeaderOn: io IfFail: blk = ( |
             broken.
            | 
            broken: [^ blk value].
            io write: 'HTTP/1.1 ', statusCode, '\n'            IfFail: broken.
            io write: 'Content-Type: ', contentType, '\n'      IfFail: broken.
            io write: 'Content-Length: ', contentsLength, '\n' IfFail: broken.
            io write: 'Location: ', location, '\n'             IfFail: broken. 
            io write: 'Connection: close', '\n'                IfFail: broken.
            rawHeaderFields do: [|:hf| io write: hf x, ': ', hf y, '\n' IfFail: broken].
            io write: '\n' IfFail: broken.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         rawHeaderFields <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'200 OK\')'
        
         statusCode <- '200 OK'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         server <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver server.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         deadProcess = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'deadProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver server deadProcess.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'deadProcess' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         abort = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'deadProcess' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         isAlive = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'deadProcess' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         isNil = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (true)'
        
         debug <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         deregisterServlet = ( |
            | 
            servlet: webserver defaultServlet.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleRequest: io = ( |
             broken.
             conn.
             r.
             req.
            | 
            broken: [
              io write: 'HTTP/1.0 501\n\n' IfFail: false.
              io closeIfFail: false. ^ self]. 
            req: webserver request copy readHeader: io IfFail: broken.
            req readBody: io                 IfFail: broken.
            conn: webserver httpconnection copy.
            conn req: req.
            conn res: webserver response copy.
            r: safeHandle: conn              IfFail: broken.
            r res writeHeaderOn: io          IfFail: broken.
            "Ignore body if only want head"
            req method != 'HEAD' ifTrue: [
                 r res writeBodyOn: io IfFail: broken].
            io closeIfFail: false.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         initialiseSocketOn: port = ( |
            | 
            serverSocket closeIfFail: [].
            serverSocket:
              os_file
                openTCPListenerOnPort: port
                IfFail: [|:e|
                     (e matchesPattern: '*UNKNOWN 125*')
                  || [e matchesPattern: '*EADDRINUSE*']
                     ifTrue: [error: e, '\n',
                                'Warning: couldn\'t start the rself server process.\n',
                                'The port (', port printString, ') is already in use, ',
                                'probably by another Self server.\n\n'.
                              ^self]
                      False: [^error: 'Couldn\'t start self server: ', e]].
            log info: 'serverSocket initialised' For: 'webserver'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         isRunning = ( |
            | 
            serverProcess isAlive).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (8080)'
        
         port <- 8080.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: automatic startup\x7fComment: Called when module loaded\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         registerForAutomaticStartup = ( |
             m.
            | 
            m: (message copy receiver: webserver server Selector: 'startFromScheduler').
            (snapshotAction schedulerInitialMessages anySatisfy: [|:e| m = e ])
              ifFalse: [snapshotAction addSchedulerInitialMessage: m].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: automatic startup\x7fModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         registerForCommandLinePortSelection = ( |
            | 
            snapshotAction
              forCommandLineArg: '-http-port'
                       DoAction: (| parent* = lobby.
                                    value: i With: arg = (
                                     webserver server port: 
                                        (snapshotAction commandLine at: i succ) asInteger. 
                                     i +2).
                                 |).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: automatic startup\x7fModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         registerForCommandLineStartupSelection = ( |
            | 
            snapshotAction
              forCommandLineArg: '-http-start'
                       DoAction: (| parent* = lobby.
                                    value: i With: arg = (
                                     'true' = (snapshotAction commandLine at: i succ)
                                          ifTrue: [ webserver server startAutomatically: true].
                                      i +2).
                                 |).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         registerServlet: s = ( |
            | servlet: s. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         runServerLoop = ( |
            | 
            [| io <- unixGlobals os_file. |
            serverSocket initialize: 'server socket'.
                io: serverSocket acceptConnection.
                (message copy receiver: self 
                              Selector: 'handleRequest:' 
                                  With: io) fork resume.
            ] loop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         safeHandle: con IfFail: blk = ( |
             k.
             p.
             w.
            | 
            k: (| p* = traits oddball.
                kill: ps After: ms = (
                 process this sleep: ms.
                 ps abortIfLive.
                 self )
               |).
            p: (message copy receiver: servlet 
                             Selector: 'handle:' 
                                 With: con) fork resume.
            w: (message copy receiver: k 
                             Selector: 'kill:After:' 
                                 With: p
                                 With: servletTimeout) fork resume.
            [p waitForDeath returnValue]
               onReturn: [|:r|
                 w abortIfLive.
                 r ifNil: [^ blk value]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (webserver server deadProcess)'
        
         serverProcess <- bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'deadProcess' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (os_file deadCopy)'
        
         serverSocket <- os_file deadCopy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (webserver defaultServlet)'
        
         servlet <- bootstrap stub -> 'globals' -> 'webserver' -> 'defaultServlet' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: internal state\x7fComment: 10 minutes\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (600000)'
        
         servletTimeout <- 600000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
             p.
            | 
            log dispatcher add: webserverLogHandler.
            serverProcess isAlive ifTrue: [stop].
            log info: 'Starting webserver on port ', port asString For: 'webserver'.
            p: (message copy receiver: self Selector: 'startServerOn:' With: port) fork.
            serverProcess: p.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: automatic startup\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         startAutomatically <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: automatic startup\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         startFromScheduler = ( |
            | 
            startAutomatically ifTrue: [isRunning ifFalse: [start]]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         startOn: aPort = ( |
            | 
            port: aPort. start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         startServerOn: port = ( |
            | 
            log info: 'About to initialise server...' For: 'webserver'.
            initialiseSocketOn: port.
            log info: 'About to start listen loop...' For: 'webserver'.
            runServerLoop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         stop = ( |
            | 
            log info: 'Stopping webserver' For: 'webserver'.
            serverSocket closeIfFail: [
              log warning: 'Could not close serverSocket' For: 'webserver'].
            serverProcess abort.
            serverProcess: deadProcess.
            log dispatcher remove: webserverLogHandler.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         webserverLogHandler = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'webserverLogHandler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver server webserverLogHandler.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'webserverLogHandler' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: entry = ( |
            | 
            entry logger = 'webserver' ifTrue: [
              entry printLine].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'webserverLogHandler' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         name = 'webserver'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'server' -> 'webserverLogHandler' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'globals' -> 'log' -> 'prototypeHandlers' -> 'baseHandler' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'servletTraits' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: con = ( |
            | 
            con res contents: 'Override "handle: aHTTPConnection" to see something here.'.
            con).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'servletTraits' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'time' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFromString: s IfFail: fb = ( |
            | 
            [|:eosBlk. :errBlk. str. t <- ''.  m. yr. mo. date. hr. min. sec. wd |
                str: (parsingStream copy init: s).
                t: (str getTokenIfEOS: eosBlk). "Get weekday"
                wd: weekdayNames findFirst: [|:e| t = e]
                                 IfPresent: [|:e. :i| i ]
                                  IfAbsent: [
                    shortWeekdayNames findFirst: [|:e| t = e]
                                 IfPresent: [|:e. :i| i ]
                                  IfAbsent: [^ errBlk value: 'unknown weekday: ', t]].
                t: (str getTokenIfEOS: eosBlk). "Get date"
                date: (t asIntegerIfFail: [^ errBlk value: 'could not convert date: ', t]).
                t: (str getTokenIfEOS: eosBlk). "Get month"
                mo: (monthNames findFirst: [|:e| t = e]
                                IfPresent: [|:e. :i| i ]
                                IfAbsent: [^ errBlk value: 'unknown month: ', t]).
                t: (str getTokenIfEOS: eosBlk). "Get year"
                yr: (t asIntegerIfFail: [^ errBlk value: 'could not convert yr: ', t]).
                t: (str getTokenIfEOS: eosBlk). "Get hour"
                hr: (t asIntegerIfFail: [^ errBlk value: 'could not convert hr: ', t]).
                t: (str getTokenIfEOS: eosBlk). "Get minutes"
                min: (t asIntegerIfFail: [^ errBlk value: 'could not convert min: ', t]).
                t: (str getTokenIfEOS: eosBlk). "Get seconds"
                sec: (t asIntegerIfFail: [^ errBlk value: 'could not convert sec: ', t]).
                str eos ifFalse: [^ errBlk value: 'unexpected stuff at end' ].
                copyYear: yr Month: mo Date: date Hours: hr Minutes: min Seconds: sec
                  IfFail: errBlk
            ] value: [|:e| ^ fb value: 'time parser: unexpected end of stream']
               With: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'time' -> () From: ( | {
         'Category: printing\x7fCategory: GMT\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         httpTimeStringGMT = ( |
            | 
            (shortWeekdayNames at: weekday), ', ', 
            (date   printStringPadWith0ToSize: 2), ' ',
            (monthNames   at: month),  ' ',
            year printString, ' ',
            (hour   printStringPadWith0ToSize: 2), ':',
            (minute printStringPadWith0ToSize: 2), ':',
            (second printStringPadWith0ToSize: 2), ' GMT').
        } | ) 



 '-- Sub parts'

 bootstrap read: 'lobbyBrowserServlet' InTree: globals modules webserver tree



 '-- Side effects'

 globals modules webserver postFileIn
