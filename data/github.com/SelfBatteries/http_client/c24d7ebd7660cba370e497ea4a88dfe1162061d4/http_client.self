 '$Revision:$'
 '
Copyright 1992-2016 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'
["preFileIn" self] value


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         http_client <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals http_client.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Internals\x7fComment: Common subset of headers.\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         commonHeaders <- bootstrap setObjectAnnotationOf: ( [|d|
	d: dictionary copyRemoveAll.
	d at: ('Accept') Put: ('text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain').
	d at: ('Accept-Charset') Put: ('utf-8').
	d at: ('Accept-Language') Put: ('cs,en-us;q=0.7,en;q=0.3').
	d at: ('Connection') Put: ('keep-alive').
	d at: ('Keep-Alive') Put: ('300').
	d at: ('User-Agent') Put: ('Self HTTP client https://github.com/Bystroushaak/http_client').
] value) From: ( |
             {} = 'ModuleInfo: Creator: globals http_client commonHeaders.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Internals\x7fCategory: Prototypes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         crc32 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( |
             {} = 'Comment: Inspired by https://github.com/cristianav/PyCRC/
Ported by Bystroushaak.\x7fModuleInfo: Creator: globals http_client crc32.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Stream methods\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         copy = ( |
            | 
            parent.copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (\'edb88320\' hexAsInteger asInt64.)\x7fVisibility: private'
        
         crc32_constant = 'edb88320' hexAsInteger asInt64.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil.)\x7fVisibility: private'
        
         crc32_table.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (\'ffffffff\' hexAsInteger asInt64.)\x7fVisibility: private'
        
         ffffffff = 'ffffffff' hexAsInteger asInt64.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (traits clonable.)'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Stream methods\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         reset = ( |
            | 
            working_register: self ffffffff.
            crc32_table: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Stream methods\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         result = ( |
            | 
            ^(working_register ^^ ffffffff) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (tests suite.)'
        
         tests* = bootstrap stub -> 'globals' -> 'tests' -> 'suite' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'crc32' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil.)\x7fVisibility: private'
        
         working_register.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Internals\x7fCategory: Constants\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         crlf = '\x0d
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultHeaders = ( |
            | 
            ^commonHeaders copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fComment: Firefox headers on Windows XP\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         ffHeaders = ( |
             dict.
            | 
            dict: commonHeaders copy.
            dict at: 'User-Agent' Put: 'Mozilla/5.0 (Windows; U; Windows NT 5.1; cs; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.13'.
            ^dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         getRequest: url = ( |
            | 
            ^getRequest: url Parameters: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         getRequest: url Headers: headers Parameters: params = ( |
            | 
            ^getRequest: url Headers: headers Parameters: params Into: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         getRequest: url Headers: headers Parameters: params Into: opened_stream = ( |
             response.
             socket.
             url_obj.
            | 
            url_obj: parsed_url fromString: url.
            socket: self openConnection: url_obj.

            socket write: 'GET ',
                           (paramsToURL: (url_obj path) Params: params),
                           ' ',
                           httpVersion,
                           crlf.
            socket write: 'Host: ', url_obj composeHost, crlf.
            sendHeaders: headers To: socket.
            socket write: crlf.

            response: parseHeaders: socket.
            response opened_stream: opened_stream.
            parseResponse: response.

            socket close.
            response socket: nil.

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         getRequest: url Parameters: params = ( |
            | 
            ^getRequest: url Headers: commonHeaders Parameters: params).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         headRequest: url = ( |
            | 
            ^headRequest: url Headers: commonHeaders).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         headRequest: url Headers: headers = ( |
             response.
             socket.
             url_obj.
            | 
            url_obj: parsed_url fromString: url.
            socket: self openConnection: url_obj.

            socket write: 'HEAD ', url_obj path, ' ', httpVersion, crlf.
            socket write: 'Host: ', url_obj composeHost, crlf.
            sendHeaders: headers To: socket.
            socket write: crlf.

            response: parseHeaders: socket.

            socket close.
            response socket: nil.

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Internals\x7fCategory: Constants\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         httpVersion = 'HTTP/1.1'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fComment: Headers from Internet Explorer 7.0 on Windows XP\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         ieHeaders = ( |
             dict.
            | 
            dict: commonHeaders copy.
            dict at: 'User-Agent' Put: 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)'.
            ^dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fComment: Firefox headers on Linux\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         lffHeaders = ( |
             dict.
            | 
            dict: commonHeaders copy.
            dict at: 'User-Agent' Put: 'Mozilla/5.0 (X11; U; Linux i686; cs; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.13'.
            ^dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         openConnection: url_obj = ( |
            | 
            url_obj protocol = 'https' ifTrue: [error: 'Sorry, can\'t process HTTPS!'].

            ^ os_file openTCPHost: (url_obj domain) Port: (url_obj portOrDefault)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fComment: <?PHP
echo \"\\n\\n\\n-----\\n\";
echo \"GET:\\n\";
foreach($_GET as $key => $value){
    echo \"\\t\".$key.\"=\".$value.\"\\n\";
}
echo \"POST:\\n\";
foreach($_POST as $key => $value){
    echo \"\\t\".$key.\"=\".$value.\"\\n\";
}
?>\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         paramsTestURL = 'http://necpmvtsv.wz.cz/params.php'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         paramsToURL: requested_url Params: params = ( |
             params_joiner.
            | 

            params == nil ifTrue: [^requested_url].
            params size = 0 ifTrue: [^requested_url].

            requested_url
              findSubstring: '?'
              IfPresent: [ params_joiner: '&' ]
              IfAbsent: [ params_joiner: '?' ].

            "for POST encoding"
            requested_url = '' ifTrue: [params_joiner: ''].

            ^(requested_url, params_joiner, serializeParams: params)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (traits clonable)'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parseHeader: line Do: blk = ( |
            | 
            line
                findSubstring: ':'
                IfPresent: [| :index |
                  blk value: (line copyFrom: 0 UpTo: index)
                      With: ((line copyFrom: (index + 1) UpTo: (line size)) shrinkwrapped).
                  ^true.
                ]
                IfAbsent: [^false]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parseHeaders: socket = ( |
             buffer.
             response_obj.
             should_continue.
            | 

            response_obj: self response clone.
            response_obj socket: socket.

            buffer: socket readLine splitOn: ' '.
            response_obj httpVersion: (buffer first).
            response_obj statusCode: ((buffer at: 1) asInteger).

            [
              buffer: socket readLine.
              should_continue: self parseHeader: buffer
                                    Do: [| :k. :v | response_obj headers at: k Put: v ].
            ] untilFalse: [ should_continue ].

            ^response_obj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parseResponse: response = ( |
             content_length.
             status_code.
             transfer_encoding.
            | 
            status_code: response statusCode.

            "Special codes with no data - defined in RFC 2616, section 4.4
            (http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.4)"
            ((status_code >= 100) && (status_code < 200)) ||
            (status_code = 204) ||
            (status_code = 304)
              ifTrue: [^response].

            "Chunked requests"
            transfer_encoding: response headers at: 'Transfer-Encoding' IfAbsent: ''.
            transfer_encoding uncapitalizeAll = 'chunked'
              ifTrue: [^readChunked: response].

            "Requests specified by ContentLength"
            content_length: response headers at: 'Content-Length' IfAbsent: nil.
            content_length != nil
              ifTrue: [^readContentLength: content_length asInteger Response: response.].

            ^readUntilEnd: response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parseTestURLResult: result = ( |
             cleaned_body.
            | 
            cleaned_body: result body splitOn: '-----'.
            cleaned_body removeFirst.
            cleaned_body: cleaned_body joinUsing: '-----'.

            ^ cleaned_body shrinkwrapped).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Internals\x7fCategory: Prototypes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parsed_url <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( |
             {} = 'Comment: I know that this is not the best possible
implementation and that it also ignores
parts of the RFC, but it is good enough
for simple HTTP client.\x7fModuleInfo: Creator: globals http_client parsed_url.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         = other = ( |
            | 
            (protocol = other protocol) &&
            (domain = other domain) &&
            (port = other port) &&
            (path = other path)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         composeHost = ( |
             host.
            | 
            host: self domain.

            self portOrDefault != 80
              ifTrue: [ host: host, ':', self portOrDefault asString ].

            ^host).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         copy = ( |
            | parent.copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         domain.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         fromString: url = ( |
             contains_qm.
             parsed.
             tmp.
             url_copy.
            | 
            parsed: self copy.
            url_copy: url copy.
            contains_qm: false.

            "parse protocol"
            (url includesSubstring: '://') ifTrue: [
              tmp: url_copy splitOn: '://'.
              parsed protocol: tmp first.
              url_copy: tmp last.
            ].

            "parse domain"
            url_copy first = '[' "IPv6"
              ifTrue: [
                (url_copy includesSubstring: ']') ifFalse: [error: 'Invalid IPv6 address!'].
                tmp: url_copy splitOn: ']'.
                parsed domain: (tmp first) , ']'.
              ] False: [
                tmp: url_copy splitOn: '/'.
                (tmp size = 1) ifTrue: [
                  tmp: (url_copy splitOn: '?').
                  contains_qm: url includesSubstring: '?'.
                  ]. "handle get parameters"
                parsed domain: tmp first.
              ].

            tmp removeFirst.
            parsed path: (self parsePath: tmp ContainsQuestionMark: contains_qm).

            "parse port"
            parsed: parsePort: parsed.

            ^parsed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Parsers\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parsePath: splitted ContainsQuestionMark: contains_qm = ( |
             qm.
            | 
            contains_qm ifTrue: [qm: '?'] False: [qm: ''].

            (splitted size = 0) ifTrue: [^'/'].
            (splitted size = 1) ifTrue: [^'/', qm, splitted first].
            ^'/', qm, (splitted joinUsing: '/')).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Parsers\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parsePort: parsed = ( |
             tmp.
            | 

            parsed domain first = '[' "IPv6"
              ifTrue: [
                (parsed domain includesSubstring: ']:') ifTrue: [
                  tmp: parsed domain splitOn: ']:'.
                  parsed domain: tmp first , ']'.
                  parsed port: tmp last asInteger.
                ]
              ] False: [
                (parsed domain includesSubstring: ':') ifTrue: [
                  tmp: parsed domain splitOn: ':'.
                  parsed domain: tmp first.
                  parsed port: tmp last asInteger.
                ].
              ].

            parsed path = '' ifTrue: [parsed path: '/'].

            ^parsed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         path.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         port.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         portOrDefault = ( |
            | 
            port != nil ifTrue: [^port].

            protocol = 'http' ifTrue: [^80].
            protocol = 'https' ifTrue: [^443].
            protocol = 'ftp' ifTrue: [^21].
            protocol = 'ssh' ifTrue: [^22].

            error: 'Protocol not set, can\'t decide default port!').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         protocol.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         test = ( |
             tmp.
            | 
            tmp: self copy.
            tmp protocol: 'http'.
            tmp domain: 'kitakitsune.org'.
            tmp port: 22.
            tmp path: '/'.

            assert: (fromString: 'http://kitakitsune.org:22/') Equals: tmp.

            tmp port: nil.
            assert: (fromString: 'http://kitakitsune.org/') Equals: tmp.
            assert: (fromString: 'http://kitakitsune.org') Equals: tmp.

            tmp protocol: nil.
            assert: (fromString: 'kitakitsune.org') Equals: tmp.

            tmp domain: '[kitakitsune.org]'.
            assert: (fromString: '[kitakitsune.org]') Equals: tmp.

            tmp: fromString: 'http://kitakitsune.org?asd'.
            assert: tmp domain Equals: 'kitakitsune.org'.
            assert: tmp path Equals: '/?asd'.

            tmp: fromString: 'http://kitakitsune.org/asd?key=val'.
            assert: tmp domain Equals: 'kitakitsune.org'.
            assert: tmp path Equals: '/asd?key=val'.

            tmp: fromString: 'https://www.httpwatch.com/httpgallery/chunked/chunkedimage.aspx'.
            assert: tmp protocol Equals: 'https'.
            assert: tmp domain Equals: 'www.httpwatch.com'.
            assert: tmp portOrDefault Equals: 443.
            assert: tmp path Equals: '/httpgallery/chunked/chunkedimage.aspx'.

            testComposeHost.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testComposeHost = ( |
             default_port_url.
             special_port_url.
            | 
            default_port_url: self fromString: 'http://kitakitsune.org'.
            special_port_url: self fromString: 'http://kitakitsune.org:8080'.

            assert: (default_port_url composeHost) Equals: 'kitakitsune.org'.
            assert: (special_port_url composeHost) Equals: 'kitakitsune.org:8080'.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (tests suite)'
        
         tests* = bootstrap stub -> 'globals' -> 'tests' -> 'suite' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         postRequest: url GETParameters: get_params POSTParameters: post_params = ( |
            | 
            ^postRequest: url
              Headers: commonHeaders
              GETParameters: get_params
              POSTParameters: post_params
              Into: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         postRequest: url Headers: headers GETParameters: get_params POSTParameters: post_params = ( |
            | 
            ^postRequest: url
              Headers: headers
              GETParameters: get_params
              POSTParameters: post_params
              Into: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         postRequest: url Headers: headers GETParameters: get_params POSTParameters: post_params Into: opened_stream = ( |
             modified_headers.
             response.
             socket.
             url_encoded_params.
             url_obj.
            | 
            url_obj: parsed_url fromString: url.
            socket: self openConnection: url_obj.

            socket write: 'POST ',
                           (paramsToURL: (url_obj path) Params: get_params),
                           ' ',
                           httpVersion,
                           crlf.
            socket write: 'Host: ', url_obj composeHost, crlf.

            url_encoded_params: paramsToURL: '' Params: post_params.

            modified_headers: headers copy.
            modified_headers at: 'Content-Type'
                             Put: 'application/x-www-form-urlencoded'.
            modified_headers at: 'Content-Length'
                             Put: url_encoded_params size asString.

            sendHeaders: modified_headers To: socket.
            socket write: crlf.
            socket write: url_encoded_params.
            socket write: crlf.

            response: parseHeaders: socket.
            response opened_stream: opened_stream.
            parseResponse: response.

            socket close.
            response socket: nil.

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         postRequest: url POSTParameters: post_params = ( |
            | 
            ^postRequest: url
              Headers: commonHeaders
              GETParameters: nil
              POSTParameters: post_params
              Into: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fCategory: Body reading modes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         readChunked: response = ( |
             chunk_length.
            | 
            [ | size_line. |
              "It may take a while to get line with chunk size.."
              [ | circuit_breaker <- 0. |
                size_line: response socket readLine shrinkwrapped.
                circuit_breaker: circuit_breaker + 1.
                circuit_breaker > 10
                  ifTrue: [error: 'Can\'t find chunk definition!'].
              ] untilTrue: [size_line size > 0].

              chunk_length: size_line shrinkwrapped hexAsInteger.
              chunk_length > 0
                ifTrue: [response write: (response socket readCount: chunk_length).].
            ] untilFalse: [ chunk_length != 0 ].

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fCategory: Body reading modes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         readContentLength: length Response: response = ( |
            | 
            response write:
              response socket
                readCount: length
                IfFail: [^response].

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fCategory: Body reading modes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         readUntilEnd: response = ( |
             buffer.
            | 
            [
              buffer: response socket readCount: 1024 IfFail: [^response].
              response write: buffer.
            ] untilFalse: [response socket isLive && (buffer size != 0)].

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Internals\x7fCategory: Prototypes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         response = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals http_client response.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         body.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         clone = ( |
             res.
            | 
            res: parent.clone.
            res headers: dictionary copy.

            ^res).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (dictionary copy.)'
        
         headers <- dictionary copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         httpVersion.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil.)'
        
         opened_stream.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil.)'
        
         socket.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         statusCode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'Category: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         write: data = ( |
            | 
            opened_stream == nil
              ifTrue: [
                body == nil ifTrue: [body: ''].
                body: body, data.
              ] False: [
                opened_stream write: data.
              ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         sendHeaders: headers To: socket = ( |
            | 
            headers == nil ifTrue: [^nil].

            headers do: [ |:v. :k. |
              socket write: (k, ': ', v, crlf).
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         serializeParams: params = ( |
             out.
            | 
            params size = 0 ifTrue: [^''].

            out: params copyMappedBy: [|:val. :key.|
              (key urlEncode), '=', (val urlEncode)
            ].

            out asSequence joinUsing: '&').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         test = ( |
            | test: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         test: run_integration = ( |
            | 
            testUrlDecodeEncode.
            testSerializeGetParams.
            testGetParamsToURL.

            self parsed_url test.

            "integration tests"
            run_integration ifTrue: [
              testChunked.
              testGETRequest.
              testPOSTRequest.
            ].

            ^ true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fCategory: Integration\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testChunked = ( |
             crc_of_result.
             resp.
            | 
            resp: getRequest: 'http://www.httpwatch.com/httpgallery/chunked/chunkedimage.aspx'
                  Headers: nil
                  Parameters: nil
                  Into: nil.

            crc_of_result: crc32 computeCRC: (resp body asString).

            assert: crc_of_result Equals: ('ca408eb6' hexAsInteger).

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fCategory: Integration\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testGETRequest = ( |
             tmp.
            | 
            tmp: parseTestURLResult: (getRequest: paramsTestURL
                                      Headers: nil
                                      Parameters: test_one_element_dict
                                      Into: nil).
            assert: tmp Equals: 'GET:\n\tkey=val\nPOST:'.

            tmp: parseTestURLResult: (getRequest: paramsTestURL
                                      Headers: nil
                                      Parameters: test_multi_element_dict
                                      Into: nil).
            assert: tmp Equals: 'GET:\n\tkey=val\n\tsomething=else\n\tlast=this is last value\nPOST:'.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testGetParamsToURL = ( |
             url.
            | 
            url: 'http://kitakitsune.org/'.

            "test nil"
            testGetParamsToURL: 'http://kitakitsune.org'
              Params: nil
              Equals: 'http://kitakitsune.org'.

            testGetParamsToURL: 'http://kitakitsune.org'
              Params: test_blank_dict
              Equals: 'http://kitakitsune.org'.

            testGetParamsToURL: 'http://kitakitsune.org'
              Params: test_one_element_dict
              Equals: 'http://kitakitsune.org?key=val'.

            testGetParamsToURL: 'http://kitakitsune.org'
              Params: test_multi_element_dict
              Equals: 'http://kitakitsune.org?key=val&something=else&last=this%20is%20last%20value'.


            "test multi element dicts with URL which already have parameters"
            testGetParamsToURL: 'http://kitakitsune.org?something=already'
              Params: test_blank_dict
              Equals: 'http://kitakitsune.org?something=already'.

            testGetParamsToURL: 'http://kitakitsune.org?something=already'
              Params: test_one_element_dict
              Equals: 'http://kitakitsune.org?something=already&key=val'.

            testGetParamsToURL: 'http://kitakitsune.org?something=already'
              Params: test_multi_element_dict
              Equals: 'http://kitakitsune.org?something=already&key=val&something=else&last=this%20is%20last%20value'.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testGetParamsToURL: url Params: params Equals: val = ( |
            | 
            ^assert: (paramsToURL: url Params: params)
             Equals: val).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fCategory: Integration\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testPOSTRequest = ( |
             tmp.
            | 
            tmp: parseTestURLResult: (postRequest: paramsTestURL
                                      Headers: commonHeaders
                                      GETParameters: nil
                                      POSTParameters: test_one_element_dict
                                      Into: nil).
            assert: tmp Equals: 'GET:\nPOST:\n\tkey=val'.

            tmp: parseTestURLResult: (postRequest: paramsTestURL
                                      Headers: commonHeaders
                                      GETParameters: test_one_element_dict
                                      POSTParameters: test_multi_element_dict
                                      Into: nil).
            assert: tmp Equals: 'GET:\n\tkey=val\nPOST:\n\tkey=val\n\tsomething=else\n\tlast=this is last value'.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testSerializeGetParams = ( |
            | 
            assert: (serializeParams: test_blank_dict) Equals: ''.
            assert: (serializeParams: test_one_element_dict) Equals: 'key=val'.
            assert: (serializeParams: test_multi_element_dict)
              Equals: 'key=val&something=else&last=this%20is%20last%20value'.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         testUrlDecodeEncode = ( |
             decoded = 'test:$#@=?%^Q^$'.
             encoded = 'test%3a%24%23%40%3d%3f%25%5eQ%5e%24'.
            | 

            assert: decoded urlEncode Equals: encoded.
            assert: encoded urlDecode Equals: decoded).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         test_blank_dict = ( |
            | 
            ^dictionary copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         test_multi_element_dict = ( |
             multi_element_dict.
            | 

            multi_element_dict: orderedDictionary copy.
            multi_element_dict at: 'key' Put: 'val'.
            multi_element_dict at: 'something' Put: 'else'.
            multi_element_dict at: 'last' Put: 'this is last value'.

            ^multi_element_dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         test_one_element_dict = ( |
             one_element_dict.
            | 
            one_element_dict: dictionary copy.
            one_element_dict at: 'key' Put: 'val'.
            ^one_element_dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         tests* = bootstrap stub -> 'globals' -> 'tests' -> 'suite' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         traceRequest: url Headers: headers = ( |
             response.
             socket.
             url_obj.
            | 
            url_obj: parsed_url fromString: url.
            socket: self openConnection: url_obj.

            socket write: 'TRACE ', url_obj path, ' ', httpVersion, crlf.
            socket write: 'Host: ', url_obj composeHost, crlf.
            sendHeaders: headers To: socket.
            socket write: crlf.

            response: parseHeaders: socket.

            socket close.
            response socket: nil.

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         http_client = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'copyright' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules http_client.

CopyDowns:
globals modules init. copy 
SlotsToOmit: copyright directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/http_client'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         myComment <- 'Homepage: https://github.com/Bystroushaak/http_client'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'clonable' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | clone).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'identity' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | == x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'identity' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | identityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'unordered' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         descendantResponsibilities = bootstrap setObjectAnnotationOf: bootstrap stub -> 'mixins' -> 'unordered' -> 'descendantResponsibilities' -> () From: ( |
             {} = 'Comment: The following methods must be implemented by a descendant.\x7fModuleInfo: Creator: mixins unordered descendantResponsibilities.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'clonable' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'lobby' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'string' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         urlDecode = ( |
             out.
             tokens.
            | 
            tokens: self splitOn: '%'.

            tokens size == 0 ifTrue: [^''].
            tokens size == 1 ifTrue: [^tokens first].

            out: tokens removeFirst.
            tokens do: [| :token |
              token size < 2
                ifTrue: [out: (out, token)]
                False: [| hex_pair. |
                  hex_pair: (token at: 0), (token at: 1).
                  out: out,
                       (hex_pair hexAsInteger asCharacter),
                       (token copyFrom: 2 UpTo: (token size)).
                ].
            ].

            ^out).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'string' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         urlEncode = ( |
             alpha.
             c.
             digit.
             out.
             reserved.
             unreserved.
             whitelist.
            | 
            out: mutableString clone.

            "Datasets specified by RFC 3986"
            reserved: '!()*' asSet. "It should be ':/?#[]@!$&\'()*+,;=' ?"
            alpha: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' asSet.
            digit: '0123456789' asSet.
            unreserved: ('-._~' asSet), alpha, digit.
            whitelist: reserved, unreserved.

            bytesDo: [| :i. high. low. |
              c: i asCharacter.
              (whitelist includes: c)
                ifTrue: [out: out, c]
                False: [out: out, '%', (i hexPrintString)].
            ].

            ^out).
        } | ) 



 '-- Side effects'

 globals modules http_client postFileIn
