USING: accessors arrays assocs byte-arrays calendar
calendar.format combinators.short-circuit continuations
http.parsers io io.crlf io.encodings io.encodings.utf8
io.servers kernel math namespaces parser present
sequences strings urls vectors vocabs.refresh words.symbol
xml.data xml.writer destructors fry html.streams io.ports
http.machine.data
http.machine.dispatch
http.machine.flow
http.machine.mime
http.machine.resource
http.machine.request
http.machine.response 
http.machine.states
http.machine.stream ;
FROM: http => read-header parse-cookie unparse-set-cookie write-header ;
FROM: debugger => print-error :c ;
FROM: html => simple-page ;
IN: http.machine

SYMBOL: machine-development?

TUPLE: machine-server < threaded-server dispatcher ;

<PRIVATE

: make-http-error ( error -- xml )
    [ "Internal server error" f ] dip
    [ print-error nl :c ] with-html-writer simple-page ;

: error-response ( response error -- response )
    make-http-error >>body
    "text/html" >>content-type
    utf8 >>content-encoding ; inline

: <500> ( error -- response )
    <machine-response> 500 >>code
    f server-keep-alive set-tx-metadata
    swap machine-development? get
    [ error-response ] [ drop ] if ;

: <404> ( -- response )
    <machine-response> 404 >>code
    f server-keep-alive set-tx-metadata ;

: init-tx ( request resource -- request response resource )
    [
        [ machine-request set ] keep
        <machine-response> [ machine-response set ] keep
    ] dip init-resource ; inline

: process-request ( request resource -- )
    [
        [ init-tx handle-request write-response ]
        [ <500> nip write-response ] recover
    ] with-destructors ; inline

: ?refresh-all ( -- )
    machine-development? get-global
    [ global [ refresh-all ] bind ] when ; inline

: with-tx ( ..a quot -- ..b keep-alive? )
    [ <machine-transaction> machine-transaction ] dip
    [ server-keep-alive tx-metadata ] compose
    with-variable ; inline

: close-port ( -- )
    output-stream get underlying-port dispose* ;

DEFER: machine-handle-client

: ?reuse-connection ( count machine-server keep-alive? -- )
    [ [ 1 + ] dip machine-handle-client ]
    [ 2drop ] if ; inline recursive

: machine-handle-client ( count machine-server -- )
    [
        [ dup connection-request-count set-tx-metadata ] dip
        ?refresh-all read-request over
        dispatcher>> lookup-resource
        [ process-request ] 
        [ <404> write-response ] if*
    ] with-tx ?reuse-connection ; inline recursive
    
PRIVATE>

M: machine-server handle-client*
    [ [ 0 ] dip machine-handle-client ] with-destructors ;

: <machine> ( dispatcher -- server )
    [ utf8 machine-server new-threaded-server ] dip >>dispatcher
        "factor machine" >>name
        "http" protocol-port >>insecure
        "https" protocol-port >>secure ;
