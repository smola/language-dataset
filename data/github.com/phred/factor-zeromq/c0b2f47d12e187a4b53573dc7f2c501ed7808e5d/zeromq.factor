! Copyright (C) 2011 Fred Alger.
! See http://factorcode.org/license.txt for BSD license.
USING: alien.c-types alien.data kernel namespaces
fry locals io.encodings.utf8 alien.strings accessors 
zeromq.ffi zeromq.context zeromq.sockets zeromq.messages ;

IN: zeromq

SYMBOL: zmq-context

: zmq-version ( -- major minor patch )
    { int int int } [ zmq_version ] with-out-parameters ;


: (zmq-context) ( -- obj )
    zmq-context get dup [ "no zmq context" throw ] unless ;

: with-zmq-context ( quot -- )
    [
        1 zmq_init
        zmq-context set
        [ zmq-context get zmq_term drop ] [ ] cleanup
    ] with-scope ; inline


: publish ( -- )
    ;   #! set up ZMQ_PUB socket

: subscribe ( -- )
    ;   #! set up ZMQ_SUB socket

GENERIC: ->message ( -- ) ;   #! recieve message from ZMQ_SUB socket
GENERIC: message-> ( -- ) ;   #! send message to ZMQ_PUB socket
