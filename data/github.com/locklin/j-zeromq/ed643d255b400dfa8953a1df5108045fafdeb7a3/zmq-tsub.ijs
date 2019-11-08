load jpath,'~/src/jstuff/j-zeromq/zeromq.ijs'
ctz=: zmq_ctx_new''
NB. need an "allocator" envelope to send the size/type of the array
allocator =: zmq_socket ctz;(socktype 'sub')
NB. subscriber is the data of interest; an array of some kind
subscriber =: zmq_socket ctz;(socktype 'sub')
zmq_connect allocator;'tcp://localhost:6666'
zmq_connect subscriber;'tcp://localhost:6666'
zmq_setsockopt allocator;(sockopt 'subscribe');'type'
zmq_setsockopt subscriber;(sockopt 'subscribe');'array'


NB. note that zeromq primitives only recognize chars, so all the allocations
NB. look like: nbytes # ' '

testloop=: 3 : 0
 addr=:10#' '                                                      NB. message envelope
 smbuff=. 8#' '                                                   NB. long int allocated for msg size
 while. 1 do.
  zmq_recv allocator;addr;(sockopt 'rcvbuf')         NB. get envelope
  zmq_recv allocator;smbuff;(sockopt 'rcvbuf')     NB. get buffer size
  sk=. _3 ic smbuff                                              NB. convert back to  an int
  giantbuff=.  sk #' '                                            NB. allocate message buffer
  zmq_recv subscriber;addr;(sockopt 'rcvbuf')      NB. get envelope
  zmq_recv subscriber;giantbuff;(sockopt 'rcvbuf')  NB. get the message
  smoutput 'data received at ',": 6!:0''               NB. you got something
  smoutput (deserialize giantbuff)                         NB. deserialize the message
end.
)


testloop''

zmq_close allocator                                         NB. cleanup allocator socket
zmq_close subscriber                                      NB. cleanup message get socket
zmq_ctx_destroy ctz                                       NB. cleanup zmq environment


NB. this implements the sub end of the simple pub-sub pattern
NB. useful for ad-hoc ticker plant types of things

