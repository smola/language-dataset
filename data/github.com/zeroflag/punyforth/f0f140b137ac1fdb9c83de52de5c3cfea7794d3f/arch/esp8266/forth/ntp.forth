NETCON load

\ Network Time Protocol implementation, originally based on the work of Craig A. Lindley
\ Usage example:
\    123 "time.nist.gov" network-time

exception: ENTP
variable: con
48   constant: SIZE
SIZE byte-array: packet

: request ( -- buffer )
    SIZE 0 do 0 i packet c! loop
    16rE3  0 packet c! \ LI, Version, Mode
    16r06  2 packet c! \ Polling interval
    16rEC  3 packet c! \ Peer clock precision
    16r31 12 packet c!
    16r4E 13 packet c!
    16r31 14 packet c!
    16r34 15 packet c!
    0 packet ;

: connect ( port host -- ) UDP netcon-connect con ! ;
: send ( -- ) con @ request SIZE netcon-send-buf ;
: receive ( -- #bytes ) con @ SIZE 0 packet netcon-read ;
: dispose ( -- ) con @ netcon-dispose ;
: ask ( port host -- #bytes ) connect { send receive } catch dispose throw ;

: parse ( -- )
    40 packet c@ 24 lshift
    41 packet c@ 16 lshift or
    42 packet c@  8 lshift or
    43 packet c@           or
    2208988800 - ;
  
: network-time ( port host -- seconds-since-1970 | throws:ENTP ) ask SIZE = if parse else ENTP throw then ;

/end

