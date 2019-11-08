
\ RTX 2000 Forth code for software FIFO interrupt service routine

: xy " dos xy example.4" evaluate ;

\ Define U< to be a macro
: U<   " - cU2/   not 0< "  evaluate ; immediate

CREATE FIFO_START  1000 ALLOT
HERE 4 - CONSTANT FIFO_END

VARIABLE FIFO_HEAD   FIFO_START FIFO_HEAD !
VARIABLE FIFO_TAIL   FIFO_START FIFO_TAIL !

: Int_Service                                  \ 2 clocks for interrupt
( \ )  12 G@               ( -- input )            \ 1 clock
  FIFO_HEAD @         ( -- input ptr )         \ 4 clocks
  2 !+                ( -- ptr' )              \ 2 clocks
  FIFO_END OVER U<    ( -- ptr' wrap? )        \ 5 clocks
  IF                                           \ 1 clock
     DROP FIFO_START                           \ 3 clocks
  THEN
  DUP FIFO_TAIL @ =   ( -- ptr' no_overrun? )  \ 8 clocks
  IF                                           \ 1 clock
    ABORT" FIFO overrun"
  THEN
  FIFO_HEAD !  EXIT  ( -- )                    \ 4 clocks
  ;

\ total time:  31 clocks


variable test_val  0 test_val !
: test  
   test_val @ DUP .  int_service
   ." FIFO_HEAD = " FIFO_HEAD @ U.
   ." FIFO_START = " FIFO_START U.
   ." FIFO_END = " FIFO_END U.
   1 test_val +! ;

: tests for test cr next ;



\ Wait loop for I/O status bit
\ Waits for bit 3 of I/O port 12 to be set

: WAIT
   8           ( -- mask value )
   BEGIN
     12 G@
     OVER AND
   UNTIL 
   DROP ;
