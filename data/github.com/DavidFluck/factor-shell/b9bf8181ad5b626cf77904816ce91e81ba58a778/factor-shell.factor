
USING: fry io kernel sequences sets splitting unix.process system io.launcher formatting io.pathnames io.files.private combinators math.parser continuations sequences.extras accessors peg.ebnf ;
QUALIFIED: unix.ffi
IN: factor-shell

: make-prompt ( -- str )
    { } cwd suffix " $" suffix "" join ;

: print-prompt ( -- )
    make-prompt write flush ;

: tokenize-line ( str -- seq )
    " " split ;

: exit-maybe ( x -- x )
    dup "exit" = [ 0 exit ] [ ] if ;

: segment-seq-for-piping ( x -- x )
   { { } } [ dup "|" = 
      [ drop { } suffix ]
      ! hide the string, dup the outer sequence, get the sequence we are appending to 
      [ [ dup last ] dip suffix [ 1 head* ] dip suffix ]
      if ] reduce ;

: cd ( path -- ) dup unix.ffi:chdir 0 = not [ "cd: no such file or directory: " swap "\n" 3append ] when write flush ;

: factor-shell ( -- )
    [ t ] [ print-prompt readln exit-maybe tokenize-line
    {
        { [ dup { "|" } contains? ] [ segment-seq-for-piping input-stream [ <process> swap " " join >>command  swap >>stdin run-process stdout>> ] reduce drop ]  }
        { [ dup first "cd" = ] [ 1 tail " " join cd ] }
        { [ dup last "&" = ] [ 1 head* run-detached drop ] }
        [ run-process wait-for-process drop ]
    } cond ] while ;

MAIN: factor-shell