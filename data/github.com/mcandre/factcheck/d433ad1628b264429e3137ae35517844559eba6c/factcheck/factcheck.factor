USING: kernel io random math sequences combinators.smart strings locals prettyprint ;
IN: factcheck

! A quotation generating a random integer.
: gen-integer ( -- n ) random-32 ;

! A quotation generating a random boolean.
: gen-bool ( -- ? ) gen-integer even? ;

! A quotation generating a random byte.
: gen-byte ( -- n ) gen-integer 256 mod ;

! A quotation generating a random character.
: gen-char ( -- ch ) gen-integer 128 mod ;

! A quotation generating a random sequence.
: gen-seq ( quot: ( -- obj ) -- seq ) gen-integer 100 mod swap replicate ; inline

! A quotation generating a random string.
: gen-string ( -- str ) [ gen-char ] gen-seq >string ;

! If the fact holds true for the generated values, print success.
! Otherwise, print the offending values.
:: for-all ( fact: ( ..a -- ? ) generator: ( -- ..a ) -- )
    100 iota [ drop
        generator { } output>sequence :> generated
        generated fact input<sequence :> ok?
        ok? [ "*** Failed!" print generated . ] unless
        ok? not
    ] find drop not [ "+++ OK, passed 100 tests." print ] when ; inline
