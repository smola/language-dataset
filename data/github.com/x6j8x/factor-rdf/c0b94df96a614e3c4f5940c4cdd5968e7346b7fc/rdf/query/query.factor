USING: assocs combinators.short-circuit generalizations kernel
lexer locals memoize rdf sequences strings namespaces ;
IN: rdf.query

TUPLE: binding name ;

MEMO: <binding> ( name -- binding )
    binding boa ;

SYNTAX: >? unclip-last <binding> suffix! ;

<PRIVATE

:: clause>triple ( clause -- bpos triple )
    H{ } clone :> bpos
    clause [
        over binding?
        [ swap bpos set-at f ] [ drop ] if
    ] map-index [ bpos ] dip seq>triple ;
    

SYMBOL: bindings

: establish-bindings ( -- seq )
    V{ } clone [ bindings set ] keep ;

: first-run ( bpos triples -- )
    [ establish-bindings ] 2dip
    [
        [ H{ } clone ] dip pick
        [ swap [ over nth ] dip 4 npick set-at ] assoc-each 
        drop pick push
    ] each 2drop ;

SYMBOL: valid-match?

:: subsequent-run ( bpos triples -- )
    V{ } clone :> newb
    bindings get [
        :> binding
        triples [
            :> triple
            t valid-match? set
            binding clone :> tmpb
            bpos [
                :> pos :> key
                key tmpb at [ 
                     pos triple nth = [ f valid-match? set ] unless
                ] [ pos triple nth key tmpb set-at ] if*
            ] assoc-each
            valid-match? get [ tmpb newb push ] when
        ] each 
    ] each
    newb bindings set ;

: ?check-bindings ( bpos triples -- )
    bindings get [ subsequent-run ] [ first-run ] if ;

PRIVATE>


: query-graph ( graph seq -- graph results )
    f bindings [
        [
            clause>triple [ over ] dip triples
            ?check-bindings
        ] each
        bindings get
    ] with-variable ;