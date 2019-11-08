USING: giplayer.search-box kernel tools.test accessors models
       giplayer.backend sequences math arrays ;
IN: giplayer.search-box.tests

: pass-thru-filter ( model -- model' )
    <filter-model> [ [ drop t ] "yep" ] dip set-filter ;

: fail-filter ( model -- model' )
    <filter-model> [ [ drop f ] "nope" ] dip set-filter ;

: two-three-filter ( model -- model' )
    <filter-model>
    [ [ 2 >= ] "lb" ] dip set-filter
    [ [ 3 <= ] "ub" ] dip set-filter ;

: run-model-once ( model -- value )
    [ activate-model ] [ value>> ] [ deactivate-model ] tri ;

{ 1 } [ { } <model> pass-thru-filter filters>> length ] unit-test

{ 0 } [
    { } <model> pass-thru-filter
    [ "yep" ] dip [ delete-filter ] keep
    filters>> length
] unit-test

{ V{ 2 3 } }
[ 5 iota <model> two-three-filter run-model-once ] unit-test

{ 0 }
[ { t } <model> fail-filter run-model-once length ] unit-test

{ 1 } [
    { t } <model> fail-filter
    [ "nope" ] dip [ delete-filter ] keep
    run-model-once length
] unit-test

{ t f } [
    listing new "test" >>name "me please" >>description
    [ "please" make-text-search call( x -- ? ) t and ]
    [ "aplease" make-text-search call( x -- ? ) t and ] bi
] unit-test

{ 1 "test" } [
    listing new "test" >>name "me please" >>description
    listing new "ignore" >>name "me please" >>description
    2array <model> <search-model>
    "test" add-text-search
    value>> [ length ] [ first name>> ] bi
] unit-test

