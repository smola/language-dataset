USING: accessors arrays assocs calendar combinators formatting
formatting.private fry generalizations hashtables io kernel locals
macros math math.parser namespaces prettyprint random sequences strings
sequences.interleaved threads ;
QUALIFIED: pairs
IN: 21-game

CONSTANT: computer-intro "\t<Computer>: "
CONSTANT: you-intro "\t<You>: "
CONSTANT: player1-prompt-text "Add 1 or 2? "
CONSTANT: i-lose "I lose? It's impossible!"
CONSTANT: you-lose "You lose! Numbers are my domain!"
CONSTANT: max-turns-to-21 12
CONSTANT: addables { 1 2 }
CONSTANT: quitter "I quit"

SINGLETONS: computer you play skip announce algo-low algo-high algo-random algo-cheat full-rest half-rest ;
SYMBOL: rest-time

MIXIN: next
INSTANCE: play next
INSTANCE: announce next
INSTANCE: skip next

MIXIN: player
INSTANCE: you player
INSTANCE: computer player

TUPLE: 21-base-game
    { current    integer    initial: 0 }
    { next-time  next       initial: play }
    { turn-hists hashtable  initial: H{ } } ;

PREDICATE: playing-game < 21-base-game
    next-time>> play? ;

PREDICATE: announcing-game < 21-base-game
    next-time>> announce? ;

PREDICATE: skipping-game < 21-base-game
    next-time>> skip? ;

PREDICATE: game-over < 21-base-game
    current>> 21 >= ;

PREDICATE: valid-human-input < integer
    addables member? ;

PREDICATE: empty-sequence < sequence
    empty? ;

PREDICATE: quit-string < string
    "q" = ;

<PRIVATE
: writefl ( str -- ) write flush ;
MACRO: printfl ( format-string -- quot )
    printf-quot
    [ [ output-stream get [ stream-write ] curry ] compose ] dip
    [ napply flush ] curry compose ;

HOOK: sleep-sec rest-time ( sec -- )
M: full-rest sleep-sec seconds sleep ;
M: half-rest sleep-sec 2 / seconds sleep ;
half-rest rest-time set-global

GENERIC: interpret-read ( string -- string/f )
M: quit-string interpret-read drop f ;
M: f interpret-read ;
M: string interpret-read ;

: read-you ( -- something )
    readln interpret-read ;

GENERIC: (slow-dots) ( string -- string )
M: empty-sequence (slow-dots) ;
M: string (slow-dots)
    [ first 1/5 sleep-sec "%c" printfl ]
    [ rest (slow-dots) ] bi ;

: slow-dots ( -- ) "..." (slow-dots) drop 1/2 sleep-sec ;

: help. ( -- ) "q is quit" print ;
: game-stats. ( game -- ) "stats\t" writefl ... ;
PRIVATE>

GENERIC: human-prompt ( player -- )
M: you human-prompt
    drop player1-prompt-text writefl ;

GENERIC: says ( who -- )
M: computer says
    drop computer-intro writefl ;
M: you says
    drop you-intro writefl ;

GENERIC: <21-base-game> ( desc -- new-game )
M: pair <21-base-game>
    first2 [ { } 2array ] bi@ 2array >hashtable
    [ 21-base-game new ] dip >>turn-hists ;

M: computer <21-base-game>
    you 2array <21-base-game> ;

GENERIC: <game-loop> ( desc -- ticker )
M: pairs:pair <game-loop>
    [ key>> [ max-turns-to-21 ] dip <repetition> ]
    [ value>> ] bi <interleaved> ;

M: computer <game-loop>
    you pairs:<pair> <game-loop> ;

: announce-next ( game -- game ) announce >>next-time ;
: skip-next ( game -- game )         skip >>next-time ;

: record-turn ( game who add -- )
    swapd [ turn-hists>> ] dip '[ _ suffix ] change-at ;

GENERIC: inc-score ( game who add -- game+ new )
M: skip inc-score
    2drop skip-next quitter ;

M: integer inc-score
    [ record-turn ]
    [ nip '[ _ + ] change-current drop ]
    [ 2drop dup current>> ] 3tri ;

DEFER: your-prompt

GENERIC: your-choice ( who input -- n )
M: f your-choice
    2drop skip ;
M: string your-choice
    string>number your-choice ;
M: valid-human-input your-choice
    nip ;
M: integer your-choice
    drop your-prompt ;

: your-prompt ( who -- n )
    dup human-prompt read-you your-choice ;

: your-turn ( game who -- game )
    [ dup your-prompt [ inc-score ] keep drop ] keep
    says { "!" "." } random "%s%s\n" printfl ;

: simple-bot-turn ( game who n -- game )
    '[ _ inc-score ] keep
    says "%d.\n" printfl ;

GENERIC: my-turn ( game who algo -- game )
M: algo-low my-turn
    drop addables infimum simple-bot-turn ;
M: algo-high my-turn
    drop addables supremum simple-bot-turn ;

M: algo-cheat my-turn
    2drop 20 >>current
    computer says "20!" print ;

M: algo-random my-turn
    drop [ addables random [ inc-score ] keep swap ] keep
    says slow-dots
    "%d, "      printfl 3/4 sleep-sec
    "I guess. " writefl 1/4 sleep-sec
    "(%d)\n"    printfl ;

GENERIC: take-turn ( game who -- game )
M: computer take-turn
    algo-random my-turn ;
M: you take-turn
    your-turn ;

GENERIC: win ( game who -- game )
M: computer win
    says you-lose print ;
M: you win
    drop computer says i-lose print ;

GENERIC: update-game-mode ( game -- game )
M: game-over update-game-mode
    announce-next ;
M: 21-base-game update-game-mode ;

GENERIC#: game-tick 1 ( game who -- game )
M: playing-game game-tick
    take-turn update-game-mode ;
M: announcing-game game-tick
    win skip-next ;
M: skipping-game game-tick
    drop ;

! boolean "OR" logic implemented with generics to avoid "if"
DEFER: 21-game-loop
<PRIVATE
GENERIC#: (game-loop2) 1 ( game loop -- game loop )
M: skipping-game (game-loop2) ;
M: 21-base-game (game-loop2)
    [ first game-tick ]
    [ rest 21-game-loop ] bi ;

GENERIC: (game-loop) ( game loop -- game loop )
M: empty-sequence (game-loop)
    (game-loop2) ;
M: sequence (game-loop)
    (game-loop2) ;
PRIVATE>

: 21-game-loop ( game loop -- game loop )
    (game-loop) ;

: game-against ( against -- game loop )
    [ <21-base-game> ]
    [ <game-loop> ] bi ;

: 21-game ( against -- )
    help. game-against 21-game-loop drop game-stats. ;

GENERIC: play-21 ( against -- )
M: you play-21
    drop computer says "You can't play against yourself!" print ;

M: computer play-21
    21-game ;

: play-21-against-computer ( -- ) computer play-21 ;
MAIN: play-21-against-computer
