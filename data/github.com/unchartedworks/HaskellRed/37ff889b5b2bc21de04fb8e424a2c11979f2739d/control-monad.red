Red [
    Title:   "Control.Monad"
    Author:  "unchartedworks"
    File: 	 %control-monad.red
    Tabs:	   4
    Rights:  "unchartedworks. All rights reserved."
    License: "MIT"
]

sequencially-compose-pass*: function [
    "Sequentially compose two actions, passing any value produced by the first as an argument to the second."
    x [object! series!]
    f [function!]
][
    case [
        object? x (sequencially-compose-pass-object* x :f)
        series? x (sequencially-compose-pass-series* x :f)
    ]
]

sequencially-compose-pass-object*: function [
    x [object!]
    f [any-function!]
][
    case [
        (isLeft x) x
        (isRight x) (f fromRight x)
        (isNothing x) Nothing
        (isJust x) (f fromJust x)
    ]
]

sequencially-compose-pass-series*: function [
    xs [series!]
    f [any-function!]
][
    case [
        (empty? xs) xs
        (series? xs) (concatMap :f xs)
    ]
]


>>=: make op! :sequencially-compose-pass*

sequencially-compose-discard*: function [
    "Sequentially compose two actions, passing any value produced by the first as an argument to the second."
    x [object! series!]
    y [object! series!]
][
    case [
        ((object? x) && (object? y)) (sequencially-compose-discard-object* x y)
        ((series? x) && (series? y)) (sequencially-compose-discard-series* x y)
    ]
]

sequencially-compose-discard-object*: function [
    x [object!]
    y [object!]
][
    case [
        (isLeft x) x
        (isRight x) y
        (isNothing x) Nothing
        (isJust x) y
    ]
]

sequencially-compose-discard-series*: function [
    xs [series!]
    ys [series!]
][
    case [
        (empty? xs) xs
        (series? xs) ys
    ]
]

>>: make op! :sequencially-compose-discard*

;;Basic Monad functions
; mapM: function [
;     "Map each element of a structure to a monadic action, evaluate these actions from left to right, and collect the results. For a version that ignores the results see mapM_."
;     f: [any-function!]
;     xs: [object! series!]
; ][
;     case [
;         object? x (mapM-object* :f xs)
;         series? x (mapM-series* :f xs)
;     ]
; ]

; mapM-object: function [
;     f: [any-function!]
;     xs: [object! series!]
; ][

; ]

; mapM-object: function [
;     f: [any-function!]
;     xs: [object! series!]
; ][
;     foldl :f
; ]

;;Monadic lifting operators
liftM: function [
    "Promote a function to a monad."
    f [any-function!]
    mx [object! series!]
][
    mx >>= (function [x] [liftM* mx :f x])
]

liftM*: function [
    mx [object! series!]
    f [any-function!]
    x
][
    case [
        (object? mx) (liftM-object* mx :f x)
        (series? mx) (liftM-series* mx :f x)
    ]
]

liftM-object*: function [
    mx [object!]
    f [any-function!]
    x
][
    case [
        ((isJust mx) || (isNothing mx)) (Just (f x))
        ((isLeft mx) || (isRight mx)) (Right (f x))
    ]
]

liftM-series*: function [
    mx [series!]
    f [any-function!]
    x
][
    either (string? mx) [to-string f x][reduce [f x]]
]

liftM2: function [
    "Promote a function to a monad, scanning the monadic arguments from left to right."
    f [any-function!]
    mx [object! series!]
    my [object! series!]
][
    ;;function [x] reduce ['function [y] 'reduce ['add :x 'y] ]
    g: (function [x] reduce ['function [y] 'reduce ['f :x 'y]])
    print mold :g
    h: function [x][g x]
    ;print mold :h
    ;i: h 2
    ;print mold (i 2)
    ;print mold :i
    
    s: (liftM :g [1 2])
    print mold :s
    []
    ;my >>= :fs
]

; liftM2*: function [
;     mx [object! series!]
;     f [any-function!]
;     x
;     y
; ][
;     case [
;         (object? mx) (liftM-object* mx :f x)
;         (series? mx) (liftM-series* mx :f x)
;     ]
; ]

; liftM-object*: function [
;     mx [object!]
;     f [any-function!]
;     x
;     y
; ][
;     case [
;         ((isJust mx) || (isNothing mx)) (Just (f x))
;         ((isLeft mx) || (isRight mx)) (Right (f x))
;     ]
; ]

; liftM-series*: function [
;     mx [series!]
;     f [any-function!]
;     x
;     y
; ][
;     either (string? mx) [to-string f x][reduce [f x]]
; ]