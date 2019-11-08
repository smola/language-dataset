Red [
    Title:          "svg-agent"
    Description:    "Contains some helper functions related to SVG"
    Author:         "Iosif Haidu"
    Tabs:           4
    Needs:          'View
    Rights:         "Copyright (c) 2016-2020 Iosif Haidu. All rights reserved."
    License: {
        Redistribution and use in source and binary forms, with or without modification,
        are permitted provided that the following conditions are met:

            * Redistributions of source code must retain the above copyright notice,
                this list of conditions and the following disclaimer.
            * Redistributions in binary form must reproduce the above copyright notice,
                this list of conditions and the following disclaimer in the documentation
                and/or other materials provided with the distribution.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
        ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
        WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
        DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
        FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
        DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
        SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
        CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
        OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
	}
]

svg-agent: object [
    data:   none
    tokens: object [
        wsp:        charset reduce [ space tab cr lf ]
        comma-wsp:  charset reduce [ comma space tab cr lf ]
        digit:      charset "0123456789"
        number:     [ any [#"+" | #"-"] some digit ]
    ]
    currentColor:   none    ;-- used by 'stroke' (and maybe other properties). It must be updated
                            ;-- by 'color' property

    width:      none        ;-- width of main container where SVG will be drawn (must be set prior of SVG processing) 
    height:     none        ;-- height of main container where SVG will be drawn (must be set prior of SVG processing)

    result: make block! 20   ; predefined to avoid allocations ( missing GC )
    temp:   make block! 5    ; predefined to avoid allocations ( missing GC )
    points: make block! 100

    display: object [
        px: 1
        pt: 1.25 * px
        pc: 15 * px
        ;mm: 3.543307 * px
        mm: 3.787878 * px       ;-- device dependent
        cm: mm * 10 * px        ;-- device dependent
        in: 90 * px
    ]
    get-user-unit: func [ text /local unit-chars unit value v device ][
        if none? text [ return none ]
        device: display
        unit-chars: charset "pxtcmin%"
        value: copy "" unit: copy ""
        either not parse text [ copy value to unit-chars copy unit thru end ][
            value: to float! text
            unit: "px"
        ][ value: to float! value ]
        case [
            any [ unit = "px" empty? unit ] [ value ]
            unit = "pt" [ value * device/pt ]
            unit = "pc" [ value * device/pc ]
            unit = "mm" [ value * device/mm ]
            unit = "cm" [ value * device/cm ]
            unit = "in" [ value * device/in ]
            unit = "%"  [ 1% * value]           ; WARNING !!! Depends on operands position for '%'
            true [ none ]
        ]
    ]
    get-percent-value: func [ value prct name [ word! ] svg-obj [ object! ] /local tmp ][
        either tmp: svg-elements/get-parent-attr name svg-obj [
            tmp * prct
        ][  value * prct ]
    ]
    get-color: func [ value /special ][
            either (copy/part value 3) = "url" [
                take/part value 3
                form pick to paren! value 1
            ][ 
                either special [ 
                    case [
                        value = "none"          [ 'off ]
                        value = "currentColor"  [ currentColor ]
                        value/1 = #"#"          [ hex-to-rgb to issue! next value ]
                        true                    [ get to word! value ]
                    ]
                ][ hex-to-rgb to issue! next value ] 
            ]
    ]
    get-transforms: function [ text [ string! none! ] /reversed ][
        if none? text [ return none ]
        
        white-space:    charset reduce [ space tab cr lf ]
        cwsp:           charset reduce [ comma space tab cr lf ]
        digit:          charset "0123456789"
        wsp:            [ any white-space ]
        comma-wsp:      [ some cwsp ]
        sign:           [ "+" | "-" ]
        digit-seq:      [ some digit ]
        integer-const:  digit-seq
        exponent:       [ [ "e" | "E" ] [ sign | none ] digit-seq ] 
        fract-const:    [ digit-seq "." digit-seq | digit-seq "." ]
        float-const:    [ fract-const [ exponent | none ] | digit-seq exponent ]
        number:         [ number-start: [ sign | none ] float-const number-end: | 
                          number-start: [ sign | none ] integer-const number-end: ] 
        number-start:   none
        number-end:     none

        either reversed [ save: :insert ][ save: :append ]
          
        get-number: func [ container [ block! ] ] [
            append container to float! copy/part number-start number-end
        ]
        set-matrix: function [ container [ block! ] value [ block! ]][
            save container compose/deep [ matrix [ ( value ) ] ]
            clear value
        ]
        set-translate: function [ container [block!] value [block!]][
            save container compose [translate (as-pair value/1 either (length? value) = 2 [ value/2 ][ 0 ]) ]
            clear value
        ]
        set-scale: function [ container [block!] value [block!]][
            save container compose [scale (either (length? value) = 2 [ reduce [ value/1 value/2 ] ] [ reduce [ value/1 value/1 ] ]) ]
            clear value
        ]
        set-rotate: function [ container [block!] value [block!]][
            save container compose [rotate (either (length? value) = 3 [ reduce [ value/1 as-pair value/1 value/2 ] ] [ value/1 ])  ]
            clear value
        ]
        set-skewX: function [ container [block!] value [block!]][
            save container compose [skew (value/1) ]
            clear value
        ]
        set-skewY: function [ container [block!] value [block!]][
            save container compose [skew 0 (value/1) ]
            clear value
        ]

        rule: [ any [
            wsp [
                "matrix"  wsp "("  wsp  
                    5 [ number ( get-number temp ) comma-wsp ] 
                    number ( get-number temp ) 
                    wsp ")" 
                    ( set-matrix result temp )
                | "translate"  wsp "("  wsp  
                    number ( get-number temp ) 
                    [ comma-wsp number ( get-number temp ) | none ]
                    wsp ")" 
                    ( set-translate result temp )
                | "scale"  wsp "("  wsp 
                    number ( get-number temp ) 
                    [ comma-wsp number ( get-number temp ) | none ]  
                    wsp ")"
                    ( set-scale result temp )
                | "rotate"  wsp "("  wsp 
                    number ( get-number temp ) 
                    [ comma-wsp number ( get-number temp ) 
                      comma-wsp number ( get-number temp ) | none ]  
                    wsp ")"
                    ( set-rotate result temp )
                | "skewX"  wsp "("  wsp 
                    number ( get-number temp )  
                    wsp ")"
                    ( set-skewX result temp )
                | "skewY"  wsp "(" wsp 
                    number ( get-number temp )  
                    wsp ")"
                    ( set-skewY result temp )
            ]
        ]]
        clear result clear temp
        parse text rule
        result
    ]
    get-multipoints: function [ data [ string! ] /path /plain ][
        clear points
        wsp:        tokens/wsp
        comma-wsp:  tokens/comma-wsp
        number:     tokens/number
        count:      0
        shape:      none
        container:  object [
            ;data:   make block! 100
            push:   function [ coll value ][ append coll value ]
            generate:   function [][
                switch first data [
                    any [#"M" #"m"] []
                    any [#"L" #"l"] []
                ]
            ]
        ]
        points-rule: [
            any [
                any wsp s: number (count: count + 1)
                some [  change [comma-wsp] "x" 
                        | change [dot [to comma-wsp | to end] ] ""  ;-- draw dialect doesn't support float! for coordinates 
                        | p: number e: (
                            count: count + 1
                            either (count % 2) = 0 [
                                container/push points to pair! copy/part s e 
                            ][ s: p ]
                          )
                ] 
            ]
        ]
        plain-rule: [ any [ s: number e: any comma-wsp ( container/push points copy/part s e ) ]]
        path-rule:  [ 
            any [ 
                [
                    [ #"M" ( container/push points [ move ] ) | #"m" ( container/push points [ 'move ] ) ] 
                    | [ #"L" ( container/push points [ line ] ) | #"l" ( container/push points [ 'line ] ) ]
                    | [ #"C" ( container/push points [ curve ] ) | #"c" ( container/push points [ 'curve ] ) ]
                    | [ #"S" ( container/push points [ curv ] ) | #"s" ( container/push points [ 'curv ] ) ]
                    | [ #"Q" ( container/push points [ qcurve ] )| #"q" ( container/push points [ 'qcurve ] ) ]
                    | [ #"T" ( container/push points [ qcurv ] ) | #"t" ( container/push points [ 'qcurv ] ) ]
                    | [ #"H" ( container/push points [ hline ] ) | #"h" ( container/push points [ 'hline ] ) ]
                    | [ #"V" ( container/push points [ vline ] ) | #"v" ( container/push points [ 'vline ] ) ]
                    | [ #"A" ( container/push points [ arc ] ) | #"a" ( container/push points [ 'arc ] ) ]
                ]
                points-rule
            ]
        ]

        rule: none
        case [
            path    [ rule: path-rule ]
            plain   [ rule: plain-rule ]
            true    [ rule: points-rule ]
        ]
        parse data rule
        points
    ]

    get-viewbox: function [ data [ string! none! ]][
        if none? data [ return none ]
        collect [ foreach point get-multipoints/plain data [keep to integer! point ]]
    ]
]
