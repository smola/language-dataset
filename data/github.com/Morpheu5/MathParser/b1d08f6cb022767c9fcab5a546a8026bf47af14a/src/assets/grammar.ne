@{%
const greekLetterMap = { "alpha": "α", "beta": "β", "gamma": "γ", "delta": "δ", "epsilon": "ε", "varepsilon": "ε", "zeta": "ζ", "eta": "η", "theta": "θ", "iota": "ι", "kappa": "κ", "lambda": "λ", "mu": "μ", "nu": "ν", "xi": "ξ", "omicron": "ο", "pi": "π", "rho": "ρ", "sigma": "σ", "tau": "τ", "upsilon": "υ", "phi": "ϕ", "chi": "χ", "psi": "ψ", "omega": "ω", "Gamma": "Γ", "Delta": "Δ", "Theta": "Θ", "Lambda": "Λ", "Xi": "Ξ", "Pi": "Π", "Sigma": "Σ", "Upsilon": "Υ", "Phi": "Φ", "Psi": "Ψ", "Omega": "Ω" }
const moo = require("moo")
const lexer = moo.compile({
    Int: /[0-9]+/,
    IdMod: /[a-zA-Z]+_(?:prime)/,
    Id: { match: /[a-zA-Z]+(?:_[a-zA-Z0-9]+)?/, keywords: {
	TrigFn: ['cos', 'sin', 'tan',
             'cosec', 'sec', 'cot',
             'cosh', 'sinh', 'tanh', 'cosech', 'sech', 'coth',
             'arccos', 'arcsin', 'arctan',
             'arccosec', 'arcsec', 'arccot',
             'arccosh', 'arcsinh', 'arctanh', 'arccosech', 'arcsech', 'arccoth',
             'acos', 'asin', 'atan',
             'acosec', 'asec', 'acot',
             'acosh', 'asinh', 'atanh', 'acosech', 'asech', 'acoth',
            ],
    Fn: ['ln', 'abs'],
    Log: ['log'],
    Radix: ['sqrt'],
    Derivative: ['diff', 'Derivative'],
         },
    },
    Rel: ['=', '==', '<', '<=', '>', '>='],
    PlusMinus: ['+', '-', '±', '-', '-'], // The minus signs are not all the same
    Pow: ['**', '^'],
    Mul: ['*', '×'],
    Div: ['/', '÷'],
    Lparen: '(',
    Rparen: ')',
    Comma: ',',
    c: /./,
})

let _window = null
try {
    _window = window
} catch (error) {
    _window = { innerWidth: 800, innerHeight: 600 }
}

const _rightChainToArray = (node) => {
    let n = node
    let a = [n]
    while (n.children.right) {
        n = n.children.right
        a = [...a, n]
    }
    return a
}

const _findRightmost = (node) => {
    let n = node
    while (n.children.right) {
        n = n.children.right
    }
    return n
}

const _safeToRemove = (node) => {
    return node && node.type === 'Brackets' && _.isEmpty(_.omit(node.children, 'argument'))
}

const _simplify = (node) => {
    node.children = _.mapValues(node.children, (v, k, c) => _simplify(v))

    if (node.type === 'Brackets' || node.type === 'Fn') {
        let argument = node.children.argument
        if (_safeToRemove(argument)) {
            node.children.argument = argument.children.argument
        }
    }
    if (node.type === 'Fraction') {
        let numerator = node.children.numerator
        if (_safeToRemove(numerator)) {
            node.children.numerator = numerator.children.argument
        }
        let denominator = node.children.denominator
        if (_safeToRemove(denominator)) {
            node.children.denominator = denominator.children.argument
        }
    }
    let superscript = node.children.superscript
    if (_safeToRemove(superscript)) {
        node.children.superscript = superscript.children.argument
    }
    return node
}

const processMain = (d) => {
    let main = _.cloneDeep(d[1])
    main.position = { x: _window.innerWidth/4, y: _window.innerHeight/3 }
    main.expression = { latex: "", python: "" }
    return _simplify(main)
}

const processRelation = (d) => {
    let lhs = _.cloneDeep(d[1])
    let rhs = _.cloneDeep(d[5])
    let relText = d[3].text === '==' ? '=' : d[3].text
    let relation = { type: 'Relation', properties: { relation: relText }, children: { right: rhs } }
    let r = _findRightmost(lhs)
    r.children['right'] = relation
    lhs = _simplify(lhs)
    return { ...lhs, position: { x: _window.innerWidth/4, y: _window.innerHeight/3 }, expression: { latex: "", python: "" } }
}

const processBrackets = (d) => {
    let arg = _.cloneDeep(d[2])
    return { type: 'Brackets', properties: { type: 'round' }, children: { argument: arg } }
}

const processFunction = (d) => {
    let arg = _.cloneDeep(d[3])
    // FIXME Split this into two functions and separate parsing rules.
    if (d[0].text === 'abs') {	
        return { type: 'AbsoluteValue', children: { argument: arg } }	
    } else {	
        return { type: 'Fn', properties: { name: d[0].text, allowSubscript: d[0].text !== 'ln', innerSuperscript: false }, children: { argument: arg } }	
    }
}

const processSpecialTrigFunction = (d_name, d_arg, d_exp = null) => {
    // First, deal with the shortened a- form of arc- functions.
    let r = new RegExp('^a([^r].+)$')
    let name = d_name.text
    if (r.test(name)) {
        name = `arc${r.exec(name)[1]}`
    }
    // Then do everything else as normal.
    let arg = _.cloneDeep(d_arg)
    let exp = _.cloneDeep(d_exp)
    if (null === exp) {
        return { type: 'Fn', properties: { name: name, allowSubscript: false, innerSuperscript: true }, children: { argument: arg } }
    } else {
        return { type: 'Fn', properties: { name: name, allowSubscript: false, innerSuperscript: true }, children: { superscript: exp, argument: arg } }
    }
}

const processLog = (arg, base = null) => {
    let log = { type: 'Fn', properties: { name: 'log', allowSubscript: true, innerSuperscript: false }, children: { argument: arg } }
    if (null !== base) {
        if (base.type === 'Num' || base.type === 'Symbol') {
            log.children['subscript'] = _.cloneDeep(base)
        }
    } else {
        // Treat log(x) as base 10 and make this very clear to the user.
        log.children['subscript'] = { type: 'Num', properties: { significand: '10' }, children: {} }
    }
    return log
}

const processRadix = (d) => {
    let arg = _.cloneDeep(d[3])
    return { type: 'Radix', children: { argument: arg } }
}

const processExponent = (d) => {
    let f = _.cloneDeep(d[0])
    let e = _.cloneDeep(d[4])
    let r = _findRightmost(f)

    if (e.type === 'BinaryOperation') {
        const exponentRight = _.cloneDeep(e.children.right.children.right)
        if (exponentRight) {
            f.children.right = exponentRight
            e.children.right.children = _.omit(e.children.right.children, 'right')
        }
    } else {
        const exponentRight = _.cloneDeep(e.children.right)
        if (exponentRight) {
            f.children.right = exponentRight
            e.children = _.omit(e.children, 'right')
        }
    }

    if (['Fn', 'Log', 'TrigFn'].includes(f.type)) {
        switch (f.properties.name) {
            case 'ln':
                return { type: 'Brackets', properties: { type: 'round' }, children: { argument: f, superscript: e } }
            case 'log':
                return { type: 'Brackets', properties: { type: 'round' }, children: { argument: f, superscript: e } }
            default:
                r.children['superscript'] = e
                return f
        }
    } else {
        r.children['superscript'] = e
        return f
    }
}

const processMultiplication = (d) => {
    let lhs = _.cloneDeep(d[0])
    let rhs = _.cloneDeep(d[d.length-1])
    let r = _findRightmost(lhs)
    r.children['right'] = rhs
    return lhs
}

const processFraction = (d) => {
    let denominatorRight = null
    if (d[4].type === 'BinaryOperation') {
        denominatorRight = _.cloneDeep(d[4].children.right.children.right)
        d[4].children.right.children = _.omit(d[4].children.right.children, 'right')
    } else {
        denominatorRight = _.cloneDeep(d[4].children.right)
        d[4].children = _.omit(d[4].children, 'right')
    }
    let numerator = _.cloneDeep(d[0])
    let numeratorChain = _rightChainToArray(numerator).map(e => { e.children = _.omit(e.children, 'right'); return e })
    let numeratorRight = numeratorChain.pop()

    let fraction = { type: 'Fraction', children: { numerator: numeratorRight, denominator: _.cloneDeep(d[4]) } }
    if (denominatorRight) {
        fraction.children.right = denominatorRight
    }
    
    if (numeratorChain.length > 0) {
        numeratorChain[numeratorChain.length-1].children.right = fraction
        return numeratorChain.reduceRight((a, e) => { e.children.right = a; return e })
    } else {
        return fraction
    }
}

const processPlusMinus = (d) => {
    let lhs = _.cloneDeep(d[0])
    let rhs = _.cloneDeep(d[4])
    let r = _findRightmost(lhs)
    r.children['right'] = { type: 'BinaryOperation', properties: { operation: d[2].text }, children: { right: rhs } }
    return lhs
}

const processUnaryPlusMinus = (d) => {
    return { type: 'BinaryOperation', properties: { operation: d[0].text }, children: { right: d[2] } }
}

const _processChainOfLetters = (s) => {
    let symbols = _.map(s.split(''), (letter) => {
        if (/[0-9]/.test(letter)) {
            return processNumber( [ {text:letter} ] )
        } else {
            return { type: 'Symbol', properties: { letter }, children: {} }
        }
    })
    let chain = _.reduceRight(symbols, (a, c) => {
        c.children['right'] = a
        return c
    })
    return chain
}

const processIdentifier = (d) => {
    const greekLetterKeys = Object.keys(greekLetterMap)
    let parts = d[0].text.split('_')

    // Perhaps we have a differential
    let patterns = ['[a-zA-Z]', ...greekLetterKeys].join('|')
    const diffMatcher = new RegExp(`^((?:d|D)(?:elta)?)(${patterns})$`)
    const diffMaybe = parts[0].match(diffMatcher)
    if (diffMaybe) {
        // We do have a differential
        return {
            type: 'Differential',
            properties: { letter: greekLetterMap[diffMaybe[1]] || diffMaybe[1] },
            children: { argument: processIdentifier([{ text: d[0].text.substring(diffMaybe[1].length) }]) }
        }
    } else {
        // We don't have a differential, business as usual
        if (greekLetterKeys.includes(parts[0])) {
            parts[0] = greekLetterMap[parts[0]]
        }
        let topChain = _processChainOfLetters(parts[0])
        if (parts.length > 1) {
            if (greekLetterKeys.includes(parts[1])) {
                parts[1] = greekLetterMap[parts[1]]
            }
            let chain = _processChainOfLetters(parts[1])
            let r = _findRightmost(topChain)
            r.children['subscript'] = chain
        }
        return topChain
    }
}

const processIdentifierModified = (d) => {
    const greekLetterKeys = Object.keys(greekLetterMap)
    let parts = d[0].text.split('_')
    if (greekLetterKeys.includes(parts[0])) {
        parts[0] = greekLetterMap[parts[0]]
    }
    let topChain = _processChainOfLetters(parts[0])
    let r = _findRightmost(topChain)
    r.properties['modifier'] = parts[1]
    return topChain
}

const processNumber = (d) => {
    return { type: 'Num', properties: { significand: d[0].text }, children: {} }
}

const processDerivative = (d) => {
    let numerator = {
        type: 'Differential',
        properties: { letter: 'd' },
        children: {
            argument: d[3]
        }
    }
    let denList = d[7].reduce((a, e) => {
        if (a.length == 0) {
            return [{ object: e, order: 1 }]
        } else {
            let last = a[a.length-1]
            if (e.type === 'Num') {
                last.order = parseInt(e.properties.significand)
                return [...a.slice(0, a.length-1), last]
            } else if (_.isEqual(e, last.object)) {
                last.order = last.order + 1
                return [...a.slice(0, a.length-1), last]
            } else {
                return [...a, { object: e, order: 1 }]
            }
        }
    }, []).map(e => {
        let differential = {
            type: 'Differential',
            properties: { letter: 'd' },
            children: {
                argument: e.object
            }
        }
        if (e.order > 1) {
            differential.children['order'] = {
                type: 'Num',
                properties: { significand: `${e.order}` },
                children: {}
            }
        }
        return differential
    })
    let order = denList.reduce( (a, e) => a + (parseInt(e.children.order ? e.children.order.properties.significand : "1")), 0)
    if (order > 1) {
        numerator.children['order'] = {
            type: 'Num',
            properties: { significand: `${order}` },
            children: {}
        }
    }
    let denominator = _.reduceRight(denList, (a, c) => {
        c.children['right'] = a
        return c
    })
    return {
        type: 'Derivative',
        children: {
            numerator,
            denominator
        }
    }
}
%}

@lexer lexer

### Behold, the Grammar!

main -> _ AS _                                                         {% processMain %}
      | _ AS _ %Rel _ AS _                                             {% processRelation %}

P ->                   %Lparen _ AS _                 %Rparen          {% processBrackets %}
   | %TrigFn           %Lparen _ AS _                 %Rparen          {% d => processSpecialTrigFunction(d[0], d[3], null) %}
   | %TrigFn %Pow NUM  %Lparen _ AS _                 %Rparen          {% d => processSpecialTrigFunction(d[0], d[5], d[2]) %}
   | %TrigFn           %Lparen _ AS _                 %Rparen %Pow NUM {% d => processSpecialTrigFunction(d[0], d[3], d[7]) %}
   | %Derivative       %Lparen _ AS _ %Comma _ ARGS _ %Rparen          {% processDerivative %}
   | %Log              %Lparen _ AS _                 %Rparen          {% (d) => { return processLog(d[3]) } %}
   | %Log              %Lparen _ AS _ %Comma _ VAR _  %Rparen          {% (d) => { return processLog(d[3], d[7]) } %}
   | %Log              %Lparen _ AS _ %Comma _ NUM _  %Rparen          {% (d) => { return processLog(d[3], d[7]) } %}
   | %Radix            %Lparen _ AS _                 %Rparen          {% processRadix %}
   | %Fn               %Lparen _ AS _                 %Rparen          {% processFunction %}
   | VAR                                                               {% id %}
   | NUM                                                               {% id %}
   | %PlusMinus _ P                                                    {% processUnaryPlusMinus %}

ARGS -> AS                                                             {% (d) => [d[0]] %}
      | ARGS _ %Comma _ AS                                             {% (d) => d[0].concat(d[4]) %}

E -> P _ %Pow _ E                                                      {% processExponent %}
   | P                                                                 {% id %}

# Multiplication and division
MD -> MD _ %Mul _ E                                                    {% processMultiplication %}
    | MD _ %Div _ E                                                    {% processFraction %}
    | MD _ E                                                           {% processMultiplication %}
    | E                                                                {% id %}

AS -> AS _ %PlusMinus _ MD                                             {% processPlusMinus %}
    | MD                                                               {% id %}

VAR -> %Id                                                             {% processIdentifier %}
     | %IdMod                                                          {% processIdentifierModified %}

NUM -> %Int                                                            {% processNumber %}

_ -> [\s]:*