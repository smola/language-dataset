# Common postprocessors
@{% const nuller = () => null %}
@{% const denest = data => data[0] %}

# We use a lexer to split the string into tokens
@{%
	const moo = require('moo')

	const lexer = moo.compile({
		comment: /#.*/,
		literal: /".*?"/, // Exact phrases can be included in double quotes
		whitespace: { match: /\s+/, lineBreaks: true },
		number: /\d+/,
		unpairedQuote: '"', // To be treated as whitespace
		orOperator: '|', // An alternative to 'OR'
		andOperator: '&', // An alternative to 'AND'
		fuzzyOperator: '~',
		boostOperator: '^',
		wildcard: /\$\d*/,
		lineNumber: /L\d+/,
		leftParen: '(',
		rightParen: ')',
		extensionOperator: '.',
		fieldOperator: '/',
		term: [
			{
				match: /[^\s"#\|&()\d\.\/~\^\$]+/,
				type: moo.keywords({
					booleanOperator: [
						'OR', 'AND', 'NOT', 'XOR',
						'or', 'and', 'not', 'xor',
					],
					proximityOperator: [
						'ADJ','NEAR', 'ONEAR', 'WITH','SAME',
						'adj','near', 'onear', 'with','same',
					],
					field: [
						'ATT', 'AT', 'KD', 'PARN', 'SRC', 'PDID', 'PD', 'PRAN', 'PRN', 'PRCO', 'PRC', 'PRAD',
						'PRD', 'PRAY', 'PRY', 'RLAN', 'RLPN', 'ART', 'UNIT', 'ASCI', 'ASCO', 'ASCC', 'ASTX', 'ASST',
						'ASZP', 'CCLS', 'COR', 'CCOR', 'CXR', 'CCXR', 'CLAS', 'ICLS', 'IOR', 'CIOR', 'IXR', 'CIXR',
						'IPCC', 'IPCR', 'IPC', 'CICL', 'DD', 'FS', 'BI', 'XA', 'XP', 'GI', 'INCI', 'INCO',
						'INCC', 'INTX', 'INST', 'INSA', 'INZP', 'PN', 'DID', 'ISD', 'PY', 'ISY', 'AB', 'BSUM',
						'CLM', 'DETD', 'DRWD', 'TI', 'PTAN', 'PTAD', 'PT3D', 'PTPN', 'PTPD', 'FRPN', 'FRCO', 'FIPC',
						'FRGP', 'FRCL', 'OREF', 'UREF', 'URGP', 'URCL', 'READ', 'REFD', 'REAN', 'REPD', 'REPN', 'R47X',
						'CPC', 'URPN', 'INV', 'AD', 'FD', 'AY', 'FY', 'PPPD', 'ASGP', 'AS', 'INGP', 'IN',
						'APNR', 'APN', 'APP', 'AP',
						'att', 'at', 'kd', 'parn', 'src', 'pdid', 'pd', 'pran', 'prn', 'prco', 'prc', 'prad',
						'prd', 'pray', 'pry', 'rlan', 'rlpn', 'art', 'unit', 'asci', 'asco', 'ascc', 'astx', 'asst',
						'aszp', 'ccls', 'cor', 'ccor', 'cxr', 'ccxr', 'clas', 'icls', 'ior', 'cior', 'ixr', 'cixr',
						'ipcc', 'ipcr', 'ipc', 'cicl', 'dd', 'fs', 'bi', 'xa', 'xp', 'gi', 'inci', 'inco',
						'incc', 'intx', 'inst', 'insa', 'inzp', 'pn', 'did', 'isd', 'py', 'isy', 'ab', 'bsum',
						'clm', 'detd', 'drwd', 'ti', 'ptan', 'ptad', 'pt3d', 'ptpn', 'ptpd', 'frpn', 'frco', 'fipc',
						'frgp', 'frcl', 'oref', 'uref', 'urgp', 'urcl', 'read', 'refd', 'rean', 'repd', 'repn', 'r47x',
						'cpc', 'urpn', 'inv', 'ad', 'fd', 'ay', 'fy', 'pppd', 'asgp', 'as', 'ingp', 'in',
						'apnr', 'apn', 'app', 'ap',
					],
				}),
			},
		],
	})
%}

@lexer lexer

query ->
	  _ clauses {% ([_, clauses]) => ({
			type: 'query',
			content: [clauses],
		}) %}
	| _ clauses comment {% ([_, clauses, comment]) => ({
			type: 'query',
			content: [
				clauses,
				comment,
			]
		}) %}

clauses -> clause:+ {% ([clauses]) => {
	if (clauses.length === 1) {
		return clauses[0]
	}
	return {
		type: 'clauses',
		content: clauses,
	}
} %}

clause ->
	  nonBooleanClause {% denest %}
	| booleanClause {% denest %}

nonBooleanClause ->
	  searchClause _ {% denest %}
	| filterClause _ {% denest %}

###################
## Filter Clause ##
# Filter clauses are generally removed from the search string
# and are used to search things other than the core text.
# Certain compound clauses (e.g. proximity) need to ignore filter clauses.
filterClause ->
	  fieldClause {% denest %}
	| lineClause {% denest %}

###################
## Search Clause ##
# A search clause is used to identify specific terms in some way.
searchClause ->
	  atomicTerm {% denest %}
	| fuzzyClause {% denest %}
	| boostClause {% denest %}
	| closedClause {% denest %}
	| proximityClause {% denest %}

booleanClause -> nonBooleanClause booleanOperator __ clause {% ([left, operator, _, right]) => ({
	type: 'booleanClause',
	left,
	operator,
	right,
}) %}

atomicTerm ->
	  %term {% ([term]) => ({
			type: 'text',
			content: term.text,
		}) %}
	| %literal {% ([literal]) => ({
			type: 'literal',
			content: literal.text.slice(1,-1),
		}) %}
	| %number {% ([number]) => ({
			type: 'text',
			content: number.text,
		}) %}
	| wildcardClause {% denest %}

##############
## Comments ##
# Anything following ‘#’ will be completely removed from the search text.
comment -> %comment {% ([comment]) => ({
	type: 'comment',
	content: comment.text.substring(1).trim(),
}) %}

####################
## Closed Clauses ##
# clauses contained in parentheses
# TODO: This parser assumes balanaced parentheses
closedClause -> %leftParen _ clause %rightParen {% ([lParen, _, clause, rParen]) => clause %}

#######################
## Proximity Clauses ##
# clauses that identify pairs of nearby terms
proximityClause -> searchClause _ proximityOperator __ searchClause {% ([left, _1, operator, _2, right ]) => ({
	type: 'proximityClause',
	left,
	operator,
	right,
})%}

###################
## Field Clauses ##
# Users can search via specific field, either invoking
# - extension: `*.FIELD`
# - field flag: `FIELD/*`
fieldClause ->
	  extension {% denest %}
	| flag {% denest %}

extension -> atomicTerm %extensionOperator %field {% ([atomicTerm, extensionOperator, field]) => ({
	type: 'fieldClause',
	field: field.text,
	term: atomicTerm,
})%}

flag -> %field %fieldOperator atomicTerm {% ([field, fieldOperator, atomicTerm]) => ({
	type: 'fieldClause',
	field: field.text,
	term: atomicTerm,
})%}

##################
## Fuzzy Clause ##
# ‘~’ if used in search text will always have a number following ‘~’
# and  will be interpreted as ‘FUZZY’ of the preceding string with a
# similarity of the following number.
fuzzyClause -> atomicTerm %fuzzyOperator %number {% ([atomicTerm, fuzzyOperator, number]) => ({
	type: 'fuzzyClause',
	term: atomicTerm,
	fuzzyValue: number.text,
}) %}

##################
## Boost Clause ##
# ‘^’ if used in search text will always have a number following ‘^’
# and this number will be used as ‘BOOST’ value for the string preceding ‘^’.
boostClause -> atomicTerm %boostOperator %number {% ([atomicTerm, boostOperator, number]) => ({
	type: 'boostClause',
	term: atomicTerm,
	boostValue: number.text,
}) %}

##################
## Wildcard Clause ##
# ‘$‘ will be interpreted as any number of characters
# ‘$n’ will be interpreted as n number of characters
wildcardClause -> atomicTerm %wildcard {% ([atomicTerm, wildcard]) => {
	const modifier = '0' + wildcard.text.substring(1);
	return {
		type: 'postfixWildcard',
		term: atomicTerm,
		modifier: Number.parseInt(modifier, 10),
	}
} %}

#################
## Line Clause ##
# Line numbers used in search text will be of the form L followed by the line number
lineClause -> %lineNumber {% ([lineNumber]) => ({
	type: 'lineClause',
	lineNumber: lineNumber.text.substring(1),
}) %}

#######################
## Boolean Operators ##
# These operators allow for combined clauses.
# - OR (or |)
# - AND (or &)
# - NOT
# - XOR
booleanOperator ->
	  %booleanOperator {% ([operator]) => ({ type: operator.text }) %}
	| %orOperator {% ([operator]) => ({ type: 'OR' }) %}
	| %andOperator {% ([operator]) => ({ type: 'AND' }) %}

#########################
## Proximity Operators ##
# Clauses that contain proximity operators.
#
# Proximity operators make it possible to compare distance between terms
# - ADJ: TermA next to TermB in the order specified in the same sentence.
# - NEAR: next to Terms in any order in the same sentence.
# - ONEAR: same as NEAR but order matters
# - WITH: TermA in the same sentence with TermB.
# - SAME: TermA in the same paragraph with Terms
#
# You can also modify distances for some proximity clauses
# - ADJn: TermA within n terms of Bin the order specified in the same sentence.
# - NEARn: TermA within n terms of B in any order in the same sentence.
# - ONEARn: same as NEARn but order matters
# - SAMEn: TermA within n paragraphs of TermB
# where 'n' is a number
proximityOperator ->
	  %proximityOperator {% ([operator]) => ({
			type: operator.text,
			modifier: null
		}) %}
	| %proximityOperator %number {% ([operator, modifier]) => ({
			type: operator.text,
			modifier: modifier.text,
		}) %}

################
## Whitespace ##
_ -> (whitespace:+):? {% nuller %}
__ -> whitespace {% nuller %}

whitespace ->
	  %whitespace
	| %unpairedQuote
