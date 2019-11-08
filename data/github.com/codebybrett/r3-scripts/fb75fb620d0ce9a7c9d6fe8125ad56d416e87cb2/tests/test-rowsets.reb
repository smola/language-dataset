REBOL []


do http://codeconscious.com/rebol-scripts/rowsets.r

players: [
	words [name score]
	rows [
		[{Tom} 4]
		[{Dick} 9]
		[{Harry} 7]
	]
]

join-and-select: func [] [

	rowset/query [

		select [
			w: x/name
			l: y/name
			s: x/score * 10
		]
		join [
			x/score < y/score
			x/name <> y/name
		]
		from x players
		from y players
	]
]

simple-update: func [] [

	rowset/query [

		select *
		update [
			#new
			x: x + 10
			#fn [{Old: } x {New: } new/x]
		]
		from a [words [x] rows [[1] [2] [3]]]
	]
]

requirements 'test-rowsets [

	[{Join and Select.}

		equal? join-and-select [
			words [w l s]
			rows [
				["Tom" "Dick" 40]
				["Tom" "Harry" 40]
				["Harry" "Dick" 70]
			]
		]
	]

	[{Updates.}

		equal? simple-update [
			words [x]
			rows [
				[11]
				[12]
				[13]
			]
		]

	]
]