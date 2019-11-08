REBOL [
	Title: "Get Gravatar"
	File: %gravatar.r
	Type: 'module
	Exports: [get-gravatar]
	Date: 27-Nov-2012
	Purpose: "Generate Gravatar Profile Image URLs"
]

get-gravatar: use [to-md5][
	to-md5: func [val [any-string!]][
		lowercase enbase/base checksum/method lowercase trim copy val 'md5 16
	]
	func [
		"Obtain the Gravatar Profile Image for the given Email Address"
		email [email!] "Email Address to Lookup"
		/default image [url! word!]
		/size width [integer!]
	][
		join http://www.gravatar.com/avatar/ [
			to-md5 email
			to-webform/prefix compose [
				d (all [image form image])
				s (width)
				; 404 mm identicon monsterid wavatar retro blank
			]
		]
	]
]