REBOL [
	title: "SO chat classifier"
	file: %httpd.reb
	author: [abolka "Graham Chiu"]
	date: [4-Nov-2009 11-May-2014]
	version: 0.0.7
	notes: {
            0.0.7 change to using userids and not names since names can change every 30 days

          }
]

do http://reb4.me/r3/altwebform.r
import http://reb4.me/r3/altjson.r

so-db: [
	admin [
		"76852" ["password" "GrahamChiu"]
		"135724" ["password" "earl"]
		"1864998" ["password" "johnk"]
		"292969" ["password" "rgchris"]
	]
	"altme-announce" []
	"announce" []
	"android" []
	"Cheyenne" []
	"curecode" []
	"databases" []
	"general" [10 11 12]
	"google-groups" []
	"parse" []
	"schemes" []
	"rebol2" []
	"rebol3" [1 2 3]
	"red" [4 5 6]
	"r3gui" [7 8 9]
	"sl4a" []
]

save-db: does [save %so-db.reb so-db]

either exists? %so-db.reb [
	so-db: load %so-db.reb
][
	save-db
]

remove-key: funct [key so-db][
	foreach [class db] next next so-db [
		if at: find db key [remove at]
	]
]
code-map: make map! [200 "OK" 400 "Forbidden" 404 "Not Found"]
mime-map: make map! ["html" "text/html" "jpg" "image/jpeg" "r" "text/plain"]
error-template: {
    <html><head><title>$code $text</title></head><body><h1>$text</h1>
    <p>Requested URI: <code>$uri</code></p><hr><i>shttpd.r</i> on
    <a href="http://www.rebol.com/rebol3/">REBOL 3</a> $r3</body></html>
}

error-response: func [code uri /local values] [
	values: [code (code) text (code-map/:code) uri (uri) r3 (system/version)]
	reduce [code "text/html" reword error-template compose values]
]

send-response: func [port res /local code text type body] [
	set [code type body] res
	write port ajoin ["HTTP/1.0 " code " " code-map/:code crlf]
	write port ajoin ["Content-type: " type crlf crlf]
	write port body
]

separator: "^/^/"
handle-request: func [config req
	/local uri data result outer db admin-db user-data start finish class verb
] [
	print ["Request is: " to string! req]
	data: {"notok - default fall thru"}
	either parse to string! req [
		copy verb to space space [
			copy uri to space if (remove uri equal? verb "GET") to end
			|
			thru separator copy URI to end
		]
	][
		print "parsed request okay"
		?? uri
		either error? try [
			uri: load-webform uri
		][
			data: {"notok - faulty parameters"}
		][
			?? URI
			?? verb
			switch/default verb [
				"GET" [
					; gets from start to end of result
					class: start: finish: none
					either parse URI [
						some [
							'class set class string!
							|
							'start set start string! (start: attempt [to integer! start])
							|
							'end set finish string! (finish: attempt [to integer! finish])
						]
					][
						case [
							all [none? start none? finish none? class][
								;  wants all the group names
								data: to-json collect [
									foreach [class db] next next so-db [
										keep class
									]
								]
							]
							all [none? start none? finish class][
								; assume wants all this named group values
								either db: select so-db class [
									data: to-json append/only copy reduce [class] db
								][
									; class does not exist
									data: {"notok - noexistent class"}
								]
							]
							all [class start finish][
								; named class with a specified range
								set [start finish] sort reduce [start finish]
								either db: select so-db class [
									result: collect [
										foreach id db [
											if all [
												id >= start
												id <= finish
											][
												keep id
											]
										]

									]

									data: to-json append/only copy reduce [class] result
								][
									; class does not exist
									data: {"notok - noexistent class"}
								]
							]
							all [start finish none? class][
								; unnamed class with a specified range
								set [start finish] sort reduce [start finish]
								outer: copy []
								foreach [class db] next next so-db [
									result: collect [
										foreach id db [
											if all [
												id >= start
												id <= finish
											][
												keep id
											]
										]

									]

									if not empty? result [
										append outer class
										append/only outer result
									]
								]
								probe outer
								data: to-json outer
							]
							true [
								data: {"notok - wrong combination of parameters for GET"}
							]
						]
					][
						; default for not being able to parse parameters
						data: to-json collect [
							foreach [class db] next next so-db [
								keep class
							]
						]

					]
				]
				"DELETE" [; DELETE "user=Graham&password=xyz&start=n&end=n"
					print "entered DELETE"
					user: password: start: finish: none
					either parse URI [
						some [
							'user set user string!
							|
							'password set password string!
							|
							'start set start string! (start: attempt [to integer! start])
							|
							'end set finish string! (finish: attempt [to integer! finish])
						]
					][
						print "parsed DELETE url okay"
						either all [
							user password start finish
						][; now to check if authorised
							admin-db: select so-db 'admin
							either user-data: select admin-db user [
								print "found user"
								?? user-data
								either user-data/1 = password [
									print "user passsword is okay"
									; authorised - found user and password matches
									set [start finish] sort reduce [start finish]
									; now start deleting all keys.  Might be duplicates so go thru all groups
									while [start <= finish] [
										remove-key start so-db
										++ start
									]
									save-db
									data: {"ok"}
								][
									data: {"notok - not authorised"}
								]
							][
								data: {"notok - not authorised"}
							]
						][
							data: {"notok - not all params supplied to DELETE needs user password start end"}
						]
					][
						data: {"notok - not parse PUT rule"}
					]
				]

				"PUT" [
					print "entered PUT"
					either parse URI [
						some [
							'class set class string!
							|
							'start set start string! (start: attempt [to integer! start])
							|
							'end set finish string! (finish: attempt [to integer! finish])
						]
					][
						print "parsed PUT url okay"
						either all [
							class start finish
						][
							either db: select so-db class [
								set [start finish] sort reduce [start finish]
								while [start <= finish] [
									if not find db start [
										append db start
									]
									++ start
								]
								db: sort unique db
								save-db
								data: {"ok"}
							][
								data: {"notok - unknown class"}
							]
						][
							data: {"notok - not all params supplied to PUT"}
						]
					][
						data: {"notok - not parse PUT rule"}
					]
				]

				"POST" [; used for admin functions such as adding groups, changing passwords etc
					data: {"notok - currently not supported"}
				]
			][
				data: {"notok - unrecognised html verb"}
			]
		]
	][
		print "failed to parse req"
		data: {"notok - failed to parse request"}
	]
	reduce [200 "application/json" data]
]

awake-client: func [event /local port res] [
	port: event/port
	switch event/type [
		read [
			either find port/data to-binary join crlf crlf [
				res: handle-request port/locals/config port/data
				send-response port res
			] [
				read port
			]
		]
		wrote [close port]
		close [close port]
	]
]
awake-server: func [event /local client] [
	if event/type = 'accept [
		client: first event/port
		client/awake: :awake-client
		read client
	]
]


serve: func [web-port web-root /local listen-port] [
	print ajoin ["Serving on local port " web-port " at " what-dir]
	listen-port: open join tcp://: web-port
	listen-port/locals: construct compose/deep [config: [root: (web-root)]]
	listen-port/awake: :awake-server
	wait listen-port
]

serve 8080 system/options/path
halt

