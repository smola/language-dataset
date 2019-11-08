Red [
	Title:		"ZeroMQ request/reply server example"
	Author:		"Kaj de Vos"
	Rights:		"Copyright (c) 2011-2014 Kaj de Vos"
	License: {
		PD/CC0
		http://creativecommons.org/publicdomain/zero/1.0/
	}
	Needs: {
		Red >= 0.4.3
		%ZeroMQ-binding.red
	}
	Tabs:		4
]


#include %../ZeroMQ-binding.red


prin ["0MQ version:" major-version]
	prin #"."  prin minor-version
	prin #"."  print patch-version
prin newline


; Hello World client/server

address: any [get-argument 1  tcp://*:5555]
reply: "World"

log-error: does [  ; FIXME: should go to stderr
	print form-error system-error
]

server: function [] [
	either zero? pool: make-pool 1 [
		log-error
	][
		either zero? socket: open-socket pool reply! [
			log-error
		][
			either serve socket address [
				print ["Awaiting requests on" address]
				data: ""

;				forever [
				while [yes] [
					unless all [
						receive/into socket data
						(
							print ["Received request:" data]
							send socket reply
						)
					][
						log-error
					]
				]
			][
				log-error
			]
			unless close-socket socket [log-error]
		]
		unless end-pool pool [log-error]
	]
]

server
