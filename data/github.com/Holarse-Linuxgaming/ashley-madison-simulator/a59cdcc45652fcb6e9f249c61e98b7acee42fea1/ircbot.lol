OBTW
	irc_random_bot -- ein minimaler irc-bot
	Use with: [ ] #1 [ ] #2
	TODO: clean up
	TODO: allow more than two options
	TODO: make better code
TLDR

HAI 1.4
	CAN HAS STDIO?
	CAN HAS SOCKS?
	CAN HAS STRING?
	CAN HAS STDLIB?

	I HAS A channel ITZ "#holarse-gaming"
	I HAS A nick ITZ "Rainer_Zufall"
	I HAS A server ITZ "irc.oftc.net"
	I HAS A port ITZ 6667
	I HAS A local_port ITZ 12345

	BTW local
	I HAS A local
	local R I IZ SOCKS'Z BIND YR "ANY" AN YR local_port MKAY

	BTW get server address
	I HAS A addr
	addr R I IZ SOCKS'Z RESOLV YR server MKAY
	VISIBLE "IP of " AN server AN " is " AN addr

	BTW connect
	VISIBLE "Connecting…"
	I HAS A remote
	remote R I IZ SOCKS'Z KONN YR local AN YR addr AN YR port MKAY

	BTW register
	VISIBLE "Register…"
	I IZ SOCKS'Z PUT YR local AN YR remote AN YR SMOOSH "NICK " AN nick AN ":(0D):(0A)" MKAY MKAY
	I IZ SOCKS'Z PUT YR local AN YR remote AN YR "USER test test test ::test:(0D):(0A)" MKAY
	I IZ SOCKS'Z PUT YR local AN YR remote AN YR SMOOSH "JOIN " AN channel AN ":(0D):(0A)" MKAY MKAY

	HOW IZ I parse YR input AN YR start
		BTW VISIBLE "Start is:: " AN start
		BTW String-Länge
		I HAS A max_length ITZ I IZ STRING'Z LEN YR input MKAY
		BTW VISIBLE "max_length:: " AN max_length
		I HAS A j ITZ start
		I HAS A string ITZ A YARN
		I HAS A char
		IM IN YR loop UPPIN YR i TIL BOTH SAEM i AN max_length
			char R I IZ STRING'Z AT YR input AN YR j MKAY
			BTW VISIBLE "char:: " AN char AN " at " AN j
			DIFFRINT char AN " ", O RLY?
			YA RLY
				string R SMOOSH string AN char MKAY
				j R SUM OF j AN 1
				i R j
			NO WAI
				FOUND YR string
			OIC
		IM OUTTA YR loop
		FOUND YR string
	IF U SAY SO

	HOW IZ I parse_nick YR input AN YR start
		BTW VISIBLE "Start is:: " AN start
		BTW String-Länge
		I HAS A max_length ITZ I IZ STRING'Z LEN YR input MKAY
		BTW VISIBLE "max_length:: " AN max_length
		I HAS A j ITZ start
		I HAS A string ITZ A YARN
		I HAS A char
		IM IN YR loop UPPIN YR i TIL BOTH SAEM i AN max_length
			char R I IZ STRING'Z AT YR input AN YR j MKAY
			BTW VISIBLE "char:: " AN char AN " at " AN j
			DIFFRINT char AN "!", O RLY?
			YA RLY
				string R SMOOSH string AN char MKAY
				j R SUM OF j AN 1
				i R j
			NO WAI
				FOUND YR string
			OIC
		IM OUTTA YR loop
		FOUND YR string
	IF U SAY SO

	HOW IZ I find_text YR input AN YR text AN YR start
		VISIBLE "Searching for:: " AN text
		BTW VISIBLE "Beginning at:: " AN start
		I HAS A max_length ITZ I IZ STRING'Z LEN YR input MKAY
		I HAS A j ITZ start
		I HAS A text_length ITZ I IZ STRING'Z LEN YR text MKAY
		I HAS A text_i ITZ 0
		I HAS A found ITZ FAIL

		IM IN YR loop UPPIN YR i TIL BOTH SAEM i AN max_length
			I HAS A char ITZ I IZ STRING'Z AT YR input AN YR j MKAY
			BTW VISIBLE "char:: " AN char AN " at " AN j
			BOTH SAEM char AN I IZ STRING'Z AT YR text AN YR text_i MKAY, O RLY?
			YA RLY
				BTW textletter found
				text_i R SUM OF text_i AN 1
			NO WAI
				text_i R 0
			OIC
			BOTH SAEM text_i AN text_length, O RLY?
			YA RLY
				VISIBLE "Found your text:: " AN text
				FOUND YR DIFF OF j AN DIFF OF text_length AN 1
			OIC
			j R SUM OF j AN 1
			i R j
		IM OUTTA YR loop
		VISIBLE "Didn't found your text:: " AN text
		FOUND YR "-1"
	IF U SAY SO

	HOW IZ I random_text YR beginning AN YR end AN YR input
		BTW VISIBLE "Beginning at:: " AN beginning
		BTW VISIBLE "End at:: " AN end
		I HAS A text ITZ A YARN
		I HAS A j ITZ SUM OF beginning AN 3
		I HAS A k ITZ DIFF OF end AN SUM OF beginning AN 3
		I HAS A i ITZ A  NUMBR
		IM IN YR loop UPPIN YR i TIL BOTH SAEM i AN k
			I HAS A char ITZ I IZ STRING'Z AT YR input AN YR j MKAY
			BTW VISIBLE "char:: " AN char AN " at " AN j
			text R SMOOSH text AN char MKAY
			j R SUM OF j AN 1
		IM OUTTA YR loop
		FOUND YR text
	IF U SAY SO

	IM IN YR main_loop
		BTW get a input
		I HAS A input
		VISIBLE "Waiting for input."
		BTW GIMMEH input
		input R I IZ SOCKS'Z GET YR local AN YR remote AN YR 16385 MKAY
		VISIBLE "Input is:: " AN input

		BTW command finden
		I HAS A start ITZ 0
		I HAS A untersuchung ITZ WIN
		IM IN YR loop UPPIN YR i WILE ANY OF BOTH SAEM untersuchung AN WIN AN BOTH SAEM i AN SUM OF I IZ STRING'Z LEN YR input MKAY AN 1 MKAY
			I HAS A command ITZ A YARN
			DIFFRINT start AN BIGGR OF start AN I IZ STRING'Z LEN YR input MKAY, O RLY?    BTW start < input
			YA RLY
				command R I IZ parse YR input AN YR start MKAY
				VISIBLE "Command is:: " AN command
			OIC

			command, WTF?
			OMG "PING"
				BTW reply PONG
				i R SUM OF I IZ STRING'Z LEN YR command MKAY AN 1
				I HAS A ping ITZ I IZ parse YR input AN YR SUM OF i AN start MKAY
				VISIBLE "Ping is:: " AN ping
				I HAS A reply ITZ SMOOSH "PONG " AN ping AN ":(0D):(0A)" MKAY
				VISIBLE "Reply is:: " AN reply
				I IZ SOCKS'Z PUT YR local AN YR remote AN YR reply MKAY
				untersuchung R FAIL
				GTFO
			OMG "ERROR"
				VISIBLE "Something bad has happened!"
				untersuchung R FAIL
				GTFO
			OMG "NOTICE"
				VISIBLE "Nothing to do here."
				untersuchung R FAIL
				GTFO
			OMG "JOIN"
				VISIBLE "Nothing to do here."
				untersuchung R FAIL
				GTFO
			OMG "QUIT"
				VISIBLE "Nothing to do here."
				untersuchung R FAIL
				GTFO
			OMG "001"
				VISIBLE "Nothing to do here."
				untersuchung R FAIL
				GTFO
			OMG "PRIVMSG"
				start R SUM OF start AN SUM OF I IZ STRING'Z LEN YR command MKAY AN 1
				I HAS A mention ITZ I IZ parse YR input AN YR start MKAY
				mention, WTF?
				OMG "#holarse-gaming"
					VISIBLE "ChannelMSG"
					BTW search for nick
					start R SUM OF start AN SUM OF I IZ STRING'Z LEN YR channel MKAY AN 1
					I HAS A text_start ITZ start
					I HAS A highlight ITZ I IZ find_text YR input AN YR nick AN YR start MKAY
					highlight, WTF?
					OMG "-1"
						BTW parse text for random
						I HAS A random_start1 ITZ I IZ find_text YR input AN YR "[ ]" AN YR start MKAY
						BTW VISIBLE "Random_start1:: " AN random_start1
						BOTH SAEM random_start1 AN SUM OF start AN 1, O RLY?
						YA RLY
							I HAS A random_start2 ITZ I IZ find_text YR input AN YR "[ ]" AN YR SUM OF start AN 5 MKAY
							DIFFRINT random_start2 AN "-1", O RLY?
							YA RLY
								BTW VISIBLE "Random_start2:: " AN random_start2
								BTW random
								BTW VISIBLE "Searching for text1"
								I HAS A text1 ITZ I IZ random_text YR random_start1 AN YR random_start2 AN YR input MKAY
								BTW VISIBLE "Searching for text2"
								I HAS A text2 ITZ I IZ random_text YR random_start2 AN YR I IZ STRING'Z LEN YR input MKAY AN YR input MKAY

								I HAS A random_number ITZ I IZ STDLIB'Z BLOW YR 2 MKAY
								BTW VISIBLE "random number:: " AN random_number

								I HAS A reply ITZ A YARN
								BOTH SAEM random_number AN 0, O RLY?
								YA RLY
									reply R SMOOSH "☑" AN text1 AN "☐" AN text2 MKAY
								NO WAI
									reply R SMOOSH "☐" AN text1 AN "☑" AN text2 MKAY
								OIC

								reply R SMOOSH "PRIVMSG " AN channel AN " ::" AN reply AN ":(0D):(0A)" MKAY
								VISIBLE "Reply is:: " AN reply
								I IZ SOCKS'Z PUT YR local AN YR remote AN YR reply MKAY
								untersuchung R FAIL
							OIC
						NO WAI
							GTFO
						OIC
						untersuchung R FAIL
						GTFO
					OMGWTF
						VISIBLE "Highlight at:: " AN highlight
						I HAS A source ITZ I IZ parse_nick YR input AN YR 1 MKAY
						I HAS A reply ITZ SMOOSH "PRIVMSG " AN channel AN " ::I ♥ U " AN source AN "!!!1!:(0D):(0A)" MKAY
						VISIBLE "Reply is:: " AN reply
						I IZ SOCKS'Z PUT YR local AN YR remote AN YR reply MKAY
						untersuchung R FAIL
						GTFO
					OIC
					untersuchung R FAIL
					GTFO
				OMG "hugbot"
					VISIBLE "PrivateMSG"
					BTW TODO: search for quit command
					untersuchung R FAIL
					GTFO
				OIC
				untersuchung R FAIL
				GTFO
			OMGWTF
				start R SUM OF start AN 1
				start R SUM OF start AN I IZ STRING'Z LEN YR command MKAY

				BOTH SAEM start AN BIGGR OF start AN I IZ STRING'Z LEN YR input MKAY, O RLY?    BTW start >= input
				YA RLY
					untersuchung R FAIL
				OIC

				GTFO
			OIC
		IM OUTTA YR loop

	IM OUTTA YR main_loop

	BTW close
	I IZ SOCKS'Z CLOSE YR local MKAY
KTHXBYE
