#!/usr/bin/gawk -f
#
# Script to transform ModSecurity alert messages into JSON format.
# Copyright 2017, Christian Folini
#

# [2017-03-29 11:29:28.238891] [-:error] 127.0.0.1:58152 WNt@eH8AAQEAABoE82MAAAAA [client 127.0.0.1] ModSecurity: Warning. Matched phrase "etc/passwd" at ARGS:test. [file "/home/dune73/data/git/crs-official/rules/REQUEST-932-APPLICATION-ATTACK-RCE.conf"] [line "448"] [id "932160"] [rev "1"] [msg "Remote Command Execution: Unix Shell Code Found"] [data "Matched Data: etc/passwd found within ARGS:test: /etc/passwd"] [severity "CRITICAL"] [ver "OWASP_CRS/3.0.0"] [maturity "1"] [accuracy "8"] [tag "application-multi"] [tag "language-shell"] [tag "platform-unix"] [tag "attack-rce"] [tag "OWASP_CRS/WEB_ATTACK/COMMAND_INJECTION"] [tag "WASCTC/WASC-31"] [tag "OWASP_TOP_10/A1"] [tag "PCI/6.5.2"] [hostname "localhost"] [uri "/index.html"] [unique_id "WNt@eH8AAQEAABoE82MAAAAA"]

# [client 127.0.0.1] ModSecurity: Warning. Matched phrase "etc/passwd" at ARGS:test. [file "/home/dune73/data/git/crs-official/rules/REQUEST-932-APPLICATION-ATTACK-RCE.conf"] [line "448"] [id "932160"] [rev "1"] [msg "Remote Command Execution: Unix Shell Code Found"] [data "Matched Data: etc/passwd found within ARGS:test: /etc/passwd"] [severity "CRITICAL"] [ver "OWASP_CRS/3.0.0"] [maturity "1"] [accuracy "8"] [tag "application-multi"] [tag "language-shell"] [tag "platform-unix"] [tag "attack-rce"] [tag "OWASP_CRS/WEB_ATTACK/COMMAND_INJECTION"] [tag "WASCTC/WASC-31"] [tag "OWASP_TOP_10/A1"] [tag "PCI/6.5.2"] [hostname "localhost"] [uri "/index.html"] [unique_id "WNt@eH8AAQEAABoE82MAAAAA"]

# [client 127.0.0.1]
# ModSecurity: Warning.
# Matched phrase "etc/passwd" at ARGS:test.
# [file "/home/dune73/data/git/crs-official/rules/REQUEST-932-APPLICATION-ATTACK-RCE.conf"]
# [line "448"]
# [id "932160"]
# [rev "1"]
# [msg "Remote Command Execution: Unix Shell Code Found"]
# [data "Matched Data: etc/passwd found within ARGS:test: /etc/passwd"]
# [severity "CRITICAL"]
# [ver "OWASP_CRS/3.0.0"]
# [maturity "1"]
# [accuracy "8"]
# [tag "application-multi"]
# [tag "language-shell"]
# [tag "platform-unix"]
# [tag "attack-rce"]
# [tag "OWASP_CRS/WEB_ATTACK/COMMAND_INJECTION"]
# [tag "WASCTC/WASC-31"]
# [tag "OWASP_TOP_10/A1"]
# [tag "PCI/6.5.2"]
# [hostname "localhost"]
# [uri "/index.html"]
# [unique_id "WNt@eH8AAQEAABoE82MAAAAA"]


# {
# 	"modsec-alert": {
# 		"client": "127.0.0.1",
# 		"description": "Warning. Matched phrase \"etc/passwd\" at ARGS:test",
# 		"file": "/home/dune73/data/git/crs-official/rules/REQUEST-932-APPLICATION-ATTACK-RCE.conf",
# 		"line": "448",
# 		"id": "932160",
# 		"rev": "1",
# 		"msg": "Remote Command Execution: Unix Shell Code Found",
# 		"data": "Matched Data: etc/passwd found within ARGS:test: /etc/passwd",
# 		"severity": "CRITICAL",
# 		"ver": "OWASP_CRS/3.0.0",
# 		"maturity": "1",
# 		"accuracy": "8",
# 		"tags": ["application-multi", "language-shell", "platform-unix", "attack-rce", "OWASP_CRS/WEB_ATTACK/COMMAND_INJECTION", "WASCTC/WASC-31", "OWASP_TOP_10/A1", "PCI/6.5.2"],
# 		"hostname": "localhost",
# 		"uri": "/index.html",
# 		"unique_id": "WNt@eH8AAQEAABoE82MAAAAA"
# 	}
# }



# {
# 	"modsec-alert": {
# 		"description": "Warning. Matched phrase \"etc/passwd\" at ARGS:test",
# 		"id": "932160",
# 		"client": "127.0.0.1",
# 		"hostname": "localhost",
# 		"uri": "/index.html",
# 		"unique_id": "WNt@eH8AAQEAABoE82MAAAAA"
# 		"msg": "Remote Command Execution: Unix Shell Code Found",
# 		"data": "Matched Data: etc/passwd found within ARGS:test: /etc/passwd",
# 		"severity": "CRITICAL",
# 		"tags": ["application-multi", "language-shell", "platform-unix", "attack-rce", "OWASP_CRS/WEB_ATTACK/COMMAND_INJECTION", "WASCTC/WASC-31", "OWASP_TOP_10/A1", "PCI/6.5.2"],
# 		"file": "/home/dune73/data/git/crs-official/rules/REQUEST-932-APPLICATION-ATTACK-RCE.conf",
# 		"line": "448",
# 		"rev": "1",
# 		"ver": "OWASP_CRS/3.0.0",
# 		"maturity": "1",
# 		"accuracy": "8",
# 	}
# }

BEGIN {
	verbose = 0
}

{
	n_tags = 0 # num of elements in tags array
	Description = ""
	json_out = ""

	for (i = 1; i <= NF; i++) {
		# read values
		switch (FN) {
		case "client":    Client	= substr($i, 1, length($i) - 1); FN = ""; break
		# description
		case "file":      File 		= substr($i, 2, length($i) - 3); FN = ""; break
		case "line":      Line 		= substr($i, 2, length($i) - 3); FN = ""; break
		case "id":        Id 		= substr($i, 2, length($i) - 3); FN = ""; break
		case "rev":       Rev 		= substr($i, 2, length($i) - 3); FN = ""; break
		case "severity":  Severity 	= substr($i, 2, length($i) - 3); FN = ""; break
		case "ver":       Ver 		= substr($i, 2, length($i) - 3); FN = ""; break
		case "maturity":  Maturity	= substr($i, 2, length($i) - 3); FN = ""; break
		case "accuracy":  Accuracy	= substr($i, 2, length($i) - 3); FN = ""; break
		# tag
		case "hostname":  Hostname	= substr($i, 2, length($i) - 3); FN = ""; break
		case "uri":       Uri	        = substr($i, 2, length($i) - 3); FN = ""; break
		case "unique_id": UniqueID 	= substr($i, 2, length($i) - 3); FN = ""; break
		case "msg":
			if ( substr($i, 1, 1) == "\"" ) {
				Msg = substr($i, 2, length($i) - 1)
				break
			}
			else {
				if ( substr($i, length($i) - 1, 2) == "\"]" ) {
					Msg = Msg " " substr($i, 1, length($i) - 2)
					FN = ""
					break
				}
				else {
					Msg = Msg " " $i
					break
				}
			}
		case "data":
			if ( substr($i, 1, 1) == "\"" ) {
				Data = substr($i, 2, length($i) - 1)
				if ( substr($i, length($i) - 1, 2) == "\"]" ) {
					# New string, but it is already finished
					FN = ""
				}
				break
			}
			else {
				if ( substr($i, length($i) - 1, 2) == "\"]" ) {
					# data string finished
					Data = Data " " substr($i, 1, length($i) - 2)
					FN = ""
					break
				}
				else {
					# data string that will continue
					Data = Data " " $i
					break
				}
			}
		case "tag":
			if ( substr($i , length($i) -1, 2) == "\"]" ) {
				n_tags = n_tags + 1
				if ( substr($i, 1, 1) == "\"") {
					Tags[n_tags] = substr($i, 2, length($i) - 3)
				}
				else {
					Tags[n_tags] = Tags[n_tags] " " substr($i, 1, length($i) -2)
				}
				FN= ""
				break
			}
			else {
				if ( substr($i, 1, 1) == "\"") {
					Tags[n_tags+1] = substr($i, 2, length($i))
				} else {
					Tags[n_tags+1] = $i
				}
			}
		default:
			# examine keys
			if ( i <= 2 ) {
				# Timestamp
				# FIXME: Make other timestamp formats also accessible
				if ( i == 1 ) {
					Timestamp = substr($i, 2, length($i) - 1)
				}
				else {
					Timestamp = Timestamp " " substr($i, 1, length($i) - 1)
				}
			}
			else {
				switch ($i) {
				case "[client":		FN = "client"; break
				case "[file": 		FN = "file"; break
				case "[line": 		FN = "line"; break
				case "[id": 		FN = "id"; break
				case "[rev": 		FN = "rev"; break
				case "[msg": 		FN = "msg"; break
				case "[data": 		FN = "data"; break
				case "[severity":	FN = "severity"; break
				case "[ver": 		FN = "ver"; break
				case "[maturity":	FN = "maturity"; break
				case "[accuracy":	FN = "accuracy"; break
				case "[tag": 		FN = "tag"; break
				case "[hostname":	FN = "hostname"; break
				case "[uri": 		FN = "uri"; break
				case "[unique_id": 	FN = "unique_id"; break
				}
				if ( FN == "" ) {
					# Description: Everything outside of square bracket pairs. FIXME: not done yet
					if ( Description == "" && $i == "ModSecurity:" ) {
						Description = $i
					}
					else {
						if ( length(Description) != 0 ) {
							Description = Description " " $i
						}
					}
				}
			}
		}
	}

	# Escaping
	gsub(/"/, "\\\"", Description)
	gsub(/"/, "\\\"", Msg)
	gsub(/"/, "\\\"", Data)

	if ( verbose ) {
		print "Timestamp", Timestamp
		print "Description", Description
		print "Client", Client
		print "File", File
		print "Line", Line
		print "Id", Id
		print "Rev", Rev
		print "Msg", Msg
		print "Data", Data
		print "Severity", Severity
		print "Tags", get_tags(Tags)
		print "Ver", Ver
		print "Maturity", Maturity
		print "Accuracy", Accuracy
		print "Hostname", Hostname
		print "Uri", Uri
		print "UniqueID", UniqueID
	}

	if ( Id != "" ) {
		json_out = json_out "{ "
		json_out = json_out "\"modsec-alert\": {"
		json_out = json_out "\"description\": \"" Description "\","
		json_out = json_out "\"id\": " Id ","
		json_out = json_out "\"client\": \"" Client "\","
		json_out = json_out "\"hostname\": \"" Hostname "\","
		json_out = json_out "\"uri\": \"" Uri "\","
		json_out = json_out "\"uniqueID\": \"" UniqueID "\","
		json_out = json_out "\"msg\": \"" Msg "\","
		json_out = json_out "\"data\": \"" Data "\","
		json_out = json_out "\"severity\": \"" Severity "\","
		json_out = json_out "\"tags\": " get_tags(Tags) ","
		json_out = json_out "\"file\": \"" File "\","
		json_out = json_out "\"line\": " Line ","
		json_out = json_out "\"rev\": \"" Rev "\","
		json_out = json_out "\"ver\": \"" Ver "\","
		json_out = json_out "\"maturity\": \"" Maturity "\","
		json_out = json_out "\"rule_template\": \"# ModSec Rule Exclusion: " Id " : " Msg " (severity: " Maturity " " Severity ")\""
		json_out = json_out "} }"

		n_tags = 0
		Description = ""
		Id = ""
		Client = ""
		Hostname = ""
		Uri = ""
		UniqueID = ""
		Msg = ""
		Data = ""
		Severity = ""
		File = ""
		Line = ""
		Rev = ""
		Ver = ""
		Maturity = ""

		print json_out

	}
}

END {
}

function get_tags (Tags) {
	mytags = ""
	for (i in Tags) {
		if ( length(mytags) > 0 ) {
			mytags = mytags ", "
		}
			mytags = mytags "\"" Tags[i] "\""
	}
	return "[" mytags "]"
}
