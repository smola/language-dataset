define program
	[repeat config]
	| [makefile]
end define

define config
	[protocol] ': [list packet_type] '> [endian] '< [skip_bits] [opt debug_flag] ';
end define

define protocol
	[id]
end define

define packet_type
	[id]
end define

define endian
	'B
	| 'L
end define

define skip_bits
	[number]
end define

define debug_flag
	'-DEBUG
end define

%% Makefile

define makefile
	[CXX] [NL]
	[CFLAG] [NL]
	[LDLIBS] [NL]
	[OBJS] [NL] [NL]
	[repeat target]
end define

define CXX
	'CXX '= [id]
end define

define CFLAG
	'CFLAGS '= [repeat id]
end define

define LDLIBS
	'LDLIBS '= [repeat id]
end define

define OBJS
	'OBJS '= [repeat id]
end define

define target
	[id] ': [SP] [opt id] [NL]
		[opt command] [NL] [NL]
end define

define command
	[repeat id]
end define

function main
	replace [program]
		P [program]

	construct packetTypes [repeat id]
		_
	export packetTypes
		packetTypes

	deconstruct P
		Configs [repeat config]

	by
		P [generateTypes each Configs]
			[makeMake]
end function

function generateTypes Config [config]
	replace [program]
		P [program]

	deconstruct [config] Config
		Proto [protocol] ': PT [list packet_type] '> E [endian] '< SB [skip_bits] D [opt debug_flag]';
	deconstruct [protocol] Proto
		Type [id]
	import packetTypes [repeat id]
	export packetTypes
		packetTypes [. Type]

	by
		P
end function

function makeMake
	replace [program]
		P [program]

	import packetTypes [repeat id]
	
	construct CXXP [CXX]
		'CXX '= g++
	
	construct CFLAGSP [CFLAG]
		'CFLAGS '= -Wall -std=c++11 -Wno-unused-variable
	
	construct LDLIBSP [LDLIBS]
		'LDLIBS '= -ltins -lboost_regex
	
	construct PredefinedObjs [repeat id]
		src/Main.o src/Identifier.o src/StringMatchingIdentifier.o src/SpidIdentifier.o src/SpidInterface.o src/Fingerprint.o src/AttributeFingerprintHandler.o src/ProtocolModel.o src/Parser.o src/packet.o src/putilities.o
	construct ProtocolObjs [repeat id]
		_ [generateObjs each packetTypes]
	construct finalObjs [repeat id]
		PredefinedObjs [. ProtocolObjs]
	construct OBJSP [OBJS]
		'OBJS '= finalObjs

	construct All [repeat target]
		'all ': TCPStream

	construct tabC [id]
		_ [+ "	"]
	construct tabS [repeat id]
		tabC

	construct rmLine [repeat id]
		rm -f **/*.o ./TCPStream
	construct rmCmd [repeat id]
		tabS [. rmLine]
	construct Clean [target]
		'clean ':
			rmCmd

	construct ProgramLine [repeat id]
		$(CXX) $(CFLAGS) -o $@ $(OBJS) $(LDLIBS)
	construct ProgramCmd [repeat id]
		tabS [. ProgramLine]
	construct ProgramTarget [target]
		TCPStream: $(OBJS)
			ProgramCmd

	construct cppLine [repeat id]
		$(CXX) $(CFLAGS) -o $*.o -c $*.cpp
	construct cppCmd [repeat id]
		tabS [. cppLine]
	construct cppTargets [target]
		.cpp.o:
			cppCmd

	construct Targets [repeat target]
		All [. Clean] [. ProgramTarget] [. cppTargets]

	by
		CXXP
		CFLAGSP
		LDLIBSP
		OBJSP
		Targets
end function

function generateObjs packetType [id]
	replace [repeat id]
		Stmts [repeat id]
	construct newObj [id]
		_ [+ "src/"] [+ packetType] [+ "_Generated.o"]
	by
		Stmts [. newObj]
end function