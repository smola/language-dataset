#pragma ModuleName=MinTest

/////////////////////////////////////////////////////////////////////////////////
// Public Functions /////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

// (1) check whether values of two variables (real numbers) are equal,
//     where NaN is equal to NaN
Function eq_var(got, want)
	Variable got, want

	if( eq_as_number(got, want) )
		return pass()
	else
		print_callers_information()
		print "\tgot :", got
		print "\twant:", want
		return fail()
	endif
End

// (2) check whether values of two strings are equal
Function eq_str(got, want)
	String got, want	

	if( eq_as_string(got, want) )
		return pass()
	else
		print_callers_information()
		print "\tgot :", got
		print "\twant:", want
		return fail()
	endif
End

// (3) check whether values of two numerical waves are equal, 
//     where invalid wave reference is equal to a zero wave {}
Function eq_wave(got, want)
	WAVE got, want

	if( eq_as_numerical_wave(got, want) )
		return pass()
	else
		print_callers_information()
		print "\tgot :", got
		print "\twant:", want
		return fail()
	endif
End

// (4) check whether values of two text waves are equal, 
//     where invalid wave reference is equal to a zero wave {}
Function eq_text(got, want)
	WAVE/T got, want

	if( eq_as_text_wave(got, want) )
		return pass()
	else
		print_callers_information()
		print "\tgot :", got
		print "\twant:", want
		return fail()
	endif
End

////////////////////////////////////////////////////////////////////////////////
// Functions for equlity testing ///////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

static Function eq_as_number(v1, v2)
	Variable v1, v2
	return (NumType(v1) == 2 && NumType(v2) == 2) || v1 == v2
End

static Function eq_as_string(s1,s2)
	String s1, s2
	return !cmpstr(s1,s2)
End

static Function eq_as_numerical_wave(w1, w2)
	WAVE w1, w2
	if( numpnts(w1) == 0 && numpnts(w2) == 0 )
		return 1
	elseif( eq_wave_size(w1,w2) )
		Make/FREE/N=(DimSize(w1, 0), DimSize(w1, 1), DimSize(w1, 2), DimSize(w1, 3)) bool
		bool = eq_as_number(w1,w2)
		return WaveMin(bool)
	else
		return 0
	endif
End

static Function eq_as_text_wave(w1, w2)
	WAVE/T w1, w2
	if( numpnts(w1) == 0 && numpnts(w2) == 0 )
		return 1
	elseif( eq_wave_size(w1,w2) )
		Make/FREE/N=(DimSize(w1, 0), DimSize(w1, 1), DimSize(w1, 2), DimSize(w1, 3)) bool
		bool = eq_as_string(w1,w2)
		return WaveMin(bool)
	else
		return 0
	endif
End

static Function eq_wave_size(w1, w2)
	WAVE w1, w2
	Variable bool = 1
	bool = bool && DimSize(w1, 0) == DimSize(w2, 0)
	bool = bool && DimSize(w1, 1) == DimSize(w2, 1)
	bool = bool && DimSize(w1, 2) == DimSize(w2, 2)
	bool = bool && DimSize(w1, 3) == DimSize(w2, 3)
	return bool
End

////////////////////////////////////////////////////////////////////////////////
// Functions for logging ///////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

static Function pass()
	NewDataFolder/O root:Packages
	NewDataFolder/O root:Packages:MinTest
	WAVE/T pass = root:Packages:MinTest:W_pass
	WAVE/T fail = root:Packages:MinTest:W_fail
	if(!WaveExists(pass))
		Make/T/N=0 $"root:Packages:MinTest:W_pass"/WAVE=pass
	endif
	if(!WaveExists(fail))
		Make/T/N=0 $"root:Packages:MinTest:W_fail"/WAVE=fail
	endif
	
	String info = callers_information()
	Extract/O/T fail, fail, cmpstr(fail, info)
	Extract/O/T pass, pass, cmpstr(pass, info)

	InsertPoints DimSize(pass,0), 1, pass
	pass[inf] = info
	return 1
End
static Function fail()
	NewDataFolder/O root:Packages
	NewDataFolder/O root:Packages:MinTest
	WAVE/T fail = root:Packages:MinTest:W_fail
	if(!WaveExists(fail))
		Make/T/N=0 $"root:Packages:MinTest:W_fail"/WAVE=fail
	endif

	String info = callers_information()
	Extract/O/T fail, fail, cmpstr(fail, info)

	InsertPoints DimSize(fail,0), 1, fail
	fail[inf] = info
	return 0
End

static Function/S callers_information()
	String stacks = GetRTStackInfo(3)
	String info = StringFromList(ItemsInList(stacks)-4, stacks)
	String win = StringFromList(1, info, ",")
	Variable line = Str2Num(StringFromList(2, info, ","))
	String text = StringFromList(line, ProcedureText("", -1, win), "\r")
	SplitString/E = "^[\\s\\t]*(.*)$" text, text
	return info + ": " + text
End

static Function print_callers_information()
	print "---", callers_information()
End

static Function/S status()
	WAVE/T pass = root:Packages:MinTest:W_pass
	WAVE/T fail = root:Packages:MinTest:W_fail
	Variable num_pass = WaveExists(pass) ? DimSize(pass,0) : 0
	Variable num_fail = WaveExists(fail) ? DimSize(fail,0) : 0
	Variable num_total = num_pass + num_fail
	if(num_total == 0)
		return ""
	elseif(num_fail)
		return "NG! (pass " + Num2Str(num_pass) + "/" + Num2Str(num_total) + ")"
	elseif(num_total)
		return "OK! (pass " + Num2Str(num_pass) + "/" + Num2Str(num_total) + ")"
	endif	
End

static Function start()
	if(WaveExists(root:Packages:MinTest:W_pass))
		KillWaves/Z root:Packages:MinTest:W_pass
	endif
	if(WaveExists(root:Packages:MinTest:W_fail))
		KillWaves/Z root:Packages:MinTest:W_fail
	endif
End

static Function finish()
	BuildMenu "All"
	String s = status()
	print SelectString(strlen(s), "NO TEST", s)
End


/////////////////////////////////////////////////////////////////////////////////
// Menu Functions ///////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

strconstant MinTest_Menu="Test"
Menu MinTest#MenuTitle(), dynamic
	
	MinTest#MenuTitleJump()
	MinTest#MenuItemJump(0),/Q,MinTest#MenuCommandJump(0)
	MinTest#MenuItemJump(1),/Q,MinTest#MenuCommandJump(1)
	MinTest#MenuItemJump(2),/Q,MinTest#MenuCommandJump(2)
	MinTest#MenuItemJump(3),/Q,MinTest#MenuCommandJump(3)
	MinTest#MenuItemJump(4),/Q,MinTest#MenuCommandJump(4)
	
	MinTest#MenuItemExecuteAll(),/Q,MinTest#MenuCommandExecuteAll()
	
	"-"
	"(Test"
	FunctionList("Test*",";","NPARAMS:0"),/Q,MinTest#MenuCommandExecute()
End

static Function/S MenuTitle()
	String s = status()
	return SelectString(strlen(s),"Test","Test:"+s)
End

static Function/S MenuTitleJump()
	WAVE/T w = root:Packages:MinTest:W_fail
	return SelectString(WaveExists(w) && DimSize(w,0),"","(Jump")
End

static Function/S MenuItemJump(i)
	Variable i
	WAVE/T w = root:Packages:MinTest:W_fail
	if(WaveExists(w) && i<DimSize(w,0))
		return "\M0"+w[i]
	endif
End

static Function MenuCommandJump(i)
	Variable i
	WAVE/T w = root:Packages:MinTest:W_fail
	String win,line
	SplitString/E = "^[^,]+,([^,]+),([0-9]+):" w,win,line
	DisplayProcedure/W=$win/L=(Str2Num(line))
End

static Function/S MenuItemExecuteAll()
	WAVE/T w = root:Packages:MinTest:W_fail
	return SelectString(WaveExists(w) && DimSize(w,0),"","-;")+"Execute All Tests"
End

static Function MenuCommandExecuteAll()
	start()
	
	String tests = FunctionList("Test*",";","NPARAMS:0")
	Variable i, N = ItemsInList(tests)
	for(i = 0; i < N; i += 1)
		printf num2char(cmpstr(IgorInfo(2),"Macintosh") ? 42 : -91)
		print StringFromList(i, tests) + "()"
		Execute/Z StringFromList(i, tests) + "()"
	endfor
	
	finish()
End

static Function MenuCommandExecute()
	start()
	
	GetLastUserMenuInfo
	printf num2char(cmpstr(IgorInfo(2),"Macintosh") ? 42 : -91)
	print S_Value + "()"
	Execute S_Value + "()"
	
	finish()
End 