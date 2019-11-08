Strict

Import tricky_units.Dirry
Import tricky_units.initfile2

MKL_Version "","" 
MKL_Lic         "",""



Global appSup$ = Dirry("$AppSupport$")
Global mydir$ = appsup + "/Phantasar Productions/"+StripAll(AppFile)
Global langdir$ = mydir + "/Langs"
CreateDir langdir,1

Global mainconfigfile$ = mydir+"/Config.ini"
Global mainconfig:TIni = New TIni
If Not FileType(mainconfigfile) SaveString("[rem]~nYeah yeah",mainconfigfile)
LoadIni mainconfigfile,mainconfig

Global lang$
Global langdata:TIni

Function LNG$(t$)
	Local ret$ = langdata.c(t)
	If Not ret ret = "Unknown tag ~q"+t+"~q"
	Return ret
End Function


Global configdir$ = mydir+"/Config"
CreateDir configdir,1
Global configfile$
Global config:TIni

Global NaamLeerling$

Type tleerling
	Field tasks
	Field Time
	Field correct
	Field wrong
	
	Method stime$()
		Local seconds,minutes,hours
		seconds = Time
		While seconds>=60
			minutes = minutes + 1
			seconds = seconds - 60
		Wend
		While minutes>=60
			hours:+1
			minutes:-60
		Wend
		Local ret$
		If hours ret = hours+":"
		ret:+Right("0"+minutes,2)+":"+Right("0"+seconds,2)
		Return ret
	End Method
End Type

Global Leerlingen:Tmap = New tmap
