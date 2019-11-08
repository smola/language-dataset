// Licence: GNU General Public License version 2 (GPLv2)
#pragma rtGlobals=3		// Use modern global access method and strict wave access.
  

function /S tools_addtowavename(sn, sna)
	string sn, sna
	string srn=""
	if(strsearch(sn[strlen(sn)-1,strlen(sn)],"'",0)==0)
		srn= sn[0,strlen(sn)-2]+sna+"'"
	else
		srn=sn+sna
	endif
	return srn
end


function /S tools_mycleanupstr(name)
	string name
	if(cmpstr(name[strlen(name)-1],"\r")==0)
		name = name[0,strlen(name)-2]
	endif
	return name
end


Function tools_round_dec(val,dec) 
	variable val, dec 
	dec=round(dec) 
	return round(10^dec*val)/10^dec 
End 


function /S tools_getwavepathfromgraph(specwave)
	Wave specwave
	string path = ""
	if(waveexists(specwave))
		path = GetWavesDataFolder(specwave,2)
	endif
	return path
end

