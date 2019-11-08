PROGRAM_NAME='Radio'

DEFINE_TYPE

    structure Radio
    {
		char Name[100]
		char image[500]
		char URL[500]
		char Type[20]
		char subtext[200]
	
    }

define_variable
Radio RadioList[200]
char arURL[10][200]
integer aList
integer cur10r
integer curPocetR


DEFINE_FUNCTION put2PLr(integer num)
{
	stack_var integer curnum
	
	if(PLtotal< MAX_PL_LENGTH)
	{
		curnum = 10*cur10r+num
		if(RadioList[curnum].type='audio')
		{
			PLtotal++
			Playlist[PLtotal].nam = RadioList[curnum].Name
			Playlist[PLtotal].path = RadioList[curnum].URL
			Playlist[PLtotal].metadata = 'iradio'
			Playlist[PLtotal].image =  RadioList[curnum].image
			
			ShowPL(PLtotal)
			PLcur10=(PLtotal-1)/10
		}
		
	}
}




DEFINE_FUNCTION ShowBRr()
{
	stack_var 
	long i
	long j
	
	RESET_Browse()
	For(i=10*cur10r+1; i<= 10*cur10r+10; i++)
	{
		j++
		sendUNItext2arr(vTP ,j, RadioList[i].Name)
		SEND_LEVEL vTP, 10+j, 4
		if(i=curPocetr) break;	
	}
	
	
	SEND_COMMAND vTP,"'^TXT-101,0,',itoa(cur10r+1),'/',itoa(((curPocetr-1)/10)+1)"

}




DEFINE_FUNCTION GetRadio()
{
aList=0
 RESET_Browse()
    http_get("'http://opml.radiotime.com'")
}


DEFINE_FUNCTION GetRadioCon(char URL[])
{
 RESET_Browse()
    http_get(URL)
}



DEFINE_FUNCTION PlayRadioFromPlayList(Integer num)
{
stack_var char URL[600]
URL=string_replace(Playlist[num].path,'&','&amp;')

S="'<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><s:Body><u:SetAVTransportURI xmlns:u="urn:schemas-upnp-org:service:AVTransport:1"><InstanceID>0</InstanceID><CurrentURI>',URL,'</CurrentURI><CurrentURIMetaData>&lt;?xml version=&quot;1.0&quot; encoding=&quot;utf-8&quot;?&gt;&lt;DIDL-Lite xmlns=&quot;urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/&quot; xmlns:dc=&quot;http://purl.org/dc/elements/1.1/&quot; xmlns:upnp=&quot;urn:schemas-upnp-org:metadata-1-0/upnp/&quot; xmlns:dlna=&quot;urn:schemas-dlna-org:metadata-1-0/&quot;&gt;&lt;item id=&quot;1&quot; parentID=&quot;1&quot; restricted=&quot;1&quot;&gt;&lt;dc:title&gt;',Playlist[num].nam,'&lt;/dc:title&gt;&lt;upnp:class&gt;object.item.audioItem.musicTrack&lt;/upnp:class&gt;&lt;res;&gt;',URL,'&lt;/res&gt;&lt;/item&gt;&lt;/DIDL-Lite&gt;</CurrentURIMetaData></u:SetAVTransportURI></s:Body></s:Envelope>'"

	http_post_SOAP_ACTION("'http://',Devicer[ActualSelectPlayer].IP,':',itoa(Devicer[ActualSelectPlayer].port),Devicer[ActualSelectPlayer].controlURL",s,'SetAVTransportURI')
	
	SEND_COMMAND vTP,"'^RMF-NowPlaying,',Playlist[num].image"
	wait 3 SetPlay()
}

DEFINE_FUNCTION PlayRadio(Integer num)
{
stack_var char URL[600]
URL=string_replace(RadioList[num].URL,'&','&amp;')

S="'<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><s:Body><u:SetAVTransportURI xmlns:u="urn:schemas-upnp-org:service:AVTransport:1"><InstanceID>0</InstanceID><CurrentURI>',URL,'</CurrentURI><CurrentURIMetaData>&lt;?xml version=&quot;1.0&quot; encoding=&quot;utf-8&quot;?&gt;&lt;DIDL-Lite xmlns=&quot;urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/&quot; xmlns:dc=&quot;http://purl.org/dc/elements/1.1/&quot; xmlns:upnp=&quot;urn:schemas-upnp-org:metadata-1-0/upnp/&quot; xmlns:dlna=&quot;urn:schemas-dlna-org:metadata-1-0/&quot;&gt;&lt;item id=&quot;1&quot; parentID=&quot;1&quot; restricted=&quot;1&quot;&gt;&lt;dc:title&gt;',RadioList[num].Name,'&lt;/dc:title&gt;&lt;upnp:class&gt;object.item.audioItem.musicTrack&lt;/upnp:class&gt;&lt;res;&gt;',URL,'&lt;/res&gt;&lt;/item&gt;&lt;/DIDL-Lite&gt;</CurrentURIMetaData></u:SetAVTransportURI></s:Body></s:Envelope>'"

	http_post_SOAP_ACTION("'http://',Devicer[ActualSelectPlayer].IP,':',itoa(Devicer[ActualSelectPlayer].port),Devicer[ActualSelectPlayer].controlURL",s,'SetAVTransportURI')
	
	SEND_COMMAND vTP,"'^RMF-NowPlaying,',RadioList[num].image"
	wait 3 SetPlay()
}

DEFINE_FUNCTION getBrowseRadio(integer num)
{
if(RadioList[10*cur10r+num].Type='link')
{
aList++
arURL[aList]=RadioList[10*cur10r+num].URL
GetRadioCon(RadioList[10*cur10r+num].URL)
}
else
{
PlayRadio(10*cur10r+num)
}
}

DEFINE_FUNCTION backBrowseRadio()
{
cur10r=0
    if(aList>1){
	aList--
	GetRadioCon(arURL[aList])
    }
    else
    {
    
	aList=0
	GetRadio()
    }
}

DEFINE_FUNCTION ShowBRupR()
{
	if(cur10r > 0){cur10r--  ShowBRr() }
}

DEFINE_FUNCTION ShowBRdwR()
{
	if(curPocetR >(10*cur10r+10)){cur10r++  ShowBRr() }
}




DEFINE_FUNCTION http_response_received_radio(http_response resp)
{
local_var integer en
local_var integer i
local_var integer pos

en = 1
i=1			
pos = 1	   
cur10r = 0

 While(pos > 0)
	    {
	    

				    pos = find_string(resp.body, "'type="'", en)
				    en  = find_string(resp.body, '"', pos+6)
				    if(pos>0){
				    RadioList[i].Type=mid_string(resp.body,pos+6,en-pos-6)
				  
				    pos = find_string(resp.body, "'text="'", en)
				    en  = find_string(resp.body, '"', pos+6)
				    RadioList[i].Name=string_replace(mid_string(resp.body,pos+6,en-pos-6),'&amp;','&')
				    
				    pos = find_string(resp.body, "'URL="'", en)
				    en  = find_string(resp.body, '"', pos+5)
				    RadioList[i].URL=string_replace(mid_string(resp.body,pos+5,en-pos-5),'&amp;','&')
				    
				    if( RadioList[i].Type="'audio'"){
				    
					pos = find_string(resp.body, "'subtext="'", en)
					en  = find_string(resp.body, '"', pos+9)
					RadioList[i].subtext=mid_string(resp.body,pos+9,en-pos-9)
					
					pos = find_string(resp.body, "'image="'", en)
					en  = find_string(resp.body, '"', pos+7)
					RadioList[i].image=SetCoverArtPath(mid_string(resp.body,pos+7,en-pos-7))

				    }
				    
				    
				    if(i<11) 
				    {
				    sendUNItext2arr(vTP,i,RadioList[i].Name)
				    SEND_LEVEL vTP, 10+i, 4
				    }
				    
				    curPocetR = i
				    i++
				    SEND_COMMAND vTP,"'^TXT-101,0,',itoa(cur10r+1),'/',itoa(((curPocetr-1)/10)+1)"
				    }
			    
			    if(i>200) break
	    
	    }
	

}