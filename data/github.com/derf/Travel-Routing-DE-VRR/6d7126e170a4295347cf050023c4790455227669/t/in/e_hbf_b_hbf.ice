<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"><html xmlns:vrr="http://www.vrr.de/" xmlns:mdv="http://www.mentzdv.de/">
<head>
<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>Fahrplanauskunft</title>
<link rel="stylesheet" type="text/css" href="css/map.css">
<link rel="stylesheet" type="text/css" href="css/MDVMapContextMenu.css">
<link rel="stylesheet" type="text/css" href="css/MDVMapControl.css">
<link rel="stylesheet" type="text/css" href="css/calendar.css">
<link rel="stylesheet" type="text/css" href="css/itpTravelService.css">
<link rel="stylesheet" type="text/css" href="css/vrr.css"><script type="text/javascript"><!--
			
			function attachEventListener(target, eventType, functionRef, capture) {
			  if (typeof target.addEventListener != "undefined") {
				target.addEventListener(eventType, functionRef, capture);
			  }
			  else if (typeof target.attachEvent != "undefined") {
				target.attachEvent("on" + eventType, functionRef);
			  }
			  else {
				eventType = "on" + eventType;
			
				if (typeof target[eventType] == "function") {
				  var oldListener = target[eventType];
			
				  target[eventType] = function() {
					oldListener();
			
					return  functionRef();
				  }
				}
				else {
				  target[eventType] = functionRef;
				}
			  }
			
			  return true; 
			}
					
			function checkPwdBoxes()
			{
				if(document.forms[0].ppPersonNetPassword.value!=document.forms[0].ppPersonConfirmNetPassword.value || document.forms[0].ppPersonNetPassword.value=='')
				{
					alert('Ihre Passwortangaben sind ungültig. Bitte geben Sie zweimal dasselbe Passwort ein.');
					document.forms[0].ppPersonNetPassword.value="";
					document.forms[0].ppPersonConfirmNetPassword.value="";
					return false;
				}
				else return true;
			}
			
			function concatDate()
			{
				var completDate;
				completDate = document.forms[0].itdLPxx_date.value;
				completDate = completDate + "-" + document.forms[0].itdLPxx_month.value;
				completDate = completDate + "-" + document.forms[0].itdLPxx_year.value;
				
				document.forms[0].filterDateValid.value=completDate;
			}
			
			var viaInMobilityRestrictionOptions = 'true';
			var viaFlag = 'false';
			var identifiedOdv = {
				'origin' : '::',
				'destination' : '::',
				'via' : '::',
				'command' : ''
			};
			var sid = 'efa2.vrr.de_65057343';
			var curLanguage = 'de';
			var dmStop = "Haltestelle";
			var dmPOI = "wichtiger Punkt";
			
			var includedMeans = [];
			
			var excludedMeans = [];
			
			
			function checkMeans() {

				var f = document.getElementsByName('jp')[0]; 
				
				if (!f || sid!=='0') {
					return true;
				}
				
				if (includedMeans.length >=1) {
					for (var i=0; i < includedMeans.length; i++) {
						if(f['inclMOT_' + includedMeans[i]]) {
							f['inclMOT_' + includedMeans[i]].checked = false;
						}
					}
				} else if (excludedMeans.length >=1) {
					for (var j=0; j < excludedMeans.length; j++) {
						if (f['inclMOT_' + excludedMeans[j]]) {
							f['inclMOT_' + excludedMeans[j]].checked = false;
						}
					}
				}
			}	
			
			attachEventListener(window, 'load', checkMeans, false);
				//--></script><script type="text/javascript" src="JavaScriptFunctions/mdv.js"></script><script type="text/javascript" src="JavaScriptFunctions/odvScript.js"></script><script type="text/javascript" src="/mdv/mdvMap/mdvMap.js"></script><script type="text/javascript" src="/mdv/mdvMap/prototype-1.4.0.js"></script><script type="text/javascript" src="mdvMapSettings.js"></script><script type="text/javascript" src="miniMapSettings.js"></script><script type="text/javascript" src="JavaScriptFunctions/mdvMapHandler.js"></script><script type="text/javascript"><!--
					var bCreateMapRequest = false;
					--></script><script type="text/javascript"><!--
						/* language of the map */
						var mapLanguage = 'de';
						var odv = new Array();
					--></script><script>function initMdvMapConfig() {var usages = new Array("origin", "destination", "via", "dm", "stt");for(var i=0; i<usages.length; i++) {var usage = usages[i];mdvMapConfigs[usage] = mdvMapConfig.clone();mdvMapConfigs[usage].add("serverURL", "http://efa.vrr.de/maps/nrw/");mdvMapConfigs[usage].add("efaURL", "../lite/XSLT_COORD_REQUEST");mdvMapConfigs[usage].add("transparentImg", "images/transparent.gif");mdvMapConfigs[usage].add("defaultScale", "2");mdvMapConfigs[usage].add("toolTimeOut", "20");mdvMapConfigs[usage].add("zoomOnDoubleClick", "true");mdvMapConfigs[usage].add("useMagnifyGlass", "true");mdvMapConfigs[usage].add("cursorMove", "images/grabbing.cur");mdvMapConfigs[usage].add("poiArea", "true");mdvMapConfigs[usage].add("poiPoint", "true");mdvMapConfigs[usage].add("language", "de");mdvMapConfigs[usage].add("imagePath", "images/mdvMap/");mdvMapConfigs[usage].add("xCenterReal", "3368363");mdvMapConfigs[usage].add("yCenterReal", "450530");mdvMapConfigs[usage].add("mapName", "NAV3");mdvMapConfigs[usage].add("network", "NRW");mdvMapConfigs[usage].add("useBubbleForEFAInfo", "true");mdvMapConfigs[usage].add("info.stop.size.width", "250");mdvMapConfigs[usage].add("info.stop.size.height", "200");mdvMapConfigs[usage].add("info.poi.size.width", "250");mdvMapConfigs[usage].add("info.poi.size.height", "120");mdvMapConfigs[usage].add("useBubbleOverflowForEFAInfo", "true");mdvMapConfigs[usage].add("block", "100");mdvMapConfigs[usage].add("language", "de");mdvMiniMapConfigs[usage] = mdvMiniMapConfig.clone();mdvMiniMapConfigs[usage].add("serverURL", "http://efa.vrr.de/maps/nrw/");mdvMiniMapConfigs[usage].add("efaURL", "../lite/XSLT_COORD_REQUEST");mdvMiniMapConfigs[usage].add("transparentImg", "images/transparent.gif");mdvMiniMapConfigs[usage].add("defaultScale", "2");mdvMiniMapConfigs[usage].add("toolTimeOut", "20");mdvMiniMapConfigs[usage].add("zoomOnDoubleClick", "true");mdvMiniMapConfigs[usage].add("useMagnifyGlass", "true");mdvMiniMapConfigs[usage].add("cursorMove", "images/grabbing.cur");mdvMiniMapConfigs[usage].add("language", "de");mdvMiniMapConfigs[usage].add("imagePath", "images/mdvMap/");mdvMiniMapConfigs[usage].add("xCenterReal", "3368363");mdvMiniMapConfigs[usage].add("yCenterReal", "450530");mdvMiniMapConfigs[usage].add("mapName", "NAV3");mdvMiniMapConfigs[usage].add("network", "NRW");mdvMiniMapConfigs[usage].add("block", "100");mdvMiniMapConfigs[usage].add("language", "de");}}
	</script></head>
<body onload="">
<form action="XSLT_TRIP_REQUEST2" id="jp" name="jp" method="POST"><input type="hidden" name="sessionID" id="sessionID" value="efa2.vrr.de_65057343"><input type="hidden" name="language" value="de"><input type="hidden" name="requestID" id="requestID" value="1"><input type="hidden" name="command" id="command" value=""><input type="hidden" name="itdLPxx_ShowFare" value=" "><input type="hidden" name="itdLPxx_view" value=""><input type="hidden" name="useRealtime" value="1"><input type="hidden" name="itdLPxx_enableMobilityRestrictionOptionsWithButton" value=""><input type="hidden" name="execInst" id="execInst" value=""><input type="hidden" name="itdLPxx_mdvMap2_origin" id="itdLPxx_mdvMap2_origin" value="::"><input type="hidden" name="itdLPxx_mdvMap2_destination" id="itdLPxx_mdvMap2_destination" value="::"><input type="hidden" name="itdLPxx_mdvMap2_via" id="itdLPxx_mdvMap2_via" value="::"><input type="hidden" name="itdLPxx_mapState_origin" id="itdLPxx_mapState_origin" value=""><input type="hidden" name="itdLPxx_mapState_destination" id="itdLPxx_mapState_destination" value=""><input type="hidden" name="itdLPxx_mapState_via" id="itdLPxx_mapState_via" value=""><input type="hidden" name="itdLPxx_mdvMap_origin" id="itdLPxx_mdvMap_origin" value="3361949:456292:NAV3"><input type="hidden" name="itdLPxx_mdvMap_destination" id="itdLPxx_mdvMap_destination" value="::"><input type="hidden" name="itdLPxx_mdvMap_via" id="itdLPxx_mdvMap_via" value="::"><input type="hidden" name="itdLPxx_command" id="itdLPxx_command" value=""><input type="hidden" name="itdLPxx_priceCalculator" id="itdLPxx_priceCalculator" value=""><input type="hidden" name="trip" value=""><input type="hidden" name="partialTrip" value=""><input type="hidden" name="itdLPxx_transpCompany" value="vrr"><table cellspacing="0" cellpadding="0" border="0">
<tr>
<td rowspan="3" valign="top" align="left" width="155">
<table id="tabNav">
<tr>
<td class="leftHeader"></td>
</tr>
<tr>
<td><a title="en" href="XSLT_TRIP_REQUEST2?language=en&amp;itdLPxx_transpCompany=vrr"><img src="images/flag/flag_en.gif" border="0"></a> <a title="fr" href="XSLT_TRIP_REQUEST2?language=fr&amp;itdLPxx_transpCompany=vrr"><img src="images/flag/flag_fr.gif" border="0"></a> <a title="es" href="XSLT_TRIP_REQUEST2?language=es&amp;itdLPxx_transpCompany=vrr"><img src="images/flag/flag_es.gif" border="0"></a> <a title="tr" href="XSLT_TRIP_REQUEST2?language=tr&amp;itdLPxx_transpCompany=vrr"><img src="images/flag/flag_tr.gif" border="0"></a></td>
</tr>
<tr>
<td class="navLabelTextBoldColor"><a href="XSLT_TRIP_REQUEST2?language=de&amp;itdLPxx_transpCompany=vrr&amp;">Fahrplanauskunft</a></td>
</tr>
<tr>
<td><a href="XSLT_STT_REQUEST?language=de&amp;itdLPxx_transpCompany=vrr&amp;">Aushangfahrplan</a></td>
</tr>
<tr>
<td><a href="XSLT_DM_REQUEST?language=de&amp;itdLPxx_transpCompany=vrr&amp;">Abfahrtsmonitor</a></td>
</tr>
<tr>
<td><a href="XSLT_PS_REQUEST2?language=de&amp;itdLPxx_transpCompany=vrr&amp;">Persönlicher Fahrplan</a></td>
</tr>
<tr>
<td><a href="http://www.vrr.de/de/fahrplanauskunft/fahr_und_linienplaene/verbundfahrplan_schnellverkehr/index.html" target="vrr">Verbundfahrplan Schnellverkehr</a></td>
</tr>
<tr>
<td><a href="http://www.vrr.de/de/fahrplanauskunft/fahr_und_linienplaene/linienplan_schnellverkehr/index.html" target="vrr">Linienplan Schnellverkehr</a></td>
</tr>
<tr>
<td><a href="http://www.vrr.de/de/fahrplanauskunft/fahr_und_linienplaene/stadtlinienplaene/index.html" target="vrr">Stadtlinienpläne</a></td>
</tr>
<tr>
<td><a href="./Bedienungshinweise_de.htm" target="vrr">Bedienungshinweise</a></td>
</tr>
<tr>
<td><a href="XSLT_SCREEN_SAVER_REQUEST?language=de&amp;itdLPxx_transpCompany=vrr&amp;">Bildschirmschoner</a></td>
</tr>
<tr>
<td><a href="XSLT_ADDINFO_REQUEST?language=de&amp;itdLPxx_transpCompany=vrr&amp;filterPublicationStatus=current&amp;filterShowPlaceList=1&amp;filterShowLineList=1&amp;filterShowStopList=0&amp;filterShowPlaceList=0&amp;">Aktuelle Hinweise</a></td>
</tr>
<tr>
<td><a href="XSLT_TRIP_REQUEST2?language=de&amp;itdLPxx_transpCompany=vrr&amp;itdLPxx_urlGenerator=true&amp;">Ihr Link zu uns</a></td>
</tr>
<tr>
<td><a href="http://www.vrr.de/de/global/impressum/index.html">Impressum</a></td>
</tr>
<tr>
<td><a href="http://www.vrr.de/">Homepage</a></td>
</tr>
</table>
</td>
<td rowspan="3"> </td>
<td valign="top" width="630" height="55">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<tr>
<td colspan="3" height="35" align="left" class="logoHeader">
<div class="textHeader">Fahrplanauskunft</div>
</td>
</tr>
<tr>
<td colspan="3"><img src="images/dummy.gif" width="100%" height="5"></td>
</tr>
</table>
<table cellspacing="0" cellpadding="0" width="630" border="0">
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<tr>
<td colspan="5"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td width="2%" align="top" class="kaestchen"><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td width="10%"><span class="labelTextBold"> von:</span></td>
<td width="40%"><span class="labelText">Essen / Hauptbahnhof</span></td>
<td width="10%"><span class="labelTextBold">nach:     </span></td>
<td width="38%"><span class="labelText">Berlin / Hauptbahnhof</span></td>
</tr>
<tr>
<td colspan="5"><img src="images/dummy.gif" width="1" height="10" alt="*"></td>
</tr>
<tr>
<td width="2%" align="top"> </td>
<td width="10%"><span class="labelTextBold"> Abfahrt:</span></td>
<td width="40%"><span class="labelText">12:00</span><span class="labelText"> Uhr</span></td>
<td width="10%"><span class="labelTextBold">Datum:</span></td>
<td width="38%"><span class="labelText">07.06.2010</span></td>
</tr>
</table>
</td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<tr>
<td width="2%" class="kaestchen"><img src="images/dummy.gif" width="7" height="13" border="0" alt="*"></td>
<td><span class="labelTextBold"> Fahrtdaten</span></td>
<td align="center"><span class="labelTextBold"> Fahrtdauer</span></td>
<td align="center"><span class="labelTextBold"> Umsteigen</span></td>
<td align="center"><span class="labelTextBold"> Preis (Erw./Ki.)</span></td>
</tr>
<tr class="bgColor">
<td colspan="5"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td width="2%"><img src="images/dummy.gif" width="5" height="5" alt="*"></td>
<td><span class="labelText"> <a href="#Trip1"><u>1. Fahrt:</u></a>
									 am 
									<span class="7**
		">07.06.2010</span>
									 von
									 11:23 bis 15:08 Uhr</span></td>
<td align="center"><span class="labelText">03:45</span></td>
<td align="center"><span class="labelText">0</span></td>
<td align="center" class="labelText"> </td>
</tr>
<tr>
<td width="2%"><img src="images/dummy.gif" width="5" height="5" alt="*"></td>
<td><span class="labelText"> <a href="#Trip2"><u>2. Fahrt:</u></a>
									 am 
									<span class="7**
		">07.06.2010</span>
									 von
									 12:23 bis 16:11 Uhr</span></td>
<td align="center"><span class="labelText">03:48</span></td>
<td align="center"><span class="labelText">0</span></td>
<td align="center" class="labelText"> </td>
</tr>
<tr>
<td width="2%"><img src="images/dummy.gif" width="5" height="5" alt="*"></td>
<td><span class="labelText"> <a href="#Trip3"><u>3. Fahrt:</u></a>
									 am 
									<span class="7**
		">07.06.2010</span>
									 von
									 13:23 bis 17:03 Uhr</span></td>
<td align="center"><span class="labelText">03:40</span></td>
<td align="center"><span class="labelText">1</span></td>
<td align="center" class="labelText"> </td>
</tr>
<tr>
<td width="2%"><img src="images/dummy.gif" width="5" height="5" alt="*"></td>
<td><span class="labelText"> <a href="#Trip4"><u>4. Fahrt:</u></a>
									 am 
									<span class="7**
		">07.06.2010</span>
									 von
									 13:23 bis 17:08 Uhr</span></td>
<td align="center"><span class="labelText">03:45</span></td>
<td align="center"><span class="labelText">0</span></td>
<td align="center" class="labelText"> </td>
</tr>
</table>
</td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="buttonBgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="buttonBgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<tr>
<td width="2%" align="top"><img src="images/dummy.gif" width="7" height="13" border="0" alt="*"></td>
<td width="10%" align="center"><a href="javascript:document.forms[0].itdLPxx_view.value='printview';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='nop';document.forms[0].submit();"><img src="images/drucker.jpg" width="20" height="20" border="0" alt="zur Druckversion der ausgewählten Verbindungen"></a> <input type="hidden" name="configurationFontSizeNormal" value="0"><a href="javascript:document.forms[0].command.value='toPDF';document.forms[0].itdLPxx_view.value='printview';document.forms[0].submit();"><img src="images/pdf.gif" width="20" height="20" border="0" alt="In Datei Drucken"></a>  </td>
<td width="50%"><span class="labelTextBold">zur Druckversion der ausgewählten Verbindungen</span></td>
<td width="38%" align="right"><span class="labelText">(Alle Angaben ohne Gewähr.) </span></td>
</tr>
<tr>
<td colspan="4"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
</table>
</td>
</tr>
<tr class="buttonBgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="buttonBgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<td><img src="images/dummy.gif" width="50" height="1" alt="*"></td>
<td><input type="button" value="neue Fahrt" onClick="document.location.href='XSLT_TRIP_REQUEST2?language=de&amp;itdLPxx_transpCompany=vrr'"></td>
<td><input type="button" value="Rückfahrt" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='tripRetoure';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="Fahrt ändern" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='changeRequest';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="früher" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='tripPrev';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="später" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='tripNext';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="Hilfe" onclick="window.open('Bedienungshinweise_de.htm', 'help', '');"></td>
<td><img src="images/dummy.gif" width="50" height="1" alt="*"></td>
</table>
</td>
</tr>
<tr class="buttonBgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr><input type="hidden" name="tripSelection" value="on"><tr class="bgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="1" width="100%" border="0">
<tr class="bgColor">
<td colspan="8"><a name="Trip1"><img src="images/dummy.gif" width="1" height="5" border="0" alt="*"></a></td>
</tr>
<tr valign="middle">
<td align="top" class="kaestchen"><img src="images/dummy.gif" width="7" height="13" border="0" alt="*"></td>
<td align="top" colspan="8"><span class="labelTextBold"> 1. Fahrt</span></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">11:23</span></td>
<td class="bgColor nudgePartTrip"><div style="float:left;"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='1';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="früher" src="images/earlier.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='1';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">früher</a></div></td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Essen Hauptbahnhof: Gleis 4</span></td>
<td colspan="2" align="center" class="bgColor"><a href="/download/envmaps/vrr/09289_e_hbf_1.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_2.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_3.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="FILELOAD?Filename=dwa_4C0A9BA70.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 547 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">12:07</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5 E-H</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA71.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">12:11</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA72.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 557 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">13:34</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 9</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">13:37</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 9</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 547 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">15:08</span></td>
<td class="bgColor nudgePartTrip"><div style="float: left;"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='1';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="später" src="images/later.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='1';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">später</a></div></td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Berlin Hbf: Gleis 12 D - G</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td colspan="9"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8"><input type="checkbox" name="tripSelector1" checked="1"><span class="labelTextBold"> Verbindung zum Drucken auswählen</span></td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bodyBGColor">
<td colspan="12"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bgColor">
<td colspan="8"><a name="Trip2"><img src="images/dummy.gif" width="1" height="5" border="0" alt="*"></a></td>
</tr>
<tr valign="middle">
<td align="top" class="kaestchen"><img src="images/dummy.gif" width="7" height="13" border="0" alt="*"></td>
<td align="top" colspan="8"><span class="labelTextBold"> 2. Fahrt</span></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">12:23</span></td>
<td class="bgColor nudgePartTrip"><div style="float:left;"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='2';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="früher" src="images/earlier.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='2';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">früher</a></div></td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Essen Hauptbahnhof: Gleis 4</span></td>
<td colspan="2" align="center" class="bgColor"><a href="/download/envmaps/vrr/09289_e_hbf_1.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_2.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_3.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="FILELOAD?Filename=dwa_4C0A9BA73.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 849 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">13:07</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5 E-H</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA74.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">13:11</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA75.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 859 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">14:31</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 10</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">14:34</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 10</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 849 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">16:11</span></td>
<td class="bgColor nudgePartTrip"><div style="float: left;"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='2';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="später" src="images/later.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='2';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">später</a></div></td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Berlin Hbf: Gleis 12 A - D</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td colspan="9"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8"><input type="checkbox" name="tripSelector2" checked="1"><span class="labelTextBold"> Verbindung zum Drucken auswählen</span></td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bodyBGColor">
<td colspan="12"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bgColor">
<td colspan="8"><a name="Trip3"><img src="images/dummy.gif" width="1" height="5" border="0" alt="*"></a></td>
</tr>
<tr valign="middle">
<td align="top" class="kaestchen"><img src="images/dummy.gif" width="7" height="13" border="0" alt="*"></td>
<td align="top" colspan="8"><span class="labelTextBold"> 3. Fahrt</span></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">13:23</span></td>
<td class="bgColor nudgePartTrip"><div style="float:left;"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='3';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="früher" src="images/earlier.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='3';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">früher</a></div></td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Essen Hauptbahnhof: Gleis 6</span></td>
<td colspan="2" align="center" class="bgColor"><a href="/download/envmaps/vrr/09289_e_hbf_1.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_2.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_3.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="FILELOAD?Filename=dwa_4C0A9BA76.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 549 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">14:07</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5 E-H</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA77.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">14:11</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA78.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 559 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">15:34</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 9</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">15:37</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 9</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 549 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">16:54</span></td>
<td class="bgColor nudgePartTrip"><div style="float: left;"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='3';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="später" src="images/later.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='3';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">später</a></div></td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Berlin-Spandau: Gleis 6 A - C</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2"><span class="labelText">17:03</span></td>
<td class="bgColor2"> </td>
<td align="right" class="bgColor2"><span class="labelText">ab </span></td>
<td class="bgColor2"><span class="labelText">Berlin-Spandau</span></td>
<td colspan="2" align="center" class="bgColor2"></td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor2"><span class="labelText">IC 145 InterCity</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2"><span class="labelText">17:03</span></td>
<td class="bgColor2 nudgePartTrip"><div style="float: left;"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='3';document.forms[0].partialTrip.value='6';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="später" src="images/later.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='3';document.forms[0].partialTrip.value='6';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">später</a></div></td>
<td align="right" class="bgColor2"><span class="labelText">an </span></td>
<td class="bgColor2"><span class="labelText">Berlin Hbf</span></td>
<td colspan="2" align="center" class="bgColor2"></td>
<td class="bgColor2"><span class="labelText">Berlin Hbf</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor2"><span class="labelText"><i>Linie KBS 202: SnackPoint/Imbiss im Zug</i></span></td>
</tr>
<tr>
<td colspan="9"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8"><input type="checkbox" name="tripSelector3" checked="1"><span class="labelTextBold"> Verbindung zum Drucken auswählen</span></td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bodyBGColor">
<td colspan="12"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bgColor">
<td colspan="8"><a name="Trip4"><img src="images/dummy.gif" width="1" height="5" border="0" alt="*"></a></td>
</tr>
<tr valign="middle">
<td align="top" class="kaestchen"><img src="images/dummy.gif" width="7" height="13" border="0" alt="*"></td>
<td align="top" colspan="8"><span class="labelTextBold"> 4. Fahrt</span></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">13:23</span></td>
<td class="bgColor nudgePartTrip"><div style="float:left;"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='4';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="früher" src="images/earlier.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='prevPartialTrip';document.forms[0].trip.value='4';document.forms[0].partialTrip.value='1';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">früher</a></div></td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Essen Hauptbahnhof: Gleis 6</span></td>
<td colspan="2" align="center" class="bgColor"><a href="/download/envmaps/vrr/09289_e_hbf_1.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_2.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="/download/envmaps/vrr/09289_e_hbf_3.htm" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/bitmapThumb.gif" width="16" height="16" border="0" alt="Umgebungsplan" title="Umgebungsplan"></a><a href="FILELOAD?Filename=dwa_4C0A9BA79.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 549 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">14:07</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5 E-H</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA7A.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">14:11</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hamm (Westf): Gleis 5</span></td>
<td colspan="2" align="center" class="bgColor"><a href="FILELOAD?Filename=dwa_4C0A9BA7B.pdf" target="mapWindow"><img src="images/dummy.gif" width="5" height="1" border="0" alt="*"><img src="images/pdfThumb.gif" width="16" height="16" border="0" alt="Stadtplan" title="Stadtplan"></a></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 559 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">15:34</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 9</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8" class="bgColor"><span class="labelText"><i>Bordrestaurant</i></span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td rowspan="2" align="right" class="bgColor2" style="padding-right:3px;"><img src="images/means/sitz.gif" width="32" height="32" alt="*"></td>
<td class="bgColor2"><span class="labelText">nicht umsteigen,</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor2" colspan="4"><img src="images/dummy.gif" width="1" height="1"></td>
<td colspan="2" align="center" class="bgColor2"> </td>
<td class="bgColor2"><span class="labelText">Weiterfahrt im selben Fahrzeug möglich</span></td>
</tr>
<tr valign="middle">
<td colspan="8"><img src="images/dummy.gif" width="1" height="1"></td>
</tr>
<tr>
<td> </td>
<td class="bgColor"><span class="labelText">15:37</span></td>
<td class="bgColor"> </td>
<td align="right" class="bgColor"><span class="labelText">ab </span></td>
<td class="bgColor"><span class="labelText">Hannover Hauptbahnhof: Gleis 9</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td rowspan="2" align="right" class="bgColor" style="padding-right:3px;"><img src="images/means/train.gif" width="32" height="32" alt="Zug"></td>
<td class="bgColor"><span class="labelText">ICE 549 InterCityExpress</span></td>
</tr>
<tr valign="middle">
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td class="bgColor"><span class="labelText">17:08</span></td>
<td class="bgColor nudgePartTrip"><div style="float: left;"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='4';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"><img alt="später" src="images/later.gif" border="0"></a></div><div class="labelTextBold"><a href="javascript:document.forms[0].command.value='nextPartialTrip';document.forms[0].trip.value='4';document.forms[0].partialTrip.value='5';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();">später</a></div></td>
<td align="right" class="bgColor"><span class="labelText">an </span></td>
<td class="bgColor"><span class="labelText">Berlin Hbf: Gleis 12 A - D</span></td>
<td colspan="2" align="center" class="bgColor"></td>
<td class="bgColor"><span class="labelText">Berlin Ostbahnhof</span></td>
</tr>
<tr>
<td colspan="9"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td><img src="images/dummy.gif" width="1" height="1" alt="*"></td>
<td colspan="8"><input type="checkbox" name="tripSelector4" checked="1"><span class="labelTextBold"> Verbindung zum Drucken auswählen</span></td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bodyBGColor">
<td colspan="12"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
</table>
</td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<tr>
<td width="2%"></td>
<td valign="center"><img src="images/Rbl_nOK.gif" width="13" height="30" border="0" title="Abweichung" alt="Abweichung"></td>
<td valign="center"><span class="labelText" valign="center">Verspätungen sind berücksichtigt</span></td>
<td width="2%"></td>
<td valign="center"><img src="images/Rbl.gif" width="13" height="30" border="0" title="Pünktlich" alt="Pünktlich"></td>
<td valign="center"><span class="labelText" valign="center">Fahrt voraussichtlich pünktlich</span></td>
</tr>
</table>
</td>
</tr>
<tr class="bgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="bodyBGColor">
<td colspan="12"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="buttonBgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="buttonBgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<tr>
<td width="2%" align="top"><img src="images/dummy.gif" width="7" height="13" border="0" alt="*"></td>
<td width="10%" align="center"><a href="javascript:document.forms[0].itdLPxx_view.value='printview';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='nop';document.forms[0].submit();"><img src="images/drucker.jpg" width="20" height="20" border="0" alt="zur Druckversion der ausgewählten Verbindungen"></a> <input type="hidden" name="configurationFontSizeNormal" value="0"><a href="javascript:document.forms[0].command.value='toPDF';document.forms[0].itdLPxx_view.value='printview';document.forms[0].submit();"><img src="images/pdf.gif" width="20" height="20" border="0" alt="In Datei Drucken"></a>  </td>
<td width="50%"><span class="labelTextBold">zur Druckversion der ausgewählten Verbindungen</span></td>
<td width="38%" align="right"><span class="labelText">(Alle Angaben ohne Gewähr.) </span></td>
</tr>
<tr>
<td colspan="4"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
</table>
</td>
</tr>
<tr class="buttonBgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr class="buttonBgColor">
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<td><img src="images/dummy.gif" width="50" height="1" alt="*"></td>
<td><input type="button" value="neue Fahrt" onClick="document.location.href='XSLT_TRIP_REQUEST2?language=de&amp;itdLPxx_transpCompany=vrr'"></td>
<td><input type="button" value="Rückfahrt" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='tripRetoure';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="Fahrt ändern" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='changeRequest';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="früher" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='tripPrev';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="später" onClick="document.forms[0].itdLPxx_view.value='';document.forms[0].itdLPxx_ShowFare.value='';document.forms[0].command.value='tripNext';document.forms[0].itdLPxx_view.value='';document.forms[0].submit();"></td>
<td><input type="button" value="Hilfe" onclick="window.open('Bedienungshinweise_de.htm', 'help', '');"></td>
<td><img src="images/dummy.gif" width="50" height="1" alt="*"></td>
</table>
</td>
</tr>
<tr class="buttonBgColor">
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
<tr>
<td colspan="3"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
</tr>
</table>
</td>
</tr>
<tr>
<td colspan="3">
<table cellspacing="0" cellpadding="0" width="100%" border="0">
<td height="40" valign="middle" align="left"><img src="images/dummy.gif" width="1" height="5" alt="*"></td>
<td width="220" valign="bottom"><img src="images/ein-Service-des-VRR.gif" width="220" height="36" alt="*" align="right"></td>
</table>
</td>
</tr>
</table>
</form>
</body>
</html>
