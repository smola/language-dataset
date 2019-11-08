#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3		// Use modern global access method and strict wave access.

//Called by the buttons
Function atButtonProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	SVAR currentCmd = root:Packages:AT:currentCmd
	SVAR runCmdStr = root:Packages:AT:runCmdStr
	SVAR cdf = root:Packages:AT:currentDataFolder
	SVAR selectedWave = root:Packages:AT:selectedWave
	SVAR whichList = root:Packages:AT:whichList

	NVAR viewerOpen = root:Packages:AT:viewerOpen
	Variable i

	switch( ba.eventCode )
		case 2: // mouse up
			strswitch(ba.ctrlName)
				case "AT_RunCmd":
					ControlInfo/W=AT AT_CommandPop
					
					strswitch(S_Value)
						case "Run Cmd Line":
							SVAR wsDims = root:Packages:AT:DataSets:wsDims
							NVAR numWaveSets = root:Packages:AT:DataSets:numWaveSets
							NVAR wsn = root:Packages:AT:DataSets:wsn
							wsDims = ""
							numWaveSets = 0
							ControlInfo/W=AT cmdLineStr
							String cmdLineStr = S_Value
							
							Variable pos1 = 0,pos2 = 0,j,numWaves
							String dsRefList = "",dsName = ""
							
							//Find the data set references in the command string
							Do
								pos1 = strsearch(S_Value,"<",0)
								pos2 = strsearch(S_Value,">",pos1)
								If(pos1 != -1 && pos2 != -1)
									dsName = S_Value[pos1+1,pos2-1]
									If(!cmpstr(dsName,"wsi"))
										S_Value = S_Value[pos2+1,strlen(S_Value)-1]
										continue
									EndIf
									dsRefList += dsName + ";"
									S_Value = S_Value[pos2+1,strlen(S_Value)-1]
								Else
									break
								EndIf
							While(pos1 != -1)
							
							//Test whether they have the same number of waveset dimensions
							String dims = GetWaveSetDims(StringFromList(0,dsRefList,";"))
							Make/FREE/N=(ItemsInList(dsRefList,";")) wsDimSize //holds number of waves in each waveset
							wsDimSize[0] = str2num(dims)
							
							If(ItemsInList(dsRefList,";") > 0)	//if there are data set references, otherwise continue
								For(i=1;i<ItemsInList(dsRefList,";");i+=1)
									String testDims = GetWaveSetDims(StringFromList(i,dsRefList,";"))
									wsDimSize[i] = str2num(testDims)
									If(cmpstr(testDims,dims))
									//	Abort "Data sets must have the same dimensions"
									EndIf
								EndFor		
								
								dsName = StringFromList(0,dsRefList,";") //name of first data set found		
								numWaveSets = GetNumWaveSets(dsName)	//number wave sets
							Else
								numWaveSets = 1
							EndIf
							
							For(i=0;i<numWaveSets;i+=1)
								If(strlen(dsName))
									String theWaveSet = GetWaveSet(dsName,wsn=i)
									//numWaves = ItemsInList(theWaveSet,";")
									numWaves = WaveMax(wsDimSize)

								Else
									numWaves = 1
								EndIf
								
								For(j=0;j<numWaves;j+=1)
									runCmdStr = resolveCmdLine(cmdLineStr,i,j)
									
									//check if there is an output wave assignment, if so does it exist?
									String left,outWaveName,folder,firstWave
									Variable pos
									
									left = ""
									If(stringmatch(runCmdStr,"*=*"))
									   left = StringFromList(0,runCmdStr,"=")
									EndIf
									
									If(strlen(left) && !stringmatch(left,"*/*")) //makes sure that the equals sign is truly for a wave assignment, as opposed to a flag assignment
										pos = strsearch(left,"[",0)
										If(pos != -1)
											outWaveName = left[0,pos-1]
										Else
											outWaveName = left
										EndIf	
										
										//if its a full path, don't add a path to the name
										If(!stringmatch(outWaveName,"root:*"))
											firstWave = StringFromList(0,theWaveSet,";")
											folder = GetWavesDataFolder($firstWave,1)
										
											//full path to output wave
											outWaveName = folder + outWaveName
										
											//append full path to cmd string
											runCmdStr[0,pos-1] = ""
											runCmdStr = outWaveName + runCmdStr
										EndIf
	
										If(strlen(outWaveName) && !WaveExists($outWaveName))
											Make/O/N=(numWaves) $outWaveName
										EndIf 

									EndIf
									
									RunCmd(runCmdStr)
									print runCmdStr
								EndFor
							EndFor

							break
						case "External Function":
						case "Duplicate/Rename":
						case "Average":
						case "Error":
						case "Duplicate/Rename":
						case "Kill Waves":
							SVAR wsDims = root:Packages:AT:DataSets:wsDims
							NVAR numWaveSets = root:Packages:AT:DataSets:numWaveSets
							NVAR wsn = root:Packages:AT:DataSets:wsn
							wsDims = ""
							numWaveSets = 0
							
							//Command string
							If(cmpstr(S_Value,"External Function") == 0)
								runCmdStr = SetExtFuncCmd()
							EndIf
						
							//WaveSet data
							ControlInfo/W=AT extFuncDS
							numWaveSets = GetNumWaveSets(S_Value)
							wsDims = GetWaveSetDims(S_Value)	
							
							//Run the command for the designated number of wavesets
							Variable ref = StartMSTimer
							For(wsn=0;wsn<numWaveSets;wsn+=1)		
								RunCmd(runCmdStr)
							EndFor
							Variable dur = StopMSTimer(ref)/1000000
							print runCmdStr + "..." + num2str(numWaveSets) + " wavesets..." + num2str(dur) + " s"
							break
						default:
							RunCmd(runCmdStr)
							break
					endswitch

					break
				case "atBrowseBackButton":
					String parentDF = ParseFilePath(1,cdf,":",1,0)
					String childDF = ParseFilePath(0,cdf,":",1,0)
					
					
					SetDataFolder parentDF
					cdf = parentDF
					GetFolderListItems()
					GetFolderItems()
					
					Wave/T folderTable = root:Packages:AT:folderTable
					Variable index = tableMatch(childDF,folderTable)
					
					Wave selFolderWave = root:Packages:AT:AT_folderList
					selFolderWave = 0
					If(index != -1)
						selFolderWave[index] = 1
					EndIf
					ControlUpdate AT_cdf
					break
			
				case "selectAll_Left":
					//select all scans,ROIs,folders, or wave items
					selectALL(ba.ctrlName)
					break
				case "selectAll_Right":
					Wave selWave = root:Packages:AT:AT_selectedWave
					Wave/T listWave = root:Packages:AT:AT_waveList
					SVAR viewerRecall = root:Packages:AT:viewerRecall
					//select all scans,ROIs,folders, or wave items
					selectALL(ba.ctrlName)
				
					String selWaveList = tableToList(listWave,";")
					AppendToViewer(selWaveList)
										
					//Set to null if a selection has changed.
					viewerRecall = ""
					
					break
				
				case "AT_Help":
					ControlInfo/W=AT AT_CommandPop
					strswitch(S_Value)
						case "Data Sets":
							DisplayHelpTopic "Data Sets"
							break
						case "External Function":
							DisplayHelpTopic "External Functions"
							break
						case "Run Cmd Line":
							DisplayHelpTopic "Run Cmd Line"
							break
						default:
							DisplayHelpTopic "Selecting a function to use"
							break
					endswitch
					break
				case "extFuncHelp":
					ControlInfo/W=AT extFuncPopUp
					displayExtFuncHelp(S_Value)
					break
					
				case "addDataSet":
					Wave/T dataSetNames = root:Packages:AT:DataSets:dataSetNames
					ControlInfo/W=AT dataSetName
					addDataSet(S_Value)
					fillFilterTable()
					saveSelection(S_Value) //saves the listbox selection that was used for matching the waves to make the dataset
					
					Wave/T dsFilters = root:Packages:AT:DataSets:dsFilters
					index = tableMatch(S_Value,dsFilters)
					dsFilters[index][1] = getListRange("0-2",dsFilters[index][1],";") + ";;;;;;;"
					break
					
				case "addDataSetFromSelection":
					Wave/T dataSetNames = root:Packages:AT:DataSets:dataSetNames
					ControlInfo/W=AT dataSetName	
					addDataSet(S_Value,selection=1)
					ListBox dataSetListBox win=AT,selRow=DimSize(dataSetNames,0)-1
					fillFilterTable()
					
					Wave/T dsFilters = root:Packages:AT:DataSets:dsFilters
					index = tableMatch(S_Value,dsFilters)
					dsFilters[index][1] = getListRange("0-2",dsFilters[index][1],";") + ";;;;;;;"
					break
					
				case "delDataSet":
					Wave/T listWave = root:Packages:AT:DataSets:dataSetNames
					ControlInfo/W=AT dataSetListBox
					delDataSet(V_Value)
					
					If(V_Value == 0)
						ListBox dataSetListBox win=AT,selRow=V_Value
						index = V_Value						
					Else
						ListBox dataSetListBox win=AT,selRow=V_Value-1
						index = V_Value - 1
						Wave/T dataSetWave = $("root:Packages:AT:DataSets:DS_" + listWave[index])
						checkMissingWaves(listWave[index])
					EndIf
					
					
					//update the list box to show the newly selected data set
					UpdateDSListBox(dataSetWave)
					
					updateWSDimText()
					updateWSFilters()
					break
					
				case "reloadATButton":
					GetWindow/Z AT wsize
					KillWindow/Z AT
					
					LoadAnalysisToolbox(left=V_left,top=V_top-50)
					//-50 accounts for the initial 50 pixel offset it gets upon first loading.
					break
				case "atViewerButton":
					If(viewerOpen)
						closeViewer()
					Else
						openViewer()
					EndIf
					break
					
				case "atViewerAutoScaleButton":
					SetAxis/W=AT#atViewerGraph/A
					break
					
				case "atViewerSeparateVertButton":
					SeparateTraces("vert")
					break
					
				case "atViewerSeparateHorizButton":
					SeparateTraces("horiz")
					break
					
				case "atViewerDisplayTracesButton":
					String theTraces = TraceNameList("AT#atViewerGraph",";",1)
	
					GetWindow/Z AT wsize
					//Duplicates the Viewer graph outside of the viewer
					String winRec = WinRecreation("AT#atViewerGraph",0)
					
					pos1 = strsearch(winRec,"/W",0)
					pos2 = strsearch(winRec,"/FG",0) - 1
					
					String matchStr = winRec[pos1,pos2]
					winRec = ReplaceString(matchStr,winRec,"/W=(" + num2str(V_right+10) + "," + num2str(V_top) + "," + num2str(V_right+360) + "," + num2str(V_top+200) + ")")
					winRec = ReplaceString("/FG=(FL,VT,FR,VB)/HOST=#",winRec,"")
					Execute/Q/Z winRec
					break
					
				case "atViewerClearTracesButton":
					clearTraces()
					break
					
				case "matchStraddOR":
					ControlInfo/W=AT waveMatch
					SetVariable waveMatch win=AT,value=_STR:S_Value + "||"
					break
					
				case "notMatchStraddOR":
					ControlInfo/W=AT waveNotMatch
					SetVariable waveNotMatch win=AT,value=_STR:S_Value + "||"
					break
					
				case "OpenABF2Loader":
					InitializeABFPanel()
					break
					
			endswitch
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function atListBoxProc(lba) : ListBoxControl
	STRUCT WMListboxAction &lba
	SVAR cdf = root:Packages:AT:currentDataFolder

	SVAR selWaveList = root:Packages:AT:selWaveList
	
	SVAR viewerRecall = root:Packages:AT:viewerRecall
	NVAR viewerOpen = root:Packages:AT:viewerOpen
	Variable row = lba.row
	Variable col = lba.col
	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave
	Variable i,scanNum
	
	selWaveList = ""
	switch( lba.eventCode )
		case -1: // control being killed
			break
		case 1: // mouse down
			strswitch(lba.ctrlName)
				case "matchListBox":
					//detect right clicks
					
					If(lba.eventMod == 17)
						PopupContextualMenu/C=(lba.MouseLoc.h, lba.MouseLoc.v) "Browse Wave"
						If(V_flag)
							String dsName = whichDataSet()
							Wave/T theDataSet = $("root:Packages:AT:DataSets:DS_" + dsName)
							
							If(!WaveExists(theDataSet))
								Wave/T theDataSet = root:Packages:AT:AT_waveMatchList_FullPath
							EndIf
							
							Variable fullPathList = 0
							
							selWaveList = ""
							For(i=0;i<DimSize(theDataSet,0);i+=1)
								If(i > DimSize(selWave,0) - 1)
									break
								EndIf
								
								If(selWave[i] == 1)
									selWaveList += theDataSet[i] + ";"
								
									//If we hit a waveset label
									If(stringmatch(theDataSet[i],"*WSN*"))
										selWaveList = ReplaceString(theDataSet[i] + ";",selWaveList,"")	//remove the wsLabel from the list
										selWaveList += getWaveSet(dsName,wsLabel=theDataSet[i])	//add in the entire waveset to the list
										fullPathList = 1
									EndIf
								EndIf
							EndFor	
					
							//Browse to the selected wave
							ModifyBrowser clearSelection,selectList=selWaveList
							
						EndIf
					EndIf
					break
				case "dataSetListBox":
					If(lba.eventMod == 17)
					//recalls the selection used to make the data set originally
						PopupContextualMenu/C=(lba.MouseLoc.h, lba.MouseLoc.v) "Retrieve Selection"
						If(V_flag)
							dsName = whichDataSet()
							recallSelection(dsName)
						EndIf
					EndIf
					break
			endswitch
		case 2: // mouse up
			strswitch(lba.ctrlName)
				case "AT_ItemListBox":
					selWaveList = ""

					For(i=0;i<DimSize(listWave,0);i+=1)
						If(selWave[i] == 1)
							selWaveList += listWave[i] + ";"
						EndIf
					EndFor

					//Add the traces to viewer if its in the analysis tab
					AppendToViewer(selWaveList)
					
					break
				
				default:
					break
			endswitch
			break
		case 3: // double click
			strswitch(lba.ctrlName)
				case "AT_FolderListBox":
					//ControlInfo/W=AT folderDepth
					Variable depth = 0
					
					If(row > DimSize(listWave,0)-1)
						break
					EndIf
					cdf = cdf + listWave[row] + ":"
					SetDataFolder cdf
					GetFolderListItems()
					GetFolderItems(depth=depth)
					ControlUpdate AT_cdf
					break
					
				case "dataSetListBox":
					//Displays the data set in the Viewer if double clicked
					If(!viewerOpen)
						openViewer()
					EndIf
						
					Variable selection = lba.row
					Wave/T theDataSet = $("root:Packages:AT:DataSets:DS_"+listWave[selection])
					Variable numWaves = DimSize(theDataSet,0)
					selWaveList = ""
					
					String traceList = TraceNameList("AT#atViewerGraph",";",1)
					If(strlen(traceList))
						KillWindow/Z AT#atViewerGraph
						Display/HOST=AT/FG=(FL,VT,FR,VB)/N=atViewerGraph
					EndIf
					
					For(i=0;i<numWaves;i+=1)
						Wave/Z theWave = $theDataSet[i]
						Variable type = WaveType(theWave,1)
						If(type == 1)
							AppendToGraph/W=AT#atViewerGraph theWave
						EndIf
					EndFor
					
					//select all
					Wave/Z selWave = root:Packages:AT:AT_selectedMatchWave
					selWave = 1
				break
			endswitch
			break
		case 4: // cell selection
			strswitch(lba.ctrlName)
				case "dataSetListBox":
					Wave/T listWave = root:Packages:AT:DataSets:dataSetNames
					Wave selWave = root:Packages:AT:DataSets:dataSetSelWave
					Wave/T waveListTable = root:Packages:AT:AT_waveMatchList
					Wave/T AT_waveMatchList_FullPath = root:Packages:AT:AT_waveMatchList_FullPath
					Wave/T ogAT_waveMatchList_UnGroup = root:Packages:AT:DataSets:ogAT_waveMatchList_UnGroup
					Wave matchListselWave = root:Packages:AT:AT_selectedMatchWave
					
					selection = lba.row
					
					//If selection is past the item list
					If(selection > DimSize(listWave,0) - 1)
						break
					EndIf
					
					Wave/T dataSetWave = $("root:Packages:AT:DataSets:DS_" + listWave[selection])
					Wave/T ogDataSetWave = $("root:Packages:AT:DataSets:ogDS_" + listWave[selection])
					
					UpdateDSListBox(dataSetWave)
					
					checkMissingWaves(listWave[selection])
					updateWSDimText()
					updateWSFilters()
					
					//Set full path text wave to match the wave name text wave
					Redimension/N=(DimSize(ogDataSetWave,0)) ogAT_waveMatchList_UnGroup
					ogAT_waveMatchList_UnGroup = ogDataSetWave
					Redimension/N=(DimSize(dataSetWave,0)) AT_waveMatchList_FullPath
					AT_waveMatchList_FullPath = dataSetWave
					break
				case "extFuncDSListBox":
						Wave/T listWave = root:Packages:AT:DataSets:dataSetListWave_NamesOnly
						Wave selWave = root:Packages:AT:DataSets:dsSelWave
						selWaveList = ""
						For(i=0;i<DimSize(listWave,0);i+=1)
							If(selWave[i] == 1)
								selWaveList += listWave[i] + ";"
							EndIf
						EndFor
						
						ControlInfo/W=AT extFuncDS
						Wave/T theDataSet = $("root:Packages:AT:DataSets:DS_" + S_Value)
						
						AppendDSWaveToViewer(selWave,selWaveList,theDataSet)
						break
				case "AT_ItemListBox":	
					selWaveList = ""
					For(i=0;i<DimSize(listWave,0);i+=1)
						If(selWave[i] == 1)
							selWaveList += listWave[i] + ";"
						EndIf
					EndFor
					
					AppendToViewer(selWaveList)
										
					//Set to null if a selection has changed.
					viewerRecall = ""
					break
				endswitch
		case 5: // cell selection plus shift key
			strswitch(lba.ctrlName)
				case "extFuncDSListBox":
					Wave/T listWave = root:Packages:AT:DataSets:dataSetListWave_NamesOnly
					Wave selWave = root:Packages:AT:DataSets:dsSelWave
					
					ControlInfo/W=AT extFuncDS
					dsName = S_Value
					Wave/T theDataSet = $("root:Packages:AT:DataSets:DS_" + dsName)				
					fullPathList = 0
					
					selWaveList = ""
					For(i=0;i<DimSize(listWave,0);i+=1)
						If(selWave[i] == 1)
							selWaveList += listWave[i] + ";"
							
							//If we hit a waveset label
							If(stringmatch(listWave[i],"*WSN*"))
								Variable index = tableMatch(listWave[i],theDataSet)
								selWaveList = ReplaceString(listWave[i] + ";",selWaveList,"")	//remove the wsLabel from the list
								selWaveList += getWaveSet(dsName,wsLabel=listWave[i])	//add in the entire waveset to the list
								fullPathList = 1
							EndIf
						EndIf
					EndFor	
			
					AppendDSWaveToViewer(selWave,selWaveList,theDataSet,fullPathList=fullPathList)
					break
				case "matchListBox":
					
					Wave/T listWave = root:Packages:AT:AT_waveMatchList
					Wave selWave = root:Packages:AT:AT_selectedMatchWave
					
					dsName = whichDataSet()
					Wave/T theDataSet = $("root:Packages:AT:DataSets:DS_" + dsName)
					
					If(!WaveExists(theDataSet))
						Wave/T theDataSet = root:Packages:AT:AT_waveMatchList_FullPath
					EndIf
					
					fullPathList = 0
					
					selWaveList = ""
					For(i=0;i<DimSize(theDataSet,0);i+=1)
						If(i > DimSize(selWave,0) - 1)
							break
						EndIf
						
						If(selWave[i] == 1)
							selWaveList += theDataSet[i] + ";"
						
							//If we hit a waveset label
							If(stringmatch(theDataSet[i],"*WSN*"))
								selWaveList = ReplaceString(theDataSet[i] + ";",selWaveList,"")	//remove the wsLabel from the list
								selWaveList += getWaveSet(dsName,wsLabel=theDataSet[i])	//add in the entire waveset to the list
								fullPathList = 1
							EndIf
						EndIf
					EndFor			
					
					AppendDSWaveToViewer(selWave,selWaveList,theDataSet,fullPathList=fullPathList)
					
					break
				case "AT_ItemListBox":	
					selWaveList = ""
					For(i=0;i<DimSize(listWave,0);i+=1)
						If(selWave[i] == 1)
							selWaveList += listWave[i] + ";"
						EndIf
					EndFor
					
					AppendToViewer(selWaveList)
										
					//Set to null if a selection has changed.
					viewerRecall = ""
					break
				case "fileListBox":
					//PClamp Browser file selection
					//Loads the sweeps into memory and displays their names it the sweep list box
					LoadABF(fromAT=1)
					break
			endswitch
		case 6: // begin edit
			break
		case 7: // finish edit
			break
		case 13: // checkbox clicked (Igor 6.2 or later)
			break
	endswitch

	return 0
End

//Called by the pop up menus
Function atPopProc(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa
	SVAR currentCmd = root:Packages:AT:currentCmd
	SVAR prevCmd = root:Packages:AT:prevCmd
	SVAR runCmdStr = root:Packages:AT:runCmdStr

	switch( pa.eventCode )

		case 2: // mouse up
		
			strswitch(pa.ctrlName)
				case "AT_CommandPop":
					Variable popNum = pa.popNum
					String popStr = pa.popStr
			
					If(cmpstr(popStr[0],"-") == 0)
						return 0
					EndIf
			
					prevCmd = currentCmd
					currentCmd = popStr
					AT_ChangeControls(currentCmd,prevCmd)
					
					//Refresh external function page when it opens
					If(cmpstr(pa.popStr,"External Function") == 0)
						Wave/Z/T dataSetNames = root:Packages:AT:DataSets:dataSetNames
						SVAR DSNames = root:Packages:AT:DSNames
						DSNames = "--None--;--Wave List--;" + tableToList(dataSetNames,";")
					
						KillExtParams()
						ControlInfo/W=AT extFuncPopUp
						ResolveFunctionParameters("AT_" + S_Value)
						recallExtFuncValues(S_Value)
						//Toggle the channel pop up menu
						ControlInfo/W=AT extFuncDS
						
						If(cmpstr(S_Value,"--None--") == 0 || cmpstr(S_Value,"--Wave List--") == 0)
							//Item List selection or no wave selection
							ListBox extFuncDSListBox win=AT,disable=1
							DrawAction/W=AT delete
						Else
							//Data set selection
							SetDrawEnv/W=AT fsize=12,xcoord=abs,ycoord=abs
							DrawText/W=AT 230,117,"Waves:"
							OpenExtFuncWaveListBox(S_Value)
						EndIf
					Else
						strswitch(pa.popStr)
							case "Average":
							case "Error":
							case "Kill Waves":
							case "Run Cmd Line":
							case "Duplicate/Rename":
							case "Make Waves":
								Wave/Z/T dataSetNames = root:Packages:AT:DataSets:dataSetNames
								SVAR DSNames = root:Packages:AT:DSNames
								DSNames = "--None--;--Wave List--;" + tableToList(dataSetNames,";")		
								break
						endswitch
					EndIf
					
					break
				case "extFuncPopUp":
					KillExtParams()
					ResolveFunctionParameters("AT_" + pa.popStr)
					recallExtFuncValues(pa.popStr)
					break
				case "extFuncDS":
					SetExtFuncMenus(pa.popStr)
					SVAR wsDims = root:Packages:AT:DataSets:wsDims
					wsDims = GetWaveSetDims(pa.popStr)
					break
			endswitch
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function atSetVarProc(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva
	
	Wave/T WaveListDS = root:Packages:AT:DataSets:WaveListDS
	Wave/T ogWaveListDS = root:Packages:AT:DataSets:ogWaveListDS	
		
	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
			Variable dval = sva.dval
			String sval = sva.sval
				//Get wave match string
			strswitch(sva.ctrlName)
				case "waveMatch":
					SVAR waveMatchStr = root:Packages:AT:waveMatchStr
					waveMatchStr = sval
					
					String df = GetDataFolder(1)
					
					//Remove selection from the data set box
					ListBox dataSetListBox win=AT,selRow=-1
					String dataSetName= whichDataSet()
					
					clearFilterSet()
					
					getWaveMatchList()
					fillFilterTable()
					updateWSDimText()
					SetDataFolder $df
					
					If(!strlen(dataSetName))
						//Save the wave list table prior to grouping again, ensures its correct
						Wave/T/Z ds = root:Packages:AT:AT_waveMatchList_FullPath
						Duplicate/T/O ds,root:Packages:AT:DataSets:ogAT_waveMatchList_FullPath,root:Packages:AT:DataSets:ogAT_waveMatchList_UnGroup
					EndIf
					
					SetDataFolder $df
					
					break
				case "waveNotMatch":
					SVAR waveNotMatchStr = root:Packages:AT:waveNotMatchStr
					waveNotMatchStr = sval
					
					df = GetDataFolder(1)
					//Remove selection from the data set box
					ListBox dataSetListBox win=AT,selRow=-1
					dataSetName= whichDataSet()
					
					clearFilterSet()
					
					getWaveMatchList()
					fillFilterTable()
					updateWSDimText()
					SetDataFolder $df
					
					If(!strlen(dataSetName))
						//Save the wave list table prior to grouping again, ensures its correct
						Wave/T/Z ds = root:Packages:AT:AT_waveMatchList_FullPath
						Duplicate/T/O ds,root:Packages:AT:DataSets:ogAT_waveMatchList_FullPath,root:Packages:AT:DataSets:ogAT_waveMatchList_UnGroup
					EndIf
					
					SetDataFolder $df
					
					break	
				case "relativeFolderMatch":
					df = GetDataFolder(1)
					//Remove selection from the data set box
					ListBox dataSetListBox win=AT,selRow=-1
					clearFilterSet()
					getWaveMatchList()
					fillFilterTable()
					updateWSDimText()	
					SetDataFolder $df
					
					If(!strlen(dataSetName))
						//Save the wave list table prior to grouping again, ensures its correct
						Wave/T/Z ds = root:Packages:AT:AT_waveMatchList_FullPath
						Duplicate/T/O ds,root:Packages:AT:DataSets:ogAT_waveMatchList_FullPath,root:Packages:AT:DataSets:ogAT_waveMatchList_UnGroup
					EndIf

					SetDataFolder $df
					break
				case "prefixGroup":
				case "groupGroup":
				case "seriesGroup":
				case "sweepGroup":
				case "traceGroup":		
				case "waveGrouping":			
					dataSetName= whichDataSet()
					
					//If no data set is selected, use the wave present in the match list box
					If(!strlen(dataSetName))
						Wave/T/Z waveListTable = getWaveMatchList() //also gets the full path
						Wave/T/Z ds = root:Packages:AT:AT_waveMatchList_FullPath
						
						//original ungrouped list
						Wave/T/Z original = root:Packages:AT:DataSets:ogAT_waveMatchList_FullPath
						
						Duplicate/O original,root:Packages:AT:DataSets:ogAT_waveMatchList_UnGroup
						Wave/T/Z unGrouped = root:Packages:AT:DataSets:ogAT_waveMatchList_UnGroup
						
						//filter the waves
						filterByGroup(ds)
						filterByGroup(unGrouped)
								
						//group the waves
						setWaveGrouping(unGrouped,ds)
						
						//make sure wave sets are all valid dimensions	
						checkWaveSets(ds)
						
						//update the list box and wave counters
						updateWaveListBox()
						updateWSDimText()
						
					Else
						//data set
						Wave/T/Z ds = GetDataSetWave(dsName = dataSetName)
						
						//original ungrouped list
						Wave/T/Z original = $("root:Packages:AT:DataSets:ogDS_" + dataSetName)
						
						//group the waves
						setWaveGrouping(original,ds)
						
						//filter the waves
						filterByGroup(ds)
						
						//make sure wave sets are all valid dimensions	
						checkWaveSets(ds)
						
						//fill out the filter table
						fillFilterTable()
						
						//update the list box and wave counters
						updateWaveListBox()
						updateWSDimText()
					EndIf
	
					break
			endswitch
			break
		case -1: // control being killed
			break
	endswitch
	
	return 0
End

//Called by external procedures parameters 
Function atExtParamPopProc(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva

	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
			Variable dval = sva.dval
			String sval = sva.sval
			ControlInfo/W=AT extFuncPopUp
			UpdateExtFuncValues(S_Value)
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End