/********************************************************
:Author:	Timothy Halim
:Site:	http://timo.ink
:Email:	mail@timo.ink
:Date:	17 Jan 2016
:Update:	7 Mar 2016
********************************************************/
TextureListVersion="16.05.09"
 
/*
		Notes:
		Feel free to edit the script for personal use.
		Please don't distribute or commercialize it without my permission.
		Please notifyme and put my name on credits if you use part of this script.
*/
 
try destroydialog List_assets catch()
try destroydialog textwindow catch()

rollout List_assets "List of assets"
(
	button btn_refresh "refresh" pos:[7,7] width:16 height:16 tooltip:"Refresh Texture List" border:false
	label lbl_all "All: 0000" pos:[28,9] width:64 height:16
	checkbox chk_ok "Ok: 0000" pos:[80,8] width:85 height:17 checked:true
	checkbox chk_found "Found: 0000" pos:[165,8] width:85 height:17 checked:true
	checkbox chk_missing "Missing: 0000" pos:[250,8] width:85 height:17 checked:true
	button btn_collect "Collect" pos:[340,8] width:60 height:16 tooltip:"Collect all texture"
	button btn_resolve "Fix Path" pos:[400,8] width:60 height:16 tooltip:"Fix found path to absolute path"
	button btn_export "Export" pos:[460,8] width:60 height:16 tooltip:"Export texture list to text"
	button btn_close "X" pos:[525,8] width:17 height:17 tooltip:"Close Texture List" border:false 
	dotNetControl table_assets "System.Windows.Forms.ListView" pos:[3,28] align:#center
	progressbar prg_bar pos:[5,5] width:540 height:20 visible:off
	
	struct asset (fileclass, fileName, filepath, foundpath, filestatus)
	struct countStat (okay, found, missing, xref)
	
	Global folderdotnet = dotNetClass "System.IO.Directory"
	Global filedotnet = dotNetClass "System.IO.File"

	local FileTypeLights = #(".ies")
	local FileTypeImg = #(".jpg", ".png", ".bmp", ".tif", ".tiff", ".tga", ".psd", ".gif", ".hdr", ".exr")
	local Asset_list = #()
	local counts
	local mapdest
	local LA_h_footer = 30
	local currentimage = undefined
	local ImageFile
	
	function fn_progress i all =
	(
		if i == 0 do i = 1
		if i > all do i = all
		if all == 0 then (precents = 0 as float) else (precents = i*100.00/all as float)
		if precents >= 100 then (precents = 100 as float; prg_bar.visible = off; prg_bar.pos = [5,-30])
		else ( prg_bar.visible = on; prg_bar.pos = [5,5])
		clearlistener()
		if precents < 100 do format "% \%\n" precents
		if precents == 100 do format "Done!\n"
		
		prg_bar.value = precents
	)
	function fn_Repath asset fileclass filepath =
	(
		if filepath != undefined do
		(	
			ATSOps.Refresh() 
			if filepath != "" do fileclass.filename = filepath
		)
	)
	function fn_GetStatus file = 
	(
		local status
		local foundfile = mapPaths.getFullFilePath(file)
		if doesFileExist(file) do
		( status = #Ok; counts.okay += 1)
		if doesFileExist(file) == false and foundfile != "" do
		( status = #Found; counts.found += 1)
		if doesFileExist(file) == false and foundfile == "" do
		( status = #Missing; counts.missing += 1)
		return status
	)
	function fn_GetAsset =
	(
		counts = countStat 0 0 0 0
		templist = #()
		Asset_list = #()
		arrBMs = #()
		arrPaths = #()
		arrBMs = getClassInstances bitmapTexture
		for i=1 to arrBMs.count do
		(	
			--format "% %\n" arrBMs[i] arrBMs[i].filename
			if finditem templist arrBMs[i].filename == 0 and arrBMs[i].filename != undefined do 
			(	
				append templist arrBMs[i].filename
				fileclass = arrBMs[i]
				fileName = (getFilenameFile arrBMs[i].filename)
				filepath = (mapPaths.getFullFilePath (arrBMs[i].filename))
				foundpath = arrBMs[i].filename
				filestatus =  fn_GetStatus foundpath
				append Asset_list (asset fileclass fileName filepath foundpath filestatus)
				arrBMs[i].name = fileName
			)
			if finditem templist arrBMs[i].filename != 0 do
			(
				for a in Asset_list where arrBMs[i] != a.fileclass and arrBMs[i].fileName == a.fileclass.filename do 
				(
					replaceInstances arrBMs[i] a.fileclass
				)
			)
			fn_progress i arrBMs.count
		)
	)
	function fn_Collect =	-- get texture list and copy then repath
	( 
		ATSOps.Refresh() --Sometime asset tracking hold path strings
		for i=1 to asset_list.count where Asset_list[i].filestatus != #Missing do
		(	oldpath = Asset_list[i].filepath
			if oldpath == "" do oldpath = Asset_list[i].foundpath
			file = getFilenameFile oldpath + getFilenametype oldpath
			newpath = mapdest +"\\"+ file
			if (toLower Newpath) != (toLower Oldpath) do
			(
				try filedotnet.copy oldpath newpath true catch (format "% Not Repathed" oldmapname)
				if doesfileexist newpath do 
				(	
					fn_Repath Asset_list[i] Asset_list[i].fileclass newpath
					format "Repathed: % \nto      : %\n" oldpath newpath
				)
			)
			fn_progress i asset_list.count 
		)
		ATSOps.Refresh()
	)
	function fn_createrow file filepath status rownumber=
	(
		row = dotNetObject "System.Windows.Forms.listViewItem" (filenamefrompath filepath)
		row.SubItems.add (getFilenamePath filepath)
		row.SubItems.add (file.filestatus as string)
		if(mod rownumber 2) != 0 then row.BackColor = (dotNetClass "System.Drawing.Color").FromARGB 50 50 50
			else row.BackColor = (dotNetClass "System.Drawing.Color").FromARGB 45 45 45
		if (finditem FileTypeLights (toLower(getFilenameType filepath))) != 0 do
			row.ForeColor = (dotNetClass "System.Drawing.Color").LightYellow
		if (finditem FileTypeImg (toLower(getFilenameType filepath))) != 0 do
			row.ForeColor = (dotNetClass "System.Drawing.Color").FromARGB 220 220 220
		if status == #Missing do
			row.ForeColor = (dotNetClass "System.Drawing.Color").Red
		return row
	)
	function fn_DivideAssets ch =
	(
		local theRange = #()
		local tmpar = #()
		local row
		for i=1 to asset_list.count do
		(
			if Asset_list[i].filestatus == #Ok and not ch[1] do 
				continue
			if Asset_list[i].filestatus == #Found and not ch[2] do 
				continue 
			if Asset_list[i].filestatus == #Missing and not ch[3] do 
				continue 
			row = fn_createrow Asset_list[i] Asset_list[i].foundpath Asset_list[i].filestatus i
			append theRange row
			append tmpar Asset_list[i].filepath
		)
		free tmpar
		return theRange
	)
	function fn_createcol =
	(	
		table_assets.Columns.clear()
		table_assets.gridLines = false
		table_assets.View = (dotNetClass "System.Windows.Forms.View").Details
		table_assets.Sorting = (dotNetClass "System.Windows.Forms.SortOrder").None
		table_assets.FullRowSelect = true
		table_assets.multiSelect = true
		if Asset_list.count < 10 then
		(	
			table_assets.Columns.add "File name" 160
			table_assets.Columns.add "File path" (List_assets.width-230)
			table_assets.Columns.add "Status" 60
		)
		else
		(
			table_assets.Columns.add "File name" 160
			table_assets.Columns.add "File path" (List_assets.width-247)
			table_assets.Columns.add "Status" 60
		)
		table_assets.Items.AddRange (fn_DivideAssets(#(chk_ok.checked,chk_found.checked,chk_missing.checked)))
		table_assets.ForeColor = table_assets.ForeColor.FromARGB 220 220 220
		table_assets.BackColor = table_assets.ForeColor.FromARGB 50 50 50
	)
	function fn_refreshlist =
	(
		table_assets.Items.clear()
		fn_createcol()
		lbl_all.caption = "All : " + table_assets.items.count as string
		chk_ok.caption = "Ok : " + counts.okay as string
		chk_found.caption = "Found : " + counts.found as string
		chk_missing.caption = "Missing : " + counts.missing as string
	) 
	
	on List_assets open do
	(
		List_assets.title = "List of assets " + TextureListVersion
		table_assets.width = (List_assets.width-5) 
		table_assets.height = (List_assets.height-LA_h_footer)
		try btn_refresh.images = #("bip_copy1paste_i.bmp",undefined,10,9,9,9,9,false,true) catch ()
		try btn_close.images = #("SME_i.bmp",undefined,74,74,74,74,74,false,true) catch ()

		fn_GetAsset()
		fn_refreshlist()
	)
	on btn_close pressed do destroydialog List_assets
	local mouseIsDown = false, thePos = [0,0]
	on List_assets lbuttondown pos do
		(mouseIsDown = true ; thePos = pos)
	on List_assets lbuttonup pos do
		mouseIsDown = false
	on List_assets mousemove pos do 
		if mouseIsDown do 
		setDialogPos List_assets (mouse.screenpos - thePos)
	
	
	on btn_refresh pressed do
	(	fn_GetAsset(); fn_refreshlist())
	on chk_ok changed state do
	(	fn_refreshlist())
	on chk_found changed state do
	(	fn_refreshlist())
	on chk_missing changed state do
	(	fn_refreshlist())
	on btn_collect pressed do
	(	
		mapdest = getsavepath caption:"Choose a Project Folder."
		if mapdest != undefined do 
		(	
			fn_Collect()
			fn_GetAsset()
			fn_refreshlist()
			freeSceneBitmaps()--is good to use this function at the end 
			shellLaunch "explorer.exe" mapdest
		)
	)
	on btn_resolve pressed do
	(
		for i=1 to asset_list.count where Asset_list[i].filestatus == #Found do
		(
			fn_progress i asset_list.count
			asset_list[i].fileclass.name = asset_list[i].filename
			fn_Repath Asset_list[i] Asset_list[i].fileclass Asset_list[i].filepath
			if Asset_list[i].filestatus != #ok do 
			(
				asset_list[i].fileclass.name = asset_list[i].filename
				fn_Repath Asset_list[i] Asset_list[i].fileclass Asset_list[i].filepath
			)
		)
		fn_GetAsset()
		fn_refreshlist()
	)
	on btn_export pressed  do
	(
		try destroydialog textwindow catch ()
		rollout textwindow "Asset List" width:300 height:250
		(
			editText text1 "" pos:[-4,0] width:304 height:230 enabled:true readOnly:true
			button btn_copy "Copy to Clipboard" pos:[150,230] width:150 height:20
			button btn_save "Save" pos:[0,230] width:150 height:20
			on btn_save pressed do 
			( 	textlocation = getsavefilename caption:"Save texture list" filename:(maxfilepath + "texturelist") types:"text(*.txt)|*.txt" 
				if textlocation != undefined do
				(
					newTXT = createFile textlocation
					format text1.text to:newTXT
					close newTXT
				)
			)
			on btn_copy pressed do (setclipboardText textwindow.text1.text; messagebox "Text Copied" title:"Copy")
		)
		createDialog textwindow
		textwindow.text1.text = ""
		for i=0 to table_assets.Items.count-1 do
		(
			row = table_assets.Items.item[i]
			textwindow.text1.text += row.subItems.item[1].text+ row.subItems.item[0].text +"\n"
		)
	)
	on table_assets columnClick columnHeader do 
	(
		table_assets.ListViewItemSorter = dotnetobject "MXS_dotNet.ListViewItemComparer" columnHeader.column
		table_assets.ListViewItemSorter = undefined
	)
	on table_assets mouseDown arg do
	(
		if arg.button == (dotNetClass "System.Windows.Forms.MouseButtons").Left and arg.clicks == 1 do
		(	
			hit=(table_assets.HitTest (dotNetObject "System.Drawing.Point" arg.x arg.y))
			if currentimage != undefined do close currentimage
		)
		if arg.Button == (dotNetClass "System.Windows.Forms.MouseButtons").Left and arg.clicks == 2 do
		(
			hit=(table_assets.HitTest (dotNetObject "System.Drawing.Point" arg.x arg.y))
			if currentimage != undefined do close currentimage
			for i=1 to asset_list.count do
			(	
				if matchpattern asset_list[i].filename pattern:(getFilenameFile hit.item.subItems.item[0].text) then 
				(
					item = asset_list[i].filepath
					if item == "" do item = asset_list[i].foundpath
					if doesfileexist item do 
					(
						currentimage = openBitMap item
						display currentimage
					)
					exit
				)
			)
		)
		if arg.button == (dotNetClass "System.Windows.Forms.MouseButtons").right do
		(	
			hit=(table_assets.HitTest (dotNetObject "System.Drawing.Point" arg.x arg.y))
			hit.item.ListView.SelectedIndices.Clear()
			hit.item.Focused = true
			hit.item.Selected = true
			
			if currentimage != undefined do close currentimage
			ImageFile = hit.item.subItems.item[0].text
			RCMenu RCList
			(
				menuItem MI_View "View Image"
				menuItem MI_Folder "Open Folder"
				menuItem MI_Select "Select Object"
				menuItem MI_SendMat "Send Material to Material Editor"
				menuItem MI_SendBmp "Send Bitmap to Material Editor"
				
				on MI_View picked do
				(
					for i=1 to asset_list.count do
					(	
						if matchpattern asset_list[i].filename pattern:(getFilenameFile Imagefile) then 
						(
							imagepath = asset_list[i].filepath
							if imagepath == "" do imagepath = asset_list[i].foundpath
							if doesfileexist imagepath do 
							(
								currentimage = openBitMap imagepath
								display currentimage
							)
							exit
						)
					)
				)
				on MI_Folder picked do
				(
					for i=1 to asset_list.count do
					(	
						if matchpattern asset_list[i].filename pattern:(getFilenameFile Imagefile) then 
						(
							imagepath = asset_list[i].filepath
							if imagepath == "" do imagepath = asset_list[i].foundpath
							if doesfileexist imagepath do 
							(
								shellLaunch "explorer.exe" (getFilenamePath imagepath)
							)
							exit
						)
					)
				)
				on MI_Select picked do
				(
					clearselection()
					for i=1 to asset_list.count do
					(	
						if matchpattern asset_list[i].filename pattern:(getFilenameFile Imagefile) then 
						(
							bitmapimage = asset_list[i].fileclass
							select (refs.dependentNodes bitmapimage)
						)
					)
				)
				on MI_SendMat picked do
				(
					for i=1 to asset_list.count do
					(	
						if matchpattern asset_list[i].filename pattern:(getFilenameFile Imagefile) then 
						(
							bitmapimage = asset_list[i].fileclass
							matArr = #()
							for mtl in (refs.dependents bitmapimage) where superclassof mtl == material or superclassof mtl == textureMap do append matArr mtl
							
							SME.Open() 
							NodePos = [0,0] + [0,0]
							SelView = sme.GetView sme.ActiveView
							for i=1 to matArr.count do
							(	
								NodePos = [0,(i*100)]
								SelView.CreateNode matArr[i] &NodePos
							)
							SelView.ZoomExtents type:#all
						)
					)
					
				)
				on MI_SendBmp picked do
				(
					for i=1 to asset_list.count do
					(	
						if matchpattern asset_list[i].filename pattern:(getFilenameFile Imagefile) then 
						(
							SME.Open() 
							mtl = asset_list[i].fileclass
							NodePos = [0,0] + [0,0]
							SelView = sme.GetView sme.ActiveView
							NewNode = SelView.CreateNode mtl &NodePos
							try(
								SelView.SetSelectedNodes mtl
								sme.SetMtlInParamEditor mtl
								SelView.ZoomExtents type:#all
							)catch()
							exit
						)
					)
				)
			)
			popUpMenu RClist
		)
	)
	on List_assets close do
	(	
		try destroydialog textwindow catch()
		if currentimage != undefined do close currentimage
		sme.DeleteView (sme.GetViewByName "Bitmap") false
	)
)
createDialog List_assets width:550 height:250 style:#()
