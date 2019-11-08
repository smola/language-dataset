/*
3ds Max Batch Export/Import Tool
Author: josh_axey
Github: github.com/joshaxey
Version: 1.1.0

Based on the batch exporter/importer
script by Jos Balcaen which has been
decrypted, functionally improved.

To create a button for this script:
* Save the script to a sensible location.
* Execute the following code in the Maxscript editor:
		macroScript BatchExportImport
		category:"Custom"
		tooltip:"Batch Export/Import by josh_axey - Export / import multiple model files"
		buttonText:"EXP/IMP"
		(
			--Change the file location below to match where you have saved this script!
			fileIn @"C:\\BatchExportImport - josh_axey.ms"
		)
*/

global _data
__debug = true

struct Importer
(
	public
	function Constructor =
	(
		if (_data == undefined) do	_data = DataObject()
	),
	init = Constructor(),
	
	function MImportFile path bShowOptions:false=
	(
		importType = _data.optionImportType.Value()
		extension = getFilenameType path
		case extension of
		(
			".max": if bShowOptions then
			(
				mergeMAXFile path #prompt #promptDups #promptMtlDups #promptReparent
			)
			else
			(
				case importType of
				(
					/*
					 1:Import
					 2:Merge
					 3:Replace
					*/
					1: mergeMAXFile path
					-- We need to be able to rename.
					2: mergeMAXFile path #renameMtlDups #AutoRenameDups #useSceneMtlDups #rename
					3: mergeMAXFile path #deleteOldDups
				)
			)
			default: if bShowOptions then
			(
				importFile path
			)
			else
			(
				importFile path #noPrompt
				/*
				 Fix for import of multiple models with meshes
				 or components that have the same names,
				 otherwise, you just get a single useless import.
				*/
				$.name = getFilenameFile path
			)
		)
	)
)

struct Exporter
(
	public

	function Constructor =
	(
		if (_data == undefined) do	_data = DataObject()
	),
	init = Constructor(),
	visited_additional_objects = #(),

	function ResetObject = (),
	function ResetObjects = (),
	function DeleteTurbo = (),

	function InsertItemInArray array item =
	(
		c = array.count
		for i = 0 to c do
		(
			if (i!=c) do
			(
				array[c - (i-1)] = array[c - i]
			)
		)
		array[1] = item
		return (makeUniqueArray array)
	),

	function DeleteTurboSmoothOnObject obj =
	(
		if IsValidNode obj == false do return obj
		for m in obj.modifiers do
		(
			if classOf m == turboSmooth do deleteModifier obj m
		)
		return obj
	),
	
	fn getAllChildren obj =
	(
		all_children = #()
		for obj in obj.children do
		(
			append all_children obj
			all_children += (getAllChildren obj)
		)
		return all_children
	),

	function GetGroupMembers groupHead =
	(
		members = #()
		for c in (getAllChildren groupHead) do
		(
			if isGroupMember c do append members c
		)
		return members
	),

	function ResetGroup groupHead = 
	(
		if IsValidNode groupHead == false do return groupHead
		groupPivot = groupHead.pivot
		groupName = groupHead.name
		groupMembers = GetGroupMembers groupHead
		explodeGroup groupHead
		for i=1 to groupMembers.count do
		(
			groupMembers[i] = ResetObject groupMembers[i]
		)
		group groupMembers name:groupName
		groupHead = getNodeByName groupName exact:true ignoreCase:false all:false
		groupHead.pivot = groupPivot
		return groupHead
	),

	function ResetObject obj =
	(
		if IsValidNode obj == false do return obj
		if isGroupMember obj == true do return obj
		if isGroupHead obj == true do
		(
			return ResetGroup obj
		)
		originalPivot = obj.pivot
		originalName = obj.name
		originalWireColor = obj.wirecolor
		originalChildren = #(); for c in obj.children do append originalChildren c
		originalParent = obj.parent
		resetObj = Box()
		resetObj = convertTo resetObj PolyMeshObject
		resetObj.EditablePoly.attach obj resetObj
		resetObj.EditablePoly.SetSelection #Face #{1..6}
		resetObj.EditablePoly.delete #Face
		resetObj.name = originalName
		resetObj.wirecolor = originalWireColor

		for child in originalChildren do child.parent = resetObj

		resetObj.parent = originalParent
		if (_data.optionPivotToOrigin.Value() == false) do resetObj.pivot = originalPivot
		
		return resetObj
	),

	function MoveObjectToOrigin obj =
	(
		if IsValidNode obj == false do return obj
		obj.pos = [0,0,0]
		return obj
	),

	function RotateObject obj =
	(
		if IsValidNode obj == false do return obj
		rotArr = _data.optionRotateArr.Value()
		rotationValue = eulerangles rotArr[1] rotArr[2] rotArr[3]
		rotate obj rotationValue
		return obj
	),

	function ScaleObject obj =
	(
		if IsValidNode obj == false do return obj
		scaleValue = _data.optionScaleValue.Value()
		scale obj [scaleValue,scaleValue,scaleValue]
		return obj
	),

	function EditObject obj =
	(
		if IsValidNode obj == false do return obj
		if (_data.optionDeleteTurboSmooth.Value()) do (obj = DeleteTurboSmoothOnObject obj)
		if (_data.optionResetObj.Value()) do (obj = ResetObject obj)
		if (_data.optionMoveToOrigin.Value()) do (obj = MoveObjectToOrigin obj)
		if (_data.optionRotateObj.Value()) do (obj = RotateObject obj)
		if (_data.optionScaleObj.Value()) do (obj = ScaleObject obj)
		if (_data.optionResetObjAfter.Value()) do (obj = ResetObject obj)	
		return obj
	),

	function MatchPrefixOrPostfix s matchString numAfter:0 =
	(
		index = findString s matchString

		if ((index == (s.count - matchString.count + 1 - numAfter)) or (index == 1)) then
		(
			return true
		) else return false
	),

	function IsObjectFirstLOD obj =
	(
		bCombineLODs = _data.optionCombineLODs.Value()
		sLODString = _data.optionLODString.Value()
		if bCombineLODs do
		(
			matchString = substituteString sLODString "*" "0"
			if (MatchPrefixOrPostfix obj.name matchString) do return true
		)
		return false 
	),

	function GetBaseNameFromLOD obj =
	(
		sLODString = _data.optionLODString.Value()
		matchString = substituteString sLODString "*" "0"
		return substituteString obj.name matchString ""
	),

	function GetAdditionalObjects obj =
	(
		aditionalObjectArr = #()
		if IsValidNode obj == false do return aditionalObjectArr
		
		bCombineCollision = _data.optionCombineCollision.Value()
		sCollisionString = _data.optionCollisionString.Value()
		bCombineLODs = _data.optionCombineLODs.Value()
		sLODString = _data.optionLODString.Value()
		
		if bCombineCollision do
		(
			collisionNode = getNodeByName (sCollisionString + obj.name) exact:true ignoreCase:true all:false
			if collisionNode == undefined do (collisionNode = getNodeByName (obj.name + sCollisionString) exact:true ignoreCase:true all:false)
			if collisionNode != undefined do 
			(
				collisionNode.pivot = obj.pivot
				append aditionalObjectArr collisionNode
			)
		)

		if isGroupHead obj then
		(
			members = getGroupMembers obj
			
			setGroupOpen obj false
			explodeGroup obj
			clearSelection()
			aditionalObjectArr += members
		) 
		else
		(
			if (IsObjectFirstLOD obj) do
			(
				matchString = substituteString sLODString "*" "0"
				index = findString obj.name matchString
				start = index + sLODString.count - 1
				for i=1 to 10 do
				(
					lodName = replace obj.name start 1 (i as string)
					lodObject = getNodeByName lodName
					if lodObject == undefined then exit
					else (append aditionalObjectArr  lodObject)
				)
			)
		)

		join visited_additional_objects aditionalObjectArr
		return aditionalObjectArr
	),

	function isAdditionalObject obj = 
	(
		if (IsValidNode obj) == false do return false
		
		for o in visited_additional_objects do
		(
			if o == obj do return true
		)
		
		bCombineCollision = _data.optionCombineCollision.Value()
		sCollisionString = _data.optionCollisionString.Value()
		bCombineLODs = _data.optionCombineLODs.Value()
		sLODString = _data.optionLODString.Value()
		
		if (isGroupMember obj == true) do return true
		
		if bCombineCollision do
		(
			if (MatchPrefixOrPostfix obj.name sCollisionString) do return true
		)
		
		if bCombineLODs do
		(
			matchString = substituteString sLODString "*" ""

			if (MatchPrefixOrPostfix obj.name matchString numAfter:1) do
			(
				if (MatchPrefixOrPostfix obj.name (substituteString sLODString "*" "0")) do 
				(
					return false
				)
				return true
			)
		)
		return false
	),

	function CollapseGroups objectArr = 
	(
		newArr = #()
		for obj in objectArr do
		(
			if IsValidNode obj == false do continue
			if isGroupMember obj == true do continue
			if isGroupHead obj then
			(
				originalName = obj.name
				originalPivot = obj.pivot
				members = GetGroupMembers obj
				setGroupOpen obj false
				explodeGroup obj
				convertTo members[1] PolyMeshObject
				for i = 2 to members.count do
				(
					members[1].attach members[i] members[1]
				)
				members[1].name = originalName
				members[1].pivot = originalPivot
				append newArr members[1]

			) else append newArr obj
		)
		return newArr
	),

	function ExportObjects objectArr maxFile: =
	(
		outputPath = _data.optionOutputPath.Value(); outputPath = outputPath[1]
		extension = _data.optionFormat.Value(); extension = extension[1]
		bChangeName = _data.optionChangeName.Value()
		prefix = _data.optionPrefixName.Value()
		suffix = _data.optionSuffixName.Value()
		bExportOptions = _data.optionExportOptions.Value()
		bExportFBXPreset = _data.optionExportFBXUsePreset.Value()
		bMultipleMaxFiles = true; if maxFile == unsupplied then bMultipleMaxFiles = false
		bSeparate = _data.optionSeparate.Value()
		
		errors = #()
		successes = #()
		
		holdMaxFile()
		
		if (_data.optionCollapseGroups.Value()) do objectArr = CollapseGroups objectArr
		
		if bMultipleMaxFiles and bSeparate == false then
		(
			for i=1 to objectArr.count do
			(	
				if IsValidNode objectArr[i] == false do continue

				if (isGroupMember objectArr[i] == true) do continue

				objectArr[i] = EditObject objectArr[i]
				if objectArr[i] == undefined do continue
			)
			select objectArr
			
			if bChangeName do (maxFile = prefix + maxFile + suffix)
			filename = outputPath + "\\" + maxFile + "." + extension
			exportfile filename #noPrompt
		)

		else
		(
			for i=1 to objectArr.count do
			(	
				if IsValidNode objectArr[i] == false do continue
				
				if (isAdditionalObject objectArr[i] == true) do continue

				objectName = objectArr[i].name
				
				if (IsObjectFirstLOD objectArr[i]) do
				(
					objectName = GetBaseNameFromLOD objectArr[i]
				)
				if bChangeName do (objectName = prefix + objectName + suffix)

				if bMultipleMaxFiles then filename = outputPath + "\\" + maxFile + "_" + objectName + "." + extension
				else (filename = outputPath + "\\" + objectName + "." + extension)
				
				additionObjectArr = GetAdditionalObjects objectArr[i]

				objectArr[i] = EditObject objectArr[i]
				
				if IsValidNode objectArr[i] == true do 				
				(
					select objectArr[i]
				)

				for a=1 to additionObjectArr.count do
				(
					additionObjectArr[a] = EditObject additionObjectArr[a]
				)
				selectMore additionObjectArr

				if selection.count == 0 do
				(
					append errors ("Failed to export " + objectName)
				)
				
				modDate = undefined
				if (doesFileExist filename) do (modDate = getFileModDate filename)
				
				if extension == "max" then
				(
					saveNodes $ filename quiet:false 
				) 
				else 
				(
					if bExportFBXPreset do (FBXExporterSetParam "LoadExportPresetFile" _data.optionExportFBXPreset[1])
					
					if bExportOptions then 
					(
						exportFile filename selectedOnly:true
						bExportOptions = false
					)
					else 
					(
						exportFile filename #noPrompt selectedOnly:TRUE
					)
				)
				
				if ((doesFileExist filename) and (modDate != getFileModDate filename)) then (append successes ("Exported " + filename))
				else (append errors ("File not modified " + filename))
				
				join visited_additional_objects (getCurrentSelection()) 
				
			)

			fetchMaxFile quiet:true
			
			message = ""
			if successes.count > 0 do
			(
				message += "\n-------------\nSuccess\n-------------\n"
				for m in successes do
				(
					message += ("\n" + m)
				)
			)
			if errors.count > 0 do
			(
				message += "\n-------------\nErrors\n-------------\n"
				for m in errors do
				(
					message += ("\n" + m)
				)
			)
			
			messageBox message title:"Batch Export" beep:false
			
		)
	),

	function ResetObjects objectArr =
	(
		for i = 1 to objectArr.count do
		(
			objectArr[i] = ResetObject objectArr[i]
		)
		return objectArr
	),

	function DeleteTurboSmoothOnObjects objectArr =
	(	
		for i = 1 to objectArr.count do
		(
			objectArr[i] = DeleteTurboSmoothOnObject objectArr[i]
		)
		return objectArr
	),

	function DeleteTurboSmoothOnSelected =
	(
		if selection.count != 0 then
		(
			for obj in selection do DeleteTurboSmoothOnObject obj
		)
		else messageBox "No selected objects found."
	),

	function ResetObjectOnSelected =
	(
		if selection.count != 0 then
		(
			newSelection = #()
			for obj in selection do 
			(
				if (isValidNode obj) and (isGroupMember obj == false) do
				(
					r = ResetObject obj
					append newSelection r
				)
			)
			select newSelection
		)
		else messageBox "No selected objects found."
		
	),

	function MoveObjectToOriginOnSelected =
	(
		if selection.count != 0 then
		(
			for obj in selection do 
			(
				MoveObjectToOrigin obj
			)
		)
		else messageBox "No selected objects found."
	),

	function RotateObjectOnSelected =
	(
		if selection.count != 0 then
		(
			for obj in selection do 
			(
				RotateObject obj
			)
		)
		else messageBox "No selected objects found."
	),

	function ScaleObjectOnSelected =
	(
		if selection.count != 0 then
		(
			for obj in selection do 
			(
				ScaleObject obj
			)
		)
		else messageBox "No selected objects found."
	),

	function ExportSelectedObjects = 
	(
		selectedObjs = selection as array
		if selectedObjs.count != 0 then
		(
			ExportObjects selectedObjs
		)
		else messageBox "No selected objects found."
	),

	function ExportAllObjects =
	(
		allObjs = ($* as array)
		if allObjs.count != 0 then
		(
			ExportObjects allObjs
		)
		else messageBox "No objects found."
	)
)

struct Option
(
	public
	name,
	optionsFile,
	defaultSection,
	
	function Constructor =
	(
		optionsFile = (systemTools.getEnvVariable("APPDATA") + @"\Maxscript\BatchExportImportOptions.ini")
		defaultSection = "global"
		name = "undefined"
		
		if not (doesFileExist optionsFile) do
		(
			try
			(
				makeDir (systemTools.getEnvVariable("APPDATA") + @"\Maxscript\")
				s = createFile optionsFile
				close s
			) catch(print getCurrentException())
		)
		if not (doesFileExist optionsFile) do
		(
			s = "Please create this file manually\n" + optionsFile
			messageBox s title:"Failed to create file."
			throw "Failed to create file"
		)
		return true
	),

	init = Constructor(),
	
	function Exists = 
	(
		return hasINISetting optionsFile defaultSection name
	),

	function GetValue =
	(
		return (getINISetting optionsFile defaultSection name)
	),

	function SetValue value =
	(
		setINISetting optionsFile defaultSection name (value as string)
	),

	function Add value =
	(
		if not Exists() do 
		(
			SetValue (value as string)
		)
	),

	function GetBool =
	(
		return (GetValue() as booleanClass)
	),

	function ReplaceCharacter s chFrom chTo =
	(
		offset = 0
		s = s as string
		for i = 1 to s.count do
		(
			if s[i+offset] == chFrom then
			(
				s = replace s (i+offset) 1 chTo
				offset += (chTo.count)
			)
		)
		return s
	),

	function GetArray =
	(
		s = GetValue()
		s = ReplaceCharacter s @"\" @"\\"
		return (execute s)
	),

	function GetInt = 
	(
		return (GetValue() as integer)
	),

	function GetFloat =
	(
		return (GetValue() as float)
	)
)

struct DataObject
(
	public

	optionCommercial,
	optionSelectedTab,
	optionImportFiles,
	optionImportType,
	optionImportOptions,
	optionResetObj,
	optionResetObjAfter,
	optionPivotToOrigin,
	optionPivotToOriginAfter,
	optionDeleteTurboSmooth,
	optionMoveToOrigin,
	optionRotateObj,
	optionRotateArr,
	optionScaleObj,
	optionScaleValue,
	optionOutputPath,
	optionFormat,
	optionChangeName,
	optionPrefixName,
	optionSuffixName,
	optionCombineCollision,
	optionCollisionString,
	optionCombineLODs,
	optionLODString,
	optionCollapseGroups,
	optionExportOptions,
	optionExportFBXUsePreset,
	optionExportFBXPreset,
	optionSeparate,
	
	function Constructor =
	(
		struct OptionCommercialStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "ExportName";base.Add value)
		)
		optionCommercial = OptionCommercialStruct(); optionCommercial.Create false
		
		struct OptionSelectedTabStruct (
			base = Option(), SetValue = base.SetValue, Value = base.GetInt,
			function Create value = (base.name = "SelectedTab";base.Add value)
		)
		optionSelectedTab = OptionSelectedTabStruct(); optionSelectedTab.Create 0
		
		struct OptionImportFilesStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetArray,
			function Create value = (base.name = "ImportFiles";base.Add value)
		)
		optionImportFiles = OptionImportFilesStruct(); optionImportFiles.Create #()
		
		struct OptionImportTypeStruct (
			base = Option(), SetValue = base.SetValue, Value = base.GetInt,
			function Create value = (base.name = "ImportType";base.Add value)
		)
		optionImportType = OptionImportTypeStruct(); optionImportType.Create 1

		struct OptionImportOptionsStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "ExportOptions";base.Add value)
		)
		optionImportOptions = OptionImportOptionsStruct(); optionImportOptions.Create true
		
		struct OptionResetObjStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "ResetObj";base.Add value)
		)
		optionResetObj = OptionResetObjStruct(); optionResetObj.Create true
		
		struct OptionResetObjAfterStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "ResetObjAfter";base.Add value)
		)
		optionResetObjAfter = OptionResetObjAfterStruct(); optionResetObjAfter.Create false
		
		struct OptionPivotToOriginStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "PivotToOrigin";base.Add value)
		)
		optionPivotToOrigin = OptionPivotToOriginStruct(); optionPivotToOrigin.Create false
		
		struct OptionPivotToOriginAfterStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "PivotToOriginAfter";base.Add value)
		)
		optionPivotToOriginAfter = OptionPivotToOriginAfterStruct(); optionPivotToOriginAfter.Create false
		
		struct OptionDeleteTurboSmoothStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "DeleteTurboSmooth";base.Add value)
		)
		optionDeleteTurboSmooth = OptionDeleteTurboSmoothStruct(); optionDeleteTurboSmooth.Create true
		
		struct OptionMoveToOriginStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "MoveToOrigin";base.Add value)
		)
		optionMoveToOrigin = OptionMoveToOriginStruct(); optionMoveToOrigin.Create true
		
		struct OptionRotateStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "RotateObj";base.Add value)
		)
		optionRotateObj = OptionRotateStruct(); optionRotateObj.Create false
		
		struct OptionRotateArrStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetArray,
			function Create value = (base.name = "RotateArr";base.Add value)
		)
		optionRotateArr = OptionRotateArrStruct(); optionRotateArr.Create #(0,0,0)
		
		struct OptionScaleStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "ScaleObj";base.Add value)
		)
		optionScaleObj = OptionScaleStruct(); optionScaleObj.Create false
		
		struct OptionScaleValueStruct (
			base = Option(), SetValue = base.SetValue, Value = base.GetFloat,
			function Create value = (base.name = "ScaleValue";base.Add value)
		)
		optionScaleValue = OptionScaleValueStruct(); optionScaleValue.Create 1.0

		struct OptionOutputPathStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetArray,
			function Create value = (base.name = "OutputPath";base.Add value)
		)
		optionOutputPath = OptionOutputPathStruct(); optionOutputPath.Create #(GetDir #export)
		
		struct OptionFormatStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetArray,
			function Create value = (base.name = "Format";base.Add value)
		)
		optionFormat = OptionFormatStruct(); optionFormat.Create #("obj","fbx","3ds","max","ase","dwf","dwg","dxf","dae")
		
		struct OptionChangeNameStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "ChangeName";base.Add value)
		)
		optionChangeName = OptionChangeNameStruct(); optionChangeName.Create false
		
		struct OptionPrefixNameStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetValue,
			function Create value = (base.name = "PrefixName";base.Add value)
		)
		optionPrefixName = OptionPrefixNameStruct(); optionPrefixName.Create ""
		
		struct OptionSuffixNameStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetValue,
			function Create value = (base.name = "SuffixName";base.Add value)
		)
		optionSuffixName = OptionSuffixNameStruct(); optionSuffixName.Create ""
		
		struct OptionCollisionStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "CombineCollision";base.Add value)
		)
		optionCombineCollision = OptionCollisionStruct(); optionCombineCollision.Create true
		
		struct OptionCollisionPrefixStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetValue,
			function Create value = (base.name = "CollisionString";base.Add value)
		)
		optionCollisionString = OptionCollisionPrefixStruct(); optionCollisionString.Create "UCX_"
		
		struct OptionCombineLODStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "CombineLODs";base.Add value)
		)
		optionCombineLODs = OptionCombineLODStruct(); optionCombineLODs.Create true
		
		struct OptionLODPostfixStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetValue,
			function Create value = (base.name = "LODString";base.Add value)
		)
		optionLODString = OptionLODPostfixStruct(); optionLODString.Create "_LOD*"
		
		struct OptionCollapseGroupsStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "CollapseGroups";base.Add value)
		)
		optionCollapseGroups = OptionCollapseGroupsStruct(); optionCollapseGroups.Create true
		
		struct OptionExportOptionsStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "ExportOptions";base.Add value)
		)
		optionExportOptions = OptionExportOptionsStruct(); optionExportOptions.Create true

		struct OptionExportFBXPresetStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetArray,
			function Create value = (base.name = "FBXPreset";base.Add value)
		)
		optionExportFBXPreset = OptionExportFBXPresetStruct(); optionExportFBXPreset.Create #()
		
		struct OptionExportFBXUsePresetStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "UseFBXPreset";base.Add value)
		)
		optionExportFBXUsePreset = OptionExportFBXUsePresetStruct(); optionExportFBXUsePreset.Create false
		
		struct OptionSeparateStruct(
			base = Option(), SetValue = base.SetValue, Value = base.GetBool,
			function Create value = (base.name = "SeparateObjects";base.Add value)
		)
		optionSeparate = OptionSeparateStruct(); optionSeparate.Create true
		
		return true
	),

	init = Constructor()
)

struct ImportRollout
(
	public
	roll,
	
	function Constructor =
	(
		if (_data == undefined) do	_data = DataObject()
		
		rollout roll "Import"
		(
			button btnBrowseFile "Browse for files" width:250
			multiListbox mlbImportFiles height:18 width:250 align:#center tooltip:"Double click to delete a file" items:(_data.optionImportFiles.Value())
			button btnSelectedFiles "No files to import" align:#left across:2 offset:[0,-5] height:20 border:false
			button btnClear "Clear" align:#right height:20 offset:[0,-5] border:false
			radioButtons rbtImportType labels:#("import","merge","replace") default:(_data.optionImportType.Value()) \
			tooltip:#("Import non-native files into 3ds Max","Insert objects from external 3ds Max files into the current scene",\
				"Replaces objects in the current 3ds Max scene with objects from a exernal scene")
			checkbox cbxImportOptions "Import options" checked:(_data.optionImportOptions.Value())
			button btnImportSelection "Import Selection" width:204 height:25 across:2 offset:[35,0]
			button btnImportAll "All" width:50 height:25 offset:[43,0]

			function updateSelectedButton = 
			(
				if (mlbImportFiles.items.count == 0) do
				(
					btnSelectedFiles.text = "No files to import"
					return undefined
				)

				bitArr = mlbImportFiles.selection  
				number_of_selected_files = 0
				for i = 1 to bitArr.count do
				(
					if bitArr[i] == true do number_of_selected_files+=1
				)
				if number_of_selected_files == 0 then
				(
					btnSelectedFiles.text = "All files"
				)
				else
				(
					number_of_files = mlbImportFiles.items.count
					btnSelectedFiles.text = (number_of_selected_files as string) + "/" + number_of_files as string + " files selected"
				)
			)

			function isNothingSelected =
			(
				bitArr = mlbImportFiles.selection
				for i = 1 to bitArr.count do
				(
					if bitArr[i] == true do 
					(
						return false
						exit
					)
				)
				return true
			)

			function import all =
			(
				bitArr = mlbImportFiles.selection
				nothingSelected = isNothingSelected()
				bShowOptions = _data.optionImportOptions.Value()
				i = Importer()
				
				disableSceneRedraw()
				
				for i_files = 1 to (mlbImportFiles.items.count) do
				(
					item = mlbImportFiles.items[i_files]
					if item != undefined do
					(
						if nothingSelected or bitArr[i_files] == true or all do
						(
							fileName = item as string
							i.MImportFile fileName bShowOptions:bShowOptions
							if bShowOptions do bShowOptions = false
						)
					)
				)
				enableSceneRedraw()
				redrawViews()
			)
			
			on btnBrowseFile pressed do
			(
				browse_dialog = dotNetObject "System.Windows.Forms.OpenFileDialog"
				browse_dialog.title = "PLEASE Select One Or More Files"
				browse_dialog.Multiselect = true
				browse_dialog.Filter = "OBJ Files (*.obj)|*.obj|All Files (*.*)|*.*"
				browse_dialog.FilterIndex = 2
				result = browse_dialog.showDialog()
				if (result.Equals result.OK) do 
				(
					itemArr = mlbImportFiles.items
					join itemArr browse_dialog.fileNames
					mlbImportFiles.items = itemArr
					_data.optionImportFiles.SetValue itemArr
					updateSelectedButton()
				)
			)
			on mlbImportFiles doubleClicked index do
			(
				mlbImportFiles.items = deleteItem mlbImportFiles.items index
				_data.optionImportFiles.SetValue (mlbImportFiles.items)
				updateSelectedButton()
			)
			on mlbImportFiles selected index do (updateSelectedButton())
			on btnSelectedFiles pressed do (mlbImportFiles.selection = #{};	updateSelectedButton())
			on btnClear pressed do (
				mlbImportFiles.items = #()
				_data.optionImportFiles.SetValue #()
				updateSelectedButton()
			)
			on rbtImportType changed state do (_data.optionImportType.SetValue state)
			on cbxImportOptions changed state do (_data.optionImportOptions.SetValue state)
			on btnImportSelection pressed do (import false)
			on btnImportAll pressed do (import true)
		)
		
		return true
	),

	init = Constructor()
)

struct ExportObjectsRollout
(
	public
	roll,
	rollAdvancedOptions,
	
	function Constructor =
	(
		if (_data == undefined) do	_data = DataObject()
		
		rollout roll "Export Settings"
		(
			checkbox cbxDeleteTurbo offset:[-5,0] across:3 checked:(_data.optionDeleteTurboSmooth.Value()) \
				tooltip:"Delete turboSmooth modifiers on selected objects when exporting \n(will be undone after export)"
			button btnDeleteTurbo "Now" width:50 tooltip:"Delete turboSmooth modifiers on selected objects" offset:[-80,-3]
			label lblInfo2 "Delete turboSmooth" offset:[-85,0] align:#left
				
			checkbox cbxReset offset:[-5,0] across:4 checked:(_data.optionResetObj.Value()) \
				tooltip:"Reset geometry when exporting \n(will be undone after export)"
			button btnReset "Now" width:50 tooltip:"reset geometry now" offset:[-50,-3]
			label lblInfo "Reset geometry (" offset:[-45,0] align:#left
			checkbox chbResetPivot "Pivot)" checked:(_data.optionPivotToOrigin.Value()) offset:[-20,0] \
				tooltip:"Unchecked will the pivot stay at the same location"
			
			checkbox cbxMoveToOrigin across:3 offset:[-5,0] checked:(_data.optionMoveToOrigin.Value())
			button btnMoveToOrigin "Now" width:50 offset:[-80,-3]
			label lblMoveToOrigin "Move objects to [0,0,0]" offset:[-85,0] align:#left
			checkbox chbRotateObj across:5 offset:[-5,0] checked:(_data.optionRotateObj.Value())
			button btnRotateObj "Now" width:50 offset:[-32,-3]
			spinner spnRotateX "Rotate   " range:[-360,360,(_data.optionRotateArr.Value())[1]] type:#integer fieldwidth:30 offset:[14,0]
			spinner spnRotateY range:[-360,360,(_data.optionRotateArr.Value())[2]] type:#integer fieldwidth:30 offset:[12,0]
			spinner spnRotateZ range:[-360,360,(_data.optionRotateArr.Value())[3]] type:#integer fieldwidth:30 offset:[10,0]
			checkbox chbScaleObj across:3 offset:[-5,0] checked:(_data.optionScaleObj.Value())
			button btnScaleObj "Now" width:50 offset:[-80,-3]
			spinner spnScale "Scale     " range:[0.001,1000,(_data.optionScaleValue.Value())] type:#float fieldwidth:35 align:#left offset:[-85,0]
			
			checkbox cbxResetAfter offset:[-5,0] across:4 checked:(_data.optionResetObjAfter.Value()) \
				tooltip:"Reset geometry when exporting \n(will be undone after export)"
			button btnResetAfter "Now" width:50 tooltip:"reset geometry now" offset:[-50,-3]
			label lblInfo3 "Reset geometry after (" offset:[-45,0] align:#left
			checkbox chbResetPivotAfter "Pivot)" checked:(_data.optionPivotToOriginAfter.Value()) offset:[6,0] \
				tooltip:"Unchecked will the pivot stay at the same location"
				
			dropdownlist ddlOutputpath width:230 height:20 items:(_data.optionOutputPath.Value()) tooltip:"" across:2 offset:[-5,0]
			button btnGetFolder "..." width:25 height:20 tooltip:"Browse for folder" offset:[60,0]
			label lblformat "Format" across:2 align:#left
			dropdownlist ddlFormat items:(_data.optionFormat.Value()) selection:1 width:50 align:#left offset:[-80,-3]
			
			subRollout subRollAdvancedOptions width:275 height:140 align:#center
			button btnExportSelection "Export Selection" width:204 height:25 across:2 offset:[35,0]
			button btnExportAll "All" width:50 height:25 offset:[43,0]

			on cbxReset changed state do (_data.optionResetObj.SetValue state)
			on chbResetPivot changed state do (_data.optionPivotToOriginAfter.SetValue state)
			on cbxResetAfter changed state do (_data.optionResetObjAfter.SetValue state)
			on chbResetPivotAfter changed state do (_data.optionPivotToOriginAfter.SetValue state)
			on btnReset pressed do
			(
				e = Exporter()
				undo "Reset selected objects" on (e.ResetObjectOnSelected())
			)
			on btnResetAfter pressed do
			(
				e = Exporter()
				undo "Reset selected objects" on (e.ResetObjectOnSelected())
			)
			on cbxDeleteTurbo changed state do (_data.optionDeleteTurboSmooth.SetValue state)
			on btnDeleteTurbo pressed do
			(
				e = Exporter()
				undo "Delete turboSmooth modifiers on selected" on (e.DeleteTurboSmoothOnSelected())
			)
			on cbxMoveToOrigin changed state do (_data.optionMoveToOrigin.SetValue state)
			on btnMoveToOrigin pressed do
			(
				e = Exporter()
				undo "Move selected to origin" on (e.MoveObjectToOriginOnSelected())
			)
			on btnRotateObj pressed do
			(
				e = Exporter()
				undo "Rotate selected" on (e.RotateObjectOnSelected())
			)
			on btnScaleObj pressed do
			(
				e = Exporter()
				undo "Scale selected" on (e.ScaleObjectOnSelected())
			)
			on btnGetFolder pressed do
			(
				OutputArr = ddlOutputpath.items
				savePath = (getSavePath "Save to:" initialDir:(@"C:\Users\"+sysInfo.username+"\Desktop")) as string
				if (savePath != "undefined") do
				(
					e = Exporter()
					OutputArr = e.InsertItemInArray OutputArr savePath

					if (OutputArr.count > 10) do
					(
						newArr = #()
						for i = 1 to 10 do (newArr[i] = OutputArr[i])
						OutputArr = newArr
					)
					ddlOutputpath.items = OutputArr
					ddlOutputpath.selection = 1
					ddlOutputpath.tooltip = ddlOutputpath.items[1] as string
					_data.optionOutputPath.SetValue OutputArr
				)
			)
			on ddlOutputpath selected arg do
			(
				arr = ddlOutputpath.items
				ddlOutputpath.tooltip = arr[arg] as string 
				sel = arr[arg]
				e = Exporter()
				arr = e.InsertItemInArray arr sel
				
				ddlOutputpath.selection = 1
				ddlOutputpath.items = arr
				_data.optionOutputPath.SetValue arr
			)
			on ddlFormat selected arg do
			(
				arr = ddlFormat.items
				sel = arr[arg]
				e = Exporter()
				arr = e.InsertItemInArray arr sel
				
				ddlFormat.selection = 1
				ddlFormat.items = arr
				_data.optionFormat.SetValue arr
			)
			on chbRotateObj changed state do (_data.optionRotateObj.SetValue state)
			on spnRotateX changed value do (arr=_data.optionRotateArr.Value();arr[1]=value;_data.optionRotateArr.SetValue arr)
			on spnRotateY changed value do (arr=_data.optionRotateArr.Value();arr[2]=value;_data.optionRotateArr.SetValue arr)
			on spnRotateZ changed value do (arr=_data.optionRotateArr.Value();arr[3]=value;_data.optionRotateArr.SetValue arr)
			on chbScaleObj changed state do (_data.optionScaleObj.SetValue state)
			on spnScale changed value do (_data.optionScaleValue.SetValue value)
			on btnExportSelection pressed do (e = Exporter(); e.ExportSelectedObjects())
			on btnExportAll pressed do (e = Exporter();	e.ExportAllObjects())
		)
		
		rollout rollAdvancedOptions "Advanced"
		(
			checkbox cbxChangeName "Enable name change" checked:(_data.optionChangeName.Value()) offset:[-5,0]
					tooltip:"change exportname, example:\nhouse_+scene name+_v2" 
			edittext edtPrefix width:75 text:(_data.optionPrefixName.Value()) across:3 offset:[10,0]
			label lblObjectName "+ obj name +" offset:[4,0]
			edittext edtSuffix width:75 text:(_data.optionSuffixName.Value())
			checkbox cbxExportCollision "Combine collisionmesh, string:" offset:[-5,0] checked:(_data.optionCombineCollision.Value()) across:2
				tooltip:"objects with name x and prefix_x/postfix_x in selection will be exported together"
			edittext edtCollisionPrefix width:50 text:(_data.optionCollisionString.Value()) offset:[35,0]
			checkbox chbCombineLODs "Combine LODs, string:" offset:[-5,0] checked:(_data.optionCombineLODs.Value()) across:2 \
				tooltip:"Export meshes with given prefix/postfix (* is numbering) to the same file"
			edittext edtLODPostfix width:60 text:(_data.optionLODString.Value()) offset:[7,0]
			checkbox cbxCollapseGroups "Collapse groups" offset:[-5,0]
				tooltip:"Convert group to editable poly before exporting.\n(will be undone after export)" checked:(_data.optionCollapseGroups.Value())
			checkbox cbxExportOptions "Export options" offset:[-5,0]
				tooltip:"Display export dialog" checked:(_data.optionExportOptions.Value())
			checkbox cbxFBXPreset "FBX Preset" offset:[-5,0]
				tooltip:"FBX Preset File" checked:(_data.optionExportFBXUsePreset.Value()) across:3
			dropdownlist ddlPreset width:150 height:20 items:(_data.optionExportFBXPreset.Value()) 
				tooltip:"Preset file" offset:[-8,0]
			button btnGetPreset "..." width:25 height:20 tooltip:"Browse for preset" offset:[40,0]
			
			on cbxChangeName changed state do (_data.optionChangeName.SetValue state)
			on edtPrefix changed text do (_data.optionPrefixName.SetValue text)
			on edtSuffix changed text do (_data.optionSuffixName.SetValue text)
			on cbxExportCollision changed state do (_data.optionCombineCollision.SetValue state)
			on edtCollisionPrefix changed text do (_data.optionCollisionString.SetValue text)
			on chbCombineLODs changed state do (_data.optionCombineLODs.SetValue state)
			on edtLODPostfix changed text do (_data.optionLODString.SetValue text)
			on cbxCollapseGroups changed state do (_data.optionCollapseGroups.SetValue state)
			on cbxExportOptions changed state do (_data.optionExportOptions.SetValue state)
			on chbFBXPreset changed state do (_data.optionExportFBXUsePreset.SetValue state)
			on btnGetPreset pressed do
			(
				browse_dialog = dotNetObject "System.Windows.Forms.OpenFileDialog"
				browse_dialog.title = "Please select preset"
				browse_dialog.Multiselect = false
				browse_dialog.Filter = "FBX Preset Files (*.fbxexportpreset)|*.fbxexportpreset|All Files (*.*)|*.*"
				browse_dialog.FilterIndex = 1
				browse_dialog.InitialDirectory = "C:\Users\josh_axey\Documents\3dsMax\FBX\3dsMax2014_X64\Presets\2014.0.1\export\BatchExport.fbxexportpreset"
				result = browse_dialog.showDialog()
				
				if (result.Equals result.OK) do 
				(
					preset_arr = ddlPreset.items
					
					e = Exporter()
					preset_arr = e.InsertItemInArray preset_arr browse_dialog.fileNames[1]

					if (preset_arr.count > 10) do
					(
						newArr = #()
						for i = 1 to 10 do (newArr[i] = preset_arr[i])
						preset_arr = newArr
					)
					ddlPreset.items = preset_arr
					ddlPreset.selection = 1
					ddlPreset.tooltip = ddlPreset.items[1] as string
					_data.optionExportFBXPreset.SetValue preset_arr
					
				)
			)
			
			on ddlPreset selected arg do
			(
				arr = ddlPreset.items
				ddlPreset.tooltip = arr[arg] as string 
				sel = arr[arg]
				e = Exporter()
				arr = e.InsertItemInArray arr sel
				ddlPreset.selection = 1
				ddlPreset.items = arr
				_data.optionExportFBXPreset.SetValue arr
			)
		)
		return true
	),
	init = Constructor()
)

struct ExportFilesRollout
(
	public
	roll,
	
	function Constructor = 
	(
		if (_data == undefined) do	_data = DataObject()
		
		rollout roll "Export Multiple Max Files"
		(
			button btnBrowseFile "Browse for files" width: 250
			multiListbox mlbMaxFiles height:14 width:250 align:#center tooltip:"Double click to delete a file"
			button btnSelectedFiles "No files to export" align:#left across:2 offset:[0,-5] height:20 border:false
			button btnClear "Clear" align:#right height:20 offset:[0,-5] border:false
			checkbox cbxSeparate "Separate objects" offset:[-5,0]
				tooltip:"Export all objects to separate files" checked:(_data.optionSeparate.Value())
			button btnExportSelection "Export Selection" width:204 height:25 across:2 offset:[35,0]
			button btnExportAll "All" width:50 height:25 offset:[43,0]
			
			function updateSelectedButton = 
			(
				if (mlbMaxFiles.items.count == 0) do
				(
					btnSelectedFiles.text = "No files to export"
					return undefined
				)
				bitArr = mlbMaxFiles.selection  
				number_of_selected_files = 0
				for i = 1 to bitArr.count do
				(
					if bitArr[i] == true do number_of_selected_files+=1
				)
				if number_of_selected_files == 0 then (btnSelectedFiles.text = "All files")
				else
				(
					number_of_files = mlbMaxFiles.items.count
					btnSelectedFiles.text = (number_of_selected_files as string) + "/" + number_of_files as string + " files selected"
				)
			)

			function exportFiles onlySelected: =
			(
				bitArr = mlbMaxFiles.selection  
				e = Exporter()
				for i_files = 1 to (mlbMaxFiles.items.count) do
				(
					if onlySelected == unsupplied or bitArr[i_files] == true or btnSelectedFiles.text == "All files" do
					(
						loadMaxFile (mlbMaxFiles.items[i_files] as string) quiet:true
						select $*
						selectionList = getCurrentSelection()
		
						filename = getFilenameFile (mlbMaxFiles.items[i_files] as string)
						
						e.ExportObjects selectionList maxFile:filename
					)
				)
			)
			
			on btnBrowseFile pressed do
			(
				browse_dialog = dotNetObject "System.Windows.Forms.OpenFileDialog"
				browse_dialog.title = "Please Select One Or More Files"
				browse_dialog.Multiselect = true
				browse_dialog.Filter = "MAX Files (*.max)|*.max"
				browse_dialog.FilterIndex = 1
				result = browse_dialog.showDialog()
				if (result.Equals result.OK) do
				(
					itemArr = mlbMaxFiles.items
					join itemArr browse_dialog.fileNames
					mlbMaxFiles.items = itemArr
					updateSelectedButton()
				)
			)
			on mlbMaxFiles doubleClicked index do
			(
				mlbMaxFiles.items = deleteItem mlbMaxFiles.items index
				updateSelectedButton()
			)
			on mlbMaxFiles selected index do (updateSelectedButton())
			on btnSelectedFiles pressed do (mlbMaxFiles.selection = #{};updateSelectedButton())
			on btnClear pressed do (mlbMaxFiles.items = #();updateSelectedButton())
			on cbxSeparate changed state do (_data.optionSeparate.SetValue state)
			on btnExportSelection pressed do (exportFiles onlySelected:true)
			on btnExportAll pressed do (exportFiles())
		)
	
		return true
	),
	
	init = Constructor()
)

struct InfoRollout
(
	public
	roll,
	
	function Constructor =
	(
		rollout roll "Info"
		(
			dotNetControl lblInfo "System.Windows.Forms.Label" height:53 width:250 align:#center
			hyperLink lblLink "" address:"" color:(color 200 128 50) visitedColor:(color 200 128 50) across:2
			hyperLink lblLinkMail "" address:"" color:(color 200 128 50) visitedColor:(color 200 128 50)
			
			on roll open do
			(
				lblInfo.text = "Batch Export/Import Tool is a script to import, modify (or not) and export multiple objects at once."
				lblInfo.backcolor = lblInfo.backcolor.fromARGB 70 70 70
				lblInfo.forecolor = lblInfo.forecolor.fromARGB 200 200 200
			)
		)
		return true
	),
	init = Constructor()
	
	
)

struct LicenseRollout
(
	public
	roll,
	
	
	function CreateNonCommercial =
	(
		rollout roll "License"
		(
			hyperLink lblLink "http://creativecommons.org/licenses/by-nc-sa/4.0/" address:"http://creativecommons.org/licenses/by-nc-sa/4.0/" color:(color 200 128 50) visitedColor:(color 200 128 50)
			dotNetControl lblLicense "System.Windows.Forms.Label" height:110 width:250 align:#center
			
			on roll open do
			(
				lblLicense.text = "This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
	http://creativecommons.org/licenses/by-nc-sa/4.0/"
				lblLicense.backcolor = lblLicense.backcolor.fromARGB 70 70 70
				lblLicense.forecolor = lblLicense.forecolor.fromARGB 200 200 200
			)
		)
		
		return true
	),
	function CreateCommercial =
	(
		rollout roll "License"
		(
			hyperLink lblLink "http://creativecommons.org/licenses/by-sa/4.0/" address:"http://creativecommons.org/licenses/by-sa/4.0/" color:(color 200 128 50) visitedColor:(color 200 128 50)
			dotNetControl lblLicense "System.Windows.Forms.Label" height:110 width:250 align:#center
			
			on roll open do
			(
				lblLicense.text = "This work is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License.
	http://creativecommons.org/licenses/by-sa/4.0/"
				lblLicense.backcolor = lblLicense.backcolor.fromARGB 70 70 70
				lblLicense.forecolor = lblLicense.forecolor.fromARGB 200 200 200
			)
		)
		
		return true
	),

	function Constructor =
	(
		if (_data == undefined) do	_data = DataObject()
		if _data.optionCommercial.Value() then CreateCommercial()
		else CreateNonCommercial()
		
		return true
	),
	init = Constructor()
)

struct CommercialLicenseRollout
(
	public
	roll,
	
	function Constructor =
	(
		rollout roll "License"
		(
			hyperLink lblLink "http://creativecommons.org/licenses/by-sa/4.0/" address:"http://creativecommons.org/licenses/by-sa/4.0/" color:(color 200 128 50) visitedColor:(color 200 128 50)
			dotNetControl lblLicense "System.Windows.Forms.Label" height:110 width:250 align:#center
			
			on roll open do
			(
			
				lblLicense.text = "This work is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License.
	http://creativecommons.org/licenses/by-sa/4.0/"
				lblLicense.backcolor = lblLicense.backcolor.fromARGB 70 70 70
				lblLicense.forecolor = lblLicense.forecolor.fromARGB 200 200 200
			)
		)
		
		return true
	),
	init = Constructor()
)

struct BatchExportImport
(
	public
	
	rollBatchExportImport,
	
	function Constructor =
	(
		if (_data == undefined) do _data = DataObject()
		
		rollout rollBatchExportImport "Batch Export/Import Tool"
		(
			local import = ImportRollout()
			local export = ExportObjectsRollout()
			local exportFiles = ExportFilesRollout()
			local info = InfoRollout()
			local license = LicenseRollout()
			
			local rollArr = #(
					#("          Import          ",#(import.roll)),
					#("          Export          ",#(export.roll,exportFiles.roll)),
					#("  Info  ",#(info.roll, license.roll))
				)
			
			local lastSubRollout = 1
			
			dotNetControl dnTabs "System.Windows.Forms.TabControl" height:20 width:420 align:#left
			subRollout theSubRollout width:280 height:390 align:#center
			
			on dnTabs Selected itm do
			(
				for subroll in rollArr[lastSubRollout][2] do
				removeSubRollout theSubRollout subroll
				
				lastSubRollout = itm.TabPageIndex+1
				for subroll in rollArr[lastSubRollout][2] do	
				addSubRollout theSubRollout subroll
				
				addSubRollout export.roll.subRollAdvancedOptions export.rollAdvancedOptions rolledUp:false
			
				_data.optionSelectedTab.SetValue itm.TabPageIndex
				
			)
			
			on rollBatchExportImport open do
			(
				for aTab in rollArr do
				(
					dnTabs.TabPages.add aTab[1]
				)
				
				if _data.optionSelectedTab.Value() == 0 then
				(
					for subroll in rollArr[1][2] do	
					addSubRollout theSubRollout subroll
				) else dnTabs.SelectTab (_data.optionSelectedTab.Value())
				
			)
		)
		return true
	),

	function Show =
	(
		createDialog rollBatchExportImport 280 425 style:#(#style_toolwindow,#style_sysmenu)

		titleLabel = "Batch Export/Import Tool"
		if (_data.optionCommercial.value()) then titleLabel += " (Commercial)"
		else titleLabel += " (Non Commercial)"

		rollBatchExportImport.title = titleLabel
	),

	init = Constructor()
)

w = BatchExportImport()
w.Show()
