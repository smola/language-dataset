-- lib/ms.ms

clearlistener() 

-- lib/utils.ms

struct converter (
  fn arrayValuesToString valueArray =
  (
    if valueArray.count > 1 then
    (
      str = valueArray[1] as string
      for i = 2 to valueArray.count do
        str +=  "," + (valueArray[i] as string)
      result = "(" + str + ")"
    )
    else
      result = valueArray[1] as string
  ),
  fn stringToPoint2 str = 
  (
    elements = filterstring str "[,]"
	if elements.count > 0 then
	(
	  try
      (	  
        first = elements[1] as integer
        second = elements[2] as integer
	  ) 
	  catch 
	  (
	    messageBox "Error. Can't convert: " + str + " to Point2 value"
	  )
      pointTwo = [first, second]
	)
	else
	(
	  --messageBox "Error. Can't convert: " + str + " to Point2 value"
	  [0, 0]
	)	  
  ),
  fn stringToInteger str = 
  (
    try
    (	  
      str as integer
    )
    catch 
	(
	  --messageBox "Error. Can't convert: " + str + " to integer value"
	  -1
	)
  ),
  fn stringToFloat str = 
  (
    try
    (	  
      str as float
	)  
    catch 
	(
	  --messageBox "Error. Can't convert: " + str + " to float value"
	  -1
	)	
  ), 
  fn stringToBoolean str = 
  (
    try
    (	  
      str as boolean
	)  
    catch 
	(
	  --messageBox "Error. Can't convert: " + str + " to boolean value"
	  -1
	)	
  ),   
  fn stringToArray str = 
  (
    array = #()
    elements = filterstring str "(,)"
	if elements.count > 0 then
	  for element in elements do
        append array (element as name)
   	array  
  )
)

struct fileUtils
(
  fn isFileExist filename =
  (
    result = doesFileExist filename
  ), 
  fn getFileExtenion filename =
  (
    result = getFilenameType filename
  ),
  fn getCurrentPath filename =
  (
    result = getFilenamePath filename
  ) 
)

fileUtils = fileUtils()
converter = converter()

-- lib/ini.ms


struct iniFile
(
  filename,
  fn setValue section key value =
  (
    setINISetting filename section key (value as string)
  ),
  fn getValue section key =
  (
    getINISetting filename section key
  ),
  fn getIntegerValue section key =
  (
    strValue = getINISetting filename section key
	if strValue != "" then
      converter.stringToInteger(strValue)
	else
      result = -1	
  ),
   fn getBooleanValue section key =
  (
    strValue = getINISetting filename section key
	if strValue != "" then
      converter.stringToBoolean(strValue)
	else
      result = undefined	
  ), 
  fn getFloatValue section key =
  (
    strValue = getINISetting filename section key
	if strValue != "" then   
      converter.stringToFloat(strValue)
	else
      result = -1		  
  ),
  fn getArrayValue section key =
  (
    strValue = getINISetting filename section key
	if strValue != "" then     
      converter.stringToArray(strValue)
	else
      result = -1		   
  ),
  fn getPoint2Value section key =
  (
    strValue = getINISetting filename section key
	if strValue != "" then      
      converter.stringToPoint2 (strValue)
	else
      result = [0, 0]	  
  ),   
  fn isKeyExist section key =
  (
    hasINISetting filename section key      	  
  )
)

ini = iniFile()

-- lib/max.ms


struct maxStrings 
(
  lang = "Eng",
  errorVertNumber = "Error. Number of vertexes of the mesh is less than %d.",
  fn loadFromIni filename =
  (
    local strings = settings(this)
    strings.loadFromIniFile filename lang
  )
)

struct max3ds
(
  strings,
  VisualStyle = #Shaded,
  fn getBoundPoints mesh =
  (
    boundPoint = #()
    append boundPoint mesh.max
	append boundPoint mesh.min
	boundPoint
  ),
  fn init =
  (
    if strings == undefined then
      strings = maxStrings() 
	-- set viewport mod
    VP = NitrousGraphicsManager.GetActiveViewportSetting()
    VP.VisualStyleMode = VisualStyle
  ), 
  fn getMeshDimension msh =
  (
    meshDimensions = msh.max - msh.min
  ),
  fn getMeshVertCount msh =
  (
    result = getNumVerts msh 
  ),
  fn getMeshVerts mesh =
  (
    vertArray = #()
    for i = 1 to mesh.numVerts do 
      append vertArray (getVert mesh i)
  	result = vertArray
  ),
  fn createPlaneMesh size pos name =
  (
    vertArray = #([0, 0, 0], [size.x, 0, 0], [0, size.y, 0], [size.x, size.y, 0])
    faceArray = #([1, 2, 3], [2, 4, 3])
    idArray = #(1, 1)
    tvertArray = #([0, 0, 0], [1, 0, 0], [0, 1, 0], [1, 1, 0]) 
    msh = max.createTexturedMesh vertArray faceArray idArray tvertArray name
    if msh != undefined then
    (
      msh.pos = pos
      result = msh
    )
  ),  
  fn createMesh vertArray faceArray name =
  (
    try
	(
      msh = mesh vertices:vertArray faces:faceArray
      if name != unsupplied then
        msh.name = name 
      result = msh		
	)
	catch 
	(
	  messageBox ("ERROR. Can't create mesh: " + name + ".")
    )  	
  ), 
  fn createTexturedMesh vertArray faceArray idArray tvertArray name =
  (
    try
	(        
      if idArray == unsupplied then
        msh = mesh vertices:vertArray faces:faceArray tverts:tvertArray
      else      
        msh = mesh vertices:vertArray faces:faceArray materialIDS:idArray \
          tverts:tvertArray
    ) 
	catch 
	(
	  messageBox ("ERROR. Can't create mesh: " + name + ".")
      return undefined
    )      
    buildTVFaces msh.mesh
    for i = 1 to msh.mesh.numfaces do 
      (setTVFace msh.mesh i (getFace msh.mesh i)) 
    if name != unsupplied then      
      msh.name = name 
    result = msh			
  ),
  fn cloneMesh msh pos nodes =
  (
    maxOps.CloneNodes msh offset:pos expandHierarchy:false cloneType:#copy \
      newNodes:&nodes 
  ),
  fn moveMesh msh pos = 
  (
    move msh pos
  ),
  fn rotateMesh msh angles =
  (
    rotate msh angles
  ),
  fn copyMesh msh =
  (
    mshCopy = copy msh
	mshCopy
  ),
  fn attachMesh srcMesh dstMesh =
  (
    meshop.attach srcMesh dstMesh
  ),
  fn sliceMesh msh planeNormal planePos =
  (
  	local offset = dot planeNormal (planePos - (msh.pos * inverse msh.transform * msh.objecttransform))
  	meshop.slice msh #all planeNormal offset delete:true
  	update msh
  ),
  fn createBitmapMaterial bitmapImage =
  (
    local mat = standardMaterial diffuseMap:(bitmaptexture bitmap:bitmapImage) \
      showInViewport:true
  ),
  fn createBitmapBlendMaterial bitmapImageArray =
  (  
    blendmat = blend
    blendmat.map1 = standardMaterial \
      diffuseMap:(bitmaptexture bitmap:bitmapImageArray[1]) showInViewport:true
    blendmat.map2 = standardMaterial \
      diffuseMap:(bitmaptexture bitmap:bitmapImageArray[2]) showInViewport:true 
    blendmat.mask = gradient  
    result = blendmat    
  ),  
  fn createBitmapMultiMaterial bitmapImageArray isTexBlended =
  (  
    if isTexBlended then
      numsubs = bitmapImageArray.count + 1
    else
      numsubs = bitmapImageArray.count
    multimat = multimaterial numsubs:numsubs
    for i = 1 to bitmapImageArray.count do
      multimat[i] = standardMaterial \
      diffuseMap:(bitmaptexture bitmap:bitmapImageArray[i]) showInViewport:true
    if isTexBlended != unsupplied and isTexBlended then
      multimat[multimat.count + 1] = createBitmapBlendMaterial bitmapImageArray
    result = multimat  
  ), 
  fn addMaterialToMesh msh mat =
  (
  	msh.material = mat
  ),  
  fn addBitmapToMesh msh bitmapImage =
  (
  	msh.material = standardMaterial diffuseMap:(bitmaptexture bitmap:bitmapImage) showInViewport:true
  ),
  fn openBitmapFile filename =
  (
    try
    (
      openBitMap filename
    )  
    catch(
      messageBox ("Can't open bitmap file " + filename + ".")
      result = undefined
    )  
  ),
  fn createBitmap imageData imageWidth imageHeight filename =
  (
  	b = bitmap imageWidth imageHeight color:white 
	b.filename = filename + ".bmp"
    for line = 0 to (imageHeight - 1) do
    (
      row = for col = (line*imageWidth + 1) to (line + 1)*imageWidth collect
      (
        imageData[col]
      )
      setPixels b [0, line] row
    )       
    save b    
	b
  ),
  fn createHelperPoint name transformMatrix:0 =
  (
    hpoint = point
    hpoint.name = name
    if transformMatrix != unsupplied then
      hpoint.transform = transformMatrix
  ),
  on create do init()
)

max = max3ds()

-- lib/settings.ms


struct settings
(
  properties,
  iniFile = "settings.ini",
  fn saveToIniFile filename iniSectionTitle =
  (    
    ini.filename = iniFile    
	if fileUtils.isFileExist filename then    
      ini.filename = filename  
   
	if fileUtils.isFileExist ini.filename then 
    (
      local strValue
      names = getPropNames properties
      for name in names do
      (
        strName = name as string
        if ini.isKeyExist iniSectionTitle strName then
        (
          prop = getProperty properties name
          propClass = classof prop as string
          
          case propClass of 
          (        
            "Array":
            (
              local str = convertor.arrayValuesToString prop
              ini.setValue iniSectionTitle strName str       
            )                
            default:
              ini.setValue iniSectionTitle strName (prop as string)                     
          )                     
        )
      ) 
    )     
  ),  
  fn loadFromIniFile filename iniSectionTitle =
  (
    local intValue = 0
    local array = #()
    local point = [0, 0]
    local strValue = ""
    local boolValue = true
    
    ini.filename = iniFile    
	if fileUtils.isFileExist filename then    
      ini.filename = filename  
   
	if fileUtils.isFileExist ini.filename then 
    (
      local strValue
      names = getPropNames properties
      for name in names do
      (
        strName = name as string
        if ini.isKeyExist iniSectionTitle strName then
        (
          prop = getProperty properties name
          propClass = classof prop as string
          
          case propClass of 
          (        
            "Integer":
            (
              intValue = ini.getIntegerValue iniSectionTitle strName
              if intValue > 0 then
                setProperty properties name intValue             
            )
            "Array":
            (
              array = ini.getArrayValue iniSectionTitle strName
              if array.count > 0 then
                setProperty properties name array             
            )                
            "Point2": 
            (
              point = ini.getPoint2Value iniSectionTitle strName
              if point.x > 0 then
                setProperty properties name point            
            ) 
            "BooleanClass": 
            (
              boolValue = ini.getBooleanValue iniSectionTitle strName
              if boolValue != undefined then
                setProperty properties name boolValue  
            )    
            -- String, Value, etc...
            default:
            (
              strValue = ini.getValue iniSectionTitle strName
              if strValue != "" then
                setProperty properties name strValue             
            )            
          )                     
        )
      ) 
    )   
  )
)


-- lib/dialogs.ms


struct dialogs
(
  caption = "",
  types = "",
  filename = "",
  fn isValue txt =
  (
    if txt != "" then
    (
      try
      (
        val = execute txt
        result = true
      )
      catch 
      (
        messageBox "ERROR. Not a number: " + txt + "."
        result = false
      )
    )  
    else 
      result = false        
  ),
  fn isNonNegativeValue txt =
  (
    value = converter.stringToInteger txt
    if value <= 0 then 
    (
      messageBox ("Error. Value: " + txt + " must be greater than or equal to zero.")
      result = false
    )  
    else
      result = true  
  ),  
  fn checkInputValue txt checkType =
  (  
    result = false
    if isValue txt then  
      result = isnNonNegativeValue txt         
  ),  
  fn openFileDialog mode =
  (
  	if filename == "" then
      filename = 
      (      
	    if mode == "rb" then 
          getOpenFileName caption: caption types: types
	    else
          getSaveFileName caption: caption types: types
      )  
	if filename != undefined then
      filestream = fopen filename mode  
  ), 
  fn saveFile =
  (
  
  ),   
  fn openFolder caption =
  (
    getSavePath caption: caption
  ),
  fn showMessage caption =
  (
    messageBox caption
  )  
)

dialogs = dialogs()

-- lib/heightmap.ms

struct heightmapUtils
(
  size = 257,
  uTile = 10,
  vTile = 10,
  groundLevel = 10,
  fn generateFaces =
  (
    local faceIndex = 1
    local col = 0
    local faceArray = #()
    for i = 0 to size*(size - 2) do 
	(		
      append faceArray [faceIndex + 1, faceIndex + size, faceIndex]
      append faceArray [faceIndex + 1, faceIndex + size + 1, faceIndex + size]
      col += 1
      faceIndex += 1
      if col > (size - 2) then 
	  (
	    col = 0
        faceIndex += 1
      ) 
    )
    result = faceArray
  ),
  fn heightsToVertArray heightArray zCoeff cellWidth cellHeight =
  (
    local col = 0
    local row = size - 1
    vertArray = #()
    for point in heightArray do  
	(	
      append vertArray [(col*cellWidth as float), \
        ((size - row - 1)*cellHeight as float), \
        (z*zCoeff as float)]                 
      col += 1 
      if col > (size - 1) then 
	  (
        row -= 1
        col = 0 
      ) 
    ) 
    result = vertArray    
  ),
  fn generateIds vertArray faceArray isBlended = 
  (
    local idArray = #()
    for i = 1 to faceArray.count do
    (
      local indexes = #()
      append indexes faceArray[i].x
      append indexes faceArray[i].y
      append indexes faceArray[i].z    
      Z = 0
      for index in indexes do
      (
        if Z < vertArray[index].z then 
          z = vertArray[index].z
      )

      if Z >= groundLevel then
      (
        if isBlended then
        (
          if Z < groundLevel + 0.1 then
            append idArray 3
          else          
            append idArray 2         
        )
        else
          append idArray 2 
      )    
      else
          append idArray 1        
    )        
    result = idArray  
  ),  
  fn generateTVerts = 
  (
    local tvertArray = #()
    local u = (uTile/(size - 1) as float)
    local v = (vTile/(size - 1) as float) 
        
    for row = 0 to (size - 1) do
      for col = 0 to (size - 1) do 
        append tvertArray [(u*col as float), (v*row as float), 0]               
    result = tvertArray    
  ),
  fn getWaterLevel height = 
  (
    result = 0.4*height
  )   
)

heightmaps = heightmapUtils()

-- lib/raw.ms



struct heightmapSettings
(
  zCoeff = 1, 
  cellWidth = 1,
  cellHeight = 1,
  pointsInLine = 257, 
  pointBytes = 2,
  fn loadFromIni filename =
  (
    local settings = settings(this)
    settings.loadFromIniFile filename "ImportRolloutDialog"
  ) 
)

struct heightmapStrings
(
  lang = "Eng",
  openFileCaption = "Open heightmap file",
  saveFileCaption = "Save heightmap file",
  types = "Heightmap (*.raw)|*.raw",
  errorWriteFile = "Error. Can't write to the file %s.",
  errorReadFile = "Error. Can't read from the file %s.",
  cantOpenFile = "Error. Can't open file.", 
  errorZValue = "Error: Mesh height value must be nonzero.",  
  fn loadFromIni filename =
  (
    local settings = settings(this)
    settings.loadFromIniFile filename lang
  ) 
)

struct heightmapFile
(
  heightArray = #(),
  filename = "", 
  filestream,
  settings,
  strings,
  fn getPointCount = 
  (
    result = settings.pointsInLine*settings.pointsInLine
  ),   
  fn writeFile vertArray zHeight changeDirection flip = 
  (
    if zHeight == 0 then
    (
      dialogs.showMessage (formattedprint filename format:strings.errorZValue)  
      return false 
    )  
         
    heightCoeff = 255/zHeight as float
    count = getPointCount()    
    
    stepRow = stepCol = 1    
    if changeDirection then
    (
      stepRow = settings.pointsInLine
      startRow = 0
      endRow = count - settings.pointsInLine
      startCol = 1
      endCol = settings.pointsInLine
    ) 
    else
    (
      startRow = 1
      endRow = settings.pointsInLine 
      startCol = 1
      endCol = settings.pointsInLine          
    )    
  
    if flip == 3 then
    (
      stepRow = -stepRow
      stepCol = -stepCol
      swap startRow endRow
      swap startCol endCol      
    )  
 
    if flip == 1 then
    (      
      stepRow = -stepRow
      swap startRow endRow
    )  

    if flip == 2 then
    (
      stepCol = -stepCol     
      swap startCol endCol      
    )
    
    try
	(   
     for row = startRow to endRow by stepRow do 
        for col = startCol to endCol by stepCol do
        (   
          if changeDirection then        
	        z = heightCoeff*vertArray[row + col].z
          else 
            z = heightCoeff*vertArray[row*col].z          
	      WriteByte filestream (z as integer) --#signed
	      WriteByte filestream (z as integer) --#signed
        )          
      result = true
	)
    catch	
    (
      dialogs.showMessage (formattedprint filename format:strings.errorWriteFile)  
      result = false      
    )    
  ),
  fn readFile =
  (
    -- Every second byte in .raw file is the same as the first one
    pointsInLine = settings.pointsInLine
    count = settings.pointBytes
	try
	(
      for row = 0 to pointsInLine - 1 do  
        for col = 0 to pointsInLine - 1 do
        (
          fseek filestream (pointsInLine*row + col)*count #seek_set
          height = ReadByte filestream #unsigned	
          append heightArray (height as float)                        
        )
      result = true
    )	  
    catch	
    (
      dialogs.showMessage (formattedprint filename format:strings.errorReadFile)
      result = false
    )	
  ),
  fn openFile mode = 
  (
    dialogs.types = strings.types
    if mode == "rb" then
      dialogs.caption = strings.openFileCaption
    else
      dialogs.caption = strings.saveFileCaption    
      
    filestream = dialogs.openFileDialog mode
    if filestream != undefined then
    (
      filename = dialogs.filename
      result = filestream 
    )  
  ),
  fn closeFile = 
  (
    fclose filestream
  ),
  fn init = 
  (
    if settings == undefined then
      settings = heightmapSettings()
    if strings == undefined then
      strings = heightmapStrings()       
  ),
  on create do init()  
)



struct exportDialogSettings
(
  mainSection = "MAIN",
  lang = "ENG",
  fn loadFromIni filename =
  (
    local sets = settings(this)
    sets.loadFromIniFile filename mainSection
  )  
)

struct exportDialogStrings
(
  iniLang = "ENG",
  heighthmapFileSaved = "Heighthmap file %s was saved.",
  meshNotSelected = "Heightmap mesh isn't selected.",
  errorZeroHeight = "Mesh %s has zero height.",
  buttonCaption = "Export",
  rolloutTitle = "Export heightmap",
  radioNumObjLabels = #("first", "all"),
  numExportedObjsCaption = "Export selection",  
  exportSettingsText = "Settings",
  errorMeshSize = "Mesh: Wrong number of vertices. Must be equal %i.",
  exportGroupCaption = "Export settings",
  exportLangText = "Language",
  langCaption = "Choose language",
  langLabels = #("RU", "EN"),
  langButtonCaption = "Set",
  langGroupCaption = "Lang settings",
  errorObjectType = "Error: Wrong object type.",
  directionCheckBoxCaption = "change direction",
  flipXCheckBoxCaption = "flip x",
  flipYCheckBoxCaption = "flip y",
  fn loadFromIni filename =
  (
    local strings = settings(this)
    strings.loadFromIniFile filename iniLang
  )
)

struct exportDialog
(
  strings,
  langSettingsRollout =
  (
    rollout langSettingsRollout strings.exportLangText
    (
      local owner = if owner != undefined do owner 
      
      groupBox export_group owner.strings.langGroupCaption pos:[4, 2] \
        width:175 height:80     
      radiobuttons radio_numberObj owner.strings.langCaption \
        labels:owner.strings.langLabels pos:[40, 20] default:2 
      button button_setlang strings.langButtonCaption width:110 enabled:false
      
      on button_setlang pressed do
      (
        --
      )         
    )
  ),   
  exportSettingsRollout =
  (
    rollout exportSettingsRollout strings.exportSettingsText
    (
      local owner = if owner != undefined do owner 
      
      groupBox export_group owner.strings.exportGroupCaption pos:[4, 2] \
        width:175 height:115
      radiobuttons radio_numberObj owner.strings.numExportedObjsCaption \
        labels:owner.strings.radioNumObjLabels pos:[35, 20]  
        
      checkbox checkbox_direction owner.strings.directionCheckBoxCaption \
        pos:[35, 55]
      checkbox checkbox_flipX owner.strings.flipXCheckBoxCaption pos:[35, 70]
      checkbox checkbox_flipY owner.strings.flipYCheckBoxCaption pos:[85, 70]
      
      button button_export strings.buttonCaption width:110 pos:[35, 90]
      
      on button_export pressed do
      (
        flipX = checkbox_flipX.checked
        flipY = checkbox_flipY.checked 
          
        flip = 0
 
        if flipX and flipY then
          flip = 3
        else 
        (
          if flipX then 
            flip = 1
          if flipY then
            flip = 2
        )   
      
        owner.exportMeshToRaw checkbox_direction.checked flip      
      )    
    )
  ),  
  dialog = 
  (
    rollout dialog strings.rolloutTitle
    (
      subrollout subroll_container "container" height:260 width:196 pos:[2, 2]
    )    
  ),  
  fn exportMeshToRaw direction flip =
  (
    local minHeight = 0.0001 -- mesh height expected/taken to be lowest
    local numbMesh = selection.count 
    if numbMesh > 0 then
    (
      if numbMesh > 3 then numbMesh = 3
      for i = 1 to numbMesh do 
      (
        heightmapMesh = selection[i]  
        if classof selection[i] != Editable_Mesh then
          heightmapMesh = convertToMesh heightmapMesh
        
        if heightmapMesh == undefined then
        (
          dialogs.showMessage (formattedprint raw.filename \
            format:strings.errorObjectType)
        )
        else
        (
          raw = heightmapFile()     
          if raw.openFile "wb" != undefined then
          (
	        local vertexArray = max.getMeshVerts heightmapMesh            
            if raw.getPointCount() == (vertexArray.count) then
            (
              local height = (max.getMeshDimension heightmapMesh)[3]
              if (height > minHeight) then
              (
                if raw.writeFile vertexArray height direction flip then
                  dialogs.showMessage (formattedprint raw.filename \
                    format:strings.heighthmapFileSaved)
              )      
              else      
                dialogs.showMessage (formattedprint heightmapMesh.name \
                  format:strings.errorZeroHeight)                    
            )
            else
              dialogs.showMessage (formattedprint (raw.getPointCount()) \
                format:strings.errorMeshSize) 
            
            raw.closeFile()                 
          )
        )         
      )        
    )
    else
      dialogs.showMessage strings.meshNotSelected    
  ),
  fn init =
  (
    if strings == undefined then
      strings = exportDialogStrings()
      
    exportSettingsRollout.owner = this
    langSettingsRollout.owner = this   
    
    createDialog dialog width:200 pos:mouse.screenpos \
      style:#(#style_toolwindow, #style_sysmenu) 
    AddSubRollout dialog.subroll_container langSettingsRollout
    AddSubRollout dialog.subroll_container exportSettingsRollout      
  ),
  on create do init()
)

fn main = 
(
  currentPath = fileUtils.getCurrentPath(getThisScriptFilename())

  iniFileName = "raw_export_settings.ini"  
  sets = exportDialogSettings() 
  sets.loadFromIni (currentPath + iniFileName)

  iniFileName = "raw_export_strings.ini"  
  strings = exportDialogStrings sets.lang  
  strings.loadFromIni (currentPath + iniFileName)
   
  exportRaw = exportDialog strings 
)

main()