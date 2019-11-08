----------------------------------------------------------------
---- Cabal Online EBM file exporter script
---- Author: Kanaho
----------------------------------------------------------------

--------------------------------
-- Functions
--------------------------------

/* Writes a string and its length to the file */
function writetext _file _text =
(
	writeshort _file _text.count
	
	-- Write each character as a byte
	for i = 1 to _text.count do
		writebyte _file (bit.charasint _text[i])
)

/* Writes a 4x3 matrix to the file as a 4x4 matrix */
function writematrix _file _matrix =
(
	writefloat _file _matrix[1][1]; writefloat _file _matrix[1][2]; writefloat _file _matrix[1][3]; writefloat _file 0;
	writefloat _file _matrix[2][1]; writefloat _file _matrix[2][2]; writefloat _file _matrix[2][3]; writefloat _file 0;
	writefloat _file _matrix[3][1]; writefloat _file _matrix[3][2]; writefloat _file _matrix[3][3]; writefloat _file 0;
	writefloat _file _matrix[4][1]; writefloat _file _matrix[4][2]; writefloat _file _matrix[4][3]; writefloat _file 1;
)

/* Writes the header chunk to the file */
function writeheader _file =
(
	writelong _file ebm_header.magic
	writeshort _file ebm_header.unk0
	writebyte _file ebm_header.flag
	writebyte _file ebm_header.alpha_threshold
	writeshort _file ebm_header.unk1
	writefloat _file geometry.min[1]	-- Bounding box
	writefloat _file geometry.min[2]
	writefloat _file geometry.min[3]
	writefloat _file geometry.max[1]
	writefloat _file geometry.max[2]
	writefloat _file geometry.max[3]
	writelong _file ebm_header.scale_percentage
)

/* Writes the materials chunk to the file */
function writematerials _file =
(
	global out_materials = #()
	local meshes = (for o in geometry where classOf o == editable_mesh collect o)
	
	-- Iterate through each mesh and grab its material
	for i = 1 to meshes.count do
	(
		if	meshes[i].material != undefined and \
			(finditem out_materials meshes[i].material) == 0 then
			append out_materials meshes[i].material
	)
	
	writeshort _file out_materials.count
	
	-- Iterate through each material and write its data to the file
	for i = 1 to out_materials.count do
	(
		local has_tex = true
		
		if out_materials[i].diffusemap == undefined then
			has_tex = false
		
		-- Open the texture used by the material
		if has_tex == true then
		(
			bmp_filename = out_materials[i].diffusemap.filename
			bmp_size = getfilesize bmp_filename
			bmp_file = fopen bmp_filename "rb"
		)
		
		-- Write the material properties
		writefloat _file (out_materials[i].ambient.r / 255f)
		writefloat _file (out_materials[i].ambient.g / 255f)
		writefloat _file (out_materials[i].ambient.b / 255f)
		writefloat _file (out_materials[i].ambient.a / 255f)
		
		writefloat _file (out_materials[i].diffuse.r / 255f)
		writefloat _file (out_materials[i].diffuse.g / 255f)
		writefloat _file (out_materials[i].diffuse.b / 255f)
		writefloat _file (out_materials[i].diffuse.a / 255f)
		
		writefloat _file (out_materials[i].specular.r / 255f)
		writefloat _file (out_materials[i].specular.g / 255f)
		writefloat _file (out_materials[i].specular.b / 255f)
		writefloat _file (out_materials[i].specular.a / 255f)
		
		writefloat _file (out_materials[i].filtercolor.r / 255f)
		writefloat _file (out_materials[i].filtercolor.g / 255f)
		writefloat _file (out_materials[i].filtercolor.b / 255f)
		writefloat _file (out_materials[i].filtercolor.a / 255f)
		
		writefloat _file out_materials[i].selfillumination
		
		-- Texture
		if has_tex == true then
		(
			writetext _file (filenamefrompath bmp_filename)
			writelong _file bmp_size #unsigned
			
			for j = 1 to bmp_size do
				writebyte _file (readbyte bmp_file)
		)
		else	-- No texture?  o_O
		(
			writetext _file out_materials[i].name
			writelong _file 0
		)
		
		-- Layer
		for j = 1 to 9 do writebyte _file 0
		
		writelong _file -1
		writebyte _file 0
		writefloat _file 0
		writefloat _file 0
		writelong _file 4
		
		if has_tex == true then
			fclose bmp_file
	)
)

/* Writes the armature chunk to the file */
function writearmature _file =
(
	local mesh_skin = (for o in geometry where classOf o == editable_mesh collect o)[1].modifiers[#skin]
	
	-- Iterate through each bone and write its data to the file
	if mesh_skin != undefined then
	(
		global bone_array = #()
		global bone_names = #()
		
		max modify mode
		modpanel.setcurrentobject mesh_skin
		
		bone_array.count = skinops.getnumberbones mesh_skin
		
		for i = 1 to bone_array.count do
		(
			bone_name = skinops.getbonename mesh_skin i 1
			bone_array[i] = getnodebyname bone_name
			bone_names[i] = bone_name
		)
		
		writeshort _file bone_array.count
		
		for i = 1 to bone_array.count do
		(
			writetext _file bone_array[i].name
			
			-- Write the parent bone ID
			if bone_array[i].parent != null then
				writelong _file ((finditem bone_array bone_array[i].parent) - 1)
			else
				writelong _file -1
			
			tm = bone_array[i].transform
			tm = inverse tm
			
			if bone_array[i].parent != null then
				ptm = bone_array[i].parent.transform
			else
				ptm = matrix3 1	-- Identity matrix
			
			-- Write the transformation and parent matrices
			writematrix _file tm		-- Inverted transformation matrix
			writematrix _file ptm		-- Parent transformation matrix
		)
	)
	else
		writeshort _file 0
)

/* Writes the meshes chunk to the file */
function writemeshes _file =
(
	-- Select all the meshes if "Export only mesh?" is unchecked
	if cabalebmpanel.cb_meshonly.checked == false then
	(
		select (for o in geometry where classOf o == editable_mesh collect o)
		writeshort _file selection.count
	)
	
	global meshes = selection as array
	
	-- Iterate through each selected mesh and write its data to the file
	for i = 1 to meshes.count do
	(
		writetext _file meshes[i].name
		writematrix _file (matrix3 1)	-- World matrix
		writematrix _file (matrix3 1)	-- Local matrix		
		writelong _file -1	-- Root bone ID
		
		-- Material ID
		if (meshes[i].material != undefined) then
			writebyte _file ((finditem out_materials meshes[i].material) - 1)
		else writebyte _file 0
		
		writeshort _file meshes[i].numverts
		writeshort _file meshes[i].numfaces
		
		local uv_array = #()
		uv_array.count = meshes[i].numtverts
		
		local mesh_norm = meshes[i].modifiers[#edit_normals]
		
		local norm_array = #()
		norm_array.count = meshes[i].numverts
		
		-- Iterate through each normal and store its data for later use
		if mesh_norm != undefined then
		(
			max modify mode
			modpanel.setcurrentobject mesh_norm
			
			for j = 1 to mesh_norm.getnumfaces() do
			(
				for k in 1 to 3 do
				(
					local n = mesh_norm.getnormalid j k
					local v = mesh_norm.getvertexid j k
					
					if norm_array[v] == undefined then
						norm_array[v] = (mesh_norm.getnormal n)
				)
			)
		)
		else
		(
			for j = 1 to meshes[i].numverts do
				norm_array[j] = (getnormal meshes[i] j)
		)
		
		-- Grab the vertex UV coordinates
		for j = 1 to meshes[i].numfaces do
		(
			face = getface meshes[i] j
			tvface = gettvface meshes[i] j
			
			uv_array[face[1]] = (gettvert meshes[i] tvface[1])
			uv_array[face[2]] = (gettvert meshes[i] tvface[2])
			uv_array[face[3]] = (gettvert meshes[i] tvface[3])
		)
		
		-- Iterate through each vertex and write its data to the file
		for j = 1 to meshes[i].numverts do
		(
			writefloat _file (getvert meshes[i] j).x	-- Position
			writefloat _file (getvert meshes[i] j).y
			writefloat _file (getvert meshes[i] j).z
			
			writefloat _file norm_array[j].x	-- Normal
			writefloat _file norm_array[j].y
			writefloat _file norm_array[j].z
			
			writefloat _file uv_array[j].x	-- UV
			writefloat _file (1 - uv_array[j].y)
		)
		
		-- Iterate through each face and write its data to the file
		for j = 1 to meshes[i].numfaces do
		(
			writeshort _file ((getface meshes[i] j)[1] - 1)
			writeshort _file ((getface meshes[i] j)[2] - 1)
			writeshort _file ((getface meshes[i] j)[3] - 1)
		)
		
		-- Influences!
		if meshes[i].numfaces != 0 then	-- wtf?
			writelong _file 0x41470205	-- Influence chunk ID

		if meshes[i].modifiers[#skin] != undefined and \
			bone_array != undefined and \
			bone_array.count != 0 then
		(
			writeshort _file 1
			
			local bonevert_array = #()
			local boneweight_array = #()
		
			bonevert_array.count = bone_array.count
			boneweight_array.count = bone_array.count
			
			local mesh_skin = meshes[i].modifiers[#skin]
			max modify mode
			modpanel.setcurrentobject mesh_skin
			
			for j = 1 to bonevert_array.count do
			(
				bonevert_array[j] = #()
				boneweight_array[j] = #()
			)
			
			for j = 1 to meshes[i].numverts do
			(
				local count = skinops.getvertexweightcount mesh_skin j
				
				for k = 1 to count do
				(
					local id = skinops.getvertexweightboneid mesh_skin j k
					local func
					local ver = (maxversion())[1] / 1000
					
					if ver >= 16 then
						getbonename = skinops.getbonenamebylistid
					else
						getbonename = skinops.getbonename
					
					local real_id = finditem bone_names (getbonename mesh_skin id 1)
					--local real_id = finditem bone_names(skinops.getbonenamebylistid mesh_skin id 1)
					vert_weight = (skinops.getvertexweight mesh_skin j k)
					
					if vert_weight != 0 then
					(
						append bonevert_array[real_id] j
						append boneweight_array[real_id] vert_weight
					)
				)
			)
			
			for j = 1 to bone_array.count do
			(
				writelong _file bonevert_array[j].count
				
				for k = 1 to bonevert_array[j].count do
					writelong _file (bonevert_array[j][k] - 1)
				
				for k = 1 to boneweight_array[j].count do
					writefloat _file boneweight_array[j][k]
			)
		)
		else if meshes[i].numfaces != 0 then	-- wtf?
			writeshort _file 0
	)
)

/* Writes a single animation to the file */
function writeanimation _file _node _anim =
(
	writetext _file _node.name
	
	local range = animations[_anim]
	local count = _node.pos.controller.keys.count
	local key_times = #()
	local offset = 0.0
	
	for i = 1 to count do
	(
		local key_time = _node.pos.controller.keys[i].time
		
		if key_time >= range.start and key_time <= range.end then
			append key_times key_time
	)
	
	offset = key_times[1]
	
	writelong _file key_times.count
	
	for i = 1 to key_times.count do
	(
		local index = getkeyindex _node.pos.controller[1].controller key_times[i]
		
		writefloat _file ((key_times[i] - offset) / 30.0)
		local x = (in coordsys parent _node.pos.controller[1].keys[index].value)
		local y = (in coordsys parent _node.pos.controller[2].keys[index].value)
		local z = (in coordsys parent _node.pos.controller[3].keys[index].value)
		
		writefloat _file x
		writefloat _file y
		writefloat _file z
	)
	
	count = _node.rotation.controller.keys.count
	key_times = #()
	
	for i = 1 to count do
	(
		local key_time = _node.rotation.controller.keys[i].time
		
		if key_time >= range.start and key_time <= range.end then
			append key_times key_time
	)
	
	offset = key_times[1]
	
	writelong _file key_times.count
	
	for i = 1 to key_times.count do
	(
		local index = getkeyindex _node.rotation.controller[1].controller key_times[i]
		
		writefloat _file ((key_times[i] - offset) / 30.0)
		local x = (in coordsys parent _node.rotation.controller[1].keys[index].value)
		local y = (in coordsys parent _node.rotation.controller[2].keys[index].value)
		local z = (in coordsys parent _node.rotation.controller[3].keys[index].value)
		
		local rot = (eulerangles x y z) as quat
		
		writefloat _file rot.x
		writefloat _file rot.y
		writefloat _file rot.z
		writefloat _file rot.w
	)
)

/* Writes the animations chunk to the file */
function writeanimations _file =
(
	animationrange = interval 0 1
	slidertime = 0
	
	local anims = cabalebmpanel.lb_anims.items
	
	writeshort _file anims.count
	
	-- Iterate through each animation and write its data to the file
	for i = 1 to anims.count do
	(
		writetext _file anims[i]
		
		writeshort _file (bone_array.count + meshes.count)
		
		-- Bone animations
		for j = 1 to bone_array.count do
			writeanimation _file bone_array[j] i
		
		-- Mesh animations
		for j = 1 to meshes.count do
			writeanimation _file meshes[j] i
	)
)

--------------------------------
-- Main
--------------------------------

file_name = ""

if cabalebmpanel.cb_meshonly.checked == false then
	file_name = getsavefilename \
	caption:"Save model as..." \
	types:"Cabal Model Files|*.ebm|Cabal Character Files|*.ech"
else
	file_name = getsavefilename \
	caption:"Save mesh as..." \
	types:"Cabal Mesh Files|*.cmesh"

if file_name != undefined then
(
	local out_file = fopen file_name "wb"

	if cabalebmpanel.cb_meshonly.checked == false then
	(
		writeheader out_file
		writelong out_file 0x41470201	-- Material chunk ID
		writematerials out_file
		writelong out_file 0x41470203	-- Bone chunk ID
		writearmature out_file
		writelong out_file 0x41470202	-- Mesh chunk ID
		writemeshes out_file
		writelong out_file 0x41470204	-- Animation chunk ID
		writeanimations out_file
		
		print "EBM successfully exported!"
	)
	else
	(
		writemeshes out_file
		
		print "Mesh successfully exported!"
	)
	
	fclose out_file
	
	clearselection()
)