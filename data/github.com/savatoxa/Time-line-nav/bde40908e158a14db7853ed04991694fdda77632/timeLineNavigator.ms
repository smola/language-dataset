tags = #()
ranges = #()

function createTags = (
tags = #()
for i =1 to FrameTagManager.GetTagCount() do 
(
	id = FrameTagManager.GetTagID i
	tag_name = FrameTagManager.GetNameByID id
	append tags tag_name
	)
)

function createRanges = (
	ranges = #()
	createTags()
	local tagsCuted = #()
	for tag in tags where (tag.count > 5) do
	(
		tagCut  = substring tag (tag.count - 5) -1
		if tagCut == "_start" then ( append tagsCuted (substring tag 1 (tag.count - 6)) )
	)
	for tag in tags where (tag.count > 3) do
	(
		tagCut  = substring tag (tag.count - 3) -1
		if tagCut == "_end" then ( append tagsCuted (substring tag 1 (tag.count - 4)) )
	)
	for i = 1 to tagsCuted.count-1 do
	(
		for j = i+1 to tagsCuted.count do
		(
			if tagsCuted[i] == tagsCuted[j] then (append ranges tagsCuted[i]; exit)
		)
	)
)

	
function setRangeByRangeName rangeName = (
    rangeStart_idx = findItem tags (rangeName + "_start")
	rangeStart_id = FrameTagManager.GetTagID rangeStart_idx
	rangeEnd_idx = findItem tags (rangeName + "_end")
	rangeEnd_id = FrameTagManager.GetTagID rangeEnd_idx
	animationRange = interval (FrameTagManager.GetTimeByID rangeStart_id) (FrameTagManager.GetTimeByID rangeEnd_id)
)

function setAllAnimationRanges = (
	allTagFrames = #()
	for tag_idx = 1 to tags.count do
	(
		tag_id = FrameTagManager.GetTagID tag_idx
		tagFrame = FrameTagManager.GetTimeByID tag_id
		append allTagFrames tagFrame
	)
	sort allTagFrames
	animationRange = interval allTagFrames[1] allTagFrames[allTagFrames.count]
)

function createTagsByRangeName rangeName = (
	FrameTagManager.CreateNewTag (rangeName + "_start") animationRange.start
	FrameTagManager.CreateNewTag (rangeName + "_end") animationRange.end
)

function deleteTagsByRangeName rangeName = (
	rangeStart_idx = findItem tags (rangeName + "_start")
	rangeStart_id = FrameTagManager.GetTagID rangeStart_idx
	rangeEnd_idx = findItem tags (rangeName + "_end")
	rangeEnd_id = FrameTagManager.GetTagID rangeEnd_idx
	FrameTagManager.DeleteTag rangeStart_id
	FrameTagManager.DeleteTag rangeEnd_id
	)
	
function redefineRange rangeName = (
	rangeStart_idx = findItem tags (rangeName + "_start")
	rangeStart_id = FrameTagManager.GetTagID rangeStart_idx
	rangeEnd_idx = findItem tags (rangeName + "_end")
	rangeEnd_id = FrameTagManager.GetTagID rangeEnd_idx
	FrameTagManager.SetTimeByID rangeStart_id animationRange.start
	FrameTagManager.SetTimeByID rangeEnd_id animationRange.end
)

function renameRange oldRangeName newRangeName = (
	rangeStart_idx = findItem tags (oldRangeName + "_start")
	rangeStart_id = FrameTagManager.GetTagID rangeStart_idx
	rangeEnd_idx = findItem tags (oldRangeName + "_end")
	rangeEnd_id = FrameTagManager.GetTagID rangeEnd_idx
	FrameTagManager.SetNameByID rangeStart_id (newRangeName + "_start")
	FrameTagManager.SetNameByID rangeEnd_id (newRangeName + "_end")
)

rollout win1 "Time line navigator" width:200 height:305
(
	
	dropdownList ddl1 "Animation ranges" pos:[10,8] width:180 height:40 items: (for range in ranges collect range)
	button btn1 "Set selected range" pos:[10,55] width:180 height:20
	button btn2 "Delete selected range" pos:[10,155] width:180 height:20
	edittext edt1 "Enter range name" pos:[10,205] width:180 height:17 labelOnTop:true
	button btn3 "Create new range" pos:[10,250] width:180 height:20
	button btn4 "Redefine selected range" pos:[10,105] width:180 height:20
	button btn5 "Rename selected range" pos:[10,275] width:180 height:20
	button btn6 "Create animation ranges from tags" pos:[10,130] width:180 height:20
	button btn7 "Delete all time tags" pos:[10,180] width:180 height:20
	button btn8 "Set all animation ranges" pos:[10,80] width:180 height:20
	
		
	local rangeWork = ranges[1]
	local newRange = ""
		
	on ddl1 selected i do
	(
		rangeWork = ddl1.items[i]
		edt1.text = rangeWork
	)
	on btn1 pressed do
	(
		if ranges.count > 0 then
		(
			setRangeByRangeName rangeWork
			rangeStart_idx = findItem tags (rangeWork + "_start")
			rangeStart_id = FrameTagManager.GetTagID rangeStart_idx
			sliderTime = FrameTagManager.GetTimeByID rangeStart_id
		)
	)
	on btn2 pressed do
	(
		if ranges.count > 0 then 
		(
			deleteTagsByRangeName rangeWork 
			createRanges()
			ddl1.items = (for range in ranges collect range)
			rangeWork = ranges[1]
			ddl1.selection = 1
		)
	)
	on edt1 entered txt do
	(
		newRange = txt
	)
	on btn3 pressed do
	(
		if newRange != "" then
		(
			createTagsByRangeName newRange
			rangeWork = newRange
			newRange = ""
			edt1.text = rangeWork
			createRanges()
			ddl1.items = (for range in ranges collect range)
		)
	)
	on btn4 pressed do
	(
		if ranges.count > 0 then
		(
			redefineRange rangeWork
			createRanges()
			ddl1.items = (for range in ranges collect range)
		)
	)
	on btn5 pressed do
	(
		if ranges.count > 0 and newRange != "" then
		(
			renameRange rangeWork newRange
			createRanges()
			rangeWork = newRange
			ddl1.items = (for range in ranges collect range)
			newRange = ""
			edt1.text = rangeWork
		)
	)
	on btn6 pressed do
	(
		createRanges()
		ddl1.items = (for range in ranges collect range)
		rangeWork = ranges[1]
		ddl1.selection = 1
	)
	on btn7 pressed do
	(
		if queryBox "Delete all time tags ?" beep: True then
		(
			FrameTagManager.ResetFrameTags()
			createRanges()
			ddl1.items = (for range in ranges collect range)
		)
	)
	on btn8 pressed do
	(
		if tags.count > 0 then
		(
		setAllAnimationRanges()
		)
	)
)

createRanges()
createDialog win1