/* 

Script to remove any weird symbols from object names

by: Laura Koekoek | laurakoekoek91@gmail.com

*/

fn cleanString st xc abc num =
(
	local theChars = xc
	if abc == true do (theChars = theChars + "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	if num == true do (theChars = theChars + "0123456789")
	
	--local theChars = ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" + xc)
	st = trimLeft (trimRight st) -- this wil remove white spaces
	if st.count != 0 do for i = st.count to 1 by -1 do
	(
		if findString theChars st[i] == undefined do (st = replace st i 1 "")
	)
	return st
)


fn uiButton xc abc num =
(
print xc
	for o in selection do (
		nameCleaned = cleanString o.name xc abc num
		print (o.name + " changed to " + nameCleaned)
		o.name = nameCleaned
	
	)
)


rollout mainRollout "Object Name Cleanup" width:200
(

	group "Keep these"(

		checkbutton abc "Keep A-Z" checked:true tooltip:"Keep letters"
		
		checkbutton num "Keep 0-9" checked:true tooltip:"Keep numbers"
	)

	group "Keep additional symbols" (
		editText uiText1 readonly:false width:170 height:20
	)
	button uiButton1 "Cleanup selected object names" width:180 height:40	

	on uiButton1 pressed do (
	print uiText1.text
		uiButton uiText1.text abc.state num.state
	print abc.state
	)
	
	on MainRollout open do
	(
		uiText1.text = "_"
	)
)

createdialog mainRollout