/*
The nPose scripts are licensed under the GPLv2 (http://www.gnu.org/licenses/gpl-2.0.txt), with the following addendum:

The nPose scripts are free to be copied, modified, and redistributed, subject to the following conditions:
	- If you distribute the nPose scripts, you must leave them full perms.
	- If you modify the nPose scripts and distribute the modifications, you must also make your modifications full perms.

"Full perms" means having the modify, copy, and transfer permissions enabled in Second Life and/or other virtual world platforms derived from Second Life (such as OpenSim).  If the platform should allow more fine-grained permissions, then "full perms" will mean the most permissive possible set of permissions allowed by the platform.
*/
integer MEMORY_TO_BE_USED_SL=58000;
integer MEMORY_TO_BE_USED_IW=116000;
integer OFFSETS_TO_BE_USED=15;

integer ADJUST = 201;
integer DUMP = 204;
integer STOPADJUST = 205;
integer SYNC = 206;
integer ADJUSTOFFSET = 208;
integer SETOFFSET = 209;
integer UNSIT = -222;
integer OPTIONS = -240;
integer PLUGIN_ACTION = -830;
integer PLUGIN_ACTION_DONE = -831;
integer PLUGIN_MENU = -832;
integer PLUGIN_MENU_DONE = -833;

integer MENU_USAGE = 34334;
integer SEAT_UPDATE = 35353;

string MY_PLUGIN_MENU_OFFSET="npose_offset";
float CurrentOffsetDelta = 0.2;

integer AdjusterChannel;
string ADJUSTER_NAME="nPose Adjuster";
string CurrentAdjusterName;
integer AdjusterListenerHandle;
list AdjusterList; //3-strided list: [adjusterKey, adjusterPositionOffset, adjusterRotationOffset, ...]
integer ADJUSTER_LIST_STRIDE=3;

list RunningAnimations; //2 stride list [avatarKey, CSV Running Animations]; This list contains the animations that are running AFTER the whole AnimationQueue is processed
list AvatarOffsets; //2-strided list: [avatarKey, avatarPositionOffet, ...]

integer SecondLifeDetected;

integer SLOTS_LIST_STRIDE=8;

list SlotsAnimation;
list SlotsPos;
list SlotsRot;
list SlotsFacials; //We need this for the Text output of the adjusters
list SlotsAvatar;
list SlotsSeatName;

list AnimationQueue; // 3-strided list: [AvatarKey, AnimationsToStop, Animations to start], use startAnimations() to add new animations to it
integer AnimationQueueRequestPending; //0: no Pending request; timestamp: pending request

integer OptionQuietAdjusters;
integer OptionAdjustRefRoot;

string BUTTON_OFFSET_FWD = "forward";
string BUTTON_OFFSET_BKW = "backward";
string BUTTON_OFFSET_LEFT = "left";
string BUTTON_OFFSET_RIGHT = "right";
string BUTTON_OFFSET_UP = "up";
string BUTTON_OFFSET_DOWN = "down";
string BUTTON_OFFSET_ZERO = "reset";
list OFFSET_BUTTONS = [
	BUTTON_OFFSET_FWD, BUTTON_OFFSET_LEFT, BUTTON_OFFSET_UP,
	BUTTON_OFFSET_BKW, BUTTON_OFFSET_RIGHT, BUTTON_OFFSET_DOWN,
	"0.2", "0.1", "0.05",
	"0.01", BUTTON_OFFSET_ZERO
];

integer GridType;
integer GRID_TYPE_OTHER=0; 
integer GRID_TYPE_SL=1; //Second Life
integer GRID_TYPE_IW=2; //InWorldz
integer GRID_TYPE_DW=4; //DigiWorldz
string GRID_TYPE_SL_STRING="Second Life Server";
string GRID_TYPE_IW_STRING="Halcyon Server";
string GRID_TYPE_DW_STRING="OpenSim";

debug(list message){
	llOwnerSay((((llGetScriptName() + "\n##########\n#>") + llDumpList2String(message,"\n#>")) + "\n##########"));
}

//helper
string deleteNodes(string path, integer start, integer end) {
	return llDumpList2String(llDeleteSubList(llParseStringKeepNulls(path, [":"], []), start, end), ":");
}

//helper
string buildParamSet1(string path, integer page, string prompt, list additionalButtons, list pluginParams) {
	//pluginParams are: string pluginLocalPath, string pluginName, string pluginMenuParams, string pluginActionParams
	//We can't use colons in the promt, because they are used as a seperator in other messages
	//so we replace them with a UTF Symbol
	return llDumpList2String([
		path,
		page,
		llDumpList2String(llParseStringKeepNulls(prompt, [","], []), "‚"), // CAUTION: the 2nd "‚" is a UTF sign!
		llDumpList2String(additionalButtons, ",")
	] + llList2List(pluginParams + ["", "", "", ""], 0, 3), "|");
}

checkMemory() {
	//if memory is low, discard the oldest cache entry
	if((GridType && GRID_TYPE_SL) || (GridType && GRID_TYPE_IW)) {
		integer memoryToBeUsed=MEMORY_TO_BE_USED_SL;
		if(GridType && GRID_TYPE_IW) {
			memoryToBeUsed=MEMORY_TO_BE_USED_IW;
		}
		while(llGetUsedMemory()>memoryToBeUsed && llGetListLength(AvatarOffsets)) {
			AvatarOffsets=llDeleteSubList(AvatarOffsets, 0, 1);
		}
	}
	else {
		//in OpenSimulator we are not able to detect the current used memory
		integer numberOfOffsets=llGetListLength(AvatarOffsets);
		if(numberOfOffsets>OFFSETS_TO_BE_USED) {
			AvatarOffsets=llDeleteSubList(AvatarOffsets, 0, numberOfOffsets - OFFSETS_TO_BE_USED - 1);
		}
	}
}

integer getAvatarLinkNumber(key avatarKey) {
	integer linkCount=llGetNumberOfPrims();
	while(avatarKey!=llGetLinkKey(linkCount)) {
		if(llGetAgentSize(llGetLinkKey(linkCount))==ZERO_VECTOR) {
			return -1;
		}
		linkCount--;
	}
	return linkCount;
}

moveLinkedAvatar(integer slotNumber) {
	key avatarKey=llList2Key(SlotsAvatar, slotNumber);
	if(avatarKey) {
		integer linkNumber=getAvatarLinkNumber(avatarKey);
		if(~linkNumber) {
			rotation localRot;
			vector localPos;
			//check if OptionAdjustRefRoot is off and the prim containing this script is in a linked prim
			if(OptionAdjustRefRoot == 0 && llGetLinkNumber() > 1) {  
				localRot = llGetLocalRot();
				localPos = llGetLocalPos();
			}

			rotation avRot=llList2Rot(SlotsRot, slotNumber);
			vector avPos=llList2Vector(SlotsPos, slotNumber);
			//override the pos/rot information in the Slots list, if adjusters are active
			if(AdjusterListenerHandle) {
				avRot=llList2Rot(AdjusterList, slotNumber*ADJUSTER_LIST_STRIDE+2);
				avPos=llList2Vector(AdjusterList, slotNumber*ADJUSTER_LIST_STRIDE+1);
			}

			rotation offsetRot;
			vector offsetPos;
			if(!AdjusterListenerHandle) {
				//no adjuster: apply individual avatar offsets
				integer index=llListFindList(AvatarOffsets, [avatarKey]);
				if(~index) {
					offsetPos=llList2Vector(AvatarOffsets, index+1);
					//TODO: maybe we want an individual rotation offset too?
				}
			}
			
			if(!SecondLifeDetected) {
				//Open Simulator doesn't move just seated avatars
				llSleep(0.2);
			}
//debug([avPos, RAD_TO_DEG*llRot2Euler(avRot), localPos, RAD_TO_DEG*llRot2Euler(localRot), offsetPos, RAD_TO_DEG*llRot2Euler(offsetRot)]);
//PRIM_POSITION, ((avpos - (llRot2Up(avrot) * size.z * 0.02638)) * localrot) + localpos,
//PRIM_ROTATION, avrot * localrot / llGetRootRotation()
			//TODO: check tis formula
			llSetLinkPrimitiveParamsFast(linkNumber, [
				PRIM_ROT_LOCAL, localRot * avRot * offsetRot,
				PRIM_POS_LOCAL, localPos + avPos * localRot + offsetPos * localRot * avRot
			]);
		}
	}	
}

setAvatarOffset(key avatar, vector offset) {
	integer slotNumber=llListFindList(SlotsAvatar, [avatar]);
	if(~slotNumber) {
		integer avatarOffsetsIndex = llListFindList(AvatarOffsets, [avatar]);
		if(offset == ZERO_VECTOR && avatarOffsetsIndex >= 0) {
			AvatarOffsets = llDeleteSubList(AvatarOffsets, avatarOffsetsIndex, avatarOffsetsIndex+1);
			moveLinkedAvatar(slotNumber);
			return;
		}
		if(~avatarOffsetsIndex) { 
			offset = llList2Vector(AvatarOffsets, avatarOffsetsIndex+1) + offset;
			AvatarOffsets = llDeleteSubList(AvatarOffsets, avatarOffsetsIndex, avatarOffsetsIndex+1);
		}
		checkMemory();
		//move existing av and offset to the endo of the list for safe keeping
		AvatarOffsets = AvatarOffsets + [avatar, offset];
		moveLinkedAvatar(slotNumber);
	}
}

getAdjusterName() {
	CurrentAdjusterName="";
	integer length = llGetInventoryNumber(INVENTORY_OBJECT);
	integer index;
	while(index<length && CurrentAdjusterName=="") {//step through the Objects
		string name = llGetInventoryName(INVENTORY_OBJECT, index);
		if(!llSubStringIndex(name, ADJUSTER_NAME)) {
			CurrentAdjusterName=name;
		}
		index++;
	}
}

sendAdjusterUpdate(integer slotNumber) {
	key adjusterId=llList2Key(AdjusterList, slotNumber*ADJUSTER_LIST_STRIDE);
	if(adjusterId!=NULL_KEY) {
		llRegionSayTo(adjusterId, AdjusterChannel, addCommand("", [
			"SA_UPDATE",
			slotNumber,
			OptionAdjustRefRoot,
			OptionQuietAdjusters, 
			llList2String(SlotsAnimation, slotNumber), //Animation
			llList2String(SlotsPos, slotNumber), //Postition
			llList2String(SlotsRot, slotNumber), //Rotation
			llList2String(SlotsFacials, slotNumber), //Facials
			llList2String(SlotsAvatar, slotNumber), //SitterKey
			llList2String(SlotsSeatName, slotNumber) //SeatName+
		]));
	}
}

string addCommand(string commands, list commandWithParamList) {
	if(commands=="") {
		return llList2Json(JSON_ARRAY, [llList2Json(JSON_ARRAY, commandWithParamList)]);
	}
	else {
		return llList2Json(JSON_ARRAY, llJson2List(commands) + [llList2Json(JSON_ARRAY, commandWithParamList)]);
	}
}

addRemoveAdjusters(integer newNumberOfAdjuster) {
	if(AdjusterListenerHandle) {
		integer numberOfAdjusters=llGetListLength(AdjusterList)/ADJUSTER_LIST_STRIDE;
		if(newNumberOfAdjuster>numberOfAdjusters) {
			if(llGetInventoryType(CurrentAdjusterName) == INVENTORY_OBJECT) {
				vector myPos=llGetPos();
				while(newNumberOfAdjuster>numberOfAdjusters) {
					llRezAtRoot(CurrentAdjusterName, myPos, ZERO_VECTOR, ZERO_ROTATION, 0);
					AdjusterList+=[NULL_KEY, llList2Vector(SlotsPos, numberOfAdjusters), llList2Rot(SlotsRot, numberOfAdjusters)];
					numberOfAdjusters++;
				}
			}
		}
		else if(newNumberOfAdjuster<numberOfAdjusters) {
			while(newNumberOfAdjuster<numberOfAdjusters) {
				key lastAdjusterId=llList2Key(AdjusterList, -3);
				if(lastAdjusterId!=NULL_KEY) {
					llRegionSayTo(lastAdjusterId, AdjusterChannel, addCommand("", ["SA_DIE"]));
				}
				AdjusterList=llDeleteSubList(AdjusterList, -3, -1);
				numberOfAdjusters--;
			}
		}
	}
}

startAnimations(key avatarKey, string animationsToStart) {
	//Use this function to start new animations
	//it takes care that previous animations are stopped
	
	if(animationsToStart=="") {
		animationsToStart="Sit";
	}
	
	//Get the running animations
	string runningAnimations;
	integer index=llListFindList(RunningAnimations, [avatarKey]);
	if(~index) {
		runningAnimations=llList2String(RunningAnimations, index+1);
	}
	else {
		runningAnimations="Sit";
	}
	//add to queue
	AnimationQueue+=[avatarKey, runningAnimations, animationsToStart];
	
	//Update RunningAnimations
	index=llListFindList(RunningAnimations, [avatarKey]);
	if(~index) {
		RunningAnimations=llListReplaceList(RunningAnimations, [avatarKey, animationsToStart], index, index);
	}
	else {
		RunningAnimations+=[avatarKey, animationsToStart];
	}

	animationQueueCheck();
}

animationQueueCheck() {
	integer now=llGetUnixTime();
	if(AnimationQueueRequestPending<now) {
		integer found;
		while(llGetListLength(AnimationQueue) && !found) {
			//remove invalid avatars
			key avatarKey=llList2Key(AnimationQueue, 0);
			if(~llListFindList(SlotsAvatar, [avatarKey])) {
				if(~getAvatarLinkNumber(avatarKey)) {
					found=TRUE;
				}
			}
			if(!found) {
				AnimationQueue=llDeleteSubList(AnimationQueue, 0, 2);
			}
		}
		if(llGetListLength(AnimationQueue)) {
			AnimationQueueRequestPending=now+5; //5 seconcds for timeout
			llRequestPermissions(llList2Key(AnimationQueue, 0), PERMISSION_TRIGGER_ANIMATION);
		}
		else {
			AnimationQueueRequestPending=0;
		}
	}
}

string vectorToString(vector value, integer precision) {
	return
		 "<" +
		floatToString(value.x, precision) + 
		", " +
		floatToString(value.y, precision) + 
		", " +
		floatToString(value.z, precision) + 
		">"
	;
}

string floatToString(float value,  integer precision) {
	// precision: number of decimal places
	// return (string)value;
	string valueString=(string)((float)llRound(value*llPow(10,precision))/llPow(10,precision));
	string char;
	do {
		char=llGetSubString(valueString, -1, -1);
		if(char=="." || char=="0") {
			valueString=llDeleteSubString(valueString, -1, -1);
		}
	} while (char=="0");
	return valueString;
}

default {
	state_entry() {
		getAdjusterName();
		string simChannel=llGetEnv("sim_channel");
		GridType=
			GRID_TYPE_SL * (simChannel==GRID_TYPE_SL_STRING) + 
			GRID_TYPE_DW * (simChannel==GRID_TYPE_DW_STRING) + 
			GRID_TYPE_IW * (simChannel==GRID_TYPE_IW_STRING)
		;
		SecondLifeDetected=llGetEnv("sim_channel")=="Second Life Server";
		AdjusterChannel=(integer)("0x7F" + llGetSubString((string)llGetKey(), 1, 6));
	}
	
	listen(integer channel, string name, key id, string message) {
		if(llGetOwnerKey(id) == llGetOwner()) {
			integer slotNumber;
			integer index=llListFindList(AdjusterList, [id]);
			if(~index) {
				slotNumber=index/3;
			}
			else if(!llSubStringIndex(name, ADJUSTER_NAME)) {
				//unknown Adjuster
				index=llListFindList(AdjusterList, [NULL_KEY]);
				if(~index) {
					//we need this adjuster
					slotNumber=index/ADJUSTER_LIST_STRIDE;
					AdjusterList=llListReplaceList(AdjusterList, [id, llList2Vector(SlotsPos, slotNumber), llList2Rot(SlotsRot, slotNumber)], index, index+ADJUSTER_LIST_STRIDE-1);
					moveLinkedAvatar(slotNumber); //to make sure that individual offsets are removed and the adjuster takes over
				}
				else {
					//we don't need this adjuster
					llRegionSayTo(id, AdjusterChannel, addCommand("", ["SA_DIE"]));
					return;
				}
			}
			else {
				//unknow object, no adjuster
				return;
			}
			
			//at this point we have a valid slotNumber
			
			if(llJsonValueType(message, [])==JSON_ARRAY) {
				list commandLines=llJson2List(message);
				while(llGetListLength(commandLines)) {
					list commandParts=llJson2List(llList2String(commandLines, 0));
					commandLines=llDeleteSubList(commandLines, 0, 0);
					string cmd=llList2String(commandParts, 0);
					if(cmd=="AS_UPDATE_REQUEST") {
						sendAdjusterUpdate(slotNumber);
					}
					else if(cmd=="AS_INVENTORY_REQUEST") {
						integer length=llGetListLength(commandParts);
						for(index=1; index<=length; index++) {
							integer success;
							string item=llList2String(commandParts, index);
							if(llGetInventoryType(item)!=INVENTORY_NONE) {
								if(llGetInventoryPermMask(item, MASK_OWNER) & PERM_COPY) {
									llGiveInventory(id, item);
									success=TRUE;
								}
							}
						}
					}
					else if(cmd=="AS_SAY") {
					}
					else if(cmd=="AS_POS_ROT") {
						AdjusterList=llListReplaceList(AdjusterList, [(vector)llList2String(commandParts, 1), (rotation)llList2String(commandParts, 2)], slotNumber*ADJUSTER_LIST_STRIDE+1, slotNumber*ADJUSTER_LIST_STRIDE+2);
						moveLinkedAvatar(slotNumber);
					}
				}
			}
		}
	}
 
	link_message(integer sender, integer num, string str, key id) {
		if(num == ADJUSTOFFSET || num == SETOFFSET) {
			setAvatarOffset(id, (vector)str);
		}
		else if(num == SEAT_UPDATE){
			list slots = llParseStringKeepNulls(str, ["^"], []);
			str = "";
			integer numberOfSlots=llGetListLength(slots)/8;
			
			SlotsAnimation=[];
			SlotsPos=[];
			SlotsRot=[];
			SlotsFacials=[];
			SlotsAvatar=[];
			SlotsSeatName=[];
			
			integer index;
			for(index = 0; index < numberOfSlots; index++) {
				SlotsAnimation+=llList2String(slots, index*SLOTS_LIST_STRIDE);
				SlotsPos+=(vector)llList2String(slots, index*SLOTS_LIST_STRIDE+1); 
				SlotsRot+=(rotation)llList2String(slots, index*SLOTS_LIST_STRIDE+2);
				SlotsFacials+=llList2String(slots, index*SLOTS_LIST_STRIDE+3);
				SlotsAvatar+=(key)llList2String(slots, index*SLOTS_LIST_STRIDE+4);
				SlotsSeatName+=llList2String(slots, index*SLOTS_LIST_STRIDE+7);
			}
			//garbarge collection
			index=0;
			while(index < llGetListLength(RunningAnimations)) {
				if(!~llListFindList(SlotsAvatar, [llList2Key(RunningAnimations, index)])) {
					RunningAnimations=llDeleteSubList(RunningAnimations, index, index+1);
				}
				else {
					index+=2;
				}
			}

			//garbarge collection
			index=0;
			while(index < llGetListLength(AnimationQueue)) {
				if(!~llListFindList(SlotsAvatar, [llList2Key(AnimationQueue, index)])) {
					AnimationQueue=llDeleteSubList(AnimationQueue, index, index+2);
				}
				else {
					index+=3;
				}
			}

			//keep track of the right number of adjusters
			addRemoveAdjusters(numberOfSlots);
			//other updates
			for(index = 0; index < numberOfSlots; index++) {
				key currentAvatar=llList2Key(SlotsAvatar, index);
				if(currentAvatar!=NULL_KEY && currentAvatar!="") {
					//Update Avatar Positions
					moveLinkedAvatar(index);
					//Update Animations
					startAnimations(currentAvatar, llList2String(SlotsAnimation, index));
				}
				//Update Adjusters
				sendAdjusterUpdate(index);
			}
		}
		else if(num == UNSIT) {
			key avatarUuid=(key)str;
			if(avatarUuid) {
				if(~getAvatarLinkNumber(avatarUuid)) {
					llUnSit(avatarUuid);
				}
			}
		}
		else if(num == SYNC) {
			integer length=llGetListLength(SlotsAnimation);
			integer index;
			list temp;
			//TODO: check if we really need to start "Sit" animations for ALL avatars first
			for(index=0; index<length; index++) {
				key avatarKey=llList2Key(SlotsAvatar, index);
				if(avatarKey!=NULL_KEY && avatarKey!="") {
					startAnimations(avatarKey, "Sit");
					temp+=[avatarKey, llList2String(SlotsAnimation, index)];
				}
			}
			while(llGetListLength(temp)) {
				startAnimations(llList2Key(temp, 0), llList2String(temp, 1));
				temp=llDeleteSubList(temp, 0, 1);
			}
		}
		else if((num == ADJUST)) {
			//adjust has been chosen from the menu
			if(llGetInventoryType(CurrentAdjusterName)==INVENTORY_OBJECT) {
				if(!AdjusterListenerHandle) {
					AdjusterListenerHandle=llListen(AdjusterChannel, "", NULL_KEY, "");
				}
				addRemoveAdjusters(llGetListLength(SlotsAnimation));
			}
		}
		else if(num == STOPADJUST) { //stopadjust has been chosen from the menu
			string command=addCommand("", ["SA_DIE"]);
			llRegionSay(AdjusterChannel, command);
			AdjusterList = [];
			llListenRemove(AdjusterListenerHandle);
			AdjusterListenerHandle=0;
			integer index;
			integer length=llGetListLength(SlotsAvatar);
			for(index = 0; index < length; index++) {
				key currentAvatar=llList2Key(SlotsAvatar, index);
				if(currentAvatar!=NULL_KEY && currentAvatar!="") {
					//Update Avatar Positions
					moveLinkedAvatar(index);
				}
			}
		}
		else if(num == OPTIONS) {
			//save new option(s) from LINKMSG
			list optionsToSet = llParseStringKeepNulls(str, ["~","|"], []);
			integer length = llGetListLength(optionsToSet);
			integer index;
			for(index=0; index<length; ++index) {
				list optionsItems = llParseString2List(llList2String(optionsToSet, index), ["="], []);
				string optionItem = llToLower(llStringTrim(llList2String(optionsItems, 0), STRING_TRIM));
				string optionString = llList2String(optionsItems, 1);
				string optionSetting = llToLower(llStringTrim(optionString, STRING_TRIM));
				integer optionSettingFlag = optionSetting=="on" || (integer)optionSetting;

				if(optionItem == "quietadjusters") {
					OptionQuietAdjusters = optionSettingFlag;
					if(AdjusterListenerHandle) {
						//update adjusters on the fly
						integer adjusterListLength=llGetListLength(AdjusterList);
						integer adjusterListIndex;
						for(adjusterListIndex=0; adjusterListIndex<adjusterListLength; adjusterListIndex++) {
							sendAdjusterUpdate(adjusterListIndex);
						}
					}
				}
				if(optionItem == "adjustrefroot") {
					OptionAdjustRefRoot = optionSettingFlag;
				}
			}
		}
		else if(num == DUMP) {
			integer index;
			integer length=llGetListLength(SlotsAnimation);
			for(index=0; index<length; index++) {
				list seatName=llParseStringKeepNulls(llList2String(SlotsSeatName, index), ["§"], []);
				string output="\nSet card for this data is '" + llList2String(seatName, 3) + "', SeatNumber: " + (string)(index+1);
				string action=llList2String(seatName, 2);
				output+="\n\n" + action;
				if(llSubStringIndex(action, "ANIM")!=0) {
					output+="|" + (string)(index+1);
				}
				output+="|" + (string)llList2String(SlotsAnimation, index);
				if(AdjusterListenerHandle) {
					output+="|" + vectorToString(llList2Vector(AdjusterList, index*ADJUSTER_LIST_STRIDE + 1), 3);
					output+="|" + vectorToString(RAD_TO_DEG * llRot2Euler(llList2Rot(AdjusterList, index*ADJUSTER_LIST_STRIDE + 2)), 2);
				}
				else {
					output+="|" + vectorToString(llList2Vector(SlotsPos, index), 3);
					output+="|" + vectorToString(RAD_TO_DEG * llRot2Euler(llList2Rot(SlotsRot, index)), 2);
				}
				string facials=llList2String(SlotsFacials, index);
				string userSeatName=llList2String(seatName, 0);
				if(facials!="" || userSeatName!="") {
					output+="|" + facials;
				}
				if(userSeatName!="") {
					output+="|" + userSeatName;
				}
				llRegionSayTo(llGetOwner(), 0, output);
			}
//			llRegionSay(AdjusterChannel, addCommand("", ["SA_DUMP"]));
		}

		else if(num==PLUGIN_ACTION || num==PLUGIN_MENU) {
			//offset menu
			list params=llParseStringKeepNulls(str, ["|"], []);
			string path=llList2String(params, 0);
			integer page=(integer)llList2String(params, 1);
			string prompt=llList2String(params, 2);
			string additionalButtons=llList2String(params, 3);
			string pluginLocalPath=llList2String(params, 4);
			string pluginName=llList2String(params, 5);
			string pluginMenuParams=llList2String(params, 6);
			string pluginActionParams=llList2String(params, 7);

			if(pluginName==MY_PLUGIN_MENU_OFFSET) {
				//this is the offset menu. It can be move to any other script easily.
				if(num==PLUGIN_ACTION) {
					// 1) Do the action if needed
					// 2) correct the path if needed
					// 3) finish with a PLUGIN_ACTION_DONE call
					if(pluginLocalPath!="") {
						vector direction;
						if(pluginLocalPath == BUTTON_OFFSET_FWD) {direction=<1, 0, 0>;}
						else if(pluginLocalPath == BUTTON_OFFSET_BKW) {direction=<-1, 0, 0>;}
						else if(pluginLocalPath == BUTTON_OFFSET_LEFT) {direction=<0, 1, 0>;}
						else if(pluginLocalPath == BUTTON_OFFSET_RIGHT) {direction=<0, -1, 0>;}
						else if(pluginLocalPath == BUTTON_OFFSET_UP) {direction=<0, 0, 1>;}
						else if(pluginLocalPath == BUTTON_OFFSET_DOWN) {direction=<0, 0, -1>;}
						else if((float)pluginLocalPath) {CurrentOffsetDelta = (float)pluginLocalPath;}
						if(direction!=ZERO_VECTOR || pluginLocalPath==BUTTON_OFFSET_ZERO) {
							setAvatarOffset(id, direction * CurrentOffsetDelta);
						}
						//one level back
						path=deleteNodes(path, -1, -1);
					}
					llMessageLinked(LINK_SET, PLUGIN_ACTION_DONE, buildParamSet1(path, 0, prompt, [], []), id);
				}
				else if(num==PLUGIN_MENU) {
					// 1) set a prompt if needed
					// 2) generate your buttons if needed
					// 3) finish with a PLUGIN_MENU_DONE call
					prompt="Adjust by " + (string)CurrentOffsetDelta+ "m, or choose another distance.";
					llMessageLinked(LINK_SET, PLUGIN_MENU_DONE, buildParamSet1(path, page, prompt, OFFSET_BUTTONS, []), id);
				}
			}
		}
		else if(num == MENU_USAGE) {
			llSay(0,"Memory Used by " + llGetScriptName() + ": " + (string)llGetUsedMemory() + " of " + (string)llGetMemoryLimit()
			 + ", Leaving " + (string)llGetFreeMemory() + " memory free.");
		}
	}
 

	run_time_permissions(integer perm) {
		AnimationQueueRequestPending=0;
		integer preCheckOk;
		key avatarKey = llGetPermissionsKey();
		integer slotNumber=llListFindList(SlotsAvatar, [avatarKey]);
		if(avatarKey==llList2Key(AnimationQueue, 0)) {
			//The avatar holding the permission is the first one in out Animation Queue
			if(PERMISSION_TRIGGER_ANIMATION & perm) {
				//The granted permissions are correct
				if(~slotNumber) {
					//the avatar is still in our slot list
					if(~getAvatarLinkNumber(avatarKey)) {
						//the avatar is still sitting
						preCheckOk=TRUE;
					}
				}
			}
		}
		if(!preCheckOk) {
			animationQueueCheck();
			return;
		}
		//stop animations
		list animations=llCSV2List(llList2String(AnimationQueue, 1));
		while(llGetListLength(animations)) {
			string animation=llList2String(animations, -1);
			llStopAnimation(animation);
			animations=llDeleteSubList(animations, -1, -1);
		}
		//start animations
		animations=llCSV2List(llList2String(AnimationQueue, 2));
		while(llGetListLength(animations)) {
			string animation=llList2String(animations, 0);
			llStartAnimation(animation);
			animations=llDeleteSubList(animations, 0, 0);
		}
		//Remove the current entry from the queue and recheck it
		AnimationQueue=llDeleteSubList(AnimationQueue, 0, 2);
		animationQueueCheck();
	}

	changed(integer change) {
		if(change & CHANGED_REGION) {
			llSleep(1.0);
			llMessageLinked(LINK_SET, SYNC, "", NULL_KEY);
		}
		if(change & CHANGED_INVENTORY) {
			getAdjusterName();
		}
	}
	on_rez(integer start_param) {
		AdjusterChannel=(integer)("0x7F" + llGetSubString((string)llGetKey(), 1, 6));
	}
}
