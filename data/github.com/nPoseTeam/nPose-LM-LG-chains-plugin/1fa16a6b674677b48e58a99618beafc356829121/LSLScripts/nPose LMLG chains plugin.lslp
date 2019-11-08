// Started at: 04.05.2013 17:04:00
// Authors: XandrineX and Perl Nakajima
//
// Modified by Pfil Payne: 
// - make the Plugin usable for more than on victim
// - add LMv2 compatibility
// - scan for leashpoints, if the cuffs are added later
// - Particle config
// texture = particle texture as uuid
// xsize   = particle X size as float (0.03125 to 4.0)
// ysize   = particle Y size as float (0.03125 to 4.0)
// gravity = particle gravity as float
// life	= particle life time as float in seconds
// red	 = red part of the particle color as float (0 to 1)
// green   = green part of the particle color as float (0 to 1)
// blue	= blue part of the particle color as float (0 to 1)
//
// Modified by Leona (slmember1 Resident):
// - fixed some timing issues
// - allow multiple chains to one lockmeister mooring point (does of course not work for lockguard) 


// --- configuration ---
integer gLOCKMEISTER_CHANNEL		  = -8888;
integer gLOCKGUARD_CHANNEL			= -9119;

// particle
string  gTexture					  = "245ea72d-bc79-fee3-a802-8e73c0f09473";
float   gXsize						= 0.07;
float   gYsize						= 0.07;
float   gGravity					  = 0.03;
float   gLife						 = 1;
float   gRed						  = 1;
float   gGreen						= 1;
float   gBlue						 = 1;

// incomming link messages...
integer gCMD_SET_CHAINS				= 2732; // cmdId, set chains in msg
integer gCMD_REM_CHAINS				= 2733; // cmdId, remove all chains
integer gCMD_CONFIG					= 2734; // cmdId, config in msg

string  gSET_MAIN_SEPARATOR			= "|"; // separator from linkmessage
string  gSET_SEPARATOR				= "~"; // separator from linkmessage
integer gPLUGIN_COMMAND_REGISTER	= 310;

// timer
integer gTIMER_MODE_IDLE			= 0; //timer not activated
integer gTIMER_MODE_CONTROL			= 1; //timer waits for a new search for missing leashpoints
integer gTIMER_MODE_ADD				= 2; //timer waits for a timeout after adding chains
integer gTIMER_MODE_REMOVE			= 3; //timer waits for a timeout after removing chains
integer gTIMER_MODE_UPDATE			= 4; //timer waits for a timeout after updating the chains parameters

float gTIMER_TIME_IDLE				= 0.0; // should be 0.0
float gTIMER_TIME_CONTROL			= 15.0; // rescantimer, if leashpoints missing
float gTIMER_TIME_ADD				= 2.0; //timeout after adding chains
float gTIMER_TIME_REMOVE			= 0.5; //timeout after removing chains
float gTIMER_TIME_UPDATE			= 0.5; //timeout after updating chains

// For trying to use lockguard as a fallback, you need to specify mappings here
// from the lockmeister to lockguard attachment IDs. I know that this is not
// perfect, since lockmeister and lockguard offer different cuff points and
// additionally work just the other way round. So I tried to do my best and
// give the best mapping I could think of. But you might need to change it,
// depending on your needs and piece of furniture.
// For reference and available points, see:
// http://wiki.secondlife.com/wiki/LSL_Protocol/LockMeister_System
// https://web.archive.org/web/20130224185823/http://lslwiki.net/lslwiki/wakka.php?wakka=exchangeLockGuardItem
list   gLM_TO_LG_MAPPINGS = [
	"rcuff",	"rightwrist",
	"rbiceps",  "rightupperarm",
	"lbiceps",  "leftupperarm",
	"lcuff",	"leftwrist",
	"lblade",   "harnessleftloopback",  // ?
	"rblade",   "harnessrightloopback", // ?
	"rnipple",  "rightnipplering",
	"lnipple",  "leftnipplering",
	"rfbelt",   "rightbeltloop",
	"lfbelt",   "leftbeltloop",
	"rtigh",	"rightupperthigh",
	"ltigh",	"leftupperthigh",
	"rlcuff",   "rightankle",
	"llcuff",   "leftankle",
	"lbbelt",   "harnessleftloopback",  // ?
	"rbbelt",   "harnessrightloopback", // ?
	"pelvis",   "clitring",			 // ?
	"fbelt",	"frontbeltloop",
	"bbelt",	"backbeltloop",
	"rcollar",  "collarrightloop",
	"lcollar",  "collarleftloop",
	"thead",	"topheadharness",	   // ?
	"collar",   "collarfrontloop",
	"lbit",	 "leftgag",			  // ?
	"rbit",	 "rightgag",			 // ?
	"nose",	 "nosering",
	"bcollar",  "collarbackloop",
	"back",	 "harnessbackloop",	  // ?
	"lhand",	"leftwrist",			// ?
	"rhand",	"rightwrist"			// ?
];

// --- global variables ---
list	gPrimIDs;		 // description => linkId
integer gListenLMHandle;  // storing lockmeister listening handle
integer gListenLGHandle;  // storing lockguard listening handle
list	gParticles;
integer gTimerMode;
list	gCommandQueue;	//3-strided list: [cmdId, avatarKey, commandParamsString]
list	gChains;
//0: avatarKey,
//1: desc(unique Prim desribption with a leading "|"),
//2: lmMooring,
//3: status (0: no cuffs found, 1: request pending, 2: lockguard in use, 3: lockmeister in use),
//4: (string)lmMooringKey

// ============================================================
debug(list message){
	llOwnerSay((((llGetScriptName() + "\n##########\n#>") + llDumpList2String(message,"\n#>")) + "\n##########"));
}


setTimerMode(integer mode) {
	if(mode==gTIMER_MODE_IDLE) {
		gTimerMode=gTIMER_MODE_IDLE;
		llSetTimerEvent(gTIMER_TIME_IDLE);
	}
	else if(mode==gTIMER_MODE_CONTROL) {
		gTimerMode=gTIMER_MODE_CONTROL;
		llSetTimerEvent(gTIMER_TIME_CONTROL);
	}
	else if(mode==gTIMER_MODE_ADD) {
		gTimerMode=gTIMER_MODE_ADD;
		llSetTimerEvent(gTIMER_TIME_ADD);
	}
	else if(mode==gTIMER_MODE_REMOVE) {
		gTimerMode=gTIMER_MODE_REMOVE;
		llSetTimerEvent(gTIMER_TIME_REMOVE);
	}
	else if(mode==gTIMER_MODE_UPDATE) {
		gTimerMode=gTIMER_MODE_UPDATE;
		llSetTimerEvent(gTIMER_TIME_UPDATE);
	}
}

lmV1Request(key avatarKey, string lmMooring) {
	if (!gListenLMHandle) gListenLMHandle = llListen( gLOCKMEISTER_CHANNEL, "", NULL_KEY, "" );
	llWhisper( gLOCKMEISTER_CHANNEL, (string)avatarKey + lmMooring );
}

lmV2Request(key target, key avatarKey, string lmMooring) {
	if (!gListenLMHandle) gListenLMHandle = llListen( gLOCKMEISTER_CHANNEL, "", NULL_KEY, "" );
	llRegionSayTo(target, gLOCKMEISTER_CHANNEL, llDumpList2String([
		avatarKey,
		"LMV2",
		"RequestPoint",
		lmMooring
	], "|"));
}

lgSetChain(key avatarKey, string desc, string lmMooring, integer responseRequest) {
	integer index = llListFindList( gPrimIDs, [ desc ] );
	if( index != -1 ) {
		integer primID = llList2Integer( gPrimIDs, index + 1 );
		index = llListFindList( gLM_TO_LG_MAPPINGS, [ lmMooring ] );
		if( index != -1 ) {
			string ping;
			if(responseRequest) {
				ping=" ping";
				if (!gListenLGHandle) gListenLGHandle = llListen( gLOCKGUARD_CHANNEL, "", NULL_KEY, "" );
			}
			string lgMooring  = llList2String( gLM_TO_LG_MAPPINGS, index + 1 );
			llWhisper( gLOCKGUARD_CHANNEL, llDumpList2String([
				"lockguard",
				avatarKey,
				lgMooring,
				"texture",
				gTexture,
				"size",
				gXsize,
				gYsize,
				"gravity",
				gGravity,
				"color",
				gRed,
				gGreen,
				gBlue,
				"life",
				gLife,
				"link",
				llGetLinkKey( primID )
				
			], " ") + ping);
		}
	}
}
lgRemoveChain(key avatarKey, string lmMooring) {
	integer index = llListFindList( gLM_TO_LG_MAPPINGS, [ lmMooring ] );
	if( index != -1 ) {
		string lgMooring  = llList2String( gLM_TO_LG_MAPPINGS, index + 1 );
		llWhisper( gLOCKGUARD_CHANNEL, llDumpList2String([
			"lockguard",
			avatarKey,
			lgMooring,
			"unlink"
		], " "));
	}
}

query_set_chains( key avatarKey, list params ) {
	list items=llParseStringKeepNulls(llList2String(params, 0), [gSET_SEPARATOR], []);
	key userDefinedAvatar=(key)llList2String(params, 1);
	if(userDefinedAvatar) {
		avatarKey=userDefinedAvatar;
	}
	
	integer itemLength = llGetListLength( items );
	integer i;
	for( i=0; i < itemLength; i+=2 ) {
		string desc	= "|" + llList2String( items, i );
		integer index  = llListFindList( gPrimIDs, [ desc ] );
		if( index == -1 ) {
//			  llOwnerSay( "/me Error: no ring " + desc + " found" );
		}
		else {
			integer primId = llList2Integer( gPrimIDs, index + 1 );
			string mooring = llList2String(  items,	i + 1 );
			if( mooring != "" ) {
//				llOwnerSay( "Chain: " + desc + " -> " + mooring
//					+ " PrimId: " + (string)primId );
				if(~(index=llListFindList(gChains, [desc]))) {
					//usually this shouldn't happen. If it happens the user didn't remove a chain before setting a new one
					//I don't know if we should try to fix it, because if we try to fix it it could fail because of timing issues
//					llOwnerSay("Already in use. Remove it first.");
				}
				else {
					llLinkParticleSystem( primId, [] );
					gChains+=[avatarKey, desc, mooring, 1, ""];
					lmV1Request(avatarKey, mooring);
					lgSetChain(avatarKey, desc, mooring, TRUE);
					setTimerMode(gTIMER_MODE_ADD);
				}
			}
		}
	}
}

query_rem_chains(list params ) {
	list descriptions=llParseStringKeepNulls(llList2String(params, 0), [gSET_SEPARATOR], []);

	integer length = llGetListLength( descriptions );
	integer i;
	for( i=0; i < length; i+=1 ) {
		string desc = "|" + llList2String( descriptions, i );
		integer index = llListFindList( gPrimIDs, [ desc ] );
		if( index != -1 ) {
			integer primID = llList2Integer( gPrimIDs, index + 1 );
			if(~(index = llListFindList( gChains, [ desc ] ))) {
				//remove lockmeister chains regardless of their state
				llLinkParticleSystem(primID , [] );
				//remove lockguard chains if they could be set
				integer status=llList2Integer(gChains, index + 2);
				if(status==2) {
					//status==2 lockguard used
					lgRemoveChain(llList2Key(gChains, index-1), desc);
					setTimerMode(gTIMER_MODE_REMOVE);
				}
				gChains=llDeleteSubList(gChains, index-1, index+3);
			}
		}
	}
}

update_chains() {
	//this is called if the chain parameters (texture, color ...) are changed
	integer length = llGetListLength( gChains );
	integer index;
	for( index=0; index < length; index+=5 ) {
		string desc	= llList2String(gChains, index+1);
		integer primID = llList2Integer( gPrimIDs, llListFindList(gPrimIDs, [desc])+1 );
		integer status = llList2Integer(gChains, index + 3);
		if(status==2) {
			//lockguard update
			lgSetChain(llList2Key(gChains, index), desc, llList2String(gChains, index+2), FALSE);
			setTimerMode(gTIMER_MODE_UPDATE);
		}
		else if(status==3) {
			//lockmeister update
			llLinkParticleSystem(primID, gParticles + [PSYS_SRC_TARGET_KEY, (key)llList2String(gChains, index + 4)]);
		}
	}
}

control_chains() {
	//try to find former missing cuffs
	integer length = llGetListLength( gChains );
	integer index;
	for( index=0; index < length; index+=5 ) {
		if(!llList2Integer(gChains, index + 3)) {
			//missing cuff
			gChains=llListReplaceList(gChains, [1], index + 3, index + 3);
			key avatarKey=llList2Key(gChains, index);
			string desc=llList2String(gChains, index+1);
			string mooring=llList2String(gChains, index+2);
			lmV1Request(avatarKey, mooring);
			lgSetChain(avatarKey, desc, mooring, TRUE);
		}
	}
}

query_config(list params) {
	list items=llParseStringKeepNulls(llList2String(params, 0), [gSET_SEPARATOR], []);
	
	integer length = llGetListLength( items );
	integer i;
	for( i=0; i < length; i+=1 ) {
		list line = llParseString2List( llList2String( items, i ), ["="], [] );
		string item = llList2String( line, 0 );
		
		if ( item == "texture" )	  gTexture = llList2String( line, 1 );
		else if ( item == "xsize" )   gXsize   = llList2Float( line, 1 );
		else if ( item == "ysize" )   gYsize   = llList2Float( line, 1 );
		else if ( item == "gravity" ) gGravity = llList2Float( line, 1 );
		else if ( item == "life" )	gLife	= llList2Float( line, 1 );
		else if ( item == "red" )	 gRed	 = llList2Float( line, 1 );
		else if ( item == "green" )   gGreen   = llList2Float( line, 1 );
		else if ( item == "blue" )	gBlue	= llList2Float( line, 1 );
	}
	set_particle();
	update_chains();
}

set_particle() {
	gParticles = [  // start of particle settings
					PSYS_PART_START_SCALE,	 <gXsize, gYsize, FALSE>,
					PSYS_PART_END_SCALE,	   <gXsize, gYsize, FALSE>,
					PSYS_PART_MAX_AGE,		 gLife,
					PSYS_SRC_ACCEL,			<0, 0, (gGravity*-1)>,		
					PSYS_SRC_TEXTURE,		  gTexture, 
					PSYS_SRC_PATTERN,		  PSYS_SRC_PATTERN_DROP,		
					PSYS_SRC_BURST_PART_COUNT, 1,
					PSYS_SRC_BURST_RATE,	   0,
					PSYS_PART_START_COLOR,	 <gRed, gGreen, gBlue>,
					PSYS_PART_END_COLOR,	   <gRed, gGreen, gBlue>, 
					PSYS_PART_FLAGS,
						PSYS_PART_FOLLOW_VELOCITY_MASK |
						PSYS_PART_FOLLOW_SRC_MASK |
						PSYS_PART_TARGET_POS_MASK |
						PSYS_PART_INTERP_SCALE_MASK
				];
}

executeCommands() {
	integer break;
	while(!break && llGetListLength(gCommandQueue)) {
		integer commandId=llList2Integer(gCommandQueue, 0);
		key avatarKey=llList2Key(gCommandQueue, 1);
		list params=llParseStringKeepNulls(llList2String(gCommandQueue, 2), [ gSET_MAIN_SEPARATOR ], [] );
		if( commandId == gCMD_REM_CHAINS ) {
			if(gTimerMode==gTIMER_MODE_IDLE || gTimerMode==gTIMER_MODE_CONTROL || gTimerMode==gTIMER_MODE_REMOVE) {
				query_rem_chains(params);
				gCommandQueue=llDeleteSubList(gCommandQueue, 0, 2);
			}
			else {
				break=TRUE;
			}
		}
		else if( commandId == gCMD_SET_CHAINS ) {
			if(gTimerMode==gTIMER_MODE_IDLE || gTimerMode==gTIMER_MODE_CONTROL || gTimerMode==gTIMER_MODE_ADD) {
				query_set_chains(avatarKey, params);
				gCommandQueue=llDeleteSubList(gCommandQueue, 0, 2);
			}
			else {
				break=TRUE;
			}
		}
		else if( commandId == gCMD_CONFIG ) {
			if(gTimerMode==gTIMER_MODE_IDLE || gTimerMode==gTIMER_MODE_CONTROL) {
				query_config(params);
				gCommandQueue=llDeleteSubList(gCommandQueue, 0, 2);
			}
			else {
				break=TRUE;
			}
		}
	}
}

integer countMissingChains(integer forceTimeout) {
	 //reports the number of missing chains (status==0 and status==1)
	//if forceTimeout==TRUE: set the status from 1(request) to 0(missingChain)
	integer numberOfMissingChains;
	integer index;
	integer length=llGetListLength(gChains);
	for(index=0; index<length; index+=5) {
		integer status=llList2Integer(gChains, index+3);
		if(status==0) {
			numberOfMissingChains++;
		}
		else if(status==1 && forceTimeout) {
			gChains=llListReplaceList(gChains, [0], index+3, index+3);
			numberOfMissingChains++;
		}
	}
	return numberOfMissingChains;
}
// ============================================================
default {
	state_entry() {
		integer number_of_prims = llGetNumberOfPrims();
		integer i;
		for( i=0; i < number_of_prims + 1; ++i ) { //Walk throug the whole linkset including a single prim
			string desc = llList2String( llGetLinkPrimitiveParams( i, [ PRIM_DESC ] ), 0 );
			if( desc != "" && desc != "(No description)" ) {
				if( -1 == llListFindList( gPrimIDs, [ desc ] ) ) { // only accept unique descriptions
					gPrimIDs += [ "|" + desc, i ];
				}
				else {
					llOwnerSay( "/me Error: prim description " + desc
						+ " isn't unique, please make it unique... ignoring" );  
				}
			}
		}
		set_particle();
		llSleep(1.5);
		llMessageLinked(LINK_SET, gPLUGIN_COMMAND_REGISTER, llDumpList2String(["CHAINS_ADD", gCMD_SET_CHAINS, 1, 0], "|"), "");
		llMessageLinked(LINK_SET, gPLUGIN_COMMAND_REGISTER, llDumpList2String(["CHAINS_REMOVE", gCMD_REM_CHAINS, 1, 0], "|"), "");
		llMessageLinked(LINK_SET, gPLUGIN_COMMAND_REGISTER, llDumpList2String(["CHAINS_CONFIG", gCMD_CONFIG, 1, 0], "|"), "");
//	  gPrimIDs = llListSort( gPrimIDs, 2, TRUE );
//	  llOwnerSay( "PrimIDs: " + llDumpList2String( gPrimIDs, "\t" ) );
	}

	link_message( integer primId, integer commandId, string message, key avatarKey ) {
		if( commandId == gCMD_REM_CHAINS || commandId == gCMD_SET_CHAINS || commandId == gCMD_CONFIG) {
			gCommandQueue+=[commandId, avatarKey, message];
			executeCommands();
		}
	}
	
	listen( integer channel, string cuffName, key cuffKey, string message ) {
		if( channel == gLOCKGUARD_CHANNEL ) {
			list parts = llParseString2List( message, [ " " ], [] );
			if(llToLower(llList2String(parts, 0))=="lockguard") {
				key avatar = (key)llList2String( parts, 1 );
				string lgmooring = llList2String( parts, 2 );
				integer index = llListFindList( gLM_TO_LG_MAPPINGS, [ lgmooring ] );
				if( index != -1 ) {
					string lmMooring  = llList2String( gLM_TO_LG_MAPPINGS, index - 1 );
					integer length=llGetListLength(gChains);
					integer removeLgChain;
					for(index=0; index<length; index+=5) {
						if(llList2Key(gChains, index)==avatar) {
							if(llList2String(gChains, index+2)==lmMooring) {
								if(llList2Integer(gChains, index+3)==3) {
									removeLgChain=TRUE;
								}
								else {
									gChains=llListReplaceList(gChains, [2], index+3, index+3);
									//debug(["lgChain confirmed.", lmMooring]);
								}
							}
						}
					}
					if(removeLgChain) {
						lgRemoveChain(avatar, lmMooring);
						//debug(["lgChain removed because lm already set.", lmMooring]);
					}
				}
			}
		}
		else if( channel == gLOCKMEISTER_CHANNEL ) {
			if( llGetSubString( message, -3, -1 ) == " ok" ) {//it's an old style v1 LM reply
				key avatar=(key)llGetSubString( message, 0, 35 );
				string lmMooring=llGetSubString( message, 36, -4 );
				integer removeLgChain;
				integer index;
				integer length=llGetListLength(gChains);
				for(index=0; index<length; index+=5) {
					if(llList2Key(gChains, index)==avatar) {
						if(llList2String(gChains, index+2)==lmMooring) {
							if(llList2Integer(gChains, index+3)==2) {
								//remove lgChain
								removeLgChain=TRUE;
							}
							gChains=llListReplaceList(gChains, [3, (string)cuffKey], index+3, index+4);
							// send lockmeister chain
							integer primID=llList2Integer(gPrimIDs, llListFindList(gPrimIDs, [llList2String(gChains, index+1)]) + 1);
							llLinkParticleSystem( primID, gParticles + [ PSYS_SRC_TARGET_KEY, cuffKey ] );
							// now send a v2 style LM message, because if the target attachment is using v2 style messages,
							// then the chains will be better targetted
							lmV2Request(cuffKey, avatar, lmMooring);
							//debug(["lmV1Chain set", primID] + llList2List(gChains, index, index+4));
						}
					}
				}
				if(removeLgChain) {
					lgRemoveChain(avatar, lmMooring);
					//debug(["lgChain removed because lm already set.", lmMooring]);
				}
			}
			else {
				//v2 style LM reply ?
				list parts = llParseString2List( message, ["|"], [] );
				if( llList2String( parts, 1 ) == "LMV2" && llList2String( parts, 2 ) == "ReplyPoint" ) {
					//looks like it is
					key avatar=(key)llList2String(parts, 0);
					string lmMooring=llList2String(parts, 3);
					key replyPointKey=(key)llList2String(parts, 4);
					integer index;
					integer length=llGetListLength(gChains);
					for(index=0; index<length; index+=5) {
						if(llList2Key(gChains, index)==avatar) {
							if(llList2String(gChains, index+2)==lmMooring) {
								//renew the target of the particle cahin
								gChains=llListReplaceList(gChains, [3, (string)replyPointKey], index+3, index+4);
								// send lockmeister chain
								integer primID=llList2Integer(gPrimIDs, llListFindList(gPrimIDs, [llList2String(gChains, index+1)]) + 1);
								llLinkParticleSystem( primID, gParticles + [ PSYS_SRC_TARGET_KEY, replyPointKey ] );
								//debug(["lmV2Chain set", primID] + llList2List(gChains, index, index+4));
							}
						}
					}
				}
			}
		}
	}

	timer() {
		//debug(["Timer triggred, mode: ", gTimerMode]);
		if ( gTimerMode == gTIMER_MODE_CONTROL ) {
			setTimerMode(gTIMER_MODE_ADD);
			control_chains();
		}
		else {
			setTimerMode(gTIMER_MODE_IDLE);
			llListenRemove( gListenLMHandle );
			gListenLMHandle = FALSE;
			llListenRemove( gListenLGHandle );
			gListenLGHandle = FALSE;
			
			if(countMissingChains(TRUE)) {
				setTimerMode(gTIMER_MODE_CONTROL);
			}
		}
		executeCommands();
	}
	
	on_rez(integer param) {
		llResetScript();
	}
/*
	changed(integer change) {
		if (change & CHANGED_LINK) {
			//TODO: it would be nice if we could reread the linknubers/descriptions if the user make changes in the linkset
			//but this also triggers if someone just sit on the object
		}
	}
*/
} // default
// ============================================================
