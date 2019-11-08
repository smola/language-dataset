////////////////////////////////////////////////////////////////////////////////////
// ------------------------------------------------------------------------------ //
//                              OpenNC - update                                   //
//                              version 3.980                                     //
// ------------------------------------------------------------------------------ //
// Licensed under the GPLv2 with additional requirements specific to Second Life® //
// and other virtual metaverse environments.                                      //
// ------------------------------------------------------------------------------ //
// ©   2008 - 2013  Individual Contributors and OpenCollar - submission set free™ //
// ©   2013 - 2014  OpenNC                                                        //
// ------------------------------------------------------------------------------ //
// Not now supported by OpenCollar at all                                         //
////////////////////////////////////////////////////////////////////////////////////
integer g_iListener0;
key wearer;
string g_sUpdaterName="OpenNC Updater";
integer LM_CUFF_CMD = -551001;        // used as channel for linkemessages - sending commands
integer MENUNAME_RESPONSE = 3001;
key g_kMenuID;
string CTYPE = "cuffs";
integer g_iUpdateChan = -7483216;
integer g_iUpdateHandle;
key g_kConfirmUpdate;
key g_kUpdaterOrb;
key g_kUpdater; // key of avi who asked for the update
integer g_iUpdatersNearBy = -1;
integer g_iWillingUpdaters = -1;

key Dialog(key kRCPT, string sPrompt, list lChoices)
{
    key kID = llGenerateKey();
    llDialog(wearer, sPrompt, lChoices, 69); //Lets listen
    return kID;
} 

Notify(key kID, string sMsg)
{
    if (kID == wearer)
        llOwnerSay(sMsg);
}

ConfirmUpdate(key kUpdater)
{
    string sPrompt = "3 Golden Rules for Updates:\n1.Create a Backup\n2.Rezz next to the updater.\n3.Low Lag regions make happy Updates\nATTENTION: Do not rez any collars or cuffs until the update has started.\nReady?";
    g_kConfirmUpdate = Dialog(wearer, sPrompt, ["Yes", "No"]);
}

integer IsOpenCollarScript(string sName)
{
    if (llList2String(llParseString2List(sName, [" - "], []), 0) == "OpenCollar")
        return TRUE;
    return FALSE;
}

SayUpdatePin(key orb)
{
    integer pin = (integer)llFrand(99999998.0) + 1; //set a random pin
    llSetRemoteScriptAccessPin(pin);
    llRegionSayTo(orb, g_iUpdateChan, "ready|" + (string)pin ); //give the ok to send update sripts etc...
}

string LeftOfDecimal(string str)
{
    integer idx = llSubStringIndex(str, ".");
    if (idx == -1)
        return str;
    return llGetSubString(str, 0, idx - 1);
}

string RightOfDecimal(string str)
{
    integer idx = llSubStringIndex(str, ".");
    if (idx == -1)
        return "0";
    return llGetSubString(str, idx + 1, -1);
}

integer SecondStringBigger(string s1, string s2)
{
    // first compare the pre-decimal parts.
    integer i1 = (integer)LeftOfDecimal(s1);
    integer i2 = (integer)LeftOfDecimal(s2);

    if (i2 == i1)
    {
        // pre-decimal parts are the same.  Need to compare the bits after.
        integer j1 = (integer)RightOfDecimal(s1);
        integer j2 = (integer)RightOfDecimal(s2);
        return j2 > j1;
    }
    else return i2 > i1;
}

// used in the 'objectversion' command.
integer GetOwnerChannel(key kOwner, integer iOffset)
{
    integer iChan = (integer)("0x"+llGetSubString((string)kOwner,2,7)) + iOffset;
    if (iChan>0)
        iChan=iChan*(-1);
    if (iChan > -10000)
        iChan -= 30000;
    return iChan;
}

default
{
    state_entry()
    {
        llListenRemove(g_iListener0);
        //check if we're in an updater.  if so, then just shut self down and
        //don't do regular startup routine.
        if (llSubStringIndex(llGetObjectName(), "Updater") != -1)
            llSetScriptState(llGetScriptName(), FALSE);
        // we just started up.  Remember owner.
        wearer = llGetOwner();
        g_iListener0 = llListen(69, "", "", "");//yeh why not?
        llMessageLinked(LINK_THIS, LM_CUFF_CMD, "reset", "");
    }

    on_rez(integer param)
    {
        if (llGetAttached() >= 1)
            llSetScriptState(llGetScriptName(),FALSE);
        llListenRemove(g_iListener0);
        llResetScript();
    }
    
    touch_start(integer nCnt)
    {
        key id = llDetectedKey(0);
        if (llGetAttached() == 0) // Only do if rezzed
        {
            if (id == wearer)
                {
                    string sVersion = llList2String(llParseString2List(llGetObjectDesc(), ["~"], []), 1);
                    g_iUpdatersNearBy = 0;
                    g_iWillingUpdaters = 0;
                    g_kUpdater = id;
                    Notify(id,"Searching for a nearby updater.");
                    g_iUpdateHandle = llListen(g_iUpdateChan, "", "", "");
                    llRegionSay(g_iUpdateChan, "UPDATE|" + sVersion);
                    llSetTimerEvent(10.0); //set a timer to close the g_iListener if no response
                }
                else
                    Notify(id,"Only the wearer can update the " + CTYPE + ".");
            }
        }

    link_message(integer sender, integer num, string str, key id )
    {   //llSay(0,(string)num+ " "+str);
        if (num == MENUNAME_RESPONSE)
        {
            if ((str == "Main|Cuff Poses") || (str == "Main| Poses") || (str == "Main|Cuffs")) // change the updater channel here since we are in a main cuff
                g_iUpdateChan = -7483215;
        }
    }

    listen(integer channel, string name, key id, string message)
    {   //cuff and updater have to have the same Owner else do nothing!
        if (llGetOwnerKey(id) == wearer)
        {
            list lTemp = llParseString2List(message, [","],[]);
            string sCommand = llList2String(lTemp, 0);
            if(message == "nothing to update")
                g_iUpdatersNearBy++;
            else if( message == "get ready")
            {
                g_iUpdatersNearBy++;
                g_iWillingUpdaters++;
                g_kUpdaterOrb = id;
                ConfirmUpdate(g_kUpdaterOrb); 
                g_iUpdatersNearBy = -1;
                g_iWillingUpdaters = -1;
                llSetTimerEvent(0);
            }
        }
            if (message == "Yes")
                    SayUpdatePin(g_kUpdaterOrb);
    }

    timer()
    {
        llSetTimerEvent(0.0);
        llListenRemove(g_iUpdateHandle);
        if (g_iUpdatersNearBy > -1)
        {
            if (!g_iUpdatersNearBy)
                Notify(g_kUpdater,"No updaters found.  Please rez a cuff updater within 10m and try again");
            else if (g_iWillingUpdaters > 1)
                    Notify(g_kUpdater,"Multiple updaters were found within 10m.  Please remove all but one and try again");
            g_iUpdatersNearBy = -1;
            g_iWillingUpdaters = -1;
        }
    }
}