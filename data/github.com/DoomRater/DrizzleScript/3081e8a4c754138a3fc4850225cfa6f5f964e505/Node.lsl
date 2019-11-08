/*==========================================================
DrizzleScript v1.00
Created By: Ryhn Teardrop
Date: Dec 3rd, 2011

Programming Contributors: Ryhn Teardrop
Resource Contributors: Murreki Fasching

License: RPL v1.5 (Outlined at http://www.opensource.org/licenses/RPL-1.5)

The license can also be found in the folder these scripts are distributed in.
Essentially, if you edit this script you have to publicly release the result.
(Copy/Mod/Trans on this script) This stipulation helps to grow the community and
the software together, so everyone has access to something potentially excellent.


*Leave this header here, but if you contribute, add your name to the list!*
============================================================*/

/* 
*  5 instances of Node.lsl in separate prims are used to store carer
*  names. The prims containing these scripts must be linked
*  to the object with Menu.lsl in it. 
*/

//todo: update the code so as to not rely on hard coded 5 nodes- instead allow SaveSettings (which is the final node) to report that it has reached the end of the carer list
integer myNum;

//quick and dirty integer scrape from the script name to assign to myNum
//assumes there is a space between the script main name and the version, if there is anything
//past the script name.  SubStringIndex returns -1 if it's not found
integer getMyNum() {
    string scrapedNum = llGetScriptName();
    scrapedNum = llGetSubString(scrapedNum, 4, llSubStringIndex(scrapedNum, " "));
    return (integer) scrapedNum;
}

//relevant LSL limits:
//-the longest SL name possible is 63 characters including the space in between
//-the maximum length of prim description is 127 characters, long enough to store at least 2 carers
integer getListSize() {
    string storedData = llGetObjectDesc();
    if(storedData == "") {// Empty
        return 0;
    }
    else {// One item or more saved already
        list temp = llCSV2List(storedData);
        integer size = llGetListLength(temp);
        return size;
    }
}

updateColors() {
    integer size = getListSize();
    if(size < 1) {
        llSetColor(<0.0, 1.0, 0.0>, ALL_SIDES);
    }
    else if(size == 1) {
        llSetColor(<1.0, 1.0, 0.0>, ALL_SIDES);   
    }
    else {
        llSetColor(<1.0, 0.0, 0.0>, ALL_SIDES);   
    }
}

integer isFull() {
    if(getListSize() >= 2) {
        return TRUE;
    }
    else {
        return FALSE;
    }
}

removeInfo(string msg, key id) {
    list storedList = llCSV2List(llGetObjectDesc());
    integer i = llListFindList(storedList, [msg]);
    
    if(~i) {//Found
        llSetObjectDesc(llList2CSV(llDeleteSubList(storedList, i, i)));
        return;
    }
    else {// Forward
        if(myNum != 5) {
            llMessageLinked(LINK_ALL_CHILDREN, myNum+1, msg, id);
            return;
        }
    }
}

saveInfo(string msg) {
    string storedData = llGetObjectDesc();
    integer size = getListSize();
    if(size == 0) {// Empty
        storedData += msg;
        llSetObjectDesc(storedData);
    }
    else {
        storedData += "," + msg;
        llSetObjectDesc(storedData);
    }
}

forwardMessage(string msg) {
    llMessageLinked(LINK_ALL_CHILDREN, myNum+1, msg, NULL_KEY);
}

wipeList() {
    llSetObjectDesc("");
    llMessageLinked(LINK_ALL_CHILDREN, myNum+1, "WIPE", NULL_KEY);
}

sendList() {
    //Safe for 1 -> 5, their numbers will tell main what to do.
    
    llMessageLinked(LINK_ROOT, myNum, llGetObjectDesc(), NULL_KEY); // Send main this object's list
    llMessageLinked(LINK_ALL_CHILDREN, myNum+1, "SEND", NULL_KEY); // Pass the message on to the next object
}

default {
    changed(integer c) {
        if(c & CHANGED_OWNER) {
            llSetObjectDesc("");
        }
        updateColors();
    }
    
    state_entry() {
        myNum = getMyNum();
        llOwnerSay((string)myNum); //debug
        updateColors();
    }

    link_message(integer sender_num, integer num, string msg, key id) {
        if(msg == "SEND") {
            if(num == myNum) {
                sendList();
                return;
            }
            return;   
        }
        else if(msg == "WIPE") { //used to reset the carers
            if(num == myNum) {
                wipeList();
                updateColors();
            }
        }
        else if(id) { // Valid key in message is used to delete
            if(num == myNum) {
                removeInfo(msg, id);
                updateColors();
                return;
            }
        }
        else {
            if(num == myNum) {
                if(!isFull()) {
                    saveInfo(msg);
                    updateColors();
                    return;
                }
                else {
                    if(myNum != 5) {
                        forwardMessage(msg);
                        return;
                    }
                    else {
                        llMessageLinked(LINK_ROOT, 5, "I'm sorry! There is no more room for carers, please delete one.", NULL_KEY);
                        return;
                    }
                }
            } 
        }
    }
}
