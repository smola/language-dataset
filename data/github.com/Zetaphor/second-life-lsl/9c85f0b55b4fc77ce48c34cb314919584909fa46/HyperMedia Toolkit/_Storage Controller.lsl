integer storage_num; //Number of storage scripts in inventory
integer storage_ready; //Integer of number of storage scripts loaded
string page_note = "_Pages";
string config_note = "_Config";

integer nline;
key nquery;
list pages;
string read = "config";
string errorp;

setperms(string perm)
{
    if (llToLower(perm) == "owner")
    {
        llMessageLinked(LINK_THIS,900000,(string)PRIM_MEDIA_PERM_OWNER,"");
    }
    else if (llToLower(perm) == "group")
    {
        llMessageLinked(LINK_THIS,900000,(string)PRIM_MEDIA_PERM_GROUP,"");        
    }
    else if (llToLower(perm) == "all")
    {
        llMessageLinked(LINK_THIS,900000,(string)PRIM_MEDIA_PERM_ANYONE,"");        
    }
    else if (llToLower(perm) == "none")
    {
        llMessageLinked(LINK_THIS,900000,(string)PRIM_MEDIA_PERM_NONE,"");        
    }
}

default
{    
    state_entry()
    {
        integer i;
        integer storcheck = llGetInventoryNumber(INVENTORY_SCRIPT);
        for (i=0; i<storcheck; i++)
        {
            if (llListFindList(llParseString2List(llGetInventoryName(INVENTORY_SCRIPT,i),[" "],[]),["~StorageObject"]) != -1)
            {
                ++storage_num;
            }
        }
    }
    
    link_message(integer se, integer n, string str, key id)
    {
        if (n==500000)
        {
            if (llGetInventoryType(page_note)!=INVENTORY_NONE)
            {
                llOwnerSay("Loading Configuration...");
                nline = 0;
                read = "config";
                nquery = llGetNotecardLine(config_note,0);
            }
        }
        else if (n==100000)
        {
            integer i;
            integer found;
            for (i=0; i<llGetListLength(pages); ++i)
            {
                if (str == llList2String(pages,i))
                {
                    found = TRUE;
                    i = llGetListLength(pages)+1;
                }
            }
            if (found == FALSE)
            {
                llMessageLinked(LINK_THIS,100001,"404",id);
            }
        }
        else if (id == "22222222-2222-2222-2222-222222222222")
        {
            ++storage_ready;
            if (storage_ready == storage_num)
            {
                llOwnerSay("Pages read into storage memory, starting server");
                llMessageLinked(LINK_THIS,500001,errorp,"");
            }
            else
            {
                llOwnerSay("Read "+(string)storage_ready+"/"+(string)storage_num);
            }
        }
    }
    
    dataserver(key q, string str)
    {
        if (q == nquery)
        {
            if (str!=EOF)
            {
                if (read == "pages")
                {                
                    list pdata = llParseString2List(str,[","],[]);
                    pages = llListInsertList(pages,[llList2String(pdata,1)],llList2Integer(pdata,0));
                    ++nline;
                    nquery = llGetNotecardLine(page_note,nline);
                }
                else if (read == "config")
                {
                    list data = llParseString2List(str,[","],[]);
                    if (llList2String(data,0) == "404")
                    {
                        errorp = llList2String(data,1);
                        llOwnerSay("404 page set to: "+errorp);                        
                    }
                    else if (llList2String(data,0) == "Interact")
                    {
                        setperms(llList2String(data,1));
                        llOwnerSay("Interact permission set to "+llList2String(data,1));
                    }
                    ++nline;
                    nquery = llGetNotecardLine(config_note,nline);                    
                }
            }
            else
            {
                if (read == "config")
                {
                    read = "pages";
                    llOwnerSay("Initializing Storage Objects");
                    nline = 0;
                    nquery = llGetNotecardLine(page_note,0);
                }
                else if (read == "pages")
                {
                    integer i;
                    for (i=0; i<llGetListLength(pages); i++)
                    {
                        llMessageLinked(LINK_THIS,i,llList2String(pages,i),"11111111-1111-1111-1111-111111111111");
                    } 
                }
            }
        }
    }
}
