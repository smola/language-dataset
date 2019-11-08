PROGRAM_NAME='RelayConfig'

// Relay config definitions.

#if_not_defined __RELAY_CONFIG__
#define __RELAY_CONFIG__

#include 'Debug.axi'
#include 'ConfigUtils.axi'
#include 'ConfigServerUtils.axi'

DEFINE_CONSTANT

MAX_RELAYS = 100

RELAY_TYPE_UNKNOWN	= 0
RELAY_TYPE_ACTIVE	= 1
RELAY_TYPE_PASSIVE	= 2

DEFINE_TYPE

structure RelayGeneral
{
    integer	mEnabled		// Whether relays are even present in this system
    dev		mDevControl		// Device for AMX internal control
    integer	mTpPort			// Port for Touch Panel events
}

structure RelayItem
{
    integer	mId
    char	mName[32]
    char	mShortName[32]
    int		mType
    devchan	mDevChan1
    devchan	mDevChan2
}

DEFINE_CONSTANT

READING_NONE			= 0
READING_GENERAL			= 1
READING_RELAY			= 2

DEFINE_VARIABLE

volatile RelayGeneral	   gGeneral
volatile RelayItem	   gRelays[MAX_RELAYS]
volatile integer	   gThisItem = 0
volatile integer	   gReadMode = READING_NONE

DEFINE_FUNCTION handleHeading (char moduleName[], char heading[])
{
    debug (moduleName, 8, "'read config heading: <',heading,'>'")
    switch (heading)
    {
    case 'general':
    {
	gReadMode = READING_GENERAL
	break
    }
    case 'relay':
    {
	gReadMode = READING_RELAY
	break
    }
    default:
    {
	debug (moduleName, 0, "'unknown config heading: ',heading")
    }
    }
}

DEFINE_FUNCTION handleProperty (char moduleName[], char propName[], char propValue[])
{    
    debug (moduleName, 8, "'read config property (',propName,'): <',propValue,'>'")
    switch (gReadMode)
    {
    case READING_GENERAL:
    {
	switch (propName)
	{
	case 'enabled':
	{
	    lower_string (propValue)
	    gGeneral.mEnabled = (propValue = 'true' || propValue = 't' || propValue = 1)
	    break
	}
	case 'dev-control':
	{
	    parseDev (gGeneral.mDevControl, propValue)
	    break
	}
	case 'tp-port':
	{
	    gGeneral.mTpPort = atoi(propValue)
	    break
	}
	} // switch
    }

    case READING_RELAY:
    {
	switch (propName)
	{
	case 'id':
	{
	    gThisItem = atoi(propValue)
	    gRelays[gThisItem].mId = gThisItem
	    break
	}
	case 'name':
	{
	    gRelays[gThisItem].mName = propValue
	    if (gRelays[gThisItem].mShortName = '')
	    {
		// Copy to the short name (may be overridden)
		gRelays[gThisItem].mShortName = propValue
	    }
	    break
	}
	case 'type':
	{
	    gRelays[gThisItem].mType = relayTypeFromStr (propValue)
	}
	case 'short-name':
	{
	    gRelays[gThisItem].mShortName = propValue
	    break
	}
	case 'dev-chan':
	{
	    parseDev (gRelays[gThisItem].mDevChan1.device, propValue)
	    remove_string (propValue,',',1)
	    gRelays[gThisItem].mDevChan1.channel = atoi(propValue)
	    break
	}
	case 'on-dev-chan':
	{
	    parseDev (gRelays[gThisItem].mDevChan1.device, propValue)
	    remove_string (propValue,',',1)
	    gRelays[gThisItem].mDevChan1.channel = atoi(propValue)
	    break
	}
	case 'off-dev-chan':
	{
	    parseDev (gRelays[gThisItem].mDevChan2.device, propValue)
	    remove_string (propValue,',',1)
	    gRelays[gThisItem].mDevChan2.channel = atoi(propValue)
	    break
	}
	} // switch
    }

    default:
    {
	debug (moduleName, 1, "'error: property with no heading (',propName,'): <',propValue,'>'")
    }
    }
}

DEFINE_FUNCTION integer relayTypeFromStr (char str[])
{
    lower_string (str)
    switch (str)
    {
    case 'passive':	return RELAY_TYPE_PASSIVE
    case 'active':	return RELAY_TYPE_ACTIVE
    default:		return RELAY_TYPE_UNKNOWN
    }
}

DEFINE_START
gGeneral.mTpPort = 1		// Anything but zero


#end_if // __RELAY_CONFIG__
