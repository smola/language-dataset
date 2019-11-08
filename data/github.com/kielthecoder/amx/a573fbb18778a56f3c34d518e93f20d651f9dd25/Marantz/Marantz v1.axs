MODULE_NAME='Marantz v1' (DEV vdv, DEV dv)

DEFINE_CONSTANT

STOP   = 2
PAUSE  = 3
RECORD = 8

DEFINE_EVENT

// DEVICE EVENTS ////////////////////////////////////////////////////////////

DATA_EVENT[dv]
{
    ONLINE:
    {
	SEND_COMMAND dv,"'SET BAUD 9600,N,8,1'"
    }
}

// VIRTUAL DEVICE EVENTS ////////////////////////////////////////////////////

// CHANNEL EVENTS ///////////////////////////////////////////////////////////

CHANNEL_EVENT[vdv,STOP]
{
    ON:
    {
	SEND_STRING dv,"'@02354',$0D"
    }
}

CHANNEL_EVENT[vdv,PAUSE]
{
    ON:
    {
	SEND_STRING dv,"'@02348',$0D"
    }
}

CHANNEL_EVENT[vdv,RECORD]
{
    ON:
    {
	SEND_STRING dv,"'@02355',$0D"
    }
}
