PROGRAM_NAME='IP Socket Manager - Header'

DEFINE_FUNCTION fnIPSM_Debug (dev vdvDev, integer nDebug)
{
	if (nDebug)
		send_command vdvDev, 'Debug 1'
	else
		send_command vdvDev, 'Debug 0'
}

DEFINE_FUNCTION fnIPSM_KeepAlive (dev vdvDev, integer nKeepAlive)
{
	if (nKeepAlive)
		send_command vdvDev, 'KeepAlive 1'
	else
		send_command vdvDev, 'KeepAlive 0'
}

DEFINE_FUNCTION fnIPSM_Delay (dev vdvDev, long nDelay)
{
	send_command vdvDev, "'Delay ', itoa(nDelay)"
}

DEFINE_FUNCTION fnIPSM_Reinitialize (dev vdvDev)
{
	send_command vdvDev, 'Reinit'
}

DEFINE_FUNCTION fnIPSM_OpenConnection (dev vdvDev)
{
	send_command vdvDev, 'OpenConnection'
}

DEFINE_FUNCTION fnIPSM_CloseConnection (dev vdvDev)
{
	send_command vdvDev, 'CloseConnection'
}
