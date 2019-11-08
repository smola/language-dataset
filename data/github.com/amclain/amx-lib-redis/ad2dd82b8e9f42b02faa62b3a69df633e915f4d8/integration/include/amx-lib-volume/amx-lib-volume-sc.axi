(***********************************************************
    AMX VOLUME CONTROL LIBRARY
    v1.0.0
    
    Website: https://github.com/amclain/amx-lib-volume
    
    This is a snake case wrapper for the volume control
    library functions. To use this file, make sure to include
    amx-lib-volume.axi in the workspace.
    
    For more information, see:
        amx-lib-volume.axi
    
*************************************************************
Copyright 2011, 2012, 2014 Alex McLain

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************)

#include 'amx-lib-volume'

#if_not_defined AMX_LIB_VOLUME_SC
#define AMX_LIB_VOLUME_SC 1
(***********************************************************)

/*
 *  Initialize volume control.
 *
 *  Parameters min, max, and numSteps can be set to 0 if not needed.
 */
define_function vol_init(volume v, integer lvl, char muteState, integer min, integer max, integer numSteps)
{
    volInit(v, lvl, muteState, min, max, numSteps);
}

/*
 *  Get pre-mute volume level.
 *  Returns current volume level.
 *
 *  This function ignores mute status but respects min/max limits
 *  and dim state.
 */
define_function integer vol_get_level(volume v)
{
    return volGetLevel(v);
}

/*
 *  Get post-mute volume level.
 *  Returns current volume level.
 *
 *  This function takes into account mute status, min/max limits,
 *  and dim state.
 */
define_function integer vol_get_level_post_mute(volume v)
{
    return volGetLevelPostMute(v);
}

/*
 *  Get pre-mute volume level.
 *  Returns current volume level.
 *
 *  This function ignores mute status but respects min/max limits.
 *  Return value is scaled from an integer to a byte.
 */
define_function char vol_get_level_as_byte(volume v)
{
    return volGetLevelAsByte(v);
}

/*
 *  Get post-mute volume level.
 *  Returns current volume level.
 *
 *  This function takes into account mute status and min/max limits.
 *  Return value is scaled from an integer to a byte.
 */
define_function sinteger vol_get_level_post_mute_as_byte(volume v)
{
    return volGetLevelPostMuteAsByte(v);
}

/*
 *  Get a volume level that fits on a touch panel's bargraph.
 *  Returns current volume level scaled to a range of 0-255
 *      taking into account min/max.
 */
define_function char vol_get_touch_panel_level(volume v)
{
    return volGetTouchPanelLevel(v);
}

/*
 *  Set volume level.
 *  Returns status:
 *  (VOL_SUCCESS | VOL_LIMITED)
 *
 *  This function takes into account min/max limits.
 *  This function does not affect mute status.
 */
define_function sinteger vol_set_level(volume v, integer value)
{
    return volSetLevel(v, value);
}

/*
 *  Set volume level.
 *  Returns status:
 *  (VOL_SUCCESS | VOL_LIMITED)
 *
 *  This function takes into account min/max limits.
 *  This function does not affect mute status.
 *  Input value is scaled from a byte to an integer.
 */
define_function sinteger vol_set_level_as_byte(volume v, char value)
{
    return volSetLevelAsByte(v, value);
}

/*
 *  Set max limit.
 *  Input: value > 0 to enable, value = 0 to disable.
 *  Returns VOL_SUCCESS | VOL_LIMITED.
 */
define_function sinteger vol_set_max(volume v, integer value)
{
    return volSetMax(v, value);
}

/*
 *  Set max limit.
 *  Input: value > 0 to enable, value = 0 to disable.
 *  Returns VOL_SUCCESS | VOL_LIMITED.
 *  
 *  Input value is scaled from a byte to an integer.
 */
define_function sinteger vol_set_max_as_byte(volume v, char value)
{
    return volSetMaxAsByte(v, value);
}

/*
 *  Set minimum limit.
 *  Input: value > 0 to enable, value = 0 to disable.
 *  Returns VOL_SUCCESS | VOL_LIMITED.
 */
define_function sinteger vol_set_min(volume v, integer value)
{
    return volSetMin(v, value);
}

/*
 *  Set minimum limit.
 *  Input: value > 0 to enable, value = 0 to disable.
 *  Returns VOL_SUCCESS | VOL_LIMITED.
 *  
 *  Input value is scaled from a byte to an integer.
 */
define_function sinteger vol_set_min_as_byte(volume v, char value)
{
    return volSetMinAsByte(v, value);
}

/*
 *  Mute the channel.
 */
define_function vol_mute(volume v)
{
    volMute(v);
}

/*
 *  Unmute the channel.
 */
define_function vol_unmute(volume v)
{
    volUnmute(v);
}

/*
 *  Toggle the channel's mute state.
 *
 *  Returns the resulting mute state after toggle:
 *  (VOL_MUTED | VOL_UNMUTED)
 */
define_function integer vol_toggle_mute(volume v)
{
    return volToggleMute(v);
}

/*
 *  Get the control's mute state.
 *  Returns sinteger:
 *  (VOL_MUTED | VOL_UNMUTED)
 */
define_function integer vol_get_mute_state(volume v)
{
    return volGetMuteState(v);
}

/*
 *  Set the amount that the level increases/decreases when incremented.
 */
define_function vol_set_step(volume v, integer value)
{
    volSetStep(v, value);
}

/*
 *  Set the amount that the level increases/decreases when incremented.
 *
 *  Input value is scaled from a byte to an integer.
 */
define_function vol_set_step_as_byte(volume v, char value)
{
    volSetStepAsByte(v, value);
}

/*
 *  Set the number of steps the control can be incremented/decremented.
 *
 *  This is an alternative to defining the value of the step.
 */
define_function sinteger vol_set_number_of_steps(volume v, integer steps)
{
    return volSetNumberOfSteps(v, steps);
}

/*
 *  Increase the volume by incrementing the level by one step.
 *  Is not affected by mute state.
 *  Returns status:
 *  (VOL_SUCCESS | VOL_LIMITED | VOL_PARAM_NOT_SET)
 */
define_function sinteger vol_increment(volume v)
{
    return volIncrement(v);
}

/*
 *  Decrease the volume by decrementing the level by one step.
 *  Is not affected by mute state.
 *  Returns status:
 *  (VOL_SUCCESS | VOL_LIMITED | VOL_PARAM_NOT_SET)
 */
define_function sinteger vol_decrement(volume v)
{
    return volDecrement(v);
}

/*
 *  Dim the volume level.
 */
define_function vol_dim(volume v)
{
    volDim(v);
}

/*
 *  Undim the volume level, returning it to its "normal" level.
 */
define_function vol_undim(volume v)
{
    volUndim(v);
}

/*
 *  Get volume dim state.
 *  Returns status:
 *  (VOL_DIM_ON | VOL_DIM_OFF)
 */
define_function sinteger vol_get_dim_state(volume v)
{
    return volGetDimState(v);
}

/*
 *  Get the amount that the level is dimmed when dim is on.
 */
define_function integer vol_get_dim_amount(volume v)
{
    return volGetDimAmount(v);
}

/*
 *  Get the amount that the level is dimmed when dim is on.
 *
 *  Returns a byte scaled from an integer.
 */
define_function char vol_get_dim_amount_as_byte(volume v)
{
    return volGetDimAmountAsByte(v);
}

/*
 *  Set the amount that the level dims.
 */
define_function sinteger vol_set_dim_amount(volume v, integer amount)
{
    return volSetDimAmount(v, amount);
}

/*
 *  Set the amount that the level dims.
 *
 *  Input is scaled from a byte to an integer.
 */
define_function sinteger vol_set_dim_amount_as_byte(volume v, char amount)
{
    return volSetDimAmountAsByte(v, amount);
}

/*
 *  Initialize an array of volume controls.
 *  
 *  Parameters min, max, and numSteps can be set to 0 if not needed.
 */
define_function vol_array_init(volume v[], integer lvl, char muteState, integer min, integer max, integer numSteps)
{
    volArrayInit(v, lvl, muteState, min, max, numSteps);
}

/*
 *  Get the volume level for a control in an array at the given index.
 *  Returns current volume level.
 *
 *  This function takes into account mute status and min/max limits.
 */
define_function integer vol_array_get_level(volume v[], integer index)
{
    return volArrayGetLevel(v, index);
}

/*
 *  Get the volume level for a control in an array at the given index.
 *  Returns current volume level.
 *
 *  This function takes into account mute status and min/max limits.
 *  Return value is scaled from an integer to a byte.
 */
define_function char vol_array_get_level_as_byte(volume v[], integer index)
{
    return volArrayGetLevelAsByte(v, index);
}

/*
 *  Set the volume level for all controls in an array.
 */
define_function sinteger vol_array_set_level(volume v[], integer value)
{
    return volArraySetLevel(v, value);
}

/*
 *  Set the volume level for all controls in an array.
 */
define_function sinteger vol_array_set_level_as_byte(volume v[], char value)
{
    return volArraySetLevelAsByte(v, value);
}

/*
 *  Set the max volume limit for all controls in an array.
 *
 *  Returns VOL_LIMITED if any instance in the array is limited.
 *  Otherwise returns VOL_SUCCESS.
 */
define_function sinteger vol_array_set_max(volume v[], integer value)
{
    return volArraySetMax(v, value);
}

/*
 *  Set the max volume limit for all controls in an array.
 *
 *  Returns VOL_LIMITED if any instance in the array is limited.
 *  Otherwise returns VOL_SUCCESS.
 */
define_function sinteger vol_array_set_max_as_byte(volume v[], char value)
{
    return volArraySetMaxAsByte(v, value);
}

/*
 *  Set the min volume limit for all controls in an array.
 *
 *  Returns VOL_LIMITED if any instance in the array is limited.
 *  Otherwise returns VOL_SUCCESS.
 */
define_function sinteger vol_array_set_min(volume v[], integer value)
{
    return volArraySetMin(v, value);
}

/*
 *  Set the min volume limit for all controls in an array.
 *
 *  Returns VOL_LIMITED if any instance in the array is limited.
 *  Otherwise returns VOL_SUCCESS.
 */
define_function sinteger vol_array_set_min_as_byte(volume v[], char value)
{
    return volArraySetMinAsByte(v, value);
}

/*
 *  Set the volume step amount for all controls in an array.
 */
define_function vol_array_set_step(volume v[], integer value)
{
    volArraySetStep(v, value);
}

/*
 *  Set the volume step amount for all controls in an array.
 */
define_function vol_array_set_step_as_byte(volume v[], char value)
{
    volArraySetStepAsByte(v, value);
}

/*
 *  Set the number of steps all controls in the array can be
 *  incremented or decremented.
 */
define_function sinteger vol_array_set_number_of_steps(volume v[], integer steps)
{
    return volArraySetNumberOfSteps(v, steps);
}

/*
 *  Mute all of the controls in the array.
 */
define_function vol_array_mute(volume v[])
{
    volArrayMute(v);
}

/*
 *  Unmute all of the controls in the array.
 */
define_function vol_array_unmute(volume v[])
{
    volArrayUnmute(v);
}

/*
 *  Increase the volume of all controls in the array
 *  by one step.
 */
define_function sinteger vol_array_increment(volume v[])
{
    return volArrayIncrement(v);
}

/*
 *  Decrease the volume of all controls in the array
 *  by one step.
 */
define_function sinteger vol_array_decrement(volume v[])
{
    return volArrayDecrement(v);
}

/*
 *  Dim the volume level of all controls in the array.
 */
define_function vol_array_dim(volume v[])
{
    volArrayDim(v);
}

/*
 *  Undim the volume level of all controls in the array,
 *  returning them to their "normal" level.
 */
define_function vol_array_undim(volume v[])
{
    volArrayUndim(v);
}

/*
 *  Set the amount that the level dims for an array.
 */
define_function vol_array_set_dim_amount(volume v[], integer amount)
{
    volArraySetDimAmount(v, amount);
}

/*
 *  Set the amount that the level dims for an array.
 *
 *  Input is scaled from a byte to an integer.
 */
define_function vol_array_set_dim_amount_as_byte(volume v[], char amount)
{
    volArraySetDimAmountAsByte(v, amount);
}

(***********************************************************)
#end_if
