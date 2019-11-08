#pragma rtGlobals=3		// Use modern global access method and strict wave access.

Function /WAVE PulseGetParamNames()
	Variable nParameters=3
	Make /T /FREE /N=(nParameters) parameterNames
	parameterNames[0]="delay"
	parameterNames[1]="duration"
	parameterNames[2]="amplitude"
	return parameterNames
End

Function /WAVE PulseGetParamDispNames()
	Variable nParameters=3
	Make /T /FREE /N=(nParameters) parameterNames
	parameterNames[0]="Delay"
	parameterNames[1]="Duration"
	parameterNames[2]="Amplitude"
	return parameterNames
End

//Function /WAVE PulseGetDfltParams()
//	Variable nParameters=3
//	Make /FREE /N=(nParameters) parametersDefault
//	parametersDefault[0]=20		// ms
//	parametersDefault[1]=100		// ms
//	parametersDefault[2]=1
//	return parametersDefault
//End

Function /WAVE PulseGetDfltParamsAsStr()
	Variable nParameters=3
	Make /T /FREE /N=(nParameters) parametersDefault
	parametersDefault[0]="20"			// ms
	parametersDefault[1]="100"		// ms
	parametersDefault[2]="1"
	return parametersDefault
End

Function PulseAreParamsValid(paramsAsStrings)
	Wave /T paramsAsStrings

	Wave parameters=DoubleWaveFromTextWave(paramsAsStrings)
	
	Variable delay=parameters[0]
	Variable duration=parameters[1]
	Variable amplitude=parameters[2]

	return (duration>=0)
End

Function /WAVE PulseGetParamUnits()
	Variable nParameters=3
	Make /T /FREE /N=(nParameters) paramUnits
	paramUnits[0]="ms"
	paramUnits[1]="ms"
	paramUnits[2]=""
	return paramUnits
End

//Function PulseFillFromParams(w,parameters)
//	Wave w
//	Wave parameters
//
//	Variable delay=parameters[0]
//	Variable duration=parameters[1]
//	Variable amplitude=parameters[2]
//
//       // Somewhat controversial, but in the common case that pulse starts are sample-aligned, and pulse durations are
//       // an integer multiple of dt, this ensures that each pulse is exactly pulseDuration samples long
//	Variable dt=deltax(w)      	
//	Variable delayTweaked=delay-dt/2
//
//	w=amplitude*unitPulse(x-delayTweaked,duration)
//End

Function PulseOverlayFromParams(w,paramsAsStrings)
	Wave w
	Wave /T paramsAsStrings

	Wave parameters=DoubleWaveFromTextWave(paramsAsStrings)

	Variable delay=parameters[0]
	Variable duration=parameters[1]
	Variable amplitude=parameters[2]

       // Somewhat controversial, but in the common case that pulse starts are sample-aligned, and pulse durations are
       // an integer multiple of dt, this ensures that each pulse is exactly pulseDuration samples long
	Variable dt=deltax(w)      	
	Variable delayTweaked=delay-dt/2

	w += amplitude*unitPulse(x-delayTweaked,duration)
End

Function /S PulseGetSignalType()
	return "DAC"
End
