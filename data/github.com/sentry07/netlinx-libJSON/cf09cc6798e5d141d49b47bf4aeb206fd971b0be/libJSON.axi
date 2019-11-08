PROGRAM_NAME='libJSON v2_0'
// ---------------------------------------------------------------------------------------------------------------------
// LIBRARY:	libJSON
// AUTHOR:	https://github.com/sentry07
// PURPOSE:	To provide functions to verify, parse, and build JSON encoded objects
//
// THIS LIBRARY RELIES ON THE FACT THAT FUNCTION PARAMETERS ARE BYREF, SO MOST DATA IS RETURNED THROUGH PARAMETERS
// MAKE SURE YOU READ THE NOTES ON ALL THE FUNCTIONS BEFORE USING THEM
//
// ---------------------------------------------------------------------------------------------------------------------
// LIBRARY NOTES:
// The two main functions you will probably use from this library are JSON_ParseObject and JSON_ParseArray.
//
// JSON_ParseObject takes in the JSON string and the return object as parameters, and returns the number of KV pairs the
// function parsed. JSON_ParseObject will call JSON_Validate before parsing to make sure it's a valid object, so calling
// JSON_Validate on a string is not necessary unless you are checking for a valid object before calling JSON_Parse.
// JSON_Validate returns compacted JSON with all extra whitespace removed. The resulting _JSON_Object has the K:V pair
// count in it, as well as an array of Key:Value pair objects, which include the value type. Please see the structures
// below.
//
// JSON_ParseArray takes a JSON array string and the return object as parameters, and returns the number of items in the
// array parsed.
//
// ---------------------------------------------------------------------------------------------------------------------
//
// Valid JSON data is repesented in Key:Value pairs, where the Key is a string literal and Value can be one of:
//   * String literal 							(enclosed in double quotes)
//   * positive or negative integer or float 	(non-enclosed)
//   * an array of values 						(enclosed in square brackets)
//   * a nested JSON object 					(enclosed in squigly brackets)
//   * True/False 								(true or false, non-enclosed)
//   * Null 									(null, non-enclosed)
//
// ---------------------------------------------------------------------------------------------------------------------
// Version Notes:
// v2_0:	A full rewrite from the ground up to make using JSON data easier and parse properly instead of the hacky
//		stuff I was doing before
// v1_0:	First version that was very hard to use and relied on all values to be strings
// ---------------------------------------------------------------------------------------------------------------------
// User Structures:
//
//	STRUCTURE _JSON_Object
//	{
//		INTEGER	Count							// Number of Key:Value pairs in this object
//		_KVPair	KV[_JSON_MaxPairs]				// The actual Key:Value data
//			STRUCTURE _KVPair
//			{
//				CHAR	K[_JSON_KeySize]			// Key name
//				CHAR	V[_JSON_ValueSize]			// Value data
//				INTEGER	nType						// Value type
//			}
//	}
//
//	STRUCTURE _JSON_Array
//	{
//		INTEGER	Count							// Number of values stored in array
//		_Value	List[_JSON_ArraySize]			// Value storage
//			STRUCTURE _Value
//			{
//				CHAR	Value[_JSON_ValueSize]		// Actual value
//				INTEGER	nType						// Value type
//			}
//	}

// ---------------------------------------------------------------------------------------------------------------------
// Functions:
//	DEFINE_FUNCTION CHAR[] 		_JSON_TrimWhiteSpace	(CHAR cInput[])
//	DEFINE_FUNCTION INTEGER 	JSON_FindObjectInString	(CHAR cInput[],CHAR cJSON_String[])
//	DEFINE_FUNCTION INTEGER 	JSON_ParseObject		(CHAR cInput[],_JSON_Object jObject)
//	DEFINE_FUNCTION INTEGER 	JSON_Validate			(CHAR cInput[],CHAR cCleaned[])
//	DEFINE_FUNCTION INTEGER 	JSON_ParseValidObject	(CHAR cInput[],_JSON_Object jObject)
//	DEFINE_FUNCTION INTEGER 	JSON_ParseArray			(CHAR cInput[],_JSON_Array jReturn)
//	DEFINE_FUNCTION INTEGER 	JSON_HasKey				(_JSON_Object jObject,CHAR cKeyName[])
//	DEFINE_FUNCTION CHAR[] 		JSON_GetValue			(_JSON_Object jObject,CHAR cKeyName[])
//	DEFINE_FUNCTION SINTEGER 	JSON_SetValue			(_JSON_Object jObject,CHAR cKeyName[],CHAR cValue[],INTEGER nType)
//	DEFINE_FUNCTION SINTEGER 	JSON_DeleteKey			(_JSON_Object jObject,CHAR cKeyName[])
//	DEFINE_FUNCTION INTEGER 	JSON_GetType			(_JSON_Object jObject,CHAR cKeyName[])
//	DEFINE_FUNCTION CHAR[]		JSON_ObjectToString		(_JSON_Object jObject)
//	DEFINE_FUNCTION CHAR[]		JSON_ArrayToString		(_JSON_Array jArray)
//	DEFINE_FUNCTION INTEGER 	JSON_ClearObject		(_JSON_Object jObject)
//	DEFINE_FUNCTION CHAR[]		NumArrayToJSON			(SLONG nArray[],INTEGER nLength)
//	DEFINE_FUNCTION CHAR[]		StringArrayToJSON		(CHAR cArray[][],INTEGER nLength)
// ---------------------------------------------------------------------------------------------------------------------

// ---------------------------------------------------------------------------------------------------------------------
// JSON Constants
// ---------------------------------------------------------------------------------------------------------------------
DEFINE_CONSTANT

CHAR JSON_WhiteSpace[] 		= { $20, $0D, $0A, $09 }	// Space literal, CR, LF, Tab

// ---------------------------------------------------------------------------------------------------------------------
// JSON Object size constants
// ---------------------------------------------------------------------------------------------------------------------
INTEGER _JSON_InputSize		= 20000			// Size of buffer for JSON string
INTEGER _JSON_MaxPairs		= 30			// Max number of Key:Value pairs in the _JSON_Object Structure; adjust this if you get OutOfMemory error
INTEGER _JSON_KeySize		= 20			// String size for all Keys
INTEGER _JSON_ValueSize		= 2000			// String size for all Values
INTEGER _JSON_ArraySize		= 64			// Max number of values in _JSON_Array

// ---------------------------------------------------------------------------------------------------------------------
// Data type constants
// ---------------------------------------------------------------------------------------------------------------------
INTEGER _JSON_IsObject		= 1
INTEGER _JSON_IsString		= 2
INTEGER _JSON_IsNumber		= 3
INTEGER _JSON_IsArray		= 4
INTEGER _JSON_IsBool		= 5
INTEGER _JSON_IsNull		= 6

CHAR _JSON_Types[][10] = {
	'Object',
	'String',
	'Number',
	'Array',
	'Bool',
	'Null'
}

// ---------------------------------------------------------------------------------------------------------------------
// Error/Status codes
// ---------------------------------------------------------------------------------------------------------------------
SINTEGER _JSON_Success		= 0
SINTEGER _JSON_KeyNotFound	= -1		// GetValue, DeleteKey
SINTEGER _JSON_OutOfMemory	= -2		// SetValue
SINTEGER _JSON_InvalidValue = -3		// SetValue

// ---------------------------------------------------------------------------------------------------------------------
// JSON Structures
// ---------------------------------------------------------------------------------------------------------------------
DEFINE_TYPE

STRUCTURE _Value
{
	CHAR	Value[_JSON_ValueSize]			// Actual value
	INTEGER	nType							// Value type
}

STRUCTURE _JSON_Array
{
	INTEGER	Count							// Number of values stored in array
	_Value	List[_JSON_ArraySize]			// Value storage
}

// Basic Key:Value pair
STRUCTURE _KVPair
{
	CHAR	K[_JSON_KeySize]				// Key name
	CHAR	V[_JSON_ValueSize]				// Value data
	INTEGER	nType							// Value type
}

// The actual object that will be returned
STRUCTURE _JSON_Object
{
	INTEGER	Count							// Number of Key:Value pairs in this object
	_KVPair	KV[_JSON_MaxPairs]				// The actual Key:Value data
}

// ---------------------------------------------------------------------------------------------------------------------
// JSON Parser Variables
// ---------------------------------------------------------------------------------------------------------------------
DEFINE_VARIABLE

VOLATILE INTEGER nJSONDebug = 0


// ---------------------------------------------------------------------------------------------------------------------
// Helper Functions
// ---------------------------------------------------------------------------------------------------------------------
DEFINE_FUNCTION CHAR[_JSON_InputSize] _JSON_TrimWhiteSpace(CHAR cInput[])
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION:	_JSON_TrimWhiteSpace
// PURPOSE:		Special version of _TrimWhiteSpace with bigger return array size
//				Trims all whitespace characters from the beginning and end of string
//
// EXAMPLE:		cReturn = _JSON_TrimWhiteSpace('			Hello	 ')
// RETURNS:		'Hello'
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR CHAR cTempInput[_JSON_InputSize]
	
	cTempInput = cInput
	
	WHILE(FIND_STRING(JSON_WhiteSpace,LEFT_STRING(cTempInput,1),1))
	{
		cTempInput = RIGHT_STRING(cTempInput,LENGTH_STRING(cTempInput)-1)	// Trim first byte
	}
	
	WHILE(FIND_STRING(JSON_WhiteSpace,RIGHT_STRING(cTempInput,1),1))
	{
		cTempInput = LEFT_STRING(cTempInput,LENGTH_STRING(cTempInput)-1)	// Trim last byte
	}
	
	RETURN cTempInput
}

// ---------------------------------------------------------------------------------------------------------------------
// JSON Functions
// ---------------------------------------------------------------------------------------------------------------------

DEFINE_FUNCTION INTEGER JSON_ParseObject (CHAR cInput[],_JSON_Object jObject)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_ParseObject
// PURPOSE:		Replaces the old JSON_ParseObject; acts as a wrapper for the Validate and Parse functions so that we can
// 				use JSON_ParseValidObject without having to run the validation routine
//
// EXAMPLE:		nResult = JSON_ParseObject(cInput,jObject)
// RETURNS:		1 if valid; cJSON_String is updated with JSON string if found
//				0 if not valid; cJSON_String is updated with error
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER nReturn							// Return value from JSON_Validate
	STACK_VAR CHAR cTempInput[_JSON_InputSize]			// Copy cInput so we're not modifying the input parameter
	STACK_VAR CHAR cValidated[_JSON_InputSize]			// The validated JSON string or error

	cTempInput = cInput
	nReturn = JSON_Validate(cTempInput,cValidated)		// Check the string to make sure it's valid
	
	IF (!nReturn)										// The JSON was not valid; return the error
	{
		SEND_STRING 0,"'[JSON] Validator returned the following error: ',cValidated"
		RETURN 0
	}
	ELSE												// The JSON was valid, parse it
	{
		nReturn = JSON_ParseValidObject(cValidated,jObject)
		RETURN nReturn
	}
}

DEFINE_FUNCTION INTEGER JSON_FindObjectInString(CHAR cInput[],CHAR cJSON_String[])
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_FindObjectInString
// PURPOSE:		Traverses a string input to a) verify there is an object in the string and b) return the first valid JSON
//				string found.
//
// EXAMPLE:		nResult = JSON_FindObjectInString(cDeviceBuffer,cJSON_String)
// RETURNS:		1 if valid; cJSON_String is updated with JSON string if found
//				0 if not valid; cJSON_String is updated with error
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR CHAR cTempInput[_JSON_InputSize]		// Temporary string storage
	STACK_VAR CHAR cTempOutput[_JSON_InputSize]		// Temporary string storage
	STACK_VAR INTEGER F1							// Loop over bytes in string
	STACK_VAR INTEGER nFind							// Find location
	STACK_VAR INTEGER bInKey						// Currently in the key
	STACK_VAR INTEGER bInValue						// Currently in the value
	STACK_VAR INTEGER bEndOfValue					// Value should have ended
	STACK_VAR INTEGER bInObj						// Currently in an object
	STACK_VAR INTEGER bInArray						// Currently in an array
	STACK_VAR INTEGER BrObj							// Current number of open object brackets
	STACK_VAR INTEGER BrArray						// Current number of open array brackets
	
	cTempInput = _JSON_TrimWhiteSpace(cInput)
	F1 = 1
	
	WHILE (F1 <= LENGTH_STRING(cTempInput))
	{
		SWITCH (cTempInput[F1])
		{
			CASE $0D:
			CASE $0A:
			CASE $20:
			CASE $09:
			{
				// Ignore, we don't want this in our validated output
			}
			CASE '"':
			{
				nFind = FIND_STRING(cTempInput,'"',F1+1)
				IF (nFind)
				{
					cTempOutput = "cTempOutput,MID_STRING(cTempInput,F1,nFind-(F1-1))"
					F1 = nFind
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': No string terminator'"
					RETURN 0
				}
			}
			CASE ':':
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (bInKey)				// A : should only occur between the key and value in a pair. If it happens anywhere else not in a string, it's invalid
				{
					bInKey = 0
					bInValue = 1
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Colon in value or invalid position'"
					RETURN 0
				}
			}
			CASE ',':					// A , should only occur between key:value pairs to separate them, or inside an array to separate values. If it happens anywhere else not in a string, it's invalid
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (bInArray)				// This occured inside an array which is valid
				{
					// This is fine, ignore it
				}
				ELSE IF (bInValue)			// This occurred at the end of the value, transition back to looking for key
				{
					bInValue = 0
					bInKey = 1
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Comma only valid between key:value or array values.'"
					RETURN 0
				}
			}
			CASE '{':					// Start of object
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				BrObj++
				bInObj = 1
				bInArray = 0
				bInKey = 1
				bInValue = 0
			}
			CASE '}':					// End of object
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				IF (BrObj)				// Make sure we've entered an object before this point; if not, it's not valid JSON
				{
					BrObj--
					bInObj = 0
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': End of object unexpected.'"
					RETURN 0
				}
			}
			CASE '[':					// Start of array
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				IF (bInValue)				// Make sure we're looking at the value of the pair; if not, it's not valid JSON
				{
					BrArray++
					bInArray = 1
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Start of array only valid in value.'"
					RETURN 0
				}
			}
			CASE ']':
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (BrArray)				// Make sure we've entered an array before this point; if not, it's not valid JSON
				{
					BrArray--
					bInArray = 0
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': End of array unexpected.'"
					RETURN 0
				}
			}
			CASE 'T':		// True
			CASE 't':		// true
			{
				IF (bInValue)
				{
					IF (LOWER_STRING(MID_STRING(cTempInput,F1,4)) == 'true')
					{
						cTempOutput = "cTempOutput,'true'"
						F1 = F1 + 3
					}
					ELSE
					{
						cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected character t.'"
						RETURN 0
					}
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected character t.'"
					RETURN 0
				}
			}
			CASE 'F':		// False
			CASE 'f':		// false
			{
				IF (bInValue)
				{
					IF (LOWER_STRING(MID_STRING(cTempInput,F1,5)) == 'false')
					{
						cTempOutput = "cTempOutput,'false'"
						F1 = F1 + 4
					}
					ELSE
					{
						cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected character f.'"
						RETURN 0
					}
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected character f.'"
					RETURN 0
				}
			}
			CASE 'N':		// Null
			CASE 'n':		// null
			{
				IF (bInValue)
				{
					IF (LOWER_STRING(MID_STRING(cTempInput,F1,4)) == 'null')
					{
						cTempOutput = "cTempOutput,'null'"
						F1 = F1 + 3
					}
					ELSE
					{
						cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected character n.'"
						RETURN 0
					}
				}
				ELSE
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected character n.'"
					RETURN 0
				}
			}
			CASE '-':			// Number Value
			CASE '1':			// Number value
			CASE '2':			// Number value
			CASE '3':			// Number value
			CASE '4':			// Number value
			CASE '5':			// Number value
			CASE '6':			// Number value
			CASE '7':			// Number value
			CASE '8':			// Number value
			CASE '9':			// Number value
			CASE '0':			// Number value
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (!bInValue)	// Only time a number outside a string is valid is in the value, also in an array which is in the value
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected integer in key.'"
					RETURN 0
				}
			}
			CASE '.':			// A period is only valid outside a string in a number value
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (!bInValue)	// Only time a period outside a string is valid is in the value when it's a number
				{
					cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected period in key.'"
					RETURN 0
				}
			}
			DEFAULT:
			{
				cJSON_String = "'ERROR at position ',ITOA(F1),': Unexpected character.'"
				RETURN 0
			}
		}
		
		IF (!BrObj && !BrArray)
		{
			cJSON_String = cTempOutput
			cInput = RIGHT_STRING(cTempInput,LENGTH_STRING(cTempInput) - F1)
			RETURN 1
		}
		ELSE
		{
			F1++
		}
	}

	cJSON_String = "'End of string. No valid JSON strings found.'"
	RETURN 0
}

DEFINE_FUNCTION INTEGER JSON_Validate(CHAR cInput[],CHAR cCleaned[])
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_Validate
// PURPOSE:		Parses cInput to validate that it is a valid JSON structure; this also cleans out all unnecessary
//				whitespace characters and returns the cleaned string in the cCleaned parameter
//
// EXAMPLE:		nResult = JSON_Validate(cDeviceBuffer,cValidated)
// RETURNS:		1 if valid, 0 if not; cValidated is updated with compacted text
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR CHAR cTempInput[_JSON_InputSize]		// Temporary string storage
	STACK_VAR CHAR cTempOutput[_JSON_InputSize]		// Temporary string storage
	STACK_VAR INTEGER F1							// Loop over bytes in string
	STACK_VAR INTEGER nFind							// Find location
	STACK_VAR INTEGER bInKey						// Currently in the key
	STACK_VAR INTEGER bInValue						// Currently in the value
	STACK_VAR INTEGER bEndOfValue					// Value should have ended
	STACK_VAR INTEGER bInObj						// Currently in an object
	STACK_VAR INTEGER bInArray						// Currently in an array
	STACK_VAR INTEGER BrObj							// Current number of open object brackets
	STACK_VAR INTEGER BrArray						// Current number of open array brackets
	
	cTempInput = _JSON_TrimWhiteSpace(cInput)
	F1 = 1
	
	WHILE (F1 <= LENGTH_STRING(cTempInput))
	{
		SWITCH (cTempInput[F1])
		{
			CASE $0D:
			CASE $0A:
			CASE $20:
			CASE $09:
			{
				// Ignore, we don't want this in our validated output
			}
			CASE '"':
			{
				nFind = FIND_STRING(cTempInput,'"',F1+1)
				IF (nFind)
				{
					cTempOutput = "cTempOutput,MID_STRING(cTempInput,F1,nFind-(F1-1))"
					F1 = nFind
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': No string terminator'"
					RETURN 0
				}
			}
			CASE ':':
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (bInKey)				// A : should only occur between the key and value in a pair. If it happens anywhere else not in a string, it's invalid
				{
					bInKey = 0
					bInValue = 1
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Colon in value or invalid position'"
					RETURN 0
				}
			}
			CASE ',':					// A , should only occur between key:value pairs to separate them, or inside an array to separate values. If it happens anywhere else not in a string, it's invalid
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (bInArray)				// This occured inside an array which is valid
				{
					// This is fine, ignore it
				}
				ELSE IF (bInValue)			// This occurred at the end of the value, transition back to looking for key
				{
					bInValue = 0
					bInKey = 1
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Comma only valid between key:value or array values.'"
					RETURN 0
				}
			}
			CASE '{':					// Start of object
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				BrObj++
				bInObj = 1
				bInArray = 0
				bInKey = 1
				bInValue = 0
			}
			CASE '}':					// End of object
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				IF (BrObj)				// Make sure we've entered an object before this point; if not, it's not valid JSON
				{
					BrObj--
					bInObj = 0
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': End of object unexpected.'"
					RETURN 0
				}
			}
			CASE '[':					// Start of array
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				IF (bInValue)				// Make sure we're looking at the value of the pair; if not, it's not valid JSON
				{
					BrArray++
					bInArray = 1
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Start of array only valid in value.'"
					RETURN 0
				}
			}
			CASE ']':
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (BrArray)				// Make sure we've entered an array before this point; if not, it's not valid JSON
				{
					BrArray--
					bInArray = 0
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': End of array unexpected.'"
					RETURN 0
				}
			}
			CASE 'T':		// True
			CASE 't':		// true
			{
				IF (bInValue)
				{
					IF (LOWER_STRING(MID_STRING(cTempInput,F1,4)) == 'true')
					{
						cTempOutput = "cTempOutput,'true'"
						F1 = F1 + 3
					}
					ELSE
					{
						cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected character t.'"
						RETURN 0
					}
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected character t.'"
					RETURN 0
				}
			}
			CASE 'F':		// False
			CASE 'f':		// false
			{
				IF (bInValue)
				{
					IF (LOWER_STRING(MID_STRING(cTempInput,F1,5)) == 'false')
					{
						cTempOutput = "cTempOutput,'false'"
						F1 = F1 + 4
					}
					ELSE
					{
						cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected character f.'"
						RETURN 0
					}
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected character f.'"
					RETURN 0
				}
			}
			CASE 'N':		// Null
			CASE 'n':		// null
			{
				IF (bInValue)
				{
					IF (LOWER_STRING(MID_STRING(cTempInput,F1,4)) == 'null')
					{
						cTempOutput = "cTempOutput,'null'"
						F1 = F1 + 3
					}
					ELSE
					{
						cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected character n.'"
						RETURN 0
					}
				}
				ELSE
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected character n.'"
					RETURN 0
				}
			}
			CASE '-':			// Number Value
			CASE '1':			// Number value
			CASE '2':			// Number value
			CASE '3':			// Number value
			CASE '4':			// Number value
			CASE '5':			// Number value
			CASE '6':			// Number value
			CASE '7':			// Number value
			CASE '8':			// Number value
			CASE '9':			// Number value
			CASE '0':			// Number value
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (!bInValue)	// Only time a number outside a string is valid is in the value, also in an array which is in the value
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected integer in key.'"
					RETURN 0
				}
			}
			CASE '.':			// A period is only valid outside a string in a number value
			{
				cTempOutput = "cTempOutput,cTempInput[F1]"
				
				IF (!bInValue)	// Only time a period outside a string is valid is in the value when it's a number
				{
					cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected period in key.'"
					RETURN 0
				}
			}
			DEFAULT:
			{
				cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected character.'"
				RETURN 0
			}
		}
		
		F1++
	}
	
	IF (BrObj || BrArray)
	{
		cCleaned = "'ERROR at position ',ITOA(F1),': Unexpected end of JSON data.'"
		RETURN 0
	}
	ELSE
	{
		cCleaned = cTempOutput
		RETURN 1
	}
}


DEFINE_FUNCTION INTEGER JSON_ParseValidObject(CHAR cValidJSON[],_JSON_Object jObject)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_ParseValidObject
// PURPOSE:		Parses cValidJSON and puts the Key:Value pairs in the _JSON_Object that is passed in the second parameter
//
// EXAMPLE:		nResult = JSON_ParseObject(cDeviceBuffer,jObject)
// RETURNS:		number of key:value pairs parsed; jObject has the parsed data
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR _JSON_Object jReturn					// Local storage of JSON data
	STACK_VAR CHAR cTempInput[_JSON_InputSize]		// Temporary string storage
	STACK_VAR CHAR cValidated[_JSON_InputSize]		// Validated JSON data
	STACK_VAR CHAR cTemp[5000]						// Temporary string storage
	STACK_VAR INTEGER nReturn						// Return from the validator
	STACK_VAR INTEGER F1							// Loop over bytes in string
	STACK_VAR INTEGER nFind							// Find location
	STACK_VAR INTEGER bInKey						// Currently in the key
	STACK_VAR INTEGER bInValue						// Currently in the value
	STACK_VAR INTEGER bEndOfValue					// Value should have ended
	STACK_VAR INTEGER bInObj						// Currently in an object
	STACK_VAR INTEGER bInChildObj					// Currently in a child object
	STACK_VAR INTEGER bInArray						// Currently in an array
	STACK_VAR INTEGER BrObj							// Current number of open object brackets
	STACK_VAR INTEGER BrChildObj					// Current number of open child object brackets
	STACK_VAR INTEGER nChildObjStart				// Start of the current child object
	STACK_VAR INTEGER BrArray						// Current number of open array brackets
	STACK_VAR INTEGER BrArrayStart					// Start of the current array
	STACK_VAR INTEGER nCurrentKV					// Current KV pair we're processing
	
	cValidated = cValidJSON
	
	IF (jObject.Count)
	{
		JSON_ClearObject(jObject)
	}
	
	nCurrentKV = 1
	F1 = 1
	
	WHILE (F1 <= LENGTH_STRING(cValidated))			// Loop through string and look at almost every character
	{
		SWITCH (cValidated[F1])
		{
			CASE '"':
			{
				IF (!bInChildObj && !bInArray)				// Looking for strings in either the Key position, or single string values in the Value position
				{
					nFind = FIND_STRING(cValidated,'"',F1+1)
					cTemp = MID_STRING(cValidated,F1+1,nFind-(F1+1))
					IF (bInKey)
					{
						IF (LENGTH_STRING(jReturn.KV[nCurrentKV].K))	// If we've already set a key name in the current index, increment the index; this is where we move to the next K:V pair
						{
							nCurrentKV++
						}
						jReturn.KV[nCurrentKV].K = cTemp
					}
					ELSE IF (bInValue && !bInArray && !bInChildObj)
					{
						jReturn.KV[nCurrentKV].V = cTemp
						jReturn.KV[nCurrentKV].nType = _JSON_IsString
					}
					F1 = nFind
				}
			}
			CASE ':':
			{
				IF (!bInChildObj && !bInArray)		// Child
				{
					IF (bInKey)				// A : should only occur between the key and value in a pair. If it happens anywhere else not in a string, it's invalid
					{
						bInKey = 0
						bInValue = 1
					}
					ELSE
					{
						RETURN 0
					}
				}
			}
			CASE ',':					// A , should only occur between key:value pairs to separate them, or inside an array to separate values. If it happens anywhere else not in a string, it's invalid
			{
				IF (!bInChildObj && !bInArray)
				{
					bInValue = 0
					bInKey = 1
				}
			}
			CASE '{':					// Start of object
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a { at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInArray)
				{
					IF (bInChildObj)
					{
						BrChildObj++
					}
					ELSE
					{
						IF (bInValue)
						{
							BrChildObj++			// This will cause all data to get copied to the value of the current key until we reach the end of the current object
							nChildObjStart = F1
							bInChildObj = 1
						}
						ELSE
						{
							BrObj++
							BrChildObj = 0
							bInObj = 1
							bInArray = 0
							bInKey = 1
							bInValue = 0
						}
					}
				}
			}
			CASE '}':					// End of object
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a } at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInArray)
				{
					IF (bInChildObj)
					{
						BrChildObj --
						IF (BrChildObj == 0)			// This is the end of the child object
						{
							nFind = F1
							jReturn.KV[nCurrentKV].nType = _JSON_IsObject
							jReturn.KV[nCurrentKV].V = MID_STRING(cValidated,nChildObjStart,nFind-(nChildObjStart-1))
							nChildObjStart = 0
							bInChildObj = 0
						}
					}
					ELSE IF (BrObj)				// Make sure we've entered a object before this point; if not, it's not valid JSON
					{
						BrObj--
						bInObj = 0
					}
				}
			}
			CASE '[':						// Start of array
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a [ at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInChildObj)
				{
					BrArray++
					bInArray = 1
					IF (BrArray == 1)				// Outer array
					{
						BrArrayStart = F1
					}
					IF (nJSONDebug) SEND_STRING 0,"'[JSON] Array start at ',ITOA(F1),': BrArray: ',ITOA(BrArray),', BrArrayStart: ',ITOA(BrArrayStart)"
				}
			}
			CASE ']':						// End of Array
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a ] at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInChildObj)
				{
					BrArray--
					IF (nJSONDebug) SEND_STRING 0,"'[JSON] Array stop at ',ITOA(F1),': BrArray: ',ITOA(BrArray),', BrArrayStart: ',ITOA(BrArrayStart)"
					IF (BrArray == 0)
					{
						nFind = F1
						jReturn.KV[nCurrentKV].V = MID_STRING(cValidated,BrArrayStart,nFind-(BrArrayStart-1))
						jReturn.KV[nCurrentKV].nType = _JSON_IsArray
						BrArrayStart = 0
						bInArray = 0
					}
				}
			}
			CASE 't':		// true
			{
				IF (!bInChildObj && !bInArray)
				{
					jReturn.KV[nCurrentKV].nType = _JSON_IsBool
					jReturn.KV[nCurrentKV].V = 'true'
					F1 = F1 + 3
				}
			}
			CASE 'f':		// false
			{
				IF (!bInChildObj && !bInArray)
				{
					jReturn.KV[nCurrentKV].nType = _JSON_IsBool
					jReturn.KV[nCurrentKV].V = 'false'
					F1 = F1 + 4
				}
			}
			CASE 'n':		// null
			{
				IF (!bInChildObj && !bInArray)
				{
					jReturn.KV[nCurrentKV].nType = _JSON_IsNull
					jReturn.KV[nCurrentKV].V = 'null'
					F1 = F1 + 3
				}
			}
			CASE '-':			// Number value
			CASE '1':			// Number value
			CASE '2':			// Number value
			CASE '3':			// Number value
			CASE '4':			// Number value
			CASE '5':			// Number value
			CASE '6':			// Number value
			CASE '7':			// Number value
			CASE '8':			// Number value
			CASE '9':			// Number value
			CASE '0':			// Number value
			{
				IF (!bInChildObj && !bInArray && bInValue)
				{
					nFind = FIND_STRING(cValidated,',',F1)			// Find end of number by finding the end of the K:V pair
					IF (!nFind)
					{
						nFind = LENGTH_STRING(cValidated) + 1		// Simulate finding the comma after the end of the string
					}
					cTemp = MID_STRING(cValidated,F1,nFind-F1)
					jReturn.KV[nCurrentKV].nType = _JSON_IsNumber
					jReturn.KV[nCurrentKV].V = cTemp
					F1 = nFind
					bInValue = 0
					bInKey = 1
				}
			}
		}
		
		F1++
	}
	
	IF (nJSONDebug) SEND_STRING 0,"'[JSON] ',ITOA(F1),' bytes parsed.'"
	jReturn.Count = nCurrentKV
	jObject = jReturn
	RETURN nCurrentKV
}

DEFINE_FUNCTION INTEGER JSON_ParseArray(CHAR cInput[],_JSON_Array jReturn)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_ParseArray
// PURPOSE:		Parses a JSON array string and returns an array object of the values found
//				Must be formatted like JSON: [element,element,element]
//
// EXAMPLE:		cResult = JSON_ParseArray(cJSONArray,jParsedArray)
// RETURNS:		return is number of values in array; jParsedArray is an array object of the values
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1							// Normal FOR loop var
	STACK_VAR INTEGER nFind							// Temp search variable
	STACK_VAR CHAR cTemp[_JSON_ValueSize]			// Temporary string
	STACK_VAR CHAR cTempInput[_JSON_ValueSize]		// Trimmed copy of the input array
	STACK_VAR _JSON_Array jTempArray				// Temporary array to store the values
	STACK_VAR INTEGER nValueCount					// Number of values found so far in array
	STACK_VAR INTEGER bInChildObj					// Currently in a child object
	STACK_VAR INTEGER bInArray						// Currently in an array
	STACK_VAR INTEGER BrChildObj					// Current number of open child object brackets
	STACK_VAR INTEGER nChildObjStart				// Start of the current child object
	STACK_VAR INTEGER BrArray						// Current number of open array brackets
	STACK_VAR INTEGER BrArrayStart					// Start of the current array
	
	// Clean up unnecessary whitespace around array
	cTempInput = _JSON_TrimWhiteSpace(cInput)
	
	// Make sure it's an array
	IF (!(LEFT_STRING(cTempInput,1) == '[' && RIGHT_STRING(cTempInput,1) == ']'))
	{
		RETURN 0
	}
	
	// Remove the square brackets and clean up the whitespace again; now we should only have data
	cTempInput = _JSON_TrimWhiteSpace(MID_STRING(cTempInput,2,LENGTH_STRING(cTempInput)-2))
	//SEND_STRING 0,"cTempInput"
	
	nValueCount = 1
	F1 = 1
	
	WHILE (F1 <= LENGTH_STRING(cTempInput))
	{
		//SEND_STRING 0,"'Checking: ',ITOA(F1),' ',cTempInput[F1]"
		SWITCH (cTempInput[F1])
		{
			CASE $0D:
			CASE $0A:
			CASE $20:
			CASE $09:
			{
				// Ignore
			}
			CASE '"':
			{
				IF (!bInChildObj && !bInArray)
				{
					nFind = FIND_STRING(cTempInput,'"',F1+1)
					cTemp = MID_STRING(cTempInput,F1+1,nFind-(F1+1))
					jTempArray.List[nValueCount].nType = _JSON_IsString
					jTempArray.List[nValueCount].Value = cTemp
					F1 = nFind
				}
			}
			CASE ',':						// A , that's not part of a string should only occur between value pairs to separate them
			{
				IF (!bInChildObj && !bInArray)
				{
					nValueCount++
				}
			}
			CASE '{':						// Start of object
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a { at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInArray)
				{
					BrChildObj++				// This will cause all data to get copied to the value of the current key until we reach the end of the current object
					nChildObjStart = F1
					bInChildObj = 1
				}
			}
			CASE '}':						// End of object
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a } at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInArray)
				{
					BrChildObj --
					IF (BrChildObj == 0)			// This is the end of the child object
					{
						nFind = F1
						jTempArray.List[nValueCount].nType = _JSON_IsObject
						jTempArray.List[nValueCount].Value = MID_STRING(cTempInput,nChildObjStart,nFind-(nChildObjStart-1))
						nChildObjStart = 0
						bInChildObj = 0
					}
				}
			}
			CASE '[':						// Start of array
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a [ at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInChildObj)
				{
					BrArray++
					bInArray = 1
					IF (BrArray == 1)				// Outer array
					{
						BrArrayStart = F1
					}
					IF (nJSONDebug) SEND_STRING 0,"'[JSON] Array start at ',ITOA(F1),': BrArray: ',ITOA(BrArray),', BrArrayStart: ',ITOA(BrArrayStart)"
				}
			}
			CASE ']':						// End of Array
			{
				IF (nJSONDebug) SEND_STRING 0,"'[JSON] I see a ] at ',ITOA(F1),' (bInArray: ',ITOA(bInArray),', bInChildObj: ',ITOA(bInChildObj),')'"
				IF (!bInChildObj)
				{
					BrArray--
					IF (nJSONDebug) SEND_STRING 0,"'[JSON] Array stop at ',ITOA(F1),': BrArray: ',ITOA(BrArray),', BrArrayStart: ',ITOA(BrArrayStart)"
					IF (BrArray == 0)
					{
						nFind = F1
						jTempArray.List[nValueCount].nType = _JSON_IsArray
						jTempArray.List[nValueCount].Value = MID_STRING(cTempInput,BrArrayStart,nFind-(BrArrayStart-1))
						BrArrayStart = 0
						bInArray = 0
					}
				}
			}
			CASE 't':		// true
			{
				IF (!bInChildObj && !bInArray)
				{
					jTempArray.List[nValueCount].nType = _JSON_IsBool
					jTempArray.List[nValueCount].Value = 'true'
					F1 = F1 + 3
				}
			}
			CASE 'f':		// false
			{
				IF (!bInChildObj && !bInArray)
				{
					jTempArray.List[nValueCount].nType = _JSON_IsBool
					jTempArray.List[nValueCount].Value = 'false'
					F1 = F1 + 4
				}
			}
			CASE 'n':		// null
			{
				IF (!bInChildObj && !bInArray)
				{
					jTempArray.List[nValueCount].nType = _JSON_IsNull
					jTempArray.List[nValueCount].Value = 'null'
					F1 = F1 + 3
				}
			}
			CASE '-':			// Number value
			CASE '1':			// Number value
			CASE '2':			// Number value
			CASE '3':			// Number value
			CASE '4':			// Number value
			CASE '5':			// Number value
			CASE '6':			// Number value
			CASE '7':			// Number value
			CASE '8':			// Number value
			CASE '9':			// Number value
			CASE '0':			// Number value
			{
				IF (!bInChildObj && !bInArray)
				{
					nFind = FIND_STRING(cTempInput,',',F1)			// Find end of number by finding the comma
					IF (!nFind)							// Last value in the array
					{
						nFind = LENGTH_STRING(cTempInput) + 1			// We're making up for the fact the comma would be the first character after the number
					}
					cTemp = MID_STRING(cTempInput,F1,nFind-F1)
					jTempArray.List[nValueCount].nType = _JSON_IsNumber
					jTempArray.List[nValueCount].Value = cTemp
					F1 = nFind - 1
				}
			}
		}
		
		F1++
	}
	
	IF (nValueCount)
	{
		SET_LENGTH_ARRAY(jTempArray.List,nValueCount)
		jTempArray.Count = nValueCount
		jReturn = jTempArray
		
		RETURN nValueCount
	}
	ELSE
	{
		jReturn.List[1].Value = 'JSON_ParseArray: Not a valid array.'
		RETURN 0
	}
}

DEFINE_FUNCTION INTEGER JSON_HasKey(_JSON_Object jObject,CHAR cKeyName[])
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_HasKey
// PURPOSE:		Iterates through the KV pairs in the JSON object and returns true/false if the key is found or not
//
// EXAMPLE:		cResult = JSON_HasKey(jData,'RoomName')
// RETURNS:		<1 or 0>
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Normal FOR loop var
	
	FOR (F1 = 1; F1 <= jObject.Count; F1++)
	{
		IF (LOWER_STRING(jObject.KV[F1].K) = LOWER_STRING(cKeyName))
		{
			RETURN 1
		}
	}
	RETURN 0
}

DEFINE_FUNCTION CHAR[_JSON_ValueSize] JSON_GetValue(_JSON_Object jObject,CHAR cKeyName[])
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_GetValue
// PURPOSE:		Iterates through the KV pairs in the JSON object and returns the value of the Key if found
//
// EXAMPLE:		cResult = JSON_GetValue(jData,'RoomName')
// RETURNS:		<value of RoomName>
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Normal FOR loop var
	
	FOR (F1 = 1; F1 <= jObject.Count; F1++)
	{
		IF (LOWER_STRING(jObject.KV[F1].K) = LOWER_STRING(cKeyName))
		{
			RETURN jObject.KV[F1].V
		}
	}
	
	#IF_DEFINED libSystemLog_v1_2_Met
	SendToLog('JSON',"'JSON_GetValue: Key not found: ',cKeyName")
	#END_IF
	
	RETURN ''
}

DEFINE_FUNCTION SINTEGER JSON_SetValue(_JSON_Object jObject,CHAR cKeyName[],CHAR cValue[],INTEGER nType)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_SetValue
// PURPOSE:		Creates a key, or updates a key, with the specified value and type
//
// EXAMPLE:		nResult = JSON_SetValue(jData,'RoomName','Conference Room',_JSON_IsString)
// RETURNS:		<status code>
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1							// Normal FOR loop var
	STACK_VAR CHAR cTempValue[_JSON_ValueSize]		// Temporary storage
	STACK_VAR CHAR cValidValue[_JSON_ValueSize]		// Verified value

	cTempValue = _JSON_TrimWhiteSpace(cValue)

	SWITCH (nType)
	{
		CASE _JSON_IsArray:
		{
			IF (LEFT_STRING(cTempValue,1) == '[' && RIGHT_STRING(cTempValue,1) == ']')
			{
				cValidValue = cTempValue
			}
			ELSE
			{
				RETURN _JSON_InvalidValue
			}
		}
		CASE _JSON_IsObject:
		{
			IF (LEFT_STRING(cTempValue,1) == '{' && RIGHT_STRING(cTempValue,1) == '}')
			{
				cValidValue = cTempValue
			}
			ELSE
			{
				RETURN _JSON_InvalidValue
			}
		}
		CASE _JSON_IsBool:
		{
			SELECT
			{
				ACTIVE (LOWER_STRING(cTempValue) == 'true' || cTempValue == '1'):			// We'll accept 0 and 1 as false and true
				{
					cValidValue = 'true'
				}
				ACTIVE (LOWER_STRING(cTempValue) == 'false' || cTempValue == '0'):
				{
					cValidValue = 'false'
				}
				ACTIVE (1):
				{
					RETURN _JSON_InvalidValue
				}
			}
		}
		CASE _JSON_IsNull:
		{
			cValidValue = 'null'
		}
		CASE _JSON_IsNumber:
		{
			IF (FTOA(ATOF(cTempValue)) == cTempValue)				// Convert string to a float, then back to a string and see if it matches
			{
				cValidValue = cTempValue
			}
			ELSE
			{
				RETURN _JSON_InvalidValue
			}
		}
		CASE _JSON_IsString:
		{
			cValidValue = cTempValue
		}
	}

	FOR (F1 = 1; F1 <= jObject.Count; F1++)
	{
		IF (LOWER_STRING(jObject.KV[F1].K) = LOWER_STRING(cKeyName))
		{
			jObject.KV[F1].V = cValidValue
			jObject.KV[F1].nType = nType
			RETURN _JSON_Success
		}
	}
	
	IF (jObject.Count < _JSON_MaxPairs)
	{
		jObject.Count++
		jObject.KV[jObject.Count].K = cKeyName
		jObject.KV[jObject.Count].V = cValidValue
		jObject.KV[jObject.Count].nType = nType
		RETURN _JSON_Success
	}
	ELSE
	{
		#IF_DEFINED libSystemLog_v1_2_Met
		SendToLog('JSON',"'JSON_SetValue: Out of memory in JSON object while trying to add ',cKeyName,':',cValidValue")
		#END_IF
		
		RETURN _JSON_OutOfMemory
	}
}

DEFINE_FUNCTION SINTEGER JSON_DeleteKey(_JSON_Object jObject,CHAR cKeyName[])
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_DeleteKey
// PURPOSE:		Deletes a key from the object if found
//
// EXAMPLE:		nResult = JSON_DeleteKey(jData,'RoomName')
// RETURNS:		<status code>
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Normal FOR loop var
	STACK_VAR INTEGER nIndex					// Which index matched
	STACK_VAR _KVPair KV						// Empty KV pair
	
	FOR (F1 = 1; F1 <= jObject.Count; F1++)
	{
		IF (LOWER_STRING(jObject.KV[F1].K) = LOWER_STRING(cKeyName))
		{
			nIndex = F1
			BREAK
		}
	}
	
	IF (nIndex)
	{
		FOR (F1 = nIndex; F1 <= jObject.Count; F1++)
		{
			IF (F1 < _JSON_MaxPairs)
			{
				jObject.KV[F1] = jObject.KV[F1+1]
			}
			ELSE
			{
				jObject.KV[F1] = KV
			}
		}
		RETURN _JSON_Success
	}
	ELSE
	{
		#IF_DEFINED libSystemLog_v1_2_Met
		SendToLog('JSON',"'JSON_DeleteKey: Key not found: ',cKeyName")
		#END_IF
		
		RETURN _JSON_KeyNotFound
	}
}

DEFINE_FUNCTION INTEGER JSON_GetType(_JSON_Object jObject,CHAR cKeyName[])
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION: 	JSON_GetType
// PURPOSE:		Iterates through the KV pairs in the JSON object and returns the type of the value of the Key if found
//
// EXAMPLE:		cResult = JSON_GetValueType(jData,'RoomName')
// RETURNS:		<integer value matching _JSON_IsXXXX constants>
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Normal FOR loop var
	
	FOR (F1 = 1; F1 <= jObject.Count; F1++)
	{
		IF (LOWER_STRING(jObject.KV[F1].K) = LOWER_STRING(cKeyName))
		{
			RETURN jObject.KV[F1].nType
		}
	}
	
	#IF_DEFINED libSystemLog_v1_2_Met
	SendToLog('JSON',"'JSON_GetType: Key not found: ',cKeyName")
	#END_IF
	
	RETURN 0
}

DEFINE_FUNCTION CHAR[_JSON_InputSize] JSON_ObjectToString(_JSON_Object jObject)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION:	JSON_ObjectToString
// PURPOSE:  	Output a JSON object with the records included in the _JSON_Object parameter
//
// EXAMPLE:		JSON_ObjectToString(jData)
// RETURNS:		<string containing JSON formatted data>
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Standard FOR loop variable
	STACK_VAR CHAR cComma[1]					// The comma, should we not be at the end of the array
	STACK_VAR CHAR cReturn[_JSON_InputSize]		// Temp storage for return variable
	
	FOR (F1 = 1; F1 <= jObject.Count; F1++)
	{
		cReturn = "cReturn,$09,'"',jObject.KV[F1].K,'": '"
		
		IF (F1 < jObject.Count)			// Fun, huh?
		{
			cComma = ','
		}
		ELSE
		{
			cComma = ''
		}
		
		SWITCH (jObject.KV[F1].nType)
		{
			CASE _JSON_IsObject:
			{
				cReturn = "cReturn,jObject.KV[F1].V,cComma,13"
			}
			CASE _JSON_IsArray:
			{
				cReturn = "cReturn,jObject.KV[F1].V,cComma,13"
			}
			CASE _JSON_IsString:
			{
				cReturn = "cReturn,'"',jObject.KV[F1].V,'"',cComma,13"
			}
			CASE _JSON_IsNumber:
			{
				cReturn = "cReturn,jObject.KV[F1].V,cComma,13"
			}
			CASE _JSON_IsBool:
			{
				cReturn = "cReturn,jObject.KV[F1].V,cComma,13"
			}
			CASE _JSON_IsNull:
			{
				cReturn = "cReturn,jObject.KV[F1].V,cComma,13"
			}
		}
	}
	
	cReturn = "'{',13,cReturn,'}'"
	RETURN cReturn
}

DEFINE_FUNCTION CHAR[_JSON_InputSize] JSON_ArrayToString(_JSON_Array jArray)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION:	JSON_ArrayToString
// PURPOSE:  	Output a JSON array with the elements included in the _JSON_Array parameter
//
// EXAMPLE:		JSON_ArrayToString(jArray)
// RETURNS:		<string containing JSON formatted data>
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Standard FOR loop variable
	STACK_VAR CHAR cComma[1]					// The comma, should we not be at the end of the array
	STACK_VAR CHAR cReturn[_JSON_InputSize]		// Temp storage for return variable
	
	FOR (F1 = 1; F1 <= jArray.Count; F1++)
	{
		IF (F1 < jArray.Count)			// Fun, huh?
		{
			cComma = ','
		}
		ELSE
		{
			cComma = ''
		}
		
		SWITCH (jArray.List[F1].nType)
		{
			CASE _JSON_IsObject:
			{
				cReturn = "cReturn,jArray.List[F1].Value,cComma,13"
			}
			CASE _JSON_IsArray:
			{
				cReturn = "cReturn,jArray.List[F1].Value,cComma,13"
			}
			CASE _JSON_IsString:
			{
				cReturn = "cReturn,'"',jArray.List[F1].Value,'"',cComma,13"
			}
			CASE _JSON_IsNumber:
			{
				cReturn = "cReturn,jArray.List[F1].Value,cComma,13"
			}
			CASE _JSON_IsBool:
			{
				cReturn = "cReturn,jArray.List[F1].Value,cComma,13"
			}
			CASE _JSON_IsNull:
			{
				cReturn = "cReturn,jArray.List[F1].Value,cComma,13"
			}
		}
	}
	
	cReturn = "'[',cReturn,']'"
	RETURN cReturn
}

DEFINE_FUNCTION INTEGER JSON_ClearObject(_JSON_Object jObject)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION:	JSON_ClearObject
// PURPOSE:		Clear a _JSON_Object variable of all data, if using a global variable to store data; most of the time you
//				should be using STACK_VARs which will be destroyed when the code block ends
//
// EXAMPLE:		JSON_ClearObject(jData)
// RETURNS:		Nothing
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1
	STACK_VAR _KVPair EmptyRecord
	
	FOR (F1 = 1; F1 <= jObject.Count; F1++)
	{
		jObject.KV[F1] = EmptyRecord
	}
	
	jObject.Count = 0
}

DEFINE_FUNCTION CHAR[_JSON_InputSize] NumArrayToJSON(SLONG nArray[],INTEGER nLength)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION:	NumArrayToJSON
// PURPOSE:		Input a SLONG, LONG, or INTEGER array and return the JSON string equivalent of the array
//
// EXAMPLE:		NumArrayToJSON(slArray,5)
// RETURNS:		'[ 1, 2, 3, 4, 5 ]'
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Standard FOR loop variable
	STACK_VAR CHAR cComma[1]					// The comma, should we not be at the end of the array
	STACK_VAR CHAR cReturn[_JSON_InputSize]		// Temp storage for return variable
	
	IF (MAX_LENGTH_ARRAY(nArray) < nLength)
	{
		RETURN ''
	}
	
	FOR (F1 = 1; F1 <= nLength; F1++)
	{
		IF (F1 < nLength)			// Fun, huh?
		{
			cComma = ','
		}
		ELSE
		{
			cComma = ''
		}
		
		cReturn = "cReturn,ITOA(nArray[F1]),cComma"
	}
	
	cReturn = "'[',cReturn,']'"
	RETURN cReturn
}

DEFINE_FUNCTION CHAR[_JSON_InputSize] StringArrayToJSON(CHAR cArray[][],INTEGER nLength)
// ---------------------------------------------------------------------------------------------------------------------
// FUNCTION:	StringArrayToJSON
// PURPOSE:		Input a CHAR array and return the JSON string equivalent of the array
//
// EXAMPLE:		StringArrayToJSON(cArray,5)
// RETURNS:		'[ "Hello", "my", "name", "is", "JSON" ]'
// ---------------------------------------------------------------------------------------------------------------------
{
	STACK_VAR INTEGER F1						// Standard FOR loop variable
	STACK_VAR CHAR cComma[1]					// The comma, should we not be at the end of the array
	STACK_VAR CHAR cReturn[_JSON_InputSize]		// Temp storage for return variable
	
	IF (MAX_LENGTH_ARRAY(cArray) < nLength)
	{
		RETURN ''
	}
	
	FOR (F1 = 1; F1 <= nLength; F1++)
	{
		IF (F1 < nLength)			// Fun, huh?
		{
			cComma = ','
		}
		ELSE
		{
			cComma = ''
		}
		
		cReturn = "cReturn,'"',cArray[F1],'"',cComma"
	}
	
	cReturn = "'[',cReturn,']'"
	RETURN cReturn
}
