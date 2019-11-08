#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3 // Use modern global access method and strict wave access.

static StrConstant PackageName = "Plotly"
static Constant PackageVersion = 1
static StrConstant PrefsFileName = "PlotlyPreferences.bin"
static Constant PrefsRecordID = 0
static Constant reserved = 100

// Global Preferences stored in Igor Folder
Structure PlotlyPrefs
	uint32 version

	char   username[50]
	char   api_key[20]

	uint32 reserved[reserved]
EndStructure

//	Sets prefs structure to default values.
static Function DefaultPackagePrefsStruct(prefs)
	STRUCT PlotlyPrefs &prefs

	Variable i

	prefs.version  = PackageVersion
	prefs.username = ""
	prefs.api_key  = ""

	for(i = 0; i < reserved; i += 1)
		prefs.reserved[i] = 0
	endfor
End

static Function SyncPackagePrefsStruct(prefs)
	STRUCT PlotlyPrefs &prefs

	prefs.version = PackageVersion
End

Function PlotlyLoadPackagePrefs(prefs)
	STRUCT PlotlyPrefs &prefs

	string username, api_key

	LoadPackagePreferences PackageName, PrefsFileName, PrefsRecordID, prefs

	// If error or prefs not found or not valid, initialize them.
	if(!!V_flag || !V_bytesRead)
		DefaultPackagePrefsStruct(prefs)
		PlotlySavePackagePrefs(prefs)
	elseif(prefs.version!= PackageVersion)
		username = prefs.username
		api_key = prefs.api_key

		DefaultPackagePrefsStruct(prefs)

		prefs.username = username
		prefs.api_key = api_key

		PlotlySavePackagePrefs(prefs)
	endif
End

Function PlotlySavePackagePrefs(prefs)
	STRUCT PlotlyPrefs &prefs

	SyncPackagePrefsStruct(prefs)
	SavePackagePreferences PackageName, PrefsFileName, PrefsRecordID, prefs
End
