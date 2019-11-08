#AutoIt3Wrapper_Au3Check_Parameters=-d -w 1 -w 2 -w 3 -w- 4 -w 5 -w 6 -w 7

;~ ADO Enumerated Constants
;~ http://msdn.microsoft.com/en-us/library/windows/desktop/ms678353%28v=vs.85%29.aspx

; ADCPROP_ASYNCTHREADPRIORITY_ENUM
Global Const $ADOENUM_adPriorityLowest = 1
Global Const $ADOENUM_adPriorityBelowNormal = 2
Global Const $ADOENUM_adPriorityNormal = 3
Global Const $ADOENUM_adPriorityAboveNormal = 4
Global Const $ADOENUM_adPriorityHighest = 5

; ADCPROP_AUTORECALC_ENUM
Global Const $ADOENUM_adRecalcUpFront = 0
Global Const $ADOENUM_adRecalcAlways = 1

; ADCPROP_UPDATECRITERIA_ENUM
Global Const $ADOENUM_adCriteriaKey = 0
Global Const $ADOENUM_adCriteriaAllCols = 1
Global Const $ADOENUM_adCriteriaUpdCols = 2
Global Const $ADOENUM_adCriteriaTimeStamp = 3

; ADCPROP_UPDATERESYNC_ENUM
Global Const $ADOENUM_adResyncNone = 0
Global Const $ADOENUM_adResyncAutoIncrement = 1
Global Const $ADOENUM_adResyncConflicts = 2
Global Const $ADOENUM_adResyncUpdates = 4
Global Const $ADOENUM_adResyncInserts = 8
Global Const $ADOENUM_adResyncAll = 15

; AffectEnum
Global Const $ADOENUM_adAffectCurrent = 1
Global Const $ADOENUM_adAffectGroup = 2
Global Const $ADOENUM_adAffectAll = 3
Global Const $ADOENUM_adAffectAllChapters = 4

; BookmarkEnum
Global Const $ADOENUM_adBookmarkCurrent = 0
Global Const $ADOENUM_adBookmarkFirst = 1
Global Const $ADOENUM_adBookmarkLast = 2

; CommandTypeEnum
Global Const $ADOENUM_adCmdUnspecified = -1
Global Const $ADOENUM_adCmdText = 1
Global Const $ADOENUM_adCmdTable = 2
Global Const $ADOENUM_adCmdStoredProc = 4
Global Const $ADOENUM_adCmdUnknown = 8
Global Const $ADOENUM_adCmdFile = 256
Global Const $ADOENUM_adCmdTableDirect = 512

; CompareEnum
Global Const $ADOENUM_adCompareLessThan = 0
Global Const $ADOENUM_adCompareEqual = 1
Global Const $ADOENUM_adCompareGreaterThan = 2
Global Const $ADOENUM_adCompareNotEqual = 3
Global Const $ADOENUM_adCompareNotComparable = 4

; ConnectModeEnum
Global Const $ADOENUM_adModeUnknown = 0
Global Const $ADOENUM_adModeRead = 1
Global Const $ADOENUM_adModeWrite = 2
Global Const $ADOENUM_adModeReadWrite = 3
Global Const $ADOENUM_adModeShareDenyRead = 4
Global Const $ADOENUM_adModeShareDenyWrite = 8
Global Const $ADOENUM_adModeShareExclusive = 12
Global Const $ADOENUM_adModeShareDenyNone = 16
Global Const $ADOENUM_adModeRecursive = 0x400000

; ConnectOptionEnum
Global Const $ADOENUM_adConnectUnspecified = -1
Global Const $ADOENUM_adAsyncConnect = 16

; ConnectPromptEnum
Global Const $ADOENUM_adPromptAlways = 1
Global Const $ADOENUM_adPromptComplete = 2
Global Const $ADOENUM_adPromptCompleteRequired = 3
Global Const $ADOENUM_adPromptNever = 4

; CopyRecordOptionsEnum
Global Const $ADOENUM_adCopyUnspecified = -1
Global Const $ADOENUM_adCopyOverWrite = 1
Global Const $ADOENUM_adCopyNonRecursive = 2
Global Const $ADOENUM_adCopyAllowEmulation = 4

; CursorLocationEnum
Global Const $ADOENUM_adUseNone = 1
Global Const $ADOENUM_adUseServer = 2
Global Const $ADOENUM_adUseClient = 3

; CursorOptionEnum
Global Const $ADOENUM_adAddNew = 0x1000400
Global Const $ADOENUM_adApproxPosition = 0x4000
Global Const $ADOENUM_adBookmark = 0x2000
Global Const $ADOENUM_adDelete = 0x1000800
Global Const $ADOENUM_adFind = 0x80000
Global Const $ADOENUM_adHoldRecords = 0x100
Global Const $ADOENUM_adIndex = 0x100000
Global Const $ADOENUM_adMovePrevious = 0x200
Global Const $ADOENUM_adNotify = 0x40000
Global Const $ADOENUM_adResync = 0x20000
Global Const $ADOENUM_adSeek = 0x200000
Global Const $ADOENUM_adUpdate = 0x1008000
Global Const $ADOENUM_adUpdateBatch = 0x10000

;~ CursorTypeEnum
;~ https://msdn.microsoft.com/en-us/library/windows/desktop/ms681771(v=vs.85).aspx
Global Const $ADOENUM_adOpenUnspecified = -1 ; Does not specify the type of cursor.
Global Const $ADOENUM_adOpenForwardOnly = 0 ; Default. Uses a forward-only cursor. Identical to a static cursor, except that you can only scroll forward through records. This improves performance when you need to make only one pass through a Recordset.
Global Const $ADOENUM_adOpenKeyset = 1 ; Uses a keyset cursor. Like a dynamic cursor, except that you can't see records that other users add, although records that other users delete are inaccessible from your Recordset. Data changes by other users are still visible.
Global Const $ADOENUM_adOpenDynamic = 2 ; Uses a dynamic cursor. Additions, changes, and deletions by other users are visible, and all types of movement through the Recordset are allowed, except for bookmarks, if the provider doesn't support them.
Global Const $ADOENUM_adOpenStatic = 3 ; Uses a static cursor, which is a static copy of a set of records that you can use to find data or generate reports. Additions, changes, or deletions by other users are not visible.

; DataTypeEnum
Global Const $ADOENUM_adArray = 0x2000
Global Const $ADOENUM_adBigInt = 20
Global Const $ADOENUM_adBinary = 128
Global Const $ADOENUM_adBoolean = 11
Global Const $ADOENUM_adBSTR = 8
Global Const $ADOENUM_adChapter = 136
Global Const $ADOENUM_adChar = 129
Global Const $ADOENUM_adCurrency = 6
Global Const $ADOENUM_adDate = 7
Global Const $ADOENUM_adDBDate = 133
Global Const $ADOENUM_adDBTime = 134
Global Const $ADOENUM_adDBTimeStamp = 135
Global Const $ADOENUM_adDecimal = 14
Global Const $ADOENUM_adDouble = 5
Global Const $ADOENUM_adEmpty = 0
Global Const $ADOENUM_adError = 10
Global Const $ADOENUM_adFileTime = 64
Global Const $ADOENUM_adGUID = 72
Global Const $ADOENUM_adIDispatch = 9
Global Const $ADOENUM_adInteger = 3
Global Const $ADOENUM_adIUnknown = 13
Global Const $ADOENUM_adLongVarBinary = 205
Global Const $ADOENUM_adLongVarChar = 201
Global Const $ADOENUM_adLongVarWChar = 203
Global Const $ADOENUM_adNumeric = 131
Global Const $ADOENUM_adPropVariant = 138
Global Const $ADOENUM_adSingle = 4
Global Const $ADOENUM_adSmallInt = 2
Global Const $ADOENUM_adTinyInt = 16
Global Const $ADOENUM_adUnsignedBigInt = 21
Global Const $ADOENUM_adUnsignedInt = 19
Global Const $ADOENUM_adUnsignedSmallInt = 18
Global Const $ADOENUM_adUnsignedTinyInt = 17
Global Const $ADOENUM_adUserDefined = 132
Global Const $ADOENUM_adVarBinary = 204
Global Const $ADOENUM_adVarChar = 200
Global Const $ADOENUM_adVariant = 12
Global Const $ADOENUM_adVarNumeric = 139
Global Const $ADOENUM_adVarWChar = 202
Global Const $ADOENUM_adWChar = 130

; EditModeEnum
Global Const $ADOENUM_adEditNone = 0
Global Const $ADOENUM_adEditInProgress = 1
Global Const $ADOENUM_adEditAdd = 2
Global Const $ADOENUM_adEditDelete = 4

; ErrorValueEnum ; https://msdn.microsoft.com/en-us/library/windows/desktop/ms681549(v=vs.85).aspx

#CS
	Global Const $ADOENUM_adErrBoundToCommand = 3707
	Global Const $ADOENUM_adErrCannotComplete = 3732
	Global Const $ADOENUM_adErrCantChangeConnection = 3748
	Global Const $ADOENUM_adErrCantChangeProvider = 3220
	Global Const $ADOENUM_adErrCantConvertvalue = 3724
	Global Const $ADOENUM_adErrCantCreate = 3725
	Global Const $ADOENUM_adErrCatalogNotSet = 3747
	Global Const $ADOENUM_adErrColumnNotOnThisRow = 3726
	Global Const $ADOENUM_adErrDataConversion = 3421
	Global Const $ADOENUM_adErrDataOverflow = 3721
	Global Const $ADOENUM_adErrDelResOutOfScope = 3738
	Global Const $ADOENUM_adErrDenyNotSupported = 3750
	Global Const $ADOENUM_adErrDenyTypeNotSupported = 3751
	Global Const $ADOENUM_adErrFeatureNotAvailable = 3251
	Global Const $ADOENUM_adErrFieldsUpdateFailed = 3749
	Global Const $ADOENUM_adErrIllegalOperation = 3219
	Global Const $ADOENUM_adErrIntegrityViolation = 3719
	Global Const $ADOENUM_adErrInTransaction = 3246
	Global Const $ADOENUM_adErrInvalidArgument = 3001
	Global Const $ADOENUM_adErrInvalidConnection = 3709
	Global Const $ADOENUM_adErrInvalidParamInfo = 3708
	Global Const $ADOENUM_adErrInvalidTransaction = 3714
	Global Const $ADOENUM_adErrInvalidURL = 3729
	Global Const $ADOENUM_adErrItemNotFound = 3265
	Global Const $ADOENUM_adErrNoCurrentRecord = 3021
	Global Const $ADOENUM_adErrNotExecuting = 3715
	Global Const $ADOENUM_adErrNotReentrant = 3710
	Global Const $ADOENUM_adErrObjectClosed = 3704
	Global Const $ADOENUM_adErrObjectInCollection = 3367
	Global Const $ADOENUM_adErrObjectNotSet = 3420
	Global Const $ADOENUM_adErrObjectOpen = 3705
	Global Const $ADOENUM_adErrOpeningFile = 3002
	Global Const $ADOENUM_adErrOperationCancelled = 3712
	Global Const $ADOENUM_adErrOutOfSpace = 3734
	Global Const $ADOENUM_adErrPermissionDenied = 3720
	Global Const $ADOENUM_adErrProviderFailed = 3000
	Global Const $ADOENUM_adErrProviderNotFound = 3706
	Global Const $ADOENUM_adErrReadFile = 3003
	Global Const $ADOENUM_adErrResourceExists = 3731
	Global Const $ADOENUM_adErrResourceLocked = 3730
	Global Const $ADOENUM_adErrResourceOutOfScope = 3735
	Global Const $ADOENUM_adErrSchemaViolation = 3722
	Global Const $ADOENUM_adErrSignMismatch = 3723
	Global Const $ADOENUM_adErrStillConnecting = 3713
	Global Const $ADOENUM_adErrStillExecuting = 3711
	Global Const $ADOENUM_adErrTreePermissionDenied = 3728
	Global Const $ADOENUM_adErrUnavailable = 3736
	Global Const $ADOENUM_adErrUnsafeOperation = 3716
	Global Const $ADOENUM_adErrURLDoesNotExist = 3727
	Global Const $ADOENUM_adErrURLNamedRowDoesNotExist = 3737
	Global Const $ADOENUM_adErrVolumeNotFound = 3733
	Global Const $ADOENUM_adErrWriteFile = 3004
	Global Const $ADOENUM_adWrnSecurityDialog = 3717
	Global Const $ADOENUM_adWrnSecurityDialogHeader = 3718
#CE
Global Const $ADOENUM_adErrProviderFailed = 3000
Global Const $ADOENUM_adErrInvalidArgument = 3001
Global Const $ADOENUM_adErrOpeningFile = 3002
Global Const $ADOENUM_adErrReadFile = 3003
Global Const $ADOENUM_adErrWriteFile = 3004
Global Const $ADOENUM_adErrNoCurrentRecord = 3021
Global Const $ADOENUM_adErrCantChangeProvider = 3220
Global Const $ADOENUM_adErrInTransaction = 3246
Global Const $ADOENUM_adErrFeatureNotAvailable = 3251
Global Const $ADOENUM_adErrItemNotFound = 3265
Global Const $ADOENUM_adErrObjectInCollection = 3367
Global Const $ADOENUM_adErrObjectNotSet = 3420
Global Const $ADOENUM_adErrDataConversion = 3421
Global Const $ADOENUM_adErrObjectClosed = 3704
Global Const $ADOENUM_adErrObjectOpen = 3705
Global Const $ADOENUM_adErrProviderNotFound = 3706
Global Const $ADOENUM_adErrInvalidParamInfo = 3708
Global Const $ADOENUM_adErrInvalidConnection = 3709
Global Const $ADOENUM_adErrNotReentrant = 3710
Global Const $ADOENUM_adErrStillExecuting = 3711
Global Const $ADOENUM_adErrOperationCancelled = 3712
Global Const $ADOENUM_adErrStillConnecting = 3713
Global Const $ADOENUM_adErrInvalidTransaction = 3714
Global Const $ADOENUM_adErrNotExecuting = 3715
Global Const $ADOENUM_adErrUnsafeOperation = 3716
Global Const $ADOENUM_adWrnSecurityDialog = 3717
Global Const $ADOENUM_adWrnSecurityDialogHeader = 3718
Global Const $ADOENUM_adErrIntegrityViolation = 3719
Global Const $ADOENUM_adErrPermissionDenied = 3720
Global Const $ADOENUM_adErrDataOverflow = 3721
Global Const $ADOENUM_adErrSchemaViolation = 3722
Global Const $ADOENUM_adErrSignMismatch = 3723
Global Const $ADOENUM_adErrCantConvertvalue = 3724
Global Const $ADOENUM_adErrCantCreate = 3725
Global Const $ADOENUM_adErrColumnNotOnThisRow = 3726
Global Const $ADOENUM_adErrURLDoesNotExist = 3727
Global Const $ADOENUM_adErrTreePermissionDenied = 3728
Global Const $ADOENUM_adErrInvalidURL = 3729
Global Const $ADOENUM_adErrResourceLocked = 3730
Global Const $ADOENUM_adErrResourceExists = 3731
Global Const $ADOENUM_adErrCannotComplete = 3732
Global Const $ADOENUM_adErrVolumeNotFound = 3733
Global Const $ADOENUM_adErrOutOfSpace = 3734
Global Const $ADOENUM_adErrResourceOutOfScope = 3735
Global Const $ADOENUM_adErrUnavailable = 3736
Global Const $ADOENUM_adErrURLNamedRowDoesNotExist = 3737
Global Const $ADOENUM_adErrDelResOutOfScope = 3738
Global Const $ADOENUM_adErrCatalogNotSet = 3747
Global Const $ADOENUM_adErrCantChangeConnection = 3748
Global Const $ADOENUM_adErrFieldsUpdateFailed = 3749
Global Const $ADOENUM_adErrDenyTypeNotSupported = 3751

; EventReasonEnum
Global Const $ADOENUM_adRsnAddNew = 1
Global Const $ADOENUM_adRsnDelete = 2
Global Const $ADOENUM_adRsnUpdate = 3
Global Const $ADOENUM_adRsnUndoUpdate = 4
Global Const $ADOENUM_adRsnUndoAddNew = 5
Global Const $ADOENUM_adRsnUndoDelete = 6
Global Const $ADOENUM_adRsnRequery = 7
Global Const $ADOENUM_adRsnResynch = 8
Global Const $ADOENUM_adRsnClose = 9
Global Const $ADOENUM_adRsnMove = 10
Global Const $ADOENUM_adRsnFirstChange = 11
Global Const $ADOENUM_adRsnMoveFirst = 12
Global Const $ADOENUM_adRsnMoveNext = 13
Global Const $ADOENUM_adRsnMovePrevious = 14
Global Const $ADOENUM_adRsnMoveLast = 15

; EventStatusEnum
Global Const $ADOENUM_adStatusOK = 1
Global Const $ADOENUM_adStatusErrorsOccurred = 2
Global Const $ADOENUM_adStatusCantDeny = 3
Global Const $ADOENUM_adStatusCancel = 4
Global Const $ADOENUM_adStatusUnwantedEvent = 5

; ExecuteOptionEnum
Global Const $ADOENUM_adAsyncExecute = 0x10
Global Const $ADOENUM_adAsyncFetch = 0x20
Global Const $ADOENUM_adAsyncFetchNonBlocking = 0x40
Global Const $ADOENUM_adExecuteNoRecords = 0x80
Global Const $ADOENUM_adExecuteStream = 0x400
Global Const $ADOENUM_adExecuteRecord = 2048
Global Const $ADOENUM_adOptionUnspecified = -1

; FieldEnum
Global Const $ADOENUM_adDefaultStream = -1
Global Const $ADOENUM_adRecordURL = -2

; FieldAttributeEnum
Global Const $ADOENUM_adFldCacheDeferred = 0x1000
Global Const $ADOENUM_adFldFixed = 0x10
Global Const $ADOENUM_adFldIsChapter = 0x2000
Global Const $ADOENUM_adFldIsCollection = 0x40000
Global Const $ADOENUM_adFldKeyColumn = 0x8000
Global Const $ADOENUM_adFldIsDefaultStream = 0x20000
Global Const $ADOENUM_adFldIsNullable = 0x20
Global Const $ADOENUM_adFldIsRowURL = 0x10000
Global Const $ADOENUM_adFldLong = 0x80
Global Const $ADOENUM_adFldMayBeNull = 0x40
Global Const $ADOENUM_adFldMayDefer = 0x2
Global Const $ADOENUM_adFldNegativeScalem = 0x4000
Global Const $ADOENUM_adFldRowID = 0x100
Global Const $ADOENUM_adFldRowVersion = 0x200
Global Const $ADOENUM_adFldUnknownUpdatable = 0x8
Global Const $ADOENUM_adFldUnspecified = -1
Global Const $ADOENUM_adFldUpdatable = 0x4

; FieldStatusEnum
Global Const $ADOENUM_adFieldOK = 0
Global Const $ADOENUM_adFieldCantConvertValue = 2
Global Const $ADOENUM_adFieldIsNull = 3
Global Const $ADOENUM_adFieldTruncated = 4
Global Const $ADOENUM_adFieldSignMismatch = 5
Global Const $ADOENUM_adFieldDataOverflow = 6
Global Const $ADOENUM_adFieldCantCreate = 7
Global Const $ADOENUM_adFieldUnavailable = 8
Global Const $ADOENUM_adFieldIntegrityViolation = 10
Global Const $ADOENUM_adFieldSchemaViolation = 11
Global Const $ADOENUM_adFieldBadStatus = 12
Global Const $ADOENUM_adFieldDefault = 13
Global Const $ADOENUM_adFieldIgnore = 15
Global Const $ADOENUM_adFieldDoesNotExist = 16
Global Const $ADOENUM_adFieldInvalidURL = 17
Global Const $ADOENUM_adFieldResourceLocked = 18
Global Const $ADOENUM_adFieldResourceExists = 19
Global Const $ADOENUM_adFieldCannotComplete = 20
Global Const $ADOENUM_adFieldVolumeNotFound = 21
Global Const $ADOENUM_adFieldOutOfSpace = 22
Global Const $ADOENUM_adFieldCannotDeleteSource = 23
Global Const $ADOENUM_adFieldResourceOutOfScope = 25
Global Const $ADOENUM_adFieldAlreadyExists = 26
Global Const $ADOENUM_adFieldPendingChange = 0x40000
Global Const $ADOENUM_adFieldPendingDelete = 0x20000
Global Const $ADOENUM_adFieldPendingInsert = 0x10000
Global Const $ADOENUM_adFieldPendingUnknown = 0x80000
Global Const $ADOENUM_adFieldPendingUnknownDelete = 0x100000
Global Const $ADOENUM_adFieldPermissionDenied = 0x9
Global Const $ADOENUM_adFieldReadOnly = 0x24

; FilterGroupEnum
Global Const $ADOENUM_adFilterNone = 0
Global Const $ADOENUM_adFilterPendingRecords = 1
Global Const $ADOENUM_adFilterAffectedRecords = 2
Global Const $ADOENUM_adFilterFetchedRecords = 3
Global Const $ADOENUM_adFilterConflictingRecords = 5

; GetRowsOptionEnum
Global Const $ADOENUM_adGetRowsRest = -1

; IsolationLevelEnum
Global Const $ADOENUM_adXactUnspecified = -1
Global Const $ADOENUM_adXactChaos = 16
Global Const $ADOENUM_adXactBrowse = 256
Global Const $ADOENUM_adXactReadUncommitted = 256
Global Const $ADOENUM_adXactCursorStability = 4096
Global Const $ADOENUM_adXactReadCommitted = 4096
Global Const $ADOENUM_adXactRepeatableRead = 65536
Global Const $ADOENUM_adXactIsolated = 1048576
Global Const $ADOENUM_adXactSerializable = 1048576

; LineSeparatorsEnum
Global Const $ADOENUM_adCRLF = -1
Global Const $ADOENUM_adLF = 10
Global Const $ADOENUM_adCR = 13

; LockTypeEnum
Global Const $ADOENUM_adLockUnspecified = -1
Global Const $ADOENUM_adLockReadOnly = 1
Global Const $ADOENUM_adLockPessimistic = 2
Global Const $ADOENUM_adLockOptimistic = 3
Global Const $ADOENUM_adLockBatchOptimistic = 4

; MarshalOptionsEnum
Global Const $ADOENUM_adMarshalAll = 0
Global Const $ADOENUM_adMarshalModifiedOnly = 1

; MoveRecordOptionsEnum
Global Const $ADOENUM_adMoveUnspecified = -1
Global Const $ADOENUM_adMoveOverWrite = 1
Global Const $ADOENUM_adMoveDontUpdateLinks = 2
Global Const $ADOENUM_adMoveAllowEmulation = 4

;~ ObjectStateEnum
;~ https://msdn.microsoft.com/en-us/library/windows/desktop/ms675546(v=vs.85).aspx
Global Const $ADOENUM_adStateClosed = 0 ;   The object is closed
Global Const $ADOENUM_adStateOpen = 1 ;   The object is open
Global Const $ADOENUM_adStateConnecting = 2 ;   The object is connecting
Global Const $ADOENUM_adStateExecuting = 4 ;   The object is executing a command
Global Const $ADOENUM_adStateFetching = 8 ;   The rows of the object are being retrieved

; ParameterAttributesEnum
Global Const $ADOENUM_adParamSigned = 16
Global Const $ADOENUM_adParamNullable = 64
Global Const $ADOENUM_adParamLong = 128

; ParameterDirectionEnum
Global Const $ADOENUM_adParamUnknown = 0
Global Const $ADOENUM_adParamInput = 1
Global Const $ADOENUM_adParamOutput = 2
Global Const $ADOENUM_adParamInputOutput = 3
Global Const $ADOENUM_adParamReturnValue = 4

; PersistFormatEnum
Global Const $ADOENUM_adPersistADTG = 0
;!!!!
Global Const $ADOENUM_adPersistADO = 1
;!!!!
Global Const $ADOENUM_adPersistXML = 1
Global Const $ADOENUM_adPersistProviderSpecific = 2

; PositionEnum
Global Const $ADOENUM_adPosEOF = -3
Global Const $ADOENUM_adPosBOF = -2
Global Const $ADOENUM_adPosUnknown = -1

; PropertyAttributesEnum
Global Const $ADOENUM_adPropNotSupported = 0
Global Const $ADOENUM_adPropRequired = 1
Global Const $ADOENUM_adPropOptional = 2
Global Const $ADOENUM_adPropRead = 512
Global Const $ADOENUM_adPropWrite = 1024

; RecordCreateOptionsEnum
Global Const $ADOENUM_adFailIfNotExists = -1
Global Const $ADOENUM_adCreateNonCollection = 0
Global Const $ADOENUM_adCreateCollection = 0x2000
Global Const $ADOENUM_adCreateOverwrite = 0x4000000
Global Const $ADOENUM_adCreateStructDoc = 0x80000000
Global Const $ADOENUM_adOpenIfExists = 0x2000000

; RecordOpenOptionsEnum
Global Const $ADOENUM_adDelayFetchFields = 0x8000
Global Const $ADOENUM_adDelayFetchStream = 0x4000
Global Const $ADOENUM_adOpenAsync = 0x1000
Global Const $ADOENUM_adOpenExecuteCommand = 0x10000
Global Const $ADOENUM_adOpenRecordUnspecified = -1
Global Const $ADOENUM_adOpenOutput = 0x800000

; RecordStatusEnum
Global Const $ADOENUM_adRecCanceled = 0x100
Global Const $ADOENUM_adRecCantRelease = 0x400
Global Const $ADOENUM_adRecConcurrencyViolation = 0x800
Global Const $ADOENUM_adRecDBDeleted = 0x40000
Global Const $ADOENUM_adRecDeleted = 0x4
Global Const $ADOENUM_adRecIntegrityViolation = 0x1000
Global Const $ADOENUM_adRecInvalid = 0x10
Global Const $ADOENUM_adRecMaxChangesExceeded = 0x2000
Global Const $ADOENUM_adRecModified = 0x2
Global Const $ADOENUM_adRecMultipleChanges = 0x40
Global Const $ADOENUM_adRecNew = 0x1
Global Const $ADOENUM_adRecObjectOpen = 0x4000
Global Const $ADOENUM_adRecOK = 0
Global Const $ADOENUM_adRecOutOfMemory = 0x8000
Global Const $ADOENUM_adRecPendingChanges = 0x80
Global Const $ADOENUM_adRecPermissionDenied = 0x10000
Global Const $ADOENUM_adRecSchemaViolation = 0x20000
Global Const $ADOENUM_adRecUnmodified = 0x8

; RecordTypeEnum
Global Const $ADOENUM_adSimpleRecord = 0
Global Const $ADOENUM_adCollectionRecord = 1
Global Const $ADOENUM_adRecordUnknown = -1
Global Const $ADOENUM_adStructDoc = 2

; ResyncEnum
Global Const $ADOENUM_adResyncUnderlyingValues = 1
Global Const $ADOENUM_adResyncAllValues = 2

; SaveOptionsEnum
Global Const $ADOENUM_adSaveCreateNotExist = 1
Global Const $ADOENUM_adSaveCreateOverWrite = 2

; SchemaEnum
; https://msdn.microsoft.com/en-us/library/ms675274(v=vs.85).aspx
Global Const $ADOENUM_adSchemaProviderSpecific = -1
Global Const $ADOENUM_adSchemaAsserts = 0
Global Const $ADOENUM_adSchemaCatalogs = 1
Global Const $ADOENUM_adSchemaCharacterSets = 2
Global Const $ADOENUM_adSchemaCollations = 3
Global Const $ADOENUM_adSchemaCheckConstraints = 5
Global Const $ADOENUM_adSchemaColumns = 4
Global Const $ADOENUM_adSchemaConstraintColumnUsage = 6
Global Const $ADOENUM_adSchemaConstraintTableUsage = 7
Global Const $ADOENUM_adSchemaKeyColumnUsage = 8
Global Const $ADOENUM_adSchemaReferentialConstraints = 9
Global Const $ADOENUM_adSchemaTableConstraints = 10
Global Const $ADOENUM_adSchemaColumnsDomainUsage = 11
Global Const $ADOENUM_adSchemaIndexes = 12
Global Const $ADOENUM_adSchemaColumnPrivileges = 13
Global Const $ADOENUM_adSchemaTablePrivileges = 14
Global Const $ADOENUM_adSchemaUsagePrivileges = 15
Global Const $ADOENUM_adSchemaProcedures = 16
Global Const $ADOENUM_adSchemaSchemata = 17
Global Const $ADOENUM_adSchemaSQLLanguages = 18
Global Const $ADOENUM_adSchemaStatistics = 19
Global Const $ADOENUM_adSchemaTables = 20
Global Const $ADOENUM_adSchemaTranslations = 21
Global Const $ADOENUM_adSchemaProviderTypes = 22
Global Const $ADOENUM_adSchemaViews = 23
Global Const $ADOENUM_adSchemaViewColumnUsage = 24
Global Const $ADOENUM_adSchemaViewTableUsage = 25
Global Const $ADOENUM_adSchemaProcedureParameters = 26
Global Const $ADOENUM_adSchemaForeignKeys = 27
Global Const $ADOENUM_adSchemaPrimaryKeys = 28
Global Const $ADOENUM_adSchemaProcedureColumns = 29
Global Const $ADOENUM_adSchemaDBInfoKeywords = 30
Global Const $ADOENUM_adSchemaDBInfoLiterals = 31
Global Const $ADOENUM_adSchemaCubes = 32
Global Const $ADOENUM_adSchemaDimensions = 33
Global Const $ADOENUM_adSchemaHierarchies = 34
Global Const $ADOENUM_adSchemaLevels = 35
Global Const $ADOENUM_adSchemaMeasures = 36
Global Const $ADOENUM_adSchemaProperties = 37
Global Const $ADOENUM_adSchemaMembers = 38
Global Const $ADOENUM_adSchemaTrustees = 39

; SearchDirectionEnum
; https://msdn.microsoft.com/en-us/library/ms676696(v=vs.85).aspx
Global Const $ADOENUM_adSearchBackward = -1
Global Const $ADOENUM_adSearchForward = 1

; SeekEnum
; https://msdn.microsoft.com/en-us/library/ms681524(v=vs.85).aspx
Global Const $ADOENUM_adSeekFirstEQ = 1
Global Const $ADOENUM_adSeekLastEQ = 2
Global Const $ADOENUM_adSeekAfterEQ = 4
Global Const $ADOENUM_adSeekAfter = 8
Global Const $ADOENUM_adSeekBeforeEQ = 16
Global Const $ADOENUM_adSeekBefore = 32

; StreamOpenOptionsEnum
; https://msdn.microsoft.com/en-us/library/ms676706(v=vs.85).aspx
Global Const $ADOENUM_adOpenStreamUnspecified = -1
Global Const $ADOENUM_adOpenStreamAsync = 1
Global Const $ADOENUM_adOpenStreamFromRecord = 4

; StreamReadEnum
; https://msdn.microsoft.com/en-us/library/ms679794(v=vs.85).aspx
Global Const $ADOENUM_adReadLine = -2
Global Const $ADOENUM_adReadAll = -1

; StreamTypeEnum
; https://msdn.microsoft.com/en-us/library/ms675277(v=vs.85).aspx
Global Const $ADOENUM_adTypeBinary = 1
Global Const $ADOENUM_adTypeText = 2

; StreamWriteEnum
; https://msdn.microsoft.com/en-us/library/ms678072(v=vs.85).aspx
Global Const $ADOENUM_adWriteChar = 0
Global Const $ADOENUM_adWriteLine = 1

; StringFormatEnum
Global Const $ADOENUM_adClipString = 2

; XactAttributeEnum
Global Const $ADOENUM_adXactCommitRetaining = 131072
Global Const $ADOENUM_adXactAbortRetaining = 262144


; #FUNCTION# ====================================================================================================================
; Name ..........: _ADO_ERROR_GetErrorInfo
; Description ...:
; Syntax ........: _ADO_ERROR_Description($iError)
; Parameters ....: $iError              - an integer value.
; Return values .: $sDescription
; Author ........: mLipok
; Modified ......:
; Remarks .......:
; Related .......:
; Link ..........: https://msdn.microsoft.com/en-us/library/windows/desktop/ms681549(v=vs.85).aspx
; Example .......: No
; ===============================================================================================================================
Func _ADO_ERROR_GetErrorInfo($iError)
	Local $sDescription = ''
	Switch $iError
		Case $ADOENUM_adErrProviderFailed
			$sDescription = "Provider failed to perform the requested operation."
		Case $ADOENUM_adErrInvalidArgument
			$sDescription = "Arguments are of the wrong type, are out of acceptable range, or are in conflict with one another. This error is often caused by a typographical error in an SQL SELECT statement. For example, a misspelled field name or table name can generate this error. This error can also occur when a field or table named in a SELECT statement does not exist in the data store."
		Case $ADOENUM_adErrOpeningFile
			$sDescription = "File could not be opened. A misspelled file name was specified, or a file has been moved, renamed, or deleted. Over a network, the drive might be temporarily unavailable or network traffic might be preventing a connection."
		Case $ADOENUM_adErrReadFile
			$sDescription = "File could not be read. The name of the file is specified incorrectly, the file might have been moved or deleted, or the file might have become corrupted."
		Case $ADOENUM_adErrWriteFile
			$sDescription = "Write to file failed. You might have closed a file and then tried to write to it, or the file might be corrupted. If the file is located on a network drive, transient network conditions might prevent writing to a network drive."
		Case $ADOENUM_adErrNoCurrentRecord
			$sDescription = "Either BOF or EOF is True, or the current record has been deleted. Requested operation requires a current record. 3219	adErrIllegalOperation	Operation is not allowed in this context."
		Case $ADOENUM_adErrCantChangeProvider
			$sDescription = "Supplied provider is different from the one already in use."
		Case $ADOENUM_adErrInTransaction
			$sDescription = "Connection object cannot be explicitly closed while in a transaction. A Recordset or Connection object that is currently participating in a transaction cannot be closed. Call either RollbackTrans or CommitTrans before closing the object."
		Case $ADOENUM_adErrFeatureNotAvailable
			$sDescription = "The object or provider is not capable of performing the requested operation. Some operations depend on a particular provider version."
		Case $ADOENUM_adErrItemNotFound
			$sDescription = "Item cannot be found in the collection corresponding to the requested name or ordinal. An incorrect field or table name has been specified."
		Case $ADOENUM_adErrObjectInCollection
			$sDescription = "Object is already in collection. Cannot append. An object cannot be added to the same collection twice."
		Case $ADOENUM_adErrObjectNotSet
			$sDescription = "Object is no longer valid."
		Case $ADOENUM_adErrDataConversion
			$sDescription = "Application uses a value of the wrong type for the current operation. You might have supplied a string to an operation that expects a stream, for example."
		Case $ADOENUM_adErrObjectClosed
			$sDescription = "Operation is not allowed when the object is closed. TheConnection or Recordset has been closed. For example, some other routine might have closed a global object. You can prevent this error by checking the State property before you attempt an operation."
		Case $ADOENUM_adErrObjectOpen
			$sDescription = "Operation is not allowed when the object is open. An object that is open cannot be opened. Fields cannot be appended to an open Recordset."
		Case $ADOENUM_adErrProviderNotFound
			$sDescription = "Provider cannot be found. It may not be properly installed. 3707	adErrBoundToCommand	The ActiveConnection property of a Recordset object, which has a Command object as its source, cannot be changed. The application attempted to assign a newConnection object to a Recordset that has a Commandobject as its source."
		Case $ADOENUM_adErrInvalidParamInfo
			$sDescription = "Parameter object is improperly defined. Inconsistent or incomplete information was provided."
		Case $ADOENUM_adErrInvalidConnection
			$sDescription = "The connection cannot be used to perform this operation. It is either closed or invalid in this context."
		Case $ADOENUM_adErrNotReentrant
			$sDescription = "Operation cannot be performed while processing event. An operation cannot be performed within an event handler that causes the event to fire again. For example, navigation methods should not be called from within aWillMove event handler."
		Case $ADOENUM_adErrStillExecuting
			$sDescription = "Operation cannot be performed while executing asynchronously."
		Case $ADOENUM_adErrOperationCancelled
			$sDescription = "Operation has been canceled by the user. The application has called the CancelUpdate or CancelBatch method and the current operation has been canceled."
		Case $ADOENUM_adErrStillConnecting
			$sDescription = "Operation cannot be performed while connecting asynchronously."
		Case $ADOENUM_adErrInvalidTransaction
			$sDescription = "Coordinating transaction is invalid or has not started."
		Case $ADOENUM_adErrNotExecuting
			$sDescription = "Operation cannot be performed while not executing."
		Case $ADOENUM_adErrUnsafeOperation
			$sDescription = "Safety settings on this computer prohibit accessing a data source on another domain."
		Case $ADOENUM_adWrnSecurityDialog
			$sDescription = "For internal use only. Don't use. (Entry was included for the sake of completeness. This error should not appear in your code.)"
		Case $ADOENUM_adWrnSecurityDialogHeader
			$sDescription = "For internal use only. Don't use. (Entry included for the sake of completeness. This error should not appear in your code.)"
		Case $ADOENUM_adErrIntegrityViolation
			$sDescription = "Data value conflicts with the integrity constraints of the field. A new value for a Field would cause a duplicate key. A value that forms one side of a relationship between two records might not be updatable."
		Case $ADOENUM_adErrPermissionDenied
			$sDescription = "Insufficient permission prevents writing to the field. The user named in the connection string does not have the proper permissions to write to a Field."
		Case $ADOENUM_adErrDataOverflow
			$sDescription = "Data value is too large to be represented by the field data type. A numeric value that is too large for the intended field was assigned. For example, a long integer value was assigned to a short integer field."
		Case $ADOENUM_adErrSchemaViolation
			$sDescription = "Data value conflicts with the data type or constraints of the field. The data store has validation constraints that differ from the Field value."
		Case $ADOENUM_adErrSignMismatch
			$sDescription = "Conversion failed because the data value was signed and the field data type used by the provider was unsigned."
		Case $ADOENUM_adErrCantConvertvalue
			$sDescription = "Data value cannot be converted for reasons other than sign mismatch or data overflow. For example, conversion would have truncated data."
		Case $ADOENUM_adErrCantCreate
			$sDescription = "Data value cannot be set or retrieved because the field data type was unknown, or the provider had insufficient resources to perform the operation."
		Case $ADOENUM_adErrColumnNotOnThisRow
			$sDescription = "Record does not contain this field. An incorrect field name was specified or a field not in the Fields collection of the current record was referenced."
		Case $ADOENUM_adErrURLDoesNotExist
			$sDescription = "Either the source URL or the parent of the destination URL does not exist. There is a typographical error in either the source or destination URL. You might havehttp://mysite/photo/myphoto.jpg when you should actually have http://mysite/photos/myphoto.jpginstead. The typographical error in the parent URL (in this case, photo instead of photos) has caused the error."
		Case $ADOENUM_adErrTreePermissionDenied
			$sDescription = "Permissions are insufficient to access tree or subtree. The user named in the connection string does not have the appropriate permissions."
		Case $ADOENUM_adErrInvalidURL
			$sDescription = "URL contains invalid characters. Make sure the URL is typed correctly. The URL follows the scheme registered to the current provider (for example, Internet Publishing Provider is registered for http)."
		Case $ADOENUM_adErrResourceLocked
			$sDescription = "Object represented by the specified URL is locked by one or more other processes. Wait until the process has finished and attempt the operation again. The object you are trying to access has been locked by another user or by another process in your application. This is most likely to arise in a multi-user environment."
		Case $ADOENUM_adErrResourceExists
			$sDescription = "Copy operation cannot be performed. Object named by destination URL already exists. Specify adCopyOverwriteto replace the object. If you do not specifyadCopyOverwrite when copying the files in a directory, the copy fails when you try to copy an item that already exists in the destination location."
		Case $ADOENUM_adErrCannotComplete
			$sDescription = "The server cannot complete the operation. This might be because the server is busy with other operations or it might be low on resources."
		Case $ADOENUM_adErrVolumeNotFound
			$sDescription = "Provider cannot locate the storage device indicated by the URL. Make sure the URL is typed correctly. The URL of the storage device might be incorrect, but this error can occur for other reasons. The device might be offline or a large volume of network traffic might prevent the connection from being made."
		Case $ADOENUM_adErrOutOfSpace
			$sDescription = "Operation cannot be performed. Provider cannot obtain enough storage space. There might not be enough RAM or hard-drive space for temporary files on the server."
		Case $ADOENUM_adErrResourceOutOfScope
			$sDescription = "Source or destination URL is outside the scope of the current record."
		Case $ADOENUM_adErrUnavailable
			$sDescription = "Operation failed to complete and the status is unavailable. The field may be unavailable or the operation was not attempted. Another user might have changed or deleted the field you are trying to access."
		Case $ADOENUM_adErrURLNamedRowDoesNotExist
			$sDescription = "Record named by this URL does not exist. While attempting to open a file using a Record object, either the file name or the path to the file was misspelled."
		Case $ADOENUM_adErrDelResOutOfScope
			$sDescription = "The URL of the object to be deleted is outside the scope of the current record."
		Case $ADOENUM_adErrCatalogNotSet
			$sDescription = "Operation requires a valid ParentCatalog."
		Case $ADOENUM_adErrCantChangeConnection
			$sDescription = "Connection was denied. The new connection you requested has different characteristics than the one already in use."
		Case $ADOENUM_adErrFieldsUpdateFailed
			$sDescription = "Fields update failed. For further information, examine theStatus property of individual field objects. This error can occur in two situations: when changing a Field object's value in the process of changing or adding a record to the database; and when changing the properties of the Fieldobject itself. 3750	adErrDenyNotSupported	Provider does not support sharing restrictions. An attempt was made to restrict file sharing and your provider does not support the concept."
		Case $ADOENUM_adErrDenyTypeNotSupported
			$sDescription = "Provider does not support the requested kind of sharing restriction. An attempt was made to establish a particular type of file-sharing restriction that is not supported by your provider. See the provider's documentation to determine what file-sharing restrictions are supported."
	EndSwitch
	Return $sDescription
EndFunc   ;==>_ADO_ERROR_GetErrorInfo
