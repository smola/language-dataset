-- vim: set ft=terra:
local efi = {}
local utils = require('utils')

efi.guids = {}
struct efi.Guid {
	High: uint64
	Low: uint64
}

local struct HandleTag {}
efi.Handle = &HandleTag

local struct EventTag {}
efi.Event = &EventTag

efi.String = &uint16

efi.Status = utils.enum(uint64, {
	Success = 0,

	LoadError = `1 or (1ull << 63),
	InvalidParameter = `2 or (1ull << 63),
	Unsupported = `3 or (1ull << 63),
	BadBufferSize = `4 or (1ull << 63),
	BufferTooSmall = `5 or (1ull << 63),
	NotReady = `6 or (1ull << 63),
	DeviceError = `7 or (1ull << 63),
	WriteProtected = `8 or (1ull << 63),
	OutOfResources = `9 or (1ull << 63),
	VolumeCorrupted = `10 or (1ull << 63),
	VolumeFull = `11 or (1ull << 63),
	NoMedia = `12 or (1ull << 63),
	MediaChanged = `13 or (1ull << 63),
	NotFound = `14 or (1ull << 63),
	AccessDenied = `15 or (1ull << 63),
	NoResponse = `16 or (1ull << 63),
	NoMapping = `17 or (1ull << 63),
	Timeout = `18 or (1ull << 63),
	NotStarted = `19 or (1ull << 63),
	AlreadyStarted = `20 or (1ull << 63),
	Aborted = `21 or (1ull << 63),
	IcmpError = `22 or (1ull << 63),
	TftpError = `23 or (1ull << 63),
	ProtocolError = `24 or (1ull << 63),
	IncompatibleVersion = `25 or (1ull << 63),
	SecurityViolation = `26 or (1ull << 63),
	CrcError = `27 or (1ull << 63),
	EndOfMedia = `28 or (1ull << 63),
	EndOfFile = `31 or (1ull << 63),
	InvalidLanguage = `32 or (1ull << 63),
	CompromisedData = `33 or (1ull << 63),
	IpAddressConflict = `34 or (1ull << 63),
	HttpError = `35 or (1ull << 63),

	WarnUnknownGlyph = 1,
	WarnDeleteFailure = 2,
	WarnWriteFailure = 3,
	WarnBufferTooSmall = 4,
	WarnStaleData = 5,
	WarnFileSystem = 6
})

efi.TaskPriority = utils.enum(uint64, {
	Application = 4,
	Callback = 8,
	Notify = 16,
	HighLevel = 31
})

struct efi.TableHeader {
	Signature: uint64
	Revision: uint32
	Size: uint32
	Crc32: uint32
	Reserved: uint32
}

struct efi.SimpleTextOutputProtocol
struct efi.SimpleTextInputProtocol
struct efi.RuntimeServices
struct efi.BootServices
struct efi.ConfigurationTable

struct efi.SystemTable {
	Hdr: efi.TableHeader
	FirmwareVendor: efi.String
	FirmareRevision: uint32
	StdInHandle: efi.Handle
	StdIn: &efi.SimpleTextInputProtocol
	StdOutHandle: efi.Handle
	StdOut: &efi.SimpleTextOutputProtocol
	StdErrHandle: efi.Handle
	StdErr: &efi.SimpleTextOutputProtocol
	RuntimeServices: &efi.RuntimeServices
	BootServices: &efi.BootServices
	ConfigurationEntries: uint64
	ConfigurationTable: &efi.ConfigurationTable
}

efi.PhysicalAddress = tuple(uint64)
efi.VirtualAddress = tuple(uint64)

efi.AllocateType = utils.enum(uint32, {
	AnyPages = 0,
	MaxAddress = 1,
	Address = 2
})

efi.MemoryType = utils.enum(uint32, {
	Reserved = 0,
	LoaderCode = 1,
	LoaderData = 2,
	BootServicesCode = 3,
	BootServicesData = 4,
	RuntimeServicesCode = 5,
	RuntimeServicesData = 6,
	Conventional = 7,
	Unusable = 8,
	ACPIReclaim = 9,
	ACPIMemoryNVS = 10,
	MemoryMappedIO = 11,
	MemoryMappedIOPortSpace = 12,
	PalCode = 13,
	Persistent = 14
})

struct efi.MemoryDescriptor {
	Type: uint32
	PhysicalStart: efi.PhysicalAddress
	VirtualStart: efi.VirtualAddress
	NumPages: uint64
	Attributes: uint64
}

struct efi.BootServices {
	Hdr: efi.TableHeader

	-- task priority
	RaiseTPL: &opaque
	RestoreTPL: &opaque

	-- memory
	AllocatePages: {efi.AllocateType.T, efi.MemoryType.T, uint64, &efi.PhysicalAddress} -> efi.Status.T
	FreePages: {efi.PhysicalAddress, uint64} -> efi.Status.T
	GetMemoryMap: {&uint64, &efi.MemoryDescriptor, &uint64, &uint64, &uint32} -> efi.Status.T
	AllocatePool: {efi.MemoryType.T, uint64, &&opaque} -> efi.Status.T
	FreePool: {&opaque} -> efi.Status.T

	-- events/timers
	CreateEvent: &opaque
	SetTimer: &opaque
	WaitForEvent: &opaque
	SignalEvent: &opaque
	CloseEvent: &opaque
	CheckEvent: &opaque

	-- protocol handlers
	InstallProtocolInterface: &opaque
	ReinstallProtocolInterface: &opaque
	UninstallProtocolInterface: &opaque
	HandleProtocol: &opaque
	Reserved: &opaque
	RegisterProtocolNotify: &opaque
	LocateHandle: &opaque
	LocateDevicePath: &opaque
	InstallConfigurationTable: &opaque

	-- images
	LoadImage: &opaque
	StartImage: &opaque
	Exit: {efi.Handle, efi.Status.T, uint64, efi.String} -> efi.Status.T
	UnloadImage: &opaque
	ExitBootServices: {efi.Handle, uint64} -> efi.Status.T

	-- misc
	GetNextMonotonic: &opaque
	Stall: {uint64} -> efi.Status.T
	SetWatchdogTimer: &opaque

	-- driversupport
	ConnectController: &opaque
	DisconnectController: &opaque

	-- protocols
	OpenProtocol: &opaque
	CloseProtocol: &opaque
	OpenProtocolInfo: &opaque

	-- library
	ProtocolsPerHandle: &opaque
	LocateHandleBuffer: &opaque
	LocateProtocol: &opaque
	InstallMultipleInterfaces: &opaque
	UninstallMultipleInterfaces: &opaque

	-- crc32
	CalculateCrc32: &opaque

	-- misc
	CopyMem: {&opaque, &opaque, uint64} -> {}
	SetMem: {&opaque, uint64, uint8} -> {}
	CreateEventEx: &opaque
}

struct efi.RuntimeServices {
	Hdr: efi.TableHeader

	-- time
	GetTime: &opaque
	SetTime: &opaque
	GetWakeupTime: &opaque
	SetWakeupTime: &opaque

	-- virtual memory
	SetVirtualAddressMap: &opaque
	ConvertPointer: &opaque

	-- variables
	GetVariable: &opaque
	GetNextVariableName: &opaque
	SetVariable: &opaque

	-- misc
	GetNextHighMonotonic: &opaque
	ResetSystem: &opaque

	-- capsules
	UpdateCapsule: &opaque
	QueryCapsuleCaps: &opaque

	-- misc 2.0
	QueryVariableInfo: &opaque
}

struct efi.ConfigurationTable {
	VendorGuid: efi.Guid
	VendorTable: &opaque
}

efi.guids.EfiAcpi20Table     = `efi.Guid { 0x8868E871E4F111D3ull, 0xBC220080C73C8881ull }
efi.guids.AcpiTable          = `efi.Guid { 0xEB9D2D302D8811D3ull, 0x9A160090273FC14Dull }
efi.guids.SalSystemTable     = `efi.Guid { 0xEB9D2D322D8811D3ull, 0x9A160090273FC14Dull }
efi.guids.SmbiosTable        = `efi.Guid { 0xEB9D2D312D8811D3ull, 0x9A160090273FC14Dull }
efi.guids.Smbios3Table       = `efi.Guid { 0xF2FD154497944A2Cull, 0x992EE5BBCF20E394ull }
efi.guids.MpsTable           = `efi.Guid { 0xEB9D2D2F2D8811D3ull, 0x9A160090273FC14Dull }
efi.guids.EfiAcpiTable       = `efi.Guid { 0x8868E871E4F111D3ull, 0xBC220080C73C8881ull }
efi.guids.Acpi10Table        = `efi.Guid { 0xEB9D2D302D8811D3ull, 0x9A160090273FC14Dull }
efi.guids.EfiPropertiesTable = `efi.Guid { 0x880AACA34ADC4A04ull, 0x9079B747340825E5ull }

struct efi.SimpleTextOutputProtocol {
	Reset: &opaque
	OutputString: {&efi.SimpleTextOutputProtocol, efi.String} -> efi.Status.T
	TestString: &opaque
	QueryMode: &opaque
	SetMode: &opaque
	SetAttribute: &opaque
	ClearScreen: &opaque
	SetCursorPosition: &opaque
	EnableCursor: &opaque
	Mode: &opaque
}

local efi_string_next = 0

local function efi_string_core(bytes)
	local values = {}

	-- NB ASCII only
	for i = 1, #bytes do
		local c = bytes:sub(i, i):byte()
		local value = `[uint16](c)
		table.insert(values, value)
	end

	table.insert(values, 0)
	local array = `arrayof(uint16, values)
	local storage = global(array)

	storage:setname('_string_' .. efi_string_next)
	efi_string_next = efi_string_next + 1

	return `[efi.String](storage)
end

local efi_string_memoized = terralib.memoize(efi_string_core)

local function efi_string(literal)
	local bytes = literal:asvalue()
	return efi_string_memoized(bytes)
end

efi.s = macro(terralib.memoize(efi_string))

package.loaded['efi'] = efi
