(* ****** ****** *)
//
// portaudio.sats
// All the structures, definitions, and macros for the PortAudio library
//
(* ****** ****** *)

%{#
#include "atscntrb-rk-libportaudio/portaudio.cats"
%}

#define ATS_PACKNAME "ATSCNTRB.PA"
#define ATS_EXTERN_PREFIX "atscntrb_portaudio_"

(* ****** ****** *)

typedef PaError = int
typedef PaDeviceIndex = int
typedef PaHostApiIndex = int
typedef PaTime = double
typedef PaSampleFormat = ulint
typedef PaStream = ptr
typedef PaStreamFlags = ulint
typedef PaStreamCallbackFlags = ulint

(* ****** ****** *)

abst@ype PaHostApiTypeId = int
abst@ype PaStreamCallbackResult = int

(* ****** ****** *)

typedef PaVersionInfo = $extype_struct"PaVersionInfo" of {
  versionMajor = int,
  versionMinor = int,
  versionSubMinor = int,
  versionControlRevision = string,
  versionText = string
}

typedef PaHostApiInfo = $extype_struct"PaHostApiInfo" of {
  structVersion = int,
  type = PaHostApiTypeId,
  name = string,
  deviceCount = int,
  defaultInputDevice = PaDeviceIndex,
  defaultOutputDevice = PaDeviceIndex
}

typedef PaHostErrorInfo = $extype_struct"PaHostErrorInfo" of {
  hostApiType = PaHostApiTypeId,
  errorCode = lint,
  errorText = string
}

typedef PaDeviceInfo = $extype_struct"PaDeviceInfo" of {
  structVersion = int,
  name = string,
  hostApi = PaHostApiIndex,
  maxInputChannels = int,
  maxOutputChannels = int,
  defaultLowInputLatency = PaTime,
  defaultHighInputLatency = PaTime,
  defaultLowOutputLatency = PaTime,
  defaultHighOutputLatency = PaTime,
  defaultSampleRate = double
}

typedef PaStreamParameters = $extype_struct"PaStreamParameters" of {
  device = PaDeviceIndex,
  channelCount = int,
  sampleFormat = PaSampleFormat,
  suggestedLatency = PaTime,
  hostApiSpecificStreamInfo = ptr
}

typedef PaStreamCallbackTimeInfo = $extype_struct"PaStreamCallbackTimeInfo" of {
  inputBufferAdcTime = PaTime,
  currentTime = PaTime,
  outputBufferDacTime = PaTime
}

typedef PaStreamInfo = $extype_struct"PaStreamInfo" of {
  structVersion = int,
  inputLatency = PaTime,
  outputLatency = PaTime,
  sampleRate = double
}

(* ****** ****** *)

typedef PaStreamCallback =
  (PaStream, PaStream, ulint, PaStreamCallbackTimeInfo,
  PaStreamCallbackFlags, ptr) -> int

typedef PaStreamFinishedCallback = (ptr) -> void

(* ****** ****** *)

macdef paNODevice = $extval(PaDeviceIndex, "-1")
macdef paUseHostApiSpecificDeviceSpecification = $extval(PaDeviceIndex, "-2")
macdef paFloat32 = $extval(PaSampleFormat, "0x00000001")
macdef paInt32 = $extval(PaSampleFormat, "0x00000002")
macdef paInt24 = $extval(PaSampleFormat, "0x00000004")
macdef paInt16 = $extval(PaSampleFormat, "0x00000008")
macdef paInt8 = $extval(PaSampleFormat, "0x00000010")
macdef paUInt8 = $extval(PaSampleFormat, "0x00000020")
macdef paCustomFormat = $extval(PaSampleFormat, "0x00010000")
macdef paNonInterleaved = $extval(PaSampleFormat, "0x80000000")
macdef paFormatIsSupported = 0
macdef paFramesPerBufferUnspecified = 0
macdef paNoFlag = $extval(PaStreamFlags, "0")
macdef paClipOff = $extval(PaStreamFlags, "0x00000001")
macdef paDitherOff = $extval(PaStreamFlags, "0x00000002")
macdef paNeverDropInput = $extval(PaStreamFlags, "0x00000004")
macdef paPrimeOutputBuffersUsingStreamCallback = $extval(PaStreamFlags, "0x00000008")
macdef paPlatformSpecificFlags = $extval(PaStreamFlags, "0xFFFF0000")
macdef paInputUnderflow = $extval(PaStreamCallbackFlags, "0x00000001")
macdef paInputOverflow = $extval(PaStreamCallbackFlags, "0x00000002")
macdef paOutputUnderflow = $extval(PaStreamCallbackFlags, "0x00000004")
macdef paOutputOverflow = $extval(PaStreamCallbackFlags, "0x00000008")
macdef paPrimingOutput = $extval(PaStreamCallbackFlags, "0x00000010")

(* ****** ****** *)

fun Pa_GetVersion(): void = "mac#%"
fun Pa_GetVersionText(): string = "mac#%"
fun Pa_GetVersionInfo(): PaVersionInfo = "mac#%"
fun Pa_GetErrorText(PaError): string = "mac#%"
fun Pa_Initialize(): PaError = "mac#%"
fun Pa_Terminate(): PaError = "mac#%"
fun Pa_GetHostApiCount(): PaHostApiIndex = "mac#%"
fun Pa_GetDefaultHostApi(): PaHostApiIndex = "mac#%"
fun Pa_GetHostApiInfo(PaHostApiIndex): PaHostApiInfo = "mac#%"
fun Pa_HostApiTypeIdToHostApiIndex(PaHostApiTypeId): PaHostApiIndex = "mac#%"
fun Pa_HostApiDeviceIndexToDeviceIndex(PaHostApiIndex, int): PaDeviceIndex = "mac#%"
fun Pa_GetLastHostErrorInfo(): PaHostErrorInfo = "mac#%"
fun Pa_GetDeviceCount(): PaDeviceIndex = "mac#%"
fun Pa_GetDefaultInputDevice(): PaDeviceIndex = "mac#%"
fun Pa_GetDefaultOutputDevice(): PaDeviceIndex = "mac#%"
fun Pa_GetDeviceInfo(PaDeviceIndex): PaDeviceInfo = "mac#%"
fun Pa_IsFormatSupported(
  PaStreamParameters, PaStreamParameters, double
): PaError = "mac#%"
fun Pa_OpenStream(
  PaStream, PaStreamParameters, PaStreamParameters, double,
  ulint, PaStreamFlags, PaStreamCallback, ptr
): PaError = "mac#%"
fun Pa_OpenDefaultStream(
  PaStream, int, int, PaSampleFormat, double,
  ulint, PaStreamCallback, ptr
): PaError = "mac#%"
fun Pa_CloseStream(PaStream): PaError = "mac#%"
fun Pa_SetStreamFinishedCallback(PaStream, PaStreamFinishedCallback): PaError =
  "mac#%"
fun Pa_StartStream(PaStream): PaError = "mac#%"
fun Pa_StopStream(PaStream): PaError = "mac#%"
fun Pa_AbortStream(PaStream): PaError = "mac#%"
fun Pa_IsStreamStopped(PaStream): PaError = "mac#%"
fun Pa_IsStreamActive(PaStream): PaError = "mac#%"
fun Pa_GetStreamInfo(PaStream): PaStreamInfo = "mac#%"
fun Pa_GetStreamTime(PaStream): PaTime = "mac#%"
fun Pa_GetStreamCpuLoad(PaStream): double = "mac#%"
fun Pa_ReadStream(PaStream, ptr, ulint): PaError = "mac#%"
fun Pa_WriteStream(PaStream, ptr, ulint): PaError = "mac#%"
fun Pa_GetStreamReadAvailable(PaStream): lint = "mac#%"
fun Pa_GetStreamWriteAvailable(PaStream): lint = "mac#%"
fun Pa_GetSampleSize(PaSampleFormat): PaError = "mac#%"
fun Pa_Sleep(lint): void = "mac#%"

(* End of [portaudio.sats] *)