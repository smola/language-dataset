// new module header

/// Return values. 
enum AGSP_ReturnType 
{
	/// Operation completed successfully.
	eAGSP_OK = 0,
	/// Invalid arguments were passed to the function. 
	eAGSP_ERR_ARG = -1,
	/// A system error occurred while executing the operation. 
	eAGSP_ERR_FAIL = -2,
	/// A memory allocation failed while executing the operation.
	eAGSP_ERR_MEM = -3,
	/// The requested operation is not supported by this system or device.
	eAGSP_ERR_SUPP = -4,
	/// The requested operation was executed in an uninitialized or null object;
	eAGSP_ERR_NULLPTR = -10
};

/// Port access modes. 
enum AGSP_ModeType 
{
	/// Open port for read access. 
	eAGSP_Mode_Read = 1,
	/// Open port for write access. 
	eAGSP_Mode_Write = 2,
	/// Open port for read and write access.  
	eAGSP_Mode_ReadWrite = 3
};

/// Buffer selection.
enum AGSP_BufferType 
{
	/// Input buffer. 
	eAGSP_Buf_Input = 1,
	/// Output buffer.
	eAGSP_Buf_Output = 2,
	/// Both buffers. 
	eAGSP_Buf_InputOutput = 3
};

/// Parity settings. 
enum AGSP_ParityType 
{
	/// Special value to indicate setting should be left alone. 
	eAGSP_Parity_Invalid = -1,
	/// No parity. 
	eAGSP_Parity_None = 0,
	/// Odd parity. 
	eAGSP_Parity_Odd = 1,
	///  Even parity. 
	eAGSP_Parity_Even = 2,
	/// Mark parity. 
	eAGSP_Parity_Mark = 3,
	///  Space parity. 
	eAGSP_Parity_Space = 4
};

/// RTS pin behaviour. 
enum AGSP_RTS_PinType 
{
	/// Special value to indicate setting should be left alone. 
	eAGSP_RTS_Invalid = -1,
	/// RTS off. 
	eAGSP_RTS_Off = 0,
	/// RTS on. 
	eAGSP_RTS_On = 1,
	/// RTS used for flow control. 
	eAGSP_RTS_FlowControl = 2
};

/// CTS pin behaviour. 
enum AGSP_CTS_PinType 
{
	/// Special value to indicate setting should be left alone. 
	eAGSP_CTS_Invalid = -1,
	/// CTS ignored. 
	eAGSP_CTS_Ignore = 0,
	/// CTS used for flow control. 
	eAGSP_CTS_FlowControl = 1
};

/// DTR pin behaviour. 
enum AGSP_DTR_PinType 
{
	/// Special value to indicate setting should be left alone. 
	eAGSP_DTR_Invalid = -1,
	/// DTR off. 
	eAGSP_DTR_Off = 0,
	/// DTR on. 
	eAGSP_DTR_On = 1,
	/// DTR used for flow control.
	eAGSP_DTR_FlowControl= 2
};

/// DSR pin behaviour. 
enum AGSP_DSR_PinType 
{
	/// Special value to indicate setting should be left alone. 
	eAGSP_DSR_Invalid = -1,
	/// DSR ignored. 
	eAGSP_DSR_Ignore = 0,
	/// DSR used for flow control. 
	eAGSP_DSR_FlowControl = 1
};

/// XON/XOFF flow control behaviour. 
enum AGSP_XonXoffType 
{
	/// Special value to indicate setting should be left alone. 
	eAGSP_XonXoff_Invalid = -1,
	/// XON/XOFF disabled. 
	eAGSP_XonXoff_Disabled = 0,
	/// XON/XOFF enabled for input only. 
	eAGSP_XonXoff_In = 1,
	/// XON/XOFF enabled for output only. 
	eAGSP_XonXoff_Out = 2,
	/// XON/XOFF enabled for input and output. 
	eAGSP_XonXoff_InOut = 3
};

/// Standard flow control combinations.
enum AGSP_FlowControlType 
{
	/// No flow control. 
	eAGSP_FlowControl_None = 0,
	/// Software flow control using XON/XOFF characters.
	eAGSP_FlowControl_XonXoff = 1,
	/// Hardware flow control using RTS/CTS signals. 
	eAGSP_FlowControl_RTSCTS = 2,
	/// Hardware flow control using DTR/DSR signals. 
	eAGSP_FlowControl_DTRDSR = 3
};

/// Input signals. 
enum AGSP_SignalType 
{
	/// Clear to send. 
	eAGSP_Signal_CTS = 1,
	/// Data set ready. 
	eAGSP_Signal_DSR = 2,
	/// Data carrier detect. 
	eAGSP_Signal_DCD = 4,
	/// Ring indicator. 
	eAGSP_Signal_RI = 8
};

/// Transport types.
enum AGSP_TransportType 
{
	/// Native platform serial port. 
	eAGSP_Transport_Native,
	/// USB serial port adapter. 
	eAGSP_Transport_USB,
	/// Bluetooth serial port adapter. 
	eAGSP_TransportT_Bluetooth
};

/// structure representing the configuration for a serial port.
builtin managed struct SP_PortConfig
{
  /// Create a port configuration structure.
  import static SP_PortConfig* Create(); // $AUTOCOMPLETESTATICONLY$ 
  /// The baud rate from a port configuration.
  import attribute int Baudrate;
  import int get_Baudrate(); // $AUTOCOMPLETEIGNORE$
  import void set_Baudrate(int baudrate); // $AUTOCOMPLETEIGNORE$
  /// The data bits in a port configuration.
  import attribute int Bits;
  import int get_Bits(); // $AUTOCOMPLETEIGNORE$
  import void set_Bits(int bits); // $AUTOCOMPLETEIGNORE$
  /// The parity setting in a port configuration.
  import attribute AGSP_ParityType Parity;
  import AGSP_ParityType get_Parity(); // $AUTOCOMPLETEIGNORE$
  import void set_Parity(AGSP_ParityType parity); // $AUTOCOMPLETEIGNORE$
  /// The stop bits in a port configuration.
  import attribute int StopBits;
  import int get_StopBits(); // $AUTOCOMPLETEIGNORE$
  import void set_StopBits(int stopbits); // $AUTOCOMPLETEIGNORE$
  /// The RTS pin behaviour from a port configuration
  import attribute AGSP_RTS_PinType RTS;
  import AGSP_RTS_PinType get_RTS(); // $AUTOCOMPLETEIGNORE$
  import void set_RTS(AGSP_RTS_PinType rts); // $AUTOCOMPLETEIGNORE$
  /// The CTS pin behaviour in a port configuration.
  import attribute AGSP_CTS_PinType CTS;
  import AGSP_CTS_PinType get_CTS(); // $AUTOCOMPLETEIGNORE$
  import void set_CTS(AGSP_CTS_PinType cts); // $AUTOCOMPLETEIGNORE$
  /// The DTR pin behaviour in a port configuration
  import attribute AGSP_DTR_PinType DTR;
  import AGSP_DTR_PinType get_DTR(); // $AUTOCOMPLETEIGNORE$
  import void set_DTR(AGSP_DTR_PinType dtr); // $AUTOCOMPLETEIGNORE$
  /// The DSR pin behaviour in a port configuration.
  import attribute AGSP_DSR_PinType DSR;
  import AGSP_DSR_PinType get_DSR(); // $AUTOCOMPLETEIGNORE$
  import void set_DSR(AGSP_DSR_PinType dsr); // $AUTOCOMPLETEIGNORE$
  /// The XON/XOFF configuration in a port configuration.
  import attribute AGSP_XonXoffType XonXoff;
  import AGSP_XonXoffType get_XonXoff(); // $AUTOCOMPLETEIGNORE$
  import void set_XonXoff(AGSP_XonXoffType xonxoff); // $AUTOCOMPLETEIGNORE$

  /// Sets the RTS, CTS, DTR, DSR and XON/XOFF as necessary for the specified flow control type.
  import void SetFlowControl(AGSP_FlowControlType flowcontrol); 
};

/// structure representing a serial port.
builtin managed struct SP_Port
{
  /// Creates a port from the OS-specific name of a serial port.
  import static SP_Port* Create(String portname); // $AUTOCOMPLETESTATICONLY$ 
  /// Open the specified serial port.
  import AGSP_ReturnType Open(AGSP_ModeType mode);
  /// Close the specified serial port. Port must be open.
  import AGSP_ReturnType Close();
  /// Get the name of a port. The name returned is whatever is normally used to refer to a port on the current operating system.
  import readonly attribute String Name;
  import String get_Name(); // $AUTOCOMPLETEIGNORE$
  /// Get a description for a port, to present to end user.
  import readonly attribute String Description;
  import String get_Description(); // $AUTOCOMPLETEIGNORE$
  /// Get the transport type used by a port. Port must be open.
  import readonly attribute AGSP_TransportType Transport;
  import AGSP_TransportType get_Transport(); // $AUTOCOMPLETEIGNORE$
  /// Get Port configuration. Port must be open.
  import SP_PortConfig* GetConfig();
  /// Set Port configuration. Port must be open.
  import AGSP_ReturnType SetConfig(SP_PortConfig* config);
  /// Read bytes from the specified serial port, without blocking. Port must be open.
  import String Read(int count);
  /// Write bytes to the specified serial port, without blocking. Port must be open.
  import AGSP_ReturnType Write(String buffer);
  /// Gets the number of bytes waiting in the input buffer. Port must be open.
  import readonly attribute AGSP_ReturnType WaitingBytesRead;
  import AGSP_ReturnType get_WaitingBytesRead(); // $AUTOCOMPLETEIGNORE$
  /// Gets the number of bytes waiting in the output buffer. Port must be open.
  import readonly attribute AGSP_ReturnType WaitingBytesWrite;
  import AGSP_ReturnType get_WaitingBytesWrite(); // $AUTOCOMPLETEIGNORE$
  /// Flush serial port buffers. Data in the selected buffer(s) is discarded. Port must be open.
  import AGSP_ReturnType Flush(AGSP_BufferType buffers);
  
  /// Set port Baudrate. Port must be open.
  import AGSP_ReturnType SetBaudrate(int baudrate);
  /// Set port Bits. Port must be open.
  import AGSP_ReturnType SetBits(int bits);
  /// Set port Parity. Port must be open.
  import AGSP_ReturnType SetParity(AGSP_ParityType parity);
  /// Set port Stop Bits. Port must be open.
  import AGSP_ReturnType SetStopBits(int StopBits);
  /// Sets the RTS, CTS, DTR, DSR and XON/XOFF as necessary for the specified port flow control type. Port must be open.
  import AGSP_ReturnType SetFlowControl(AGSP_FlowControlType flowcontrol);
};

builtin managed struct AGSP
{
  /// Refreshes Port Names list
  import static void UpdatePortNames();
  /// Get the number of serial ports.
  import static readonly attribute int PortNamesCount; // $AUTOCOMPLETESTATICONLY$ 
  import static int get_PortNamesCount(); // $AUTOCOMPLETEIGNORE$
  /// Get the OS-specific name of a serial port by index.
  import static readonly attribute String PortNames[]; // $AUTOCOMPLETESTATICONLY$ 
  import static String get_PortNames(int i); // $AUTOCOMPLETEIGNORE$
};
