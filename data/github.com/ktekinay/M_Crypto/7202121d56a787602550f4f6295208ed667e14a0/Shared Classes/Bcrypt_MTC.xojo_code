#tag Module
Protected Module Bcrypt_MTC
	#tag Method, Flags = &h1
		Attributes( deprecated = "Hash" ) Protected Function Bcrypt(key As String, salt As String) As String
		  return Hash( key, salt )
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Attributes( deprecated = "Hash" ) Protected Function Bcrypt(key As String, rounds As UInt8 = 10) As String
		  return Hash( key, rounds )
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function GenerateSalt(rounds As UInt8, preferredPrefix As Prefix = Prefix.Y) As String
		  dim temp as Xojo.Core.MemoryBlock = Xojo.Crypto.GenerateRandomBytes( BCRYPT_MAXSALT )
		  dim csalt as new Xojo.Core.MutableMemoryBlock( temp )
		  temp = nil
		  
		  const kMinRounds as UInt8 = 4
		  const kMaxRounds as UInt8 = 31
		  
		  if rounds < kMinRounds then
		    rounds = kMinRounds
		  elseif rounds > kMaxRounds then
		    rounds = kMaxRounds
		  end if
		  
		  return pEncodeSalt( csalt, rounds, preferredPrefix )
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Hash(key As String, salt As String) As String
		  if salt = "" or key = "" then 
		    return ""
		  end if
		  
		  #if not DebugBuild then
		    #pragma BackgroundTasks False
		    #pragma BoundsChecking False
		    #pragma NilObjectChecking False
		    #pragma StackOverflowChecking False
		  #endif
		  
		  #if kDebug then
		    dim startMs, elapsedMs as double
		    dim logPrefix as string = CurrentMethodName + ": "
		  #endif
		  
		  dim r as string
		  
		  dim state as M_Crypto.BcryptInterface
		  dim rounds as Integer
		  dim logr, minor as UInt8
		  dim ciphertext as new Xojo.Core.MutableMemoryBlock( Xojo.Core.TextEncoding.UTF8.ConvertTextToData( "OrpheanBeholderScryDoubt" ) )
		  static precomputedCiphertext as Xojo.Core.MemoryBlock = Xojo.Core.TextEncoding.UTF8.ConvertTextToData( "hprOBnaeloheSredDyrctbuo" ) // Same every time
		  dim csalt as new Xojo.Core.MutableMemoryBlock( BCRYPT_MAXSALT )
		  dim cdata as new Xojo.Core.MutableMemoryBlock( BCRYPT_BLOCKS * 4 )
		  dim n as Integer
		  
		  dim saltFields() as string = salt.Split( "$" )
		  
		  if saltFields.Ubound <> 3 then
		    return ""
		  end if
		  
		  dim saltVersionString as string = saltFields( 1 )
		  dim saltVersion as integer = Val( saltVersionString )
		  if saltVersionString = "" or saltVersion > Val( BCRYPT_VERSION ) then
		    return ""
		  end if
		  
		  dim saltRounds as string = saltFields( 2 )
		  dim saltText as string = saltFields( 3 )
		  dim saltMB as Xojo.Core.MutableMemoryBlock
		  if true then
		    dim temp as MemoryBlock = saltText
		    dim temp2 as new Xojo.Core.MemoryBlock( temp, temp.Size )
		    saltMB = new Xojo.Core.MutableMemoryBlock( temp2.Size )
		    saltMB.Left( saltMB.Size ) = temp2
		  end if
		  
		  saltVersionString = NthFieldB( saltVersionString, str( saltVersion ), 2 )
		  if saltVersionString <> "" then
		    select case saltVersionString
		    case "a", "x", "y"
		      minor = AscB( saltVersionString.Lowercase )
		    else
		      return ""
		    end select
		  end if
		  
		  n = Val( saltRounds )
		  if n < 0 or n > 31 then
		    return ""
		  end if
		  
		  logr = n
		  rounds = 2 ^ logr
		  if rounds < BCRYPT_MINROUNDS then
		    return ""
		  end if
		  
		  if ( ( saltText.LenB * 3 ) / 4 ) < BCRYPT_MAXSALT then
		    return ""
		  end if
		  
		  // Get the raw data behind the salt
		  pDecodeBase64( csalt, saltMB )
		  if minor >= AscB( "a" ) then
		    key = key + ChrB( 0 ) // Set it up as CString
		  end if
		  
		  // Set up S-Boxes and Subkeys
		  dim keyTemp as MemoryBlock = key
		  dim keyMB as new Xojo.Core.MutableMemoryBlock( keyTemp, keyTemp.Size )
		  
		  state = new Blowfish_MTC( Blowfish_MTC.Padding.NullsOnly )
		  state.ExpandState( csalt, keyMB )
		  #if kDebug then
		    startMs = Microseconds
		  #endif
		  state.Expand0State rounds, keyMB, csalt
		  #if kDebug then
		    elapsedMs = Microseconds - startMs
		    System.DebugLog logPrefix + "state.Expand0State took " + format( elapsedMs, "#,0.0##" ) + " µs"
		  #endif
		  
		  dim lastBlock as UInt32 = BCRYPT_BLOCKS - 1
		  cdata.Left( cdata.Size ) = precomputedCiphertext.Left( cdata.Size ) // Same every time, no need to recompute
		  
		  // Now to encrypt
		  #if kDebug then
		    startMs = Microseconds
		  #endif
		  for k as Integer = 0 to 63
		    state.EncryptMb( cdata )
		  next k
		  #if kDebug then
		    elapsedMs = Microseconds - startMs
		    System.DebugLog logPrefix + "encryption took " + format( elapsedMs, "#,0.0##" ) + " µs"
		  #endif
		  
		  //
		  // Copy the bytes from cdata to cipherText in reverse order
		  //
		  const kMask0 as UInt64 = &hFF00000000000000
		  const kMask1 as UInt64 = &h00FF000000000000
		  const kMask2 as UInt64 = &h0000FF0000000000
		  const kMask3 as UInt64 = &h000000FF00000000
		  const kMask4 as UInt64 = &h00000000FF000000
		  const kMask5 as UInt64 = &h0000000000FF0000
		  const kMask6 as UInt64 = &h000000000000FF00
		  const kMask7 as UInt64 = &h00000000000000FF
		  const kShift1 as UInt64 = 256 ^ 1
		  const kShift3 as UInt64 = 256 ^ 3
		  
		  dim temp as UInt64
		  dim cdataPtr as ptr = cdata.Data
		  dim cipherTextPtr as ptr = cipherText.Data
		  for i as Integer = 0 to lastBlock step 2
		    dim dataIndex as Integer = i * 4
		    temp = cdataPtr.UInt64( dataIndex )
		    cipherTextPtr.UInt64( dataIndex ) = _
		    _ // Word 1
		    ( ( temp and kMask0 ) \ kShift3 ) or _
		    ( ( temp and kMask1 ) \ kShift1 ) or _
		    ( ( temp and kMask2 ) * kShift1 ) or _
		    ( ( temp and kMask3 ) * kShift3 ) or _
		    _ // Word 2
		    ( ( temp and kMask4 ) \ kShift3 ) or _
		    ( ( temp and kMask5 ) \ kShift1 ) or _
		    ( ( temp and kMask6 ) * kShift1 ) or _
		    ( ( temp and kMask7 ) * kShift3 )
		  next i
		  
		  r = "$" + BCRYPT_VERSION + ChrB( minor ) + "$" + format( logr, "00" ) + "$"
		  
		  dim buffer as new Xojo.Core.MutableMemoryBlock( 128 )
		  pEncodeBase64( buffer, csalt )
		  r = r + buffer.CStringValue( 0 )
		  
		  ciphertext = new Xojo.Core.MutableMemoryBlock( ciphertext.Left( ciphertext.Size - 1 ) ) // Lop off last character
		  pEncodeBase64( buffer, ciphertext )
		  r = r + buffer.CStringValue( 0 )
		  
		  return r.DefineEncoding( Encodings.UTF8 )
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Hash(key As String, rounds As UInt8 = 10) As String
		  dim salt as string = GenerateSalt( rounds )
		  return Hash( key, salt )
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pChar64(c As Byte) As Integer
		  static index64() as Integer = Array( _
		  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, _
		  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, _
		  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, _
		  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, _
		  255, 255, 255, 255, 255, 255, 0, 1, 54, 55, _
		  56, 57, 58, 59, 60, 61, 62, 63, 255, 255, _
		  255, 255, 255, 255, 255, 2, 3, 4, 5, 6, _
		  7, 8, 9, 10, 11, 12, 13, 14, 15, 16, _
		  17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, _
		  255, 255, 255, 255, 255, 255, 28, 29, 30, _
		  31, 32, 33, 34, 35, 36, 37, 38, 39, 40, _
		  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, _
		  51, 52, 53, 255, 255, 255, 255, 255 _
		  )
		  
		  if c > 127 then
		    return 255
		  else
		    return index64( c )
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub pDecodeBase64(buffer As Xojo.Core.MutableMemoryBlock, data As Xojo.Core.MemoryBlock)
		  #if not DebugBuild then
		    #pragma BackgroundTasks False
		    #pragma BoundsChecking False
		    #pragma NilObjectChecking False
		    #pragma StackOverflowChecking False
		  #endif
		  
		  dim bp, p as integer
		  dim c1, c2, c3, c4 as byte
		  dim lastByteIndex as integer = buffer.Size - 1
		  dim bufferPtr as Ptr = buffer.Data
		  dim dataPtr as Ptr = data.Data
		  
		  while bp <= lastByteIndex
		    c1 = pChar64( dataPtr.Byte( p ) )
		    c2 = pChar64( dataPtr.Byte( p + 1 ) )
		    
		    if c1 = 255 or c2 = 255 then
		      exit while
		    end if
		    
		    'bufferPtr.Byte( bp ) = Bitwise.ShiftLeft( c1, 2, 8 ) or Bitwise.ShiftRight( c2 and &h30, 4, 8 )
		    bufferPtr.Byte( bp ) = ( c1 * CType( 2 ^ 2,  byte ) ) or ( ( c2 and CType( &h30, byte ) ) \ CType( 2 ^ 4, byte ) )
		    bp = bp + 1
		    if bp > lastByteIndex then
		      exit while
		    end if
		    
		    c3 = pChar64( dataPtr.Byte( p + 2 ) )
		    if c3 = 255 then
		      exit while
		    end if
		    
		    'bufferPtr.Byte( bp ) = Bitwise.ShiftLeft( c2 and &h0F, 4, 8 ) or Bitwise.ShiftRight( c3 and &h3C, 2, 8 )
		    bufferPtr.Byte( bp ) = ( ( c2 and CType( &h0F, byte ) ) * CType( 2 ^ 4, byte ) ) or ( ( c3 and CType( &h3C, byte ) ) \ CType( 2 ^ 2, byte ) )
		    bp = bp + 1
		    if bp > lastByteIndex then
		      exit while
		    end if
		    
		    c4 = pChar64( dataPtr.Byte( p + 3 ) )
		    if c4 = 255 then
		      exit while
		    end if
		    'bufferPtr.Byte( bp ) = Bitwise.ShiftLeft( c3 and &h03, 6, 8 ) or c4
		    bufferPtr.Byte( bp ) = ( ( c3 and CType( &h03, byte ) ) * CType( 2 ^ 6, byte ) ) or c4
		    bp = bp + 1
		    
		    p = p + 4
		  wend
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub pEncodeBase64(buffer As Xojo.Core.MutableMemoryBlock, data As Xojo.Core.MemoryBlock)
		  #if not DebugBuild
		    #pragma BackgroundTasks False
		    #pragma BoundsChecking False
		    #pragma NilObjectChecking False
		    #pragma StackOverflowChecking False
		  #endif
		  
		  dim lastByteIndex as integer = data.Size - 1
		  dim bp, p as integer
		  dim bufferPtr as Ptr = buffer.Data
		  dim dataPtr as Ptr = data.Data
		  dim base64AlphabetPtr as Ptr = Base64AlphabetMB
		  
		  dim c1, c2 as byte
		  
		  while ( p <= lastByteIndex )
		    c1 = dataPtr.Byte( p )
		    p = p + 1
		    
		    'bufferPtr.Byte( bp ) = base64AlphabetPtr.Byte( Bitwise.ShiftRight( c1, 2, 8 ) )
		    bufferPtr.Byte( bp ) = base64AlphabetPtr.Byte( c1 \ CType( 2 ^ 2, byte ) )
		    bp = bp + 1
		    
		    'c1 = Bitwise.ShiftLeft( c1 and &h03, 4, 8 )
		    c1 = ( c1 and CType( &h03, byte ) ) * CType( 2 ^ 4, byte )
		    if p > lastByteIndex then
		      bufferPtr.Byte( bp ) = base64AlphabetPtr.Byte( c1 )
		      bp = bp + 1
		      exit while
		    end if
		    
		    c2 = dataPtr.Byte( p )
		    p = p + 1
		    
		    'c1 = c1 or ( Bitwise.ShiftRight( c2, 4, 8 ) and &h0F )
		    c1 = c1 or ( ( c2 \ CType( 2 ^ 4, byte ) ) and CType( &h0F, byte ) )
		    bufferPtr.Byte( bp ) = base64AlphabetPtr.Byte( c1 )
		    bp = bp + 1
		    
		    'c1 = Bitwise.ShiftLeft( c2 and &h0F, 2, 8 )
		    c1 = ( c2 and CType( &h0F, byte ) ) * CType( 2 ^ 2, byte )
		    if p > lastByteIndex then
		      bufferPtr.Byte( bp ) = base64AlphabetPtr.Byte( c1 )
		      bp = bp + 1
		      exit while
		    end if
		    
		    c2 = dataPtr.Byte( p )
		    p = p + 1
		    
		    'c1 = c1 or ( Bitwise.ShiftRight( c2, 6 , 8 ) and &h03 )
		    c1 = c1 or ( ( c2 \ CType( 2 ^ 6, byte ) ) and CType( &h03, byte ) )
		    bufferPtr.Byte( bp ) = base64AlphabetPtr.Byte( c1 )
		    bp = bp + 1
		    bufferPtr.Byte( bp ) = base64AlphabetPtr.Byte( c2 and &h3F )
		    bp = bp + 1
		  wend
		  
		  bufferPtr.Byte( bp ) = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pEncodeSalt(csalt As Xojo.Core.MemoryBlock, rounds As UInt8, preferredPrefix As Prefix = Prefix.A) As String
		  dim prefixLetter as string = Chr( Integer( preferredPrefix ) )
		  
		  dim salt as string = "$" + BCRYPT_VERSION + prefixLetter + "$"
		  salt = salt + format( rounds, "00" ) + "$"
		  
		  dim buffer as new Xojo.Core.MutableMemoryBlock( csalt.Size * 2 )
		  pEncodeBase64( buffer, csalt )
		  
		  salt = salt + buffer.CStringValue( 0 )
		  return salt
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Verify(key As String, againstHash As String) As Boolean
		  dim data as string = againstHash.NthField( "$", 4 )
		  dim salt as string = againstHash.Left( 7 ) + data.Left(22 )
		  
		  dim hash as string = Hash( key, salt )
		  return hash = againstHash
		End Function
	#tag EndMethod


	#tag Note, Name = BCrypt Notes
		C code for bcrypt here:
		
		http://ftp3.usa.openbsd.org/pub/OpenBSD/src/lib/libc/crypt/bcrypt.c
	#tag EndNote

	#tag Note, Name = License
		
		This is an open-source project.
		
		This project is distributed AS-IS and no warranty of fitness for any particular purpose 
		is expressed or implied. You may freely use or modify this project or any part of 
		within as long as this notice or any other legal notice is left undisturbed.
		
		You may distribute a modified version of this project as long as all modifications 
		are clearly documented and accredited.
		
		This project was created by Kem Tekinay (ktekinay@mactechnologies.com) and
		is housed at:
		
		https://github.com/ktekinay/Blowfish
	#tag EndNote


	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  static mb as MemoryBlock = "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
			  return mb
			End Get
		#tag EndGetter
		Private Base64AlphabetMB As MemoryBlock
	#tag EndComputedProperty


	#tag Constant, Name = BCRYPT_BLOCKS, Type = Double, Dynamic = False, Default = \"6", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = BCRYPT_MAXSALT, Type = Double, Dynamic = False, Default = \"16", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = BCRYPT_MINROUNDS, Type = Double, Dynamic = False, Default = \"6", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = BCRYPT_VERSION, Type = String, Dynamic = False, Default = \"2", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kDebug, Type = Boolean, Dynamic = False, Default = \"False", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kVersion, Type = String, Dynamic = False, Default = \"2.5.2", Scope = Protected
	#tag EndConstant


	#tag Enum, Name = Prefix, Type = Integer, Flags = &h1
		A = 97
		  X = 120
		Y = 121
	#tag EndEnum


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
