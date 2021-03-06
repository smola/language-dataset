#tag Class
Class CFType
	#tag Method, Flags = &h21
		Private Sub Destructor()
		  Release me.mRef
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Equals(theObj as CFType) As Boolean
		  if theObj is nil then
		    return (me.mRef = nil)
		  end if
		  
		  #if TargetMacOS
		    soft declare function CFEqual lib CarbonLib (cf1 as Ptr, cf2 as Ptr) as Boolean
		    
		    return CFEqual(me.mRef, theObj.Reference)
		  #endif
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function TypeID() As CFTypeID
		  #if TargetMacOS
		    declare function CFGetTypeID lib CarbonLib (cf as Ptr) as UInt32
		    
		    if me.mRef <> nil then
		      return CFTypeID (CFGetTypeID(me.mRef))
		    end if
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function RefCount() As Integer
		  return RefCount(me.mRef)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Hash() As UInt32
		  #if TargetMacOS
		    soft declare function CFHash lib CarbonLib (cf as Ptr) as UInt32
		    
		    if me.mRef <> nil then
		      return CFHash(me.mRef)
		    end if
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Show()
		  #if TargetMacOS
		    soft declare sub CFShow lib CarbonLib (obj as Ptr)
		    
		    if me.mRef <> nil then
		      CFShow me.mRef
		    end if
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Retain()
		  Retain me.mRef
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Release()
		  Release me.mRef
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function NewObject(ref as Ptr, hasOwnership as Boolean, mutability as Integer = kCFPropertyListImmutable) As CFType
		  // This function never returns nil
		  // hasOwnership: pass true if ref comes from a OS's CF... call and has just been retained. The constructor will release it then.
		  
		  // Note: This function is effectively the same as using "new CF...(ref)" if the type is known in advance.
		  //  This means that this function is to be used where the type of "ref" is not known, otherwise the
		  //  explicit class constructors are to be preferred as they're faster.
		  
		  if ref = nil then
		    return new CFType() // this gives a CFType object whose "IsNULL()" function returns true
		  end if
		  
		  if not hasOwnership then
		    Retain(ref)
		    hasOwnership = true
		  end if
		  
		  declare function CFGetTypeID lib CarbonLib (cf as Ptr) as UInt32
		  
		  dim theTypeID as UInt32 = CFGetTypeID(ref)
		  
		  select case theTypeID
		    
		  case TypeValue(CFArray.ClassID)
		    if mutability <> kCFPropertyListImmutable then
		      return new CFMutableArray(ref, hasOwnership)
		    else
		      return new CFArray(ref, hasOwnership)
		    end
		    
		  case TypeValue(CFBoolean.ClassID)
		    static b as CFType = CFBoolean.GetTrue //needed to get the compiler to see the private mRef property
		    if ref = b.mRef then
		      return CFBoolean.GetTrue
		    else
		      return CFBoolean.GetFalse
		    end if
		    
		  case TypeValue(CFBundle.ClassID)
		    dim b as new CFBundle(ref, hasOwnership)
		    return b
		    
		  case TypeValue(CFData.ClassID)
		    if mutability = kCFPropertyListMutableContainersAndLeaves then
		      return new CFMutableData(ref, hasOwnership)
		    else
		      return new CFData(ref, hasOwnership)
		    end
		    
		  case TypeValue(CFDate.ClassID)
		    dim b as new CFDate(ref, hasOwnership)
		    return b
		    
		  case TypeValue(CFDictionary.ClassID)
		    if mutability <> kCFPropertyListImmutable then
		      return new CFMutableDictionary(ref, hasOwnership)
		    else
		      return new CFDictionary(ref, hasOwnership)
		    end
		    
		  case TypeValue(CFNumber.ClassID)
		    dim b as new CFNumber(ref, hasOwnership)
		    return b
		    
		  case TypeValue(CFString.ClassID)
		    if mutability = kCFPropertyListMutableContainersAndLeaves then
		      return new CFMutableString(ref, hasOwnership)
		    else
		      return new CFString(ref, hasOwnership)
		    end
		    
		  case TypeValue(CFURL.ClassID)
		    dim url as new CFURL(ref, hasOwnership)
		    return url
		    
		  case TypeValue(CFNull.ClassID)
		    dim null as new CFNull(ref, hasOwnership)
		    return null
		    
		  else
		    // It's an unknown CF type. Let's return a generic CFType so that at least
		    // this class' operations (Show, Equals, etc.) can be applied to it.
		    
		    dim cft as new CFType()
		    cft.AdoptNoVerify(ref, hasOwnership) // this avoids the type verification
		    return cft
		    
		    #if false
		      // this is not needed but remains in here in case someone wants it back:
		      #if DebugBuild
		        soft declare function CFCopyTypeIDDescription lib CarbonLib (cfid as UInt32) as CFStringRef
		        soft declare function CFCopyDescription lib CarbonLib (cf as Ptr) as CFStringRef
		        soft declare sub CFShow lib CarbonLib ( obj as Ptr )
		        
		        dim cfs as CFStringRef = CFCopyTypeIDDescription ( theTypeID )
		        dim cfd as CFStringRef = CFCopyDescription ( p )
		        System.DebugLog( "type id = " + str(theTypeID) )
		        System.DebugLog( cfs )
		        System.DebugLog( cfd )
		        CFShow(p)
		      #endif
		    #endif
		    
		  end select
		  
		  // we should never arrive here
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Compare(t as CFType) As Integer
		  // Tells whether the two CF objects are the same CF instance but not necessarily the
		  //   same value (for equality check, use the Equals() function)
		  
		  if t is nil then
		    return 1 // this comes from a "x=nil" test, so we must return "not equal" here
		  elseif me.Reference = t.Reference then
		    return 0 // they are equal
		  else
		    return 1 // not equal -- note: we cannot allow sorting of these pointers, so do not diff them!
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Sub Retain(ref as Ptr)
		  #if TargetMacOS
		    soft declare function CFRetain lib CarbonLib (cf as Ptr) as Integer
		    
		    '#if DebugBuild
		    'dim cnt as Integer = RefCount(ref)
		    'System.DebugLog "retain  "+Hex(Integer(ref))+" ("+Str(cnt)+"->"+Str(cnt+1)+")"
		    '#endif
		    
		    if ref <> nil then
		      call CFRetain(ref)
		    end if
		  #endif
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(ref as Ptr, hasOwnership as Boolean)
		  // This is the mandatory constructor for all CFType subclasses.
		  //
		  // Use it when you have declared a CF function from the Carbon framework
		  // (CarbonCore) and retrieved any CF... type. Pass the retrieved CF object
		  // as the 'ref' parameter.
		  //
		  // If the object ref was retrieved by a CF...Copy... or CF...Create... function,
		  // pass 'true' to the 'hasOwnership' parameter. Otherwise, pass 'false'.
		  //
		  // The 'hasOwnership' parameter tells this object whether to balance the
		  // release call in its destructor with a retain call.
		  
		  me.AdoptNoVerify (ref, hasOwnership)
		  me.VerifyType()
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function RefCount(ref as Ptr) As Integer
		  #if TargetMacOS
		    soft declare function CFGetRetainCount lib CarbonLib (cf as Ptr) as Integer
		    if ref <> nil then
		      return CFGetRetainCount(ref)
		    end if
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Sub Release(ref as Ptr)
		  #if TargetMacOS
		    soft declare sub CFRelease lib CarbonLib (cf as Ptr)
		    
		    '#if DebugBuild
		    'dim cnt as Integer = RefCount(ref)
		    'System.DebugLog "release "+Hex(Integer(ref))+" ("+Str(cnt)+"->"+Str(cnt-1)+")"
		    '#endif
		    
		    if ref <> nil then
		      CFRelease ref
		    end if
		  #endif
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Reference() As Ptr
		  return me.mRef
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Constructor()
		  // This is private to make sure no one creates an empty CF... instance
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsNULL() As Boolean
		  return me is nil or me.mRef = nil
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub VerifyType()
		  if not IsNULL and TypeValue(raiseEvent ClassID()) <> TypeValue(me.TypeID()) then
		    raise new TypeMismatchException
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Convert() As Ptr
		  // This is a convenience function to get the reference to the OS object,
		  // for passing to CoreFoundation functions.
		  
		  return me.Reference // Call this function (not return mRef directly) because it might be overridden
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AdoptNoVerify(ref as Ptr, hasOwnership as Boolean)
		  // This method must remain private so that only NewObject may call it,
		  // in order to create a direct CFType object (not subclassed) that doesn't
		  // call VerifyType.
		  // No outside function or subclass should be able to skip the verification,
		  // so don't mess with this.
		  
		  if not hasOwnership and ref <> nil then
		    Retain (ref)
		  end if
		  
		  me.mRef = ref
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function TypeValue(id as CFTypeID) As UInt32
		  return id.opaque
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function TypeDescription(id as CFTypeID) As String
		  #if TargetMacOS
		    declare function CFCopyTypeIDDescription lib CarbonLib (id as UInt32) as Ptr
		    // Caution: If this would return a CFStringRef, we'd have to Retain its value!
		    // Instead, "new CFString()" takes care of that below
		    
		    if id.opaque <> 0 then
		      return new CFString(CFCopyTypeIDDescription(id.opaque), true)
		    end if
		  #endif
		End Function
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event ClassID() As CFTypeID
	#tag EndHook


	#tag Note, Name = Memory Management
		CFType follows the same memory management scheme used by CFStringRef. A CFType object is
		created with whatever reference count the CFTypeRef has, and the CFTypeRef is always released
		by the destructor.
		
		This means that CFType objects created from a Core Foundation Get* function may need to have
		their reference counts incremented by hand -- This is what the "hasOwnership" parameter is for - if you
		only use the Adopt and the CFType.Constructor(Ptr,Boolean) methods, you should not have to worry
		about reference counting.
	#tag EndNote


	#tag Property, Flags = &h21
		Private mRef As Ptr
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Note
			This is mainly useful to see the value in the debugger.
			Warning: Do not use this in your code for other purposes as the value might change
			even between different Mac OS versions!
		#tag EndNote
		#tag Getter
			Get
			#if TargetMacOS
			soft declare function CFCopyDescription lib CarbonLib (cf as Ptr) as Ptr
			// Caution: If this would return a CFStringRef, we'd have to Retain its value!
			// Instead, "new CFString()" takes care of that below
			
			if not me.IsNULL then
			return new CFString(CFCopyDescription(me.mRef), true)
			end if
			#endif
			End Get
		#tag EndGetter
		Description As String
	#tag EndComputedProperty


	#tag Constant, Name = ClassName, Type = String, Dynamic = False, Default = \"CFType", Scope = Private
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Description"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
