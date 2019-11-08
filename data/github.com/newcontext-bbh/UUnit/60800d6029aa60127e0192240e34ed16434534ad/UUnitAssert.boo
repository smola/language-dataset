import System

class UUnitAssert (): 

	static def True(flag as bool, msg as string):
		return if flag 
		raise UUnitAssertException(true, false, msg);
		
	static def True(flag as bool):
		True(flag, null)
		
		
	static def False(flag as bool, msg as string):
		return unless flag
		raise UUnitAssertException(false, true, msg);
		
	static def False(flag as bool):
		False(flag, null)
		
	
	static def Exists(wanted as duck, msg as string):
		return if wanted
		raise UUnitAssertException(wanted, "(not null)", msg)

	static def Exists(wanted as duck):
		Exists(wanted as duck, null)


	static def EqualString(wanted as string, got as string, msg as string):
		return if wanted == got
		raise UUnitAssertException(wanted, got, msg)

	static def EqualString(wanted as string, got as string):
		EqualString(wanted as string, got as string, null)
	
	
	static def EqualDuck(wanted as duck, got as duck, msg as string):
		return if wanted == got 
		raise UUnitAssertException(wanted, got, msg)
	
	static def EqualDuck(wanted as duck, got as duck):
		EqualDuck(wanted, got, null)
