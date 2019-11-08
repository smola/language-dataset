#pragma rtGlobals=3		// Use modern global access method and strict wave access.
#pragma ModuleName = glob
#pragma version = 0.1

Function/WAVE glob(pattern)
	String pattern
	
	if(StringMatch(pattern, "root:*")) // absolute path
		WAVE/T buffer = glob_("root:", pattern[5, inf])
	elseif(StringMatch(pattern, ":*")) // relative path (beginning with :)
		WAVE/T buffer = glob_(":", pattern[1, inf])	
	else // relative path (others)
		WAVE/T buffer = glob_("", pattern)	
		buffer = (buffer)[1, inf] // glob_ adds : to represent a relative path
	endif
	
	// glob does not add ending : for folders unless the user adds : 
	if(StringMatch(pattern "*:"))
		buffer = RemoveEnding(buffer, ":") + ":"
	endif
	
	return buffer
End

// Returns a text wave containing matched pathnames
static Function/WAVE glob_(root, pattern)
	String root    // a relative or absolute pathname as a root
	String pattern // a maching pattern using * or **
	
	// the root pathname must end with :
	root = RemoveEnding(root, ":") + ":"

	Make/FREE/T/N=0 buffer
		
	// leaf / inner node of matching algorithm
	if(ItemsInList(pattern, ":") < 2)
		// leaf node of matching algorithm
		
		// data folder only or not 
		if(StringMatch(pattern, "*:"))
			Concatenate/T/NP {MatchedFolders(root, RemoveEnding(pattern, ":"))}, buffer
		else
			Concatenate/T/NP {MatchedObjects(root, pattern), MatchedFolders(root, pattern)}, buffer
		endif

	else
		// inner node of matching algorithm
		
		// refers a parent folder (using ::) or not
		if(StringMatch(pattern, ":*")) 
			// matching with a parent path
			
			root += SelectString(StringMatch(root, "root:"), ":", "") // the root of root: is root: itself			
			Concatenate/T/NP {glob_(root, pattern[1, inf])}, buffer					
		
		else
			// matching recursively
			
			WAVE/T folders = MatchedFolders(root, StringFromList(0, pattern, ":"))
			Variable i
			for(i = 0; i < DimSize(folders, 0); i += 1)
				Concatenate/T/NP {glob_(folders[i], pattern[strsearch(pattern, ":", 0)+1, inf])}, buffer
			endfor
		
		endif		
	endif
	
	// global matching pattern ** is equivalent to *:**
	if(strsearch(StringFromList(0, pattern, ":"), "**", 0) >= 0)
		Concatenate/T/NP {glob_(root, ReplaceString("**", pattern, "*:**", 0, 1))}, buffer
	endif	
	
	return buffer		
End

static Function/WAVE MatchedFolders(root, pattern)
	String root, pattern
	root = RemoveEnding(root, ":") + ":"

	Make/FREE/T/N=(DataFolderExists(root) ? CountObjects(root, 4) : 0) folders = PossiblyQuoteName(GetIndexedObjName(root, 4, p))
	Extract/T/O folders, folders, Match(folders, pattern)
	folders = root + folders
	
	return folders
End

static Function/WAVE MatchedObjects(root, pattern)
	String root, pattern
	root = RemoveEnding(root, ":") + ":"
	
	Make/FREE/T/N=(DataFolderExists(root) ? CountObjects(root, 1) : 0) wavs = GetIndexedObjName(root, 1, p)
	Make/FREE/T/N=(DataFolderExists(root) ? CountObjects(root, 2) : 0) vars = GetIndexedObjName(root, 2, p)
	Make/FREE/T/N=(DataFolderExists(root) ? CountObjects(root, 3) : 0) strs = GetIndexedObjName(root, 3, p)

	Make/FREE/T/N=0 objects
	Concatenate/T/NP {wavs, vars, strs}, objects
	
	Extract/T/O objects, objects, Match(objects, pattern)
	objects = root + objects
	
	return objects
End

static Function Match(str, pattern)
	String str, pattern
	// A pattern matches both of normal a name and a free name.
	// For example, "*1" matches both of "folder1" and "'folder 1'"
	//
	// If a pattern is beginning with !, the return value of StringMatch is reveresed.
	// Because ! has no meaning for the pathname-matching, compare (" " + str) with (" " + pattern) here.
	
	if(StringMatch(str, "'*'"))
		return StringMatch(" " + RemoveEnding(str[1, inf], "'"), " " + pattern) || StringMatch(" " + str, " " + pattern)
	else
		return StringMatch(" " + str, " " + pattern)
	endif	
End
