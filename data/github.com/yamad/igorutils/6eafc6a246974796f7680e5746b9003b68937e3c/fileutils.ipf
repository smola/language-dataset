#pragma rtGlobals=1		// Use modern global access method.

// fileutils - Utility functions to work with files

#ifndef FILEUTILS_INCLUDE
#define FILEUTILS_INCLUDE

// Open a file for writing and return reference (handle) to it
Function File_openForWrite(filepath)
    String filepath
    Variable fileref

    Open fileref as filepath
    return fileref
End

// Open a file for reading and return reference to it
Function File_openForRead(filepath)
    String filepath
    Variable fileref

    Open/R fileref as filepath
    return fileref
End

Function File_writeString(fileref, string_in)
    Variable fileref
    String string_in

    FBinWrite fileref, string_in
End

// Close the file pointed to by a file reference
Function File_close(fileref)
    Variable fileref
    Close fileref
End

#endif