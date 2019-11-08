 
 /*------------------------------------------------------------------------
    File        : String
    Purpose     : Stores an IMMUTABLE array of characters explicitly in memory.
                  Use as an alternative to CHARACTER and LONGCHAR when the 
                  string being stored is unlikely to change.  If it will change,
                  use StringBuilder instead.
    Description : Generally follows the implementation details from the open-
                  source Java OpenJDK class.
    Author(s)   : Abe Voelker
    Created     : Fri Nov 12 17:49:38 CST 2010
    Notes       : * This class is much slower than the Java equivalent due to
                    Progress interpreting a constant string of characters (e.g.
                    "foobar") as a CHARACTER variable, which is not a very
                    primitive datatype, making String instantiation expensive.
                    Java interprets "foobar" as being a char[], which is
                    basically just an array of bytes (2 bytes per char), making
                    it much faster.
                  * No support for special codepages or Unicode (yet)
                  * Limited to ~2GiB of memory.
  ----------------------------------------------------------------------

   Copyright 2010 Abe Voelker

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
   
  */

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS abl.lang.String IMPLEMENTS abl.lang.CharSequence: 

    DEF PRIVATE VAR mValue AS MEMPTR  NO-UNDO. /* Pointer to chunk of memory that holds characters */
    DEF PRIVATE VAR count  AS INTEGER NO-UNDO. /* The actual number of characters used */
		
	CONSTRUCTOR PUBLIC String ():
		SUPER ().
	END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC String (INPUT c AS CHAR):
	    count = LENGTH(c).
		SET-SIZE(mValue) = count + 1.
		PUT-STRING(mValue, 1) = c.
	END CONSTRUCTOR.
	
	DESTRUCTOR PUBLIC String():
	    SET-SIZE(mValue) = 0.
	END DESTRUCTOR.
	
	/* Returns the actual length of the sequence of characters used */
	METHOD PUBLIC INT size():
	    RETURN count.
	END METHOD.
	
	/**
     * Copy characters from this string into mDest starting at iDestBegin.
     * This method doesn't perform any range checking.
     */
    METHOD PUBLIC VOID getChars(mDest AS MEMPTR, iDestBegin AS INT):
        abl.lang.System:arraycopy(mValue, 1, mDest, iDestBegin, count).
    END METHOD.
    
    METHOD PUBLIC VOID getChars(iSrcBegin AS INT, iSrcEnd AS INT, mDest AS MEMPTR, iDestBegin AS INT):
        abl.lang.System:arraycopy(mValue, iSrcBegin, mDest, iDestBegin, iSrcEnd - iSrcBegin + 1).
    END METHOD.
	
	/* Notice the call to StringBuilder... if you will be doing lots of */
	/* appending, just use a StringBuilder object in the first place!   */
	METHOD PUBLIC abl.lang.String append(s AS abl.lang.String):
	    DEF VAR sb AS abl.lang.StringBuilder NO-UNDO.
	    sb = NEW abl.lang.StringBuilder(THIS-OBJECT).
	    sb:append(s).
	    RETURN sb:toStringObj().
	    FINALLY:
	      IF VALID-OBJECT(sb) THEN
            DELETE OBJECT sb NO-ERROR.
	    END.
	END METHOD.
	
	METHOD PUBLIC abl.lang.String append(c AS CHAR):
	    DEF VAR s AS abl.lang.String NO-UNDO.
	    s = NEW abl.lang.String(c).
	    RETURN append(s).
	    FINALLY:
	      IF VALID-OBJECT(s) THEN
	        DELETE OBJECT s NO-ERROR.
	    END.
	END METHOD.
	
	/* @TODO */
	METHOD PUBLIC abl.lang.CharSequence subSequence(iStart AS INT, iEnd AS INT):
	END METHOD.
	
	METHOD PUBLIC abl.lang.String toStringObj():
	    RETURN THIS-OBJECT.
	END METHOD.
	
	METHOD PUBLIC CHARACTER toChar():
	    RETURN GET-STRING(mValue, 1).
	END METHOD.
	
    METHOD PUBLIC LONGCHAR toLongChar():
        DEF VAR lc AS LONGCHAR NO-UNDO.
        IF count LE 0 THEN
            RETURN "".
        COPY-LOB FROM mValue STARTING AT 1 FOR size() TO lc.
        RETURN lc.
    END METHOD.
    
    METHOD PUBLIC OVERRIDE CHARACTER toString():
        RETURN toChar().
    END METHOD.

END CLASS.