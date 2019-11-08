/*------------------------------------------------------------------------
    File        : function_inline.p
    Created     : Fri Mar 03 09:13:41 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

/* ********************  Preprocessor Definitions  ******************** */
function MaxAnnualSpend returns decimal (input pcWhere as char):
    return 15000.
end function.

/* ***************************  Main Block  *************************** */
define buffer cust for s2k.Customer .
 
for each cust where
         Cust.Balance ge MaxAnnualSpend('')
         no-lock:
end.
        
