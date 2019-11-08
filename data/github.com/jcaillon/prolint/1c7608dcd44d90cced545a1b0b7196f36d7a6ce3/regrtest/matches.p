/*------------------------------------------------------------------------
    File        : matches.p
    Purpose     : 

    Syntax      :

    Description : some testcases for rule "matches"

    Author(s)   : jurjen
    Created     : Sat Apr 12 13:59:28 CEST 2008
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE somechar AS CHARACTER NO-UNDO.
DEFINE VARIABLE somebool AS logical NO-UNDO.

/* warn me about this use of MATCHES: */
FIND FIRST customer WHERE customer.name MATCHES "*x*":U NO-LOCK NO-ERROR.

/* but do NOT warn me about this use of MATCHES (no database scan) */
somebool = somechar MATCHES "*blah*":U.


