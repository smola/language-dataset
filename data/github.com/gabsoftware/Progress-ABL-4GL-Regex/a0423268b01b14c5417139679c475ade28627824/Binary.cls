
 /*------------------------------------------------------------------------
    File        : Binary.cls
    Purpose     :
    Syntax      :
    Description : Binary bitwise operations for INTEGER and INT64
    Author(s)   : Gabriel Hautclocq
    Created     : Fri Mar 28 14:55:58 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Binary:

    /*
        Shifts an integer bits to the left
    */
    METHOD PUBLIC STATIC INTEGER BIN_LSHIFT  ( INPUT pr_in_a AS INTEGER, INPUT pr_in_b AS INTEGER) :
        RETURN INTEGER( pr_in_a * EXP( 2, pr_in_b ) ).
    END METHOD.
    METHOD PUBLIC STATIC INT64   BIN_LSHIFT64( INPUT pr_in_a AS INT64  , INPUT pr_in_b AS INT64  ) :
        RETURN INT64  ( pr_in_a * EXP( 2, pr_in_b ) ).
    END METHOD.

    /*
        Shifts an integer bits to the right
    */
    METHOD PUBLIC STATIC INTEGER BIN_RSHIFT  ( INPUT pr_in_a AS INTEGER, INPUT pr_in_b AS INTEGER) :
        RETURN INTEGER( TRUNCATE( pr_in_a / EXP( 2, pr_in_b ), 0 ) ).
    END METHOD.
    METHOD PUBLIC STATIC INT64   BIN_RSHIFT64( INPUT pr_in_a AS INT64  , INPUT pr_in_b AS INT64  ) :
        RETURN INT64  ( TRUNCATE( pr_in_a / EXP( 2, pr_in_b ), 0 ) ).
    END METHOD.

    /*
        BIN_AND - Binary AND between two integers
        INPUT pr_in_a AS INTEGER - first operand
        INPUT pr_in_b AS INTEGER - second operand
        note : (myval & 255) is the same as (myval mod 256)
    */
    METHOD PUBLIC STATIC INTEGER BIN_AND  ( pr_in_a AS INTEGER, pr_in_b AS INTEGER):
        DEFINE VARIABLE in_res AS INTEGER NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INTEGER NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS(pr_in_a, in_i, 1) + GET-BITS(pr_in_b, in_i, 1) >= 2 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.
    METHOD PUBLIC STATIC INT64   BIN_AND64( pr_in_a AS INT64  , pr_in_b AS INT64  ):
        DEFINE VARIABLE in_res AS INT64   NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INT64   NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS(pr_in_a, in_i, 1) + GET-BITS(pr_in_b, in_i, 1) >= 2 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.

    /*
        BIN_OR - Binary OR between two integers
        INPUT pr_in_a AS INTEGER - first operand
        INPUT pr_in_b AS INTEGER - second operand
    */
    METHOD PUBLIC STATIC INTEGER BIN_OR ( INPUT pr_in_a AS INTEGER, INPUT pr_in_b AS INTEGER ):
        DEFINE VARIABLE in_res AS INTEGER NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INTEGER NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS( pr_in_a, in_i, 1 ) + GET-BITS( pr_in_b, in_i, 1 ) >= 1 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.
    METHOD PUBLIC STATIC INT64   BIN_OR64( INPUT pr_in_a AS INT64  , INPUT pr_in_b AS INT64   ):
        DEFINE VARIABLE in_res AS INT64   NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INT64   NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS( pr_in_a, in_i, 1 ) + GET-BITS( pr_in_b, in_i, 1 ) >= 1 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.

    /*
        BIN_XOR - Binary XOR between two integers
        INPUT pr_in_a AS INTEGER - first operand
        INPUT pr_in_b AS INTEGER - second operand
     */
    METHOD PUBLIC STATIC INTEGER BIN_XOR  ( INPUT pr_in_a AS INTEGER, INPUT pr_in_b AS INTEGER ) :
        DEFINE VARIABLE in_res AS INTEGER NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INTEGER NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS( pr_in_a, in_i, 1) + GET-BITS( pr_in_b, in_i, 1 ) = 1 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.
    METHOD PUBLIC STATIC INT64   BIN_XOR64( INPUT pr_in_a AS INT64  , INPUT pr_in_b AS INT64   ) :
        DEFINE VARIABLE in_res AS INT64   NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INT64   NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS( pr_in_a, in_i, 1) + GET-BITS( pr_in_b, in_i, 1 ) = 1 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.

    /*
        BIN_NOT - Binary NOT of an integer (reverses the bits of the integer)
        INPUT pr_in_a AS INTEGER - operand to negate
    */
    METHOD PUBLIC STATIC INTEGER BIN_NOT  ( INPUT pr_in_a AS INTEGER ) :
        DEFINE VARIABLE in_res AS INTEGER NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INTEGER NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS( pr_in_a, in_i, 1) = 0 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.
    METHOD PUBLIC STATIC INT64 BIN_NOT64( INPUT pr_in_a AS INT64 ) :
        DEFINE VARIABLE in_res AS INT64 NO-UNDO INITIAL 0.
        DEFINE VARIABLE in_i   AS INT64 NO-UNDO.

        DO in_i = 1 TO 32:
            IF GET-BITS( pr_in_a, in_i, 1) = 0 THEN
            DO :
                PUT-BITS( in_res, in_i, 1 ) = 1.
            END.
        END.

        RETURN in_res.
    END METHOD.

END CLASS.