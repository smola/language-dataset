       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANAGRAMM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  TEMP-FELDER.
           05  CURRENT-WORD     PIC X(20).
           05  CURRENT-WORD-REVERSED PIC X(20).
           05  CURRENT-RESULT   PIC X(12).
           05  POSITION-STRING-END PIC 99.
           05  COUNTER PIC 99.
           05  IST-ANAGRAM-KZ PIC X.
              88 IST-ANAGRAM VALUE 'J'.
              88 IST-KEIN-ANAGRAM VALUE 'N'.
           05 STRING-AUS-DB PIC XX.

       PROCEDURE DIVISION.
           DISPLAY CURRENT-RESULT
           GOBACK
          .

       1000-PROCESS-WORD SECTION.
           MOVE "JA"
             TO CURRENT-RESULT

           PERFORM 1001-FIND-STRING-ENDE

           MOVE FUNCTION REVERSE(CURRENT-WORD(1:POSITION-STRING-END))
             TO CURRENT-WORD-REVERSED


           PERFORM VARYING COUNTER FROM 1 BY 1
            UNTIL COUNTER > 20
            IF CURRENT-WORD (COUNTER:1)
              NOT = CURRENT-WORD-REVERSED (COUNTER:1)
            THEN
              MOVE "NEIN"
                TO CURRENT-RESULT

               EXIT PERFORM
            END-IF
           END-PERFORM
           .
       1000Z.
           EXIT.

       1001-FIND-STRING-ENDE SECTION.
           INITIALIZE POSITION-STRING-END
           INSPECT CURRENT-WORD
                   TALLYING POSITION-STRING-END
                   FOR TRAILING SPACE

           SUBTRACT POSITION-STRING-END
                    FROM FUNCTION LENGTH(CURRENT-WORD)
                    GIVING POSITION-STRING-END
           .
       1001Z.
           EXIT.


       END PROGRAM ANAGRAMM.
