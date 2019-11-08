       IDENTIFICATION DIVISION.
       PROGRAM-ID. PISSBALLS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 999 VALUE 1.
       01 WS-PISS PIC 999 VALUE 1.
       01 WS-BALLS PIC 999 VALUE 1.

       PROCEDURE DIVISION.
       PISSBALLS-MAIN SECTION.
           PERFORM 100 TIMES
               COMPUTE WS-PISS = FUNCTION MOD (WS-COUNT, 3)
               COMPUTE WS-BALLS = FUNCTION MOD (WS-COUNT, 5)
               IF WS-PISS = 0 THEN
                   IF WS-BALLS = 0 THEN
                       DISPLAY "PISSBALLS"
                   ELSE
                       DISPLAY "PISS"
                   END-IF
               ELSE
                   IF WS-BALLS = 0 THEN
                       DISPLAY "BALLS"
                   ELSE
                       DISPLAY WS-COUNT
                   END-IF
                END-IF
                ADD 1 TO WS-COUNT
           END-PERFORM
           STOP RUN.
