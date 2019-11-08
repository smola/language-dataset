       *>
       *> test-utf8: reads a text string and convert 
       *> encoding while maintaining correct characters 
       *> 
       *> Coder: BK 
       *>
       IDENTIFICATION DIVISION.
       program-id. test-utf8.
       *>**************************************************
       DATA DIVISION.
       working-storage section.
       *> used in calls to dynamic libraries
       01  wn-rtn-code             PIC  S99   VALUE ZERO.
       01  wc-post-name            PIC X(40)  VALUE SPACE.
       01  wc-post-value           PIC X(40)  VALUE SPACE.
       
       01  wc-pagetitle            PIC X(20) VALUE 'Test utf-8'.
       
       *>**************************************************
       PROCEDURE DIVISION.
       *>**************************************************       
       0000-main.
           
           PERFORM A0100-init
           
           PERFORM C0100-closedown
           
           GOBACK
           .
           
       *>**************************************************          
       A0100-init.       
           
           *> always send out the Content-Type before any other I/O
           CALL 'wui-print-header'  USING wn-rtn-code  
           *>  start html doc
           CALL 'wui-start-html'    USING wc-pagetitle
           
           *> decompose and save current post string
           CALL 'write-post-string' USING wn-rtn-code
           
           IF wn-rtn-code = ZERO

               MOVE ZERO TO wn-rtn-code
               MOVE SPACE TO wc-post-value
               MOVE 'text-utf8' TO wc-post-name
               CALL 'get-post-value' USING wn-rtn-code
                                           wc-post-name wc-post-value

               IF wc-post-value NOT = SPACE
                   DISPLAY wc-post-value
               END-IF
  
           END-IF

           .
       *>**************************************************
       C0100-closedown.

           CALL 'wui-end-html' USING wn-rtn-code 
           
           .
           
       *>**************************************************    
       *> END PROGRAM  
