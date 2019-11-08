&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS SaxHandler 
/*------------------------------------------------------------------------------
  File: sax.p

  Description:  

  Purpose:

  Parameters:   <none>

  Updated: 03/05/02 adams@progress.com
             Initial version
             
-------------------------------------------------------------------------------*/
/*           This .W file was created with the Progress AppBuilder.            */
/*-----------------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created by this procedure. 
   This is a good default which assures that this procedure's triggers and 
   internal procedures will execute in this procedure's storage, and that 
   proper cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE chElementValue  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE inlookupID      AS INTEGER     NO-UNDO.
DEFINE VARIABLE inRow           AS INTEGER     NO-UNDO.
DEFINE VARIABLE inCol           AS INTEGER     NO-UNDO.

{src/OfficeOpenXML/OfficeOpenXMLCommonShared.i SHARED}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SaxHandler
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SaxHandler
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW SaxHandler ASSIGN
         HEIGHT             = 10.24
         WIDTH              = 77.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB SaxHandler 
/* ************************* Included-Libraries *********************** */

{src/adm2/sax.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK SaxHandler 


/* ***************************  Main Block  *************************** */

/* If testing in the AppBuilder, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE characters SaxHandler 
PROCEDURE characters :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pmCharArray   AS MEMPTR NO-UNDO.
  DEFINE INPUT PARAMETER piArrayLength AS INTEGER NO-UNDO.
    
  ASSIGN
    chElementValue = chElementValue + GET-STRING(pmCharArray, 1, piArrayLength).



  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endDocument SaxHandler 
PROCEDURE endDocument :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

/*   STATUS DEFAULT 'Finished XML Document'. */

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endElement SaxHandler 
PROCEDURE endElement :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcNamespaceURI AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcLocalName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcQName        AS CHARACTER NO-UNDO.
  
  CASE pcLocalName:
      WHEN 'sheets':U THEN
          .
          /*DATASET dsSheets:WRITE-XML('file':U, 'sheets.xml':U, TRUE).*/
      WHEN 't':U THEN
      DO:
        IF AVAILABLE ttSharedString THEN
            ASSIGN
                ttSharedString.sharedstring = ttSharedString.sharedstring + chElementValue + ' ':U.

        chElementValue = ''.
      END.
      WHEN 'sst':U THEN
        /*DATASET dsSharedStrings:WRITE-XML('file':U, 'SharedStrings.xml':U,TRUE).*/.
      WHEN 'v':U THEN
      DO:

          IF ttSheetData.sharedString EQ 's':U THEN
          DO:

              FIND FIRST ttSharedString 
                  WHERE ttSharedString.lookupID EQ INTEGER(chElementValue) + 1
                  NO-ERROR.

              IF AVAILABLE ttSharedString THEN
                  ASSIGN 
                    ttSheetData.cellValue = ttSharedString.sharedstring.
          END.
          ELSE
            ASSIGN 
                ttSheetData.cellValue = chElementValue.

         chElementValue = ''.
      END.
  END CASE.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE error SaxHandler 
PROCEDURE error :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcMessage AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcMessage).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startDocument SaxHandler 
PROCEDURE startDocument :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

/*   MESSAGE 'Start XML document'. */

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startElement SaxHandler 
PROCEDURE startElement :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcNamespaceURI AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcLocalName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcQName        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER phAttributes   AS HANDLE NO-UNDO.

  DEFINE VARIABLE inLoop AS INTEGER     NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

    CASE pcLocalName:
        WHEN 'sheet' THEN
        DO:
            CREATE ttSheet.

            DO inLoop = 1 TO phAttributes:NUM-ITEMS:

                CASE phAttributes:GET-LOCALNAME-BY-INDEX(inLoop):
                    WHEN 'name':U THEN
                        ASSIGN
                            ttSheet.sheetname = phAttributes:GET-VALUE-BY-INDEX(inLoop).
                    WHEN 'state':U THEN
                        ASSIGN
                            ttSheet.state = phAttributes:GET-VALUE-BY-INDEX(inLoop).
                    WHEN 'sheetId':U THEN
                        ASSIGN
                            ttSheet.sheetid = phAttributes:GET-VALUE-BY-INDEX(inLoop).
                END CASE.

                CASE phAttributes:GET-QNAME-BY-INDEX(inLoop):
                    WHEN 'r:id':U THEN
                        ASSIGN
                            ttSheet.relid = phAttributes:GET-VALUE-BY-INDEX(inLoop).
                END CASE.
          END.
      END.
      WHEN 'si':U THEN  /*Shared String*/
      DO:
          CREATE ttSharedString.

          ASSIGN
              inlookupID              = inlookupID + 1
              ttSharedString.lookupID = inlookupID.  
      END.
      WHEN 'sheetData':U THEN
      DO:
        ASSIGN 
             inRow = 0
             inCol = 0.
      END.
        
      WHEN 'row':U THEN /** ROW **/
         ASSIGN 
             inRow = inRow + 1
             inCol = 0.
      WHEN 'v':U THEN  /** VALUE **/
          ASSIGN
            chElementValue = ''.
      WHEN 'c':U THEN /** CELL **/
      DO:
          CREATE ttSheetData.
            
          ASSIGN
              inCol = inCol + 1.

         ASSIGN
            ttSheetData.cellRow = inRow
            ttSheetData.cellCol = inCol.

        DO inLoop = 1 TO phAttributes:NUM-ITEMS:
            CASE phAttributes:GET-LOCALNAME-BY-INDEX(inLoop):
                WHEN 'r':U THEN
                    ASSIGN
                    ttSheetData.cellRef = phAttributes:GET-VALUE-BY-INDEX(inLoop).
                WHEN 's':U THEN
                    ASSIGN
                    ttSheetData.cellStyleID = INTEGER(phAttributes:GET-VALUE-BY-INDEX(inLoop)).
                WHEN 't':U THEN
                    ASSIGN
                        ttSheetData.sharedString = phAttributes:GET-VALUE-BY-INDEX(inLoop).
            END CASE.
        END.
    END.
    END CASE.

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

