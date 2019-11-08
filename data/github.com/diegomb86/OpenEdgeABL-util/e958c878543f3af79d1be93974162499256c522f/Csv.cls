CLASS util.Csv:
    /* CSV file generate from any table on database*/

    DEFINE PRIVATE PROPERTY file AS CHARACTER NO-UNDO
        GET.
        PRIVATE SET.
    
    DEFINE PRIVATE PROPERTY separator AS CHARACTER NO-UNDO
        GET.
        PRIVATE SET.

    DEFINE PRIVATE PROPERTY hQuery AS HANDLE NO-UNDO
        PRIVATE GET.
        PRIVATE SET.

    DEFINE PRIVATE PROPERTY directory AS CHARACTER NO-UNDO
        GET.
        PRIVATE SET.

    DEFINE PRIVATE PROPERTY cQuery AS CHARACTER NO-UNDO
        GET.
        PRIVATE SET.

    CONSTRUCTOR PUBLIC Csv ():
    END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC Csv (INPUT file AS CHARACTER):
        setFile(file).
    END CONSTRUCTOR.

    METHOD PUBLIC VOID setDirectory (INPUT directory AS CHARACTER):
        THIS-OBJECT:directory = REPLACE(directory,'~\','~/').
        IF SUBSTRING(THIS-OBJECT:directory,LENGTH(THIS-OBJECT:directory),1) <> '~/' THEN
            THIS-OBJECT:directory = THIS-OBJECT:directory + '~/'.
    END METHOD.

    METHOD PUBLIC VOID setFile (INPUT file AS CHARACTER):
        THIS-OBJECT:file = file.
    END METHOD.

    METHOD PUBLIC CHARACTER getFile ():
        RETURN THIS-OBJECT:file.
    END METHOD.
    
    METHOD PUBLIC VOID setSeparator (INPUT separator AS CHARACTER):
        THIS-OBJECT:separator = separator.
    END METHOD.
    
    METHOD PUBLIC VOID setQuery (pQuery AS CHARACTER):
        THIS-OBJECT:cQuery = pQuery.
    END METHOD.

    /* Pass a table handle in the generate method, optionally you can pass a query to delimiter data.
       Eg. generate(BUFFER customer:HANDLE,"WHERE country = 'Austria'"). */
    METHOD PUBLIC CHARACTER generate (INPUT hTable AS HANDLE,pQuery AS CHARACTER):
        setQuery(pQuery). 
        generate(hTable).
    END METHOD.

    METHOD PUBLIC CHARACTER generate (INPUT hTable AS HANDLE):
        DEFINE VARIABLE iField  AS INTEGER     NO-UNDO.
        DEFINE VARIABLE cText   AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE file AS CHARACTER   NO-UNDO.
        
        IF THIS-OBJECT:directory = "" THEN
            setDirectory(SESSION:TEMP-DIRECTORY).

        IF THIS-OBJECT:file = "" THEN
            setFile(THIS-OBJECT:directory + hTable:NAME + '.csv').

        file = THIS-OBJECT:file.
        IF THIS-OBJECT:separator = "" THEN
            setSeparator(';').

        OUTPUT TO VALUE(file) NO-CONVERT.
            /* Cabecalho */
            DO iField = 1 TO hTable:NUM-FIELDS:
                PUT UNFORMATTED hTable:BUFFER-FIELD(iField):NAME THIS-OBJECT:separator. 
            END.
            PUT UNFORMATTED SKIP.

            /* Content */
            CREATE QUERY hQuery.

            hQuery:SET-BUFFERS(hTable).
            hQuery:QUERY-PREPARE("for each " + hTable:NAME + ' ' + cQuery).
            hQuery:QUERY-OPEN().

            REPEAT:
                hQuery:GET-NEXT().
                IF hQuery:QUERY-OFF-END THEN 
                    LEAVE.
                DO iField = 1 TO hTable:NUM-FIELDS:

                    IF hTable:BUFFER-FIELD(iField):DATA-TYPE = "CHARACTER" THEN
                        cText = removeBreakLine(hTable:BUFFER-FIELD(iField):BUFFER-VALUE).
                    ELSE 
                        cText = STRING(hTable:BUFFER-FIELD(iField):BUFFER-VALUE).

                    /* Corrige para que nao exista separator no meio do cText. */
                    IF THIS-OBJECT:separator <> ',' THEN
                        cText = REPLACE(cText,THIS-OBJECT:separator,',').
                    ELSE
                        cText = REPLACE(cText,THIS-OBJECT:separator,'.').

                    PUT UNFORMATTED cText THIS-OBJECT:separator. 

                END.
                PUT UNFORMATTED SKIP.
            END.

            hQuery:QUERY-CLOSE().
            hTable:BUFFER-RELEASE().

        OUTPUT CLOSE.

        DELETE OBJECT hQuery.
        DELETE OBJECT hTable.
        setFile("").
        RETURN file.

    END METHOD.

    METHOD PUBLIC CHARACTER removeBreakLine (INPUT cText AS CHARACTER):
        RETURN TRIM(REPLACE(REPLACE(REPLACE(cText,CHR(10)," "),CHR(13)," "),CHR(9)," ")).
    END METHOD.


END CLASS.
