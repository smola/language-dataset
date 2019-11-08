{pdf/pdf.i}
 /*------------------------------------------------------------------------
    File        : MarcaDeLectura
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : nomade
    Created     : Tue Sep 04 12:09:23 ART 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS pdf.utiles.MarcaDeLectura IMPLEMENTS pdf.IObjetoPDF:
  DEFINE STATIC PROTECTED PROPERTY cinNumerador AS INTEGER NO-UNDO
    GET:
      cinNumerador = cinNumerador + 1.
      RETURN cinNumerador.
    END GET.
    SET.
    
  DEFINE STATIC PROTECTED TEMP-TABLE cttRelaciones NO-UNDO
    FIELD inPadre AS INTEGER
    FIELD inNumero AS INTEGER
    FIELD obHijo AS Progress.Lang.Object
    INDEX ixRelaciones IS PRIMARY IS UNIQUE inPadre inNumero
  {&END}
  
  DEFINE PROTECTED PROPERTY cinIdentificador AS INTEGER NO-UNDO
    GET.
    PRIVATE SET.
    
  DEFINE PUBLIC PROPERTY cobDocumento AS pdf.Documento NO-UNDO
    GET.
    PRIVATE SET.
     
  DEFINE PUBLIC PROPERTY cchTitulo AS CHARACTER NO-UNDO
    GET.
    SET.
    
  DEFINE PUBLIC PROPERTY clgExpandir AS LOGICAL NO-UNDO
    GET.
    SET.
  
  DEFINE PUBLIC PROPERTY cobPadre AS pdf.utiles.MarcaDeLectura NO-UNDO
    GET.
    SET(INPUT ipobPadre AS pdf.utiles.MarcaDeLectura):
      IF VALID-OBJECT(cobPadre) THEN
        cobPadre:QuitarHijo(THIS-OBJECT).
      cobPadre = ipobPadre.
      IF VALID-OBJECT(cobPadre) THEN
        cobPadre:AgregarHijo(THIS-OBJECT).
    END SET.
    
  DEFINE PUBLIC PROPERTY cinHijos AS INTEGER NO-UNDO
    GET.
    PRIVATE SET.
    
  DEFINE PUBLIC PROPERTY cinHijosVisibles AS INTEGER NO-UNDO
    GET.
    PRIVATE SET.
     
  DEFINE PUBLIC PROPERTY cobPagina AS pdf.Pagina NO-UNDO
    GET.
    PRIVATE SET.
    
  DEFINE PUBLIC PROPERTY cinY AS INTEGER NO-UNDO
    GET.
    SET.
    
  DEFINE PUBLIC PROPERTY cinObjetoPDF AS INTEGER NO-UNDO
    GET.
    PRIVATE SET.
    
  METHOD PUBLIC VOID AgregarHijo(INPUT ipobHijo AS pdf.utiles.MarcaDeLectura):
    DEFINE BUFFER mbfRelaciones FOR cttRelaciones.
    DEFINE VARIABLE minSiguiente AS INTEGER NO-UNDO.
    
    IF ipobHijo:cobPadre NE THIS-OBJECT THEN
      RETURN.
    cinHijos = cinHijos + 1.
    IF clgExpandir THEN
      cinHijos = cinHijosVisibles + 1.
    FOR EACH mbfRelaciones WHERE mbfRelaciones.inPadre EQ cinIdentificador BY mbfRelaciones.inNumero DESC:
      minSiguiente = mbfRelaciones.inNumero + 1.
      LEAVE.
    END.
    
    CREATE mbfRelaciones.
    ASSIGN
      mbfRelaciones.inPadre = cinIdentificador
      mbfRelaciones.obHijo = ipobHijo
      mbfRelaciones.inNumero = minSiguiente
    {&END}
  END METHOD.
  METHOD PUBLIC VOID QuitarHijo(INPUT ipobHijo AS pdf.utiles.MarcaDeLectura):
    DEFINE BUFFER mbfRelaciones FOR cttRelaciones.
    
    IF ipobHijo:cobPadre NE THIS-OBJECT THEN
      RETURN.
    IF clgExpandir THEN
      cinHijosVisibles = cinHijosVisibles - 1.
    cinHijos = cinHijos - 1.
    FOR EACH mbfRelaciones WHERE mbfRelaciones.inPadre EQ cinIdentificador:
      IF mbfRelaciones.obHijo EQ ipobHijo THEN DO:
        DELETE mbfRelaciones.
        LEAVE.
      END.
    END.
  END METHOD.

  CONSTRUCTOR MarcaDeLectura(INPUT ipobDocumento AS pdf.Documento, INPUT ipchTitulo AS CHARACTER):
    DEFINE BUFFER mbfRelaciones FOR cttRelaciones.
    
    cobDocumento = ipobDocumento.    
    cobPagina = cobDocumento:cobPaginaActual.
    cinY = cobPagina:cobEstadoTexto:cobPosicion:cinY + cobDocumento:cobLetraActual:cdePuntos.
    cchTitulo = ipchTitulo.
    cinIdentificador = pdf.utiles.MarcaDeLectura:cinNumerador.
    CREATE mbfRelaciones.
    ASSIGN
      mbfRelaciones.inPadre = cinIdentificador
      mbfRelaciones.inNumero = 0
    {&END} 
    cobDocumento:agregarMarcaDeLectura(THIS-OBJECT).
  END CONSTRUCTOR.
  CONSTRUCTOR MarcaDeLectura(INPUT ipobDocumento AS pdf.Documento, INPUT ipobMarca AS pdf.utiles.MarcaDeLectura):
    DEFINE BUFFER mbfRelaciones FOR cttRelaciones.
    
    cobDocumento = ipobDocumento.    
    cobPagina = cobDocumento:cobPaginaActual.
    cinY = ipobMarca:cinY.
    cchTitulo = ipobMarca:cchTitulo.
    cinIdentificador = pdf.utiles.MarcaDeLectura:cinNumerador.
    CREATE mbfRelaciones.
    ASSIGN
      mbfRelaciones.inPadre = cinIdentificador
      mbfRelaciones.inNumero = 0
    {&END}
    IF VALID-OBJECT(ipobMarca:cobPadre) THEN
      cobPadre = cobDocumento:MarcaDeLecturaRegistrada(ipobMarca:cobPadre:cchTitulo).
    cobDocumento:agregarMarcaDeLectura(THIS-OBJECT).
  END CONSTRUCTOR.
  
  DESTRUCTOR MarcaDeLectura():
    DEFINE BUFFER mbfRelaciones FOR cttRelaciones.
    
    FOR EACH mbfRelaciones WHERE mbfRelaciones.inPadre EQ cinIdentificador:
      DELETE mbfRelaciones.
    END.
    IF NOT TEMP-TABLE cttRelaciones:HAS-RECORDS THEN
      pdf.utiles.MarcaDeLectura:cinNumerador = 0.    
  END DESTRUCTOR.
  
  METHOD PUBLIC LOGICAL asignarObjetoPDF():
    IF cinObjetoPDF GT 0 THEN
      RETURN FALSE.
    cinObjetoPDF = cobDocumento:crearObjeto('BookMark').
    RETURN TRUE.
  END METHOD.
  
  METHOD PUBLIC VOID generarRama(INPUT ipinObjetoPadre AS INTEGER, INPUT ipinPrevio AS INTEGER, INPUT ipinSiguiente AS INTEGER):
    DEFINE BUFFER mbfRelaciones FOR cttRelaciones.
    DEFINE BUFFER mbfSiguiente FOR cttRelaciones.
    DEFINE VARIABLE mchPDF AS CHARACTER NO-UNDO.
    DEFINE VARIABLE minPrimerHijo AS INTEGER NO-UNDO.
    DEFINE VARIABLE minUltimoHijo AS INTEGER NO-UNDO.
    
    asignarObjetoPDF().
    cobDocumento:inicioObjeto(THIS-OBJECT,cinObjetoPDF).        
    mchPDF = "<<" + {&pdfSkip}.
    IF VALID-OBJECT(cobDocumento:cobEncriptador) THEN
      mchPDF = mchPDF + "/Title <" + cobDocumento:cobEncriptador:EncriptarAHex(cinObjetoPDF,cchTitulo) + ">" + {&pdfSkip}.
    ELSE 
      mchPDF = mchPDF + "/Title (" + cobDocumento:escaparTexto(cchTitulo) + ")" + {&pdfSkip}.
  
    mchPDF = mchPDF + "/Parent " + STRING(ipinObjetoPadre) + " 0 R" + {&pdfSkip}.
    IF ipinPrevio GT 0 THEN
      mchPDF = mchPDF + "/Prev " + STRING(ipinPrevio) + " 0 R" + {&pdfSkip}.
    IF ipinSiguiente GT 0 THEN
      mchPDF = mchPDf + "~/Next " + STRING(ipinSiguiente) + " 0 R" + {&pdfSkip}.

    /* If Children are associated with this Bookmark then add some processing */
    IF cinHijos GT 0 THEN DO:
      FOR EACH mbfRelaciones WHERE mbfRelaciones.inPadre EQ cinIdentificador AND mbfRelaciones.inNumero GT 0 BY mbfRelaciones.inNumero:
        CAST(mbfRelaciones.obHijo,pdf.utiles.MarcaDeLectura):asignarObjetoPDF().
      END.
      FOR EACH mbfRelaciones WHERE mbfRelaciones.inPadre EQ cinIdentificador AND mbfRelaciones.inNumero GT 0 BY mbfRelaciones.inNumero:
        minPrimerHijo = CAST(mbfRelaciones.obHijo,pdf.utiles.MarcaDeLectura):cinObjetoPDF.
        LEAVE.
      END.
      FOR EACH mbfRelaciones WHERE mbfRelaciones.inPadre EQ cinIdentificador AND mbfRelaciones.inNumero GT 0 BY mbfRelaciones.inNumero DESC:
        minUltimoHijo = CAST(mbfRelaciones.obHijo,pdf.utiles.MarcaDeLectura):cinObjetoPDF.
        LEAVE.
      END.
      mchPDF = mchPDF + "~/First " + STRING(minPrimerHijo) + " 0 R" + {&pdfSkip}
            + "~/Last " + STRING(minUltimoHijo) + " 0 R" + {&pdfSkip}
            + "~/Count " + STRING(cinHijos) + {&PDFSkip}.
    END.
    cobDocumento:cobDestino:agregar(mchPDF + "/Dest [ " + STRING(cobPagina:cinObjetoPDF) + " 0 R /XYZ 0 " + STRING(cinY) + " 0 ]" + {&PDFSkip}
          + ">>" + {&PDFskip} + "endobj" + {&PDFSkip}).
    IF cinHijos LE 0 THEN
      RETURN.
    minPrimerHijo = 0.
    FOR EACH mbfRelaciones WHERE mbfRelaciones.inPadre EQ cinIdentificador AND mbfRelaciones.inNumero GT 0 BY mbfRelaciones.inNumero:
      FIND mbfSiguiente WHERE mbfSiguiente.inPadre EQ cinIdentificador AND mbfSiguiente.inNumero EQ (mbfRelaciones.inNumero + 1) NO-ERROR.
      IF AVAILABLE mbfSiguiente THEN
        CAST(mbfRelaciones.obHijo,pdf.utiles.MarcaDeLectura):generarRama(cinObjetoPDF,minPrimerHijo,CAST(mbfSiguiente.obHijo,pdf.utiles.MarcaDeLectura):cinObjetoPDF).
      ELSE
        CAST(mbfRelaciones.obHijo,pdf.utiles.MarcaDeLectura):generarRama(cinObjetoPDF,minPrimerHijo,0).
      minPrimerHijo = CAST(mbfRelaciones.obHijo,pdf.utiles.MarcaDeLectura):cinObjetoPDF.
    END.
  END METHOD.
  
  METHOD PUBLIC VOID generarCatalogo():
  END METHOD.
END CLASS.