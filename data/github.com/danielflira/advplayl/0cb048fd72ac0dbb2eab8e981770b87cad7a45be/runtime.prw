#Include "Protheus.ch"

#Define USERFUNCTION

//Define de nome da variavel global criada para a exclusao dos RPO's
#Define C_CLEANING 'AdvPlayL_Clear'

//Chave criada para ter certeza que o RPO foi gerado pelo AdvPlayL
#Define C_ADVPLAYL 'AdvPlayL'


//-------------------------------------------------------------------
/*/{Protheus.doc} RPO2
Classe RPO2, responsavel pela compilacao de fontes em ADVPL

@author Daniel Lira
/*/
//-------------------------------------------------------------------
Class RPO2
    Data oRPO
    Data cRPO
    Data lAberto
    Data cErrStr
    Data nErrLine
    Data cPathRPO
    Data aPathInclude

    Method New() Constructor
    Method Open()
    Method Close()
    Method Reload()
    Method Compile(cFile, cSource)
EndClass


//-------------------------------------------------------------------
/*/{Protheus.doc} New
Metodo de instancia da classe

@author Daniel Lira
/*/
//-------------------------------------------------------------------
Method New(cRPO) Class RPO2
    Self:cRPO     := cRPO
    Self:lAberto  := .F.
    Self:nErrLine := 0
    Self:cErrStr  := ""
    Self:oRPO     := Nil

    // Utilizo a Rootpath para salvar o RPO, pois quando nao existe
    // Smartclient, eu encontro por padrao a Roopath ao trabahar com /
    Self:cPathRPO     := AllTrim( GetSrvProfString( 'ROOTPATH' , '' ) )    
    Self:aPathInclude := StrTokArr( AllTrim( GetSrvProfString( 'DIRINCLUDE' , '' ) ) , ';' )

    //Valida o path
    If !Empty( Self:cPathRPO )
        If Right( Self:cPathRPO , 1 ) != Iif( IsSrvUnix() , '/' , '\' )
            Self:cPathRPO += Iif( IsSrvUnix() , '/' , '\' )
        EndIf
    EndIf
Return Self


//-------------------------------------------------------------------
/*/{Protheus.doc} Open
Metodo de abertura do RPO

@author Daniel Lira
/*/
//-------------------------------------------------------------------
Method Open() Class RPO2
    Local lRet := .T.
    If ! Self:lAberto
        Self:oRPO := RPO():New()

        //Definicao dos includes
        Self:oRPO:Includes   := Self:aPathInclude
        //Define padrao para ambiente TOP, esse include ja adiciona o Protheus.ch
        Self:oRPO:MainHeader := 'PRTOPDEF.CH'

        If Self:oRPO:Open( Self:cPathRPO + Self:cRPO )
            Self:lAberto := .T.
            lRet := .T.
        Else
            lRet := .F.
        EndIf
    EndIf
Return lRet


//-------------------------------------------------------------------
/*/{Protheus.doc} Close
Metodo responsavel por fechar o RPO apos o uso

@author Daniel Lira
/*/
//-------------------------------------------------------------------
Method Close() Class RPO2
    Local lRet := .T.

    If Self:lAberto
        If Self:oRPO:Close()
            Self:lAberto := .F.
            lRet := .T.
        Else
            lRet := .F.
        EndIf
    EndIf
Return lRet


//-------------------------------------------------------------------
/*/{Protheus.doc} Reload
Metodo que efetua um refresh no RPO

@author Daniel Lira
/*/
//-------------------------------------------------------------------
Method Reload() Class RPO2
Return Self:Close() .And. Self:Open()


//-------------------------------------------------------------------
/*/{Protheus.doc} Compile
Metodo responsavel pela compilacao no RPO

@author Daniel Lira
/*/
//-------------------------------------------------------------------
Method Compile(cFile, cSource) Class RPO2
    Local cPreC := ''
    Local aDeps := {}
    Local nDate := Date() - SToD("19991231")

    If ! Self:Open()
        Return .F.
    EndIf

    If ! Self:oRPO:StartBuild( .T. )
        Self:Reload()
        Return .F.
    EndIf

    // A pre-compilacao nao e obrigatoria, porem trata diversas
    // questoes no fonte, XCOMMAND, XTRANSLATE...
    If Self:oRPO:PreComp( cFile , cSource , @cPreC , @aDeps )
        cSource := cPreC
    Else
        Self:Reload()

        If ! Self:oRPO:StartBuild( .T. )
            Return .F.
        EndIf
    EndIf

    If ! Self:oRPO:Compile( cFile , cSource , nDate , Self:oRPO:ChkSum( cSource ) )
        Self:cErrStr  := Self:oRPO:ErrStr
        Self:nErrLine := Self:oRPO:ErrLine
        Self:Reload()
        Return .F.
    EndIf

    If ! Self:oRPO:EndBuild()
        Self:Reload()
        Return .F.
    EndIf
Return Self:Reload()


//-------------------------------------------------------------------
/*/{Protheus.doc} Stdout
Funcao que permite a impressao de valores no AdvPlayl

@author Daniel Lira
/*/
//-------------------------------------------------------------------
#IfDef USERFUNCTION
User Function Stdout(cMessage)
#Else
Function Stdout(cMessage)
#EndIf
    cOutput += AsString(cMessage)
Return Nil


//-------------------------------------------------------------------
/*/{Protheus.doc} StdoutLn
Funcao que permite a impressao de valores
no AdvPlayl com quebra de linha

@author Daniel Lira
/*/
//-------------------------------------------------------------------
#IfDef USERFUNCTION
User Function StdoutLn(cMessage)
#Else
Function StdoutLn(cMessage)
#EndIf
    cOutput += AsString(cMessage) + Chr(10)
Return Nil


//-------------------------------------------------------------------
/*/{Protheus.doc} Stdin
Funcao que permite a entrada de dados no AdvPlayl

@author Daniel Lira
/*/
//-------------------------------------------------------------------
#IfDef USERFUNCTION
User Function Stdin()
#Else
Function Stdin()
#EndIf
    Local cInput := ""

    If nInput <= Len(aInput)
        cInput := aInput[nInput++]
    EndIf
Return cInput


//-------------------------------------------------------------------
/*/{Protheus.doc} SaveFile
Funcao responsavel por salvar os fontes

@author Daniel Lira
/*/
//-------------------------------------------------------------------
User Function SaveFile( cFilename , cContent )
    Local cFile := "./fontes"
    Local nFile := 0

    // cria diretorio (nao importa se nao funcionar)
    MakeDir( cFile )
    cFile += "/" + cFilename

    // salva o arquivo no disco (nao precisa conferir)
    nFile := FCreate( cFile )
    FWrite( nFile , cContent )
    FClose( nFile )
Return Nil


//-------------------------------------------------------------------
/*/{Protheus.doc} LoadFile
Funcao responsavel por ler um fonte ja existente

@author Daniel Lira
/*/
//-------------------------------------------------------------------
User Function LoadFile(__aProcParms)
    Local cContent := ""
    Local nI       := 0
    Local cName    := ""
    Local cValue   := ""
    Local cFile    := "./fontes"

    // pegando parametros do get
    For nI := 1 To  Len(__aProcParms)
        cName := Lower(__aProcParms[nI][1])
        cValue := __aProcParms[nI][2]

        If cName == "arquivo"
            cFile += "/" + cValue
        EndIf
    Next nI

    // lendo arquivo default
    If cFile == "./fontes"
        cFile += "/default.prw"
    EndIf

    // lendo conteudo do arquivo
    nFile := FOpen(cFile)
    If nFile > -1
        FRead(nFile, @cContent, 8192)
        FClose(nFile)
    EndIf
Return cContent


//-------------------------------------------------------------------
/*/{Protheus.doc} Runtime
Funcao que compila e executa o codigo ADVPL

@author Daniel Lira
/*/
//-------------------------------------------------------------------
User Function Runtime(__aCookies, __aPostParms, __nProcID, __aProcParms, __cHTTPPage)
    Local nI      := 0
    Local cName   := ""
    Local cValue  := ""
    // variaveis
    Local cUUID   := ""
    Local cCodigo := ""
    Local cEntry  := ""
    // compilacao
    Local oRPO2   := Nil
    // saida
    Private cOutput := ""
    Private aInput  := {}
    Private nInput  := 1

    // preparando parametros
    For nI := 1 To Len(__aPostParms)

        cName := Lower(__aPostParms[nI][1])
        cValue := __aPostParms[nI][2]

        If cName == "uuidv4"
            cUUID := cValue

        ElseIf cName == "codigo"
            cCodigo := cValue

        ElseIf cName == "entry"
            cEntry := cValue

        ElseIf cName == "stdin"
            // altera CR para LF
            cValue := StrTran(cValue, Chr(13), Chr(10))
            // altera LFLF para LF
            cValue := StrTran(cValue, Chr(10)+Chr(10), Chr(10))
            // quebra valores por LF
            aInput := StrTokArr2(cValue, Chr(10), .T.)

        ElseIf cName == "arquivo"
            cArquivo := cValue

        EndIf
    Next nI

    // salva codigo antes de executar
    If !Empty(cArquivo)
        U_SaveFile(cArquivo, cCodigo)
    EndIf

    // inicializa
    oRPO2 := RPO2():New( cUUID + C_ADVPLAYL + ".rpo" )

    // se compilou executa
    If oRPO2:Compile( cUUID + C_ADVPLAYL + ".prw" , cCodigo )
        ErrorBlock({|oError| cOutput := oError:ErrorStack})
        Begin Sequence
        &(cEntry)
        End Sequence

    // se nao compilou mostra erro
    Else
        cOutput := oRPO2:cErrStr
    EndIf

    // Job que faz a exclusao dos RPOs gerados
    StartJob( 'U_AdvPlaylClear' , GetEnvServer() , .F. )
    Sleep( 500 )

    oRPO2:Close()
Return cOutput


//-------------------------------------------------------------------
/*/{Protheus.doc} ClearRPO
JOB de exclusao de RPOs

@author Daniel Mendes
/*/
//-------------------------------------------------------------------
User Function AdvPlaylClear()
Local cPath := ''
Local cKey  := ''
Local aRPOs := Nil
Local nLoop := 0

cKey := GetGlbValue( C_CLEANING )

If Empty( cKey ) .Or. Seconds() >= Val( cKey ) + 900 //15 minutos
    ClearGlbValue( C_CLEANING )
    PutGlbValue( C_CLEANING , cValToChar( Seconds() ) )

    //GetUserInfoArray() //Estudar essa funcao

    cPath := AllTrim( GetSrvProfString( 'SOURCEPATH' , '' ) )

    // So efetuo a limpeza caso eu encontre a chave, caso contrario 
    // o RPO e gerado em outro lugar =P
    If !Empty( cPath )
        cPath := Iif( IsSrvUnix() , '/' , '\' )
        aRPOs := Directory( cPath + '*' + C_ADVPLAYL + '.rpo' , 'H' , Nil , .F. , 1 )

        If !Empty( aRPOs )
            For nLoop := 1 To Len( aRPOs )
                //Nao excluo os RPOs padroes do sistema...
                FErase( cPath + aRPOs[ nLoop , 1 ] )
            Next nLoop
        EndIf
    EndIf
EndIf

Return Nil