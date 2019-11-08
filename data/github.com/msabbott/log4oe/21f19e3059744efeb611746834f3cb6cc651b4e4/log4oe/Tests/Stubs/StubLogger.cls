 /*------------------------------------------------------------------------
    File        : StubLogger
    Purpose     : Used in unit tests to check that field values are set and methods are called
    Syntax      :
    Description : Stub for Logger objects
    Author(s)   : Mark Abbott
    Created     : Wed Mar 27 16:42:55 GMT 2019
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING log4oe.Logger.ILogger.
USING log4oe.LoggerConfig.ILoggerConfig.
USING log4oe.LogAppender.ILogAppender.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS log4oe.Tests.Stubs.StubLogger IMPLEMENTS ILogger:

    DEFINE PUBLIC PROPERTY ConfigObject AS ILoggerConfig NO-UNDO
    PUBLIC GET.
    PUBLIC SET.

    DEFINE PUBLIC PROPERTY AppenderObject AS ILogAppender NO-UNDO
    PUBLIC GET.
    PUBLIC SET.
    
    DEFINE PUBLIC PROPERTY LoggerName AS CHARACTER NO-UNDO
    PUBLIC GET.
    PUBLIC SET.

    METHOD PUBLIC VOID DEBUG( INPUT pcMessage AS CHARACTER ):

        UNDO, THROW NEW Progress.Lang.AppError("A METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID DEBUG( INPUT peError AS Progress.Lang.Error ):

        UNDO, THROW NEW Progress.Lang.AppError("B METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID ERROR( INPUT pcMessage AS CHARACTER ):

        UNDO, THROW NEW Progress.Lang.AppError("C METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID ERROR( INPUT peError AS Progress.Lang.Error ):

        UNDO, THROW NEW Progress.Lang.AppError("D METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID FATAL( INPUT pcMessage AS CHARACTER ):

        UNDO, THROW NEW Progress.Lang.AppError("E METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID FATAL( INPUT peError AS Progress.Lang.Error ):

        UNDO, THROW NEW Progress.Lang.AppError("F METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID INFO( INPUT pcMessage AS CHARACTER ):

        UNDO, THROW NEW Progress.Lang.AppError("G METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID INFO( INPUT peError AS Progress.Lang.Error ):

        UNDO, THROW NEW Progress.Lang.AppError("H METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID Log( INPUT piLogLevel AS INTEGER, INPUT pcMessage AS CHARACTER ):

        UNDO, THROW NEW Progress.Lang.AppError("I METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID Log( INPUT piLogLevel AS INTEGER, INPUT peError AS Progress.Lang.Error ):

        UNDO, THROW NEW Progress.Lang.AppError("J METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID TRACE( INPUT pcMessage AS CHARACTER ):

        UNDO, THROW NEW Progress.Lang.AppError("K METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID TRACE( INPUT peError AS Progress.Lang.Error ):

        UNDO, THROW NEW Progress.Lang.AppError("L METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID WARN( INPUT pcMessage AS CHARACTER ):

        UNDO, THROW NEW Progress.Lang.AppError("M METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID WARN( INPUT peError AS Progress.Lang.Error ):

        UNDO, THROW NEW Progress.Lang.AppError("N METHOD NOT IMPLEMENTED").

    END METHOD.

    METHOD PUBLIC VOID addLogAppender( INPUT poAppender AS ILogAppender ):
        AppenderObject = poAppender.
    END METHOD.

    METHOD PUBLIC ILoggerConfig getLoggerConfig():
        RETURN ConfigObject.
    END METHOD.

    METHOD PUBLIC VOID setLoggerConfig( INPUT poLoggerConfig AS ILoggerConfig ):
        ConfigObject = poLoggerConfig.
    END METHOD.
    
    METHOD PUBLIC VOID setLoggerName(INPUT pcLoggerName AS CHARACTER):
        LoggerName = pcLoggerName.
    END METHOD.
    
    METHOD PUBLIC CHARACTER getLoggerName():
        RETURN LoggerName.
    END METHOD.

END CLASS.