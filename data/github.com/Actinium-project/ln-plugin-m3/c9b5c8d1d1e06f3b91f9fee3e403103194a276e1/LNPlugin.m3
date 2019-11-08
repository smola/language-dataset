MODULE LNPlugin EXPORTS Main;

IMPORT IO, OSError;
IMPORT Stdio, Thread;
IMPORT Rd, Wr, Text;
IMPORT TextRd, FileWr;

(* 
    Opens stdin and stdout for reading/writing 
    and waits for the initial message from lightningd
    No parsing of JSON data currently available. 
*)
PROCEDURE Run() RAISES {Wr.Failure} =
    VAR
        rd       : Rd.T;
        wr       : Wr.T;
        logfile  : Wr.T;
        buf      : ARRAY [0..83] OF CHAR;
        line     : TEXT := "";
        nextline : TEXT;
        substr   : TEXT;
        len      : INTEGER;
        found1   : INTEGER;
        found2   : INTEGER;
        isInit   : BOOLEAN := FALSE;
    BEGIN
        rd := Stdio.stdin;
        wr := Stdio.stdout;
        logfile := FileWr.Open("plugin.log");
        WHILE TRUE DO
            TRY 
                WHILE NOT Rd.EOF(rd) DO
                    len := Rd.GetSub(rd, buf);
                    nextline := Text.FromChars(buf);
                    Wr.PutText(logfile, nextline);
                    IF NOT Text.Empty(nextline) THEN
                        line := Text.Cat(line, nextline & "\n");
                        found1 := Text.FindChar(line, '}', 0);
                        IF found1 > 0 THEN
                            found2 := Text.FindChar(line, '}', found1+1);
                            IF (found2 - found1) > 1 THEN
                                IF isInit THEN
                                    Wr.PutText(logfile, line);
                                    Wr.Flush(logfile);
                                    Wr.Flush(wr);
                                ELSE
                                    isInit := TRUE;
                                    Wr.PutText(wr, "{" &
                                    "\"jsonrpc\": \"2.0\"," &
                                    "\"id\": 1," &
                                    "\"result\": {" &
                                        "\"options\": []," &
                                        "\"rpcmethods\": []," &
                                        "\"subscriptions\": [\"connect\",\"disconnect\"] " &
                                    "}}");
                                    Wr.Flush(wr);
                                    Wr.PutText(logfile, "Initialized\n");
                                    Wr.Flush(logfile);
                                END;
                            END;
                        END;
                    END;
                Wr.Flush(logfile);
                END;
            EXCEPT Rd.Failure => Wr.Close(logfile);
            END;
        END;
        Wr.Flush(logfile);
        Wr.Close(logfile);
        Wr.Close(wr);
END Run;

BEGIN
  Run();
END LNPlugin.