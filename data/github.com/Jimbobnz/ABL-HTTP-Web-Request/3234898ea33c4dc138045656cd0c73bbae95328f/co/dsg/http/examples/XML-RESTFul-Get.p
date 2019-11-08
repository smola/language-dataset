USING co.dsg.http.*.

DEFINE VARIABLE objHttp             AS CLASS http         NO-UNDO.
DEFINE VARIABLE objHttpRequest      AS CLASS httpRequest  NO-UNDO.
DEFINE VARIABLE objHttpResponce     AS CLASS httpResponce NO-UNDO.

objHttp        = NEW http().
objHttpRequest = NEW httpRequest().

objHttpRequest:HttpMethod = 'GET'.
/* objHttpRequest:ContenTTYPE = 'application/x-www-form-urlencoded'. */

objHttpRequest:path = '/xml/203.146.212.103'.

objHttpResponce = objHttp:SynchronousRequest( 'http://freegeoip.net', objHttpRequest ). 

MESSAGE
    STRING(objHttpResponce:body)
    view-as alert-box info.

