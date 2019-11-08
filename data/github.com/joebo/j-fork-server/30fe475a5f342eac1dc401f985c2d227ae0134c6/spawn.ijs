load'socket'
load 'files'

WSASocket=: 'ws2_32 WSASocketW i i i i *c i i' & cd
Sleep=:'kernel32 Sleep n i' & cd
sendJ=: 'ws2_32 send i i *c i i'
closeJ=: 'ws2_32 closesocket i i' & cd

binarr  =: 3!:2
Z =: fread 'shared.txt'
ferase 'shared.txt'

sockh =: WSASocket _1;_1;_1;(binarr Z);0;0
smoutput sockh
sock =: > 0}sockh 

smoutput sock

NB. Sleep 50

body=:'<h1>hi</h1>'

T=:'HTTP/1.0 200 OK',CRLF
sent=:sendJ cd sock;T;(#T);0
smoutput sent

T=:'Connection: close',CRLF
sent=:sendJ cd sock;T;(#T);0

T=:'Content-Length: ',(": (# body)), CRLF
sent=:sendJ cd sock;T;(#T);0
smoutput sent

T=:CRLF,body
sent=:sendJ cd sock;T;(#T);0


T=:CRLF
sent=:sendJ cd sock;T;(#T);0

messageLoop =: 3 : 0
msg =. ''
while. msg -.@:-: 'exit' do.
sendJ cd sock;T;(#T);0
Sleep 500
end.
)


smoutput closeJ sock


exit''

NB. messageLoop ''
