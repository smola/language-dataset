#!/Users/mickey.winters/prog/lci/lci
HAI 1.4
  CAN HAS MANGO?
  VISIBLE "Content-Type: text/html:)"

  I HAS A CLIENT ITZ MANGO IZ CONNEKTIN YR "mongodb://localhost:27017" MKAY
  I HAS A TODOS ITZ MANGO IZ COLEKTIN YR CLIENT AN YR "test" AN YR "TODOS" MKAY
  
  O HAI IM FILTR
    I HAS A done ITZ FAIL
  KTHX
  I HAS A result ITZ MANGO IZ FINDIN YR TODOS AN YR FILTR MKAY
  VISIBLE "<form method=POST action=:"/cgi-bin/done.lol:">"
  IM IN YR LOOP UPPIN YR N TIL BOTH SAEM result'Z LEN AN N
      I HAS A DOC ITZ result'Z SRS N
      I HAS A TEXT ITZ DOC'Z text

    VISIBLE "<input type=:"radio:" name=:"text:" value=:":{TEXT}:">"
    VISIBLE TEXT
    VISIBLE "</input>"
  IM OUTTA YR LOOP
  VISIBLE "<input type=:"submit:" value=:"DOEN!:">"
  VISIBLE "</form>"
KTHXBYE