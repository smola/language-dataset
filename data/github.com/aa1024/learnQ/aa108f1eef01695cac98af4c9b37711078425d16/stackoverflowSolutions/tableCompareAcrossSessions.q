/Remote service - port 2001
q)table1:([]col1:0n 2 3 0n ;col2:"a", " ","ca")
q)table2:([]col3:1 2 0n 4 ;col4:```b`)

/-------------------------------------------------------------------

/local
q)table1:([]col1:0n 2 3 0n ;col2:"a", " ","ca")
q)table2:([]col3:1 2 0n 4 ;col4:```b`d)   //different from remote
q)table3:([]col5:1 2 0n 4 ;col6:```b`d)    //local table

q)f: { tables[]!{[t] (), {sum null x} each flip 0!value t}each tables[]}  //tables unkeyed

q)remote:`::2001(f;::)
q)remote 
(`s#`table1`table2)!((`col1`col2)!2 1;(`col3`col4)!1 3)

q)local:f[]
q)local
(`s#`table1`table2`table3)!((`col1`col2)!2 1;(`col3`col4)!1 2;(`col5`col6)!1 2)

q)remote~'local   //only table1 signature matches
(`table1`table2`table3)!100b
