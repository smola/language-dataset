//ref:https://www.bitmex.com/app/apiOverview

//settings: apiHost,apiKey,apiSecret

settings:`apiHost`apiKey`apiSecret!("testnet.bitmex.com";"";"")   //testnet

///0.common functions

//hmacsha256: by https://github.com/ogay/hmac    
.zz.dl:@[{(`:qx 2:(`loadlibrary;1))[]};`;(enlist`)!enlist(::)];
hmacsha256:{[k;m]if[11h<>type (k;m);:`error_type];:.zz.dl.hmacsha256[(k;m;::)];};

//url-encoding on querystring, only value is URL-encoded: urlencode "/a?a=1&b=1"
urlencode:{$[x like "*[?]*";$[x like "*=*";"&" sv{$[2=count x; "="sv(x 0;ssr[.h.hu x 1;"%??";upper]);x]}each "=" vs/: "&" vs x;x];x]}; 
//qtime2unix .z.Z
qtime2unix:{`long$8.64e4*10957+x};
//signature["chNOOS4KvNXR_Xq4k4c9qsfoKWvnDecLATCRlcBwyKDYnWgO";"GET";"/api/v1/instrument";qtime2unix 2018.02.08T04:30:36;""]   / c7682d435d0cfe87c16098df34ef2eb5a549d4c5a3c2b1f0f77b8af73423bf00
signature:{[secret;verb;path;nonce;data]message:`$verb,path,string[nonce],data;:string hmacsha256[`$secret;message];};   

//1.REST API (https://www.bitmex.com/app/restAPI)
/ Not authenticating when apiKey or apiSecret = ""
restapi:{[host;verb;path;data;apiKey;apiSecret]if[not(6#10h)~type each(host;verb;path;data;apiKey;apiSecret);:`status`header`body!(-1;`;`)];urlpath:urlencode path;expires:qtime2unix .z.Z+00:00:10.000;
    httpresp::(`$":https://",host) httpreq::upper[verb]," ",urlpath," HTTP/1.1\r\nHost: ",host
        ,$[(apiKey~"")|apiSecret~"";"";"\r\napi-expires: ",string[expires],"\r\napi-key: ",apiKey,"\r\napi-signature: ",signature[apiSecret;verb;urlpath;expires;data]]
        ,$[data~"";"";"\r\nContent-Length: ",string[count data],"\r\nContent-Type: application/json"],"\r\n\r\n",data;
    i:first httpresp ss "\r\n\r\n"; headers:i#httpresp;body:(i+4) _ httpresp; status:("J"$" " vs first["\r\n" vs headers])[1]; 
    header:raze{{enlist[`$x 0]!enlist x[1]}": " vs x}each "\r\n" vs "statusline: ",headers; body:.j.k (i+4) _ httpresp;
    :`status`header`body!(status;header;body);};

// examples: the following uses settings dict.
getNextClOrdID:{$[not `myClOrdID in key `.;myClOrdID::0;[myClOrdID+:1;myClOrdID]]};
//buy             // r:b x:`XBTUSD,1,  11111f
b:buy:ol:cs:{"b `sym,qty[,price]";if[0h<>type x;:`status`header`body!(-1;`;`)];o:`clOrdID`symbol`side`orderQty!(getNextClOrdID[];x 0;`Buy; x 1);if[3=count x;o:o,enlist[`price]!enlist[x 2]];r:restapi[settings`apiHost;"POST";"/api/v1/order";.j.j[o];settings`apiKey;settings`apiSecret];:r};  
//sell           // r:s x:`XBTUSD,1,  11111f
s:sell:os:cl:{"s `sym,qty[,price]";if[0h<>type x;:`status`header`body!(-1;`;`)];o:`clOrdID`symbol`side`orderQty!(getNextClOrdID[];x 0;`Sell;x 1);if[3=count x;o:o,enlist[`price]!enlist[x 2]];r:restapi[settings`apiHost;"POST";"/api/v1/order";.j.j[o];settings`apiKey;settings`apiSecret];:r};  
//cancel        // r: cc 5
c:cancel:{"c clOrdID(j)";if[-7h<>type x;:`status`header`body!(-1;`;`)];r:restapi[settings`apiHost;"DELETE";"/api/v1/order";.j.j[enlist[`clOrdID]!enlist[string x]];settings`apiKey;settings`apiSecret];:r};  
//get orders // getord[]
getord:{r:restapi[settings`apiHost;"GET";"/api/v1/order?reverse=true";"";settings`apiKey;settings`apiSecret];:update `$symbol,`$side,`$ordStatus,ltime"Z"$transactTime,ltime"Z"$timestamp from r`body;};
od:{r:restapi[settings`apiHost;"GET";"/api/v1/order?reverse=true&columns=",.j.j[`clOrdID`symbol`side`orderQty`price`ordStatus`cumQty`avgPx`timestamp];"";settings`apiKey;settings`apiSecret];:select "J"$clOrdID,ltime"Z"$timestamp,`$symbol,`$side,orderQty,price,`$ordStatus,cumQty,avgPx from r`body;}; 
//get positions // getpos[]
getpos:{r:restapi[settings`apiHost;"GET";"/api/v1/position";"";settings`apiKey;settings`apiSecret];:update `$symbol,ltime"Z"$timestamp from r`body;};   
ps:{r:restapi[settings`apiHost;"GET";"/api/v1/position?columns=",.j.j[`symbol`currentQty`avgCostPrice`marginCallPrice`maintMargin`lastPrice`timestamp];"";settings`apiKey;settings`apiSecret];:select ltime"Z"$timestamp,`$symbol,`$currency,currentQty,simpleQty,avgCostPrice,marginCallPrice,markPrice,liquidationPrice,maintMargin,lastPrice from r`body;};
//get walletsummary  // getwalletsummary[]
getwalletsummary:{r:restapi[settings`apiHost;"GET";"/api/v1/user/walletSummary";"";settings`apiKey;settings`apiSecret];:update `$currency,`$symbol,`$transactType from r`body;};   
pr:{r:restapi[settings`apiHost;"GET";"/api/v1/user/walletSummary";"";settings`apiKey;settings`apiSecret];:update `$currency,`$symbol,`$transactType from r`body;};

/
misc examples:
r:restapi[settings`apiHost;"GET";"/api/v1/position";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/apiKey?reverse=false";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/chat?count=100&reverse=true";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/announcement";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/announcement/urgent";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/execution";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/execution/tradeHistory";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/funding";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/instrument";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/order";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/orderBook/L2?symbol=XBTUSD";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/quote?symbol=XBTUSD&reverse=true";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/schema";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/schema/websocketHelp";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/stats";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/trade?symbol=XBTUSD&count=5&start=0&startTime=2018-03-01 00:20:00";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/user";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/user/wallet";"";settings`apiKey;settings`apiSecret];r`body
r:restapi[settings`apiHost;"GET";"/api/v1/user/walletSummary";"";settings`apiKey;settings`apiSecret];r`body
\

//2.WebSocket API (https://www.bitmex.com/app/wsAPI)
/wsapi: connect to websocket: Not authenticating when apiKey or apiSecret = ""
wsapi:{[host;apiKey;apiSecret] :(`$":wss://",host) "GET /realtime HTTP/1.1\r\nHost: ",host,
            $[(apiKey~"")|apiSecret~"";"";"\r\napi-nonce: ",string[nonce],"\r\napi-key: ",apiKey,"\r\napi-signature: ",signature[apiSecret;"GET";"/realtime";nonce:qtime2unix .z.Z;""]],"\r\n\r\n"};
/wsapi_cmd: wshandle: the first element returned from wsapi[...], command: a dict for command args ex.`op`args!(`subscribe;enlist`$"trade:XBTUSD")
wsapi_cmd:{[wshandle;command]neg[wshandle] .j.j command};

/subscribe:   r:wsapi[settings`apiHost; settings`apiKey; settings`apiSecret];  wsapi_sub[first[r];"trade:XBTUSD"]
wsapi_sub:{[wshandle;sub_args]wsapi_cmd[wshandle;`op`args!(`subscribe;sub_args)]};
/unsubscribe:   wsapi_unsub[first[r];"trade:XBTUSD"] 
wsapi_unsub:{[wshandle;unsub_args]wsapi_cmd[wshandle;`op`args!(`unsubscribe;unsub_args)]};
/wsapi_ping
wsapi_ping:{[wshandle]neg[wshandle]"ping"};


/
//WebSocket API examples:
/subscribe trade
trade:([]timestamp:`timestamp$();price:`float$();size:`float$());
.z.ws:{xx::.j.k[x];if[key[xx]~`table`action`data;if[xx[`action]~"insert";`trade insert select ltime`timestamp$"Z"$timestamp,`float$price,`float$size from xx[`data] ]];};
wsh:wsapi[settings`apiHost; settings`apiKey; settings`apiSecret];  
wsapi_sub[first[wsh];"trade:XBTUSD"]
wsapi_unsub[first[wsh];"trade:XBTUSD"]

/Heartbeats: https://www.bitmex.com/app/wsAPI#Heartbeats
wsapi_dt:.z.P;
WS:([]time:`time$();data:());
.z.ws:{wsapi_dt::.z.P;`WS insert (.z.T;x);0N!(.z.T;.z.w)};
wsh:wsapi[settings`apiHost; settings`apiKey; settings`apiSecret];  
wsapi_sub[first[wsh];"trade:XBTUSD"];
.z.ts:{if[00:00:05<.z.P-wsapi_dt; @[wsapi_ping;first wsh;`]];
    if[00:00:05<.z.P-wsapi_dt;
		@[hclose;first[wsh];`];
        wsh::.[wsapi;(settings`apiHost;settings`apiKey;settings`apiSecret);`];
        if[0<first wsh;.[wsapi_sub;(first[wsh];"trade:XBTUSD");`];wsapi_dt::.z.P];
        0N!(.z.Z;first[wsh];`reconnected);
	  ];};
system"t 1000";
-10#WS
.z.W
\

