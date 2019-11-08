%{#

#include "hiredis/hiredis.h"

redisReply*          redisCommandExn(redisContext* c, char* fmt, ...);
int                  _redisContextErr(redisContext* c);    
char*                _redisContextErrStr(redisContext* c); 
int                  _redisReplyGetType(redisReply* reply);     
long long            _redisReplyGetInteger(redisReply* reply);  
char*                _redisReplyGetStr(redisReply* reply);      
size_t               _redisReplyGetElements(redisReply* reply); 
struct redisReply ** _redisReplyGetElement(redisReply* reply);  

%} 