xquery version "1.0-ml";
 
import module "http://marklogic.com/xdmp/security" at "/MarkLogic/security.xqy";

import module namespace cd = "http://marklogic.com/data-explore/lib/check-database-lib" at "/server/lib/check-database-lib.xqy" ;
import module namespace  check-user-lib = "http://www.marklogic.com/data-explore/lib/check-user-lib" at "/server/lib/check-user-lib.xqy" ;
import module namespace ll = "http://marklogic.com/data-explore/lib/logging-lib"  at "/server/lib/logging-lib.xqy";
declare option xdmp:mapping "false";
declare function local:login(){
    cd:check-database(),
    let $user-id := xdmp:get-request-field("userid")  
    let $password := xdmp:get-request-field("password")
    let $_ := ll:trace("FERRET: api-auth:login to ferret as " || $user-id)
    return
        if (check-user-lib:is-logged-in()) then
            (xdmp:set-response-code(200,"Success"),$user-id)
        else
            let $loggedIn := try { xdmp:login($user-id, $password) } catch ($e) { fn:false()}
            return
                if ($loggedIn) then
                    (xdmp:set-response-code(200,"Success"),xdmp:get-current-user())
                else
                    (xdmp:set-response-code(401,"Failure"),"")
};
let $_ := ll:trace("FROM: /server/endpoints/api-auth.xqy")
return
local:login()