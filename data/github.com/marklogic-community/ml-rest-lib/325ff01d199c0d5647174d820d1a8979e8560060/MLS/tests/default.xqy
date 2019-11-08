xquery version "1.0-ml";

import module namespace rest="http://marklogic.com/appservices/rest"
    at "/ml-rest-lib/rest.xqy";

import module namespace rest-impl="http://marklogic.com/appservices/rest-impl"
    at "/ml-rest-lib/rest-impl.xqy";

import module namespace tests="https://github.com/marklogic/ml-rest-lib/tests"
    at "/tests/tests.xqy";

declare namespace xdmp-http="xdmp:http";
declare namespace html="http://www.w3.org/1999/xhtml";

declare default function namespace "http://www.w3.org/2005/xpath-functions";

declare option xdmp:mapping "false";

declare function local:run-test(
  $test-group as xs:string,
  $test-number as xs:decimal
)
{
  let $group    := tests:unit-tests($test-group)
  let $children := $group/*[not(self::rest:options)]
  let $test     := $children[$test-number]
  return
    if ($test/self::tests:request-test)
    then local:run-request-test($group, $test, $test-number)
    else if ($test/self::tests:content-type-test)
    then local:run-content-type-test($group, $test, $test-number)
    else if ($test/self::tests:http-test)
    then local:run-http-test($group, $test, $test-number)
    else if ($test/self::tests:auth-test)
    then local:run-auth-test($group, $test, $test-number)
    else if ($test/self::tests:param-test)
    then local:run-param-test($group, $test, $test-number)
    else if ($test/self::tests:accept-test)
    then local:run-accept-test($group, $test, $test-number)
    else if ($test/self::tests:process-test)
    then local:run-process-test($group, $test, $test-number)
    else error((), concat("Unexpected test type: ", node-name($test)))
};

declare function local:run-request-test(
  $group as element(tests:test-group),
  $test as element(tests:request-test),
  $test-number as xs:decimal
)
{
  let $_      := if (empty($test)) then error((), "There's no such test.") else ()

  let $skip   := if (exists($group/@not-role) and xdmp:role($group/@not-role))
                 then concat("(test cannot pass if run with ", $group/@not-role, " role)")
                 else if (exists($group/@role) and not(xdmp:role($group/@role)))
                      then concat("(test cannot pass unless run with ", $group/@not-role, " role)")
                      else ()

  let $uri    := concat(tests:server-root(), $test/tests:url)

  let $reqenv := rest-impl:request-environment()
  let $_      := map:put($reqenv, "params", local:compute-params($uri))
  let $_      := map:put($reqenv, "uri", concat("/tests", $test/tests:url))
  let $_      := map:put($reqenv, "method",
                         if (empty($test/@method)) then "GET" else string($test/@method))

  let $request := if ($test/@request)
                  then $group/rest:options/rest:request[xs:decimal($test/@request)]
                  else $group/rest:options/rest:request[1]

  let $result := if (exists($skip))
                 then
                   ()
                 else
                   (xdmp:log(concat("Run test: ", $group/@id, ": ", $test-number, ": ", $uri)),
                    let $rewrite := rest-impl:rewrite($group/rest:options/rest:request, $reqenv)
                    let $_       := map:put($reqenv, "params", local:patch-params($rewrite))
                    let $params  := if (empty($rewrite))
                                    then ()
                                    else rest-impl:process-request($request, $reqenv)
                    let $errs    := if (empty($params) and empty($test/tests:result))
                                    then ()
                                    else local:errors($test, $params)
                    let $pf      := if (empty($errs)) then "PASS" else "FAIL"
                    return
                      (<dt xmlns="http://www.w3.org/1999/xhtml" class="{lower-case($pf)}">
                         { concat($pf, ": ", $group/@id, ": ", $test-number, " of ",
                                  count($group/tests:request-test)) }
                        </dt>,
                        $errs))
  return
    if (exists($skip))
    then
      <dt class="skip">
        { concat("SKIP: ", $group/@id, ": ", $test-number, " of ",
                 count($group/tests:request-test), " ", $skip) }
      </dt>
    else
      $result
};

declare function local:compute-params(
  $uri as xs:string
) as map:map
{
  let $params := map:map()
  let $qp := if (contains($uri, "?")) then (substring-after($uri, "?")) else ""
  let $_  := for $pair in tokenize($qp, "&amp;")
             let $name   := xdmp:url-decode(substring-before($pair, "="))
             let $val    := xdmp:url-decode(substring-after($pair, "="))
             let $values := (map:get($params, $name), $val)
             return
               map:put($params, $name, $values)
  return
    $params
};

declare function local:patch-params(
  $uri as xs:string?
) as map:map
{
  let $params := map:map()
  return
    if (empty($uri) or not(contains($uri, "?")))
    then
      $params
    else
      let $qp := substring-after($uri, "?")
      let $_  := for $pair in tokenize($qp, "&amp;")
                 let $name   := xdmp:url-decode(substring-before($pair, "="))
                 let $val    := xdmp:url-decode(substring-after($pair, "="))
                 let $values := (map:get($params, $name), $val)
                 return
                   map:put($params, $name, $values)
      return
        $params
};

declare function local:errors(
  $test as element(),
  $params as map:map
) as element(html:dd)?
{
  let $result  := $test/tests:result
  let $keyerrs := (if (count($result/tests:entry) = count(map:keys($params)))
                   then ()
                   else concat("Expected ", count($result/tests:entry), " keys; got ", count(map:keys($params))),
                   for $key in map:keys($params)
                   where empty($result/tests:entry[@key = $key])
                   return
                     concat("No actual value for expected key: ", $key),
                   for $entry in $result/tests:entry
                   where not(string($entry/@key) = map:keys($params))
                   return
                     concat("Unexpected key: ", $entry/@key))

  let $valerrs := for $key in map:keys($params)
                  let $val := map:get($params, $key)
                  let $exp := $result/tests:entry[@key = $key]
                  return
                    if ($exp/tests:value)
                    then
                      local:seq-err($key, $exp/tests:value/string(), $val)
                    else
                      if (string($exp) = string($val))
                      then ()
                      else concat($key, ": expected '", $exp, "' got '", $val, "'")
  return
    if (empty($keyerrs) and empty($valerrs))
    then ()
    else
      <dd xmlns="http://www.w3.org/1999/xhtml">
        { for $msg in ($keyerrs, $valerrs)
          return
            (<span>{$msg}</span>, <br/>)
        }
      </dd>
};

declare function local:run-content-type-test(
  $group as element(tests:test-group),
  $test as element(tests:content-type-test),
  $test-number as xs:decimal
)
{
  let $trace  := xdmp:log(concat("Run test: ", $group/@id, ": ", $test-number, ": content-type-test"))
  let $result := rest-impl:get-return-types($test/tests:type, $test/tests:header)
  let $errs   := local:seq-err("", $test/tests:result/tests:value, $result)
  let $pf     := if (empty($errs)) then "PASS" else "FAIL"
  return
    (<dt xmlns="http://www.w3.org/1999/xhtml" class="{lower-case($pf)}">
    { concat($pf, ": ", $group/@id, ": ", $test-number, " of ",
             count($group/tests:content-type-test)) }
     </dt>,
     if (empty($errs))
     then ()
     else
      <dd xmlns="http://www.w3.org/1999/xhtml">
        { for $msg in $errs
          return
            (<span>{$msg}</span>, <br/>)
        }
      </dd>)
};

declare function local:run-http-test(
  $group as element(tests:test-group),
  $test as element(tests:http-test),
  $test-number as xs:decimal
)
{
  let $trace  := xdmp:log(concat("Run test: ", $group/@id, ": ", $test-number, ": http-test"))
  let $request := $test/rest:request
  let $verb    := string($test/tests:verb)
  let $check   := try {rest-impl:check-request($request)}
                  catch ($e) {$e/error:code/string()}
  let $matches := try { string(rest-impl:method-matches($request,$verb,true())) }
                  catch ($e) {$e/error:code/string()}
  let $result  := if ($matches = $test/tests:result) then () else $matches
  let $errs    := ($check, $result)
  let $pf      := if (empty($errs)) then "PASS" else "FAIL"
  return
    (<dt xmlns="http://www.w3.org/1999/xhtml" class="{lower-case($pf)}">
    { concat($pf, ": ", $group/@id, ": ", $test-number, " of ",
             count($group/tests:http-test)) }
     </dt>,
     if (empty($errs))
     then ()
     else
      <dd xmlns="http://www.w3.org/1999/xhtml">
        { for $msg in $errs
          return
            (<span>{$msg}</span>, <br/>)
        }
      </dd>)
};

declare function local:seq-err(
  $key as xs:string,
  $exp as xs:string*,
  $val as xs:string*
) as xs:string*
{
  (if (count($exp) = count($val))
   then ()
   else concat($key, ": expected ", count($exp), " values; got ", count($val)),
   for $e at $index in $exp
   let $v := $val[$index]
   where string($e) != string($v)
   return
     concat($key, " value ", $index, ": '", $e, "' != '", $v, "'"))
};

declare function local:run-accept-test(
  $group as element(tests:test-group),
  $test as element(tests:accept-test),
  $test-number as xs:decimal
)
{
  let $trace   := xdmp:log(concat("Run test: ", $group/@id, ": ", $test-number, ": accept-test"))
  let $request := $test/rest:request
  let $reqenv  := rest-impl:request-environment()
  let $_       := map:put($reqenv, "uri", "/")
  let $_       := map:put($reqenv, "method", $test/tests:verb/string())

  let $accept-header := $test/tests:accept-header/string()

  let $result :=
        try {rest-impl:accept($request,$reqenv,$accept-header)}
        catch ($e) {$e/error:code/string()}
  let $result := if ($result = $test/tests:result/string())
                 then ()
                 else $result

  let $check :=
        try {rest-impl:check-request($request)}
        catch ($e) {$e/error:code/string()}
  let $errs    := ($check, $result)
  let $pf      := if (empty($errs)) then "PASS" else "FAIL"
  return
    (<dt xmlns="http://www.w3.org/1999/xhtml" class="{lower-case($pf)}">
       { concat($pf, ": ", $group/@id, ": ", $test-number, " of ",
         count($group/tests:accept-test)) }
     </dt>,
     if (empty($errs))
     then ()
     else
       <dd xmlns="http://www.w3.org/1999/xhtml">
        { for $msg in $errs
          return
            (<span>{$msg}</span>, <br/>)
        }
      </dd>)
};

declare function local:run-param-test(
  $group as element(tests:test-group),
  $test as element(tests:param-test),
  $test-number as xs:decimal
)
{
  let $trace   := xdmp:log(concat("Run test: ", $group/@id, ": ", $test-number, ": param-test"))
  let $request := $test/rest:request
  let $params  := let $field-map := map:map()
                  let $values :=
                    for $param in $test/tests:param
                    return map:put($field-map,$param/@name,
                                   (map:get($field-map,$param/@name), data($param)))
                  return
                    $field-map
  let $reqenv  := rest-impl:request-environment()
  let $_       := map:put($reqenv, "uri", "/")
  let $_       := map:put($reqenv, "method", $test/tests:verb/string())
  let $_       := map:put($reqenv, "params", $params)
  let $result  := try { rest-impl:process-request($request, $reqenv) }
                  catch ($e) { $e/error:code/string() }

  (: This is a terrible hack. If the expected result is a map, then we
     have to do some additional work, otherwise, it must be an error string :)

  let $expected :=
    if ($test/tests:result/map:map)
    then map:map($test/tests:result/map:map)
    else $test/tests:result/string()

  let $result :=
    if (($expected instance of map:map) and ($result instance of map:map))
    then if (local:map-equal($expected, $result))
         then ()
         else $result
    else if (string($result) = string($expected))
         then ()
         else $result

  let $do-check := empty($test/tests:check) or not($test/tests:check/@skip eq 'true')
  let $check :=
    if ($do-check) then
    try {rest:check-request($request)}
    catch ($e) {$e/error:code/string()}
    else ()
  let $check := if (deep-equal($check, $test/tests:check/*)) then () else $check/string()

  let $errs    := ($check, $result)
  let $pf      := if (empty($errs)) then "PASS" else "FAIL"
  return
    (<dt xmlns="http://www.w3.org/1999/xhtml" class="{lower-case($pf)}">
       { concat($pf, ": ", $group/@id, ": ", $test-number, " of ",
         count($group/tests:param-test)) }
     </dt>,
     if (empty($errs))
     then ()
     else
      <dd xmlns="http://www.w3.org/1999/xhtml">
        { for $msg in $errs
          return
            (<span>{$msg}</span>, <br/>)
        }
      </dd>)
};

declare function local:run-process-test(
  $group as element(tests:test-group),
  $test as element(tests:process-test),
  $test-number as xs:decimal
)
{
  let $request := $test/rest:request
  let $reqenv  := rest-impl:request-environment()
  let $uri     := $test/tests:url/string()
  let $_       := map:put($reqenv, "uri", $uri)
  let $_       := map:put($reqenv, "method", "GET")
  let $_       := map:put($reqenv, "params", local:compute-params($uri))
  let $params  := rest-impl:process-request($request, $reqenv)
  let $errs    := local:errors($test, $params)
  let $pf      := if (empty($errs)) then "PASS" else "FAIL"
  return
    (<dt xmlns="http://www.w3.org/1999/xhtml" class="{lower-case($pf)}">
       { concat($pf, ": ", $group/@id, ": ", $test-number, " of ",
         count($group/tests:process-test)) }
     </dt>,
     if (empty($errs))
     then ()
     else
      <dd xmlns="http://www.w3.org/1999/xhtml">
        { for $msg in $errs
          return
            (<span>{$msg}</span>, <br/>)
        }
      </dd>)

};

declare function local:map-equal($map1 as map:map, $map2 as map:map) as xs:boolean {
  (: this is kind of crude...see also http://rfetrack.marklogic.com:8080/detail/1889 :)
  let $matches
    := (for $key in map:keys($map1)
        return
          not(empty(map:get($map2, $key))) and deep-equal(map:get($map1,$key), map:get($map2,$key)),
        for $key in map:keys($map2)
        return
          not(empty(map:get($map1, $key))) and deep-equal(map:get($map1,$key), map:get($map2,$key)))
  return
    not(false() = $matches)
};

declare function local:run-auth-test(
  $group as element(tests:test-group),
  $test as element(tests:auth-test),
  $test-number as xs:decimal
)
{
  let $trace  := xdmp:log(concat("Run test: ", $group/@id, ": ", $test-number, ": auth-test"))
  let $request := $test/rest:request
  let $reqenv  := rest-impl:request-environment()
  let $_       := map:put($reqenv, "uri", "/")
  let $_       := map:put($reqenv, "method", $test/tests:verb/string())
  let $query :=
    'import module namespace rest-impl="http://marklogic.com/appservices/rest-impl"
     at "/ml-rest-lib/rest-impl.xqy";
     declare variable $request as element() external;
     declare variable $reqenv as map:map external;
     rest-impl:conditions-match($request,$reqenv,true())'

    let $vars := (xs:QName("request"),$request,xs:QName("reqenv"),$reqenv)

    (: nobody is shipped with the server, and does not have the expected priv :)
    let $eval-opts :=
        <options xmlns="xdmp:eval">
            <user-id>{xdmp:user("nobody")}</user-id>
        </options>

    let $result := try {xdmp:eval($query,$vars,$eval-opts)}
                   catch ($e) {$e/error:code/string()}

    let $result := if ($result = $test/tests:result/string())
                   then ()
                   else $result

    let $check :=
        try {rest:check-request($request)}
        catch ($e) {$e/error:code/string()}

    let $errs    := ($check, $result)
    let $pf      := if (empty($errs)) then "PASS" else "FAIL"
    return
      (<dt xmlns="http://www.w3.org/1999/xhtml" class="{lower-case($pf)}">
         { concat($pf, ": ", $group/@id, ": ", $test-number, " of ",
           count($group/tests:auth-test)) }
       </dt>,
       if (empty($errs))
       then ()
       else
        <dd xmlns="http://www.w3.org/1999/xhtml">
          { for $msg in $errs
            return
              (<span>{$msg}</span>, <br/>)
          }
        </dd>)

};

(: Just for simplicity in tracing the flow, let's not use the code here :)
let $group := xdmp:get-request-field("group")
let $test  := if (empty(xdmp:get-request-field("test")))
              then ()
              else xs:int(xdmp:get-request-field("test"))
return
  <html xmlns="http://www.w3.org/1999/xhtml">
  <head>
  <title>REST Library Unit Test Results</title>
  <style type="text/css">
.pass {{
  background-color: #AAFFAA;
}}

.fail {{
  background-color: #FFAAAA;
}}

.skip {{
  background-color: #FFFFAA;
}}

dd {{
  color: #AAAAAA;
}}
  </style>
  </head>
  <body>
    <h1>REST Library Unit Test Results</h1>
    { if (exists($group) and exists($test))
      then
        local:run-test($group, $test)
      else
        for $group in tests:unit-tests()/tests:test-group
        return
          <div>
            <h3>Test group: "{string($group/@id)}"</h3>
            <dl>
              { for $number at $index in $group/tests:*
                return
                  local:run-test($group/@id, $index)
              }
            </dl>
          </div>
    }
    <p>End of tests.</p>
  </body>
  </html>
