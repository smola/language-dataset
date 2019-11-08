xquery version "1.0-ml";

(:
 : Copyright (c) 2011-2013 Michael Blakeley. All Rights Reserved.
 :
 : Licensed under the Apache License, Version 2.0 (the "License");
 : you may not use this file except in compliance with the License.
 : You may obtain a copy of the License at
 :
 : http://www.apache.org/licenses/LICENSE-2.0
 :
 : Unless required by applicable law or agreed to in writing, software
 : distributed under the License is distributed on an "AS IS" BASIS,
 : WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 : See the License for the specific language governing permissions and
 : limitations under the License.
 :
 : The use of the Apache License does not indicate that this project is
 : affiliated with the Apache Software Foundation.
 :
 :)

import module namespace trb="com.blakeley.task-rebalancer"
  at "lib-trb.xqy" ;

declare variable $ASSIGNMENT as xs:unsignedLong external ;
declare variable $URI as xs:string external ;

declare variable $DOC as node()? := doc($URI) ;
(: Someday it would be nice to support properties... :)
declare variable $PROP as node()? := xdmp:document-properties($URI) ;

(: In an attempt to catch low-level database errors,
 : wrap the entire body.
 :)
try {

  (: We need a bail-out mechanism to stop the respawns.
   : This document acts as a kill signal.
   : Do not throw an error, since that could flood the server logs.
   :)
  if ($trb:FATAL) then xdmp:log(
    '[rebalance.xqy] FATAL is set: stopping', 'fine')
  else if ($ASSIGNMENT = xdmp:document-forest($URI)) then xdmp:log(
    text { '[rebalance.xqy]', $URI, 'is already in', $ASSIGNMENT },
    'fine')

  (: Update the existing document, by overwriting it.
   : explicitly place the document in the correct forest.
   :)
  else if (empty($DOC)) then error(
    (), 'TRB-EMPTY',
    text { 'No document for', $URI })

  else xdmp:document-insert(
    $URI,
    $DOC,
    xdmp:document-get-permissions($URI),
    xdmp:document-get-collections($URI),
    xdmp:document-get-quality($URI),
    $ASSIGNMENT) }

catch ($ex) {
  (: Set fatal if the exception looks like a database problem.
   : This tries to avoid flooding the logs with duplicate error messages.
   : However these are low-level errors so there are no guarantees.
   :)
  if (not($ex/error:code = $trb:FATAL-CODES)) then ()
  else trb:fatal-set(true()),
  xdmp:rethrow() }


(: rebalance.xqy :)
