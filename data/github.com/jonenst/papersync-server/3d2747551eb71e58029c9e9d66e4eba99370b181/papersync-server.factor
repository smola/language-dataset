! Copyright (C) 2013 Jon Harper.
! See http://factorcode.org/license.txt for BSD license.
USING: papersync-server.gcm accessors assocs continuations db.sqlite
furnace.actions furnace.alloy furnace.auth furnace.auth.basic
furnace.auth.features.registration furnace.auth.providers
furnace.json html.forms http.server http.server.dispatchers
http.server.responses io.servers io.sockets.secure kernel
logging namespaces sequences sets validators vectors papersync-server.conf ;
IN: papersync-server

LOG: current-username DEBUG
LOG: register-regid DEBUG
: log-current-user ( -- ) logged-in-user get username>> current-username ;

: ?vector-adjoin ( elt set/f -- set' )
  [ 1 <vector> ] unless* [ adjoin ] keep ;
: uadjoin ( value key -- ) [ ?vector-adjoin ] with uchange ;
: register-id-submit ( -- response )
  log-current-user
  "regid" [ value dup register-regid ] [ uadjoin ] bi
  t <json-content> ;
: register-id-validate ( -- )
  { { "regid" [ v-required ] } } validate-params ;
: <register-id-action> ( -- action )
  <action>
    [ register-id-validate ] >>validate
    [ register-id-submit ] >>submit  ;

LOG: paired-username DEBUG
LOG: paired-user-ids DEBUG
LOG: current-user-ids DEBUG
: paired-regid ( -- ids )
  "pair-username" uget dup paired-username [ users get-user
   profile>> "regid" of dup paired-user-ids ] [ f ] if*
   profile   "regid" of dup current-user-ids union ;

LOG: gcm-response DEBUG
LOG: gcm-error ERROR
: send-submit ( -- response )
  log-current-user
  "url" value paired-regid
  [ gcm-send gcm-response t <json-content> ]
  [ gcm-error 2drop <400> ] recover ;
: send-validate ( -- )
  { { "url" [ v-url ] } } validate-params ;
: <send-action> ( -- action )
  <action>
    [ send-validate ] >>validate
    [ send-submit ] >>submit  ;

: (pair-users) ( userA userB -- userA userB' )
  [ [ username>> "pair-username" ] [ profile>> ] bi* set-at ] 2keep ;
: users-changed ( userA userB -- )
  [ t swap changed?<< ] bi@ ;
: pair-users ( userA userB -- )
    (pair-users) swap (pair-users) users-changed ;
: pair-submit ( -- response )
  log-current-user
  "pair-username" value dup paired-username users get-user dup [ save-user-after ] when*
  logged-in-user get 2dup and [
    pair-users t <json-content>
  ] [ 2drop <400> ] if ;
: pair-validate ( -- )
  { { "pair-username" [ v-username ] } } validate-params ;
: <pair-action> ( -- action )
  <action>
    [ pair-validate ] >>validate
    [ pair-submit ] >>submit ;

: nop-display ( -- response ) t <json-content> ;
: <check-action> ( -- action )
  <action>
    [ nop-display ] >>display ;

TUPLE: papersync-app < dispatcher ;
: <papersync-dispatcher> ( -- responder )
    papersync-app new-dispatcher
        <register-id-action> "register-id" add-responder
        <pair-action> "pair" add-responder
        <check-action> "check" add-responder
        <send-action> "send" add-responder <protected> ;

USING: db.sqlite furnace.alloy namespaces ;

: papersync-db ( -- db ) "resource:papersync.db" <sqlite-db> ;

: <auth-config> ( responder -- responder' )
  "papersync" <basic-auth-realm>
    allow-registration ;

: <papersync-secure-config> ( -- config )
  <secure-config>
     DH-FILE  >>dh-file
     KEY-FILE >>key-file
     PASSWORD >>password ;

: <papersync-app> ( -- responder )
  <papersync-dispatcher>
    <auth-config>
    papersync-db <alloy> ;

: <papersync-website-server> ( -- threaded-server )
  <http-server>
    "papersync" >>name
    <papersync-secure-config> >>secure-config
    INSECURE-PORT >>insecure
    SECURE-PORT   >>secure ;

: run-papersync ( -- )
  <papersync-app> main-responder set-global
  papersync-db start-expiring
  <papersync-website-server> start-server drop ;

MAIN: run-papersync
