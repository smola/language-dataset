module Haproxy =
    autoload xfm

    let eol = Util.eol
    let hard_eol = del "\n" "\n"
    let indent = del /[ \t]{0,4}/ "    "
    let ws = del /[ \t]+/ " "
    let store_to_eol = store Rx.space_in
    let store_to_ws = store /[^ \t\n]+/
    let store_time = store /[0-9]+(us|ms|s|m|h|d)?/

    let simple_option (r:regexp) = [ indent . key r . eol ]
    let kv_option (r:regexp) = [ indent . key r . ws . store_to_eol . eol ]
    let true_bool_option (r:regexp) = [ Util.del_str "option" . ws . key r . value "true" . eol ]
    let false_bool_option (r:regexp) = [ Util.del_str "no option" . ws . key r . value "false" . eol ]
    let bool_option (r:regexp) = ( false_bool_option r | true_bool_option r )

    (*************************************************************************
      LOG OPTION
     *************************************************************************)
    let log_facility = "kern" | "user" | "mail" | "daemon" | "auth" | "syslog"
                     | "lpr" | "news" | "uucp" | "cron" | "auth2" | "ftp"
                     | "ntp" | "audit" | "alert" | "cron2" | "local0"
                     | "local1" | "local2" | "local3" | "local4" | "local5"
                     | "local6" | "local7"
    let log_level = "emerg" | "alert" | "crit" | "err" | "warning" | "notice"
                  | "info" | "debug"

    let log_opt = [ indent . key "log" .
        ws . [ label "address" . store_to_ws ] .
        ws . [ label "facility" . store log_facility ] .
        ( 
            ws . [ key "max" . ws . store log_level ] . 
            ( ws . [ key "min" . ws . store log_level ] )?
        )? ] . eol

    (*************************************************************************
      STATS OPTION
     *************************************************************************)
    let stats_level = "user" | "operator" | "admin"
    let stats_uid = [ key /(uid|user)/ . ws . store_to_ws ]
    let stats_gid = [ key /(gid|group)/ . ws . store_to_ws ]
    let stats_mode = [ key "mode" . ws . store_to_ws ]
    let stats_socket = [ indent . Util.del_str "stats socket" .
        label "stats_socket" . [ ws . label "path" . store_to_ws ] .
        ( [ ws . key /(uid|user)/ . ws . store_to_ws ] )? .
        ( [ ws . key /(gid|group)/ . ws . store_to_ws ] )? .
        ( [ ws . key "mode" . ws . store_to_ws ] )? .
        ( [ ws . key "level" . ws . store stats_level ] )?
        ] . eol
    let stats_timeout = [ indent . Util.del_str "stats timeout" .
        label "stats_timeout" . ws . store_time ] . eol
    let stats_maxconn =  [ indent . Util.del_str "stats maxconn" .
        label "stats_maxconn" . ws . store /[0-9]+/ ] . eol
    let stats = ( stats_socket | stats_timeout | stats_maxconn )

    (*************************************************************************
      GLOBAL SECTION
     *************************************************************************)
    let global_simple_opts = "daemon" | "noepoll" | "nokqueue" | "noepoll"
                         | "nosepoll" | "nosplice" | "debug" | "quiet"
    let global_kv_opts = "chroot" | "gid" | "group" | "log-send-hostname"
                       | "nbproc" | "pidfile" | "uid" | "ulimit-n" | "user"
                       | "node" | "description" | "maxconn" | "maxpipes"
                       | "spread-checks" | "tune.bufsize" | "tune.chksize"
                       | "tune.maxaccept" | "tune.maxpollevents" 
                       | "tune.maxrewrite" | "tune.rcvbuf.client"
                       | "tune.rcvbuf.server" | "tune.sndbuf.client"
                       | "tune.sndbuf.server"

    let global = [ key "global" . eol .
        (simple_option global_simple_opts|kv_option global_kv_opts|stats|log_opt)*
        ]

    (*************************************************************************
      USER LISTS
     *************************************************************************)
    let userlist_group =
        let name = [ label "name" . store Rx.no_spaces ] in
        let group_user = [ label "user" . store /[^ \t\n,]+/ ] in
        let users = [ key "users" . Sep.space . group_user .
            ( Util.del_str "," . group_user )* ] in
        indent . [ key "group" . Sep.space . name . ( Sep.space . users)? ] . Util.eol

    let userlist_user =
        let name = [ label "name" . store Rx.no_spaces ] in
        let password = [ key /password|insecure-password/ . Sep.space .
            store Rx.no_spaces ] in
        let user_group = [ label "group" . store /[^ \t\n,]+/ ] in
        let groups = [ key "groups" . Sep.space . user_group .
            ( Util.del_str "," . user_group )* ] in
        Util.indent . [ key "user" . Sep.space . name .
            ( Sep.space . password )? . ( Sep.space . groups )? ] . Util.eol

    let userlist =
        let name = [ label "name" . store Rx.no_spaces ] in
        [ key "userlist" . Sep.space . name . Util.eol .
            ( userlist_user | userlist_group )* ]

    (*************************************************************************
     SERVER AND DEFAULT-SERVER
     *************************************************************************)
    let source =
        let addr = [ label "address" . store (/[^ \t\n:]+/ - /client(ip)?|hdr_ip.*/) ]
        in let port = [ label "port" . store Rx.no_spaces ]
        in let addr_and_port = addr . ( Util.del_str ":" . port )?
        in let client = [ key "client" ]
        in let clientip = [ key "clientip" ]
        in let interface = [ key "interface" . Sep.space . store Rx.no_spaces ]
        in let hdr = [ label "header" . store /[^ \t\n,\)]+/ ]
        in let occ = [ label "occurrence" . store /[^ \t\n\)]+/ ]
        in let hdr_ip = Util.del_str "hdr_ip(" . hdr .
            ( Util.del_str "," . occ )? . Util.del_str ")"
        in let usesrc = [ key "usesrc" . Sep.space . ( clientip | client | addr_and_port | hdr_ip ) ]
        in [ key "source" . Sep.space . addr_and_port .
            ( Sep.space . ( usesrc | interface ) )? ]

    let server_options =
        let health_addr = [ key "health_address" . Sep.space . store Rx.no_spaces ] in
        let backup = [ key "backup" ] in
        let check = [ key "check" ] in
        let cookie = [ key "cookie" . Sep.space . store Rx.no_spaces ] in
        let disabled = [ key "disabled" ] in
        let id = [ key "id" . Sep.space . store Rx.no_spaces ] in
        let observe = [ key "observe" . Sep.space . store Rx.no_spaces ] in
        let redir = [ key "redir" . Sep.space . store Rx.no_spaces ] in
        let server_source = source in
        let track = [ key "track" . Sep.space .
            ( [ label "proxy" . store /[^ \t\n\/]+/ . Util.del_str "/" ] )? .
            [ label "server" . store /[^ \t\n\/]+/ ] ] in
        ( health_addr | backup | check | cookie | disabled | id | observe |
            redir | server_source | track )

    let default_server_options =
        let error_limit = [ key "error-limit" . Sep.space . store Rx.no_spaces ] in
        let fall = [ key "fall" . Sep.space . store Rx.no_spaces ] in
        let inter = [ key "inter" . Sep.space . store Rx.no_spaces ] in
        let fastinter = [ key "fastinter" . Sep.space . store Rx.no_spaces ] in
        let downinter = [ key "downinter" . Sep.space . store Rx.no_spaces ] in
        let maxconn = [ key "maxconn" . Sep.space . store Rx.no_spaces ] in
        let maxqueue = [ key "maxqueue" . Sep.space . store Rx.no_spaces ] in
        let minconn = [ key "minconn" . Sep.space . store Rx.no_spaces ] in
        let on_error = [ key "on-error" . Sep.space . store Rx.no_spaces ] in
        let health_port = [ key "health_port" . Sep.space . store Rx.no_spaces ] in
        let rise = [ key "rise" . Sep.space . store Rx.no_spaces ] in
        let slowstart = [ key "slowstart" . Sep.space . store Rx.no_spaces ] in
        let weight = [ key "weight" . Sep.space . store Rx.no_spaces ] in
        ( error_limit | fall | inter | fastinter | downinter | maxconn |
            maxqueue | minconn | on_error | health_port | rise | slowstart |
            weight )

    let default_server = [ key "default-server" . Sep.space .
        default_server_options . ( Sep.space . default_server_options )* ]

    let server =
        let name = [ label "name" . store Rx.no_spaces ] in
        let addr = [ label "address" . store /[^ \t\n:]+/ ] in
        let port = [ label "port" . store Rx.no_spaces ] in
        let addr_and_port = addr . ( Util.del_str ":" . port )? in
        let options = ( server_options | default_server_options ) in
        Util.indent . [ key "server" . Sep.space . name . Sep.space .
            addr_and_port . ( Sep.space . options )* ]

    (*************************************************************************
      PROXY OPTIONS
     *************************************************************************)
    let acl = indent . [ key "acl" . ws
        . [ label "name" . store_to_ws ] . ws
        . [ label "value" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let appsession = indent . [ key "appsession" . ws
        . [ label "cookie" . store_to_ws ] . ws
        . [ key "len" . store_to_ws ] . ws
        . [ key "timeout" . store_time ]
        . ( ws . [ key "request-learn" ] )?
        . ( ws . [ key "prefix" ] )?
        . ( ws . [ key "mode" . store /(path-parameters|query-string)/ ] )?
        ] . eol

    let backlog = kv_option "backlog"

    let balance = indent . [ key "balance" . ws
        . [ label "algorithm" . store_to_ws ]
        . ( ws . [ label "params" . store /[^ \t][^\n]+/ ] )?
        ] . hard_eol

    let bind_address = [ seq "bind_addr" 
        . ( [ label "address" . store /[^ \t,]+/ ] )?
        . Util.del_str ":" . [ label "port" . store /[0-9-]+/ ] ]
    let bind_address_list = bind_address . ( Util.del_str "," . bind_address)*
    let bind = indent . [ key "bind" . ws
        . [ label "bind_addr" . bind_address_list ]
        . ( ws . [ key "interface" . store_to_ws ] )?
        . ( ws . [ key "mss" . store_to_ws ] )?
        . ( ws . [ key "transparent" ] )?
        . ( ws . [ key "id" . store_to_ws ] )?
        . ( ws . [ key "name" . store_to_ws ] )?
        . ( ws . [ key "defer-accept" ] )?
        ] . eol

    let bind_process_id = [ key /[0-9]+/ ]
    let bind_process_id_list = [ label "number"
        . bind_process_id . ( ws . bind_process_id )*
        ]
    let bind_process = indent . [ key "bind-process" . ws
        . (store /(all|odd|even)/|bind_process_id_list)
        ] . eol

    let block = indent . [ key "block" . ws
        . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let capture_cookie = indent . Util.del_str "capture cookie" . ws
        . [ label "capture_cookie"
            . [ label "name" . store_to_ws ] . ws
            . [ label "len" . store /[0-9]+/ ]
        ] . eol

    let capture_request_header = indent 
        . Util.del_str "capture request header" . ws
        . [ label "capture_request_header"
            . [ label "name" . store_to_ws ] . ws
            . [ label "len" . store /[0-9]+/ ]
        ] . eol

    let capture_response_header = indent
        . Util.del_str "capture response header" . ws
        . [ label "capture_response_header"
            . [ label "name" . store_to_ws ] . ws
            . [ label "len" . store /[0-9]+/ ]
        ] . eol

    let clitimeout = kv_option "clitimeout"

    let contimeout = kv_option "contimeout"

    let cookie = indent . [ key "cookie" . ws
        . [ label "name" . store_to_ws ]
        . ( ws . [ label "method" . store /(rewrite|insert|prefix)/ ] )?
        . ( ws . [ key "indirect" ] )?
        . ( ws . [ key "nocache" ] )?
        . ( ws . [ key "postonly" ] )?
        . ( ws . [ key "preserve" ] )?
        . ( ws . [ key "httponly" ] )?
        . ( ws . [ key "secure" ] )?
        . ( ws . [ key "domain" . store_to_ws ] )?
        . ( ws . [ key "maxidle" . store_time ] )?
        . ( ws . [ key "maxlife" . store_time ] )?
        ] . eol
    
    (* #XXX default-server *)

    let default_backend = kv_option "default_backend"

    let disabled = simple_option "disabled"

    let dispatch = indent . [ key "dispatch" . ws
        . [ label "address" . store /[^ \t,]+/ ]
        . Util.del_str ":" . [ label "port" . store /[0-9-]+/ ] ]

    let enabled = simple_option "enabled"

    let errorfile = indent . [ key "errorfile" . ws
        . [ label "code" . store /[0-9]+/ ] . ws
        . [ label "file" . store_to_eol ]
        ] . eol

    let error_redir (keyword:string) = indent . [ key keyword . ws
        . [ label "code" . store /[0-9]+/ ] . ws
        . [ label "url" . store_to_eol ]
        ] . eol

    let errorloc = error_redir "errorloc"
    let errorloc302 = error_redir "errorloc302"
    let errorloc303 = error_redir "errorloc303"

    let force_persist = indent . [ key "force-persist" . ws
        . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let fullconn = kv_option "fullconn"

    let grace = kv_option "grace"

    let hash_type = kv_option "hash-type"

    let http_check_disable_on_404 = indent
        . Util.del_str "http-check disable-on-404"
        . [ label "http_check_disable_on_404" ] . eol

    let http_check_expect = indent . Util.del_str "http-check expect"
        . [ label "http_check_expect"
            . ( ws . [ Util.del_str "!" . label "not" ] )?
            . ws . [ label "match" . store /(status|rstatus|string|rstring)/ ]
            . ws . [ label "pattern" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let http_check_send_state = indent . Util.del_str "http-check send-state"
        . [ label "http_check_keep_state" ] . eol

    let http_request =
        let allow = [ key "allow" ]
        in let deny = [ key "deny" ]
        in let realm = [ key "realm" . Sep.space . store Rx.no_spaces ]
        in let auth = [ key "auth" . ( Sep.space . realm )? ]
        in let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in Util.indent . [ Util.del_str "http-request" . label "http_request" .
            Sep.space . ( allow | deny | auth ) . ( Sep.space . cond )? ]
            . Util.eol

    let http_send_name_header = kv_option "http-send-name-header"

    let id = kv_option "id"

    let ignore_persist = indent . [ key "ignore-persist" . ws
        . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let log = (indent . [ key "log" . store "global" ] . eol ) | log_opt

    let maxconn = kv_option "maxconn"

    let mode = kv_option "mode"

    let monitor_fail = indent . Util.del_str "monitor fail"
        . [ key "monitor_fail" . ws
            . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let monitor_net = kv_option "monitor-net"

    let monitor_uri = kv_option "monitor-uri"

    let abortonclose = bool_option "abortonclose"

    let accept_invalid_http_request = bool_option "accept-invalid-http-request"
    
    let accept_invalid_http_response = bool_option "accept-invalid-http-response"

    let allbackups = bool_option "allbackups"

    let checkcache = bool_option "checkcache"

    let clitcpka = bool_option "clitcpka"

    let contstats = bool_option "contstats"

    let dontlog_normal = bool_option "dontlog-normal"

    let dontlognull = bool_option "dontlognull"

    let forceclose = bool_option "forceclose"

    let forwardfor =
        let except = [ key "except" . Sep.space . store Rx.no_spaces ]
        in let header = [ key "header" . Sep.space . store Rx.no_spaces ]
        in let if_none = [ key "if-none" ]
        in Util.indent . [ Util.del_str "option forwardfor" . label "forwardfor" .
            ( Sep.space . except )? . ( Sep.space . header )? .
            ( Sep.space . if_none )? ] . Util.eol

    let http_no_delay = bool_option "http-no-delay"

    let http_pretend_keepalive = bool_option "http-pretend-keepalive"

    let http_server_close = bool_option "http-server-close"

    let http_use_proxy_header = bool_option "http-use-proxy-header"

    let httpchk =
        let uri = [ label "uri" . Sep.space . store Rx.no_spaces ]
        in let method = [ label "method" . Sep.space . store Rx.no_spaces ]
        in let version = [ label "version" . Sep.space . store_to_eol ]
        in Util.indent . [ Util.del_str "option httpchk" . label "httpchk" .
            ( uri | method . uri . version? )? ] . Util.eol

    let httpclose = bool_option "httpclose"

    let httplog =
        let clf = [ Sep.space . key "clf" ]
        in Util.indent . [ Util.del_str "option httplog" . label "httplog" .
            clf? ] . Util.eol

    let http_proxy = bool_option "http_proxy"

    let independant_streams = bool_option "independant-streams"

    let ldap_check = bool_option "ldap-check"

    let log_health_checks = bool_option "log-health-checks"

    let log_separate_errors = bool_option "log-separate-errors"

    let logasap = bool_option "logasap"

    let mysql_check =
        let user = [ key "user" . Sep.space . store Rx.no_spaces ]
        in Util.indent . [ Util.del_str "option mysql-check" .
            label "mysql_check" . ( Sep.space . user )? ] . Util.eol

    let nolinger = bool_option "nolinger"

    let originalto =
        let except = [ key "except" . Sep.space . store Rx.no_spaces ]
        in let header = [ key "header" . Sep.space . store Rx.no_spaces ]
        in Util.indent . [ Util.del_str "option originalto" . label "originalto" .
            ( Sep.space . except )? . ( Sep.space . header )? ] . Util.eol


    let persist = bool_option "persist"

    let redispatch = bool_option "redispatch"

    let smtpchk =
        let hello = [ label "hello" . store Rx.no_spaces ]
        in let domain = [ label "domain" . store Rx.no_spaces ]
        in Util.indent . [ Util.del_str "option smtpchk" . label "smtpchk" .
            ( Sep.space . hello . Sep.space . domain )? ] . Util.eol

    let socket_stats = bool_option "socket-stats"

    let splice_auto = bool_option "splice-auto"

    let splice_request = bool_option "splice-request"

    let splice_response = bool_option "splice-response"

    let srvtcpka = bool_option "srvtcpka"

    let ssl_hello_chk = bool_option "ssl-hello-chk"

    let tcp_smart_accept = bool_option "tcp-smart-accept"

    let tcp_smart_connect = bool_option "tcp-smart-connect"

    let tcpka = bool_option "tcpka"

    let tcplog = bool_option "tcplog"

    let old_transparent = bool_option "transparent"

    let persist_rdp_cookie = indent . [ Util.del_str "persist rdp-cookie" . 
        label "persist-rdp-cookie" . ( Util.del_str "(" . store /[^\)]+/ . Util.del_str ")" )?
        ] . hard_eol

    let rate_limit_sessions = indent . [ Util.del_str "rate-limit sessions" . ws .
        label "rate-limit-sessions" . store /[0-9]+/ ] . eol

    let redirect =
        let location = [ key "location" ]
        in let prefix = [ key "prefix" ]
        in let to = [ label "to" . store Rx.no_spaces ]
        in let code = [ key "code" . Sep.space . store Rx.no_spaces ]
        in let option_drop_query = [ key "drop-query" ]
        in let option_append_slash = [ key "append-slash" ]
        in let option_set_cookie = [ key "set-cookie" . Sep.space .
            [ label "cookie" . store /[^ \t\n=]+/ ] .
            ( [ Util.del_str "=" . label "value" . store Rx.no_spaces ] )? ]
        in let option_clear_cookie = [ key "clear-cookie" . Sep.space .
            [ label "cookie" . store Rx.no_spaces ] ]
        in let options = (option_drop_query | option_append_slash | option_set_cookie | option_clear_cookie)
        in let option = [ label "options" . options . ( Sep.space . options )* ]
        in let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in Util.indent . [ key "redirect" . Sep.space . ( location | prefix ) .
            Sep.space . to . ( Sep.space . code )? . ( Sep.space . option )? .
            ( Sep.space . cond )? ] . Util.eol

    let reqadd = kv_option "reqadd"

    let reqallow = kv_option "reqallow"

    let reqiallow = kv_option "reqiallow"

    let reqdel = kv_option "reqdel"

    let reqidel = kv_option "reqdel"

    let reqdeny = kv_option "reqdeny"

    let reqideny = kv_option "reqideny"

    let reqpass = kv_option "reqpass"

    let reqipass = kv_option "reqipass"

    let reqrep = kv_option "reqrep"

    let reqirep = kv_option "reqirep"

    let reqtarpit = kv_option "reqtarpit"

    let reqitarpit = kv_option "reqitarpit"

    let retries = kv_option "retries"

    let rspadd = kv_option "rspadd"

    let rspdel = kv_option "rspdel"

    let rspidel = kv_option "rspidel"

    let rspdeny = kv_option "rspdeny"

    let rspideny = kv_option "rspideny"

    let rsprep = kv_option "rsprep"

    let rspirep = kv_option "rspirep"

    (* XXX server *)


    let srvtimeout = kv_option "srvtimeout"

    let stats_admin =
        let cond = [ key /if|unless/ . Sep.space . store Rx.space_in ]
        in Util.indent . [ Util.del_str "stats admin" . label "stats_admin" .
            Sep.space . cond ] . Util.eol

    let stats_auth =
        let user = [ label "user" . store /[^ \t\n:]+/ ]
        in let passwd = [ label "passwd" . store /[^ \t\n]+/ ]
        in Util.indent . [ Util.del_str "stats auth" . label "stats_auth" .
            Sep.space . user . Util.del_str ":" . passwd ] . Util.eol

    let stats_enable = Util.indent . [ Util.del_str "stats enable" .
        label "stats_enable" ] . Util.eol

    let stats_hide_version = Util.indent . [ Util.del_str "stats hide-version" .
        label "stats_hide_version" ] . Util.eol

    let stats_http_request =
        let allow = [ key "allow" ]
        in let deny = [ key "deny" ]
        in let realm = [ key "realm" . Sep.space . store Rx.no_spaces ]
        in let auth = [ key "auth" . ( Sep.space . realm )? ]
        in let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in Util.indent . [ Util.del_str "stats http-request" .
            label "stats_http_request" . Sep.space . ( allow | deny | auth ) .
            ( Sep.space . cond )? ] . Util.eol

    let stats_realm = Util.indent . [ Util.del_str "stats realm" .
        label "stats_realm" . Sep.space . store /[^ \t\n]+/ ] . Util.eol

    let stats_refresh = Util.indent . [ Util.del_str "stats refresh" .
        label "stats_refresh" . Sep.space . store /[^ \t\n]+/ ] . Util.eol

    let stats_scope = Util.indent . [ Util.del_str "stats scope" .
        label "stats_scope" . Sep.space . store /[^ \t\n]+/ ] . Util.eol

    let stats_show_desc =
        let desc = [ label "description" . store_to_eol ]
        in Util.indent . [ Util.del_str "stats show-desc" .
            label "stats_show_desc" . ( Sep.space . desc )? ] . Util.eol

    let stats_show_legends = Util.indent . [ Util.del_str "stats show-legends" .
        label "stats_show_legends" ] . Util.eol

    let stats_show_node =
        let node = [ label "node" . store_to_eol ]
        in Util.indent . [ Util.del_str "stats show-node" .
            label "stats_show_node" . ( Sep.space . node )? ] . Util.eol

    let stats_uri = Util.indent . [ Util.del_str "stats uri" .
        label "stats_uri" . Sep.space . store_to_eol ] . Util.eol

    let stick_match =
        let table = [ key "table" . Sep.space . store Rx.no_spaces ]
        in let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in let pattern = [ label "pattern" . store Rx.no_spaces ]
        in Util.indent . [ Util.del_str "stick match" . label "stick_match" .
            Sep.space . pattern . ( Sep.space . table )? .
            ( Sep.space . cond )? ] . Util.eol

    let stick_on =
        let table = [ key "table" . Sep.space . store Rx.no_spaces ]
        in let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in let pattern = [ label "pattern" . store Rx.no_spaces ]
        in Util.indent . [ Util.del_str "stick on" . label "stick_on" .
            Sep.space . pattern . ( Sep.space . table )? .
            ( Sep.space . cond )? ] . Util.eol

    let stick_store_request =
        let table = [ key "table" . Sep.space . store Rx.no_spaces ]
        in let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in let pattern = [ label "pattern" . store Rx.no_spaces ]
        in Util.indent . [ Util.del_str "stick store-request" .
            label "stick_store_request" . Sep.space . pattern .
            ( Sep.space . table )? . ( Sep.space . cond )? ] . Util.eol

    let stick_table =
        let type_ip = [ key "type" . Sep.space . store "ip" ]
        in let type_integer = [ key "type" . Sep.space . store "integer" ]
        in let len = [ key "len" . Sep.space . store Rx.no_spaces ]
        in let type_string = [ key "type" . Sep.space . store "string" .
            ( Sep.space . len )? ]
        in let type = ( type_ip | type_integer | type_string )
        in let size = [ key "size" . Sep.space . store Rx.no_spaces ]
        in let expire = [ key "expire" . Sep.space . store Rx.no_spaces ]
        in let nopurge = [ key "nopurge" ]
        in Util.indent . [ key "stick-table" . Sep.space . type . Sep.space .
            size . ( Sep.space . expire )? . ( Sep.space . nopurge )? ] .
            Util.eol

    let tcp_request_content_accept =
        let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in Util.indent . [ Util.del_str "tcp-request content accept" .
            label "tcp_request_content_accept" . ( Sep.space . cond )? ] .
            Util.eol

    let tcp_request_content_reject =
        let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in Util.indent . [ Util.del_str "tcp-request content reject" .
            label "tcp_request_content_reject" . ( Sep.space . cond )? ] .
            Util.eol

    let tcp_request_inspect_delay = Util.indent .
        [ Util.del_str "tcp-request inspect-delay" .
            label "tcp_request_inspect_delay" . Sep.space . store_to_eol ] .
        Util.eol

    let timeout_check = Util.indent . [ Util.del_str "timeout check" .
        label "timeout_check" . Sep.space . store_to_eol ] . Util.eol

    let timeout_client = Util.indent . [ Util.del_str "timeout client" .
        label "timeout_client" . Sep.space . store_to_eol ] . Util.eol

    let timeout_clitimeout = Util.indent . [ Util.del_str "timeout clitimeout" .
        label "timeout_clitimeout" . Sep.space . store_to_eol ] . Util.eol

    let timeout_connect = Util.indent . [ Util.del_str "timeout connect" .
        label "timeout_connect" . Sep.space . store_to_eol ] . Util.eol

    let timeout_contimeout = Util.indent . [ Util.del_str "timeout contimeout" .
        label "timeout_contimeout" . Sep.space . store_to_eol ] . Util.eol

    let timeout_http_keep_alive = Util.indent . [ Util.del_str "timeout http-keep-alive" .
        label "timeout_http_keep_alive" . Sep.space . store_to_eol ] . Util.eol

    let timeout_http_request = Util.indent . [ Util.del_str "timeout http-request" .
        label "timeout_http_request" . Sep.space . store_to_eol ] . Util.eol

    let timeout_queue = Util.indent . [ Util.del_str "timeout queue" .
        label "timeout_queue" . Sep.space . store_to_eol ] . Util.eol

    let timeout_server = Util.indent . [ Util.del_str "timeout server" .
        label "timeout_server" . Sep.space . store_to_eol ] . Util.eol

    let timeout_srvtimeout = Util.indent . [ Util.del_str "timeout srvtimeout" .
        label "timeout_srvtimeout" . Sep.space . store_to_eol ] . Util.eol

    let timeout_tarpit = Util.indent . [ Util.del_str "timeout tarpit" .
        label "timeout_tarpit" . Sep.space . store_to_eol ] . Util.eol

    let transparent = simple_option "transparent"

    let use_backend =
        let cond = [ key /if|unless/ . Sep.space . store_to_eol ]
        in Util.indent . [ key "use_backend" . Sep.space . store Rx.no_spaces .
        Sep.space . cond ] . Util.eol

    let lns = global

    let xfm = transform lns (incl "/etc/haproxy/haproxy.cfg")
