module Test_haproxy =
    let global = "global\n    daemon\nlog 10.0.0.1 local0\n"

    test Haproxy.global get global = 
        { "global"
            { "daemon" }
            { "log"
                { "address" = "10.0.0.1" }
                { "facility" = "local0" }
            }
        }

    (*
     * LOG
     *)

    let basic_log = "log 127.0.0.1 local0\n"
    let log_with_max = "log 127.0.0.1 local0 max emerg\n"
    let log_with_max_min = "log 127.0.0.1 local0 max emerg min warning\n"

    test Haproxy.log_opt get basic_log =
        { "log"
            { "address" = "127.0.0.1" }
            { "facility" = "local0" }
        }

    test Haproxy.log_opt get log_with_max =
        { "log"
            { "address" = "127.0.0.1" }
            { "facility" = "local0" }
            { "max" = "emerg" }
        }

    test Haproxy.log_opt get log_with_max_min =
        { "log"
            { "address" = "127.0.0.1" }
            { "facility" = "local0" }
            { "max" = "emerg" }
            { "min" = "warning" }
        }

    (*
     * STATS SOCKET
     *)

    let basic_stats_socket = "stats socket /tmp/foo.sock\n"
    let stats_socket_uid = "stats socket /tmp/foo.sock uid 1000\n"
    let stats_socket_gid = "stats socket /tmp/foo.sock gid 1234\n"
    let stats_socket_mode = "stats socket /tmp/foo.sock mode 0444\n"
    let stats_socket_level = "stats socket /tmp/foo.sock level operator\n"
    let stats_socket_everything = "stats socket /tmp/foo.sock uid 1000 gid 1234 mode 0444 level operator\n"

    test Haproxy.stats_socket get basic_stats_socket =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
        }

    test Haproxy.stats_socket get stats_socket_uid =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "uid" = "1000" }
        }

    test Haproxy.stats_socket get stats_socket_gid =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "gid" = "1234" }
        }

    test Haproxy.stats_socket get stats_socket_mode =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "mode" = "0444" }
        }

    test Haproxy.stats_socket get stats_socket_level =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "level" = "operator" }
        }

    test Haproxy.stats_socket get stats_socket_everything =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "uid" = "1000" }
            { "gid" = "1234" }
            { "mode" = "0444" }
            { "level" = "operator" }
        }

    (*
     * STATS TIMEOUT
     *)

    let stats_timeout = "stats timeout 100000\n"
    let stats_timeout_unit = "stats timeout 10s\n"

    test Haproxy.stats_timeout get stats_timeout =
        { "stats_timeout" = "100000" }

    test Haproxy.stats_timeout get stats_timeout_unit =
        { "stats_timeout" = "10s" }

    (*
     * BIND
     *)

    let bind_addr_list = ":443,10.0.0.1:80,*:22,::ff:80"

    test Haproxy.bind_address_list get bind_addr_list =
        { "1"
            { "port" = "443" }
        }
        { "2"
            { "address" = "10.0.0.1" }
            { "port" = "80" }
        }
        { "3"
            { "address" = "*" }
            { "port" = "22" }
        }
        { "4"
            { "address" = "::ff" }
            { "port" = "80" }
        }

    test Haproxy.abortonclose get "no option abortonclose\n" = { "abortonclose" = "false" }
    test Haproxy.persist_rdp_cookie get "persist rdp-cookie\n" = { "persist-rdp-cookie" }
    test Haproxy.persist_rdp_cookie get "persist rdp-cookie(foo)\n" = { "persist-rdp-cookie" = "foo" }

    test Haproxy.rate_limit_sessions get "rate-limit sessions 100\n" = { "rate-limit-sessions" = "100" }

    test Haproxy.reqadd get "reqadd X-Proxy:\ SSL if foo\n" = {
        "reqadd" = "X-Proxy:\ SSL if foo"
    }
    test Haproxy.reqadd get "reqadd X-Proxy:\ SSL unless foo bar\n" = {
        "reqadd" = "X-Proxy:\ SSL unless foo bar"
    }

    test Haproxy.reqallow get "reqallow ^Host:\ www\. if foo\n" = {
        "reqallow" = "^Host:\ www\. if foo"
    }

    test Haproxy.stats_admin get "stats admin if LOCALHOST\n" = {
        "stats_admin"
            { "if" = "LOCALHOST" }
    }

    test Haproxy.stats_auth get "stats auth admin:foo\n" = {
        "stats_auth"
            { "user" = "admin" }
            { "passwd" = "foo" }
    }

    test Haproxy.stats_enable get "stats enable\n" = {
        "stats_enable"
    }

    test Haproxy.stats_realm get "stats realm foo\n" = {
        "stats_realm" = "foo"
    }

    test Haproxy.stats_refresh get "stats refresh 100\n" = {
        "stats_refresh" = "100"
    }

    test Haproxy.stats_scope get "stats scope .\n" = {
        "stats_scope" = "."
    }

    test Haproxy.stats_show_desc get "stats show-desc\n" = {
        "stats_show_desc"
    }

    test Haproxy.stats_show_desc get "stats show-desc something\n" = {
        "stats_show_desc"
            { "description" = "something" }
    }

    test Haproxy.stats_show_node get "stats show-node\n" = {
        "stats_show_node"
    }

    test Haproxy.stats_show_node get "stats show-node foo\n" = {
        "stats_show_node"
            { "node" = "foo" }
    }

    test Haproxy.stats_uri get "stats uri /admin?stats\n" = {
        "stats_uri" = "/admin?stats"
    }

    test Haproxy.tcp_request_content_accept get "tcp-request content accept\n" = {
        "tcp_request_content_accept"
    }

    test Haproxy.tcp_request_content_accept get "tcp-request content accept if FOO\n" = {
        "tcp_request_content_accept"
            { "if" = "FOO" }
    }

    test Haproxy.tcp_request_content_accept get "tcp-request content accept unless A\n" = {
        "tcp_request_content_accept"
            { "unless" = "A" }
    }

    test Haproxy.tcp_request_content_reject get "tcp-request content reject\n" = {
        "tcp_request_content_reject"
    }

    test Haproxy.tcp_request_content_reject get "tcp-request content reject if FOO\n" = {
        "tcp_request_content_reject"
            { "if" = "FOO" }
    }

    test Haproxy.tcp_request_content_reject get "tcp-request content reject unless A\n" = {
        "tcp_request_content_reject"
            { "unless" = "A" }
    }

    test Haproxy.tcp_request_inspect_delay get "tcp-request inspect-delay 100\n" = {
        "tcp_request_inspect_delay" = "100"
    }

    test Haproxy.timeout_check get "timeout check 100\n" = {
        "timeout_check" = "100"
    }

    test Haproxy.timeout_client get "timeout client 100\n" = {
        "timeout_client" = "100"
    }

    test Haproxy.timeout_clitimeout get "timeout clitimeout 100\n" = {
        "timeout_clitimeout" = "100"
    }

    test Haproxy.timeout_connect get "timeout connect 100\n" = {
        "timeout_connect" = "100"
    }

    test Haproxy.timeout_contimeout get "timeout contimeout 100\n" = {
        "timeout_contimeout" = "100"
    }

    test Haproxy.timeout_http_keep_alive get "timeout http-keep-alive 100\n" = {
        "timeout_http_keep_alive" = "100"
    }

    test Haproxy.timeout_http_request get "timeout http-request 100\n" = {
        "timeout_http_request" = "100"
    }

    test Haproxy.timeout_queue get "timeout queue 100\n" = {
        "timeout_queue" = "100"
    }

    test Haproxy.timeout_server get "timeout server 100\n" = {
        "timeout_server" = "100"
    }

    test Haproxy.timeout_srvtimeout get "timeout srvtimeout 100\n" = {
        "timeout_srvtimeout" = "100"
    }

    test Haproxy.timeout_tarpit get "timeout tarpit 100\n" = {
        "timeout_tarpit" = "100"
    }

    test Haproxy.use_backend get "use_backend foo if BAR\n" = {
        "use_backend" = "foo"
            { "if" = "BAR" }
    }

    test Haproxy.use_backend get "use_backend foo unless BAR\n" = {
        "use_backend" = "foo"
            { "unless" = "BAR" }
    }

    test Haproxy.http_request get "http-request allow if nagios\n" = {
        "http_request"
            { "allow" }
            { "if" = "nagios"}
    }

    test Haproxy.http_request get "http-request allow if local_net auth_ok\n" = {
        "http_request"
            { "allow" }
            { "if" = "local_net auth_ok" }
    }

    test Haproxy.http_request get "http-request auth realm Gimme if local_net auth_ok\n" = {
        "http_request"
            { "auth"
                { "realm" = "Gimme" }
            }
            { "if" = "local_net auth_ok" }
    }

    test Haproxy.http_request get "http-request deny\n" = {
        "http_request"
            { "deny" }
    }

    test Haproxy.forwardfor get "option forwardfor except 127.0.0.1\n" = {
        "forwardfor"
            { "except" = "127.0.0.1" }
    }

    test Haproxy.forwardfor get "option forwardfor header X-Client if-none\n" = {
        "forwardfor"
            { "header" = "X-Client" }
            { "if-none" }
    }

    test Haproxy.httpchk get "option httpchk OPTIONS * HTTP/1.1\\r\\nHost:\ www\n" = {
        "httpchk"
            { "method" = "OPTIONS" }
            { "uri" = "*" }
            { "version" = "HTTP/1.1\\r\\nHost:\ www" }
    }

    test Haproxy.httpchk get "option httpchk\n" = {
        "httpchk"
    }

    test Haproxy.httpchk get "option httpchk /\n" = {
        "httpchk"
            { "uri" = "/" }
    }

    test Haproxy.httpchk get "option httpchk GET /_ping\n" = {
        "httpchk"
            { "method" = "GET" }
            { "uri" = "/_ping" }
    }

    test Haproxy.httplog get "option httplog\n" = {
        "httplog"
    }

    test Haproxy.httplog get "option httplog clf\n" = {
        "httplog"
            { "clf" }
    }

    test Haproxy.mysql_check get "option mysql-check\n" = {
        "mysql_check"
    }

    test Haproxy.mysql_check get "option mysql-check user foo\n" = {
        "mysql_check"
            { "user" = "foo" }
    }

    test Haproxy.originalto get "option originalto\n" = {
        "originalto"
    }

    test Haproxy.originalto get "option originalto except 127.0.0.1\n" = {
        "originalto"
            { "except" = "127.0.0.1" }
    }

    test Haproxy.originalto get "option originalto header X-Client-Dst\n" = {
        "originalto"
            { "header" = "X-Client-Dst" }
    }

    test Haproxy.originalto get "option originalto except 127.0.0.1 header X-Client-Dst\n" = {
        "originalto"
            { "except" = "127.0.0.1" }
            { "header" = "X-Client-Dst" }
    }

    test Haproxy.smtpchk get "option smtpchk\n" = {
        "smtpchk"
    }

    test Haproxy.smtpchk get "option smtpchk HELO mydomain.org\n" = {
        "smtpchk"
            { "hello" = "HELO" }
            { "domain" = "mydomain.org" }
    }

    test Haproxy.redirect get "redirect prefix https://mysite.com set-cookie SEEN=1 if !foo\n" = {
        "redirect"
            { "prefix" }
            { "to" = "https://mysite.com" }
            { "options"
                { "set-cookie"
                    { "cookie" = "SEEN" }
                    { "value" = "1" }
                }
            }
            { "if" = "!foo" }
    }

    test Haproxy.redirect get "redirect prefix https://mysite.com if login_page !secure\n" = {
        "redirect"
            { "prefix" }
            { "to" = "https://mysite.com" }
            { "if" = "login_page !secure" }
    }

    test Haproxy.redirect get "redirect prefix http://mysite.com drop-query if login_page !uid_given\n" = {
        "redirect"
            { "prefix" }
            { "to" = "http://mysite.com" }
            { "options"
                { "drop-query" }
            }
            { "if" = "login_page !uid_given" }
    }

    test Haproxy.redirect get "redirect location / clear-cookie USERID=       if logout\n" = {
        "redirect"
            { "location" }
            { "to" = "/" }
            { "options"
                { "clear-cookie"
                    { "cookie" = "USERID=" }
                }
            }
            { "if" = "logout" }
    }

    test Haproxy.redirect get "redirect prefix / code 302 drop-query append-slash unless missing_slash\n" = {
        "redirect"
            { "prefix" }
            { "to" = "/" }
            { "code" = "302" }
            { "options"
                { "drop-query" }
                { "append-slash" }
            }
            { "unless" = "missing_slash" }
    }

    test Haproxy.stick_match get "stick match src\n" = {
        "stick_match"
            { "pattern" = "src" }
    }

    test Haproxy.stick_match get "stick match src table pop\n" = {
        "stick_match"
            { "pattern" = "src" }
            { "table" = "pop" }
    }

    test Haproxy.stick_match get "stick match src table pop if FOO\n" = {
        "stick_match"
            { "pattern" = "src" }
            { "table" = "pop" }
            { "if" = "FOO" }
    }

    test Haproxy.stick_table get "stick-table type string len 10 size 100 expire 1 nopurge\n" = {
        "stick-table"
            { "type" = "string"
                { "len" = "10" }
            }
            { "size" = "100" }
            { "expire" = "1" }
            { "nopurge" }
    }

    test Haproxy.source get "source 127.0.0.1" = {
        "source"
            { "address" = "127.0.0.1" }
    }

    test Haproxy.source get "source 127.0.0.1:8000" = {
        "source"
            { "address" = "127.0.0.1" }
            { "port" = "8000" }
    }

    test Haproxy.source get "source 127.0.0.1:8000 usesrc 127.0.0.2:9000" = {
        "source"
            { "address" = "127.0.0.1" }
            { "port" = "8000" }
            { "usesrc"
                { "address" = "127.0.0.2" }
                { "port" = "9000" }
            }
    }

    test Haproxy.source get "source 127.0.0.1:8000 usesrc client" = {
        "source"
            { "address" = "127.0.0.1" }
            { "port" = "8000" }
            { "usesrc"
                { "client" }
            }
    }

    test Haproxy.source get "source 127.0.0.1:8000 usesrc clientip" = {
        "source"
            { "address" = "127.0.0.1" }
            { "port" = "8000" }
            { "usesrc"
                { "clientip" }
            }
    }

    test Haproxy.source get "source 127.0.0.1:8000 usesrc hdr_ip(Foo)" = {
        "source"
            { "address" = "127.0.0.1" }
            { "port" = "8000" }
            { "usesrc"
                { "header" = "Foo" }
            }
    }

    test Haproxy.source get "source 127.0.0.1:8000 usesrc hdr_ip(Foo,5)" = {
        "source"
            { "address" = "127.0.0.1" }
            { "port" = "8000" }
            { "usesrc"
                { "header" = "Foo" }
                { "occurrence" = "5" }
            }
    }

    test Haproxy.source get "source 127.0.0.1:8000 interface bond1" = {
        "source"
            { "address" = "127.0.0.1" }
            { "port" = "8000" }
            { "interface" = "bond1" }
    }

    test Haproxy.server get "    server foo1 127.0.0.1:80 check maxconn 400 weight 1\n" = {
        "server"
            { "name" = "foo1" }
            { "address" = "127.0.0.1" }
            { "port" = "80" }
            { "check" }
            { "maxconn" = "400" }
            { "weight" = "1" }
    }

    test Haproxy.userlist_group get "   group G1 users tiger,scott\n" = {
        "group"
            { "name" = "G1" }
            { "users"
                { "user" = "tiger" }
                { "user" = "scott" }
            }
    }

    let userlist_conf1 = "userlist L1
    group G1 users tiger,scott
    group G2 users xdb,scott
    user tiger password $6$k6y3o.eP$JlKBx9za9667qe4xHSwRv6J.C0/D7cV91
    user scott insecure-password elgato
    user xdb insecure-password hello\n"

    test Haproxy.userlist get userlist_conf1 = {
        "userlist"
            { "name" = "L1" }
            { "group"
                { "name" = "G1" }
                { "users"
                    { "user" = "tiger" }
                    { "user" = "scott" }
                }
            }
            { "group"
                { "name" = "G2" }
                { "users"
                    { "user" = "xdb" }
                    { "user" = "scott" }
                }
            }
            { "user"
                { "name" = "tiger" }
                { "password" = "$6$k6y3o.eP$JlKBx9za9667qe4xHSwRv6J.C0/D7cV91" }
            }
            { "user"
                { "name" = "scott" }
                { "insecure-password" = "elgato" }
            }
            { "user"
                { "name" = "xdb" }
                { "insecure-password" = "hello" }
            }
    }

    let userlist_conf2 = "userlist L2
    group G1
    group G2
    user tiger password $6$k6y3o.eP$JlKBx(...)xHSwRv6J.C0/D7cV91 groups G1
    user scott insecure-password elgato groups G1,G2
    user xdb insecure-password hello groups G2\n"

    test Haproxy.userlist get userlist_conf2 = {
        "userlist"
            { "name" = "L2" }
            { "group"
                { "name" = "G1" }
            }
            { "group"
                { "name" = "G2" }
            }
            { "user"
                { "name" = "tiger" }
                { "password" = "$6$k6y3o.eP$JlKBx(...)xHSwRv6J.C0/D7cV91" }
                { "groups"
                    { "group" = "G1" }
                }
            }
            { "user"
                { "name" = "scott" }
                { "insecure-password" = "elgato" }
                { "groups"
                    { "group" = "G1" }
                    { "group" = "G2" }
                }
            }
            { "user"
                { "name" = "xdb" }
                { "insecure-password" = "hello" }
                { "groups"
                    { "group" = "G2" }
                }
            }
    }
