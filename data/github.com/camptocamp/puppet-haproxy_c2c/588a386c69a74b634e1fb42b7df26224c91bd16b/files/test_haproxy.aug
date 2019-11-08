(*
    Test for haproxy lens 

    Author: Bryon Roche <bryon@gogrid.com>

    About: License
    This file is licensed under the LGPL

    We test a config parse, then a section removal, then a section add.
*)

module Test_haproxy =

   let conf = "# this config needs haproxy-1.1.28 or haproxy-1.2.1

global
    #log 127.0.0.1 local0
    #log 127.0.0.1 local1 notice
    #log loghost local0 info
    log 192.168.7.22 local0
    maxconn 4096
    #chroot /usr/share/haproxy
    user haproxy
    group haproxy
    daemon
    #debug
    #quiet

defaults
    log global
    mode http
    option httplog
    option dontlognull
    retries 3
    option redispatch
    maxconn 2000
    contimeout 5000
    clitimeout 50000
    srvtimeout 50000


listen proxy1 0.0.0.0:80
    mode http
    log global
    cookie SERVERID insert indirect
    balance roundrobin
    redispatch
    option forwardfor
    no option httplog
    option dontlognull
    maxconn 60000
    retries 3
    grace 3000
    server lb1 192.168.7.122:80 cookie cookie1 inter 300
    server lb2 192.168.7.123:80 cookie cookie1 inter 300
    server lb3 192.168.7.124:80 cookie cookie1 inter 300
"

    let confrmtest ="# this config needs haproxy-1.1.28 or haproxy-1.2.1

global
    #log 127.0.0.1 local0
    #log 127.0.0.1 local1 notice
    #log loghost local0 info
    log 192.168.7.22 local0
    maxconn 4096
    #chroot /usr/share/haproxy
    user haproxy
    group haproxy
    daemon
    #debug
    #quiet

listen proxy1 0.0.0.0:80
    mode http
    log global
    cookie SERVERID insert indirect
    balance roundrobin
    redispatch
    option forwardfor
    no option httplog
    option dontlognull
    maxconn 60000
    retries 3
    grace 3000
    server lb1 192.168.7.122:80 cookie cookie1 inter 300
    server lb2 192.168.7.123:80 cookie cookie1 inter 300
    server lb3 192.168.7.124:80 cookie cookie1 inter 300
"

    let confredeftest ="# this config needs haproxy-1.1.28 or haproxy-1.2.1

global
    #log 127.0.0.1 local0
    #log 127.0.0.1 local1 notice
    #log loghost local0 info
    log 192.168.7.22 local0
    maxconn 4096
    #chroot /usr/share/haproxy
    user haproxy
    group haproxy
    daemon
    #debug
    #quiet

defaults
 log global
listen proxy1 0.0.0.0:80
    mode http
    log global
    cookie SERVERID insert indirect
    balance roundrobin
    redispatch
    option forwardfor
    no option httplog
    option dontlognull
    maxconn 60000
    retries 3
    grace 3000
    server lb1 192.168.7.122:80 cookie cookie1 inter 300
    server lb2 192.168.7.123:80 cookie cookie1 inter 300
    server lb3 192.168.7.124:80 cookie cookie1 inter 300
"

    test Haproxy.lns get conf =
        { "#comment" = "this config needs haproxy-1.1.28 or haproxy-1.2.1" }
        {}
        { "global" 
            { "#comment" = "log 127.0.0.1 local0" }
            { "#comment" = "log 127.0.0.1 local1 notice" }
            { "#comment" = "log loghost local0 info" }
            { "log" = "192.168.7.22 local0" }
            { "maxconn" = "4096" }
            { "#comment" = "chroot /usr/share/haproxy" }
            { "user" = "haproxy" }
            { "group" = "haproxy" }
            { "daemon" }
            { "#comment" = "debug" }
            { "#comment" = "quiet" }
            {}
        }
        { "defaults" 
            { "log" = "global" }
            { "mode" = "http" }
            { "option httplog" }
            { "option dontlognull" }
            { "retries" = "3" }
            { "option redispatch" }
            { "maxconn" = "2000" }
            { "contimeout" = "5000" }
            { "clitimeout" = "50000" }
            { "srvtimeout" = "50000" }
            {}
            {}
        }
        { "listen" 
            { "proxy1" = "0.0.0.0:80"
                { "mode" = "http" }
                { "log" = "global" }
                { "cookie" = "SERVERID insert indirect" }
                { "balance" = "roundrobin" }
                { "redispatch" }
                { "option forwardfor" }
                { "no option httplog" }
                { "option dontlognull" }
                { "maxconn" = "60000" }
                { "retries" = "3" }
                { "grace" = "3000" }
                { "server" = "lb1 192.168.7.122:80 cookie cookie1 inter 300" }
                { "server" = "lb2 192.168.7.123:80 cookie cookie1 inter 300" }
                { "server" = "lb3 192.168.7.124:80 cookie cookie1 inter 300" }
            }
        }

    test Haproxy.lns put conf after rm "/defaults" = confrmtest

    test Haproxy.lns put confrmtest after insa "defaults" "/global" ; set "/defaults/log" "global" = confredeftest




