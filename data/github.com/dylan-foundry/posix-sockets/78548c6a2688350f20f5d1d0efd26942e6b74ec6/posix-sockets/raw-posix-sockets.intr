Module: %posix-sockets
Synopsis: Auto-generated bindings for the POSIX sockets API.
Author: Bruce Mitchener, Jr.
Copyright: See LICENSE file in this distribution.

define simple-C-mapped-subtype <C-buffer-offset> (<C-void*>)
  export-map <machine-word>, export-function: identity;
end;

define class <socket-error> (<simple-error>)
  constant slot socket-error-status :: <integer>,
    required-init-keyword: status:;
  constant slot socket-error-message :: <string>,
    init-keyword: message:,
    init-value: "Unknown error";
end;

// These aren't exported yet.
ignore(socket-error-status);
ignore(socket-error-message);

define inline C-function io-errno
  result val :: <C-int>;
  c-name: "io_errno";
end C-function;

define C-mapped-subtype <socket-status> (<C-int>)
  import-map <integer>,
    import-function:
      method (result :: <integer>) => (checked :: <integer>)
        if ((result < 0) & (result ~= $EAGAIN))
          let errno = io-errno();
          let message = as(<byte-string>, strerror(errno));
          error(make(<socket-error>,
                     status: errno, message: message,
                     format-string: "Socket error (%d): %s",
                     format-arguments: vector(errno, message)))
        else
          result
        end
      end;
end;

define interface
  #include {
      "sys/socket.h",
      "netinet/in.h",
      "netinet/tcp.h",
      "sys/un.h",
      "arpa/inet.h",
      "netdb.h",
      "unistd.h",
      "sys/select.h",
      "string.h",
      "errno.h"
    },
    inline-functions: inline,
    import: {
      "accept",
      "bind",
      "close",
      "connect",
      "freeaddrinfo",
      "gai_strerror",
      "getaddrinfo",
      "gethostname",
      "getnameinfo",
      "getpeername",
      "getsockname",
      "getsockopt",
      "inet_ntop",
      "inet_pton",
      "listen",
      "recv",
      "recvfrom",
      "select",
      "send",
      "sendto",
      "setsockopt",
      "shutdown",
      "socket",
      "struct linger",
      "struct sockaddr",
      "struct sockaddr_in",
      "struct sockaddr_in6",
      "struct sockaddr_storage"
    },
    import: {
      "AF_INET",
      "AF_INET6",
      "AF_UNIX",
      "AF_UNSPEC",
      "AI_ALL",
      "AI_CANONNAME",
      "AI_NUMERICHOST",
      "AI_NUMERICSERV",
      "AI_PASSIVE",
      "AI_V4MAPPED",
      "INET_ADDRSTRLEN",
      "INET6_ADDRSTRLEN",
      "IPPROTO_ICMP",
      "IPPROTO_IP",
      "IPPROTO_IPV6",
      "IPPROTO_RAW",
      "IPPROTO_TCP",
      "IPPROTO_UDP",
      "MSG_OOB",
      "MSG_PEEK",
      "MSG_DONTROUTE",
      "MSG_EOR",
      "MSG_TRUNC",
      "MSG_CTRUNC",
      "MSG_WAITALL",
      "NI_DGRAM",
      "NI_MAXHOST",
      "NI_MAXSERV",
      "NI_NAMEREQD",
      "NI_NOFQDN",
      "NI_NUMERICHOST",
      "NI_NUMERICSERV",
      "PF_INET",
      "PF_INET6",
      "PF_UNSPEC",
      "SO_DEBUG",
      "SO_KEEPALIVE",
      "SO_LINGER",
      "SO_RCVBUF",
      "SO_RCVTIMEO",
      "SO_REUSEADDR",
      "SO_SNDBUF",
      "SO_SNDTIMEO",
      "SOCK_DGRAM",
      "SOCK_RAW",
      "SOCK_STREAM",
      "SOL_SOCKET",
      "SHUT_RD",
      "SHUT_RDWR",
      "SHUT_WR",
      "TCP_KEEPCNT",
      "TCP_KEEPINTVL",
      "TCP_NODELAY"
    },
    import: {
      "strerror",
      "EAGAIN"
    };

  function "getnameinfo" => %getnameinfo;
  function "freeaddrinfo" => %freeaddrinfo;
  function "gai_strerror" => %gai-strerror;
  function "getaddrinfo" => %getaddrinfo;

  function "inet_ntop" => %inet-ntop;

  function "accept" => %accept,
    map-result: <socket-status>;
  function "bind" => %bind,
    map-result: <socket-status>;
  function "close" => %close,
    map-result: <socket-status>;
  function "connect" => %connect,
    map-result: <socket-status>;
  function "gethostname" => %gethostname,
    map-result: <socket-status>;
  function "getpeername" => %getpeername,
    map-result: <socket-status>;
  function "getsockname" => %getsockname,
    map-result: <socket-status>;
  function "getsockopt" => %getsockopt,
    map-result: <socket-status>;
  function "inet_pton" => %inet-pton,
    map-result: <socket-status>;
  function "listen" => %listen,
    map-result: <socket-status>;
  function "recv" => %recv,
    map-argument: { 2 => <C-buffer-offset> },
    map-result: <socket-status>;
  function "recvfrom" => %recvfrom,
    map-argument: { 2 => <C-buffer-offset> },
    map-result: <socket-status>;
  function "select" => %select,
    map-result: <socket-status>;
  function "send" => %send,
    map-argument: { 2 => <C-buffer-offset> },
    map-result: <socket-status>;
  function "sendto" => %sendto,
    map-argument: { 2 => <C-buffer-offset> },
    map-result: <socket-status>;
  function "setsockopt" => %setsockopt,
    map-result: <socket-status>;
  function "socket" => %socket,
    map-result: <socket-status>;
  function "shutdown" => %shutdown,
    map-result: <socket-status>;

  struct "struct linger",
    pointer-type-name: <linger*>;

  struct "struct sockaddr_in",
    pointer-type-name: <sockaddr-in*>;
  struct "struct sockaddr_in6",
    pointer-type-name: <sockaddr-in6*>;
  struct "struct sockaddr_storage",
    pointer-type-name: <sockaddr-storage*>;
end interface;
