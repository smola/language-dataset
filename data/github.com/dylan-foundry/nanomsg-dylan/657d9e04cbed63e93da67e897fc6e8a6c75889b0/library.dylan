module: dylan-user
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define library nanomsg
  use dylan;
  use common-dylan;
  use io;
  use c-ffi;

  export nanomsg;
end library;

define module nanomsg
  use common-dylan, exclude: { format-to-string };
  use c-ffi;
  use dylan-direct-c-ffi;
  use streams;

  export
    nn-errno,
    nn-strerror,
    nn-symbol,
    nn-term,
    nn-socket,
    nn-close,
    nn-setsockopt,
    nn-getsockopt,
    nn-bind,
    nn-connect,
    nn-shutdown,
    nn-allocmsg,
    nn-freemsg,
    nn-send,
    // nn-sendmsg,
    nn-recv,
    // nn-recvmsg,
    nn-device;

  export
    <nn-error>,
    nn-error-status,
    nn-error-message;

  export
    $NN-VERSION-AGE,
    $NN-VERSION-CURRENT,
    $NN-VERSION-REVISION,
    $EACCESS,
    $EADDRINUSE,
    $EADDRNOTAVAIL,
    $EAFNOSUPPORT,
    $EAGAIN,
    $EBADF,
    $EFAULT,
    $EFSM,
    $EINTR,
    $EINVAL,
    $EMFILE,
    $ENAMETOOLONG,
    $ENODEV,
    $ENOMEM,
    $ENOPROTOOPT,
    $ENOTSUP,
    $EPROTONOSUPPORT,
    $ETERM,
    $ETIMEDOUT,
    $AF-SP,
    $AF-SP-RAW,
    $NN-MSG,
    $NN-DOMAIN,
    $NN-PROTOCOL,
    $NN-SOCKADDR-MAX,
    $NN-PAIR,
    $NN-INPROC,
    $NN-IPC,
    $NN-TCP,
    $NN-PUB,
    $NN-SUB,
    $NN-REP,
    $NN-REQ,
    $NN-PUSH,
    $NN-PULL,
    $NN-SURVEYOR,
    $NN-RESPONDENT,
    $NN-BUS,
    $NN-SOL-SOCKET,
    $NN-LINGER,
    $NN-SNDBUF,
    $NN-RCVBUF,
    $NN-SNDTIMEO,
    $NN-RCVTIMEO,
    $NN-RECONNECT-IVL,
    $NN-RECONNECT-IVL-MAX,
    $NN-SNDPRIO,
    $NN-SNDFD,
    $NN-RCVFD,
    $NN-SUB-SUBSCRIBE,
    $NN-SUB-UNSUBSCRIBE,
    $NN-REQ-RESEND-IVL,
    $NN-SURVEYOR-DEADLINE,
    $NN-DONTWAIT,
    $NN-TCP-NODELAY,
    $NN-IPV4ONLY,
    $NN-SOCKET-NAME;
end module;
