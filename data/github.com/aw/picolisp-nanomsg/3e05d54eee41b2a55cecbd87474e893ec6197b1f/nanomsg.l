# nanomsg.l
#
# The MIT License (MIT)
#
# Copyright (c) 2015-2018 Alexander Williams, Unscramble <license@unscramble.jp>

(setq
  MSG_MAX_SIZE (if (sys "NANOMSG_MAX_SIZE") (format @) 8192) # 8KB should be enough

  *Nanomsg (pack (car (file)) ".lib/libnanomsg.so") )

# ffi-bindings
[de nn-socket (Domain Protocol)
  (native `*Nanomsg "nn_socket" 'I
                                (symbol-val Domain)
                                (symbol-val Protocol) ]

(de nn-close (Sock)
  (native `*Nanomsg "nn_close" 'I Sock) )

[de nn-setsockopt (Sock Level Option Optval Length)
  (native `*Nanomsg "nn_setsockopt" 'I
                                    Sock
                                    (symbol-val Level)
                                    (symbol-val Option)
                                    Optval
                                    Length ]

# Set Length to NIL and nn_getsockopt will automatically calculate the correct size
# returns the result of the call in 'car', value of &buf in 'cdr'
[de nn-getsockopt (Sock Level Option &buf Length)
  (use Buf
    (cons
      (native `*Nanomsg "nn_getsockopt" 'I
                                        Sock
                                        (symbol-val Level)
                                        (symbol-val Option)
                                        (cons 'Buf &buf 0)
                                        Length )
      Buf ]

(de nn-bind (Sock Addr)
  (native `*Nanomsg "nn_bind" 'I Sock Addr) )

(de nn-connect (Sock Addr)
  (native `*Nanomsg "nn_connect" 'I Sock Addr) )

(de nn-shutdown (Sock Endpoint)
  (native `*Nanomsg "nn_shutdown" 'I Sock Endpoint) )

(de nn-send (Sock Msg Length Flags)
  (native `*Nanomsg "nn_send" 'I Sock Msg Length Flags) )

# returns the number of bytes in 'car', msg in 'cdr'
[de nn-recv (Sock &buf Length Flags)
  (use Buf
    (cons
      (native `*Nanomsg "nn_recv" 'I
                                  Sock
                                  (cons 'Buf &buf 0)
                                  Length
                                  Flags )
      Buf ]

(de nn-errno ()
  (native `*Nanomsg "nn_errno" 'I) )

(de nn-strerror (Errnum)
  (native `*Nanomsg "nn_strerror" 'S Errnum) )

(de nn-symbol (Index &value)
  (native `*Nanomsg "nn_symbol" 'S Index &value) )

# returns the number of 'nn_pollfds' structures in 'car', results in 'cdr'
[de nn-poll (&fds Nfds Timeout)
  (use Fds
    (cons
      (native `*Nanomsg "nn_poll" 'I
                                  (list 'Fds &fds)
                                  Nfds
                                  Timeout )
      Fds ]

(de nn-device (Sock1 Sock2)
  (native `*Nanomsg "nn_device" 'I Sock1 Sock2) )

# WARNING: only polls 1 file descriptor specified by Sockfd
#          Level-triggered (single-threaded) only (i.e: does not support EPOLLET|EPOLLONESHOT)
# returns the number of 'nn_pollfds' structures (0 or 1)
[de nn-poll-lt-1 (Sockfd Nfds Timeout)
  (native `*Nanomsg "nn_poll" 'I
                              (list 'NIL
                                    (8 (B B B B I))
                                    (0 . 2)
                                    (0 . 2)
                                    (- Sockfd) )
                              Nfds
                              Timeout ]

# internal
[de fetch-symbols ()
  (let (Index -1 P)
    (make
      (while (nn-symbol (inc 'Index) '(P (4 . I)))
        (link (cons @ P)) ]

(setq *NN_Symbols (fetch-symbols))

(de symbol-val (Symbol)
  (cdr (assoc Symbol *NN_Symbols)) )

(de exit-with-error (Sock Endpoint)
  (when (and Endpoint (ge0 Endpoint)) (nn-shutdown Sock Endpoint))
  (when Sock (nn-close Sock))
  (throw 'InternalError (cons 'NanomsgError (nn-strerror (nn-errno))) ) )

[de exit-with-error-maybe (Dontwait Result Sock)
  (cond ((and (bool Dontwait)
              (= -1 Result)
              (= (nn-errno) (symbol-val "EAGAIN")) )
        T )
        ((= -1 Result) (exit-with-error Sock)) ]

[de make-socket (Addr Type Flag Domain)
  (let Sock (create-socket Type Domain)
    (let Endpoint (make-endpoint Addr Type Flag Sock)
      (check-endpoint Sock Endpoint) ]

[de create-socket (Type Domain)
  (default Domain "AF_SP")
  (prog1
    (nn-socket Domain Type)
    (if (= -1 @)
        (exit-with-error @)
        @ ]

[de make-endpoint (Addr Type Flag Sock)
  (case Type  ("NN_REP"        (nn-bind Sock Addr))
              ("NN_REQ"        (nn-connect Sock Addr))
              ("NN_PUB"        (nn-bind Sock Addr))
              ("NN_SUB"        (nn-connect Sock Addr))
              ("NN_BUS"        (if (= Flag "BIND") (nn-bind Sock Addr) (nn-connect Sock Addr)))
              ("NN_PAIR"       (if (= Flag "BIND") (nn-bind Sock Addr) (nn-connect Sock Addr)))
              ("NN_PULL"       (nn-bind Sock Addr))
              ("NN_PUSH"       (nn-connect Sock Addr))
              ("NN_SURVEYOR"   (nn-bind Sock Addr))
              ("NN_RESPONDENT" (nn-connect Sock Addr)) ]

[de check-endpoint (Sock Endpoint)
  (cond ((not Endpoint)  (exit-with-error Sock))
        ((= -1 Endpoint) (exit-with-error Sock Endpoint))
        (T (cons Sock Endpoint)) ]

[de sub-unsub (Sock Topic Type)
  (let Result
    (cond ((= Type "NN_SUB_SUBSCRIBE")   (nn-setsockopt Sock "NN_SUB" Type Topic (size Topic)))
          ((= Type "NN_SUB_UNSUBSCRIBE") (nn-setsockopt Sock "NN_SUB" Type Topic (size Topic))) )
    (cond ((not Result)  (exit-with-error Sock))
          ((= -1 Result) (exit-with-error Sock))
          (T Result) ]

(de non-blocking-io (Dontwait)
   (when (bool Dontwait) (symbol-val "NN_DONTWAIT")) )


# public REQ/REP
(de rep-bind (Addr Domain)
  (make-socket Addr "NN_REP" NIL Domain) )

(de req-connect (Addr Domain)
  (make-socket Addr "NN_REQ" NIL Domain) )


# public PUB/SUB
(de pub-bind (Addr Domain)
  (make-socket Addr "NN_PUB" NIL Domain) )

(de sub-connect (Addr Domain)
  (make-socket Addr "NN_SUB" NIL Domain) )

(de subscribe (Sock Topic)
  (sub-unsub Sock Topic "NN_SUB_SUBSCRIBE") )

(de unsubscribe (Sock Topic)
  (sub-unsub Sock Topic "NN_SUB_UNSUBSCRIBE") )


# public BUS
(de bus-bind (Addr Domain)
  (make-socket Addr "NN_BUS" "BIND" Domain) )

(de bus-connect (Addr Domain)
  (make-socket Addr "NN_BUS" NIL Domain) )


# public PAIR
(de pair-bind (Addr Domain)
  (make-socket Addr "NN_PAIR" "BIND" Domain) )

(de pair-connect (Addr Domain)
  (make-socket Addr "NN_PAIR" Domain) )


# public PUSH/PULL (PIPELINE)
(de pull-bind (Addr Domain)
  (make-socket Addr "NN_PULL" NIL Domain) )

(de push-connect (Addr Domain)
  (make-socket Addr "NN_PUSH" NIL Domain) )


# public SURVEY
(de survey-bind (Addr Domain)
  (make-socket Addr "NN_SURVEYOR" NIL Domain) )

(de respond-connect (Addr Domain)
  (make-socket Addr "NN_RESPONDENT" NIL Domain) )


# public generic
[de end-sock (Sockpair)
  (nn-shutdown (car Sockpair) (cdr Sockpair))
  (nn-close (car Sockpair)) ]

[de msg-recv (Sock Dontwait)
  (let Result (nn-recv Sock '(`MSG_MAX_SIZE B . `MSG_MAX_SIZE) MSG_MAX_SIZE (non-blocking-io Dontwait) )
    (unless (exit-with-error-maybe Dontwait Result Sock)
      (pack (mapcar char (head (car Result) (cdr Result)))) ]

[de msg-send (Sock Msg Dontwait)
  (let Result (nn-send Sock Msg (size Msg) (non-blocking-io Dontwait) )
    (unless (exit-with-error-maybe Dontwait Result Sock)
      Result ]

(de protocol-bind (Protocol Addr Domain)
  (make-socket Addr (pack "NN_" Protocol) "BIND" Domain) )

(de protocol-connect (Protocol Addr Domain)
  (make-socket Addr (pack "NN_" Protocol) NIL Domain) )
