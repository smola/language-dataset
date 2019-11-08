use zeromq

/**
   0MQ bindings

   These are distributed under a BSD license. See the 'LICENSE' file for more details.

   :author: Amos Wenger (nddrylliog)
 */

// 0MQ versioning support

zmq_version: extern func (major: Int*, minor: Int*, patch: Int*)

// 0MQ errors

ErrorT: extern cover from error_t
ENOTSUP, EPROTONOSUPPORT, ENOBUFS, ENETDOWN, EADDRINUSE, EADDRNOTAVAIL, ECONNREFUSED, EINPROGRESS: extern ErrorT

// Native 0MQ error codes

EMTHREAD, EFSM, ENOCOMPATPROTO, ETERM: extern ErrorT

/**
   This function retrieves the errno as it is known to 0MQ library. The goal
   of this function is to make the code 100% portable, including where 0MQ
   compiled with certain CRT library (on Windows) is linked to an
   application that uses different CRT library.
 */
zmq_errno: extern func -> Int

/**
   Resolves system errors and 0MQ errors to human-readable string.
 */
zmq_strerror: extern func (errnum: Int) -> Char*

// 0MQ message definition

ZMQ_MAX_VSM_SIZE: extern Int
ZMQ_DELIMITER   : extern Int
ZMQ_VSM         : extern Int
ZMQ_MSG_MORE    : extern Int
ZMQ_MSG_SHARED  : extern Int

MessageStruct: cover from zmq_msg_t {
    content: extern Pointer
    flags, vsm_size: extern UChar
    vsm_data: extern UChar*
}

Message: cover from MessageStruct* {

    new: static func ~empty -> This {
        this := gc_malloc(MessageStruct size)
        zmq_msg_init(this)
        this
    }

    new: static func ~withSize (size: SizeT) -> This {
        this := gc_malloc(MessageStruct size)
        zmq_msg_init_size(this, size)
        this
    }

    new: static func ~withData (data: Pointer, size: SizeT, dealloc: Pointer, hint: Pointer) -> This {
        this := gc_malloc(MessageStruct size)
        zmq_msg_init_data(this, data, size, dealloc, hint)
        this
    }

    close: inline func {
        zmq_msg_close(this)
    }

    move: inline func (src: Message) {
        zmq_msg_move(this, src)
    }

    copy: inline func (src: Message) {
        zmq_msg_copy(this, src)
    }

    data: inline func -> Pointer {
        zmq_msg_data(this)
    }

    size: inline func -> SizeT {
        zmq_msg_size(this)
    }

}

zmq_msg_init: extern func (Message)
zmq_msg_init_size: extern func (Message, SizeT)
zmq_msg_init_data: extern func (Message, Pointer, SizeT, Pointer, Pointer)
zmq_msg_close: extern func (Message)
zmq_msg_move: extern func (Message, Message)
zmq_msg_copy: extern func (Message, Message)
zmq_msg_data: extern func (Message) -> Pointer
zmq_msg_size: extern func (Message) -> SizeT

// 0MQ infrastructure (a.k.a context) initialisation & termination.

Context: cover from Pointer {

    new: static func (ioThreads: Int) -> This {
        zmq_init(ioThreads)
    }

    term: inline func -> Int {
        zmq_term(this)
    }

}

zmq_init: extern func (Int) -> Pointer
zmq_term: extern func (Pointer) -> Int

// 0MQ socket definition

SocketType: enum {
    pair      : extern(ZMQ_PAIR)
    pub       : extern(ZMQ_PUB)
    sub       : extern(ZMQ_SUB)
    req       : extern(ZMQ_REQ)
    rep       : extern(ZMQ_REP)
    xreq      : extern(ZMQ_XREQ)
    xrep      : extern(ZMQ_XREP)
    upstream  : extern(ZMQ_UPSTREAM)
    downstream: extern(ZMQ_DOWNSTREAM)
}

SocketOption: enum {
    hwm         : extern(ZMQ_HWM)
    swap        : extern(ZMQ_SWAP)
    affinity    : extern(ZMQ_AFFINITY)
    identity    : extern(ZMQ_IDENTITY)
    subscribe   : extern(ZMQ_SUBSCRIBE)
    unsubscribe : extern(ZMQ_UNSUBSCRIBE)
    rate        : extern(ZMQ_RATE)
    recoveryIvl : extern(ZMQ_RECOVERY_IVL)
    mcastLoop   : extern(ZMQ_MCAST_LOOP)
    sndBuf      : extern(ZMQ_SNDBUF)
    rcvBuf      : extern(ZMQ_RCVBUF)
    rcvMore     : extern(ZMQ_RCVMORE)
}

SendRecvOption: enum {
    defaults = 0
    noBlock: extern(ZMQ_NOBLOCK)
    sndMore: extern(ZMQ_SNDMORE)
}

Socket: cover from Pointer {

    new: static func (ctx: Pointer, type: SocketType) -> This {
        zmq_socket(ctx, type as Int)
    }

    close: inline func { zmq_close(this) }

    setOption: inline func (option: SocketOption, optval: const Pointer, optvalLength: SizeT) -> Int {
        zmq_setsockopt(this, option as Int, optval, optvalLength)
    }

    getOption: inline func (option: SocketOption, optval: Pointer, optvalLength: SizeT*) -> Int {
        zmq_getsockopt(this, option as Int, optval, optvalLength)
    }

    bind: inline func (addr: String) -> Int {
        zmq_bind(this, addr)
    }

    connect: inline func (addr: String) -> Int {
        zmq_connect(this, addr)
    }

    send: inline func ~noOpts (msg: Message) -> Int {
        send(msg, SendRecvOption defaults)
    }

    send: inline func (msg: Message, opt: SendRecvOption) -> Int {
        zmq_send(this, msg as Pointer, opt as Int)
    }

    recv: inline func ~allocOurselves -> Message {
        msg := Message new()
        recv(msg)
        msg
    }

    recv: inline func ~noOpts (msg: Message) -> Int {
        recv(msg, SendRecvOption defaults)
    }

    recv: inline func (msg: Message, opt: SendRecvOption) -> Int {
        zmq_recv(this, msg as Pointer, opt as Int)
    }

}

zmq_socket: extern func (Pointer, Int) -> Socket
zmq_close: extern func (Pointer) -> Int
zmq_setsockopt: extern func (Pointer, Int, const Pointer, SizeT) -> Int
zmq_getsockopt: extern func (Pointer, Int, Pointer, SizeT*) -> Int
zmq_bind: extern func (Pointer, const Char*) -> Int
zmq_connect: extern func (Pointer, const Char*) -> Int
zmq_send: extern func (Pointer, Message, Int) -> Int
zmq_recv: extern func (Pointer, Message, Int) -> Int

// I/O multiplexing

ZMQ_POLLIN, ZMQ_POLLOUT, ZMQ_POLLERR: extern Int

PollItemStruct: cover from zmq_pollitem_t {
    socket: extern Pointer
    fd: extern Int // not correct on Win32
    events, revents: extern Short
}

PollItem: cover from PollItemStruct* {

}

zmq_poll: extern func (PollItem, Int, Long) -> Int

// Devices - experimental

ZMQ_STREAMER, ZMQ_FORWARDER, ZMQ_QUEUE: extern Int

zmq_device: extern func (Int, Pointer, Pointer) -> Int


