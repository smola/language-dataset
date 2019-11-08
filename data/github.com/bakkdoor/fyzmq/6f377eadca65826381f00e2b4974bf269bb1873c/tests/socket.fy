require: "../lib/fyzmq"

FancySpec describe: ZMQ Socket with: {
  before_each: {
    @context = ZMQ Context new
  }

  it: "creates a new ZMQ Socket from a ZMQ Context" with: 'socket: when: {
    sock = @context socket: ZMQ PULL
    sock is_a?: ZMQ Socket . is: true
  }

  it: "creates a pull and push socket and sends data" with: 'socket: when: {
    pull = @context socket: ZMQ PULL
    push = @context socket: ZMQ PUSH

    pull bind: "ipc://127.0.0.1:4567"
    push connect: "ipc://127.0.0.1:4567"

    messages = ["hello, world!", ('hello, 'world), [1,2,3,4]]
    messages each: |msg| {
      msg = msg to_s
      push send: msg
      pull recv is: msg
    }
  }
}