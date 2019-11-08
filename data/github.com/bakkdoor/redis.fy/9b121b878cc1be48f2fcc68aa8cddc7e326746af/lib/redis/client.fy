class Redis {
  class Client {
    """
    Redis Client class.

    Example:
          redis = Redis Client new # defaults to localhost and default Redis port
          redis set: ('library_name, \"redis.fy\")
          redis get: 'library_name  # => \"redis.fy\"
    """

    DefaultHost = "localhost"
    DefaultPort = 6379
    read_slots: ('db, 'password)
    read_write_slot: 'connection_retries

    def self redis_commands: commands {
      commands each: |cmd| {
        command_name = cmd to_s substitute: "_" with: " "
        define_method: "#{cmd}:" with: |args| {
          call: (args to_a unshift: command_name)
        }
      }
    }

    redis_commands: ['append, 'auth, 'bgrewriteaof, 'bgsave,
    'bitcount, 'bitop, 'blpop, 'brpop, 'brpoplpush, 'config_get,
    'config_set, 'config_resetstat, 'dbsize, 'debug_object,
    'debug_segfault, 'decr, 'decrby, 'del, 'discard, 'dump, 'echo,
    'eval, 'exec, 'exists, 'expire, 'expireat, 'flushall, 'flushdb,
    'get, 'getbit, 'getrange, 'getset, 'hdel, 'hexists, 'hget,
    'hgetall, 'hincrby, 'hincrbyfloat, 'hkeys, 'hlen, 'hmget, 'hmset,
    'hset, 'hsetnx, 'hvals, 'incr, 'incrby, 'incrbyfloat, 'info,
    'keys, 'lastsave, 'lindex, 'linsert, 'llen, 'lpop, 'lpush,
    'lpushx, 'lrange, 'lrem, 'lset, 'ltrim, 'mget, 'migrate, 'monitor,
    'move, 'mset, 'msetnx, 'multi, 'object, 'persist, 'pexpire,
    'pexpireat, 'ping, 'psetex, 'psubscribe, 'pttl, 'publish,
    'punsubscribe, 'quit, 'randomkey, 'rename, 'renamenx, 'restore,
    'rpop, 'rpoplpush, 'rpush, 'rpushx, 'sadd, 'save, 'scard,
    'script_exists, 'script_flush, 'script_kill, 'script_load, 'sdiff,
    'sdiffstore, 'select, 'set, 'setbit, 'setex, 'setnx, 'setrange,
    'shutdown, 'sinter, 'sinterstore, 'sismember, 'slaveof, 'slowlog,
    'smembers, 'smove, 'sort, 'spop, 'srandmember, 'srem, 'strlen,
    'subscribe, 'sunion, 'sunionstore, 'sync, 'time, 'ttl, 'type,
    'unsubscribe, 'unwatch, 'watch, 'zadd, 'zcard, 'zcount, 'zincrby,
    'zinterstore, 'zrange, 'zrangebyscore, 'zrank, 'zrem,
    'zremrangebyrank, 'zremrangebyscore, 'zrevrange,
    'zrevrangebyscore, 'zrevrank, 'zscore, 'zunionstore]

    def initialize: host (DefaultHost) port: port (DefaultPort) db: @db (nil) password: @password (nil) {
      @connection = Connection new: host port: port
      @thread_safe = true
      @mutex = Mutex new
      @channel_handlers = <[]>
      @connection_retries = 2
      connect
    }

    def initialize: host db: db password: password (nil) {
      initialize: host port: DefaultPort db: db password: password
    }

    def initialize: host password: password {
      initialize: host port: DefaultPort db: nil password: password
    }

    def disable_thread_safety! {
      @thread_safe = false
      def @mutex synchronize: block {
        block call
      }
    }

    def connect {
      unless: connected? do: {
        @connection open
        { call: ['auth, @password] } if: @password
        { call: ['select, @db] } if: @db
      }
    }

    def reconnect {
      disconnect
      connect
    }

    def disconnect {
      @connection close
    }

    def connected? {
      @connection open?
    }

    def thread_safe? {
      @thread_safe
    }

    def transaction: block {
      try {
        call: 'multi
        block call: [self]
        call: 'exec
      } finally {
        call: 'discard
      }
    }

    def call: command {
      command = command to_a
      cmd_name = command first

      match cmd_name {
        case 'hgetall -> return handle_hgetall: command
        case 'keys -> return handle_keys: command
        case 'subscribe -> return handle_subscribe: command
      }

      reply = command: command

      match cmd_name {
        case 'smove -> boolean: reply

        case 'sadd ->
          match command skip: 2 . size {
            case 1 -> boolean: reply
            case _ -> reply
          }

        case 'srem ->
          match command skip: 2 . size {
            case 1 -> boolean: reply
            case _ -> reply
          }

        case _ -> reply
      }
    }

    # special commands handled differently

    def [command] {
      call: $ command to_a
    }

    def handle_hgetall: command {
      reply = command: command
      match reply {
        case Array ->
          h = <[]>
          reply in_groups_of: 2 . each: |pair| {
            field, value = pair
            h[field]: value
          }
          h
        case _ -> reply
      }
    }

    def handle_keys: command {
      reply = command: command
      match reply {
        case String -> reply split: " "
        case _ -> reply
      }
    }

    def handle_subscribe: command {
      channel_handlers = command second
      channel_handlers each: |chan block| {
        chan = chan to_s
        { @channel_handlers[chan]: [] } unless: $ @channel_handlers[chan]
        @channel_handlers[chan] << block
      }

      command = command second keys unshift: 'subscribe
      reply = command: $ command

      { start_subscribe_thread } unless: @subscribe_thread

      reply
    }

    # private

    def command: command {
      @mutex synchronize: {
        @connection_retries times_try: {
          @connection send_command: command
          @connection read_reply
        } retry_with: { reconnect }
      }
    }

    def boolean: reply {
      reply to_i == 1
    }

    def start_subscribe_thread {
      @subscribe_thread = Thread new: {
        loop: {
          @mutex synchronize: {
            reply = @connection_retries times_try: {
              @connection read_reply
            } retry_with: { reconnect }
            if: (reply first == "message") then: {
              type, chan, message = reply
              @channel_handlers[chan] each: @{ call: [message] }
            } else: {
              @subscribe_thread = nil
              *stderr* println: "Expected message publish reply. Got: #{reply inspect}"
              break
            }
          }
        }
      }
    }
  }
}