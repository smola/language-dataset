/*
 * redis_client.pmod -- Charlotte Koch <dressupgeekout@gmail.com>
 *
 * TODO
 *    - pipelining support
 *    - asynchronous API?
 *    - monitor command?
 *    - settle on pub/sub behavior
 *    - figure out the "real" way to do error handling in Pike
 */

class Redis {
  constant REDIS_CLIENT_PMOD_VERSION = "0.0.0";
  private constant DEFAULT_HOST = "127.0.0.1";
  private constant DEFAULT_PORT = 6379;

  string host, unix_path;
  int port;
  Stdio.FILE socket;

  private int(0..1) pipeline;
  private string pipeline_s;

  /*
   * Creates a new object which represents a connection to a Redis server.
   * Possibly throws an error if a socket couldn't be created. By default,
   * command pipelining is turned off.
   *
   * XXX This won't resolve host names, it only accepts IP addresses. What
   * is Pike's way of doing that?
   */
  void
  create(string|void _host, int|void _port, string|void unix_socket_path)
  {
    host = _host ? _host : DEFAULT_HOST;
    port = _port ? _port : DEFAULT_PORT;
    unix_path = unix_socket_path;
    pipeline = 0;
    socket = Stdio.FILE();
    if (!unix_path)
      if (!socket->open_socket()) error("ERROR can't open socket: ");
    else
      if (!socket->open_socket(0, 0, "SOCK_STREAM")) error("ERROR can't open socket: ");
    socket->set_blocking();
  }


  /*
   * Returns true if the connection was successfully made, otherwise we
   * return false and throw an error.
   */
  int(0..1)
  connect()
  {
    if (!unix_path) { //connecting over TCP
      if (socket->connect(host, port)) {
        return 1;
      } else {
        error("ERROR can't connect to %s:%d: ", host, port);
        return 0;
      }
    } else { //connecting over a unix domain socket
      if (socket->connect_unix(unix_path)) {
        return 1;
      } else {
        error("ERROR can't connect to %s: ", unix_path);
        return 0;
      }
    }
  }


  /*
   * Closes the connection to the Redis server.
   */
  void
  disconnect()
  {
    socket->close();
  }

  void
  begin_pipeline()
  {
    werror("WARNING begin_pipeline() isn't implemented yet\n");
    //pipeline = true;
    //pipeline_s = "";
  }

  mixed  
  end_pipeline()
  {
    werror("WARNING end_pipeline() isn't implemented yet\n");
    //pipeline = false;
    //socket->write(pipeline_s);
    //pipeline_s = "";
    //string reply = socket->gets();
    //return reply;
  }

  /*
   * The abstract "send" method. It sends instructions in Redis' line
   * protocol and blocks for a response. When the response comes, this
   * method returns it as a value casted to the most useful type. I suppose
   * if you want a string (Redis uses strings to represent floats, for
   * example) then you can just cast the result yourself.
   *
   * XXX Accommodate for pipelining.
   */
  mixed
  send(string name, mixed ... args)
  {
    string reply_type;
    mixed response;
    int i;

    socket->write("*%d\r\n", sizeof(args) + 1); 
    socket->write("$%d\r\n", sizeof(name));
    socket->write("%s\r\n", name); 

    for (int i = 0; i < sizeof(args); i++) {
      string arg_s = (string)args[i];
      socket->write("$%d\r\n", sizeof(arg_s));
      socket->write("%s\r\n", arg_s);
    }

    reply_type = socket->read(1);
    socket->unread(reply_type);

    switch (reply_type) {
    case "+": // Status reply
      // By definition, status and error replies are only 1 line long.
      response = replace(socket->gets()[1..], "\r\n", "");
      break;
    case "-": // Error reply
      error("ERROR %s: ", replace(socket->gets()[1..], "\r\n", ""));
      break;
    case ":": // Integer reply
      sscanf(socket->gets(), ":%d\r\n", response);
      break;
    case "$": // Bulk reply
      // Make sure the length isn't -1 -- used for errors sometimes.  If
      // that happens, the Ruby binding sends the nil object, not the string
      // "(nil)", nor does it ever raise an error. So we send 0 here.
      sscanf(socket->gets(), "$%d\r\n", i);

      if (i < 0) {
        response = 0;
      } else {
        response = socket->read(i + 2); // 2 for \r\n
      }

      break;
    case "*": // Multi-bulk reply
      response = ({});
      int a_len;

      sscanf(socket->gets(), "*%d\r\n", a_len);

      for (i = 0; i < a_len; i++) {
        socket->gets(); // Skip strlen response
        response = Array.push(response, replace(socket->gets(), "\r\n", "")); 
      }
      break;
    default: // NOTREACHED ever (hopefully)
      error("ERROR redis protocol error: ");
    } /* switch (reply_type) */

    return response;
  } /* send() */

  /* */

  int(0..) append(string key, string value) {
    return send("APPEND", key, value);
  }

  string auth(string password) {
    return send("AUTH", password);
  }

  string bgrewriteaof() {
    return send("BGREWRITEAOF");
  }

  string bgsave() {
    return send("BGSAVE");
  }

  int(0..) bitcount(string key, int|void start, int|void end) {
    return send("BITCOUNT", key, start, end);
  }

  int(0..) bitop(string op, string destkey, mixed ... keys) {
    return send("BITOP", op, destkey, @keys);
  }

  array blpop(mixed ... args) {
    return send("BLPOP", @args);
  }

  array brpop(mixed ... args) {
    return send("BRPOP", @args);
  }

  mixed brpoplpush(string src, string dest, int(0..) timeout) {
    return send("BRPOPLPUSH", src, dest, timeout);
  }

  mixed client(string subcommand, mixed ... args) {
    return send("CLIENT", subcommand, @args);
  }

  string client_kill(string ip_port) {
    return client("KILL", ip_port);
  }

  string client_list() {
    return client("LIST");
  }

  string client_getname() {
    return client("GETNAME");
  }

  string client_setname(string cxn_name) {
    return client("SETNAME", cxn_name);
  }

  mixed config(string subcommand, mixed ... args) {
    return send("CONFIG", subcommand, @args);
  }

  array(string) config_get(string param) {
    return config("GET", param);
  }

  string config_rewrite() {
    return config("REWRITE");
  }

  string config_set(string param, mixed value) {
    return config("SET", param, value);
  }

  string config_resetstat() {
    return config("RESETSTAT");
  }

  int(0..) dbsize() {
    return send("DBSIZE");
  }

  /*
   * DRY front-end to all of the DEBUG subcommands.
   */
  mixed debug(string subcommand, mixed ... args) {
    return send("DEBUG", subcommand, @args);
  }

  string debug_object(string key) {
    return debug("OBJECT", key);
  }
  
  string debug_segfault() {
    return debug("SEGFAULT");
  }

  int decr(string key) {
    return send("DECR", key);
  }

  int decrby(string key, int decrement) {
    return send("DECRBY", key, decrement);
  }

  int del(string ... keys) {
    return send("DEL", @keys);
  }

  string discard() {
    return send("DISCARD");
  }

  string dump(string key) {
    return send("DUMP", key);
  }

  string echo(string message) {
    return send("ECHO", message);
  }

  mixed eval(string script, int(0..) numkeys, mixed ... keys_and_args) {
    return send("EVAL", script, numkeys, @keys_and_args);
  }

  mixed evalsha(string sha1, int(0..) numkeys, mixed ... keys_and_args) {
    return send("EVALSHA", sha1, numkeys, @keys_and_args);
  }

  array exec() {
    return send("EXEC");
  }

  int(0..1) exists(string key) {
    return send("EXISTS", key);
  }

  int(0..1) expire(string key, int seconds) {
    return send("EXPIRE", key, seconds);
  }

  int(0..1) expireat(string key, int timestamp) {
    return send("EXPIREAT", key, timestamp);
  }

  string flushall() {
    return send("FLUSHALL");
  }

  string flushdb() {
    return send("FLUSHDB");
  }

  string get(string key) {
    return send("GET", key);
  }

  int getbit(string key, int offset) {
    return send("GETBIT", key, offset);
  }

  string getrange(string key, int start, int end) {
    return send("GETRANGE", key, start, end);
  }

  string getset(string key, string|int|float value) {
    return send("GETSET", key, value);
  }

  int hdel(string key, string ... fields) {
    return send("HDEL", key, @fields);
  }

  int(0..1) hexists(string key, string field) {
    return send("HEXISTS", key, field);
  }

  mixed hget(string key, string field) {
    return send("HGET", key, field);
  }

  array(mixed) hgetall(string key) {
    return send("HGETALL", key);
  }

  int hincrby(string key, string field, int increment) {
    return send("HINCRBY", key, field, increment);
  }

  float hincrbyfloat(string key, string field, float increment) {
    return send("HINCRBYFLOAT", key, field, increment);
  }

  array hkeys(string key) {
    return send("HKEYS", key);
  }

  int hlen(string key) {
    return send("HLEN", key);
  }

  array hmget(string key, string ... fields) {
    return send("HMGET", key, @fields);
  }

  string hmset(string key, mixed ... fields_and_values) {
    return send("HMSET", key, @fields_and_values);
  }

  int(0..1) hset(string key, string field, mixed value) {
    return send("HSET", key, field, value);
  }

  int(0..1) hsetnx(string key, string field, mixed value) {
    return send("HSETNX", key, field, value);
  }

  array hvals(string key) {
    return send("HVALS", key);
  }

  int incr(string key) {
    return send("INCR", key);
  }

  int incrby(string key, int increment) {
    return send("INCRBY", key, increment);
  }

  float incrbyfloat(string key, float increment) {
    return send("INCRBYFLOAT", key, increment);
  }

  string info(string|void section) {
    return send("INFO", section);
  }

  array(string) keys(string pattern) {
    return send("KEYS", pattern);
  }

  int lastsave() {
    return send("LASTSAVE");
  }

  mixed lindex(string key, int index) {
    return send("LINDEX", key, index);
  }

  int linsert(string key, string where, string pivot, string value) {
    return send("LINSERT", key, where, pivot, value);
  }

  int llen(string key) {
    return send("LLEN", key);
  }

  mixed lpop(string key) {
    return send("LPOP", key);
  }

  int lpush(string key, mixed ... values) {
    return send("LPUSH", key, @values);
  }

  int lpushx(string key, mixed value) {
    return send("LPUSHX", key, value);
  }

  array lrange(string key, int start, int stop) {
    return send("LRANGE", key, start, stop);
  }

  int lrem(string key, int count, mixed value) {
    return send("LREM", key, count, value);
  }

  string lset(string key, int index, mixed value) {
    return send("LSET", key, index, value);
  }

  string ltrim(string key, int(0..) start, int(0..) stop) {
    return send("LTRIM", key, start, stop);
  }

  array mget(string ... keys) {
    return send("MGET", @keys);
  }

  string migrate(string host,
                 int(0..) port,
                 string destination_db,
                 int(0..) timeout,
                 string ... copy_and_or_replace)
  {
    return send(
      "MIGRATE", host, port, destination_db, timeout, @copy_and_or_replace
    );
  }

  // Not sure how to deal with this yet
  void monitor()
  {
    werror("WARNING monitor() not implemented yet\n");
  }

  int(0..1) move(string key, string db) {
    return send("MOVE", key, db);
  }

  string mset(mixed ... keys_and_values) {
    return send("MSET", @keys_and_values);
  }

  int(0..1) msetnx(mixed ... keys_and_values) {
    return send("MSETNX", @keys_and_values);
  }

  string multi() {
    return send("MULTI");
  }

  /*
   * DRY front-end to the OBJECT subcommands.
   *
   * NOTE. Unlike other Redis commands with subcommands, the DRY part here
   * is prefixed with an underscore. That's because 'object' is a keyword in
   * Pike. Just be aware.
   */
  mixed _object(string subcommand, mixed ... args) {
    return send("OBJECT", subcommand, @args);
  }

  int object_refcount(string key) {
    return _object("REFCOUNT", key);
  }

  string object_encoding(string key) {
    return _object("ENCODING", key);
  }

  int object_idletime(string key) {
    return _object("IDLETIME", key);
  }

  int(0..1) persist(string key) {
    return send("PERSIST", key);
  }

  int(0..1) pexpire(string key, int(0..) ms) {
    return send("PEXPIRE", key, ms);
  }

  int(0..1) pexpireat(string key, int(0..)mstime) {
    return send("PEXPIREAT", key, mstime);
  }

  string ping() {
    return send("PING");
  }

  string psetex(string key, int(0..)ms, mixed value) {
    return send("PSETEX", key, ms, value);
  }

  array psubscribe(string ... patterns) {
    return send("PSUBSCRIBE", @patterns);
  }

  mixed pubsub(string subcommand, mixed ... args) {
    return send("PUBSUB", subcommand, @args);
  }

  int pttl(string key) {
    return send("PTTL", key);
  }

  int(0..) publish(string channel, string msg) {
    return send("PUBLISH", channel, msg);
  }

  array punsubscribe(string ... patterns) {
    return send("PUNSUBSCRIBE", @patterns);
  }

  string quit() {
    return send("QUIT");
  }

  string randomkey() {
    return send("RANDOMKEY");
  }

  string rename(string key, string newkey) {
    return send("RENAME", key, newkey);
  }

  int(0..1) renamenx(string key, string newkey) {
    return send("RENAMENX", key, newkey);
  }

  string restore(string key, int(0..) ttl, string serialized_value) {
    return send("RESTORE", key, ttl, serialized_value);
  }

  mixed rpop(string key) {
    return send("RPOP", key);
  }

  mixed rpoplpush(string src, string dest) {
    return send("RPOPLPUSH", src, dest);
  }

  int rpush(string key, mixed ... values) {
    return send("RPUSH", key, @values);
  }

  int rpushx(string key, mixed value) {
    return send("RPUSHX", key, value);
  }

  int(0..) sadd(string key, mixed ... members) {
    return send("SADD", key, @members);
  }

  string save() {
    return send("SAVE");
  }

  int(0..) scard(string key) {
    return send("SCARD", key);
  }

  /*
   * DRY front-end to all of the SCRIPT subcommands.
   */
  mixed script(string subcommand, mixed ... args) {
    return send("SCRIPT", subcommand, @args);
  }

  array script_exists(string ... scripts) {
    return script("EXISTS", @scripts);
  }

  string script_flush() {
    return script("FLUSH");
  }

  string script_kill() {
    return script("KILL");
  }

  string script_load(string _script) {
    return script("LOAD", _script);
  }

  array sdiff(string ... keys) {
    return send("SDIFF", @keys);
  }

  int(0..) sdiffstore(string dest, string ... keys) {
    return send("SDIFFSTORE", dest, @keys);
  }

  string select(int(0..) index) {
    return send("SELECT", index);
  }

  /*
   * Man, they made this simple command a lot more difficult. Thank goodness
   * Pike supports variadic functions out of the box...
   */
  string set(string key, string value, mixed ... args) {
    return send("SET", key, value, @args);
  }

  int(0..1) setbit(string key, int(0..) offset, int(0..1) value) {
    return send("SETBIT", key, offset, value);
  }

  string setex(string key, int(0..) seconds, mixed value) {
    return send("SETEX", key, seconds, value);
  }

  int(0..1) setnx(string key, mixed value) {
    return send("SETNX", key, value);
  }

  int(0..) setrange(string key, int(0..) offset, mixed value) {
    return send("SETRANGE", key, offset, value);
  }

  string|void shutdown(string|void save_behavior) {
    return send("SHUTDOWN", save_behavior);
  }

  array sinter(string ... keys) {
    return send("SINTER", @keys);
  }

  int sinterstore(string dest, string ... keys) {
    return send("SINTERSTORE", dest, @keys);
  }

  int(0..1) sismember(string key, mixed member) {
    return send("SISMEMBER", key, member);
  }

  /*
   * Note the weird argument types. That's because it's possible for "host"
   * and port" to equal "NO" and "ONE", respectively.
   */
  string slaveof(string host, int(0..)|string port) {
    return send("SLAVEOF", host, port);
  }

  mixed|void slowlog(string subcommand, mixed|void arg) {
    return send("SLOWLOG", subcommand, arg);
  }

  array smembers(string key) {
    return send("SMEMBERS", key);
  }

  int(0..1) smove(string src, string dest, mixed member) {
    return send("SMOVE", src, dest, member);
  }

  array sort(string key, mixed ... args) {
    return send("SORT", key, @args);
  }

  mixed spop(string key) {
    return send("SPOP", key);
  }

  mixed srandmember(string key, int(1..)|void count) {
    return send("SRANDMEMBER", key, count);
  }

  int(0..) srem(string key, mixed ... members) {
    return send("SREM", key, @members);
  }

  int(0..) strlen(string key) {
    return send("STRLEN", key);
  }

  /*
   * SUBSCRIBE actually returns a multi-bulk reply with three elements: (1)
   * the word "subscribe", (2) the name of the channel to which we just
   * subscribed, and (3) not sure, but probably a boolean indicating success
   * ("1") or failure ("0"). The last element literally is a number, as in
   * ":42", not just "42". Why isn't this documented on redis.io?
   */
  array subscribe(string ... channels) {
    return send("SUBSCRIBE", @channels);
  }

  array sunion(string ... keys) {
    return send("SUNION", @keys);
  }

  int(0..) sunionstore(string dest, string ... keys) {
    return send("SUNIONSTORE", dest, @keys);
  }

  /*
   * What is the proper return value for the SYNC command? It *CAN* fail, so
   * I suspect a bool would be best. It seems to return an integer > 0, though.
   * On the other hand, it's just an internal command...
   */
  /* void sync(); */

  array(int) time() {
    array(string) res_s = send("TIME");
    array(int) res_i = ({});
    foreach (res_s, string s) res_i = Array.push(res_i, (int)s);
    return res_i;
  }

  int ttl(string key) {
    return send("TTL", key);
  }

  string type(string key) {
    return send("TYPE", key);
  }

  /*
   * UNSUBSCRIBE actually returns a multi-bulk reply of 3 elements, like
   * SUBSCRIBE, it seems.
   */
  array unsubscribe(string ... channels) {
    return send("UNSUBSCRIBE", @channels);
  }

  string unwatch() {
    return send("UNWATCH");
  }

  string watch(string ... keys) {
    return send("WATCH", @keys);
  }

  int(0..) zadd(string key, mixed ... scores_and_members) {
    return send("ZADD", key, @scores_and_members);
  }

  int(0..) zcard(string key) {
    return send("ZCARD", key);
  }

  int(0..) zcount(string key, int min, int max) {
    return send("ZCOUNT", key, min, max);
  }

  float zincrby(string key, int incr, mixed member) {
    return send("ZINCRBY", key, incr, member);
  }

  int(0..) zinterstore(string dest, int(0..) numkeys, mixed ... args) {
    return send("ZINTERSTORE", dest, numkeys, @args);
  }

  array zrange(string key, int start, int stop, string|void withscores) {
    return send("ZRANGE", key, start, stop, withscores);
  }

  array zrangebyscore(string key,
                      string|int(0..) min,
                      string|int(0..) max,
                      mixed ... args)
  {
    return send("ZRANGEBYSCORE", key, min, max, @args);
  }

  int(0..)|string zrank(string key, mixed member) {
    return send("ZRANK", key, member);
  }

  int(0..) zrem(string key, mixed ... members) {
    return send("ZREM", key, @members);
  }

  int(0..) zremrangebyrank(string key, int start, int stop) {
    return send("ZREMRANGEBYRANK", key, start, stop);
  }

  int(0..) zremrangebyscore(string key, int|string min, int|string max) {
    return send("ZREMRANGEBYSCORE", key, min, max);
  }

  array zrevrange(string key, int start, int stop, string|void withscores) {
    return send("ZREVRANGE", key, start, stop, withscores);
  }

  array zrevrangebyscore(string key,
                         string|int max,
                         string|int min,
                         mixed ... args)
  {
    return send("ZREVRANGEBYSCORE", key, max, min, @args);
  }

  int(0..)|string zrevrank(string key, mixed member) {
    return send("ZREVRANK", key, member);
  }

  float zscore(string key, mixed member) {
    return send("ZSCORE", key, member);
  }

  int(0..) zunionstore(string dest, int(0..)numkeys, mixed ... args) {
    return send("ZUNIONSTORE", dest, numkeys, @args);
  }
} /* class Redis */

