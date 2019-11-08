use "collections"
use "files"
use "json"
use "net"

use "./handlers"

// DEBUG
use "debug"
use "time"

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.set(FileRead).set(FileStat) end
    try
      let stations = JsonLoader.deserialize_file[Station val](
          FilePath(env.root, "data/stations.json", caps))
      let systems = JsonLoader.deserialize_file[System val](
          FilePath(env.root, "data/systems.json", caps))
      let commodities = JsonLoader.deserialize_file[Commodity val](
          FilePath(env.root, "data/commodities.json", caps))
      let prices = PriceLoader.deserialize_prices(
          FilePath(env.root, "data/listings.bin", caps))

      let universe : Universe val = Universe.create(stations, systems, commodities, prices)

      env.out.print("Data loaded: " + systems.size().string() +
          " systems containing " + stations.size().string() +
          " stations selling " + commodities.size().string() +
          " commodities at " + prices.size().string() + " prices.")

      // Create the HTTP server and its various handlers.
      let routes = recover val
        var rts = Map[String, Handler tag]()
        rts("echo") = EchoHandler.create(env, universe)
        rts("systems") = SystemsHandler.create(env, universe)
        rts
      end
      TCPListener.ip4(recover Listener(env, universe, routes) end)
    end

type Routes is Map[String, Handler tag] val

class Listener is TCPListenNotify
  let _env: Env
  let _universe: Universe
  let _routes: Routes

  var _host: String = ""
  var _service: String = ""

  new create(env: Env, uni: Universe, rts: Routes) =>
    _env = env
    _universe = uni
    _routes = rts

  fun ref listening(listen: TCPListener ref) =>
    try
      (_host, _service) = listen.local_address().name()
      _env.out.print("listening on " + _host + ":" + _service)
    else
      _env.out.print("couldn't get local address")
      listen.close()
    end

  fun ref not_listening(listen: TCPListener ref) =>
    _env.out.print("couldn't listen")
    listen.close()

  fun ref connected(listen: TCPListener ref) : TCPConnectionNotify iso^ =>
    Server(_env, _universe, _routes)

class Server is TCPConnectionNotify
  let _env: Env
  let _universe: Universe

  let _routes: Routes

  new iso create(env: Env, uni: Universe, rts: Routes) =>
    _env = env
    _universe = uni
    _routes = rts

  fun ref received(conn: TCPConnection ref, data: Array[U8] iso) =>
    // Read the data as a JSON object.
    let start = Time.now()
    let s : String val = recover val
      var r : String ref = String()
      r.append(consume data)
    end

    var msg : (JsonObject val | None) = recover val
      try
        let doc : JsonDoc val = recover val
          var d : JsonDoc iso = JsonDoc.create()
          d.parse(s)
          consume d
        end

        match doc.data
        | let x : JsonObject val => x
        else
          None
        end
      end
    end
    let done = Time.now()
    Debug("JSON receive and parse time: " + (done._1 - start._1).string() +
    " seconds and " + (done._2 - start._2).string() + " nanos")

    try
      match msg
      | let m : JsonObject val =>
        let request = m.data("request") as String
        let t = Time.now()
        _routes(request).handle(conn, m)
        _env.out.print("Request launched at " + t._1.string() +
            " seconds and " + t._2.string() + " nanos")
      else error end
    else
      conn.write("""{ "error": "Request was not valid JSON" }""")
    end


  fun ref closed(conn: TCPConnection ref) =>
    _env.out.print("server closed")


interface Handler
  """Interface for a Handler, which dispatches for a particular route. Might do
  some processing on the input, and then hands off to a single-use actor for the
  main processing of the request."""
  be handle(conn: TCPConnection tag, req: JsonObject val)


