Socket
Dispatcher
UUID

Generys := HttpServer clone do (
//metadoc Generys category Networking
  //doc Generys version Returns version number of running Generys instance.
  version     := 0.3
  //doc Generys routes List of all available routes.
  routes      := list()
  
  //doc Generys controllers List of all available controllers.
  controllers := list()
  controllers at = method(name,
    name = name .. "Controller"
    self select(type == name) first)

  //doc Generys formatters List of all available <em>ReponseFormatter</em>s.
  formatters  := list()
  //doc Generys sessions Hold session object
  sessions    := nil
  //doc Generys webSockets List of all open <em>WebSocket</em>s.
  webSockets  := Map clone
  //doc Generys futureRespones List of all cloned <em>FutureResponse</em>s.
  futureResponses := Map clone
  
  //doc Generys config Object holding configuration.
  config := Object clone do(
    host                := "127.0.0.1"
    port                := 4000
    urlPrefixPath       := ""
    sessionCookieName   := "_s"
    sessionStore        := "Map"
    poweredByHeader     := "Io/" .. System version
    useXSendFileHeader  := false
    logLevel            := "debug"
    logFile             := nil
    
    env                 := "dev"
  )
  //doc Generys serverURL Returns complete URL, including protocol, port and prefix.
  serverURL := lazySlot("http://#{host}:#{port}#{urlPrefixPath}" interpolate(self config))
  envDir    := lazySlot(self root .. "/config/env/" .. (self config env))

  //doc Generys loadConfig Loads configuration file for current environment.
  loadConfig := method(
    self config = (doFile((self envDir) .. "/config.json") asObject) appendProto(self config))

  //doc Generys serve Call this method to fireup Generys.
  serve := method(
    self loadConfig
    self staticDir := Generys root .. "/static"
    self tmpDir    := Generys root .. "/tmp"

    self sessions = Object getSlot(self config sessionStore) clone

    doFile(self root .. "/config/routes.io")
    doFile(self envDir .. "/init.io")

    doFiles(self root .. "/app/models")
    doFiles(self root .. "/app/controllers")
    doFiles(self root .. "/lib")

    self  setHost(self config host)\
          setPort(self config port)

    log info("You can find Generys on route #{self config host} near exit ##{self config port}")

    self start)

  /*doc Generys renderResponse(request, response)
  Passes handling to Dispatcher. This method is called by HttpServer, therefore not for end users. */
  renderResponse := method(req, resp,
    Dispatcher handleRequest(req, resp))

  /*doc Generys getSession(request, response)
  Returns session object or creates new one for request/response pair.
  Cookie is being used for keeping session id, which is then used to get it from <code>Generys sessions</code>. */
  getSession := method(req, resp,
    cookieName := self config sessionCookieName
    sessionId := req cookies[cookieName]
    
    sessionId ifNil(
      sessionId = UUID uuidRandom
      resp setCookie(cookieName, sessionId))
    self sessions[sessionId] ifNil(
      session := Object clone
      session setSlot("sessionId", sessionId)
      self sessions atPut(sessionId, session))

    self sessions[sessionId])
)
//doc Generys clone Returns Generys (singleton).
Generys clone := Generys

System userInterruptHandler = method(
  log info("Generys has been put to sleep.")
  Generys stop)
