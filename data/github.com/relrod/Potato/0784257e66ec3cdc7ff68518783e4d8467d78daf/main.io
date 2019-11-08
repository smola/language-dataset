#!/usr/bin/env io

Potato := Object clone do (
  __GET_methods__ := Map clone
  __POST_methods__ := Map clone

  GET := method(route, implementation,
    __GET_methods__ atPut(route, implementation)
  )

  POST := method(route, implementation,
    __POST_methods__ atPut(route, implementation)
  )

  server := Object clone
  server port := 2000

  server headers := Map clone do(
    atPut("Content-Type", "text/html")
  )

  ok := method(body,
    response := "HTTP/1.1 200 OK\n"
    server headers foreach(name, value,
      response = response .. "#{name}: #{value}\n" interpolate
    )
    response = response .. "\n" .. body
    response
  )

  bad_request := method(body,
    response := "HTTP/1.1 400 Bad Request\n"
    server headers foreach(name, value,
      response = response .. "#{name}: #{value}\n" interpolate
    )
    response = response .. "\n" .. body
    response
  )

  not_found := method(body,
    response := "HTTP/1.1 404 Not Found\n"
    server headers foreach(name, value,
      response = response .. "#{name}: #{value}\n" interpolate
    )
    response = response .. "\n" .. body
    response
  )


  // Take the path given by the user, and attempt to route it to a block.
  // Return the block if it exists or `nil` if not.
  // Note that this method does not *CALL* the block, it simply returns it.
  route := method(given,
    if (Potato __GET_methods__ keys contains(given),
      Potato __GET_methods__ at(given),
      nil
    )
  )


  server handleRequest := method(sock,
    sock streamReadNextChunk
    if (sock readBuffer beginsWithSeq("GET"),

      // Split things up into a request object that we can send back to the
      // block.
      request := Object clone
      request ip := sock host

      queryStringSplit := sock readBuffer betweenSeq("GET ", " HTTP") split("?")
      request path := queryStringSplit at(0) // This will exist even if no qs does.
      request queryString := queryStringSplit at(1)
      request args := Map clone

      if (request queryString,
        qsSplit := request queryString split("&")
        qsSplit foreach(q,
          // Split it - if it splits into 2, set key=value, otherwise set key=""
          querySplit := q split("=")
          if (querySplit size == 1,
            request args atPut(querySplit at(0), "")
          )
          if (querySplit size == 2,
            request args atPut(querySplit at(0), querySplit at(1))
          )
        )
      )


      "[request] #{27 asCharacter}[34m#{request ip}#{27 asCharacter}[0m - #{27 asCharacter}[32m#{request path}#{27 asCharacter}[0m" interpolate println
      if (controller := Potato route(request path)) then (
        sock write(controller call(request))
        sock close
      ) else (
        "  -> 404 Not Found" println
        sock write(Potato not_found("404!"))
        sock close
      )
    )
  )

  server serve := method(
    Server clone do (
      setPort(Potato server port)
      handleSocket := method (sock,
        Potato server @handleRequest(sock)
      )
      start
    )
  )

  run := method(server serve)

  form := Object clone do (
    field := Object clone do (

    )
  )
)

# Create a new Potato app.
app := Potato clone

# Another way of doing this is changing the variables which set the defaults
# then not overriding the serve method. Either of these options work just fine.
app server port := 2001

# Let's create a simple method.
app GET("/", block(request,
  f := File with("potato.html") openForReading contents
  app ok(f)
))

app GET("/random", block(request,
  app ok(Random value(100000) floor)
))

app GET("/ip", block(request,
  app ok(request ip)
))

app GET("/tmp", block(request,
  app ok(Directory clone with ("/tmp") items map(file,
    "<a href=\"#{file name}\">#{file name}</a>" interpolate
  ) join("<br>"))
))

# And a simple method dealing with GET arguments.
app GET("/greet", block(request,
  app ok("Hi there, #{request args at(\"name\")}" interpolate)
))

app run
