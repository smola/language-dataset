module poks.server

import redis.clients.jedis.Jedis
import gololang.Errors
import spark.Spark
import spark.augmentations
import jedis.augmentations
import http
import JSON
import gololang.Async
import gololang.concurrent.workers.WorkerEnvironment


function checkServices = |env, rdb| {
  return env: spawn(|options| {
    while (options: stop() is false) { # TODO: trying()
      trying({
        rdb: keys("*:*"): each(|key| {
          let service = rdb: getFromJson(key)
          # call the service:
          # promise
          http.getJSONData(
            service: url()+"/hello" # service must have a `hello` route
          )
          : onSet(|result| { # if success
            if(result: code(): equals(404)) {
              println("😡: " + service: url() + " can't be reached | 404")
              # delete in redis db
              trying({
                rdb: del(service: name()+":"+service: id())
              }) # TODO: add either

            } else {
              println("😺: " + service: url() + " is active | " + result: data())
            }

          })
          : onFail(|error| { # if failed
            println("😡: " + service: url() + " can't be reached | " + error: message())
            # delete in redis db
            trying({
              rdb: del(service: name()+":"+service: id())
            }) # TODO: add either
          })
        }) # end of rdb: keys parsing
      }) # end of trying -> TODO: add either

      sleep(options: refresh())
    }
    env: shutdown()
  })
}

function defineRoutes = |port, rdb, env| {
  println("😃  Redis connection is ok" )

  # start checking
  let checking = checkServices(env, rdb)
  checking: send(DynamicObject()
    : refresh(5000_L)
    : stop(false)
  )

  # TODO: get details of services
  # http://localhost:8080/services
  get("/services", |request, response| -> trying({
    return rdb: keys("*:*")
  })
  : either(
    |content| -> response: jsonPayLoad(content),
    |error| -> response: jsonPayLoad(DynamicObject(): message(error: message()))
  ))

  # http://localhost:8080/services/addition:0001
  get("/services/:service_id", |request, response| -> trying({
    let service = rdb: getFromJson(request: params(":service_id"))
    return service # this is a DynamicObject()
  })
  : either(
    |content| -> response: jsonPayLoad(content),
    |error| -> response: jsonPayLoad(DynamicObject(): message(error: message()))
  ))

  post("/hey", |request, response| -> trying({
    println("🤖 headers: " + request: headers())

    let serviceCredentials = request: headers("Authorization")
    println("🙈 credentials: " + serviceCredentials)

    let serverCredentials = System.getenv(): get("SERVER_CREDENTIALS")
    println("😈 server credentials: " + serverCredentials)

    if(serverCredentials: equals(serviceCredentials)) { # Credentials are OK

      let data = request: body()
      let serviceInformations = JSON.toDynamicObjectTreeFromString(data)

      println("Credentials are OK 😘")
      let key = serviceInformations: name() + ":" + serviceInformations: id()
      println("key: " + key)

      println("😃 service: " + serviceInformations: name())
      println(" id: " + serviceInformations: id())
      println(" version: " + serviceInformations: version())
      println(" url: " + serviceInformations: url())
      println(" operations: " + JSON.stringify(serviceInformations: operations()))

      # update redis ...
      return trying({
        rdb: setAsJson(
          key,
          JSON.stringify(serviceInformations)
        )
        return true
      })
      : either(
        |success| {
          return DynamicObject(): message("roger 👍")
        },
        |error| {
          error: printStackTrace()
          return DynamicObject(): message("Something bad with Redis 😡")
        }
      )

    } else { # Credentials are KO
      let message = "Bad credentials 😡"
      println(message)
      return DynamicObject(): message(message)
    }

  })
  : either(
    |content| {
      response: status(201)
      return response: jsonPayLoad(content)
    },
    |error| {
      response: status(505)
      return response: jsonPayLoad(DynamicObject(): message(error: message()))
    }
  ))

  println("🌍 poks server is listening on " + port + "...")
}

function main = |args| {

  #let env = WorkerEnvironment.builder(): withCachedThreadPool()
  if (System.getenv(): get("SERVER_CREDENTIALS") is null) { raise("😡 no server credential") }
  if (System.getenv(): get("PORT") is null) { raise("😡 no http port") }
  if (System.getenv(): get("REDIS_URL") is null) { raise("😡 no redis url") }

  trying({
    let port =  Integer.parseInt(System.getenv(): get("PORT"))
    setPort(port)
    staticFileLocation("/public")
    let rdb = redis.clients.jedis.Jedis(System.getenv(): get("REDIS_URL"))
    rdb: get("") # trying to query redis server
    return DynamicObject(): port(port): rdb(rdb): env(WorkerEnvironment.builder(): withCachedThreadPool())
  })
  : either(
    |config| -> defineRoutes(config: port(), config: rdb(), config: env()),
    |error| -> println("😡 " + error: message())
  )

}
