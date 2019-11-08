module adapter.rocket.chat

import http
import JSON
import gololang.Errors

augmentation botRocketChatFeatures = {
  function yo = |this| -> println("🤖 Yo! I'm the Rocket.Chat adapter")

  @result
  function connect = |this, url, user, password| {
    let baseUri = url+"/api"
    let cli = httpClient()
      : baseUri(baseUri)
      : headers(list[
          header("Content-Type", "application/json"),
          header("User-Agent", "IndyBot/1.0.0")
        ])
    this: httpClient(cli)

    this: name(user)

    let respResult = this: httpClient(): postData("/login", map[
      ["user", user], ["password", password]
    ])

    return respResult: either(
      |resp| {
        let userLogin = JSON.toDynamicObjectTreeFromString(resp: data())
        this: httpClient(): headers(): append(header("X-Auth-Token", userLogin: data(): authToken()))
        this: httpClient(): headers(): append(header("X-User-Id", userLogin: data(): userId()))
        return userLogin
      },
      |err| {
        return err
      }
    )
  }

  @result
  function getPublicRooms = |this| {
    let respResult = this: httpClient(): getData("/publicRooms")
    return respResult: either(
      |resp| {
        return JSON.toDynamicObjectTreeFromString(resp: data())
      },
      |err| {
        return err
      }
    )
  }

  @result
  function getRoomByName = |this, name| {
    let respResult = this: httpClient(): getData("/publicRooms")
    return respResult: either(
      |resp| {
        let rooms = JSON.toDynamicObjectTreeFromString(
          resp: data()
        ): rooms()
        return rooms: find(|room| -> room: name(): equals(name))
      },
      |err| {
        return err
      }
    )
  }

  @result
  function send = |this, message, roomId| {
    let respResult = this: httpClient(): postData("/rooms/"+roomId+"/send", map[
      ["msg", message]
    ])
    return respResult: either(
      |resp| {
        return JSON.toDynamicObjectTreeFromString(resp: data())
      },
      |err| {
        return err
      }
    )
  }

  @result
  function sendMessage = |this, message, roomName| {
    return this: getRoomByName(roomName): either(
      |room| {
        this: send(message, room: _id())
      },
      |err| {
        return err
      }
    )
  }

  @result
  function getMessages = |this, roomName| {
    return this: getRoomByName(roomName): either(
      |room| {
        return this: httpClient(): getData("/rooms/"+room: _id()+"/messages"): either(
          |resp| {
            return JSON.toDynamicObjectTreeFromString(resp: data())
          },
          |err| {
            return err
          }
        )
      },
      |err| {
        return err
      }
    )
  }

  function listen = |this, roomName, receiver, onNotification| {
    let delay = this: delay() # pooling

    # get last 50 messages
    let messages = this: getMessages(roomName): get(): messages()

    this: startJob(delay, |this| {
      let mentioned =  this: getMessages(roomName): get(): messages()
        : filter(|message| -> message: mentions() isnt null)
        : filter(|message| -> message: mentions(): find(|mention| -> mention: username(): equals(receiver)) isnt null )

      mentioned: each(|messageForReceiver| {
        messages: find(|message| -> message: _id(): equals(messageForReceiver: _id())) orIfNull {
          onNotification(messageForReceiver) # only if message is new
          messages: append(messageForReceiver)
        }()
      })

    })
  }

  function listen = |this, roomName, onNotification| {
    this: listen(roomName, this: name(), onNotification)
  }

}

augment bots.types.indyBot with botRocketChatFeatures
