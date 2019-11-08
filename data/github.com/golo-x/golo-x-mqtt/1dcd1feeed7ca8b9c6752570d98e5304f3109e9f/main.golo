#!/usr/bin/env golosh
module demo

import io.netty.handler.codec.mqtt.MqttQoS
import io.vertx.core.buffer.Buffer
import io.vertx.mqtt.MqttEndpoint
import io.vertx.core.Vertx
import io.vertx.mqtt.MqttServer
import io.vertx.mqtt.MqttServerOptions

import gololang.Errors

struct mqttOptions = { port }
struct mqttBroker = { clients, subscriptions }

augment java.util.Map {
  @option
  function getOptional = |this, key| -> this: get(key)
}

@result
function yoServer = |options, onConnection, onSubscription, onMessage| {
  let vertx = Vertx.vertx()
  let mqtt_options = MqttServerOptions(): setPort(options: port())
  let mqtt_server = MqttServer.create(vertx, mqtt_options)

  let broker = mqttBroker(clients=map[] ,subscriptions=map[])

  mqtt_server: endpointHandler(|endpoint| {
    endpoint: accept(false)

    # update clients connection
    broker: clients(): put(endpoint: clientIdentifier(), endpoint)

    onConnection(broker, endpoint)

    endpoint: subscribeHandler(|subscribe| {
      # update clients subscriptions
      subscribe: topicSubscriptions(): each(|subscription| {
        broker: subscriptions(): put(
            endpoint: clientIdentifier() + "-" + subscription: topicName(),
            true
          )
      })
      onSubscription(broker, endpoint, subscribe)
    })

    endpoint: publishHandler(|message| {
      # You've got a 📬
      # for each 👨‍ check and dispatch messages
      broker: clients(): each(|identifier, client| {
        # if 👨‍ has subscribed to the current topic, then send 💌
        broker: subscriptions(): getOptional(identifier + "-" + message: topicName())
          : either(
            default= { # no subscription
              # nothing todo
            },
            mapping= |isSubscriptionActive| {
              if isSubscriptionActive is true {client: publish(
                  message: topicName()
                , Buffer.buffer(message: payload(): toString())
                , MqttQoS.AT_LEAST_ONCE()
                , false
                , false
              )}
            }
          )
      })
      onMessage(broker, endpoint, message)
    })
    # 🚧 TODO: other events
  })
  return mqtt_server
}

function main = |args| {

  yoServer(
    options= mqttOptions(port=1883),
    onConnection= |broker, endpoint| {
      println("🤖 connected client: " + endpoint: clientIdentifier())
    },
    onSubscription= |broker, endpoint, subscribe| {
      println("😊 new subscriptions(s): " + subscribe: topicSubscriptions(): head())
    },
    onMessage= |broker, endpoint, message| {
      println("📬 message on topic: " + message: topicName())
      println("👋 you've got a 📩: " + message: payload())
    }
  ): either(
      recover = |error| -> println("😡 Huston? " + error: message()),
      mapping = |server| {
        server: listen()
        println("😄 Yo is listening on " + server: actualPort())
      }
  )
}
