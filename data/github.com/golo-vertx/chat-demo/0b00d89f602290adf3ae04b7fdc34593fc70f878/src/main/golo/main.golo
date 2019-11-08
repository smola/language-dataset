module chat

import io.vertx.lang.golo.GoloVerticleFactory
import io.vertx.golo.core.Vertx
import io.vertx.core.VertxOptions
import io.vertx.core.DeploymentOptions
import io.vertx.core.eventbus.DeliveryOptions

function main = |args|{
	var chatVertStart = |verticle|{
		let vertx = verticle: getVertx()
    	let eb = vertx: eventBus()
    	eb: consumer("chat-demo"):handler(|msg|{
    		println("[Chat-Demo] Received Msg from "+msg:headers():get("sent-by")+ " : " + msg:body())
    	})
	}
	var chatVert = GoloVerticleFactory():createGoloVerticle(chatVertStart,|v|{})

	io.vertx.core.Vertx.clusteredVertx(VertxOptions(),|rep|{
        let vertx = rep:result() 
        let eb = vertx:eventBus()
        println(chatVert)
        vertx: deployVerticle(chatVert,|depId|{
        	println("Chat Program Running!")
        	eb:publish("chat-demo","[Test Msg]",DeliveryOptions():addHeader("sent-by","[chat program]"))
        	var mainBlockingCode = {
				var userName = readln("Enter User Name? ")
				var finish = false
				while(not finish){
					var msg = readln()
					if(msg == "exit"){
						finish = true
					}
					eb:publish("chat-demo",msg,DeliveryOptions():addHeader("sent-by",userName))
				}
				return "Stopping Chat Program"
			}
			vertx:executeBlocking(|f|->f:complete(mainBlockingCode()),|r|->println(r:result()))
        })
    })
}

