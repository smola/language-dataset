module coap

import org.eclipse.californium.core.coap.CoAP$ResponseCode
import org.eclipse.californium.core.CoapClient

# you need last version
import gololang.Adapters

struct coapHandler = { 
  $onLoad,
  $onError
}

augment coapHandler {

  function initialize = |self| { # self : ref to coapHandler structure
    println("initialize")
    let coapHandlerDefinition = Adapter()
      : interfaces(["org.eclipse.californium.core.CoapHandler"])
      : implements("onLoad", |this, coapResponse| {
          self: $onLoad()(coapResponse) # test if is null before
        })
      : implements("onError", |this| {
          self: $onError()()
        })        
      : definition()

    let coapHandlerInstance = AdapterFabric()
      : maker(coapHandlerDefinition)
      : newInstance()   

    return coapHandlerInstance        
  }

}  

# --- server ---
function CREATED = -> CoAP$ResponseCode.CREATED()
function CONTENT = -> CoAP$ResponseCode.CONTENT()

struct coapResource = { 
  identifier,   # resourceIdentifier
  name, # displayName
  $get,
  $post,
  $put,
  $delete
}

augment coapResource {

  function initialize = |self| { # self : ref to coapResource structure

    let coapResourceDefinition = Adapter()
      : extends("org.eclipse.californium.core.CoapResource")
      : implements("handleGET", |this, coapExchange| {
          self: $get()(coapExchange) # test if is null before
        })
      : implements("handlePOST", |this, coapExchange| {
          self: $post()(coapExchange)
        })
      : implements("handlePUT", |this, coapExchange| {
          self: $put()(coapExchange)
        })  
      : implements("handleDELETE", |this, coapExchange| {
          self: $delete()(coapExchange)
        })                
      : definition()

    let coapResourceInstance = AdapterFabric()
      : maker(coapResourceDefinition)
      : newInstance(self: identifier())   

    #set display name
    coapResourceInstance: getAttributes(): setTitle(self: name())
    return coapResourceInstance       
  }

 }  


struct coapServer = {
  port,
  _server
}

augment coapServer {
  function initialize = |this| {
    if this: port() is null {
      this: _server(org.eclipse.californium.core.CoapServer())
    } else {
      this: _server(org.eclipse.californium.core.CoapServer(this: port()))
    }
    return this
  }
  function add = |this, resource| {
    this: _server(): add(resource)
    return this
  }
  function start = |this| {
    this: _server(): start()
    return this
  }
} 
