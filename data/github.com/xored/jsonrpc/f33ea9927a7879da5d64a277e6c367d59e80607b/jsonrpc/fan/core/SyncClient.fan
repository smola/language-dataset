using util

abstract const class Client
{
  static SyncClient sync(SyncTransport transport)
  {
    SyncClient(transport)
  }
  
  static AsyncClient async(AsyncTransport transport)
  {
    AsyncClient(transport)
  }
  
  static Str makeRequest(Str method, Obj? params, Obj? id := null)
  {
    JsonOutStream.writeJsonToStr(
      Request { it.method = method; it.id = id; it.params = params }.toJson
    )
  }
  
  static Response makeResponse(Str responseStr) 
  { 
    Response.fromJson(RpcUtils.fromJsonStr(responseStr))
  }
}

const class SyncClient : Client, RpcUtils
{
  new make(SyncTransport transport)
  {
    this.transport = transport
  }
  
  private const SyncTransport transport

  ** Throws:
  ** - [IOErr]`sys::IOErr` on transport errs
  ** - [RpcErr]`jsonrpc::RpcErr` on server errs
  Obj? request(Str method, Obj? params := null)
  {
    if(params isnot Map && params isnot List) params = [params]
    response := Response.fromJson(
      fromJsonStr(
        transport.send(
          makeRequest(method, params, Duration.nowTicks))))
    if(response is ErrResponse) throw (response as ErrResponse).err
    return (response as ResultResponse).result
  }
  
  ** Throws:
  ** - [IOErr]`sys::IOerr` on transport errs 
  Void notify(Str method, Obj? params := null)
  {
    if(params isnot List && params isnot Map) params = [params]
    transport.send(makeRequest(method, params))
  }
}

abstract const class SyncTransport
{
  abstract Str? send(Str request)
}