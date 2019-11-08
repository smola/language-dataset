module dvcs

import http

struct client = {
  baseUri, credentials
}

augment client {
  function getHeaders = |this| -> [
    http.header("Content-Type", "application/json"),
    http.header("User-Agent", "GitHubGolo/1.0.0"),
    http.header("Accept", "application/vnd.github.v3.full+json"),
    http.header("Authorization", this: credentials())
  ]

  function getUri = |this, path| {
    return this: baseUri() + path
  }
  function getData = |this, path| {
    let resp = http.request("GET", this: getUri(path), null, this: getHeaders())
    return resp
  }

  function postData = |this, path, data| {
    let resp =  http.request("POST", this: getUri(path), JSON.stringify(data), this: getHeaders())
    return resp
  }

  function putData = |this, path, data| {
    let resp =  http.request("PUT", this: getUri(path), JSON.stringify(data), this: getHeaders())
    return resp
  }

  function fetchContent = |this, path, owner, repository, decode| {
    let resp = this: getData("/repos/"+owner+"/"+repository+"/contents/"+path)
    let data = JSON.toDynamicObjectFromJSONString(resp: data())
    if decode is true {
      let decoder = java.util.Base64.getMimeDecoder()
      let bytesDecoded = decoder: decode(data: content(): getBytes())
      data: content(String(bytesDecoded))
    }
    return data
  }

}

----
 constructor of dvcsClient
----
function Client = |uri, token| {

  let credentials = match {
    when token isnt null and token: length() > 0 then "token" + ' ' + token
    otherwise null
  }
  return client(
    baseUri= uri,
    credentials= credentials
  )
}