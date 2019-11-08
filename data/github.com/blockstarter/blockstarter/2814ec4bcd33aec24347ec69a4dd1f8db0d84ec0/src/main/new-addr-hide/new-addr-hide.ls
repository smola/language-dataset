require! {
    md5
    \../encrypt-private-key.js : \encryptor
}

state =
    config: null
new-addr  =
    eth: require \../new-addr/new-addr-eth.js
    btc: require \../new-addr/new-addr-btc.js
    ltc: require \../new-addr/new-addr-ltc.js
init-firebase = (url)->
    init-firebase.i = init-firebase.i ? require \firebase
    if not init-firebase[url]?
      config =
         databaseURL: url
      init-firebase[url] = 
           init-firebase.i.initialize-app config, url
    (info)->
      id = md5 JSON.stringify info
      init-firebase[url]
              .database!
              .ref \private/ + id
              .set info
init-resource = (url)->
    | url.index-of(\firebaseio.com) > -1 => init-firebase url
    | _ => throw "Not Supported"
send-data = (url, info, cb)->
    request = init-resource(url) info
    success = ->
        cb null 
    fail = (err)->
        cb err
    request.then success, fail .catch fail
upload-info = ([head, ...tail], info, cb)-->
    return cb null if not head?
    err <-! send-data head, info
    return cb err if err?
    err <-! upload-info tail, info
    cb err
module.exports = (coin, info, cb)-->
    return cb "Key is not defined" if not state.config?
    return cb "You need repos" if not state.config.repos?
    return cb "You need at least 2 repos" if state.config.repos.length < 2
    return cb "You need a key" if typeof! state.config.key isnt \String
    generator = new-addr[coin]
    return cb "Generator is not found for #{coin}" if typeof! generator isnt \Function
    key = generator!
    create-key = md5(state.config.key + coin + JSON.stringify(info))
    err, private-key <-! encryptor.encrypt key.private-key, create-key
    return cb err if err?
    key-info =
       private: private-key
       coin: coin
       info: info
    err <-! upload-info state.config.repos, key-info
    cb err, key.address
module.exports.setup = (encrypted-config, cb)->
    key = process.env[\configkey] ? \8e3dc782fc8375c43f59403a337db4ba
    err, decrypted-config <-! encryptor.decrypt encrypted-config, key
    return cb err if err?
    config = JSON.parse decrypted-config
    state.config = config
    cb null