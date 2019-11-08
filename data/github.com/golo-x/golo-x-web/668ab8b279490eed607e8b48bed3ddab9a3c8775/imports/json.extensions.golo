module JSON

function toDynamicObjectTree = |obj| {

  let isJSONObject = |obj| -> obj oftype org.json.simple.JSONObject.class
  let isJSONArray = |obj| -> obj oftype org.json.simple.JSONArray.class

  let parse = |level, obj, dyno| {
    let parseMembers = |obj, dyno| {
      obj: each(|key, value| {
        dyno: define(key, value)
        parse(key, value, dyno)
      })
    }

    if isJSONObject(obj) {
      if level is null { # root
        parseMembers(obj, dyno)
      } else {
        dyno: define(level, DynamicObject())
        parseMembers(obj, dyno: get(level))
      }
    } else if isJSONArray(obj) {
        dyno: define(level, list[])
        obj: each(|item| {
          if isJSONObject(item) is false and isJSONArray(item) is false {
            dyno: get(level): append(item)
          } else if isJSONObject(item) {
              let subDyno = DynamicObject()
              parseMembers(item, subDyno)
              dyno: get(level): append(subDyno)
          }
        })
    }
    return dyno
  }
  return parse(null, obj, DynamicObject())
}

function toDynamicObjectTreeFromString = |str| -> toDynamicObjectTree(JSON.parse(str))
