module promise_demo

import gololang.concurrent.async.Promise

function main = |args| { 

  let divide42 = { # my promise

    let promise = Promise()

    let randomNumber = java.util.Random():nextInt(5)
    if randomNumber > 0 {
      let result = 42.0 / randomNumber
      # resolve
      promise: set([randomNumber, result])
    } else {
      # reject
      promise: fail(java.lang.Exception("Divided by 0!"))
    }

    promise: future()
      : onSet(|values| { #then
          println("😊 👍 42/" + values: get(0) + "=" + values: get(1))
        })
      : onFail(|err| { #catch
          println("😡 👎 ouch!: " + err: getMessage())
        })
  }
  
  10: times(-> divide42())
}