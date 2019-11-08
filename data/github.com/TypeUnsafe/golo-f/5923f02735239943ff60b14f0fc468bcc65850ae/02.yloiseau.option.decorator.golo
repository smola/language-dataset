module optionAsDecorator

import gololang.error.Errors


@option
function toInt = |sParam| {
  println("--------------------")
  println("-> sParam: " + sParam)
  return java.lang.Integer.parseInt(sParam: trim())
}


function main = |args| {
  println(
    toInt("hello") # Optional.empty
  )
  println(
    toInt("42hello") # Optional.empty
  )
  println(
    toInt("hello42") # Optional.empty
  )

  let test = toInt("42")
  println(
    test # Optional[42]
  )
  println("value of test: " + test: get())


  println(
    toInt(42) # Optional.empty
  )

}