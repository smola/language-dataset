module funcGolo


function main = |args| {
  let bob =
    DynamicObject()
      : id("bob")
      : address(
          DynamicObject()
            : email("bob@github.com")
            : country("US")
        )
  # {"address":{"country":"US","email":"bob@github.com"},"id":"bob"}

  let john =
    DynamicObject()
      : id("john")
      : address(
          DynamicObject()
            : country("US")
        )
  # {"address":{"country":"US"},"id":"john"}

  let jane =
    DynamicObject()
      : id("jane")
  # {"id":"jane"}

  let buddies = [bob, jane, john]

  println( buddies: get(2)?: address()?: email() orIfNull "no email" )
  println( buddies: get(0)?: address()?: email() orIfNull "no email" )
  println( buddies: get(1)?: address()?: email() orIfNull "no email" )


}