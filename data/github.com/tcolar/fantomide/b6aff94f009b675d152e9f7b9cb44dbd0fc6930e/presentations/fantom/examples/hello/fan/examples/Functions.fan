**
** Functions examples
**
class Functions
{
  Void example()
  {
    func1 := |Int a, Int b->Int| {a+b}  // function that takes two Int and return an Int
    
    a := func1.callList([2,3]) //  5
    b := func1.call(2,3) //  5
    c := func1(2,3) // 5

    func3 := func1.bind([3]) // [Int->Int]
    p := func3(3) // 5
      
    executor(Obj#.method("echo").func, ["Hello"]) // Hello        
  }

  Void executor(Func func, Obj[] params)
  {
      echo("... Starting execution")
      func.call(params)  
      echo("... Stopping execution")
  }
  
  Void funcParam(Int i, |Str->Int| func) {}
  
  Void exampleInstanceParams()
  {
    m := Str#replace
    f := m.func
    m.params// [sys::Str from, sys::Str to]
    f.params// [sys::Str this, sys::Str from, sys::Str to]
    // call as a method on instance
    s1 := "hi!".replace("hi", "hello")
    // call the function, passing an instance
    s2 := f("hi!", "hi", "hello")
  }
  
}