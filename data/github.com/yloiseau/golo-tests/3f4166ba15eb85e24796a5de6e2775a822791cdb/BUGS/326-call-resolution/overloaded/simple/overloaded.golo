
module TestOverloaded

import test

local function test = |f, e| {
  try {
    let v = f()
    if not v: startsWith(e) {
      println("FAILED: should be %s but is %s": format(e, v))
    } else {
      println("OK: " + e)
    }
  } catch (e) {
    println("ERROR: " + e)
  }
}

function main = |args| {
  let t = OverloadedMethod()
  let sup = |s| -> |v| -> s + v

  ## These are always OK...
  # test(-> t: foo("a"), "String")
  # test(-> t: foo(Wrapper.of("w")), "Wrapper")
  # test(-> t: foo(OverloadedMethod.fi("s")), "FunctionalInterface")

  test(-> t: foo(2.5), "Number")
  # this one is influenced by the presence of the previous one!
  test(-> t: foo(1), "Integer")

  # May dispatch on any of the 3 possible methods:
  test(-> t: foo(|v| -> "a" + v), "FunctionReference")
  # May cause an error (not a direct method handle) if used as a FI 
  # (see https://github.com/eclipse/golo-lang/issues/277)
  test(-> t: foo(sup("b")), "FunctionReference")

}
