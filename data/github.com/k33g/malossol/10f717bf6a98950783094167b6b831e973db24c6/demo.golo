module demo

import acme.Toon
import gololang.Async

import malossol

----
 Create your own matcher
----
augmentation halfMatcher = {
  function toBeHalf = |this, expectedValue| {
    require(
      this: actualValue(): equals(expectedValue/2), 
      this: actualValue() + " isn't half " + expectedValue
    )
    println(" OK: " + this: actualValue() + " is half " + expectedValue)
    return this
  }
}
----
 Add the matcher to matchers
----
augment malossol.types.matchers with halfMatcher

function main = |args| {

  describe("Search something ...", {
    it("code response is 200 and request duration is less than 2000 ms", {

      timer(): start(|self| {
        # synchronous request
        let response = getHttp("http://www.google.com", HTML())
        expect(response: code()): toEqual(200)

      }): stop(|self| {

        expect(self: duration()): toBeLessThan(2000_L)
      })


    })
  })

  describe("Test some numbers", {
    it("4/2 = 2", {
      expect(2): toBeHalf(4): toBeInteger()
    })
  })

  describe("Test some strings", {
    it("string contains 'world'", {
      expect("hello world!"): toContain("world")
    })

    it("string starts with 'hello'", {
      expect("hello world!"): toStartWith("hello")
    })
  })

  describe("Testing Java: Elmira", {

    let Elmira = Toon("Elmira")

    it("is Elmira", {
      expect(Elmira: getName()): toEqual("Elmira")
    })

    it("loves Toons", {
      
      let Buster = Toon(): name("Buster Bunny")
      
      expect(Elmira: hug(Buster)): toEqual("I <3 " + Buster: name())

    })

  })

}