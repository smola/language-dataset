actor Main
  new create(env: Env) =>
    let person_val: Person val = recover Person("Andrew Turley", 40) end
    let person_ref: Person ref = recover Person("Matthew Turley", 35) end

    pretty_print_person(person_val, env.out)
    pretty_print_person(person_ref, env.out)

  fun pretty_print_person(person: Person box, out: OutStream) =>
    out.print(person.name() + " is " + person.age().string() + " years old")

class Person
  var _name: String
  var _age: U64

  new create(n: String, a: U64) =>
    _name = n
    _age = a

  fun name(): String =>
    _name

  fun ref increment_age() =>
    _age = _age + 1

  fun age(): U64 =>
    _age
