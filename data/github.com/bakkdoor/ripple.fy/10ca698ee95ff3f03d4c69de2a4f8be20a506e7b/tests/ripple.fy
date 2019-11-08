require: "../lib/ripple"

FancySpec describe: Ripple Model with: {
  class Person
  class Pet {
    include: Ripple Model
    properties: {
      name: { type: String }
    }
    key: 'name
    has_one: 'person type: Person
  }

  class Person {
    include: Ripple Model
    properties: {
      name: { type: String }
      age: { type: Fixnum }
    }
    key: 'name
    has_many: 'pets type: Pet
  }


  it: "returns the nested data" when: {
    kitty = Pet new: @{
      name: "Kitty"
    }
    tom = Person new: @{
      name: "Tom"
      age: 25
      pets: [kitty]
    }
    kitty person: tom

    # kitty save
    # tom save

    kitty to_hash_with_nested_fields: <['person => ['name, 'age]]> . is: <[
      'key => "Kitty",
      'person => <[
        'key => "Tom",
        'name => "Tom",
        'age => 25
      ]>
    ]>

    # kitty delete
    # tom delete
  }

  it: "finds model instances" when: {
    [1,2] each: |i| {
      Person create: @{
        name: "Person #{i}"
        age: i
      }
    }

    p1 = Person find: "Person 1"
    p2 = Person find: "Person 2"

    p1 is_not: nil
    p2 is_not: nil

    found = Person find_keys: ["Person 1", "Person 2", "Person 3"]
    found is: [p1,p2,nil]

    p1 delete
    p2 delete
  }
}