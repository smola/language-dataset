----
This is a little DSL to simplify the Golo Adapters' use

sample: 

	let myDefinition = Adapter()
		: interfaces(["IHugable", "ICrazyable"])
		: extends("org.acme.Toon")
		: implements("hello", |this| -> "hello I'm " + this: name())
		: definition() # it's a getter (return the map definition)

	let ToonInstance = AdapterFabric()
		: maker(myDefinition)
		: newInstance("Babs")

	println(ToonInstance: hello())

----
module io.connectedworkers.dsl.adapter

----
The `definition` member of the `Adapter` structure contains the adapter configuration map
----
struct Adapter = { definition }

----
Adapter augmentations: this DSL is fluent
----
augment Adapter {
----
`interface` method, parameter is an array of Strings (list of interfaces)
----	
  function interfaces = |this, arrayInterfaces| {
    if this: definition() is null { this: definition(map[]) }
    this: definition(): put("interfaces", arrayInterfaces)
    return this
  }
----
`extends` method, parameter is a String (class name)
----  
  function extends = |this, className| {
    if this: definition() is null { this: definition(map[]) }
    this: definition(): put("extends", className)
    return this
  }
----
`implements` method, parameter is a String (method name) and a closure
----  
  function implements = |this, methodName, closure| {
    if this: definition(): get("implements") is null {
      this: definition(): put("implements", map[])
    }
    this: definition(): get("implements"): put(methodName, closure)
    return this
  }
----
`overrides` method, parameter is a String (method name) and a closure
----  
  function overrides = |this, methodName, closure| {
    if this: definition(): get("overrides") is null {
      this: definition(): put("overrides", map[])
    }
    this: definition(): get("overrides"): put(methodName, closure)
    return this
  }
} 
