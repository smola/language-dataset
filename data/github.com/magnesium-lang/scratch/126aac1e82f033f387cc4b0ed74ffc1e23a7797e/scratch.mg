// Expressions
5 + 5
9++
if x > 10
x > 10 ? 40 : 0

// Statements (pattern matching & binding)
x = 10
&y = x
p = "Hello World" // str
q: str
name: str = "Sabree Blackmon"

if x > 10
    console << "Hello World"

console::err << "Error encountered"

(x, y) = (2 + 2, 4 + 4)

//// Existential Qualifier

// Uninhabited
()
()! // Refutation

// x is inhabited by 10 only if y > 5
x = 10 if y > 5
x = y > 5 ? 10 : ()

// ? & ! operators

// If a function or a type value expression contain branches that return () or (), the caller must handle the possibility it can be refuted at runtime. 
// We can do that with ? or !, or via a match statement


    
// Streams
file_name << console::in // str

use file::open(file_name)
    << "Hello world!"
    << "New line. Rather cheaky?"

// Functions
print_hello() // print_hello is bound to a function constructor of type () -> ()
fn print_hello() // construct print_hello with following block
    console << "Hello World"

fn say_name(name: &str)
    console::out << "Hello" << name

fn concat_name(first: str, last: str) :: io -> str // concat_name is bound to a function constructor of type (str, str) -> str with side effects io
concat_name(first, last) // construct concat_name with first and last bound to args str and str, with following block
    str(first + last)

// Function definition - with sugar
// The function constructor is followed immediately by a block 
fn concat_name(first: str, last: str) :: io) -> str
    str(first + last)

// -> and :: are left associative operators that and have lower precedence than function invocation
// Desugars to 
concat_name = ((fn(str, str) :: io) -> str) // fn() actually takes type arguments
concat_name(first, last)
    str(first + last)

// Function alias
fn say_hello() {
    console::out << "Hello!"
}

greetings = say_hello
greetings() // "Hello!"

// Advanced pattern matching
type Coordinate = (float, float)
locate = fn(Coordinate)
locate((x, y)) // Match against tuples and enum types with single variant?
    console::out << "Cordinates: " << x << ", " << y

type Miles = (int)

// Function constructors vs functions
type Concat = fn(str, str) : IO -> str  // Function constructor assigned to type Callback
// Binds Callback to type constructor for a function of type (str, str) -> str, and  has io side effects

concat_name = Concat() 
concat_name(first, last)
    str(first + last)

concat_name("Sabree", "Blackmon") // str: "Sabree Blackmon"
concat_name_alt = Concat()
concat_name_alt()
    str(first + "" + last)
// Constructor can be used multiple times to create additional functions of the same type.

// Partial Application




// Constant functions (compile time expressions)






// Anonymous

// How does operator procedance work here?
(fn(val: str) -> str){ val }("Hello World") 
// Hello World

fn(val: str) -> str
    val 
// fn(str) -> str

// Anonymous functions can be constructed from function type constructors
type Callback = fn(str) -> str
process = fn(Callback) -> str
process(callback)
    callback()

console::out << process(Callback(val){ val.upercase() }) // Prefer creating a new type function type
console::out << process((fn(val: str) -> str){ val.upercase() }) // but this is also acceptable

////  Functions -- With value condition (A state must exist in order for function to be evoked -- a state is a block that returns a boolean)
pay = fn(dollars: float) -> str if [dollars > 0.0]
    console::log << "You paid: $" << dollars

//// Instead, could define a newtype that can be used as function argument
//// Here, there is a sigma type with a constructor [block] that returns a value or uninhabited ()
//// In this case, the value is explicitedly refuted using the ()! syntax
type Dollars = (float)(value)
    value > 0.0 ? value : ()!

pay = fn(amount: Dollars) -> str
    console::log << "You paid: $" << amount 

// Functions - Type Arguments
debug = fn(str, @T) if T is Debug // Optional condition -- only exists if T has Debug trait (is returns boolean)
debug(name, value)
    console::out << name << ": " << value // Works for all type T that implements Debug

// Functions - With type specialization
// This isn't currently possible with Rust. This may require Mg to be able to compute the type at transcompile time
// This may be extremely difficult.

debug = fn(str, @T) if T is Debug
debug(name, value: str) // bind name is optional but type must be provided
    ...
debug(name, value: int)
    ...
debug(name, value) // Possible ambiguity in these cases.
    console::out << name << ": " << value // Works for all type T that implements Debug

// Complex add example -- the return type is the same as argument type @B, so 
add = fn(@A, @B) -> @B
add(a: uint, b: uint)
    ..
add(a: uint, b: float) if b > 0.0 // Specialized function are allowed state guards
    ...
add(a: str, b: str)
    ...

// Complex add example - The return value is a type parameter and must be specified
// here, the parameter bound name is provided in the constructor
add = fn(a: @A, b: @B) -> @C
add(int, int) -> int
    a + b
add(int, float) -> float
    float(a) + b
add(int, str) -> str
    str(a) + b

// Functions - With phantom type parameters
// This, essentially, becomes a functional, nonexhaustive match type. 
will_it_blend = fn(@T) -> bool
will_it_blend(Grapefruit) // Types are matched, but are not bound
    true
will_it_blend(Avocado)
    true
will_it_blend(Concrete)
    false
// Turn it into exhaustive match by matching to all other types
will_it_blend(_) 
    false


// Traits

// Trait aliases

// Record Update syntax

// Type constructors