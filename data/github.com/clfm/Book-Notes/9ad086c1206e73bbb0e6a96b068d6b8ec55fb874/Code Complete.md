# Code Complete

#### Second Edition || By Steve McConnell

## Overview

_Code Complete_ outlines a variety of software construction principles and techniques. Many software professionals hold it in high esteem as a reference for developing well-crafted, easily-maintained systems.

The book covers many aspects of creating high quality software, from high-level architecture design to low-level code formatting. Topics include designing clean and user-friendly APIs, choosing thoughtful names, and performing testing, debugging, and refactoring in structured ways.

These notes are organized by chapter, presenting main points for each subsection, followed by a few of my favorite quotes and distilled "takeaways".


<a name="toc"></a>
## Table of Contents
+ __Part I - Laying the Foundation__
    - [1) Welcome to Software Construction](#ch1)
    - [2) Metaphors for a Richer Understand of Software Development](#ch2)
    - [3) Measure Twice, Cut Once: Upstream Prerequisites](#ch3)
    - [4) Key Construction Decisions](#ch4)
+ __Part II - Creating High-Quality Code__
    - [5) Design in Construction](#ch5)
    - [6) Class Foundations: Abstract Data Types (ADTs)](#ch6)
    - _Work in progress..._
+ __Part III - Variables__
    - _Work in progress..._
+ __Part IV - Statements__
    - _Work in progress..._
+ __Part V - Code Improvements__
    - _Work in progress..._
+ __Part VI - System Considerations__
    - _Work in progress..._
+ __Part VII - Software Craftsmanship__
    - [31) Layout and Style](#ch31)
    - _Work in progress..._



<a name="ch1"></a>
## Chapter 1 - Welcome to Software Construction

[(return to table of contents)](#toc)

### Summary

#### 1.1 - What is Software Construction?

+ **Construction** activities mostly refer to _detailed design, coding, debugging, and integration/unit testing_
    - The heart of construction is coding and debugging, which the existence and functionality of the final product depends on
+ **Nonconstruction** activities include _problem/requirements definition, software architecture design, system testing, and maintenance_

#### 1.2 - Why is Software Construction Important?

+ Construction is a _large and central part of development_ (and the only part guaranteed to be performed)
+ Good construction techniques can improve developer productivity and code readability


### Quotes

> Requirements specification and design documents can go out of date, but the source code is always up to date.


### Takeaways

+ Informally, construction is "programming" - the central, most essential step in software development
+ Construction does not refer to the preliminary stages of software development (e.g., problem definition), nor the later stages (e.g., maintenance)
+ Doing construction well delivers a variety of benefits throughout the software lifecycle (such as easier maintenance and testing)



<a name="ch2"></a>
## Chapter 2 - Metaphors for a Richer Understand of Software Development

[(return to table of contents)](#toc)

### Summary

#### 2.1 The Importance of Metaphors

+ _Models_ are metaphors relating a less-understood topic to a better-understood one
    - Have often brought about breakthroughs or "paradigm shifts" in science
    - Can be overextended, and vary in their relevance/accuracy

#### 2.2 - How to Use Software Metaphors

+ Metaphors are _more like heuristics than algorithms_ (that is, they are not a definite route to a perfect solution, but may lead in the right direction)
+ In software development, metaphors are useful for _conceptualizing ways to approach new problems_ (or new ways to approach old problems)

#### 2.3 Common Software Metaphors

+ __Penmanship:__ might be an okay metaphor for very small programs written by an individual; beyond that, fails to convey the complexity of development, the software lifecycle, and the importance of planning
+ __Farming:__ attempts to express the value of incremental development ("growing" a system bit-by-bit); the metaphor doesn't extend well, mostly because code doesn't autonomously "grow", and is never "harvested"
+ __Accretion:__ like an oyster making a pearl; a better metaphor for incremental development than "farming"
+ __Building:__ both software and physical construction require different approaches and degrees of planning as they scale up in size and complexity; extensible to testing/inspections, importance of careful early design, and more
    - Better as a metaphor for linear "waterfall" models than iterative development (you can't just keep adding new features to a skyscraper with rapid turnaround time)


### Quotes

> In general, the power of models is that they're vivid and can be grasped as conceptual wholes. They suggest properties, relationships, and additional areas of inquiry.

> A software metaphor is more like a searchlight than a road map. It doesn't tell you where to find the answer; it tells you how to look for it.


### Takeaways

+ Metaphors serve as heuristics to help conceptualize potential solutions to software development problems
+ One of the best metaphors is building a physical structure, although many others are possible
    - The building metaphor highlights the importance of scale on the development process; the bigger the product, the more rigid the methodology



<a name="ch3"></a>
## Chapter 3 - Measure Twice, Cut Once: Upstream Prerequisites

[(return to table of contents)](#toc)

### Summary

#### 3.1 - Importance of Prerequisites

+ Construction is the middle portion of software development
    - Poorly done upstream prerequisites can already sink a project by this point, whereas proper preparation can expedite smooth construction
+ Preparation is often neglected because many programmers lack the required skills, or the team jumps to construction too early

#### 3.2 - Determine the Kind of Software You're Working On

+ In _sequential development_, the cost of neglected prerequisites is paid at end of the project; in _iterative development,_ it is paid throughout
+ Sequential approaches are appropriate when requirements are stable and well-understood; iterative approaches are appropriate when they are not

#### 3.3 - Problem-Definition Prerequisite

+ The problem definition is the foundation of development; it should not include solutions, only a _statement of the problem to be solved,_ in "user language"

#### 3.4 - Requirements Prerequisite

+ Explicit requirements ensure that users' needs are driving development, and provide a _basis for software functionality_
+ Fixing errors in requirements becomes more costly as the project progresses; thus, they should be well-specified early in development
+ Changes in requirements are typical; however, the _cost and necessity of changes should be considered_ before actually going through with them
+ Projects likely to have many requirements changes should consider an iterative or evolutionary development model

#### 3.5 - Architecture Prerequisite

+ The quality of software architecture can greatly impact how easy the product will be to construct and maintain
+ Like requirements, _changes to architecture become more costly as development proceeds_
+ Components of a system architecture include an overview, design rationale, module definitions, communication rules, user interface design, resource management plans, security considerations, performance/scalability estimates, I/O schemes, and error handling approach

#### 3.6 - Amount of Time to Spend on Upstream Prerequisites

+ About "10 to 20 percent" of effort and "20 to 30 percent" of time should go to upstream prerequisites
+ The time spent can vary based on size and formality of the project


### Quotes

> If the foundation hasn't been laid well or the planning is inadequate, the best you can do during construction is to keep damage to a minimum.


### Takeaways

+ Problem definition, requirements definition, and architecture design should all be considered before construction begins
    - More time should be spent on these prerequisites for larger, more complex, or more critical systems
+ Poorly-handled prerequisites are costly, causing construction to be done more slowly, or even done multiple times
+ The type of project determines whether a sequential or iterative is more appropriate (and likely less costly)



<a name="ch4"></a>
## Chapter 4 - Key Construction Decisions

[(return to table of contents)](#toc)

### Summary

#### 4.1 - Choice of Programming Language

+ Programmers are more productive in languages they are more familiar with (obviously)
+ High-level languages generally increase programmer productivity and code reliability

#### 4.2 - Programming Conventions

+ Adhering to style conventions at both the architectural and construction levels allows software to appear as a cohesive whole, rather than a messy conglomerate of different styles; this makes code easier to read and work with

#### 4.3 - Your Location on the Technology Wave

+ Late in a "technology wave", development practices are well-established, with well-tested, documented languages, compilers, and other tools
+ Early in a "technology wave", fewer languages and tools exist, with less documentation and more bugs

#### 4.4 - Selection of Major Construction Practices

+ Practices (e.g., defining conventions, solo/pair programming, version control, tests) should be decided on and documented before beginning construction


### Quotes

> One key to successful programming is avoiding arbitrary variations so that your brain can be free to focus on the variations that are really needed.


### Takeaways

+ Conventions for construction should be established before programming begins
+ Consider how well-suited the language of choice is for the project
+ Be aware of your location on the "wave" of whatever technologies are being used



<a name="ch5"></a>
## Chapter 5 - Design in Construction

[(return to table of contents)](#toc)

### Summary

#### 5.1 - Design Challenges

+ "Design" is the step between requirements definition and actual coding
+ When developing a high-level design, high-level tradeoffs (such as development time vs optimal performance) should be considered
+ Software designs typically entail lots of trial and error; they are iterative, evolutionary products

#### 5.2 - Key Design Concepts

+ A fundamental design challenge is managing the complexity of software solutions to real-world problems, such that the program's behavior is readily comprehensible and verifiably correct
    - The standard solution to managing complexity is modularity; complex systems should be composed of relatively independent subsystems (e.g., classes and packages) that interact in only necessary, well-specified ways
+ Characteristics of good designs include minimal complexity, ease of maintenance, loose coupling, easy extensibility/reusability, portability, and leanness
+ Five levels of design: software system design, division into subsystems/packages, division into classes, division into routines, and internal routine design

#### 5.3 - Design Building Blocks: Heuristics

+ **Object orientation** - identify attributes and behaviors of real-world objects, then use these as a model for software objects
+ **Abstraction** - safely "ignore" details within lower levels (e.g., routine and class interfaces)
+ **Encapsulation** - hide details within lower levels ("need to know" basis)
+ **Inheritance** - allows sharing of behavior/attributes between objects sharing some higher, "inherited" level of abstraction, while also allowing specific operations based on key differences
+ **Information hiding** - similar to encapsulation, in that both hide complexity
    - Bugs in properly hidden data/routines are more easily tracked down than bugs in code with unrestricted scope
    - In the design phase, information hiding consists of deciding which fields have what degree of visibility/access (e.g., public, private, protected, etc)
    - Worries about performance penalties incurred by indirect access routines are best postponed until performance can actually be measured - i.e., not in the design phase
+ **Change anticipation** - encapsulate and isolate parts of code that are likely to change (minimizes the effects of those changes on the system)
+ **Loose coupling** - coupling refers to how tightly related two or more classes/routines are; loose coupling refers to these links being "small, direct, visible, and flexible"
+ **Design patterns** - many software problems can benefit from existing solution patterns
    - Patterns provide abstractions and well-understood common ground
    - They consist of a set of possible pre-made design schemes, which is generally preferable to starting from scratch

#### 5.4 - Design Practices

+ **Iteration** - cycling through different potential designs, iteratively improving them, is a good way to balance both high and low-level concerns
+ **Divide and conquer** - focusing on subcomponents of the overall design one at a time can help manage complexity
+ **Top-down design** - start at a high level of abstraction (e.g., "base" classes), then work down to specifics
    - Starting with general classes, then iteratively decomposing them into more specific classes, is a divide and conquer process that reduces cognitive load at any one time
+ **Bottom-down design** - start at a low level of abstraction (e.g., specific objects), then generalize
    - Starting with specific, required low-level functions, then composing them into more generalized groupings may be easier for certain design challenges
+ **Prototyping** - to better understand the problem (and whether a potential solution is viable), create a minimal amount of throwaway code to test out the design
+ **Collaborative design** - consult with others, formally or informally, about the design (this could also be "collaboration" with oneself, via review of older work)
+ _Design documentation_ can be captured as in-code comments (especially class-level comments), in a dedicated Wiki, in written summaries, or in diagrams (anywhere from pictures of whiteboards to UML diagrams)

#### 5.5. - Comments on Popular Methodologies

+ Neither extensive up-front design of every detail, nor a complete lack of a design phase are appropriate; some balance should be struck, depending on the project


### Quotes

> The point of design is partly to create possibilities and partly to restrict possibilities.

> When a project reaches the point at which no one completely understands the impact that code changes in one area will have on other areas, progress grinds to a halt.


### Takeaways

+ The overarching goal of design is to manage complexity cleanly, making the rest of construction easier and less error-prone
+ Modularity, information hiding, and encapsulation are key considerations in any complex design



<a name="ch6"></a>
## Chapter 6 - Working Classes

[(return to table of contents)](#toc)

### Summary

#### 6.1 - Class Foundations: Abstract Data Types (ADTs)

+ An _abstract data type (ADT)_ is a collection of data and operations on that data
+ ADTs can hide implementation details from parts of the program that don't need to know about them; this makes changing internal details relatively painless
+ ADTs gather related code in one place, making it easier to design a clean interface, assess correctness, and improve performance
+ ADTs facilitate self-documentating code; a clean interface should include clear, unambiguous routine names
+ As their name implies, ADTs provide a level of abstraction that allow the rest of the program to work with data at a higher, "real-world" level, without undue concern over the implementation
    - For example, a program can call `dequeue()` on a queue ADT (treated as a real-world queue) without giving much thought to the underlying code
    - Of course, the caller may be concerned about implementation in terms of performance/efficiency on various metrics
+ ADTs can also be layered, however, such that a general-purpose "stack" ADT could underlie a real-world "employees" ADT
+ ADTs can be implemented in non-object-oriented languages, although interfacing with them becomes more complex
+ A class is an "abstract data type plus inheritance and polymorphism"

#### 6.2 - Good Class Interfaces

+ Class interface methods should be a cohesive group of methods with names using abstract language (e.g., `addEmployee()` instead of `pushEmployeeToStack()`
+ The interface should deal with only one level of abstraction
+ Accessibility - users should not be able to access any more fields or methods than necessary

#### 6.3 - Design and Implementation Issues

+ Containment ("has a" relationships) should be implemented by making one object a member of another
+ Inheritance ("is a" relationships) should be implemented through subclassing
    - Issues to consider: visibility of methods/members to subclasses; default implementations of methods; `final` members; `abstract` members; overly deep inheritance hierarchies

#### 6.4 - Reasons to Create a Class

+ **Modeling real-world objects**
+ **Modeling abstract objects** - such as generalizations of a class of concrete, real-world objects
+ **Reducing/isolating complexity** - once the implementation of a class is written, its abstract API can be used, allowing implementation details to be "ignored"
+ **Limiting the effects of changes** - if encapsulation is done well, changes to implementation should be localized
+ **Hiding global data** - accessing global data through methods encapsulates it better, insulating user classes from implementation changes

#### 6.5 - Language-Specific Issues

+ These include whether methods are "virtual" by default, whether methods can be overridden, and whether operators can/should be overridden

#### 6.6 - Beyond Classes: Packages

+ Grouping related classes into the same namespace is generally good practice - it's just modularity at a higher level

### Quotes

> A key to being an effective programmer is maximizing the portion of a program that you can safely ignore while working on any one section of code. Classes are the primary tool for accomplishing that objective.


### Takeaways

+ Classes should present a clean, user-friendly API at a single level of abstraction
+ Classes should be leveraged to hide implementation details as much as possible, to insulate other classes from internal changes
+ Classes are for managing complexity; they should be designed with that goal in mind



<a name="ch31"></a>
## Chapter 31 - Layout and Style

[(return to table of contents)](#toc)

### Summary

#### 31.1 - Layout Fundamentals

+ Well-formatted code is code that highlights the logical structure of the program
    - Programmers don't typeset; they program; so, logical readability trumps how "pretty" the text looks
+ Formatting (especially whitespace) should reflect the intent of the code
    - Blocks should be indented properly
    - Operands in complex expressions should be grouped according to the order of operations

#### 31.2 - Layout Techniques

+ Use blank lines to separate logically-related groups of code within a routine
    - Also use blank lines to separate routines, classes, and other high-level structures
+ Indent code consistently with the logical blocks that contain them
    - Studies show that two-to-four space indents are the most readable
+ Use parentheses to express the intent of complex expressions, even when not required by the order of operations

#### 31.3 - Layout Styles

+ _This section outlines several block indentation styles; they are not all repeated here - just pick one that is conventional to the language/company/team/project, and use it consistently_

#### 31.4 - Laying Out Control Structures

+ Place blank lines between logical blocks, not only blocks defined by control structures
+ Use a consistent style for one-statement blocks (e.g., an if statement with only one line)
+ Break long conditionals into multiple lines in whichever way best conveys the intent and makes any mistakes obvious (e.g., line up similar checks vertically)

#### 31.5 - Laying Out Individual Statements

+ Line length limits (e.g., 80 characters) help avoid overly long lines and overly nested blocks
+ Use spaces to make statements more readable; avoid overly dense blocks of text
+ Put each statement on its own line, even if the language allows multiple statements to share one line
+ Avoid including operations with side-effects in another line of code
    - A common example of this is the increment and decrement operators (`++` and `--`), which are often hidden within other lines, where they can be easily overlooked; additionally, the order of evaluation may cause undue confusion
+ One variable declaration per line is generally the easiest to read, as are declarations grouped by type

#### 31.6 - Laying Out Comments

+ Indent comments so they're flush with the corresponding code
+ Surrounding comments (on top and/or bottom) with blank lines can help readability

#### 31.7 - Laying Out Routines

+ Use blank lines to separate logically separate blocks within routines

#### 31.8 - Laying Out Classes

+ A standard order to present class members in is: header comment, constructors/destructors, public routines, protected routines, private routines
    - Alternatively, private routines could be placed near the public routines that use them
+ Clearly identify separate classes, if multiple exist within the same file
+ Preferably, put each class in its own file, and name that file in correspondence with the class it contains
+ Use at least two blank lines to separate top-level members of a class (e.g., routines)


### Quotes

> ...the details of a specific method of structuring a program are much less important than the fact that the program is structured consistently.


### Takeaways

+ The overarching goal of layout and formatting is to make the logical structure and intent obvious
+ Following a convention consistently is vital; inconsistent formatting makes code less readable

