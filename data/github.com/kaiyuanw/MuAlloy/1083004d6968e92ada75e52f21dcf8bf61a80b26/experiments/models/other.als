/*
Further simplification of Dan's work
access is granted to all assigned groups. 
problem: assigned groups defined as *at least* alas and peds.
fix: assigned groups definded as *only* alas and peds.
*/

//people
sig Person {
   member_of : some Group
}
pred CanEnter(p: Person, r:Room) {
	p.member_of in r.located_in
}

// groups 
sig Group {}
one sig alas extends Group {}  
one sig peds extends Group {}

//rooms
sig Room {
  located_in: set Group
}
one sig seclab extends Room {}
// the problem; this permits, but doesn't restrict
fact {
  alas in seclab.located_in and peds in seclab.located_in
} 

// assertion
assert no_thief_in_seclab {
   all p : Person | CanEnter[p, seclab] implies alas in p.member_of or peds in p.member_of
}
check no_thief_in_seclab

/*
The specification
-----------------

We consider 3 classes:
- Persons
- Groups
- Rooms

There are two groups in particular: the systems and logic groups. 
There is one room in particular: the secure_lab.

We make the following assumptions:

1. Each person is a member_of some Group.

2. For each Room there is a set of Groups that are located_in it.

3. At least, the systems and logic groups are located_in the secure_lab.

4. A Person can_enter a Room only if the Person is a member_of a Group located_in the Room.

The assertion
--------------
 
Here is the assertion that we thought would be true:

* Each person that can_enter the secure_lab is a member_of the logic or the systems Group. 

--------------------------------------------------------------
*/
