%%%% CSci 117, Lab 9 %%%%

% Answer written questions within block comments, i.e. /* */
% Answer program related questions with executable code (executable from within the Mozart UI) 

% Note: While many of these questions are based on questions from the book, there are some
% differences; namely, extensions and clarifications. 

/* Question 1: Rewrite the function SumList, and the function ScanL, 
where the state is stored in a memory cell when the function is called, 
and a helper function performs the recursive algorithm. */

declare
fun {SumList L}
  A = {NewCell 0}
  fun {SumListH L}
    % Your code goes here
     case L of nil then @A
     [] X|Xr then 
	A := @A + X
	{SumListH Xr}
     end	
  end
in
  {SumListH L}
end
{Browse {SumList [1 2 3]}}


/* ScanL will be handled similarly, 
except the initial value of your memory cell A will be the Z value passed into the function */
declare
fun {Add X Y}
   X+Y
end
fun {ScanL L F Z}
   A = {NewCell Z}
   fun {ScanH L}
      %Your code goes here
      @A|case L of nil then nil
	 [] X|Xr then
	    A := {F @A X}
	    {ScanH Xr}
	 end
   end
in
   {ScanH L}
end
{Browse {ScanL [1 2 3] Add 0}}


/* Question 2: Assuming a memory cell A points to a list of integers, 
write a procedure that sums this list and assigns the sum to A. 
You are only allowed to use a single memory cell in your procedure. */

declare
A = {NewCell [0 2 4 6 1 3]}
proc {SumL A}
  B = {NewCell @A#0} % Initialize B with some value
  proc {SumLHelp}
    % SumList algorithm code, which only has access to memory cell B, you cannot use A in this procedure
     case @B of nil#Z then B:=Z
     [] (X|Xs)#Z then
	B := Xs#(Z+X)
	{SumLHelp}
     end
  end
in
 {SumLHelp} % B is now pointing to the sum of @A
 A := @B    % @A is now the value of its former list, summed
end

{SumL A}
{Browse @A} % will print 16




/* Question 3: Assuming a memory cell A points to a list of integers, 
write a procedure that reverses this list and assigns the reversed list to A. 
You are only allowed to use a single memory cell in your procedure. 
This will be handled similarly to Question 2, except your initialization of B will be different. */
declare
A = {NewCell [0 2 4 6 1 3]}
proc {RevL A}
  B = {NewCell @A#nil} % Initialize B with some value
  proc {RevLHelp}
    % RevList algorithm code, which only has access to memory cell B, you cannot use A in this procedure
     case @B of nil#Z then B:=Z
     [] (X|Xs)#Z then
	B := Xs#(X|Z)
	{SumLHelp}
     end
  end
in
 {RevLHelp} % B is now pointing to the reverse list of @A
 A := @B    % @A is now the value of its former list, reversed
end

{RevL A}
{Browse @A} % will print reverse of A




/* Question 4: Rewrite the functional stream that generates the numbers
starting form 0 then adding one up to infinity, (0 1 2 3 …), 
but instead use a local memory cell, such that {Generate} will return a zero argument function, 
and executing that zero argument function gives the next value in the stream. */

declare
fun {Generate}
   A = {NewCell 0}
   fun {Accum}
      A:=@A+1
   end
in
   Accum
end

% For example,
GenF = {Generate}
{Browse {GenF}} % outputs 0
{Browse {GenF}} % outputs 1
{Browse {GenF}} % outputs 2



/* Question 5: Return to Nested List Flattening. */
/* (a) Use a memory cell to count the number of list creation operations i.e. when ‘|’ is used, 
within the two versions of flattening a nested list from lab 5. */


declare
A = {NewCell 0}
fun {Append Xs Ys}
   case Xs
   of nil then Ys
   [] X|Xr then A:=@A+1
      X|{Append Xr Ys}
   end
end

declare
fun {Flatten Xs} 
  case Xs
  of nil then nil
  [] X|Xr andthen {IsList X} then
    {Append {Flatten X} {Flatten Xr}} 
  [] X|Xr then
     A:=@A+1
    X|{Flatten Xr}
  end 
end
local Y = {Flatten [[1 2 3] [1 2] [1 2 [2 3 4]] 3 4]} in
   {Browse @A}
end

declare
B = {NewCell 0}
fun {Flatten Xs}
  proc {FlattenD Xs ?Ds}
    case Xs
    of nil then Y in Ds=Y#Y
    [] X|Xr andthen {IsList X} then Y1 Y2 Y4 in
      Ds=Y1#Y4 
      {FlattenD X Y1#Y2}
      {FlattenD Xr Y2#Y4}
    [] X|Xr then Y1 Y2 in
       B:=@B+1
      Ds=(X|Y1)#Y2 {FlattenD Xr Y1#Y2}
    end 
  end Ys
  in {FlattenD Xs Ys#nil} Ys
end
local X = {Flatten [[1 2 3] [1 2] [1 2 [2 3 4]] 3 4]} in
   {Browse @B}
end

/* (b) Verify that your program is correct by running the example [[1 2 3] [1 2] [1 2 [2 3 4]] 3 4] from lab 5, 
												       along with three other examples of your choosing. */
/*
List: [[1 2 3] [1 2] [1 2 [2 3 4]] 3 4]
First: 25
Second: 12

List: [1 2 [3 4] [5 6 [7] 8]]
First: 15
Second: 8

List: [1 2 3 4]
First: 4
Second: 4

List: [[1] [2] [3] 4 [5 6]]
First: 11
Second: 6
*/


/* (c) Create a function that takes in a list of nested lists, 
and returns the average for both flatting function of list creation operations for these nested lists. 
Test this on the list containing all possible nested lists of 3 elements with nesting depth 2,
 i.e., [[1 2 3]  [[1] 2 3]  [[1] [2] 3] … and give the average for both of the flattening functions. */
declare
A = {NewCell 0}
fun {Append Xs Ys}
   case Xs
   of nil then Ys
   [] X|Xr then A:=@A+1
      X|{Append Xr Ys}
   end
end

fun {Flatten Xs} 
  case Xs
  of nil then nil
  [] X|Xr andthen {IsList X} then
     A:=@A+1
     {Append {Flatten X} {Flatten Xr}}
  [] X|Xr then
     A:=@A+1
    X|{Flatten Xr}
  end 
end
local Y = {Flatten [[1 2 3]  [[1] 2 3]  [[1] [2] 3] [[1] 2 [3]] [[1] [2] [3]] [1 2 [3]] [1 [2] [3]] [1 [2] 3] [[1 2] 3] [[1 2] [3]] [1 [2 3]] [[1] [2 3]] [[1 2 3]]]} in
   {Browse @A}
   {Browse {Int.toFloat @A}/{Int.toFloat 13}} 
end

/*

-Total: 135
-Avg:  10.385

*/

declare
B = {NewCell 0}
fun {Flatten Xs}
  proc {FlattenD Xs ?Ds}
    case Xs
    of nil then Y in Ds=Y#Y
    [] X|Xr andthen {IsList X} then Y1 Y2 Y4 in
      Ds=Y1#Y4 
      {FlattenD X Y1#Y2}
      {FlattenD Xr Y2#Y4}
    [] X|Xr then Y1 Y2 in
       B:=@B+1
      Ds=(X|Y1)#Y2 {FlattenD Xr Y1#Y2}
    end 
  end Ys
  in {FlattenD Xs Ys#nil} Ys
end
local X = {Flatten [[1 2 3]  [[1] 2 3]  [[1] [2] 3] [[1] 2 [3]] [[1] [2] [3]] [1 2 [3]] [1 [2] [3]] [1 [2] 3] [[1 2] 3] [[1 2] [3]] [1 [2 3]] [[1] [2 3]] [[1 2 3]]]} in
   {Browse @B}
   {Browse {Int.toFloat @B}/{Int.toFloat 13}}
end


/*

-Total: 39
-Avg: 3

*/
