module main

// This is a typical comment

/*
This is a multiline
comment
*/

/* main returns a void 
as it is the entry point of the program
*/
fn main() {
	println("This is V Hello World Sample")
	m := multi(4,5)
	d := div(16,  4)
	println("The result of multiplication is: $m")
	println("The result of division is: $d")
}

/* multi returns the integer result of a & b
*/
fn multi(a int, b int) int {
	return a * b
}

/* div returns the non-zero result of a / b where a > b
*/
fn div(a int, b int) int {
	if a < b  || b == 0 {
		return 0
	} else {
		return a / b
	}
}