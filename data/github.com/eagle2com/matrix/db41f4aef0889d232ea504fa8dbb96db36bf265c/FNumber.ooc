//this class can use numbers as fractions =)

FNumber: class {
	
	up,down: Int   //upper and lower part of the fraction
	
	init: func {
		init(0,1)
	}
	
	init: func ~vals(=up,=down) {
	}
	
	init: func ~val(=up) {
		down = 1
	}
	
	print: func {
		if(down == 1)
			printf("%d",up)
		else
			printf("%d/%d",up,down)
	}
	
	reduce: func {
		if ( up % down == 0 ) {
			up /= down
			down = 1
		}
	}
	
}


operator +(f1,f2: FNumber) -> FNumber {
	if(f1 down == f2 down) {
		return FNumber new(f1 up + f2 up,f1 down)
	} 
	
	ret := FNumber new(f1 up * f2 down + f2 up * f1 down,f1 down * f2 down)
	if ( ret up % ret down == 0 ) {
		ret up /= ret down
	}
	return ret
}

operator -(f1,f2: FNumber) -> FNumber {
	if(f1 down == f2 down) {
		return FNumber new(f1 up - f2 up,f1 down)
	} 
	
	ret := FNumber new(f1 up * f2 down - f2 up * f1 down,f1 down * f2 down)
	ret reduce()
	return ret
}

operator *(f1,f2: FNumber) -> FNumber {
	ret := FNumber new(f1 up * f2 up,f1 down * f2 down)
	ret reduce()
	return ret
}

operator /(f1,f2: FNumber) -> FNumber {
	ret := FNumber new(f1 up * f2 down,f1 down * f2 up)
	ret reduce()
	return ret
}

operator *(f1: FNumber, n: Int) -> FNumber {
	ret := FNumber new(f1 up * n,f1 down)
	ret reduce()
	return ret
}

operator /(f1: FNumber, n: Int) -> FNumber {
	ret := FNumber new(f1 up, f1 down * n)
	ret reduce()
	return ret
}
