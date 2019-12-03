#include <platform.h>
#include <xs1.h>
#include <stdio.h>

main() {
	streaming chan x, y;
//	chan x, y;
	par{
		on stdcore[0]: {
			int a,b;
			x <: 0;
			y <: 1;
			x :> a;
			y :> b;
			if (a != 2 || b !=3) {
				printf("FAIL\n");
			} else {
			x <: 0;
			y <: 1;	
			x :> a;	
			y :> b;
			printf("SUCCES\n");
			}
		}
		on stdcore[3]: {
			int a, b;
			x <: 2;
			y <: 3;
			x :> a;
			y :> b;	
			if (a != 0 || b !=1) {
				printf("FAIL\n");
			}
			x <: 2;
			y <: 3;
			x :> a;
			y :> b;	
		}
	}
}
