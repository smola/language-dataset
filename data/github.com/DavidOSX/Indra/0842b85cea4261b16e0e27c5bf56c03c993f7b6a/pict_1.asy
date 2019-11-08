import math;
import settings;
outformat = "pdf";
//==NUMBER OF LEVELS==
int N_ITER = 5;
//====================
struct circle { 
    pair C; 
    real r; 
    void operator init(pair p, real R) {
    this.C=p;
    this.r=R;
    }
}

struct mob_matrix { pair a; pair b; pair c; pair d; 
    void operator init(pair A, pair B,pair C,pair D) {
    this.a=A;
    this.b=B;
    this.c=C;
    this.d=D;
    }
}

real cxabs(pair z) { return sqrt(z.x*z.x+z.y*z.y); }

pair TxPoint(mob_matrix T, pair p) { return (T.a*p + T.b)/(T.c*p+T.d); }

circle TxCirc(mob_matrix T, circle c) {
    circle res;
    pair z;
    z=c.C-(c.r^2,0)/conj(T.d/T.c+c.C);
    res.C=TxPoint(T,z);
    res.r=cxabs(res.C-TxPoint(T,c.C+c.r));
    return res;
}

void constrWords(int[] m,int level, int mlen, mob_matrix[] mm,pen[] colors, circle[] circ) {
     int i,k,l,s,j; circle[] tmp;
     for(k = 1; k < 5;++k) { i = m.pop();
	   if(i == (k%4))  {
		m.push((int)k%4);
		
		if(level < mlen) { ++level;
			m.push((int) (k-1)%4); constrWords(m,level,mlen,mm,colors,circ);
			m.push((int) k%4); constrWords(m,level,mlen,mm,colors,circ);
			m.push((int)(k+1)%4); constrWords(m,level,mlen,mm,colors,circ);
		 }
		 else { for(i=0;i<4;++i) tmp.push(circ[i]);
		 for(l=0;l<m.length;++l) { s = m.pop(); if(l==0) j = s;
		    for(i=j-2;i<j+1;++i) { 
		      circ[i%4] = TxCirc(mm[(s-1)%4],circ[i%4]); if(circ[i%4].r > (real)10^(-4)) radialshade(shift(circ[i%4].C)*scale(circ[i%4].r)*unitcircle,white,circ[i%4].C,circ[i%4].r/2,colors[(l+1)%colors.length],circ[i%4].C,circ[i%4].r); 
		     } m.insert(0,s);
		  } for(i=3;i>-1;--i) circ[i] = tmp.pop();
		 }
        } else m.push(i);
    } m.pop();
    //for(k = j-2;k < j+1; ++k) circ[k%4] = TxCirc(mm[(i+1)%4],circ[k%4]);
}

size(10cm);
//real theta = asin(1/sqrt(2));
real p = 0.5; real q = 0.9;
real c = sqrt(1+p^2); real d = sqrt(1+q^2);
real k = (1 + sqrt(1-p^2*q^2))/(p*q); 
int i; 
int j;
int[] m;
circle[] circ;
mob_matrix[] mm;
pen[] colors = {red,blue,green,cyan,magenta,yellow};
circle C_B = circle((-c/p,0),1/p);
circle C_a = circle((0,k*d/q),k/q);
circle C_A = circle((0,-k*d/q),k/q);
circle C_b = circle((c/p,0),1/p);
//real[] centas; //real[] radiuses;
mob_matrix a = mob_matrix((d,0),(0,k*q),(0,(-1)*q/k),(d,0));
mob_matrix b = mob_matrix((c,0),(p,0),(p,0),(c,0));
mob_matrix A = mob_matrix((d,0),(0,(-1)*k*q),(0,q/k),(d,0));
mob_matrix B = mob_matrix((c,0),((-1)*p,0),((-1)*p,0),(c,0));

radialshade(shift(C_B.C)*scale(C_B.r)*unitcircle,white,C_B.C,C_B.r/2,red,C_B.C,C_B.r);
radialshade(shift(C_a.C)*scale(C_a.r)*unitcircle,white,C_a.C,C_a.r/2,red,C_a.C,C_a.r);
radialshade(shift(C_A.C)*scale(C_A.r)*unitcircle,white,C_A.C,C_A.r/2,red,C_A.C,C_A.r);
radialshade(shift(C_b.C)*scale(C_b.r)*unitcircle,white,C_b.C,C_b.r/2,red,C_b.C,C_b.r);
// draw(shift(C_B.C)*scale(C_B.r)*unitcircle,red);
// draw(shift(C_a.C)*scale(C_a.r)*unitcircle,red);
// draw(shift(C_A.C)*scale(C_A.r)*unitcircle,red);
// draw(shift(C_b.C)*scale(C_b.r)*unitcircle,red);
mm.push(a); mm.push(b); mm.push(A); mm.push(B); 
circ.push(C_a); circ.push(C_b); circ.push(C_A); circ.push(C_B);  //write(c); write(d);
for(i=1; i<5; ++i) { m.push((int)i%4); constrWords(m,1,N_ITER,mm,colors,circ);  }
