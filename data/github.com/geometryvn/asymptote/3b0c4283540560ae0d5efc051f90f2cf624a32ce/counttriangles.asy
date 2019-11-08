import geometry;
//import g2geo;
unitsize(1cm);
defaultpen(fontsize(10pt));

bool g2checkips(segment s1, segment s2)
{
	bool temp;
	point P=intersectionpoint(s1, s2);
	if (defined(P)) temp=true;
	return temp;
}	

bool g2t(segment s1, segment s2, segment s3){
	bool temp;
	point P1 = intersectionpoint(s2, s3);
	point P2 = intersectionpoint(s3, s1);
	point P3 = intersectionpoint(s1, s2);
			
	if (defined(P1) && defined(P2) && defined(P3) && (P1!=P2) && (P1!=P3) && (P2!=P3) && (P3@line(P1,P2)==false) ) temp=true; 
	return temp;
}
	
int g2countriangle(segment[] s){
	int temp=0;
	for (int i=0; i<s.length; ++i)
		for (int j=i+1; j<s.length; ++j)
			for (int k=j+1; k<s.length; ++k){
				if (g2t(s[i],s[j],s[k])){
					++temp;
				}
		}
	return temp;	
}
	
real x=8;
	
point A=(0,x); dot(Label("$A$",align=NW),A);
point B=(0,0); dot(Label("$B$",align=SW),B);
point C=(x,0); dot(Label("$C$",align=SE),C);
point D=(x,x); dot(Label("$D$",align=NE),D);
point X=midpoint(A--B); dot(Label("$X$",align=NW),X);
point Y=midpoint(B--C); dot(Label("$Y$",align=SE),Y);
point Z=midpoint(C--D); dot(Label("$Z$",align=NE),Z);
point T=midpoint(D--A); dot(Label("$T$",align=NW),T);
	
segment[] s;
	
s[0] = segment(A,B); s[1] = segment(B,C); s[2] = segment(C,D); s[3] = segment(D,A);
s[4] = segment(A,Y); s[5] = segment(A,Z); s[6] = segment(B,Z); s[7] = segment(B,T);
s[8] = segment(C,T); s[9] = segment(C,X); s[10] = segment(D,X); s[11] = segment(D,Y);
	
for (int i=0; i<s.length; ++i){
	draw(s[i]);
}
	
dot(Label("Triangles: "+string(g2countriangle(s)),align=SE),(0,-1));
	
for (int i=0; i<s.length; ++i)
	for (int j=i+1; j<s.length; ++j){
		if (g2checkips(s[i],s[j])){
			point temp=intersectionpoint(s[i],s[j]);
			dot(temp,Fill(white));
		}
	}
