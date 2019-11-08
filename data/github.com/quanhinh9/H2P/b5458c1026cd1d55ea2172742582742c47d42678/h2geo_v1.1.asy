//Một số hàm asymptote.
//Version 1.1 of 09/20/2019
//Quân.T nhóm Quán Hình. email: quanhinh9@gmail.com
//https://www.facebook.com/groups/205466756603509/
//http://asymptote.sourceforge.net/doc/index.html

//Phép vị tự

transform h2scale(pair center, real k)
{
	return shift(center)*scale(k)*shift(-center);
}

//Phép nghịch đảo

pair h2inverse(pair O, real k, pair M)
{
	return (O + k*unit(M-O)/abs(M-O));
}

//Đường thẳng AB
path h2line(pair A=(0,0), pair B=(0,0), real a=0.1, real b=a)
{
	return (a*(A-B)+A)--(b*(B-A)+B);
}

//Cung AB + 10^o
path h2arc(pair O, pair A, pair B, int a=10, real b=a)
{
//	pair A1=rotate(-a,O)*A;
//	pair B1=rotate(b,O)*B;
	return arc(O,rotate(-a,O)*A,rotate(b,O)*B);
}

//Trọng tâm G
pair h2centroid(pair A,pair B,pair C)
{
	return (A+B+C)/3;
}

//Hình chiếu vuông góc của điểm P lên AB
pair h2project(pair P, pair A, pair B)
{
	return midpoint(P--reflect(A,B)*P);
}

//Trực tâm H của tam giác ABC
pair h2orthocenter(pair A, pair B, pair C)
{
	return extension(B, reflect(C,A)*B, C, reflect(A,B)*C);
}

//Đường tròn tâm O, bán kính OA	
path h2circle1p(pair O, pair A)
{
	return circle(O,abs(O-A));
}

//Đường tròn đường kính AB
path h2circle2p(pair A, pair B)
{
	return circle(midpoint(A--B),abs(midpoint(A--B)-A));
}

//Tâm ngoại tiếp O của tam giác ABC
pair h2center3p(pair A, pair B, pair C)
{
	pair mAB=midpoint(A--B);
	pair mAC=midpoint(A--C);
	return extension(mAB, rotate(90,mAB)*A, mAC, rotate(90,mAC)*A);
}

//Đường tròn ngoại tiếp của tam giác ABC
path h2circle3p(pair A, pair B, pair C)
{
	pair O=h2center3p(A,B,C);
	return circle(O, abs(O-A));
}


//Tâm nội tiếp I
pair h2incenter(pair A, pair B, pair C)
{
	return extension(A, A+dir(A--B,A--C), B, B+dir(B--A,B--C));
}

//Đường tròn nội tiếp của tam giác ABC
path h2incircle(pair A, pair B, pair C)
{
	pair I=h2incenter(A,B,C);
	return circle(I, abs(I-h2project(I,B,C)));
}

//Tâm bàng tiếp Ia
pair h2excenter(pair A, pair B, pair C)
  {
   	return extension(B, B+rotate(90)*dir(B--A,B--C),C, C+rotate(90)*dir(C--A,C--B));
  }

// Đường tròn bàng tiếp
path h2excircle(pair A, pair B, pair C)
{
	pair Ia=h2excenter(A,B,C);
	return circle(Ia, abs(Ia-h2project(Ia,B,C)));
}  
  
//Điểm liên hợp đẳng giác của P đối với tam giác ABC
pair h2isoconj(pair P, pair A, pair B, pair C)
{
	pair I=h2incenter(A,B,C);
	return extension(B, reflect(B,I)*P, C, reflect(C,I)*P);
}  

//Tâm đường tròn Mixtilinear nôi
pair h2inmixcenter(pair A, pair B, pair C)
{
	pair I=h2incenter(A, B,C);
	pair Z=extension(A, B, I, rotate(90,I)*A);
	return extension(A, I, Z, rotate(90,Z)*A);
}

//Tiếp điểm của đường tròn Mixtilinear nội
pair h2inmixt(pair A, pair B, pair C)
{
	pair I=h2incenter(A,B,C);
	pair P=midpoint(I--h2excenter(A,B,C));
	pair O=h2center3p(A,B,C);
	pair Q=rotate(180,O)*P;
	return extension(Q, I, O, h2inmixcenter(A,B,C));
}

//Đường tròn Mixtilinear nội
path h2inmixcircle(pair A, pair B, pair C)
{
	pair I=h2incenter(A, B,C);
	pair Z=extension(A, B, I, rotate(90,I)*A);
	pair K=extension(A, I, Z, rotate(90,Z)*A);
	return circle(K, abs(K-Z));
}

//Tâm Mixtilinear ngoại
pair h2exmixcenter(pair A, pair B, pair C)
{
	pair Ia=h2excenter(A, B,C);
	pair Z=extension(A, B, Ia, rotate(90,Ia)*A);
	return extension(A, Ia, Z, rotate(90,Z)*A);
}

//Tiếp điểm Mixtilinear ngoại

pair h2exmixt(pair A, pair B, pair C)
{
	pair Ia=h2excenter(A, B,C);
	pair P=midpoint(h2incenter(A,B,C)--Ia);
	pair O=h2center3p(A,B,C);
	pair Q=rotate(180,O)*P;
	return extension(Q,Ia,O,h2exmixcenter(A,B,C));
}

//Đường tròn Mixtilinear ngoại
path h2exmixcircle(pair A, pair B, pair C)
{
	pair Ia=h2excenter(A,B,C);
	pair Z=extension(A, B, Ia, rotate(90,Ia)*A);
	pair Ka=extension(A, Ia, Z,rotate(90,Z)*A);
	return circle(Ka, abs(Ka-Z));
}

//Điểm Feuerback nội Fe
pair h2fe(pair A, pair B, pair C)
{
	pair M=midpoint(B--C);
	pair I=h2incenter(A, B,C);
	pair D=midpoint(I--reflect(B,C)*I);
	pair D1=reflect(A,I)*D;
	return reflect(I, reflect(M,D1)*I)*D1;
}

//Điểm Feuerback ngoại Fa
pair h2feex(pair A, pair B, pair C)
{
	pair M=midpoint(B--C);
	pair Ia=h2excenter(A, B,C);
	pair Da=midpoint(Ia--reflect(B,C)*Ia);
	pair D1=reflect(A,Ia)*Da;
	return reflect(Ia, reflect(M,D1)*Ia)*D1;
}

//Gergonne Point Ge
pair h2ge(pair A,pair B,pair C)
{
	pair I=h2incenter(A,B,C);
	return extension(B, h2project(I,C,A), C, h2project(I,A,B));
}

//Nine Point Center N9
pair h2n9(pair A,pair B,pair C)
{
	return midpoint(h2orthocenter(A,B,C)--h2center3p(A,B,C));
}

//Đường tròn Nine Point - Euler
path h2n9circle(pair A, pair B, pair C)
{
	pair N9=h2n9(A,B,C);
	return circle(N9,abs(N9-midpoint(B--C)));
}

//Nagel Point Na
pair h2na(pair A,pair B,pair C)
{
	pair E=h2project(h2excenter(B,C,A),C,A);
	pair F=h2project(h2excenter(C,A,B),A,B);
	return extension(B, E, C, F);
}

//Spieker Point Sp
pair h2sp(pair A,pair B,pair C)
{	
	pair Ma=midpoint(B--C);
	pair Mb=midpoint(C--A);
	pair Mc=midpoint(A--B);
	return h2incenter(Ma, Mb, Mc);
}

//Kosnita Point Ka
pair h2ka(pair A,pair B,pair C)
{	
	pair O=h2center3p(A,B,C);
	return extension(B, h2center3p(O,C,A), C, h2center3p(O,A,B));
}

//Schiffler Point Sc
pair h2sc(pair A,pair B,pair C)
{	
	pair I=h2incenter(A,B,C);
	return extension(h2orthocenter(A,B,C), h2center3p(A,B,C), h2orthocenter(I,B,C), h2center3p(I,B,C));
}

//Lemoine Point Le
pair h2le(pair A,pair B,pair C)
{	
	return h2isoconj(h2centroid(A,B,C), A, B, C);
}

//Humpty Point Hm
pair h2hm(pair A, pair B, pair C)
{
	return h2project(h2orthocenter(A,B,C), A, midpoint(B--C));
}

//Mittenpunkt Point Mi
pair h2mi(pair A,pair B, pair C)
{
	pair G=h2centroid(A,B,C);
	pair Ge=h2ge(A,B,C);
	return h2scale(Ge, 3/2)*G;
}

//Điểm Steiner của tam giác ABC
//https://en.wikipedia.org/wiki/Steiner_point_(triangle)
pair h2steiner(pair A,pair B, pair C)
{
	pair O=h2center3p(A,B,C);
	pair Le=h2le(A,B,C);
	pair Oa=reflect(B,C)*O; pair La=reflect(B,C)*Le;
	pair Ob=reflect(C,A)*O; pair Lb=reflect(C,A)*Le;
	pair Oc=reflect(A,B)*O; pair Lc=reflect(A,B)*Le;
	pair E=extension(Oa, La, Oc, Lc);
	pair F=extension(Oa, La, Ob, Lb);
	return extension(B, E, C, F);
}

//Điểm Anti-Steiner của P đối với tam giác ABC
pair h2antisteiner(pair P,pair A,pair B,pair C)
{
	pair H=h2orthocenter(A,B,C);
	return extension(reflect(C,A)*H, reflect(C,A)*P, reflect(A,B)*H, reflect(A,B)*P);
}

//Tâm của đường inconic
pair h2inconiccenter(pair P,pair A, pair B, pair C)
{
	pair D=extension(A,P,B,C);
	pair E=extension(B,P,C,A);
	pair F=extension(C,P,A,B);
	return extension(B, midpoint(D--F), C, midpoint(D--E));
}
