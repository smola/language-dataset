F:=Open("input11.txt","r");
s:=StringToInteger(Gets(F));
id:=10;
r:=300;

g:=[[((((x+id)*y+s)*(x+id) div 100) mod 10)-5:x in [1..r]]:y in [1..r]];

p:=[];
for y in [1..r] do
	p[y]:=[];
	for x in [1..r] do
		p[y,x]:=y eq 1 select g[y,x] else (x eq 1 select g[y,x]+p[y-1,x] else g[y,x]+p[y-1,x]+p[y,x-1]-p[y-1,x-1]);
	end for;
end for;

f:=[<x,y,l,p[y+l-1,x+l-1]-(y gt 1 select p[y-1,x+l-1] else 0)-(x gt 1 select p[y+l-1,x-1] else 0)+(y gt 1 and x gt 1 select p[y-1,x-1] else 0)>:y in [1..r-l+1], x in [1..r-l+1], l in [1..r]];

function find_coord(f:min:=1,max:=r)
	best:=0;
	for c in f do
		if c[3] ge min and c[3] le max and c[4] gt best then
			x:=c[1];
			y:=c[2];
			radius:=c[3];
			best:=c[4];
		end if;
	end for;
	return x,y,radius,best;
end function;

p1x,p1y:=find_coord(f:min:=3,max:=3);
PrintFile("day11.txt",Sprintf("%o,%o",p1x,p1y));
p2x,p2y,p2r:=find_coord(f);
PrintFile("day11.txt",Sprintf("%o,%o,%o",p2x,p2y,p2r));
