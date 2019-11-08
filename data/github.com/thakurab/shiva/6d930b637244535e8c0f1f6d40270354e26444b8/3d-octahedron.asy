settings.outformat = "pdf";
settings.render = 16;
size(300);
import solids;

currentprojection=orthographic (
camera=(8,5,4),
up=(0,0,1),
target=(2,2,2),
zoom=0.5
);

// save predefined 2D orientation vectors
pair NN=N;
pair SS=S;
pair EE=E;
pair WW=W;

//%points on cube

triple A = (0,0,0);
triple B = (4,0,0);
triple D = (0,4,0);
triple C = (4,4,0);
triple E = (2,2,2.8);
triple F = (2,2,-2.8);

// triple[] cubicCornerA={A,C,F,H,};

triple[] cubicCornerB={A,B,D,C,E,F,};

real cornerAR=0.05;
real cornerBR=0.2;
real faceCR=0.2;
real connR=faceCR;

pen backPen=gray(0.5)+dashed+1bp;
pen frontPen=gray(0.2)+dashed+1bp;

real cylR=0.062;

void Draw(guide3 g,pen p=currentpen){
  draw(
    cylinder(
      point(g,0),cylR,arclength(g),point(g,1)-point(g,0)
    ).surface(
               new pen(int i, real j){
                 return p;
               }
             )
  );
}

pen connectPen=lightgray;

void drawSpheres(triple[] C, real R, pen p=currentpen){
  for(int i=0;i<C.length;++i){
    draw(sphere(C[i],R+1.8).surface(
                        new pen(int i, real j){return p;}
                        )
    );
  }
}

drawSpheres(cubicCornerB,cornerBR,orange);

//triple arcBeg=point(Q--E,0.4);
//triple arcEnd=point(Q--N,0.4);
//triple arcMid=point(Q--(point(E--N,0.5)),0.62);
