require 'rgb'
require 'viewmat'

NB. usage: vector2image matrix
vector2image =: 3 : 0
RGB <.255*| y
)

NB. takes boxed list of parameters
NB. Camera Loc
NB. Camera look at position
NB. image dimensions
NB. horizontal fov 
RayGen =: 3 : 0

norm =. % +/&.(*:"_)"1
cross=. [: > [: -&.>/ .(*&.>) (<"1=i.3) , ,:&:(<"0)
dot  =. +/ .* 

cameraPos =. > 0 { y
cameraLookAt =. norm > 1 { y
fov =. > 3 { y

w =. -cameraLookAt
u =. 0 0 1 cross w
v =. w cross u

loc =. dimensions $ cameraPos

ix =. dimensions $(0{dimensions)$i: 2%~ 0{dimensions 
iy =. |: ix

dist =. 0{dimensions % 2 * 3 o. fov % 2

NB. -w * dist + ix * u + iy * v

rayDir =. ((dimensions,3)$(-w*dist))+(iy*(dimensions,3)$v)+(ix*(dimensions,3)$u)
rayDir =. norm rayDir


   ((dimensions,3)$cameraPos) ; rayDir                                   

)

NB. Return distance of sphere intersection 
NB. Usage (spherePos ; sphereRadius ; materialIndex) intersectSphere ((dimensions, 3)$vector)
IntersectSphere =: 4 : 0 " 1 1
norm =. % +/&.(*:"_)"1
cross=. [: > [: -&.>/ .(*&.>) (<"1=i.3) , ,:&:(<"0)
dot  =. +/ .*"1 

direction =. >1} y
assert (3 = ({: $ direction))
position =. >0} y
assert (3 = {:@$ position)
center =. >0} x
assert (3 = $ center)
radius =. >1} x
assert (1 = $ radius)

A =. dot~ direction
B =. 2*(direction dot (position -"1 center))
C =. (radius^2) -~ dot~ (position -"1 center) 

rootPart =. (*~B) - (4*A*C)
rootPart =. convertToInf rootPart
rootPart =. (%:"0) rootPart
plusPart =. rootPart + -B
minusPart =. rootPart -~ -B
plusPart =. plusPart % 2*A
minusPart =. minusPart % 2*A
plusPart <. minusPart
)

IntersectTriangle =: 4 : 0 " 1 1
norm =. % +/&.(*:"_)"1
cross=. [: > [: -&.>/ .(*&.>) (<"1=i.3) , ,:&:(<"0)
dot  =. +/ .*"1 

direction =. >1} y
assert (3 = ({: $ direction))
position =. >0} y
assert (3 = {:@$ position)
p1 =. >0} x
assert (3 = {:@$ p1)
p2 =. >1} x
assert (3 = {:@$ p2)
p3 =. >2} x
assert (3 = {:@$ p3)

v1 =. p2 - p1
v2 =. p3 - p2
tNorm =. norm (v1 cross v2)

time =: (tNorm dot (p1 -"1 position)) % (direction dot tNorm)

intersectPoint =. position + direction * time

v1v3 =. p1 - p3
v2v1 =. p2 - p1
v3v2 =. p3 - p2

rayPointV1 =. intersectPoint -"1 p1
rayPointV2 =. intersectPoint -"1 p2
rayPointV3 =. intersectPoint -"1 p3

test1 =. (v2v1 cross"1 rayPointV1) dot tNorm
test2 =. (v3v2 cross"1 rayPointV2) dot tNorm
test3 =. (v1v3 cross"1 rayPointV3) dot tNorm
thing =:(0 < test1) *. (0 < test2) *. (0 < test3)
convertToInf thing * time
)

convertToInf =: 3 : 0 "0
if. 0 >: y
do. _
else.
y
end.
)

convertToDrawable =: 3 : 0 "0
if. _ = y
do. 0
else.
y
end.
)

compareWithIndex =: 4 : 0 "1
if. 0{x < 0{y
do. x
else.
y
end.
)

IntersectSpheres =: 4 : 0 
sphereints =. x IntersectSphere y
converted =. convertToInf sphereints
sphereIndex =. i. # x
indexAppended =.  converted ,"0 sphereIndex
combinedTValues =.  (compareWithIndex)/ indexAppended
combinedTValues
)

IntersectTriangles =: 4 : 0
triangleInts =. x IntersectTriangle y
triangleIndex =. i. # x
indexAppended =. triangleInts ,"0 triangleIndex
combinedTValues =. (compareWithIndex)/indexAppended
combinedTValues
)


NB. TESTING CODE
width =. 32
height =. 32
dimensions =: width,height

rays =. RayGen 0 0 0; 0 _1 0; dimensions; 1p1%2
sphere =. 0 _5 0; 1 ; 0
sphere2 =. 2 _3 0; 0.5; 0
spheres =. sphere ,: sphere2
triangle1 =. 0 _5 _1; _1 _5 1; 1 _5 1
triangle2 =. 1 _6 _2; 1 _6 0; 2 _6 _1
triangles =. triangle1,:triangle2

inf20 =. (0:`[@.([>(-_:)))"0
(width, (height*2))$;(spheres IntersectSpheres rays)
(width, (height*2))$; triangles IntersectTriangles rays
NB. viewmat convertToDrawable (spheres IntersectSpheres rays)

