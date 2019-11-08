
config const epsilon: real = 1e-6;

class RA3 {
  forwarding var repr: [0..#3] real;
}

proc RA3(x:real,y:real,z:real){
  return [x,y,z];
}

class Point3D {
  forwarding var repr: RA3;
}

class Vector3D {
  forwarding var repr: RA3;
}

proc RA3.x(){
  return this(0);
}
proc RA3.y(){
  return this(1);
}
proc RA3.z(){
  return this(2);
}

proc Vector3D.normalized(){
  var m = norm(this);
  if(m!=0):
    return this.times(1/m);
  else:
    return this;
}

class Ray{
  var origin: Point3D;
  var direction: Vector3D;
}

proc Ray(origin: Point3D, direction: Vector3D){
  this.origin = origin;
  this.direction = direction.normalized();
}

proc Ray(p1: Point3D, p2: Point3D){
  this.origin = p1;
  this.direction = vectorBetween(p1,p2).normalized();
}

proc Ray.transform(t: transform){
  return new Ray(this.origin.transform(t),
		 this.direction.transform(t));
}

proc Ray.pointAlong(d: real){
  return this.origin.plus(times(this.direction,d)) as Point3D;
}

class Plane{
  var origin: Point3D;
  var normal: Vector3D;
}

proc Plane(p1: Point3D, p2: Point3D, p3: Point3D){
  this.origin = p1;
  var v1 = vectorBetween(p1,p2);
  var v2 = vectorBetween(p1,p2);
  this.normal = cross(v1,v2);
}

proc Plane.transform(t: transform){
  return new Plane(this.origin.transform(t),
		   this.normal.transform(t));
}

class Triangle{
  var a,b,c: Point3D;
  var normal: Vector3D;
}

proc Triangle(a: Point3D, b: Point3D, c: Point3D){
  this.a = a;
  this.b = b;
  this.c = c;
  this.normal = cross(minus(b,a),minus(c,a));
}

proc Triangle.transform(t: Transformation){
  return new Triangle(this.a.transform(t),
		      this.b.transform(t),
		      this.c.transform(t));
}


class Transform {
  forwarding var repr: [0..#4,0..#4];
}

proc Transform.compose(t: Transform){
  return dot(this,t.linearComponent);

}

proc augment(v: RA3, s: real = 1): [0..#4] real{
  return [v(0),v(1),v(2),s];
}

proc deaugment(v: [0..#4] real): RA3{
  return [v(0),v(1),v(2)]
}

proc translate(along: Vector3D): Transform{
  return new Transform([[1,0,0,along.x()],
			[0,1,0,along.y()],
			[0,0,1,along.z()],
			[0,0,0,1]]);
}

proc scale(amount: Vector3D): Transform{
  return new Transform([[amount.x(),0,0,0],
			[0,amount.y(),0,0],
			[0,0,amount.z(),0],
			[0,0,0,1]]);
}

proc rotate(amount: real, along: Vector3D): Transform{
  var u  = augment(along.normalized(),0);
  var s,c: real = sin(amount),cos(amount);
  var tm = outer(u,u).times(1-c).plus(times([[1,    -u.z(),u.y(),0],
					     [u.z(), 1,   -u.x(),0],
					     [-u.y(),u.x(),1,    0],
					     [0,     0,    0,    1]],

					    [[c,s,s,0],
					     [s,c,s,0],
					     [s,s,c,0],
					     [0,0,0,1]]));
  return new Transform(tm);
}

proc reflect(across: Plane): Transform{
  var n = across.normal.normalized();
  var u, ut = augment(n,0), augment(n,dot(n,across.origin));
  var tm = outer(u,ut);
  tm(3,3) = 1;
  return new Transform(tm);
}

proc RA3.transform(t: Transform) {
    return deaugment(dot(augment(this),t.linearComponent).plus(t.translation));
}
/*
 *2D coordinate space
 */
class Coordinates2D{
  var origin: Point3D;
  var axis1, axis2: Vector3D;
}

proc Coordinates2D.transform(t: Transform){
  return new Coordinates2D(this.origin.transform(t),
			   this.axis1.transform(t),
			   this.axis2.transform(t));
}

proc Coordinates2D.pointAt(x: real, y: real){
  return this.origin.plus(plus(times(x,this.axis1)),
			       times(y,this.axis2));
}

proc intersectionDist(r: Ray, p: Plane): Point3D{
  var slope = dot(r.direction,p.normal);
  if(slope > epsilon){
    t = dot(vectorBetween(r.origin,p.origin),p.normal);
    if(t>=0){
      return t;
    }
  }
  return math.NAN;
}

proc vectorBetween(p1: Point3D, p2: Point3D): real{
  return new Vector3D(p2.x()-p1.x(),p2.y()-p1.y(),p2.z()-p1.z());
}

/*
 * simple euclidean distance between two points
 */
proc distance(p1: Point3D, p2: Point3D): real{
  return vectorBetween(p1,p2).magnitude();
}
