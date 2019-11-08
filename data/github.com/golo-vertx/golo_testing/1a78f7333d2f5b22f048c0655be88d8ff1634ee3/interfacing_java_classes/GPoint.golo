module golographics2d.GPoint

#import io.vertx.golo.core.buffer.Buffer
#import io.vertx.golo.core.net.SocketAddress
import graphics2d.Point

var JPoint = null
#var JPoint1 = Point(2.0,3.0)

function GPoint = |x,y|{
	JPoint = Point()
	JPoint: setX(x)
	JPoint: setY(y)
	return JPoint
}

function getX = { 
	return JPoint : getX() 
}

function setX = |x|{
	JPoint : setX(x)
}

function test = {
	return "hi"
}
	#public double getY() { return coords[1]; }
	#public void setX(double x) { coords[0] = x; }
	#public void setY(double y) { coords[1] = y; }
	#public void translate(double d) { coords[0] += d; coords[1] += d; }
	#public String toString() { return "(" + coords[0] + "," + coords[1] + ")" ;}
	#public int getId() {return id;}
	#public static int getCount() {return count;} 
