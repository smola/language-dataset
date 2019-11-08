module nitConstraints

# This class defines an object to represent a two dimentional position 
public class Position_2d

	var x: Float is writable
	var y: Float is writable

	fun update_position(x: Float, y: Float)
	do 
		self.x = x
		self.y = y
	end 

end 
# This class defines an object to represent dimensions 
public class Dimensions_2d
	
	var height: Float is writable
	var width: Float is writable

end
# This class defines an object to represent an area in the world... this could be really useful to implement requests 

public class Area_2d
	# The top left position 
	private var p1: Position_2d
	# The lower right position 
	private var p2: Position_2d

	init do compute_area
	# This function will compute the area using the top left and the lower right positions 
	private fun compute_area  
	do
		#TODO
	end 
	# This function will check if the area contains a given point
	fun contains (p: Position_2d): Bool 
	do
		  #TODO
		  return true
	end 
end
# This class defines an object to represent a two dimentional vector
public class Vector_2d
	
	var v_x: Float is writable
	var v_y: Float is writable

	# This function will apply a transformation on a vector using another vector 
	fun transform_vector(v: Vector_2d)
	do	
		var temp = v_x

		v_x = v_x * v.v_y - v_y * v.v_x
		v_y = temp * v.v_x + v_y * v.v_y

	end

	# This function will return the length of the vector
	fun length: Float do return (v_x * v_x + v_y * v_y).sqrt  
	
	# This functions returns the noremalized vector 
	fun get_normalized_vector: Vector_2d do return new Vector_2d(self.v_x / self.length, self.v_y / self.length)
	
	# This function will perform an addition of another vector 
	fun add(v: Vector_2d) 
	do
		self.v_x += v.v_x
		self.v_y += v.v_y
	end 

	# This function will intensify the value of the vector by a given intensity 
	fun intensify (i: Float)
	do 
		self.v_x *= i
		self.v_y *= i
	end

end

# This class represent a future feature that we want to add to the library , 

public class Request
	
		#TODO
end 	

