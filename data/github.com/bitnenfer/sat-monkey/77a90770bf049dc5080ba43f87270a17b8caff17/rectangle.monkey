#REM
	Version 0.4 - Copyright 2014 -  Jim Riecken <jimr@jimr.ca>
	Released under the MIT License - https://github.com/jriecken/sat-js
	A simple library for determining intersections of circles and
	polygons using the Separating Axis Theorem.
	@preserve SAT.js - Version 0.4 - Copyright 2014 - Jim Riecken <jimr@jimr.ca> - 
	released under the MIT License. https://github.com/jriecken/sat-js
	
	Ported to Monkey by Felipe Alfonso <contact@shin.cl> -
	https://github.com/ilovepixel/sat-monkey/
#END

Strict

Import sat.vec2
Import sat.polygon
Import sat.base

Class Rectangle Extends Vec2
	
	Field width:Float
	Field height:Float
	
	Method New(x:Float = 0.0, y:Float = 0.0, width:Float = 0.0, height:Float = 0.0)
		Super.New(x, y)
		Self.width = width
		Self.height = height
	End
	
	Method ToPolygon:Polygon ()
		Local pos:Vec2 = Self
		Local w:Float = Self.width
		Local h:Float = Self.height
		
		Return New Polygon(pos.x, pos.y, New VecStack([
		New Vec2(), New Vec2(w, 0), 
		New Vec2(w, h), New Vec2(0, h)]))
	End
	
	Method GetBounds:Rectangle ()
		Return ToPolygon().GetBounds()
	End
	
	Method DebugDraw:Void ()
		Self.ToPolygon().DebugDraw()
	End
	
	Method GetPosition:Vec2 ()
		Return Self
	End
	
	Method SetPosition:Void (x:Float, y:Float)
		Self.Set(x, y)
	End
	
	Method SetPosition:Void (vec:Vec2)
		Self.Copy(vec)
	End
	
	Method GetType:Int ()
		Return ShapeType.POLYGON
	End
End