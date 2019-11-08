use glew
import glew
import Model
import engine/Types
import physics/Body
import gfx/gl/GLPrimitives

include math

Line: class extends Model {
	
	begin, end, color: Float3
	
	init: func ~line(.name, begin := Float3 new(), end := Float3 new(), color := Float3 new(1, 0, 0)) {
		super(name)
        (this begin, this end) = (begin, end)
		set("begin", begin)
		set("end", end)
        set("color", color)
	}
	
	init: func ~withmodel(.name,m1,m2: Body) {
		lineb := Float3 new()
		linee := Float3 new()
	
		m1 getPos() bind(lineb)
		m2 getPos() bind(linee)
		
		init(name,linee,lineb)
	}
	
	render: func {
        begin = get("begin", Float3)
        end   = get("end",   Float3)
        color = get("color", Float3)
        
		glColor3f(color x, color y, color z)
        glLine(begin x, end x, begin y, end y, begin z, end z)
	}


}
