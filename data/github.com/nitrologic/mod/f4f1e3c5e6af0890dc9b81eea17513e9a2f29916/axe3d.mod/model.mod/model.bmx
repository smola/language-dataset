
Strict

Rem
bbdoc: Model - basic interface for 3d modelling
End Rem
Module axe3d.model

ModuleInfo "Version: 1.00"
ModuleInfo "Author: Simon Armstrong"
ModuleInfo "Copyright: Blitz Research Ltd"
ModuleInfo "Copyright: Armstrong Communications Ltd"

ModuleInfo "History: 0.01 Release"
ModuleInfo "History: basic concrete interfaces"

Import axe3d.axe3d

blitz3d_driver = New TConcreteModelDriver


Type TModelTextureLock Extends TTextureLock

	Field _owner:TModelTexture
	Field _frame:Int 
	Field _pixmap:TPixmap
	
	Method Init:TModelTextureLock(owner:TModelTexture,frame:Int)
		_owner=owner
		_frame=frame
		Return Self
	End Method
	
	Method Lock()
		_pixmap=_owner._pixmap
	End Method

	Method Unlock()
		_pixmap=Null
	End Method

	Method SetRGBA(x,y,rgba)
		_pixmap.WritePixel x,y,rgba
	End Method

	Method GetRGBA(x,y)
		Return _pixmap.ReadPixel(x,y)
	End Method
End Type

Const MAXTEXTUREDEPTH%=32

Type TModelTexture Extends TTexture
	Global _all:TList=New TList	
	Field _pixmap:TPixmap
	Field _flags
	Field _name$
	Field _buffer:TTextureLock[MAXTEXTUREDEPTH]
	Field _blendmode
	Field _fx
	Field _uvlayer
	Field _scaleu#
	Field _scalev#
	Field _posu#
	Field _posv#
	Field _rot#
	Field _cubemode
	Field _cubeface

	Method Init:TModelTexture(pix:TPixmap,flags)
		_pixmap=pix
		_flags=flags
		_scaleu=1
		_scalev=1
		ListAddLast _all,Self
		Return Self
	End Method

	Method FreeTexture()
	End Method
	
	Method TextureBlend(blendmode)
		_blendmode=blendmode
	End Method
	
	Method TextureCoords(coords)
		_uvlayer=coords
	End Method
	
	Method ScaleTexture(u#,v#)
		_scaleu=u
		_scalev=v
	End Method
	
	Method PositionTexture(u#,v#)
		_posu=u
		_posv=v
	End Method

	Method RotateTexture(angle#)
		_rot=angle
	End Method

	Method TextureWidth()		
		Return _pixmap.width
	End Method

	Method TextureHeight()
		Return _pixmap.height
	End Method

	Method TextureName$()
		Return _name
	End Method

	Method SetCubeFace(face)
		_cubeface=face
	End Method

	Method SetCubeMode(mode)
		_cubemode=mode
	End Method
		
	Method TextureBuffer:TTextureLock(frame)
		If Not _buffer[frame]
			_buffer[frame]=New TModelTextureLock.Init(Self,frame)
		EndIf
		Return _buffer[frame]
	End Method

	Method LockTexture:TPixmap(buffer:TTextureLock)
		Return _pixmap
	End Method
	
	Method UnlockTexture(buffer:TTextureLock)
	End Method
	
End Type

Type TModelSurface Extends TSurface
	Const VSPAN%=3+3+4+3+3 ' x,y,z, nx,ny,nz, r,g,b,a, u0,v0,w0, u1,v1,w1..
	Global _all:TList=New TList
	Field _brush:TModelBrush
	Field _verts:Float[]
	Field _tris:Int[]
	Field _vertsize
	Field _vertcount
	Field _tricount

	Method GetSurfaceBrush:TBrush()	
		Return _brush
	End Method
	
	Method PaintSurface(brush:TBrush)
		_brush=TModelBrush(brush.Copy())
	End Method
	
	Method ClearSurface(clear_verts,clear_tris)
		If clear_verts _vertcount=0
		If clear_tris _tricount=0
	End Method
	
	Method CountVertices()
		Return _vertcount
	End Method
	
	Method CountTriangles()
		Return _tricount
	End Method

	Method lockvert:Float Ptr(v)
		Local n
		n=v*VSPAN
		If n>=_verts.length
			_verts=_verts[.._verts.length+n+VSPAN*32]
		EndIf
		Return Varptr _verts[n]
	End Method

	Method locktri:Int Ptr(t)
		Local n
		n=t*3
		If n>=_tris.length
			_tris=_tris[.._tris.length+n+32+6*32]
		EndIf
		Return Varptr _tris[t*3]
	End Method
	
	Method AddVertex(x#,y#,z#,u#=0.0,v#=0.0,w#=0.0)
		Local p:Float Ptr
		p=lockvert(_vertcount)
		p[0]=x
		p[1]=y
		p[2]=z
		p[10]=u
		p[11]=v		
		p[12]=w		
		_vertcount:+1
	End Method
	
	Method AddTriangle(v0,v1,v2)
		Local t:Int Ptr
		t=locktri(_tricount)
		_tricount:+1
		t[0]=v0
		t[1]=v1
		t[2]=v2
	End Method
		
	Method VertexCoords(v,x#,y#,z#)
		Local p:Float Ptr
		p=lockvert(v)
		p[0]=x
		p[1]=y
		p[2]=z
	End Method
	
	Method VertexColor(v,r#,g#,b#,a#=1.0)
		Local p:Float Ptr
		p=lockvert(v)
		p[6]=r
		p[7]=g
		p[8]=b
		p[9]=a
	End Method

	Method VertexNormal(v,nx#,ny#,nz#)
		Local p:Float Ptr
		p=lockvert(v)
		p[3]=nx
		p[4]=ny
		p[5]=nz
	End Method

	Method VertexTexCoords(vid,u#,v#,w#,uvlayer)
		Local p:Float Ptr
		p=lockvert(v)
		p[10+uvlayer*3]=u
		p[11+uvlayer*3]=v
		p[12+uvlayer*3]=w
	End Method

	Method VertexX#(v)
		Return _verts[v*VSPAN+0]
	End Method
	
	Method VertexY#(v)
		Return _verts[v*VSPAN+1]
	End Method

	Method VertexZ#(v)
		Return _verts[v*VSPAN+2]
	End Method

	Method VertexRed#(v)
		Return _verts[v*VSPAN+6]
	End Method

	Method VertexGreen#(v)
		Return _verts[v*VSPAN+7]
	End Method

	Method VertexBlue#(v)
		Return _verts[v*VSPAN+8]
	End Method

	Method VertexAlpha#(v)
		Return _verts[v*VSPAN+9]
	End Method

	Method VertexNX#(v)
		Return _verts[v*VSPAN+3]
	End Method

	Method VertexNY#(v)
		Return _verts[v*VSPAN+4]
	End Method

	Method VertexNZ#(v)
		Return _verts[v*VSPAN+5]
	End Method

	Method VertexU#(v,coord_set=0)
		Return _verts[v*VSPAN+3*coord_set+10]
	End Method

	Method VertexV#(v,coord_set=0)
		Return _verts[v*VSPAN+3*coord_set+11]
	End Method

	Method VertexW#(v,coord_set=0)
		Return _verts[v*VSPAN+3*coord_set+12]
	End Method

	Method TriangleVertex(tri,corner)
		Return _tris[tri*3+corner]
	End Method

	Method UpdateNormals()
		RestoreSurface
	End Method

	Method RestoreSurface() Abstract
	
End Type

Type TModelBrush Extends TBrush
	Global _all:TList=New TList	
	Global _brushcount

	Field _handle
	
	Field _r#,_g#,_b#
	Field _a#
	Field _shiny#
	Field _fx
	Field _blend
	Field _texture:TModelTexture
	
	Method Init:TModelBrush(texture:TModelTexture)
		_brushcount:+1
		_r=1
		_g=1
		_b=1
		_a=1
		_texture=texture
		ListAddLast _all,Self
		Return Self
	End Method
	
	Function LoadBrush:TBrush(texture_path$,flags=1,u_scale#=1,v_scale#=1) 
		Local pix:TPixmap
		Local tex:TModelTexture
		If texture_path
			pix=LoadPixmap(texture_path)
		EndIf
		tex=New TModelTexture.Init(pix,flags)
		tex.ScaleTexture u_scale,v_scale		
		Return New TModelBrush.Init(tex)
	End Function
	
	Function CreateBrush:TBrush(red#,green#,blue#)
		Local brush:TModelBrush
		brush=New TModelBrush.Init(Null)
		brush.BrushColor red,green,blue
		Return brush
	End Function
	
	Method FreeBrush()
	End Method

	Method GetBrushTexture:TTexture(index=0)
		Return _texture
	End Method

	Method Copy:TBrush()
		Local brush:TModelBrush
		brush=New TModelBrush.Init(_texture)
		brush.BrushColor _r,_g,_b
		brush.BrushAlpha _a
		brush.BrushBlend _blend
		brush.BrushShininess _shiny
		brush.BrushFX _fx
		Return brush
	End Method
			
	Method BrushColor(r#,g#,b#)
		_r=r
		_g=g
		_b=b
	End Method
	
	Method BrushAlpha(a#)
		_a=a
	End Method
	
	Method BrushShininess(s#)
		_shiny=s
	End Method
	
	Method BrushTexture(texture:TTexture,frame=0,index=0)
		_texture=TModelTexture(texture)
	End Method
	
	Method BrushBlend(blend)
		_blend=blend
	End Method
	
	Method BrushFX(fx)
		_fx=fx
	End Method
	
End Type

Type TModelEntity Extends TEntity
	Global _all:TList=New TList
	
	Field _class$
	Field _name$
	Field _brush:TBrush
	Field _parent:TModelEntity
	Field _kids:TList=New TList
	
	Field _surfaces:TList=New TList
			
	Method Init:TModelEntity(class$,parent:TEntity)
		_class=class
		SetParent(parent)
		ListAddLast _all,Self
		Return Self
	End Method
		
	Method SetParent(parent:TEntity,glob=True) 		
		Local e:TModelEntity
		e=TModelEntity(parent)
		_parent=e
		If e
			e._kids.addlast Self
		EndIf
	End Method
	
	Method AddSurface(surface:TSurface)
		ListAddLast _surfaces,surface
	End Method
	
	Method GetParent:TEntity() 
		Return _parent
	End Method

	Method CopyEntity:TEntity(parent:TEntity=Null) 
		Local e:TModelEntity
		e=New TModelEntity.Init(_class,_parent)
		e._name=_name
		e._brush=_brush.Copy()
		' todo - recurse?
		Return e
	End Method

	Method EntityClass$() 
		Return _class
	End Method

	Method EntityName$() 
		Return _name
	End Method

	Method CountChildren() 
		Return _kids.Count()
	End Method

	Method GetChild:TEntity(childindex) 
		Return TEntity(_kids.ValueAtIndex(childindex))
	End Method

	Method FindChild:TEntity(name$) 
		Local kid:TModelEntity
		For kid=EachIn _kids
			If kid._name=name
				Return kid
			EndIf
		Next		
	End Method

	Method FreeEntity() 
	End Method

	Method PositionEntity(x#,y#,z#,glob=False) 
	End Method
	
	Method MoveEntity(mx#,my#,mz#) 
	End Method

	Method TranslateEntity(tx#,ty#,tz#,glob=False) 
	End Method

	Method ScaleEntity(x#,y#,z#,glob=False) 
	End Method
	
	Method RotateEntity(x#,y#,z#,glob=False) 	
	End Method

	Method TurnEntity(x#,y#,z#,glob=False) 
	End Method

	Method PointEntity(target:TEntity,roll#=0) 
	End Method

	Method AlignToVector(vector_x#,vector_y#,vector_z#,axis,rate#=1.0)
	End Method
	
	Method LoadAnimSeq(path$) 
	End Method

	Method ExtractAnimSeq(first_frame,last_frame,seq=0) 
	End Method

	Method Animate(mode=1,speed#=1.0,seq=0,trans=0) 
	End Method

	Method SetAnimTime(time#,seq=0) 
	End Method

	Method AnimSeq() 
	End Method
	
	Method AnimLength() 
	End Method
	
	Method AnimTime#() 
	End Method

	Method Animating() 
	End Method

	Method EntityColor(r#,g#,b#) 
	End Method

	Method EntityShininess(s#) 
	End Method

	Method EntityTexture(texture:TTexture,frame=0,index=0) 
	End Method

	Method EntityBlend(blend_no) 
	End Method

	Method EntityFX(fx_no) 
	End Method

	Method EntityAutoFade(near#,far#) 
	End Method

	Method PaintEntity(brush:TBrush) 
		Local surface:TSurface
		For surface=EachIn _surfaces
			surface.PaintSurface(brush)
		Next
	End Method

	Method ShowEntity() 
	End Method

	Method HideEntity() 
	End Method

	Method NameEntity(name$) 
		_name=name
	End Method

	Method EntityX#(glob=False) 
	End Method

	Method EntityY#(glob=False) 	
	End Method

	Method EntityZ#(glob=False) 	
	End Method

	Method EntityPitch#(glob=False) 
	End Method

	Method EntityYaw#(glob=False) 
	End Method

	Method EntityRoll#(glob=False) 
	End Method


	Method EntityPick:TEntity(range#) 
	End Method

	Method LinePick:TEntity(x#,y#,z#,dx#,dy#,dz#,radius#=0.0)
	End Method
	
	Method DeltaYaw#(dest:TEntity) 
	End Method

	Method DeltaPitch#(dest:TEntity) 
	End Method
	
	Method GetMatElement#(row,col) 
	End Method

	Method ResetEntity() 
	End Method

	Method EntityRadius(rx#,ry#=0.0) 
	End Method

	Method EntityBox(x#,y#,z#,w#,h#,d#) 	
	End Method

	Method EntityType(type_no,recursive=False) 	
	End Method

	Method EntityPickMode(no,obscure=True) 	
	End Method

	Method EntityCollided:TEntity(type_no) 
	End Method

	Method CountCollisions() 
	End Method

	Method CollisionX#(index) 
	End Method

	Method CollisionY#(index) 
	End Method

	Method CollisionZ#(index) 
	End Method

	Method CollisionNX#(index) 
	End Method

	Method CollisionNY#(index) 
	End Method

	Method CollisionNZ#(index) 
	End Method

	Method CollisionTime#(index) 
	End Method

	Method CollisionEntity:TEntity(index) 
	End Method

	Method CollisionSurface:TSurface(index) 
	End Method

	Method CollisionTriangle(index) 
	End Method

	Method MeshCullRadius(radius#) 
	End Method

	Method EntityScaleX#(glob=False) 
		Return 1
	End Method

	Method EntityScaleY#(glob=False) 
		Return 1
	End Method

	Method EntityScaleZ#(glob=False) 
		Return 1
	End Method

	Method GetEntityType() 
	End Method

	Method GetEntityBrush:TBrush() 
		Return _brush
	End Method

	Method EntityOrder(order_no) 
	End Method
	
	Method LightRange(range#)
	End Method
	
	Method LightColor(red#,green#,blue#)
	End Method

	Method LightConeAngles(inner_angle#,outer_angle#)
	End Method

	Method CameraClsColor(r#,g#,b#)
	End Method
		
End Type


Type TModelDriver Extends TBlitz3DDriver

	Field _world:TModelEntity
	
	Method New()
		_world=New TModelEntity.Init("world",Null)
	End Method

	Method CreateBrush:TBrush(red#,green#,blue#) 
		Return New TModelBrush.CreateBrush(red,green,blue)
	End Method	

	Method LoadBrush:TBrush(texture_path$,flags=1,u_scale#=1,v_scale#=1) 
		Return New TModelBrush.LoadBrush(texture_path,flags,u_scale,v_scale)
	End Method	

	Method LoadTexture:TTexture(file$,flags=1) 
		Return New TModelTexture.Init(LoadPixmap(file),flags)
	End Method
	
	Method CreateTexture:TTexture(width,height,flags=0,frames=1) 
		Local pix:TPixmap
		pix=CreatePixmap(width,height,PF_RGBA8888)
		Return New TModelTexture.Init(pix,flags)
	End Method

	Method CreateMesh:TEntity(parent:TEntity=Null)
		Return New TModelEntity.Init("mesh",parent)
	End Method
	
	Method CreateSphere:TEntity(segments=8,parent:TEntity=Null)
		Return New TModelEntity.Init("sphere",parent)
	End Method
	
	Method CreatePivot:TEntity(parent:TEntity=Null)
		Return New TModelEntity.Init("pivot",parent)
	End Method

	Method CreateCylinder:TEntity(segments=8,solid=True,parent:TEntity=Null) 
		Return New TModelEntity.Init("cylinder",parent)
	End Method

	Method CreateCone:TEntity(segments=8,solid=True,parent:TEntity=Null) 
		Return New TModelEntity.Init("cone",parent)
	End Method

	Method CreatePlane:TEntity(divisions=1,parent:TEntity=Null) 
		Return New TModelEntity.Init("plane",parent)
	End Method

	Method CreateMirror:TEntity(parent:TEntity=Null) 
		Return New TModelEntity.Init("mirror",parent)
	End Method

	Method CreateCube:TEntity(parent:TEntity=Null)
		Local brush:TBrush
		Local cube:TEntity
		Local surf:TSurface
		Local x#,y#,z#
		
		cube=CreateMesh(parent)
		surf=cube.CreateSurface()
		
		x=1
		y=1
		z=1
		'top
		surf.AddVertex -x,y,-z ,0,0
		surf.AddVertex x,y,-z  ,1,0
		surf.AddVertex x,y,z   ,1,1
		surf.AddVertex -x,y,z  ,0,1
		surf.AddTriangle 0,1,2
		surf.AddTriangle 0,2,3
		'bot
		surf.AddVertex -x,-y,z ,0,0
		surf.AddVertex x,-y,z  ,1,0
		surf.AddVertex x,-y,-z   ,1,1
		surf.AddVertex -x,-y,-z  ,0,1
		surf.AddTriangle 4,5,6
		surf.AddTriangle 4,6,7
	
		'front
		surf.AddVertex -x,y,z ,0,0
		surf.AddVertex x,y,z  ,1,0
		surf.AddVertex x,-y,z   ,1,1
		surf.AddVertex -x,-y,z  ,0,1
		surf.AddTriangle 8,9,10
		surf.AddTriangle 8,10,11
		'back
		surf.AddVertex -x,-y,-z ,1,1
		surf.AddVertex x,-y,-z  ,0,1
		surf.AddVertex x,y,-z   ,0,0
		surf.AddVertex -x,y,-z  ,1,0
		surf.AddTriangle 12,13,14
		surf.AddTriangle 12,14,15
	
		'left
		surf.AddVertex -x,y,-z  ,0,0
		surf.AddVertex -x,y,z   ,1,0
		surf.AddVertex -x,-y,z   ,1,1
		surf.AddVertex -x,-y,-z  ,0,1
		surf.AddTriangle 16,17,18
		surf.AddTriangle 16,18,19
		'back
		surf.AddVertex x,y,z ,0,0
		surf.AddVertex x,y,-z  ,1,0
		surf.AddVertex x,-y,-z   ,1,1
		surf.AddVertex x,-y,z  ,0,1
		surf.AddTriangle 20,21,22
		surf.AddTriangle 20,22,23
		
		cube.UpdateNormals
				
		Return cube
		
'		Return New TModelEntity.Init("cube",parent)
	End Method

	Method CreateLight:TEntity(light_type,parent:TEntity=Null) 
		Return New TModelEntity.Init("light",parent)
	End Method
	
	Method CreateCamera:TEntity(parent:TEntity=Null) 
		Return New TModelEntity.Init("camera",parent)
	End Method

	Method AmbientLight(r#,g#,b#) 
		_world.LightColor r,g,b
	End Method

	Method TFormPoint(x#,y#,z#,src:TEntity,dest:TEntity) 
	End Method

	Method TFormVector(x#,y#,z#,src:TEntity,dest:TEntity) 
	End Method

	Method TFormNormal(x#,y#,z#,src:TEntity,dest:TEntity) 
	End Method

	Method TFormedX#() 
	End Method

	Method TFormedY#() 
	End Method

	Method TFormedZ#() 
	End Method

	Method ProjectedX#() 
	End Method

	Method ProjectedY#() 
	End Method

	Method ProjectedZ#() 
	End Method

	Method LinePick:TEntity(x#,y#,z#,dx#,dy#,dz#,radius#) 
	End Method
	Method PickedX#() 
	End Method
	Method PickedY#() 
	End Method
	Method PickedZ#() 
	End Method
	Method PickedNX#() 
	End Method
	Method PickedNY#() 
	End Method
	Method PickedNZ#() 
	End Method
	Method PickedTime#() 
	End Method
	Method PickedEntity:TEntity() 
	End Method
	Method PickedSurface:TSurface() 
	End Method
	Method PickedTriangle() 
	End Method
End Type

Type TConcreteModelDriver Extends TModelDriver
	Method Graphics3D:TGraphics(w,h,d=0,m=0,r=60)
	End Method
	Method AntiAlias(samples)
	End Method
	Method Wireframe(enable)
	End Method

	Method Dither(enable)
	End Method
	Method WBuffer(enable)
	End Method
	Method HWMultiTex(enable)
	End Method

	Method TextureFilter(match_text$,flags)
	End Method

	Method ClearTextureFilters()
	End Method



	Method ClearWorld(entities=True,brushes=True,textures=True) 
	End Method
	Method RenderWorld(tween#=1.0) 
	End Method
	Method TrisRendered() 
	End Method

	Method ClearCollisions() 
	End Method
	Method Collisions(src_no,dest_no,method_no,response_no=0) 
	End Method
	Method CaptureWorld() 
	End Method
	Method UpdateWorld(anim_speed#=1.0) 
	End Method
	
End Type
