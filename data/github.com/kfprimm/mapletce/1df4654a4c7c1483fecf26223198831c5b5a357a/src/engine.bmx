
Strict

Import MaxB3D.Functions
Import MaxB3D.Primitives

Import MaxB3D.BSPLoader

Import MaxB3DEx.Grid
Import MaxB3DEx.GLGrid
Import MaxB3DEx.D3D9Grid

Import MaxB3DEx.Lightmapper

Import BRL.BMPLoader
Import BRL.JPGLoader

Const MAPLETMODE_MOVEXZ	= 1
Const MAPLETMODE_MOVEY	= 2
Const MAPLETMODE_ROTATE	= 3
Const MAPLETMODE_PLANE  = 4

Const MAPLETVIEW_FACES			= 1
Const MAPLETVIEW_WIREFRAME	= 2

Function BSPCube:TList(matrix:TMatrix=Null)
	If matrix=Null matrix=TMatrix.Identity()
	Local point:TVector[8]
	point[0] = matrix.TransformVector(Vec3( 1.0, 1.0, 1.0))
	point[1] = matrix.TransformVector(Vec3(-1.0, 1.0, 1.0))
	point[2] = matrix.TransformVector(Vec3(-1.0,-1.0, 1.0))
	point[3] = matrix.TransformVector(Vec3( 1.0,-1.0, 1.0))
	point[4] = matrix.TransformVector(Vec3( 1.0, 1.0,-1.0))
	point[5] = matrix.TransformVector(Vec3(-1.0, 1.0,-1.0))
	point[6] = matrix.TransformVector(Vec3(-1.0,-1.0,-1.0))
	point[7] = matrix.TransformVector(Vec3( 1.0,-1.0,-1.0))
	
	Local list:TList=New TList
	list.AddLast New TBSPPolygon.Create([ point[0],point[1],point[2],point[3] ])
	list.AddLast New TBSPPolygon.Create([ point[7],point[6],point[5],point[4] ])
	list.AddLast New TBSPPolygon.Create([ point[0],point[3],point[7],point[4] ])
	list.AddLast New TBSPPolygon.Create([ point[0],point[4],point[5],point[1] ])
	list.AddLast New TBSPPolygon.Create([ point[5],point[6],point[2],point[1] ])
	list.AddLast New TBSPPolygon.Create([ point[3],point[2],point[6],point[7] ])	
	Return list
End Function

Private
Function Round#(v#, zoom = 0)
	v:*(zoom+1)
	Local diff#=v-Int(v)
	If Abs(diff) >= .5 Return Ceil(Abs(v))*Sgn(v)/(zoom+1)
	Return Floor(Abs(v))*Sgn(v)/(zoom+1)
End Function 

Public

Type TMapletEngine
	Field world:TWorld, light:TLight, camera:TCamera
	Field grids:TPivot, reference:TGrid, large_cursor:TGrid, small_cursor:TGrid
	Field model:TBSPModel, render:TMesh, lightmapper:TLightmapper
	Field lightmesh:TMesh
	Field zoom
	
	Field mode, view
	
	Field cursorx#, cursory#
	Field marker:TVector, begin_marker:TVector
	Field referenceplane:TPlane, markerplane:TPlane
	
	Field cube_list:TList, cube_tree:TBSPTree, cube_model:TBSPModel
	Field selection_model:TBSPModel
	
	Method New()
		world = CreateWorld()
		SetWorld world
		
		SetAmbientLight 100,100,100
		light = CreateLight()
		SetEntityRotation light,10,0,-151
		
		camera  = CreateCamera()
		SetEntityColor camera,0,0,255
		
		reference = CreateGrid(16,16)
		SetEntityRotation reference,90,0,0
		SetEntityScale reference,16,16,0
		SetEntityFX reference,FX_FULLBRIGHT
		SetEntityColor reference,0,191,255
		
		large_cursor = CreateGrid(48,48)
		SetEntityRotation large_cursor,90,0,0
		SetEntityFX large_cursor,FX_FULLBRIGHT
'		SetEntityColor large_cursor,0,128,255
		SetEntityColor large_cursor,0,191,255
		SetEntityAlpha large_cursor,.5
		SetEntityVisible large_cursor, False
		
		small_cursor = CreateGrid(,,large_cursor)
		SetEntityFX small_cursor,FX_FULLBRIGHT
'		SetEntityColor small_cursor,0,89,255
		SetEntityColor small_cursor,0,191,255
		SetEntityAlpha small_cursor,.4
		
		cube_list = BSPCube(TMatrix.Scale(-1,-1,-1))
		cube_tree = New TBSPTree
		cube_tree.Insert cube_list,BSP_OUT,BSP_OUT
		cube_model = LoadBSPModel(cube_tree)
		
		SetEntityTexture cube_model, LoadTexture("castlest.bmp")
		
		selection_model = cube_model
		SetEntityVisible cube_model,False
		
		lightmapper = New TLightmapper
		lightmapper.SetAmbient 20,20,20
		
		lightmesh = CreateCube()
		ScaleMesh lightmesh,.25,.25,.25
		SetEntityVisible lightmesh, False
		
		model = CreateBSPModel()

		Reset
	End Method
	
	Method OnEvent(event:TEvent)
		Select event.id
		Case EVENT_KEYDOWN
			Select event.data
			Case KEY_LSHIFT
				If mode = MAPLETMODE_MOVEXZ
					mode = MAPLETMODE_MOVEY
				Else
					mode = MAPLETMODE_PLANE
				EndIf
			End Select
		Case EVENT_MOUSEWHEEL
			MoveEntity camera,0,0,event.data
		Case EVENT_MOUSEMOVE
			MoveCursor event.x, event.y
		Case EVENT_MOUSEDOWN
			Select event.data
			Case 1
				If mode <> MAPLETMODE_PLANE Mark
			Case 2
				If mode = MAPLETMODE_PLANE
					mode = MAPLETMODE_MOVEY
				Else
					mode = MAPLETMODE_MOVEXZ
				EndIf
			End Select
		Case EVENT_MOUSEUP, EVENT_KEYUP
			Select event.data
			Case 1
				If mode = 0 
					If begin_marker
						If Abs(begin_marker.x - marker.x) * Abs(begin_marker.z - marker.z) > 0
							mode = MAPLETMODE_PLANE
						Else
							Mark
						EndIf
					Else
						Mark
					EndIf
				Else
					Mark
				End If
			Case 2,KEY_LSHIFT
				mode = 0
			Case KEY_A, KEY_Z
				Local inc = 1
				If event.data = KEY_Z inc = -1
				ChangeZoom inc
			Case KEY_L
				AddLight()
			Case KEY_SPACE
				Lightmap()
			End Select
		End Select
	End Method
	
	Method Resize(width, height)
		SetCameraViewport camera,0,0,width,height
	End Method
	
	Method ChangeZoom(inc)
		zoom = Min(3,Max(0, zoom + inc))
		Local size# = 48*(2^zoom), scale# = 1*(.5^zoom)
		SetGridSize small_cursor,size,size
		SetEntityScale small_cursor,scale,scale,scale
	End Method
	
	Method Reset()
		SetEntityPosition camera,0,5.5,-9.5
		SetEntityRotation camera,30,0,0
		SetEntityPosition reference,-128,0,-128
		SetEntityPosition large_cursor,-24,0,-24
		
		mode = 0
		begin_marker = Null
		marker = Vec3(0,0,0)
				
		ChangeZoom 0
	End Method

	Method SetView(v)
		view = v
		SetEntityFX model, 0
		Select view
		Case MAPLETVIEW_WIREFRAME
			SetEntityFX model, FX_WIREFRAME
		End Select
	End Method
	
	Method MoveCursor(x#,y#)
		If cursorx = x And cursory = y Return
		cursorx = x
		cursory = y
		
		referenceplane = New TPlane.FromPoint(Vec3(0,1,0), marker)
		markerplane = New TPlane.FromPoint(Vec3(0,0,1), marker)
		
		Local z#		
		Select mode
		Case MAPLETMODE_MOVEXZ
			If MouseIntersect(x,y,z,referenceplane) And GetEntityVisible(large_cursor)
				TranslateEntity camera, marker.x - x, 0, marker.z - z
			EndIf
		Case MAPLETMODE_MOVEY
			If MouseIntersect(x,y,z,markerplane) And GetEntityVisible(large_cursor)
				TranslateEntity camera, 0, marker.y - y, 0
			EndIf
		Case MAPLETMODE_PLANE
			Local plane:TPlane 
			If MouseIntersect(x,y,z,markerplane) And GetEntityVisible(large_cursor)
				TranslateEntity reference, 0, Round(y - marker.y, zoom), 0
				GetEntityPosition reference,x,marker.y,z
				GetEntityPosition large_cursor,x,y,z
				SetEntityPosition large_cursor,x,marker.y,z				
			EndIf
		Default
			If MouseIntersect(x,y,z,referenceplane)
				Local mx# = Round(x, zoom)
				Local mz# = Round(z, zoom)
				marker = Vec3(mx, marker.y, mz)
				If mx <= 16*8 And mx >= -16*8 And mz <= 16*8 And mz >= -16*8
					SetEntityVisible large_cursor,True
					SetEntityPosition large_cursor,Max(-16*8,Min(16*8 - 48,Round(marker.x) - 24)),marker.y,Max(-16*8,Min(16*8 - 48,Round(marker.z) - 24))
				Else
					SetEntityVisible large_cursor,False
				EndIf
			EndIf
		End Select
		
		UpdatePreview
	End Method
	
	Method Mark()
		If begin_marker
			If mode = MAPLETMODE_PLANE And marker.y<>begin_marker.y
				Local x#,y#,z#
				Local width#,height#,depth#
				
				width = marker.x-begin_marker.x
				height = marker.y-begin_marker.y
				depth = marker.z-begin_marker.z
				
				x = begin_marker.x+width/2.0
				y = begin_marker.y+height/2.0
				z = begin_marker.z+depth/2.0

				Local cube:TList = BSPCube(TMatrix.Transformation(x,y,z,0,0,0,-Abs(width/2.0),-Abs(height/2.0),-Abs(depth/2.0)))
				model.Insert cube,BSP_IN,BSP_IN		
				model.Reduce	
				
				marker.y = begin_marker.y
								
				GetEntityPosition reference,x,y,z
				SetEntityPosition reference,x,marker.y,z
				GetEntityPosition large_cursor,x,y,z
				SetEntityPosition large_cursor,x,marker.y,z		
				
				If render
					SetEntityVisible render, False
					render = Null
				EndIf
				SetEntityVisible model, True						
			EndIf
			SetEntityVisible selection_model, False
			begin_marker = Null
		Else
			begin_marker = marker.Copy()
			UpdatePreview
			SetEntityVisible selection_model, True
		EndIf
		
		mode = 0
	End Method
	
	Method UpdatePreview()
		If begin_marker
			Local width#,height#,depth#
			width = marker.x-begin_marker.x
			height = marker.y-begin_marker.y
			depth = marker.z-begin_marker.z
			SetEntityScale selection_model,Abs(width/2.0),Abs(height/2.0),Abs(depth/2.0)
			SetEntityPosition selection_model,begin_marker.x+width/2.0,begin_marker.y+height/2.0,begin_marker.z+depth/2.0
		EndIf
	End Method	
	
	Method AddLight()
		If marker
			lightmapper.AddLight marker.x, marker.y, marker.z, 255, 255, 255, 10, True, 3
			Local mesh:TMesh = lightmesh.Copy()
			SetEntityPosition mesh,marker.x, marker.y, marker.z
		EndIf
	End Method
	
	Method Lightmap()
		render = LoadMesh(model)
		SetEntityVisible model, False
		Local pixmap:TPixmap = lightmapper.Run(render,  0.2, 2)
		SavePixmapJPeg pixmap, "render.jpg"
		SetEntityTexture render,LoadTexture(pixmap)
	End Method
			
	Method MouseIntersect(x# Var, y# Var, z# Var, plane:TPlane)
		Local ptA:TVector = Vec3(0,0,0), ptB:TVector = Vec3(0,0,0)
		camera.Unproject cursorx,cursory,0,ptA.x,ptA.y,ptA.z
		camera.Unproject cursorx,cursory,1,ptB.x,ptB.y,ptB.z
		Local v:TVector = plane.LineIntersection(ptA, ptB)
		If v = Null Return False
		v.Get3 x,y,z
		Return True
	End Method
End Type



