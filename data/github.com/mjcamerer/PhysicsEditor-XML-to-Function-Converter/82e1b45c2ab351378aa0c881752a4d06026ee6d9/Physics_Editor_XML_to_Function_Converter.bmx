SuperStrict

Local f:TStream = OpenFile("input.xml")
Local w:TStream = WriteFile("output.txt")

Local methodName:String = "CreateCar"
WriteLine(w, "Method " + methodName + ":Entity()")
WriteLine(w, "")
WriteLine(w, "    'Set scale")
WriteLine(w, "    Local scale:Float = 64.0")
WriteLine(w, "")
WriteLine(w, "    'Create Polygon List")
WriteLine(w, "    Local pList:List<Polygon> = New List<Polygon>")
WriteLine(w, "")
WriteLine(w, "    'Create vertices array")
WriteLine(w, "    Local vertices:b2Vec2[]")
WriteLine(w, "")

For Local i:Int = 0 To 15
	ReadLine(f)
Next
Local s:String = ReadLine(f)
Local loc:Int = Instr(s, Chr(34), 1)
Local s2:String = Right(s, Len(s) - loc)
Local loc2:Int = Instr(s2, Chr(34), 1)
Local polyNum:String = Mid(s2, 1, loc2 - 1)
Print "Poly Number: " + polyNum
Print ""
ReadLine(f)
ReadLine(f)

For Local i:Int = 1 To Int(polyNum)
	ReadLine(f)
	s = ReadLine(f)
	loc = Instr(s, Chr(34), 1)
	Local vertexNum:String = Mid(s, loc + 1, 1)
	Print "Vertex Number: " + vertexNum
	ReadLine(f)
	WriteLine(w, "    'Polygon" + i)
	WriteLine(w, "    vertices = New b2Vec2[" + vertexNum + "]")
	For Local i2:Int = 1 To Int(vertexNum)
		s = ReadLine(f)
		loc = Instr(s, Chr(34), 1)
		s2 = Right(s, Len(s) - loc)
		loc2 = Instr(s2, Chr(34), 1)
		Local x:String = Mid(s2, 0, loc2 - 1)
		Local s3:String = Right(s2, Len(s2) - loc2)
		Local loc3:Int = Instr(s3, Chr(34), 1)
		Local s4:String = Right(s3, Len(s3) - loc3)
		Local loc4:Int = Instr(s4, Chr(34), loc3)
		Local y:String = Mid(s4, 0, loc4 - 1)
		Print("X: " + x + " Y: " + y)
		WriteLine(w, "    vertices[" + (i2-1) + "] = New b2Vec2(" + x + "/scale, " + y + "/scale)")
		ReadLine(f)
	Next
	WriteLine(w, "    pList.AddLast(New Polygon(vertices, " + vertexNum + "))")
	WriteLine(w, "")
	Print ""
	ReadLine(f)
Next

WriteLine(w, "    Return(Self.world.CreateMultiPolygon(960, 540, pList, True))")
WriteLine(w, "")
WriteLine(w, "End Method")

CloseFile(f)
CloseFile(w)

End
