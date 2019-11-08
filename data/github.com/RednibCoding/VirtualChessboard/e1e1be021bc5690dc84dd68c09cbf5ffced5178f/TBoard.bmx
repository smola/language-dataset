

Type TBoard
	
	Const WIDTH:Int = 8		' Number of Squares in one column
	Const HEIGHT:Int = 8		' Number of Squares in one row
	
	Const VIEW_AS_WHITE:Int = 0
	Const VIEW_AS_BLACK:Int = 1
	Field View:Int = VIEW_AS_WHITE



	Field Square:TSquare[,] ' Array of all 64 squares
	
	Field MousePicked:TSquare = New TSquare ' Picked piece held in mouse
	Field SameSquare:TSquare ' saves from which square was the piece
	
	Function Create:TBoard()
		Local b:TBoard = New TBoard
			b.Square = New TSquare[WIDTH, HEIGHT]			
			Local SquareColor:Int = 0 ' 0 = Dark, 1 = Light
			Local pieces:Int[WIDTH, HEIGHT]
			pieces = LoadDefaultFile("default.dflt")
			For Local y:Int = 7 To 0 Step -1
				SquareColor = 1 - SquareColor
				For Local x:Int = 0 To 7
					SquareColor = 1 - SquareColor
					b.Square[x, y] = New TSquare
					b.Square[x, y].Color = SquareColor
					b.Square[x, y].x = x * TSquare.SQUARE_SIZE
					b.Square[x, y].y = y * TSquare.SQUARE_SIZE
					If pieces = Null
						b.Square[x, y].Piece = 99
					Else
						b.Square[x, y].Piece = pieces[x, y]
					End If
				Next
			Next
		b.MousePicked.Piece = 99 ' No picked piece
		UndoList.Clear()
		AddUndoStep(b)
		Return b
	End Function
	
	
	Function LoadDefaultFile:Int[,](file:String)
		Local fileIn:TStream = ReadFile(file)
		If Not fileIn
			Notify("Unable to load "+ file + " file!")
			Return Null
		End If
		Local pieces:Int[WIDTH, HEIGHT]
		For Local y:Int = 7 To 0 Step -1
			For Local x:Int = 0 To 7
				pieces[x, y] = Int(ReadLine(fileIn))
			Next
		Next
		CloseStream(fileIn)
		TSquare.ListPicked.Clear()
		Return pieces
	End Function
	
	
	Method LoadPieces:Int(file:String)
		Local fileIn:TStream = ReadFile(file)
		If Not fileIn
			Notify("Unable to load "+ file + " file!")
			Return Null
		End If
		
		TSquare.SetAllUnpicked(Self)

		Local pieces:Int[WIDTH, HEIGHT]
		For Local y:Int = 7 To 0 Step -1
			For Local x:Int = 0 To 7
				Local str:String = ReadLine(fileIn)
				If str.Contains("-") ' square was a picked square	
					Square[x, y].SetPicked()
					str = Replace(str,"-","")	
				Else
					Square[x, y].Picked = False
				End If
				pieces[x, y] = Int(str)
			Next
		Next
		CloseStream(fileIn)
		For Local y:Int = 7 To 0 Step -1
			For Local x:Int = 0 To 7
				If pieces = Null
					Square[x, y].Piece = 99
				Else
					Square[x, y].Piece = pieces[x, y]
				End If
			Next
		Next
		UndoList.Clear()
		AddUndoStep(Self)
		Return True
	End Method

	
	
	Method SavePieces(file:String)
		Local fileOut:TStream = WriteFile(file)
		If Not fileOut
			Notify("Unable to create " +file+ " file", True)
			End
		End If
		For Local y:Int = 7 To 0 Step -1
			For Local x:Int = 0 To 7
				If Square[x, y].Picked = True
					WriteLine(fileOut, Square[x, y].Piece + "-") ' Square is a picked square
				Else
					WriteLine(fileOut, Square[x, y].Piece)
				End If
			Next
		Next
		Notify("Game has been saved! - " + file)
		CloseStream(fileOut)
	End Method
	
	
	Method RotateBoard()
		If View = VIEW_AS_WHITE
			Local tx:Int = 7, ty:Int = 0
			For Local y:Int = 7 To 0 Step -1
				For Local x:Int = 0 To 7
					Square[x, y].x = tx * TSquare.SQUARE_SIZE
					Square[x, y].y = ty * TSquare.SQUARE_SIZE
					tx = tx - 1
				Next
				ty = ty + 1
				tx = 7
			Next

			View = VIEW_AS_BLACK
		Else If View = VIEW_AS_BLACK
			For Local y:Int = 7 To 0 Step -1
				For Local x:Int = 0 To 7
					Square[x, y].x = x * TSquare.SQUARE_SIZE
					Square[x, y].y = y * TSquare.SQUARE_SIZE
				Next
			Next
			View = VIEW_AS_WHITE
		End If
	End Method
	
	
	Method UpdateBoard()
		For Local sqare:TSquare = EachIn Square
			If sqare.MousePicked(Self) ' If a square is clicked
			
				If MousePicked.Piece = 99 ' If no piece is held in mouse
					MousePicked.Piece = sqare.Piece ' Pick piece from clicked square
					sqare.Piece = 99 ' remove piece from clicked square
					SameSquare = sqare' save from which square was the piece
					
				Else If MousePicked.Piece <= 5 ' If a white piece is held in mouse
					If sqare.Piece > 5 ' if there is a black piece or no piece on clicked square
						sqare.Piece = MousePicked.Piece ' place piece held in mouse on clicked square
						MousePicked.Piece = 99 ' remove piece from mouse
						If sqare <> SameSquare ' no Undostep when piece is placed back to its original square
							AddUndoStep(Self) ' Add move to undo list
						Else
							TSquare.SetAllUnpicked(Self)
							SameSquare = Null
						End If
					End If
				Else If MousePicked.Piece > 5 ' If a black piece is held in mouse
					If sqare.Piece <= 5 Or sqare.Piece = 99' if there is a white piece or no piece on clicked square
						sqare.Piece = MousePicked.Piece ' place piece held in mouse on clicked square
						MousePicked.Piece = 99 ' remove piece from mouse
						If sqare <> SameSquare ' no Undostep when piece is placed back to its original square
							AddUndoStep(Self) ' Add move to undo list
						Else
							TSquare.SetAllUnpicked(Self)
							SameSquare = Null
						End If

					End If
				End If

			End If
		Next
	End Method
	
	Method DrawBoard(imgBoard:TImage)
		SetColor 150, 110, 50
		DrawRect(0, 0, 528, 528)
		SetColor 255, 255, 255
		For Local sq:TSquare = EachIn Square
			sq.DrawSquare(imgBoard)
		Next
	End Method
	
	Method DrawCoordinates()
		If View = VIEW_AS_WHITE
			' Numbers
			For Local i:Int = 0 To 7
				DrawText(i+1, 516, 480 - 5 - i*64)
			Next
			' Letters
			For Local i:Int = 0 To 7
				DrawText(Chr(i+97), 32 - 4 + i*64, 514)
			Next

		Else If View = VIEW_AS_BLACK
			' Numbers
			For Local i:Int = 7 To 0 Step - 1
				DrawText(i+1, 516, 32 - 5 + i*64)
			Next
			' Letters
			For Local i:Int = 0 To 7
				DrawText(Chr(i+97), 480 - 4 - i*64, 514)
			Next

		End If
	End Method
	
	Method DrawPieces(imgPieces:TImage)
		For Local sq:TSquare = EachIn Square
			sq.DrawPiece(imgPieces)
		Next
		MousePicked.x = XMouse - (TSquare.SQUARE_SIZE/2)
		MousePicked.y = YMouse - (TSquare.SQUARE_SIZE/2)
		MousePicked.DrawPiece(imgPieces)
	End Method
		
End Type



