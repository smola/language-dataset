'version 1
' - first release

#rem
'---------------------------------------------------------------
' QRCode for Monkey
'
' Copyright (c) 2009 Kazuhiko Arase
' Copyright (c) 2013 Ported by Jonathan Pittock For Copper Circle
'
' URL: http:'www.d-project.com/
'
' Licensed under the MIT license:
'   http:'www.opensource.org/licenses/mit-license.php
'
' The word "QR Code" is registered trademark of 
' DENSO WAVE INCORPORATED
'   http:'www.denso-wave.com/qrcode/faqpatent-e.html
'
'---------------------------------------------------------------------

#end

Strict

'imports
Import mojo
Import brl.databuffer

'native code
Private
Import "native/qrcode.${TARGET}.${LANG}"

Extern
#If LANG<>"cpp"
	Function Lsr:Int(number:Int, shiftBy:Int) = "QRCodeGlue.Lsr"
	Function Lsl:Int(number:Int, shiftBy:Int) = "QRCodeGlue.Lsl"
#Else
	Function Lsr:Int(number:Int, shiftBy:Int) = "QRCodeGlue::Lsr"
	Function Lsl:Int(number:Int, shiftBy:Int) = "QRCodeGlue::Lsl"
#End

Private

'constants
Const QRCODE_PAD0:= $EC
Const QRCODE_PAD1:= $11
Const QRCODE_NULL:= -9999
Const QRCODE_FALSE:= 0
Const QRCODE_TRUE:= 1
Const QRCODE_DEFAULT_DATA_LIST_SIZE:= 512
Const QRCODE_DEFAULT_BIT_BUFFER_SIZE:= 256
Global QRCODE_G15:= Lsl(1, 10) | Lsl(1, 8) | Lsl(1, 5) | Lsl(1, 4) | Lsl(1, 2) | Lsl(1, 1) | Lsl(1, 0)
Global QRCODE_G18:= Lsl(1, 12) | Lsl(1, 11) | Lsl(1, 10) | Lsl(1, 9) | Lsl(1, 8) | Lsl(1, 5) | Lsl(1, 2) | Lsl(1, 0)
Global QRCODE_G15_MASK:= Lsl(1, 14) | Lsl(1, 12) | Lsl(1, 10) | Lsl(1, 4) | Lsl(1, 1)

Global QRCODE_MODE_NUMBER:= Lsl(1, 0)
Global QRCODE_MODE_ALPHA_NUM:= Lsl(1, 1)
Global QRCODE_MODE_8BIT_BYTE:= Lsl(1, 2)
Global QRCODE_MODE_KANJI:= Lsl(1, 3)

Const QRCODE_MASK_PATTERN000:= 0
Const QRCODE_MASK_PATTERN001:= 1
Const QRCODE_MASK_PATTERN010:= 2
Const QRCODE_MASK_PATTERN011:= 3
Const QRCODE_MASK_PATTERN100:= 4
Const QRCODE_MASK_PATTERN101:= 5
Const QRCODE_MASK_PATTERN110:= 6
Const QRCODE_MASK_PATTERN111:= 7

Const QRCODE_ERROR_CORRECT_LEVEL_L:= 1
Const QRCODE_ERROR_CORRECT_LEVEL_M:= 0
Const QRCODE_ERROR_CORRECT_LEVEL_Q:= 3
Const QRCODE_ERROR_CORRECT_LEVEL_H:= 2

'globals
Global QRCODE_MATH_EXP_TABLE:int[256]
Global QRCODE_MATH_LOG_TABLE:int[256]

Global QRCODE_PATTERN_POSITION_TABLE:=[
	[],
	[6, 18],
	[6, 22],
	[6, 26],
	[6, 30],
	[6, 34],
	[6, 22, 38],
	[6, 24, 42],
	[6, 26, 46],
	[6, 28, 50],
	[6, 30, 54],		
	[6, 32, 58],
	[6, 34, 62],
	[6, 26, 46, 66],
	[6, 26, 48, 70],
	[6, 26, 50, 74],
	[6, 30, 54, 78],
	[6, 30, 56, 82],
	[6, 30, 58, 86],
	[6, 34, 62, 90],
	[6, 28, 50, 72, 94],
	[6, 26, 50, 74, 98],
	[6, 30, 54, 78, 102],
	[6, 28, 54, 80, 106],
	[6, 32, 58, 84, 110],
	[6, 30, 58, 86, 114],
	[6, 34, 62, 90, 118],
	[6, 26, 50, 74, 98, 122],
	[6, 30, 54, 78, 102, 126],
	[6, 26, 52, 78, 104, 130],
	[6, 30, 56, 82, 108, 134],
	[6, 34, 60, 86, 112, 138],
	[6, 30, 58, 86, 114, 142],
	[6, 34, 62, 90, 118, 146],
	[6, 30, 54, 78, 102, 126, 150],
	[6, 24, 50, 76, 102, 128, 154],
	[6, 28, 54, 80, 106, 132, 158],
	[6, 32, 58, 84, 110, 136, 162],
	[6, 26, 54, 82, 110, 138, 166],
	[6, 30, 58, 86, 114, 142, 170]]

Global QRCODE_MAX_LENGTH:=[
    [ [41,  25,  17,  10],  [34,  20,  14,  8],   [27,  16,  11,  7],  [17,  10,  7,   4] ],
    [ [77,  47,  32,  20],  [63,  38,  26,  16],  [48,  29,  20,  12], [34,  20,  14,  8] ],
    [ [127, 77,  53,  32],  [101, 61,  42,  26],  [77,  47,  32,  20], [58,  35,  24,  15] ],
    [ [187, 114, 78,  48],  [149, 90,  62,  38],  [111, 67,  46,  28], [82,  50,  34,  21] ],
    [ [255, 154, 106, 65],  [202, 122, 84,  52],  [144, 87,  60,  37], [106, 64,  44,  27] ],
    [ [322, 195, 134, 82],  [255, 154, 106, 65],  [178, 108, 74,  45], [139, 84,  58,  36] ],
    [ [370, 224, 154, 95],  [293, 178, 122, 75],  [207, 125, 86,  53], [154, 93,  64,  39] ],
    [ [461, 279, 192, 118], [365, 221, 152, 93],  [259, 157, 108, 66], [202, 122, 84,  52] ],
    [ [552, 335, 230, 141], [432, 262, 180, 111], [312, 189, 130, 80], [235, 143, 98,  60] ],
    [[652, 395, 271, 167], [513, 311, 213, 131], [364, 221, 151, 93], [288, 174, 119, 74]]]

Global QRCODE_RS_BLOCK_TABLE:=[
	[1, 26, 19],
	[1, 26, 16],
	[1, 26, 13],
	[1, 26, 9],
	[1, 44, 34],
	[1, 44, 28],
	[1, 44, 22],
	[1, 44, 16],
	[1, 70, 55],
	[1, 70, 44],
	[2, 35, 17],
	[2, 35, 13],
	[1, 100, 80],
	[2, 50, 32],
	[2, 50, 24],
	[4, 25, 9],
	[1, 134, 108],
	[2, 67, 43],
	[2, 33, 15, 2, 34, 16],
	[2, 33, 11, 2, 34, 12],
	[2, 86, 68],
	[4, 43, 27],
	[4, 43, 19],
	[4, 43, 15],
	[2, 98, 78],
	[4, 49, 31],
	[2, 32, 14, 4, 33, 15],
	[4, 39, 13, 1, 40, 14],
	[2, 121, 97],
	[2, 60, 38, 2, 61, 39],
	[4, 40, 18, 2, 41, 19],
	[4, 40, 14, 2, 41, 15],
	[2, 146, 116],
	[3, 58, 36, 2, 59, 37],
	[4, 36, 16, 4, 37, 17],
	[4, 36, 12, 4, 37, 13],
	[2, 86, 68, 2, 87, 69],
	[4, 69, 43, 1, 70, 44],
	[6, 43, 19, 2, 44, 20],
	[6, 43, 15, 2, 44, 16]]
	
Public

'classes
Class QRCodeException Extends Throwable
	Field message:String
	
	Method New(message:String = "")
		Self.message = "QRCodeException('" + message + "')"
	End
	
	Method ToString:String()
		Return message
	End
End

Class QRCode

	Field typeNumber:Int
	Field modules:Int[][]
	Field moduleCount:Int
	Field errorCorrectLevel:Int
	Field qrDataList:= New Stack<QRCodeData>
	Field cache:Int[]
	Field hasCache:Bool

	Method New(typeNumber:Int = 1, errorCorrectionLevel:Int= QRCODE_ERROR_CORRECT_LEVEL_H)
		QRCodeMath.init()
 		Self.typeNumber = typeNumber
		Self.errorCorrectLevel = errorCorrectionLevel
	End

	Method getTypeNumber:Int()
		Return Self.typeNumber
	End

	Method setTypeNumber:Void(typeNumber:Int)
		Self.typeNumber = typeNumber
	End

    Method getErrorCorrectLevel:Int()
        Return Self.errorCorrectLevel
    End

    Method setErrorCorrectLevel:Void(errorCorrectLevel:Int)
        Self.errorCorrectLevel = errorCorrectLevel
    End

	Method addData:Void(data:String, mode:Int = 0)
		If mode = 0 mode = QRCodeUtilities.getMode(data)
		Select mode
			Case QRCODE_MODE_NUMBER
				Self.addData(New QRCodeNumber(data))
	
			Case QRCODE_MODE_ALPHA_NUM
				Self.addData(New QRCodeAlphaNumber(data))
	
			Case QRCODE_MODE_8BIT_BYTE
				Self.addData(New QRCode8BitByte(data))
	
			Case QRCODE_MODE_KANJI
				Self.addData(New QRCodeKanji(data))
				
			Default
				Throw New QRCodeException("mode:" + mode)
		End
	End

	Method addData:Void(qrData:QRCodeData)
		'resize data array
		Self.qrDataList.Push(qrData)
		hasCache = False
	End
	
	Method clearData:Void()
		'null out old data
		qrDataList.Clear()
		hasCache = False
	End

	Method getDataCount:Int()
		Return Self.qrDataList.Length()
	End
	
	Method getData:QRCodeData(index:Int)
		Return Self.qrDataList.Get(index)
	End

	Method isDark:Bool(row:Int, col:Int)
		If Self.modules[row][col] <> QRCODE_NULL Return Bool(Self.modules[row][col])
		Return False
	End

	Method getModuleCount:Int()
		Return Self.moduleCount
	End

	Method make:Void()
		Self.make(False, Self.getBestMaskPattern())
	End
	
	Method createNullIntArray:Int[] (length:Int)
		' --- construct null int array ---
		Local result:Int[length]
		For Local index:= 0 Until length
			result[index] = QRCODE_NULL
		Next
		Return result
	End

	Method getBestMaskPattern:Int()

		Local minLostPoint:= 0
		Local pattern:= 0
		Local lostPoint:Int
		
		For Local i:= 0 Until 8

			Self.make(True, i)

			lostPoint = QRCodeUtilities.getLostPoint(Self)

			if i = 0 or minLostPoint > lostPoint
				minLostPoint = lostPoint
				pattern = i
			EndIf
		Next

		Return pattern
	End
	
	Method make:Void(test:Bool, maskPattern:Int)

		Self.moduleCount = Self.typeNumber * 4 + 17

		Self.modules = New Int[Self.moduleCount][]
		For Local i:= 0 Until Self.moduleCount
			Self.modules[i] = Self.createNullIntArray(Self.moduleCount)
		Next

		Self.setupPositionProbePattern(0, 0)
		Self.setupPositionProbePattern(Self.moduleCount - 7, 0)
		Self.setupPositionProbePattern(0, Self.moduleCount - 7)

		Self.setupPositionAdjustPattern()
		Self.setupTimingPattern()

		Self.setupTypeInfo(test, maskPattern)
		
		If Self.typeNumber >= 7 Self.setupTypeNumber(test)
		
		'not sure wht its being copied here?
		If hasCache = False
			Local dataArray:= qrDataList.ToArray()
			cache = QRCode.createData(Self.typeNumber, Self.errorCorrectLevel, dataArray)
			hasCache = True
		EndIf

		Self.mapData(cache, maskPattern)
	End
	
	Method modulesToString:String(lineFeed:String = "~n")
		Local build:String
		For Local row:= 0 Until getModuleCount()
			For Local col:= 0 Until getModuleCount()
				build += modules[row][col] + ","
			Next
			build += lineFeed
		Next
		Return build
	End
	
	Method mapData:Void(data:Int[], maskPattern:Int)
		
		Local inc:= -1
		Local row:= Self.moduleCount - 1
		Local bitIndex:= 7
		Local byteIndex:= 0
		Local dark:Bool
		Local mask:Bool
		
		For Local col:= Self.moduleCount - 1 Until 0 Step - 2

			If col = 6 col -= 1

			While 1 = 1
				
				For Local c:= 0 Until 2
					
					If Self.modules[row][col - c] = QRCODE_NULL
						
						dark = False

						If byteIndex < data.Length()
							dark = ( (Lsr(data[byteIndex], bitIndex) & 1) = 1)
						EndIf

						mask = QRCodeUtilities.getMask(maskPattern, row, col - c)

						If mask dark = Not (dark)

						If dark
							Self.modules[row][col - c] = QRCODE_TRUE
						Else
							Self.modules[row][col - c] = QRCODE_FALSE
						EndIf
						
						bitIndex-= 1

						If bitIndex = -1
							byteIndex += 1
							bitIndex = 7
						EndIf
					EndIf
				Next
								
				row += inc

				If row < 0 or Self.moduleCount <= row
					row -= inc
					inc = -inc
					Exit
				EndIf
			Wend
		Next
	End
	
	Method setupPositionAdjustPattern:Void()

		Local pos:= QRCodeUtilities.getPatternPosition(Self.typeNumber)
		Local row:Int
		Local col:Int
		
		For Local i:= 0 Until pos.Length

			For Local j:= 0 Until pos.Length
				row = pos[i]
				col = pos[j]
				
				If Self.modules[row][col] <> QRCODE_NULL Continue
				
				For Local r:= -2 To 2

					For Local c:= -2 To 2

						If r = -2 or r = 2 or c = -2 or c = 2 or (r = 0 And c = 0)
							Self.modules[row + r][col + c] = QRCODE_TRUE
						Else
							Self.modules[row + r][col + c] = QRCODE_FALSE
						EndIf
					Next
				Next
			Next
		Next
	End

	Method setupPositionProbePattern:Void(row:Int, col:Int)

		For Local r:= -1 To 7

			For Local c:= -1 To 7

				If row + r <= - 1 or Self.moduleCount <= row + r or col + c <= - 1 or Self.moduleCount <= col + c Continue
					
				If (0 <= r And r <= 6 And (c = 0 or c = 6)) or (0 <= c And c <= 6 And (r = 0 or r = 6)) or (2 <= r And r <= 4 And 2 <= c And c <= 4)
					Self.modules[row + r][col + c] = QRCODE_TRUE
				Else
					Self.modules[row + r][col + c] = QRCODE_FALSE
				EndIf
			Next
		Next
	End

	Method setupTimingPattern:Void()

		For Local r:= 8 Until Self.moduleCount - 8
			If Self.modules[r][6] <> QRCODE_NULL Continue
			If r Mod 2 = 0
				Self.modules[r][6] = QRCODE_TRUE
			Else
				Self.modules[r][6] = QRCODE_FALSE
			EndIf
		Next

		For Local c:= 8 Until Self.moduleCount - 8
			If Self.modules[6][c] <> QRCODE_NULL Continue
			If c Mod 2 = 0
				Self.modules[6][c] = QRCODE_TRUE
			Else
				Self.modules[6][c] = QRCODE_FALSE
			EndIf
		Next
	End

	Method setupTypeNumber:Void(test:Bool)

		Local bits:= QRCodeUtilities.getBCHTypeNumber(Self.typeNumber)
		Local flag:Bool
		For Local i:= 0 Until 18
			flag = ( Not (test) And ( (bits Shr i) & 1) = 1)
			If flag
				Self.modules[Floor(i / 3)][i Mod 3 + Self.moduleCount - 8 - 3] = QRCODE_TRUE
			Else
				Self.modules[Floor(i / 3)][i Mod 3 + Self.moduleCount - 8 - 3] = QRCODE_FALSE
			EndIf
		Next

		For Local i:= 0 Until 18
			flag = ( Not (test) And ( (bits Shr i) & 1) = 1)
			If flag
				Self.modules[i Mod 3 + Self.moduleCount - 8 - 3][Floor(i / 3)] = QRCODE_TRUE
			Else
				Self.modules[i Mod 3 + Self.moduleCount - 8 - 3][Floor(i / 3)] = QRCODE_FALSE
			EndIf
		Next
	End
	
	Method setupTypeInfo:Void(test:Bool, maskPattern:Int)

		Local data:= Lsl(Self.errorCorrectLevel, 3) | maskPattern
		
		Local bits:= QRCodeUtilities.getBCHTypeInfo(data)

		Local modValue:Int
		
		For Local i:= 0 Until 15
			If ( Not (test) And ( (bits Shr i) & 1) = 1)
				modValue = QRCODE_TRUE
			Else
				modValue = QRCODE_FALSE
			EndIf
			
			If i < 6
				Self.modules[i][8] = modValue
			ElseIf i < 8
				Self.modules[i + 1][8] = modValue
			Else
				Self.modules[Self.moduleCount - 15 + i][8] = modValue
			EndIf
		Next

		For Local i:= 0 Until 15

			If ( Not (test) And ( (bits shr i) & 1) = 1)
				modValue = QRCODE_TRUE
			Else
				modValue = QRCODE_FALSE
			EndIf
			
			If i < 8
				Self.modules[8][Self.moduleCount - i - 1] = modValue
			ElseIf i < 9
				Self.modules[8][15 - i - 1 + 1] = modValue
			Else
				Self.modules[8][15 - i - 1] = modValue
			EndIf
		Next

		If Not (test)
			Self.modules[Self.moduleCount - 8][8] = QRCODE_TRUE
		Else
			Self.modules[Self.moduleCount - 8][8] = QRCODE_FALSE
		EndIf
	End
	
	Function createData:Int[] (typeNumber:Int, errorCorrectLevel:Int, dataArray:QRCodeData[])
		
		Local rsBlocks:= QRCodeRSBlock.getRSBlocks(typeNumber, errorCorrectLevel)
		
		Local buffer:= New QRCodeBitBuffer()
		Local data:QRCodeData
		
		For Local i:= 0 Until dataArray.Length()
			data = dataArray[i]
			buffer.put(data.getMode(), 4)
			buffer.put(data.getLength(), data.getLengthInBits(typeNumber))
			data.write(buffer)
		Next
		

		Local totalDataCount:= 0
		For Local i:= 0 Until rsBlocks.Length()
			totalDataCount += rsBlocks[i].getDataCount()
		Next

		If buffer.getLengthInBits() > totalDataCount * 8
			Throw New QRCodeException("code length overflow. (" + buffer.getLengthInBits() + ">" + (totalDataCount * 8) + ")")
		EndIf

		' end code.
		If buffer.getLengthInBits() +4 <= totalDataCount * 8 buffer.put(0, 4)

		' padding
		While buffer.getLengthInBits() Mod 8 <> 0
			buffer.put(False)
		Wend

		' padding
		While 1 = 1
			If buffer.getLengthInBits() >= totalDataCount * 8 Exit
			buffer.put(QRCODE_PAD0, 8)
			
			If buffer.getLengthInBits() >= totalDataCount * 8 Exit
			buffer.put(QRCODE_PAD1, 8)
		Wend

		Return QRCode.createBytes(buffer, rsBlocks)
	End
	
	Function createBytes:Int[] (buffer:QRCodeBitBuffer, rsBlocks:QRCodeRSBlock[])

			Local offset:= 0

			Local maxDcCount:= 0
			Local maxEcCount:= 0

			Local dcdata:Int[rsBlocks.Length()][]
			Local ecdata:Int[rsBlocks.Length()][]

			For Local r:= 0 Until rsBlocks.Length()
				Local dcCount:= rsBlocks[r].dataCount
				Local ecCount:= rsBlocks[r].totalCount - dcCount

				maxDcCount = Max(maxDcCount, dcCount)
				maxEcCount = Max(maxEcCount, ecCount)

				dcdata[r] = New Int[dcCount]

				For Local i:= 0 Until dcdata[r].Length()
					dcdata[r][i] = $ff & buffer.getBuffer()[i + offset]
				Next
				offset += dcCount

				Local rsPoly:= QRCodeUtilities.getErrorCorrectPolynomial(ecCount)
				Local rawPoly:= New QRCodePolynomial(dcdata[r], rsPoly.getLength() -1)

				Local modPoly:= rawPoly.doMod(rsPoly)
				ecdata[r] = New Int[rsPoly.getLength() -1]
				For Local i:= 0 Until ecdata[r].Length()
					Local modIndex:= i + modPoly.getLength() -ecdata[r].Length()
					If modIndex >= 0
						ecdata[r][i] = modPoly.get(modIndex)
					Else
						ecdata[r][i] = 0
					EndIf
				Next
			Next

			Local totalCodeCount:= 0
			For Local i:= 0 Until rsBlocks.Length()
				totalCodeCount += rsBlocks[i].totalCount
			Next

			Local data:Int[totalCodeCount]
			Local index:= 0

			For Local i:= 0 Until maxDcCount
				For Local r:= 0 Until rsBlocks.Length()
					If i < dcdata[r].Length()
						data[index] = dcdata[r][i]
						index += 1
					EndIf
				Next
			Next

			For Local i:= 0 Until maxEcCount
				For Local r:= 0 Until rsBlocks.Length()
					If i < ecdata[r].Length()
						data[index] = ecdata[r][i]
						index += 1
					EndIf
				Next
			Next

			Return data
		End

    Function getMinimumQRCode:QRCode(data:String, errorCorrectLevel:Int = QRCODE_ERROR_CORRECT_LEVEL_L)
        Local mode:= QRCodeUtilities.getMode(data)
        Local qr:= New QRCode()
        qr.setErrorCorrectLevel(errorCorrectLevel)
        qr.addData(data, mode)
        Local length:= qr.getData(0).getLength()
		For Local typeNumber:= 1 To 10
            If length <= QRCodeUtilities.getMaxLength(typeNumber, mode, errorCorrectLevel)
                qr.setTypeNumber(typeNumber)
                Exit
            EndIf
		Next
        qr.make()
        Return qr
    End

	Method createImage:Image(size:Int = 2, margin:Int = 2, image:Image = Null)
		' -- render to monkey image ---
		'can pass int image to reuse it
		Local total:= Self.getModuleCount()
		Local dimension:Int = (total * size) + (margin * 2)
		Local pixels:Int[dimension * dimension]
		
		Local rootOffsetX:Int
		Local rootOffsetY:Int
		Local offsetX:Int
		Local offsetY:Int
		Local offsetSub:Int
		Local x:Int
		Local y:Int
		Local subX:Int
		Local subY:Int
		
		Local white:= QRCodeMath.ARGB(255,255,255)
		Local black:= QRCodeMath.ARGB(0, 0, 0)
		
		'draw pixels
		rootOffsetX = margin
		rootOffsetY = margin * dimension
		For y = 0 Until total
			offsetY = rootOffsetY + (y * size * dimension)
			For x = 0 Until total
				'blocks
				offsetX = offsetY + rootOffsetX + (x * size)

				If Self.isDark(y, x)
					For subY = 0 Until size
						offsetSub = offsetX + (subY * dimension)
						For subX = 0 Until size
							pixels[offsetSub + subX] = black
						Next
					Next
				Else
					For subY = 0 Until size
						offsetSub = offsetX + (subY * dimension)
						For subX = 0 Until size
							pixels[offsetSub + subX] = white
						Next
					Next
				EndIf
			Next
		Next
		
		'draw border
		If margin > 0
			'top
			For y = 0 Until margin
				offsetY = y * dimension
				For x = 0 Until dimension
					pixels[offsetY + x] = white
				Next
			Next
			
			'bottom
			For y = dimension - 1 Until dimension - 1 - margin Step - 1
				offsetY = y * dimension
				For x = 0 Until dimension
					pixels[offsetY + x] = white
				Next
			Next
			
			'left 
			For y = margin Until dimension - margin
				offsetY = y * dimension
				
				'left
				For x = 0 Until margin
					pixels[offsetY + x] = white
				Next
				
				'right
				For x = dimension - margin Until dimension
					pixels[offsetY + x] = white
				Next
			Next
		EndIf
		
		'reuse or create image
		If image <> Null
			If image.Width() <> dimension or image.Height() <> dimension Throw New QRCodeException("image size doesnt match")
		Else
			image = CreateImage(dimension, dimension)
		EndIf
		
		'render to image
		image.WritePixels(pixels, 0, 0, dimension, dimension)
		
		'done?
		Return image
	End
End

Private
Class QRCodeUtilities
	Function getPatternPosition:Int[] (typeNumber:Int)
		Return QRCODE_PATTERN_POSITION_TABLE[typeNumber - 1]
	End

    Function getMaxLength:Int(typeNumber:Int, mode:Int, errorCorrectLevel:Int)
        Local t:= typeNumber - 1
        Local e:= 0
        Local m:= 0

        Select errorCorrectLevel
        	Case QRCODE_ERROR_CORRECT_LEVEL_L
				e = 0
        	Case QRCODE_ERROR_CORRECT_LEVEL_M
				e = 1
	        Case QRCODE_ERROR_CORRECT_LEVEL_Q
				e = 2
	        Case QRCODE_ERROR_CORRECT_LEVEL_H
				e = 3
	        Default
				Throw New QRCodeException("e:errorCorrectLevel")
        End

        Select mode
	        Case QRCODE_MODE_NUMBER
				m = 0
	        Case QRCODE_MODE_ALPHA_NUM
				m = 1
	        Case QRCODE_MODE_8BIT_BYTE
				m = 2
	        Case QRCODE_MODE_KANJI
				m = 3
	        Default
				Throw New QRCodeException("m:mode")
        End

        Return QRCODE_MAX_LENGTH[t][e][m]
    End

	Function getErrorCorrectPolynomial:QRCodePolynomial(errorCorrectLength:Int)
		Local a:= New QRCodePolynomial([1])

		For Local i:= 0 Until errorCorrectLength
			a = a.multiply(New QRCodePolynomial([1, QRCodeMath.gexp(i)]))
		Next

		Return a
	End
		
	Function getMask:Bool(maskPattern:Int, i:Int, j:Int)
		
		Select maskPattern
			
			Case QRCODE_MASK_PATTERN000
				Return (i + j) Mod 2 = 0
			Case QRCODE_MASK_PATTERN001
				Return i Mod 2 = 0
			Case QRCODE_MASK_PATTERN010
				Return j Mod 3 = 0
			Case QRCODE_MASK_PATTERN011
				Return (i + j) Mod 3 = 0
			Case QRCODE_MASK_PATTERN100
				Return (Floor(i / 2) + Floor(j / 3)) Mod 2 = 0
			Case QRCODE_MASK_PATTERN101
				Return (i * j) Mod 2 + (i * j) Mod 3 = 0
			Case QRCODE_MASK_PATTERN110
				Return ( (i * j) Mod 2 + (i * j) Mod 3) Mod 2 = 0
			Case QRCODE_MASK_PATTERN111
				Return ( (i * j) Mod 3 + (i + j) Mod 2) Mod 2 = 0
	
			Default
				Throw New QRCodeException("mask:maskPattern")
		End
	End

	Function getLostPoint:Int(qrCode:QRCode)
		
		Local moduleCount:= qrCode.getModuleCount()
		
		Local lostPoint:= 0
		Local sameCount:Int
		Local dark:Bool
		Local count:Int
		
		' LEVEL1
		Local wtf:Int
		For Local row:= 0 Until moduleCount

			For Local col:= 0 Until moduleCount

				sameCount = 0
				dark = qrCode.isDark(row, col)
				
				For Local r:= -1 To 1

					If row + r < 0 or moduleCount <= row + r Continue

					For Local c:= -1 To c <= 1

						If col + c < 0 or moduleCount <= col + c Continue

						If r = 0 And c = 0 Continue
						
						If dark = qrCode.isDark(row + r, col + c)
							sameCount += 1
						EndIf
					Next
				Next

				If sameCount > 5
					wtf += 1
					lostPoint += (3 + sameCount - 5)
				EndIf
			Next
		Next
		
		' LEVEL2

		For Local row:= 0 Until moduleCount - 1
			For Local col:= 0 Until moduleCount - 1
				count = 0
				If qrCode.isDark(row, col) count += 1
				If qrCode.isDark(row + 1, col) count += 1
				If qrCode.isDark(row, col + 1) count += 1
				If qrCode.isDark(row + 1, col + 1) count += 1
				If count = 0 or count = 4 lostPoint += 3
			Next
		Next

		' LEVEL3

		For Local row:= 0 Until moduleCount
			For Local col:= 0 Until moduleCount - 6
				If qrCode.isDark(row, col) And Not (qrCode.isDark(row, col + 1)) And qrCode.isDark(row, col + 2) And qrCode.isDark(row, col + 3) And qrCode.isDark(row, col + 4) And Not (qrCode.isDark(row, col + 5)) And qrCode.isDark(row, col + 6)
					lostPoint += 40
				EndIf
			Next
		Next

		For Local col:= 0 Until moduleCount
			For Local row:= 0 Until moduleCount - 6
				If qrCode.isDark(row, col) And Not (qrCode.isDark(row + 1, col)) And qrCode.isDark(row + 2, col) And qrCode.isDark(row + 3, col) And qrCode.isDark(row + 4, col) And Not (qrCode.isDark(row + 5, col)) And qrCode.isDark(row + 6, col)
					lostPoint += 40
				EndIf
			Next
		Next

		' LEVEL4
		
		Local darkCount:= 0

		For Local col:= 0 Until moduleCount
			For Local row:= 0 Until moduleCount
				If qrCode.isDark(row, col) darkCount += 1
			Next
		Next
		
		Local ratio:= Abs(100 * darkCount / moduleCount / moduleCount - 50) / 5
		lostPoint += ratio * 10
		
		Return lostPoint
	End

	Function getMode:Int(s:String)
		If QRCodeUtilities.isAlphaNum(s)
			If QRCodeUtilities.isNumber(s) Return QRCODE_MODE_NUMBER
			Return QRCODE_MODE_ALPHA_NUM
		ElseIf QRCodeUtilities.isKanji(s)
			Return QRCODE_MODE_KANJI
		Else
			Return QRCODE_MODE_8BIT_BYTE
		EndIf
	End
		
	Function isNumber:Bool(s:String)
		For Local i:= 0 Until s.Length()
			If Not (48 <= s[i] And s[i] <= 57) Return False
		Next
		Return True
	End

	Function stringHasChar:Bool(haystack:String, needle:Int)
		' --- quick search ---
		'skip
		If haystack.Length = 0 Return False
		
		'scan characters
		For Local index:= 0 Until haystack.Length
			'yup
			If haystack[index] = needle Return True
		Next
		
		'nope
		Return False
	End
	
	Function isAlphaNum:Bool(s:String)
		Local c:Int
		Local match:String = " $%*+-./:"
		For Local i:= 0 Until s.Length()
			c = s[i]
			
			If Not (48 <= c And c <= 57) And Not (65 <= c And c <= 90) And stringHasChar(match, s[i]) = False Return False
		Next
		Return True
	End

	Function isKanji:Bool(s:String)
		Local c:Int
		For Local i:= 0 Until s.Length()
			c = s[i]
			If Not ($8140 <= c And c <= $9FFC) And Not ($E040 <= c And c <= $EBBF) Return False
		Next
		Return True
		
		#rem
		Local c:Int
		While i + 1 < s.Length()
			
			c = s[i]'Lsl( ($ff & s[i]), 8) | ($ff & s[i + 1])

			If Not ($8140 <= c And c <= $9FFC) And Not ($E040 <= c And c <= $EBBF) Return False
			
			i += 2
		Wend

		If i < s.Length() Return False
		
		Return True
		#end
	End
	
	Function getBCHTypeInfo:Int(data:Int)
		Local d:= Lsl(data, 10)
		While QRCodeUtilities.getBCHDigit(d) - QRCodeUtilities.getBCHDigit(QRCODE_G15) >= 0
			d = d ~ Lsl(QRCODE_G15, (QRCodeUtilities.getBCHDigit(d) - QRCodeUtilities.getBCHDigit(QRCODE_G15)))
		Wend
		Return (Lsl(data, 10) | d) ~ QRCODE_G15_MASK
	End

	Function getBCHTypeNumber:Int(data:Int)
		Local d:= Lsl(data, 12)
		While QRCodeUtilities.getBCHDigit(d) - QRCodeUtilities.getBCHDigit(QRCODE_G18) >= 0
			d = d ~ Lsl(QRCODE_G18, (QRCodeUtilities.getBCHDigit(d) - QRCodeUtilities.getBCHDigit(QRCODE_G18)))
		Wend
		Return Lsl(data, 12) | d
	End
	
	Function getBCHDigit:Int(data:Int)

		Local digit:= 0

		While data <> 0
			digit += 1
			data = Lsr(data, 1)
		Wend

		Return digit
	End
End

Class QRCodeRSBlock

	Field totalCount:Int
	Field dataCount:Int
	
	Method New(totalCount:Int, dataCount:Int)
		Self.totalCount = totalCount
		Self.dataCount = dataCount
	End
	
	Method getDataCount:Int()
		Return Self.dataCount
	End
	
	Method getTotalCount:Int()
		Return Self.totalCount
	End

	Function getRSBlocks:QRCodeRSBlock[] (typeNumber:Int, errorCorrectLevel:Int)
		Local rsBlock:= QRCodeRSBlock.getRsBlockTable(typeNumber, errorCorrectLevel)
		Local length:= rsBlock.Length() / 3

		Local count:Int
		Local totalCount:Int
		Local dataCount:Int
		
		'create list array
		'figure out total for list
		Local listTotal:Int
		Local listIndex:Int
		For Local i:= 0 Until length
			listTotal += rsBlock[i * 3 + 0]
		Next
		Local list:QRCodeRSBlock[listTotal]
		
		'now fill it				
		For Local i:= 0 Until length
			count = rsBlock[i * 3 + 0]
			totalCount = rsBlock[i * 3 + 1]
			dataCount  = rsBlock[i * 3 + 2]

			For Local j:= 0 Until count
				list[listIndex] = New QRCodeRSBlock(totalCount, dataCount)
				listIndex += 1
			Next
		Next
		
		Return list
	End
	
	Function getRsBlockTable:Int[] (typeNumber:Int, errorCorrectLevel:Int)
		Select errorCorrectLevel
			Case QRCODE_ERROR_CORRECT_LEVEL_L
				Return QRCODE_RS_BLOCK_TABLE[ (typeNumber - 1) * 4 + 0]
			Case QRCODE_ERROR_CORRECT_LEVEL_M
				Return QRCODE_RS_BLOCK_TABLE[ (typeNumber - 1) * 4 + 1]
			Case QRCODE_ERROR_CORRECT_LEVEL_Q
				Return QRCODE_RS_BLOCK_TABLE[ (typeNumber - 1) * 4 + 2]
			Case QRCODE_ERROR_CORRECT_LEVEL_H
				Return QRCODE_RS_BLOCK_TABLE[ (typeNumber - 1) * 4 + 3]
			Default
				Throw New QRCodeException("tn:typeNumber/ecl:errorCorrectLevel")
		End
	End
End

Class QRCodeNumber Extends QRCodeData

	Method New(data:String)
		Self.Setup(QRCODE_MODE_NUMBER, data)
	End
	
	Method write:Void(buffer:QRCodeBitBuffer)

		Local data:= Self.getData()
		
		Local i:= 0

		Local num:Int
		
		while i + 2 < data.Length()
			num = QRCodeNumber.parseInt(data[i .. i + 3])
			buffer.put(num, 10)
			i += 3
		Wend
		
		If i < data.Length()
			If data.Length() -i = 1
				num = QRCodeNumber.parseInt(data[i .. i + 1])
				buffer.put(num, 4)
			ElseIf data.Length() -i = 2
				num = QRCodeNumber.parseInt(data[i .. i + 2])
				buffer.put(num, 7)
			EndIf
		EndIf
	End
	
	Method getLength:Int()
		Return Self.getData().Length()
	End

	Function parseInt:Int(s:String)
		Local num:= 0
		For Local i:= 0 Until s.Length()
			num = num * 10 + QRCodeNumber.parseInt(s[i])
		Next
		Return num
	End

	Function parseInt:Int(c:Int)
		If 48 <= c And c <= 57 Return c - 48
		Throw New QRCodeException("illegal char : " + c)
	End
End

Class QRCodeKanji Extends QRCodeData

	Method New(data:String)
		Self.Setup(QRCODE_MODE_KANJI, data)
	End
	
	Method write:Void(buffer:QRCodeBitBuffer)
		Local data:= Self.getData()
		Local i:= 0
		Local c:Int

		For Local i:= 0 Until data.Length()
			c = data[i]
			
			If ($8140 <= c And c <= $9FFC)
				c -= $8140
			ElseIf $E040 <= c And c <= $EBBF
				c -= $C140
			Else
				Throw New QRCodeException("illegal char at " + (i + 1) + "/" + c)
			EndIf
			
			c = ( (c Shr 8) & $ff) * $C0 + (c & $ff)
			buffer.put(c, 13)
		Next
		
		#rem
		While i + 1 < data.Length()
			
			c = Lsl( ($ff & data[i]), 8) | ($ff & data[i + 1])

			If ($8140 <= c And c <= $9FFC)
				c -= $8140
			ElseIf $E040 <= c And c <= $EBBF
				c -= $C140
			Else
				Throw New QRCodeException("illegal char at " + (i + 1) + "/c")
			EndIf
			
			c = ( (c Shr 8) & $ff) * $C0 + (c & $ff)

			buffer.put(c, 13)
			
			i += 2
		Wend

		If i < data.Length() Throw New QRCodeException("illegal char at " + (i + 1))
		#end
	End
	
	Method getLength:Int()
		Return Self.getData().Length()
	End
End

Class QRCodeAlphaNumber Extends QRCodeData

	Method New(data:String)
		Self.Setup(QRCODE_MODE_ALPHA_NUM, data)
	End
	
	Method write:Void(buffer:QRCodeBitBuffer)

		Local i:= 0
		Local c:= Self.getData()
		
		While i + 1 < c.Length()
			buffer.put(QRCodeAlphaNumber.getCode(c[i]) * 45 + QRCodeAlphaNumber.getCode(c[i + 1]), 11)
			i += 2
		Wend
		
		If i < c.Length() buffer.put(QRCodeAlphaNumber.getCode(c[i]), 6)
	End
	
	Method getLength:Int()
		Return Self.getData().Length()
	End
	
	Function getCode:Int(c:Int)

		If 48 <= c And c <= 57
			Return c - 48
		ElseIf 65 <= c And c <= 90
			Return c - 65 + 10
		Else
			Select c
				Case 32 Return 36'<Space>
				Case 36 Return 37'$
				Case 37 Return 38'%
				Case 42 Return 39'*
				Case 43 Return 40'+
				Case 45 Return 41'-
				Case 46 Return 42'.
				Case 47 Return 43'/
				Case 58 Return 44':
				Default
					Throw New QRCodeException("illegal char : c")
			End
		EndIf
	End
End

Class QRCode8BitByte Extends QRCodeData
	Field bytes:Int[]
	
	Method New(data:String)
		Self.Setup(QRCODE_MODE_8BIT_BYTE, data)
		
		bytes = New Int[data.Length()]
		For Local i:= 0 Until data.Length()
			bytes[i] = data[i] & $ff
		Next
	End
	
	Method write:Void(buffer:QRCodeBitBuffer)
		'get data
		For Local i:= 0 Until bytes.Length()
			buffer.put(bytes[i], 8)
		Next
	End
	
	Method getLength:Int()
		Return bytes.Length()
	End
End

Class QRCodeData
	
	Field mode:Int
	Field data:String

	Method Setup:Void(mode:Int, data:String)
		Self.mode = mode
		Self.data = data
	End
	
	Method getMode:Int()
		Return Self.mode
	End
	
	Method getData:String()
		Return Self.data
	End
	
	Method getLength:Int()
		Throw New QRCodeException("not implemented.")
	End
	
	Method write:Void(buffer:QRCodeBitBuffer) 
		Throw New QRCodeException("not implemented.")
	End

	Method getLengthInBits:Int(type:Int)
		If 1 <= type And type < 10

			' 1 - 9

			Select Self.mode
				Case QRCODE_MODE_NUMBER
					Return 10
				Case QRCODE_MODE_ALPHA_NUM
					Return 9
				Case QRCODE_MODE_8BIT_BYTE
					Return 8
				Case QRCODE_MODE_KANJI
					Return 8
				Default
					Throw New QRCodeException("mode:Self.mode")
			End

		ElseIf type < 27

			' 10 - 26

			Select Self.mode
				Case QRCODE_MODE_NUMBER
					Return 12
				Case QRCODE_MODE_ALPHA_NUM
					Return 11
				Case QRCODE_MODE_8BIT_BYTE
					Return 16
				Case QRCODE_MODE_KANJI
					Return 10
				Default
					Throw New QRCodeException("mode:Self.mode")
			End

		ElseIf type < 41

			' 27 - 40

			Select Self.mode
				Case QRCODE_MODE_NUMBER
					Return 14
				Case QRCODE_MODE_ALPHA_NUM
					Return 13
				Case QRCODE_MODE_8BIT_BYTE
					Return 16
				Case QRCODE_MODE_KANJI
					Return 12
				Default
					Throw New QRCodeException("mode:Self.mode")
			End

		Else
			Throw New QRCodeException("mode:Self.mode")
		EndIf
	End
End

Class QRCodeMath
	Global doneInit:= False
	
	Function init:Void()
		If doneInit = False
			doneInit = True
			For Local i:= 0 Until 8
				QRCODE_MATH_EXP_TABLE[i] = Lsl(1, i)
			Next
	
			For Local i:= 8 Until 256
				QRCODE_MATH_EXP_TABLE[i] = QRCODE_MATH_EXP_TABLE[i - 4] ~ QRCODE_MATH_EXP_TABLE[i - 5] ~ QRCODE_MATH_EXP_TABLE[i - 6] ~ QRCODE_MATH_EXP_TABLE[i - 8]
			Next
			
			For Local i:= 0 Until 255
				QRCODE_MATH_LOG_TABLE[QRCODE_MATH_EXP_TABLE[i]] = i
			Next
		EndIf
	End

	Function glog:Int(n:Int)
		If n < 1 Throw New QRCodeException("log(" + n + ")")
		Return QRCODE_MATH_LOG_TABLE[n]
	End
	
	Function gexp:Int(n:Int)
		While n < 0
			n += 255
		Wend

		While n >= 256
			n -= 255
		Wend

		Return QRCODE_MATH_EXP_TABLE[n]
	End
	
	Function ARGB:Int(r:Int, g:Int, b:Int, a:Int = 255)
		return(a Shl 24) | (r Shl 16) | (g Shl 8) | b
	End
End

Class QRCodePolynomial

	Field num:Int[]

	Method New(num:Int[], shift:Int = 0)

		Local offset:= 0

		While offset < num.Length() And num[offset] = 0
			offset += 1
		Wend

		Self.num = New Int[num.Length() -offset + shift]
		For Local i:= 0 Until num.Length() -offset
			Self.num[i] = num[i + offset]
		Next
	End

	Method get:Int(index:Int)
		Return Self.num[index]
	End

	Method getLength:Int()
		Return Self.num.Length()
	End

	Method ToString:String()
		Local buffer:= ""

		For Local i:= 0 Until Self.getLength()
			If i > 0 buffer += ","
			buffer += Self.get(i)
		Next

		Return buffer
	End
	
	Method toLogString:String()

		Local buffer:= ""

		For Local i:= 0 Until Self.getLength()
			If i > 0 buffer += ","
			buffer += QRCodeMath.glog(Self.get(i))
		Next

		Return buffer
	End
	
	Method multiply:QRCodePolynomial(e:QRCodePolynomial)

		Local num:Int[Self.getLength() +e.getLength() -1]

		For Local i:= 0 Until Self.getLength()
			For Local j:= 0 Until e.getLength()
				num[i + j] = num[i + j] ~ QRCodeMath.gexp(QRCodeMath.glog(Self.get(i)) + QRCodeMath.glog(e.get(j)))
			Next
		Next

		Return New QRCodePolynomial(num)
	End
	
	Method doMod:QRCodePolynomial(e:QRCodePolynomial)

		If Self.getLength() -e.getLength() < 0 Return Self

		Local ratio:= QRCodeMath.glog(Self.get(0)) - QRCodeMath.glog(e.get(0))

		Local num:Int[Self.getLength()]
		For Local i:= 0 Until Self.getLength()
			num[i] = Self.get(i)
		Next
		
		For Local i:= 0 Until e.getLength()
			num[i] = num[i] ~ QRCodeMath.gexp(QRCodeMath.glog(e.get(i)) + ratio)
		Next

		'wont this create an infinite loop?
		Return New QRCodePolynomial(num).doMod(e)
	End
End

Class QRCodeBitBuffer
	Field buffer:Int[]
	Field length:Int
	
	Method New()
		Self.buffer = New Int[QRCODE_DEFAULT_BIT_BUFFER_SIZE]
		Self.length = 0
	End

	Method getBuffer:Int[] ()
		return Self.buffer
	End
		
	Method getLengthInBits:Int()
		return Self.length
	End

	Method ToString:String()
		Local buffer:= ""
		For Local i:= 0 Until Self.getLengthInBits()
			If Self.get(i)
				buffer += "1"
			Else
				buffer += "0"
			EndIf
		Next
		return buffer
	End

	Method get:Bool(index:Int)
		Return (Lsr(Self.buffer[Floor(index / 8)], (7 - index Mod 8)) & 1) = 1
	End

	Method put:Void(num:Int, length:Int)
		For Local i:= 0 Until length
			Self.put( (Lsr(num, (length - i - 1)) & 1) = 1)
		Next
	End
	
	Method put:Void(bit:Bool)
		If Self.length = Self.buffer.Length() Self.buffer = Self.buffer.Resize(Self.buffer.Length() +QRCODE_DEFAULT_BIT_BUFFER_SIZE)
		
		If bit Self.buffer[Self.length / 8] = Self.buffer[Self.length / 8] | Lsr($80, (Self.length Mod 8))

		Self.length+= 1
	End
End
Public

'functions
Function CreateQRImage:Image(text:String, size:Int = 2, margin:Int = 2, image:Image = Null)
	' --- wrapper for creating a QRImage in 1 shot ---
	Try
		Local qr:= QRCode.getMinimumQRCode(text)
		image = qr.createImage(size, margin, image)
		Return image
	Catch exception:QRCodeException
		Return Null
	End
	Return Null
End