000001*
000002*  Game of Life
000003* 
000004* 
000005* 
000006* 
000007* 
000008* 
000009* 
000010* 
000011* 
000012* 
000013* 
000014* 
000015
000016  IDENTIFICATION DIVISION.
000017  PROGRAM-ID. GameOfLife.
000018  
000019  ENVIRONMENT DIVISION.
000020  INPUT-OUTPUT SECTION.
000021  FILE-CONTROL.
000022      SELECT InputFile ASSIGN TO 'input.txt'
000023        ORGANIZATION IS LINE SEQUENTIAL
000024        FILE STATUS IS inputErr.
000025      SELECT OutputFile ASSIGN TO fname
000026        ORGANIZATION IS LINE SEQUENTIAL
000027        FILE STATUS IS outputErr.
000028             
000029  DATA DIVISION.
000030  FILE SECTION.
000031  FD InputFile.
000032  01 InputPattern.
000033      02 ReadLine PIC X(100).
000034  FD OutputFile.
000035  01 OutputPattern.
000036      02 WriteLine OCCURS 1 TO 100 TIMES DEPENDING ON strlen.
000037          03 LineContent PIC X.
000038
000039  WORKING-STORAGE SECTION.
000040  01 Pattern.
000041      02 PatternName PIC X(80).
000042      02 Generation PIC 9(5).
000043      02 Row PIC 9(3).
000044      02 Col PIC 9(2).
000045      02 Matrix PIC X(82) VALUE '0' OCCURS 1 TO 102 TIMES DEPENDING ON Row.
000046  01 inputErr PIC XX.
000047  01 outputErr PIC XX.
000048  01 str PIC X(102).
000049  01 tempstr PIC X(102).
000050  01 GenerationString PIC Z(5).
000051  01 mString PIC Z(5).
000052  01 i PIC 9(5) VALUE 2.
000053  01 j PIC 9(5) VALUE 2.
000054  01 k PIC 9(5).
000055  01 l PIC 9(5).
000056  01 m PIC 9(5) VALUE 1.
000057  01 n PIC 9(5).
000058  01 alifeCellsCount PIC 9(1).
000059  01 NextPattern.
000060      02 NextMatrix PIC X(82) VALUE '0' OCCURS 1 TO 102 TIMES DEPENDING ON Row.
000061  01 isStillLife PIC 9(1) VALUE 1.
000062  01 fname PIC X(90).
000063  01 strlen PIC 9(3).
000064  
000065  PROCEDURE DIVISION.
000066  MAIN-PARAGRAPH.
000067* Read input file
000068      OPEN INPUT InputFile
000069      IF NOT inputErr = '00' THEN
000070        DISPLAY 'No input files found. Program exit peacefully.'
000071        GO TO PROGRAM-END
000072      END-IF
000073      READ InputFile INTO PatternName
000074      READ InputFile INTO str
000075      UNSTRING str DELIMITED BY ALL SPACES
000076        INTO Generation
000077      END-UNSTRING
000078      READ InputFile INTO str
000079      UNSTRING str DELIMITED BY ALL SPACES
000080        INTO Row, Col
000081      END-UNSTRING.
000082  LOOP-1.
000083      IF i <= Row + 1 THEN
000084        READ InputFile INTO Matrix(i)(2:80)
000085        COMPUTE i = i + 1
000086        GO TO LOOP-1
000087      END-IF
000088      CLOSE InputFile
000089      COMPUTE i = 2.
000090* Count number of neighbor alife cells
000091  LOOP-2.
000092      PERFORM ALIFE-CELLS-COUNT
000093      PERFORM COMPUTE-NEXT-GEN-CELL-STATUS
000094      PERFORM CHECK-STILL-LIFE
000095      COMPUTE j = j + 1
000096      IF j <= Col + 1 THEN
000097        GO TO LOOP-2
000098      END-IF
000099      COMPUTE i = i + 1
000100      IF i <= Row + 1 THEN 
000101        COMPUTE j = 2
000102        GO TO LOOP-2
000103      END-IF
000104* Migrate to next generation
000105      COMPUTE i = 2
000106      COMPUTE j = 2
000107      IF m <= Generation AND isStillLife = 0 THEN
000108        COMPUTE n = 1
000109        PERFORM PREP-FOR-NEXT-GEN
000110        COMPUTE m = m + 1
000111        GO TO LOOP-2
000112      END-IF
000113* Write to output file
000114      PERFORM WRITE-TO-FILE.
000115  PROGRAM-END.  
000116      STOP RUN.
000117
000118  ALIFE-CELLS-COUNT SECTION.
000119* Reset values for each cell
000120      COMPUTE k = i - 1
000121      COMPUTE l = j - 1
000122      COMPUTE alifeCellsCount = 0.
000123  LOOP-3.
000124* Count for neighbor alife cells
000125      IF Matrix(k)(l:1) = '*' AND NOT (k = i AND l = j) THEN
000126        COMPUTE alifeCellsCount = alifeCellsCount + 1
000127      END-IF
000128      COMPUTE l = l + 1
000129      IF l <= j + 1 THEN
000130        GO TO LOOP-3
000131      END-IF
000132      COMPUTE k = k + 1
000133      IF k <= i + 1 THEN
000134        COMPUTE l = j - 1
000135        GO TO LOOP-3
000136      END-IF.
000137
000138  COMPUTE-NEXT-GEN-CELL-STATUS SECTION.
000139* Cells do not satisfy the subsequent IF clause will remain or become "dead"
000140      MOVE '0' to NextMatrix(i)(j:1)
000141* Conditions for cells to birth or survive
000142      IF alifeCellsCount = 3 OR (alifeCellsCount = 2 AND Matrix(i)(j:1) = '*') THEN
000143         MOVE '*' to NextMatrix(i)(j:1)
000144      END-IF.
000145  
000146  CHECK-STILL-LIFE SECTION.
000147      IF NOT Matrix(i)(j:1) = NextMatrix(i)(j:1) THEN
000148         COMPUTE isStillLife = 0
000149      END-IF.
000150
000151  PREP-FOR-NEXT-GEN SECTION.
000152      COMPUTE isStillLife = 1.
000153  LOOP-4.
000154      IF n <= Row + 1 THEN
000155        MOVE NextMatrix(n) TO Matrix(n)
000156        COMPUTE n = n + 1
000157        GO TO LOOP-4
000158      END-IF.
000159
000160  WRITE-TO-FILE SECTION.
000161      STRING 
000162        PatternName DELIMITED BY SPACES 
000163        'cob.txt' DELIMITED BY SIZE
000164        INTO fname
000165      END-STRING
000166      OPEN OUTPUT OutputFile
000167      COMPUTE strlen = Col.
000168* Output pattern
000169  LOOP-5.
000170      MOVE Matrix(i)(j:1) TO WriteLine(j - 1)
000171      COMPUTE j = j + 1
000172      IF j <= Col + 1 THEN
000173        GO TO LOOP-5
000174      END-IF
000175      WRITE OutputPattern
000176      COMPUTE i = i + 1
000177      IF i <= Row + 1 THEN 
000178        COMPUTE j = 2
000179        GO TO LOOP-5
000180      END-IF
000181* Output still life sentence
000182      MOVE Generation TO GenerationString
000183      COMPUTE m = m - 1
000184      MOVE m TO mString
000185      COMPUTE m = m + 1
000186      COMPUTE i = 0
000187      COMPUTE j = 0
000188      INSPECT GenerationString TALLYING i FOR LEADING ' '
000189      INSPECT mString TALLYING j FOR LEADING ' '
000190* Case for not a still life
000191      IF isStillLife = 0 THEN
000192         MOVE ' steps.' to tempstr
000193         IF Generation = 1 THEN
000194            MOVE ' step.' to tempstr
000195         END-IF
000196         STRING 
000197           'It is still not a still life even after ' DELIMITED BY SIZE 
000198           GenerationString(i + 1:5 - i) DELIMITED BY SIZE
000199           tempstr DELIMITED BY SIZE
000200           INTO str
000201         END-STRING
000202      END-IF
000203* Case for a still life initially
000204      IF isStillLife = 1 AND m = 1 THEN
000205         MOVE 'It is a still life initially.' TO str
000206      END-IF
000207* Case for a still life after N steps
000208      IF isStillLife = 1 AND m <= Generation + 1 AND NOT m = 1 THEN
000209         MOVE ' steps.' to tempstr
000210         STRING 
000211           'It is a still life after ' DELIMITED BY SIZE 
000212           mString(j + 1:5 - j) DELIMITED BY SIZE
000213           tempstr DELIMITED BY SIZE
000214           INTO str
000215         END-STRING
000216      END-IF
000217      COMPUTE i = 1
000218      COMPUTE strlen = 1
000219      INSPECT str TALLYING strlen FOR CHARACTERS BEFORE INITIAL '.'.
000220  LOOP-6.
000221      MOVE str(i:1) to WriteLine(i)
000222      IF i < strlen THEN
000223         COMPUTE i = i + 1
000224         GO TO LOOP-6
000225      END-IF
000226      WRITE OutputPattern     
000227      CLOSE OutputFile.
000228
