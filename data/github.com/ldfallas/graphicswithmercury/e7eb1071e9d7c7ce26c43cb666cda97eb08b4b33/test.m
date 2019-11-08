:- module test.

:- interface.

:- import_module io.

:- pred main(io::di,io::uo) is det.

:- implementation.

:- import_module bmp.
:- import_module int.
:- import_module array.

:- func some_func(int) = int.
some_func(P) = R :- %(
   XC = P mod (1280*3),  
   Y = (P / (1280*3)) - 200,
   X = (XC / 3) - 200,
   RGB = XC mod 3,
   (if (RGB = 2, (X*X + Y*Y < 500*500)) then
      R = 200%X mod 255
   else
      R = 0).

main(!IO) :-
    io.write_string("Generating 'file.bmp'... ",!IO),

    ImgArray = array.generate(3*1280*1024, some_func), 

    bmp.write_bmp("file.bmp",1280,1024,ImgArray,!IO).
    
