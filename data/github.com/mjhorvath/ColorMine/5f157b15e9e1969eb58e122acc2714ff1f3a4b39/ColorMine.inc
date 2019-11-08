// Title: ColorMine for POV-Ray
// Authors: Michael Horvath
// Version: 0.1.5
// Created: 2016-11-19
// Updated: 2018-08-23
// Description: This include file is an incomplete adaption of ColorMine 
// located online at http://colormine.org/. Unfortunately, POV-Ray does not yet 
// support the creation of objects in the object-oriented programming sense, so
// I had to make a lot of changes to the code to make everything work.
// License:
//
// The MIT License (MIT)
// 
// Copyright (c) 2013 ColorMine.org
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.


#version 3.7;

#macro GetDenominator(Vector)
	#local Color = color Vector;
	Color.red + 15 * Color.green + 3 * Color.blue;
#end

#declare XYZWhiteReference = color <95.047,100.000,108.883>;
#declare XYZEpsilon = 0.008856;
#declare XYZKappa = 903.3;
#declare uPrime = 4 * XYZWhiteReference.red/GetDenominator(XYZWhiteReference);
#declare vPrime = 9 * XYZWhiteReference.green/GetDenominator(XYZWhiteReference);

#macro round(X, Q)
	ceil(X * pow(10, Q) - 0.5) * pow(0.1, Q)
#end

// input L: between 0 and 100
// input C: between 0 and 128
// input H: between 0 and 360
// output L: between 0 and 100
// output A: between -128 and +128
// output B: between -128 and +128
#macro CLCH2LAB(Color)
	#local LCHFT = color Color;
	#local L = LCHFT.red;
	#local C = LCHFT.green;
	#local H = LCHFT.blue;
	#local hRadians = radians(H);
	#local A = cos(hRadians) * C;
	#local B = sin(hRadians) * C;
	<L,A,B>
#end


// input L: between 0 and 100
// input A: between -128 and +128
// input B: between -128 and +128
// output X: between 0 and 100
// output Y: between 0 and 100
// output Z: between 0 and 100
#macro CLAB2XYZ(Color)
	#local LABFT = color Color;
	#local L = LABFT.red;
	#local A = LABFT.green;
	#local B = LABFT.blue;
	#local Y = (L + 16) / 116;
	#local X = Y + A / 500;
	#local Z = Y - B / 200;
	#local X3 = X * X * X;
	#local Z3 = Z * Z * Z;
	#local X = XYZWhiteReference.red	* (X3 > XYZEpsilon ? X3 : (X - 16.0 / 116.0) / 7.787);
	#local Y = XYZWhiteReference.green	* (L > (XYZKappa * XYZEpsilon) ? pow(((L + 16) / 116), 3) : L / XYZKappa);
	#local Z = XYZWhiteReference.blue	* (Z3 > XYZEpsilon ? Z3 : (Z - 16.0 / 116.0) / 7.787);
	<X,Y,Z>
#end


// input X: between 0 and 100
// input Y: between 0 and 100
// input Z: between 0 and 100
// output R: between 0 and 1 (should maybe change this to betwen 0 and 255 per ColorMine)
// output G: between 0 and 1 (should maybe change this to betwen 0 and 255 per ColorMine)
// output B: between 0 and 1 (should maybe change this to betwen 0 and 255 per ColorMine)
// Note that out-of-gamut colors are *NOT* corrected.
#macro CXYZ2RGB(Color)
	#local XYZFT = color Color;
	#local X = (XYZFT.red)/100;
	#local Y = (XYZFT.green)/100;
	#local Z = (XYZFT.blue)/100;
	#local R = X *  3.2406 + Y * -1.5372 + Z * -0.4986;
	#local G = X * -0.9689 + Y *  1.8758 + Z *  0.0415;
	#local B = X *  0.0557 + Y * -0.2040 + Z *  1.0570;
	#local R = (R > 0.0031308 ? 1.055 * pow(R, 1 / 2.4) - 0.055 : 12.92 * R);
	#local G = (G > 0.0031308 ? 1.055 * pow(G, 1 / 2.4) - 0.055 : 12.92 * G);
	#local B = (B > 0.0031308 ? 1.055 * pow(B, 1 / 2.4) - 0.055 : 12.92 * B);
	<R,G,B>
#end


// untested
// input L: between 0 and 100
// input U: between -128 and +128
// input V: between -128 and +128
// output X: between 0 and 100
// output Y: between 0 and 100
// output Z: between 0 and 100
#macro CLUV2XYZ(Color)
	#local LUVFT = color Color;
	#local L = LUVFT.red;
	#local U = LUVFT.green;
	#local V = LUVFT.blue;
	#local c = -1/3;
	#local a = 1/3 * (52 * L/(U + 13 * L * uPrime) - 1);
	#local itemL_16_116 = (L + 16)/116;
	#local Y = (L > XYZKappa * XYZEpsilon ? pow(itemL_16_116,3) : L/XYZKappa);
	#local b = -5 * Y;
	#local d = Y * (39 * L/(V + 13 * L * vPrime) - 5);
	#local X = (d - b)/(a - c);
	#local Z = X * a + b;
	#local X = X * 100;
	#local Y = Y * 100;
	#local Z = Z * 100;
	<X,Y,Z>
#end


// untested
// input H: between 0 and 360
// input C: between 0 and 128
// input L: between 0 and 100
// output L: between 0 and 100
// output U: between -128 and +128
// output V: between -128 and +128
#macro CHCL2LUV(Color)
	#local HCLFT = color Color;
	#local H = HCLFT.red;
	#local C = HCLFT.green;
	#local L = HCLFT.blue;
	#local hRadians = radians(H);
	#local U = cos(hRadians) * C;
	#local V = sin(hRadians) * C;
	<L,U,V>
#end


// input R: between 0 and 1
// input G: between 0 and 1
// input B: between 0 and 1
// output R: between 00 and FF
// output G: between 00 and FF
// output B: between 00 and FF
#macro CRGB2HEX(Color)
	#local RGBFT = color Color;
	#local R = RGBFT.red;
	#local G = RGBFT.green;
	#local B = RGBFT.blue;
	concat(CDEC2HEX(R), CDEC2HEX(G), CDEC2HEX(B))
#end


// input: number between 0 and 1
// output: hexadecimal number between 00 and FF
#macro CDEC2HEX(n)
	#local HexArray = array[16] {"0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"}
	#local n = max(0, min(255, round(n * 255, 0)));
	#local Num1 = (n - mod(n, 16))/16;
	#local Num2 = mod(n, 16);
	concat(HexArray[Num1], HexArray[Num2])
#end


// input X: between 0 and 100
// input Y: between 0 and 100
// input Z: between 0 and 100
// output x: between 0 and 1
// output y: between 0 and 1
// output Y: between 0 and 100
#macro CXYZ2XYY(Color)
	#local Y1 = Color.y;
	#local xDividend = Color.x + Color.y + Color.z;
	#local X = Color.x / xDividend;
	#local y2Dividend = Color.x + Color.y + Color.z;
	#local Y2 = Color.y / y2Dividend;
	<X,Y2,Y1>
#end

// input X: between 0 and 100
// input Y: between 0 and 100
// input Z: between 0 and 100
// output L: between 0 and 100
// output A: between -128 and +128
// output B: between -128 and +128
#macro CXYZ2LAB(Color)
	#local X = PivotXyz(Color.x / XYZWhiteReference.x);
	#local Y = PivotXyz(Color.y / XYZWhiteReference.y);
	#local Z = PivotXyz(Color.z / XYZWhiteReference.z);
	#local L = max(0, 116 * Y - 16);
	#local A = 500 * (X - Y);
	#local B = 200 * (Y - Z);
	<L,A,B>
#end

// utility
#macro PivotXyz(n)
	#if (n > XYZEpsilon)
		#local nOut = pow(n, 1/3);
	#else
		#local nOut = (XYZKappa * n + 16) / 116;
	#end
	nOut
#end


// input x: between 0 and 1
// input y: between 0 and 1
// input Y: between 0 and 100
// output X: between 0 and 100
// output Y: between 0 and 100
// output Z: between 0 and 100
#macro CXYY2XYZ(Color)
	#local X = Color.x * (Color.z / Color.y);
	#local Y = Color.z;
	#local Z = (1.0 - Color.x - Color.y) * (Color.z / Color.y);
	<X,Y,Z>
#end


// input L: between 0 and 100
// input A: between -128 and +128
// input B: between -128 and +128
// output H: between 0 and 360
// output C: between 0 and 128
// output L: between 0 and 100
#macro CLAB2LCH(Color)
	#local h = atan2(Color.z, Color.y);
	#if (h > 0)
		#local h = (h / pi) * 180;
	#else
		#local h = 360 - (abs(h) / pi) * 180;
	#end
	#if (h < 0)
		#local h = h + 360;	// should use modulos instead
	#elseif (h >= 360)
		#local h = h - 360;	// should use modulos instead
	#end
	#local L = Color.x;
	#local C = sqrt(Color.y * Color.y + Color.z * Color.z);
	#local H = h;
	<L,C,H>
#end


// input L: between 0 and 100
// input U: usually between -128 and +128
// input V: usually between -128 and +128
// output H: between 0 and 360
// output C: between 0 and 200
// output L: between 0 and 100
#macro CLUV2HCL(Color)
	#local h = atan2d(Color.z, Color.y);
//	#if (h > 0)
//		#local h = (h / pi) * 180;
//	#else
//		#local h = 360 - (abs(h) / pi) * 180;
//	#end
	#if (h < 0)
		#local h = h + 360;	// should use modulos instead
	#elseif (h >= 360)
		#local h = h - 360;	// should use modulos instead
	#end
	#local L = Color.x;
	#local C = sqrt(Color.y * Color.y + Color.z * Color.z);
	#local H = h;
	<H,C,L>
#end
