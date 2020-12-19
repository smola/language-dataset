//===================================================================
// *** AGS MODULE SCRIPT HEADER ***
//
// Module:  Scumm VerbCoin GUI v1.7.2b
//
// Author:  Tom "Electroshokker" Vandepoele
//
// 1 Abstract
// This script module implements a highly modifiable gui system similar to the one used 
// in the game "The Curse Of Monkey Island", also known as Monkey Island 3
//        
// 2 Revision History
// 2007-10-21 v1.0   		First Release, module is integrated in user template
// 2007-10-25 v1.1      Keyboard support added, some bugfixes, new verbcoin graphics by Misj'
// 2007-12-01 v1.2      Minor bugfixes and code cleanup
// 2007-12-01 v1.3      Doubleclick code added for 'Act' property
// 2007-12-16 v1.4      Minor bugfix - click in inventory executed look action at hotspots under inventory
// 2007-12-22 v1.5      Minor bugfix - click on empty spot in inventory and move over item caused item to be selected
// 2008-04-16 v1.5.1    Minor bugfix - resolution 800x600 now supported
// 2008-04-16 v1.5.2    Minor bugfix - walk-to function no longer gets blocked after clicking 
//                                     on something with property 'act' set to true
// 2008-04-16 v1.5.3    Minor bugfix - Code cleaned up so it's resolution independent. Bring on 1024x768, Chris! ;-)
// 2008-05-31 v1.5.4    Minor bugfix - Property action displayed in the @overhotspot@ label now shows correctly
//                                     when moving over the verbcoin gui (sometimes it displayed the action label of
//                                     the hotspot next to it
// 2008-05-31 v1.5.5    Minor bugfix - Fixed 'use % on %' not displaying the item name correctly when using 
//                                     player.activeinventory in your script
// 2008-06-15 v1.6.0    New feature: the entire module can be disabled with a simple function call
// 2008-07-19 v1.7.0    New feature - default action text 
//                      Bugfix - the correct custom action text is set when multiple instances exist on top of each other
// 2008-07-20 v1.7.1    New feature - disable inventory
// 2008-10-11 v1.7.2    Bugfix - large scrolling rooms now display verbcoin correctly
// 2018-05-13 v1.7.2b   Upgraded to be used in AGS 3.4.1 (by Ivan Mogilko)
//
// 3 License
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
// Copyright (C) 2007-2008 Tom Vandepoele
//-------------------------------------------------------------------

// module header

enum DefaultAction { aLook, aTalk, aInteract, aCustom1, aCustom2, aCustom3 };
enum ButtonChoice { bIdle, bLook, bTalk, bInteract, bCustom1, bCustom2, bCustom3 };

struct SCUMM_VERBCOIN_GUI { 
   import static function DoubleClickSpeed(int speed);
   import static function Item_Count(int count = 10);
   import static function InvScroll_Right();
   import static function InvScroll_Left();
   import static function Inv_Border_active(bool x_borders = true,bool y_borders = false); 
   import static function Inv_Border_SetPos(int top = 40,  int bottom = 295,  int left = 40,  int right = 190);
   import static function verbgraphic(ButtonChoice, int sprite_number);
   import static function Inventory_GUI(int gInventory_ID,int gInvUnderlay_ID);
   import static function Verbcoin_GUI(int gVerbcoin_ID);
   import static function GoInventory();        //Opens and closes the inventory
   import static function DisableInventory(bool value = false); //enables/disables the inventory
   import static function Select();             //Select item or use item on something else
   import static function Deselect();           //Deselect item or quit inventory
   import static function RunInteraction(CursorMode);
   import static function DisableVerbCoinGUI(bool value = false); //enables/disables the module
   import static function UseDefaultAction(bool value = false); //use default action labels?
   import static function SetDefaultAction(DefaultAction, String action_label); //set the default action label
};

import bool doubleclick;