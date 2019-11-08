/*************************************************
  文件名：randWindow.ec
  Copyright (C), 2016, ELE-Clouds.All rights reserved.
  作者：ELE-Clouds
  日期：2016年8月31日
  版本：V0.01
  描述:           // 用于详细说明此程序文件完成的主要功能，与其他模块
                  // 或函数的接口，输出值、取值范围、含义及参数间的控
                  // 制、顺序、独立或依赖等关系
  主要函数列表:   //每条记录应包括函数名及功能简要说明
    1. ....
  修改历史记录：: //每条修改记录应包括修改日期、修改者及修改内容简述
    1.修改日期:
       修改人:
       修改内容:
    2. ...
*************************************************/

import "ecere"
import "randpassword"
//import "string"

//增加数据


class mainwindow : Window
{
   caption = $"随机密码生成器 V1.00";
   background = formColor;
   borderStyle = fixed;
   hasMinimize = true;
   hasClose = true;
   hasMenuBar = true;
   hasStatusBar = true;
   clientSize = { 640, 508 };
   anchor = { horz = -20, vert = -14 };
   dontAutoScrollArea = true;
   icon = { ".\\images\\logo_PR.png" };
   moveable = true;


//-------------------
// 菜单栏
//-------------------

   Menu fileMenu { menu, "文件(File)", f };
   MenuItem setItem
   {
      fileMenu, "设置(Set up)", t;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         //Explorer { state = normal }.Create();
         return true;
      }
   };
   MenuItem langItem
   {
      fileMenu, "语言(Language)", l;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {


         return true;
      }
   };
   MenuItem exitItem { fileMenu, "退出(Exit)\tAlt+F4", x, NotifySelect = MenuFileExit };
   Menu feaMenu { menu, "功能(Features)", f };
   MenuItem testItem
   {
      feaMenu, "密码强度评测", n;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         //Explorer { state = normal }.Create();
         return true;
      }
   };
   MenuItem passmItem
   {
      feaMenu, "密码管理器", n;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         //Explorer { state = normal }.Create();
         return true;
      }
   };
   MenuItem tempmItem
   {
      feaMenu, "模板管理器", n;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         //Explorer { state = normal }.Create();
         return true;
      }
   };
   Menu aboutMenu { menu, "关于(About)", a };
   MenuItem helpItem
   {
      aboutMenu, "帮助说明(Help)", h;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         //Explorer { state = normal }.Create();
         return true;
      }
   };
   MenuItem suppItem
   {
      aboutMenu, "技术支持(Technical Support)", t;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         //Explorer { state = normal }.Create();
         return true;
      }
   };
   MenuItem aboutItem
   {
      aboutMenu, "关于(About)", a;

      bool NotifySelect(MenuItem selection, Modifiers mods)
      {

         MessageBox { caption="关于(About)", contents = "随机密码生成器\nRand Pass C  \n V1.00 \n2016年9月7日" }.Modal();
         return true;
      }
   };



//---------------[菜单栏结束]-----------------

/*
//表格
   mainwindow()
   {
      DataRow row;
      listPass.AddField(numField);
      listPass.AddField(passField);
      listPass.AddField(gradeField);

      row = listPass.AddRow();
      row.tag = 1;
      row.SetData(numField, 1);
      row.SetData(passField, "ddddd");
      row.SetData(gradeField, "★★★★★");

      row = listPass.AddRow();
      row.tag = 2;
      row.SetData(numField, 2);
      row.SetData(passField, "dddggdd");
      row.SetData(gradeField, "★★★★★");

   }*/
//------END--------


   //标签夹声明
   TabControl setTab{ this, background = formColor, tabCycle = false, anchor = { left = 5, top = 50, right = 5, bottom = 5 } };
   norTabItem norTabForm { setTab, background = formColor };
   mobTabItem mobTabForm { setTab, background = formColor };
   //--------END--------

   //模板列表
   DropBox droTemplate     //模板下拉列表框
   {
      this, caption = $"dropBox1", size = { 304, 24 }, position = { 16, 16 }, toolTip = $"请根据需要选择合适的模板。";

      bool OnPostCreate(void)       //创建列表内容
      {
         SelectRow(AddString("默认"));     //此表默认选中的意思，即程序打死后默认选中的是该项。
         //AddString("手动");                //向下添加列表项”手动“。
         //AddString("OICQ");

         return true;      //此处改为返回"true"。
      }
   };

   Button butCreate
   {
      this, caption = $"生     成", size = { 194, 37 }, position = { 392, 8 };
      
      bool NotifyClicked(Button button, int x, int y, Modifiers mods)
      {
         //int i;
         //for(i=1;i<10;i++)norTabItem::addTabItem(i);
         norTabForm.addTabItem(10);
         mobTabForm.addTabItem(10);
         //MessageBox { caption="关于(About)", contents = Password(2) }.Modal();

         return true;
      }
   };
}

//===================
// 标签夹
//===================
// 开始
//-------------------


//===普通密码功能===
class norTabItem:Tab    //普通密码标签
{
   caption = $"常规密码";
   background = formColor;

   //void addTabItem(int _rTag,String _strPass,String _strGrade);    //表格列表增加函数

   //---------------------------
   //    表格
   //---------------------------

   DataField numField { dataType = class(int), width = 35,header = "序号",editable = false  };
   DataField passField { dataType = class(String), width = 400 ,header = "安 全 密 码",editable = false };
   DataField gradeField { dataType = class(String), width = 200 , header = "防暴等级",editable = false };



//---------------------------


//==============
//参数设置
//==============
// 标签页
//--------------
   Label groPassHead { groParameterSet, this, $" 密码头部 ",anchor = { left = 5, top = 50, right = 440, bottom = 5 }, isGroupBox = true };
   Label groPassMain { groParameterSet, this, $" 密码主体 ", anchor = { left = 220, top = 50, right = 225, bottom = 5 }, isGroupBox = true };
   Label groPassEnd { groParameterSet, this, $" 密码尾部 ", anchor = { left = 440, top = 50, right = 5, bottom = 5 },isGroupBox = true };
   Label labPassQD { groParameterSet,this, $"密码强度：", background = formColor, 1, size = { 52, 13 }, position = { 64, 24 }};
   Label labX1
   {
      groParameterSet,this, $"☆",font = { "Tahoma", 15 }, position = { 120, 16 };
      //void passQD(int dengji);;

      bool OnLeftButtonDown(int x, int y, Modifiers mods)
      {
/*         caption="★";
         labX2.caption="☆";
         labX3.caption="☆";
         labX4.caption="☆";
         labX5.caption="☆";
*/         return true;
       }
   };
   Label labX2
   {
      groParameterSet, this, $"☆", font = { "Tahoma", 15 }, position = { 144, 16 };

      bool OnLeftButtonDown(int x, int y, Modifiers mods)
      {
/*         labX1.caption="★";
         caption="★";
         labX3.caption="☆";
         labX4.caption="☆";
         labX5.caption="☆";
*/         return true;
      }
   };
   Label labX3
   {
      groParameterSet, this, $"☆", font = { "Tahoma", 15 }, position = { 168, 16 };

      bool OnLeftButtonDown(int x, int y, Modifiers mods)
      {
/*         labX1.caption="★";
         labX2.caption="★";
         caption="★";
         labX4.caption="☆";
         labX5.caption="☆";
*/         return true;
      }
   };
   Label labX4
   {
      groParameterSet, this, $"☆", font = { "Tahoma", 15 }, position = { 192, 16 };

      bool OnLeftButtonDown(int x, int y, Modifiers mods)
      {
/*         labX1.caption="★";
         labX2.caption="★";
         labX3.caption="★";
         caption="★";
        labX5.caption="☆";
*/
         return true;
      }
   };
   Label labX5
   {
      groParameterSet, this, $"☆", font = { "Tahoma", 15 }, position = { 216, 16 };

      bool OnLeftButtonDown(int x, int y, Modifiers mods)
      {
/*         caption="★";
         labX2.caption="★";
         labX3.caption="★";
         labX4.caption="★";
         caption="★";
*/       return true;
      }
   };

   //--首字符设置
   //Label groPassMain { ,this, chrProName, anchor = { left = 200, top = 50, right = 200, bottom = 5 }, isGroupBox = true };
   EditBox editHead { groPassHead, this, $"editBox5", position = { 88, 48 } };
   Button cheTCharHead { groPassHead, this, $"特殊字符", background = white, position = { 8, 48 }, isCheckbox = true };
   Label labLenHead { groPassHead, this, $"长度：", position = { 8, 72 } };
   EditBox editLenHead { groPassHead, this, $"editBox1", size = { 38, 19 }, position = { 48, 72 } };
   Button cheCharNumHead { groPassHead, this, $"0 - 9", background = white, position = { 120, 24 }, isCheckbox = true };
   Button cheCharBigHead { groPassHead, this, $"A - Z", background = white, position = { 64, 24 }, isCheckbox = true };
   Button cheCharSmallHead { groPassHead, this, $"a - z", background = white, position = { 8, 24 }, isCheckbox = true };




   //--密码主体设置
   EditBox editTMain { groPassMain, this, $"editBox5", position = { 88, 48 } };
   Button cheTCharMain { groPassMain, this, $"特殊字符", background = white, position = { 8, 48 }, isCheckbox = true };
   Label labLenMain { groPassMain, this, $"长度：", position = { 8, 72 } };
   EditBox editLenMain { groPassMain, this, $"editBox1", size = { 38, 19 }, position = { 48, 72 } };
   Button cheCharNum { groPassMain, this, $"0 - 9", background = white, position = { 120, 24 }, isCheckbox = true };
   Button cheCharBig { groPassMain, this, $"A - Z", background = white, position = { 64, 24 }, isCheckbox = true };
   Button cheCharSmall { groPassMain, this, $"a - z", background = white, position = { 8, 24 }, isCheckbox = true };



   //--密码尾部设置
   EditBox editTEnd { groPassEnd, this, $"editBox5", position = { 88, 48 } };
   Button cheTCharEnd { groPassEnd, this, $"特殊字符", background = white, position = { 8, 48 }, isCheckbox = true };
   Label labLenEnd { groPassEnd, this, $"长度：", position = { 8, 72 } };
   EditBox editLenEnd { groPassEnd, this, $"editBox1", size = { 38, 19 }, position = { 48, 72 } };
   Button cheCharNumEnd { groPassEnd, this, $"0 - 9", background = white, position = { 120, 24 }, isCheckbox = true };
   Button cheCharBigEnd { groPassEnd, this, $"A - Z", background = white, position = { 64, 24 }, isCheckbox = true };
   Button cheCharSmallEnd { groPassEnd, this, $"a - z", background = white, position = { 8, 24 }, isCheckbox = true };


   //--备用设置
   Button cheZH { groParameterSet, this, $"繁体", background = white, position = { 504, 24 }, true, isCheckbox = true };
   Button cheCH { groParameterSet, this, $"简体", background = white, position = { 448, 24 }, true, isCheckbox = true };
   Button cheJapan { groParameterSet, this, $"日文", background = white, position = { 560, 24 }, true, isCheckbox = true };
   //Label groParameterSet { this, caption = $"  参数设置  ", inactive = false, anchor = { left = 5, top = 10, right = 5, bottom = 250 }, isGroupBox = true };
   Label groParameterSet
   {
      this, caption = $"  参数设置  ", inactive = false, anchor = { left = 5, top = 10, right = 5, bottom = 250 }, isGroupBox = true;

   };
   Label labAnd1 { groParameterSet, this, $"+", font = { "Tahoma", 20 }, position = { 188,90 } };
   Label labAnd2 { groParameterSet, this, $"+", font = { "Tahoma", 20 }, position = { 408,90 } };

   Label groPassList { this, caption = $"  密码列表  ", anchor = { left = 5, top = 180, right = 5, bottom = 5 }, isGroupBox = true };
   ListBox listPass
   {
      groPassList, caption = $"listBox1", borderStyle = contour, background = mintCream,anchor = { left = 5, top = 20, right = 5, bottom = 5 }, rowHeight = 20, hasHeader = true;
      
      bool NotifyActivate(Window window, bool active, Window previous)
      {
//         listPass.AddField(nameField);
//         listPass.AddField(jobField);
//         listPass.AddField(ageField);

         return true;
      }
   };

   norTabItem()      //构造函数，创建列表中的各列
   {
      listPass.AddField(numField);
      listPass.AddField(passField);
      listPass.AddField(gradeField);
   }
   //显示产生的密码
   void addTabItem(int iNum)    //iNum 为密码的个数，即显示行数。
   {
      DataRow row;
      int i;
      listPass.Clear();               //清空显示列表。
      for(i=1;i <= iNum;i++)
      {
         row = listPass.AddRow();
         row.tag = i;
         row.SetData(numField, i);
         row.SetData(passField, Password(50));
         row.SetData(gradeField, "★★★★★");
      }

   }

   bool OnActivate(bool active, Window previous, bool * goOnWithActivation, bool direct)
   {
      if(active)
      {
         //MessageBox{caption=$"提示",contents = "普通密码标签被激活！"}.Modal();

      }
      else
      {
         //MessageBox{caption=$"提示",contents = "普通密码标签失去焦点！"}.Modal();
      }
      return true;
   }


};


//===手势密码功能===
class mobTabItem : Tab     //手势密码标签
{
   caption = $"手势密码";
   background = formColor;

//表格列区域声明

   DataField numField { dataType = class(int), width = 35,header = "序号",editable = false  };
   DataField passField { dataType = class(String), width = 150 ,header = "安 全 密 码",editable = false };
   DataField gradeField { dataType = class(String), width = 50 , header = "防暴等级",editable = false };
//----------声明结束------------------


   Label groDemo { this, caption = $"  手势图例  ", background = formColor, inactive = false, anchor = { left = 5, top = 10, right = 300, bottom = 5 }, isGroupBox = true };
   Label groPassList { this, caption = $"  密码清单  ", background = formColor, inactive = false, anchor = { left = 330, top = 10, right = 5, bottom = 5 }, isGroupBox = true };
   Label labPassQD {groDemo,this, caption=$"★★★☆☆",anchor = { left = 30, top = 30, right = 30, bottom = 50 }};
   Button butSave {groDemo,this, caption=$"保存密码",anchor = { left = 30, top = 330, right = 200, bottom = 50 }};
   Button butClean { groDemo, this, $"清除手势", anchor = { left = 200, top = 330, right = 30, bottom = 50 } };
   ListBox listGesturePass
   {
      groPassList, caption = $"listBox2", background = mintCream, borderStyle = contour, anchor = { left = 5, top = 20, right = 5, bottom = 5 }, scrollArea = { 8, 20 }, rowHeight = 20, hasHeader = true;

      bool NotifyActivate(Window window, bool active, Window previous)
      {
//         listPass.AddField(nameField);
//         listPass.AddField(jobField);
//         listPass.AddField(ageField);
         return true;
      }
   };

   mobTabItem()
   {
      listGesturePass.AddField(numField);
      listGesturePass.AddField(passField);
      listGesturePass.AddField(gradeField);
   }

   void addTabItem(int iNum)    //iNum 为密码的个数，即显示行数。
   {
      DataRow row;
      int i;
      listGesturePass.Clear();               //清空显示列表。
      for(i=1;i <= iNum;i++)
      {
         row = listGesturePass.AddRow();
         row.tag = i;
         row.SetData(numField, i);
         row.SetData(passField, Password(10));
         row.SetData(gradeField, "★★★★★");
      }

   }
   
};

mainwindow Mainwindow {};
