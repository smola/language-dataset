#!c:\ring\bin\ring.exe -cgi
/*
	#Author 	:Sara Hamdy
	#Date		:2018/01/24
	#Application	:quraan in arabic web page
*/

Load "weblib.ring"
Load "datalib.ring"
Import System.Web

website = "CRUD_MVC.ring" 				# the name of that  file 

New QuranController { Routing() }

Class QuranModel from ModelBase

cSearchColumn="text"					# change it according to value in database

Class QuranController From ControllerBase

Class QuranView From ViewBase

  oLanguage = new QuranLanguageArabic

  Func AddFuncScript oPage,oController
        return   oPage.scriptfuncajax("myadd",oController.cMainURL+
                 oController.cOperation+"=add","mysubpage")

  Func FormViewContent oController,oTranslation,oPage
        return [
                        [ oTranslation.aColumnsTitles[2], "textbox", "text",
                          oController.oModel.text, oPage.stylewidth("100%")    ],
                        [ oTranslation.aColumnsTitles[3], "textbox", "sura",
                          oController.oModel.sura, oPage.stylewidth("50%") ]
                   ]

	
Class QuranLanguageArabic
	cTitle = "سورة الفاتحة"
	cBack = "عودة"
	aColumnsTitles = ["رقم الاية","المحتوي","رقم السورة "]
	cOptions = "خيارات"
	cSearch = "بحث"
	comboitems = [" إختار...","تعديل","حذف"]
	cAddRecord = "إضافة سجل"
	cEditRecord = "تعديل سجل"
	cRecordDeleted = "تم حذف السجل!"
	aMovePages = ["بداية","السابق","التالى","نهاية"]
	cPage = "الصفحة"
	cOf = "من"
	cRecordsCount = "عددالايات"
	cSave = "حفظ"
	temp = new page
	cTextAlign = temp.StyleTextLeft()
	cNoRecords = "لا توجد ايات"




