param nbPeriodsPerDay := 4;
param nbDaySingletonChecks := 4;
param nbRooms := 13;
param nbDays := 5;
param nbCurricula := 57;
param nbCourses := 56;
param nbPeriods := nbDays * nbPeriodsPerDay;
param nbMaxListCourses := 10;
param nbMaxListPeriods := 50;
param nbMaxEventsPerCourse := 6;

set Periods   := { 1 .. nbPeriods };
set Rooms     := { 1 .. nbRooms   };  
set Courses   := { 1 .. nbCourses };
set Curricula := { 1 .. nbCurricula };
set Days      := { 1 .. nbDays };
set ListCourses := { 1 .. nbMaxListCourses };
set ListPeriods := { 1 .. nbMaxListPeriods };
set PeriodsPerDay := { 1 .. nbPeriodsPerDay };
set DaySingletonChecks := { 1 .. nbDaySingletonChecks };

param RoomHasCapacity[Rooms] := <01> 312, <02> 216, <03> 216, <04> 216, <05> 216, <06> 312, <07> 105, <08> 30, <09> 32, <10> 50, <11> 42, <12> 42, <13> 100;

param CourseHasName[Courses] := <01> "Mat2C", <02> "Mat2E", <03> "Mat2G", <04> "Mat2M", <05> "EcoIngC", <06> "EcoIngEM", <07> "EcoIngG", <08> "Fis2C", <09> "DisArc", <10> "GeoAppl", <11> "Idra", <12> "LitGeo", <13> "MecCom", <14> "TecEle", <15> "Topo", <16> "CosEdi", <17> "CosSis", <18> "ImpTec", <19> "InfIdr", <20> "PinTer", <21> "ProStr", <22> "MetPro", <23> "LabEle1", <24> "TeoSeg", <25> "ComEle", <26> "ProCir", <27> "ProCirL", <28> "Fis2G", <29> "EcoIng2G", <30> "Elettr1G", <31> "SciCos", <32> "ChiInd", <33> "EnergG", <34> "ImpChi", <35> "ModSim", <36> "SciMatG", <37> "TeoSisG", <38> "AcuApp", <39> "MacIIG", <40> "MarInd", <41> "ProEff", <42> "SciComG", <43> "SciPolG", <44> "SocInd", <45> "TecMat", <46> "TecChiM", <47> "MetalM", <48> "MecMacM", <49> "CosMacM", <50> "ProAss", <51> "TecMec", <52> "FonChi", <53> "MisEle", <54> "CCIAA2", <55> "Inglese", <56> "Inglese2";

param CourseHasTeacher[Courses] := <01> "Beato_Angelico", <02> "Botticelli", <03> "Canaletto", <04> "Canaletto", <05> "Cimabue", <06> "Correggio", <07> "Duccio_di_Buoninsegna", <08> "Filippo_Lippi", <09> "Gentile_da_Fabriano", <10> "Ghirlandaio", <11> "Giotto", <12> "Leonardo_da_Vinci", <13> "Lorenzo_Lotto", <14> "Mantegna", <15> "Masaccio", <16> "Masolino_da_Panicale", <17> "Michelangelo", <18> "Michelino_da_Besozzo", <19> "Paolo_Uccello", <20> "Perugino", <21> "Piero_della_Francesca", <22> "Pinturicchio", <23> "Pisanello", <24> "Mattia_Preti", <25> "Raffaello", <26> "Guido_Reni", <27> "Guido_Reni", <28> "Ruoppolo", <29> "Salvator_Rosa", <30> "Solimena", <31> "Tiziano", <32> "Vasari", <33> "Veronese", <34> "Andrea_del_Castagno", <35> "Antonello_da_Messina", <36> "Antonio_del_Pollaiolo", <37> "Giovanni_Bellini", <38> "Bramantino", <39> "Annibale_Carracci", <40> "Carpaccio", <41> "Pietro_Cavallini", <42> "Bernardo_Cavallino", <43> "Colantonio", <44> "Lorenzo_Costa", <45> "De_Ribera", <46> "Gianbattista_Tiepolo", <47> "Vincenzo_Foppa", <48> "Luca_Giordano", <49> "Giorgione", <50> "Giorgione", <51> "Canaletto", <52> "Piranesi", <53> "Artemisia_Gentileschi", <54> "De_Nittis", <55> "William_Blake", <56> "William_Turner";

param CourseHasStudents[Courses] := <01> 110, <02> 120, <03> 150, <04> 120, <05> 110, <06> 240, <07> 150, <08> 100, <09> 80, <10> 80, <11> 80, <12> 60, <13> 60, <14> 60, <15> 60, <16> 30, <17> 30, <18> 30, <19> 30, <20> 30, <21> 30, <22> 100, <23> 80, <24> 80, <25> 30, <26> 30, <27> 30, <28> 100, <29> 100, <30> 80, <31> 80, <32> 30, <33> 30, <34> 30, <35> 70, <36> 30, <37> 30, <38> 30, <39> 30, <40> 30, <41> 30, <42> 30, <43> 30, <44> 30, <45> 30, <46> 100, <47> 80, <48> 80, <49> 70, <50> 40, <51> 40, <52> 30, <53> 20, <54> 20, <55> 20, <56> 20;

param CourseHasEvents[Courses] := <01> 5, <02> 5, <03> 5, <04> 5, <05> 3, <06> 3, <07> 3, <08> 5, <09> 5, <10> 5, <11> 5, <12> 5, <13> 5, <14> 5, <15> 5, <16> 5, <17> 5, <18> 5, <19> 5, <20> 5, <21> 5, <22> 5, <23> 1, <24> 5, <25> 5, <26> 4, <27> 1, <28> 5, <29> 5, <30> 5, <31> 5, <32> 5, <33> 5, <34> 5, <35> 5, <36> 5, <37> 5, <38> 5, <39> 5, <40> 5, <41> 5, <42> 5, <43> 5, <44> 5, <45> 5, <46> 5, <47> 5, <48> 5, <49> 5, <50> 5, <51> 5, <52> 3, <53> 3, <54> 2, <55> 2, <56> 2;

param CourseHasMindays[Courses] := <01> 4, <02> 4, <03> 4, <04> 4, <05> 3, <06> 3, <07> 3, <08> 4, <09> 4, <10> 4, <11> 4, <12> 4, <13> 4, <14> 4, <15> 4, <16> 4, <17> 4, <18> 4, <19> 4, <20> 4, <21> 4, <22> 4, <23> 1, <24> 3, <25> 3, <26> 2, <27> 1, <28> 4, <29> 4, <30> 4, <31> 4, <32> 4, <33> 4, <34> 4, <35> 4, <36> 4, <37> 4, <38> 4, <39> 4, <40> 4, <41> 4, <42> 4, <43> 4, <44> 4, <45> 4, <46> 4, <47> 4, <48> 3, <49> 4, <50> 4, <51> 4, <52> 3, <53> 2, <54> 1, <55> 1, <56> 1;

param CourseHasDeprecatedPeriodsCount[Courses] := <01> 0, <02> 0, <03> 0, <04> 0, <05> 0, <06> 0, <07> 0, <08> 0, <09> 6, <10> 10, <11> 5, <12> 10, <13> 0, <14> 0, <15> 12, <16> 0, <17> 0, <18> 6, <19> 7, <20> 5, <21> 8, <22> 0, <23> 0, <24> 12, <25> 10, <26> 10, <27> 0, <28> 0, <29> 0, <30> 12, <31> 3, <32> 5, <33> 9, <34> 0, <35> 8, <36> 0, <37> 1, <38> 5, <39> 10, <40> 12, <41> 6, <42> 6, <43> 0, <44> 0, <45> 8, <46> 0, <47> 12, <48> 4, <49> 8, <50> 6, <51> 0, <52> 3, <53> 10, <54> 0, <55> 0, <56> 0;

param CourseHasDeprecatedPeriods[Courses * ListPeriods] := <09,1> 15, <09,2> 16, <09,3> 17, <09,4> 18, <09,5> 19, <09,6> 20, <10,1> 3, <10,2> 4, <10,3> 7, <10,4> 8, <10,5> 11, <10,6> 12, <10,7> 15, <10,8> 16, <10,9> 19, <10,10> 20, <11,1> 1, <11,2> 17, <11,3> 18, <11,4> 19, <11,5> 20, <12,1> 3, <12,2> 4, <12,3> 7, <12,4> 8, <12,5> 11, <12,6> 12, <12,7> 15, <12,8> 16, <12,9> 19, <12,10> 20, <15,1> 3, <15,2> 4, <15,3> 7, <15,4> 8, <15,5> 11, <15,6> 12, <15,7> 15, <15,8> 16, <15,9> 17, <15,10> 18, <15,11> 19, <15,12> 20, <18,1> 4, <18,2> 8, <18,3> 12, <18,4> 16, <18,5> 19, <18,6> 20, <19,1> 1, <19,2> 5, <19,3> 9, <19,4> 13, <19,5> 17, <19,6> 19, <19,7> 20, <20,1> 1, <20,2> 2, <20,3> 3, <20,4> 9, <20,5> 10, <21,1> 1, <21,2> 2, <21,3> 15, <21,4> 16, <21,5> 17, <21,6> 18, <21,7> 19, <21,8> 20, <24,1> 1, <24,2> 5, <24,3> 6, <24,4> 7, <24,5> 8, <24,6> 9, <24,7> 10, <24,8> 11, <24,9> 12, <24,10> 13, <24,11> 14, <24,12> 17, <25,1> 5, <25,2> 6, <25,3> 7, <25,4> 8, <25,5> 9, <25,6> 10, <25,7> 13, <25,8> 14, <25,9> 15, <25,10> 16, <26,1> 7, <26,2> 8, <26,3> 9, <26,4> 10, <26,5> 11, <26,6> 12, <26,7> 13, <26,8> 14, <26,9> 15, <26,10> 16, <30,1> 1, <30,2> 2, <30,3> 3, <30,4> 4, <30,5> 5, <30,6> 6, <30,7> 18, <30,8> 19, <30,9> 20, <30,10> 7, <30,11> 8, <30,12> 19, <31,1> 12, <31,2> 19, <31,3> 20, <32,1> 1, <32,2> 17, <32,3> 18, <32,4> 19, <32,5> 20, <33,1> 1, <33,2> 2, <33,3> 3, <33,4> 4, <33,5> 8, <33,6> 15, <33,7> 16, <33,8> 19, <33,9> 20, <35,1> 1, <35,2> 2, <35,3> 3, <35,4> 4, <35,5> 5, <35,6> 6, <35,7> 19, <35,8> 20, <37,1> 1, <38,1> 1, <38,2> 5, <38,3> 9, <38,4> 13, <38,5> 17, <39,1> 3, <39,2> 4, <39,3> 5, <39,4> 7, <39,5> 9, <39,6> 11, <39,7> 13, <39,8> 15, <39,9> 17, <39,10> 19, <40,1> 1, <40,2> 4, <40,3> 5, <40,4> 8, <40,5> 9, <40,6> 12, <40,7> 13, <40,8> 16, <40,9> 17, <40,10> 18, <40,11> 19, <40,12> 20, <41,1> 1, <41,2> 2, <41,3> 3, <41,4> 4, <41,5> 18, <41,6> 19, <42,1> 6, <42,2> 7, <42,3> 8, <42,4> 14, <42,5> 15, <42,6> 16, <45,1> 1, <45,2> 2, <45,3> 3, <45,4> 4, <45,5> 5, <45,6> 6, <45,7> 7, <45,8> 20, <47,1> 1, <47,2> 2, <47,3> 3, <47,4> 4, <47,5> 7, <47,6> 8, <47,7> 11, <47,8> 12, <47,9> 15, <47,10> 16, <47,11> 19, <47,12> 20, <48,1> 1, <48,2> 2, <48,3> 3, <48,4> 4, <49,1> 3, <49,2> 4, <49,3> 7, <49,4> 8, <49,5> 11, <49,6> 12, <49,7> 15, <49,8> 16, <50,1> 11, <50,2> 12, <50,3> 15, <50,4> 16, <50,5> 19, <50,6> 20, <52,1> 1, <52,2> 19, <52,3> 20, <53,1> 7, <53,2> 8, <53,3> 9, <53,4> 10, <53,5> 11, <53,6> 12, <53,7> 13, <53,8> 14, <53,9> 15, <53,10> 16;

param CurriculumHasCoursesCount[Curricula] := <01> 2, <02> 2, <03> 2, <04> 2, <05> 2, <06> 2, <07> 2, <08> 2, <09> 2, <10> 2, <11> 2, <12> 2, <13> 2, <14> 2, <15> 2, <16> 2, <17> 3, <18> 2, <19> 3, <20> 2, <21> 2, <22> 3, <23> 2, <24> 2, <25> 2, <26> 2, <27> 2, <28> 2, <29> 2, <30> 2, <31> 2, <32> 2, <33> 2, <34> 2, <35> 2, <36> 2, <37> 2, <38> 2, <39> 2, <40> 2, <41> 2, <42> 2, <43> 2, <44> 2, <45> 2, <46> 2, <47> 2, <48> 2, <49> 3, <50> 2, <51> 2, <52> 2, <53> 2, <54> 2, <55> 3, <56> 5, <57> 3;

param CurriculumHasCourses[Curricula * ListCourses] := <01,1> 1, <01,2> 5, <02,1> 1, <02,2> 5, <03,1> 8, <03,2> 29, <04,1> 11, <04,2> 48, <05,1> 10, <05,2> 11, <06,1> 9, <06,2> 11, <07,1> 13, <07,2> 15, <08,1> 14, <08,2> 15, <09,1> 12, <09,2> 15, <10,1> 17, <10,2> 21, <11,1> 18, <11,2> 21, <12,1> 20, <12,2> 21, <13,1> 16, <13,2> 21, <14,1> 19, <14,2> 21, <15,1> 2, <15,2> 6, <16,1> 22, <16,2> 28, <17,1> 23, <17,2> 24, <17,3> 30, <18,1> 25, <18,2> 37, <19,1> 25, <19,2> 26, <19,3> 27, <20,1> 3, <20,2> 7, <21,1> 28, <21,2> 29, <22,1> 23, <22,2> 30, <22,3> 31, <23,1> 31, <23,2> 46, <24,1> 32, <24,2> 34, <25,1> 35, <25,2> 36, <26,1> 33, <26,2> 35, <27,1> 35, <27,2> 49, <28,1> 35, <28,2> 37, <29,1> 40, <29,2> 41, <30,1> 41, <30,2> 44, <31,1> 40, <31,2> 42, <32,1> 42, <32,2> 44, <33,1> 40, <33,2> 43, <34,1> 43, <34,2> 44, <35,1> 40, <35,2> 47, <36,1> 44, <36,2> 47, <37,1> 38, <37,2> 40, <38,1> 38, <38,2> 44, <39,1> 18, <39,2> 40, <40,1> 18, <40,2> 44, <41,1> 39, <41,2> 40, <42,1> 39, <42,2> 44, <43,1> 40, <43,2> 45, <44,1> 44, <44,2> 45, <45,1> 4, <45,2> 6, <46,1> 8, <46,2> 46, <47,1> 47, <47,2> 48, <48,1> 36, <48,2> 49, <49,1> 23, <49,2> 30, <49,3> 49, <50,1> 33, <50,2> 49, <51,1> 50, <51,2> 51, <52,1> 35, <52,2> 37, <53,1> 18, <53,2> 39, <54,1> 42, <54,2> 43, <55,1> 15, <55,2> 52, <55,3> 55, <56,1> 23, <56,2> 24, <56,3> 30, <56,4> 54, <56,5> 55, <57,1> 26, <57,2> 53, <57,3> 55;

param CurriculumHasEventsCount[Curricula] := <01> 8, <02> 8, <03> 10, <04> 10, <05> 10, <06> 10, <07> 10, <08> 10, <09> 10, <10> 10, <11> 10, <12> 10, <13> 10, <14> 10, <15> 8, <16> 10, <17> 11, <18> 10, <19> 10, <20> 8, <21> 10, <22> 11, <23> 10, <24> 10, <25> 10, <26> 10, <27> 10, <28> 10, <29> 10, <30> 10, <31> 10, <32> 10, <33> 10, <34> 10, <35> 10, <36> 10, <37> 10, <38> 10, <39> 10, <40> 10, <41> 10, <42> 10, <43> 10, <44> 10, <45> 8, <46> 10, <47> 10, <48> 10, <49> 11, <50> 10, <51> 10, <52> 10, <53> 10, <54> 10, <55> 10, <56> 15, <57> 9;

param nbTeachers := 52;
set Teachers  := { 1 .. nbTeachers };

param TeacherHasCoursesCount[Teachers] := <01> 1, <02> 1, <03> 1, <04> 1, <05> 1, <06> 1, <07> 1, <08> 1, <09> 1, <10> 1, <11> 2, <12> 1, <13> 1, <14> 1, <15> 1, <16> 1, <17> 1, <18> 1, <19> 1, <20> 1, <21> 1, <22> 1, <23> 1, <24> 1, <25> 1, <26> 1, <27> 1, <28> 1, <29> 1, <30> 1, <31> 1, <32> 1, <33> 1, <34> 1, <35> 1, <36> 3, <37> 1, <38> 1, <39> 1, <40> 1, <41> 1, <42> 1, <43> 1, <44> 1, <45> 1, <46> 1, <47> 1, <48> 1, <49> 1, <50> 1, <51> 1, <52> 2;

param TeacherHasCourses[Teachers * ListCourses] := <01,1> 15, <02,1> 10, <03,1> 25, <04,1> 28, <05,1> 42, <06,1> 5, <07,1> 31, <08,1> 32, <09,1> 38, <10,1> 23, <11,1> 49, <11,2> 50, <12,1> 1, <13,1> 9, <14,1> 29, <15,1> 17, <16,1> 52, <17,1> 6, <18,1> 55, <19,1> 18, <20,1> 46, <21,1> 37, <22,1> 43, <23,1> 47, <24,1> 36, <25,1> 14, <26,1> 54, <27,1> 20, <28,1> 7, <29,1> 8, <30,1> 19, <31,1> 12, <32,1> 16, <33,1> 21, <34,1> 41, <35,1> 30, <36,1> 3, <36,2> 4, <36,3> 51, <37,1> 34, <38,1> 24, <39,1> 44, <40,1> 13, <41,1> 45, <42,1> 53, <43,1> 40, <44,1> 39, <45,1> 56, <46,1> 2, <47,1> 22, <48,1> 48, <49,1> 35, <50,1> 11, <51,1> 33, <52,1> 26, <52,2> 27;

param TeacherHasEventsCount[Teachers] := <01> 5, <02> 5, <03> 5, <04> 5, <05> 5, <06> 3, <07> 5, <08> 5, <09> 5, <10> 1, <11> 10, <12> 5, <13> 5, <14> 5, <15> 5, <16> 3, <17> 3, <18> 2, <19> 5, <20> 5, <21> 5, <22> 5, <23> 5, <24> 5, <25> 5, <26> 2, <27> 5, <28> 3, <29> 5, <30> 5, <31> 5, <32> 5, <33> 5, <34> 5, <35> 5, <36> 15, <37> 5, <38> 5, <39> 5, <40> 5, <41> 5, <42> 3, <43> 5, <44> 5, <45> 2, <46> 5, <47> 5, <48> 5, <49> 5, <50> 5, <51> 5, <52> 5;

param HasPeriods[Days * PeriodsPerDay] := <01,1> 1, <01,2> 2, <01,3> 3, <01,4> 4, <02,1> 5, <02,2> 6, <02,3> 7, <02,4> 8, <03,1> 9, <03,2> 10, <03,3> 11, <03,4> 12, <04,1> 13, <04,2> 14, <04,3> 15, <04,4> 16, <05,1> 17, <05,2> 18, <05,3> 19, <05,4> 20;

set HasNamedPeriods := {  <1, 1, 2, 3, 4>, <2, 5, 6, 7, 8>, <3, 9, 10, 11, 12>, <4, 13, 14, 15, 16>, <5, 17, 18, 19, 20> };
set BannedCurriculumPatterns := { 
<1, -1, -1, -1, 1, 0>, 
<1, -1, -1, 1, -1, 0>, 
<1, -1, 1, -1, -1, 0>, 
<2, -1, 1, -1, 1, 1>, 
<1, 1, -1, -1, -1, 0>, 
<2, 1, -1, -1, 1, 1>, 
<2, 1, -1, 1, -1, 1>, 
<1, 1, -1, 1, 1, 2>, 
<1, 1, 1, -1, 1, 2>};


# ------------------------------------------------------------------------------
#   Data set independent from this point on
# ------------------------------------------------------------------------------

var Taught[Periods * Rooms * Courses] binary; 
var CourseSchedule[Courses * Days] binary;
var CourseMinDayViolations[Courses] integer >= 0 <= nbDays;
var PatternPenalties[Curricula * Days] binary;

minimize value:  
    (sum <p> in Periods: sum <r> in Rooms: sum <c> in Courses with CourseHasStudents[c] > RoomHasCapacity[r]:
      (Taught[p,r,c] * (CourseHasStudents[c] - RoomHasCapacity[r]))
    )
  + 2 * (sum <cu> in Curricula: sum <d> in Days: PatternPenalties[cu,d]) 
  + 5 * (sum <c> in Courses: CourseMinDayViolations[c])
;

subto ctSessionAllocation: 
  forall <c> in Courses:
  sum <r> in Rooms: 
  sum <p> in Periods: 
  Taught[p,r,c] == CourseHasEvents[c]; 

subto ctSessionsDistinctRooms:
  forall <p> in Periods:
  forall <c> in Courses:
  sum <r> in Rooms:
  Taught[p,r,c] <= 1;

subto ctRoomOccupancy:
  forall <r> in Rooms:
  forall <p> in Periods:
  sum <c> in Courses:
  Taught[p,r,c] <= 1;

subto ctCurriculumWithoutConflicts:
  forall <p> in Periods:
  forall <cu> in Curricula:
  sum <r> in Rooms:
  sum <cnt> in ListCourses with cnt <= CurriculumHasCoursesCount[cu]:  
  Taught[p,r,CurriculumHasCourses[cu,cnt]] <= 1;

subto ctCurriculumCheck:
  forall <cu> in Curricula:
  sum <p> in Periods:
  sum <r> in Rooms:
  sum <cnt> in ListCourses with cnt <= CurriculumHasCoursesCount[cu]:  
  Taught[p,r,CurriculumHasCourses[cu,cnt]] == CurriculumHasEventsCount[cu];
  
subto ctTeacherWithoutConflicts:
  forall <p> in Periods:
  forall <t> in Teachers:
  sum <r> in Rooms:
  sum <cnt> in ListCourses with cnt <= TeacherHasCoursesCount[t]:
  Taught[p,r,TeacherHasCourses[t,cnt]] <= 1; 

subto ctTeacherCheck:
  forall <t> in Teachers:
  sum <p> in Periods:
  sum <r> in Rooms:
  sum <cnt> in ListCourses with cnt <= TeacherHasCoursesCount[t]:
  Taught[p,r,TeacherHasCourses[t,cnt]] == TeacherHasEventsCount[t]; 
 
subto ctAvailabilities: 
  forall <c> in Courses with CourseHasDeprecatedPeriodsCount[c] > 0:
  sum <cnt> in ListPeriods with cnt <= CourseHasDeprecatedPeriodsCount[c]:
  sum <r> in Rooms:
  Taught[CourseHasDeprecatedPeriods[c,cnt],r,c] == 0; 
  
subto ctSurveyWorkingDays1:
  forall <c> in Courses:
  forall <d> in Days:
  forall <cnt> in PeriodsPerDay:
  sum <r> in Rooms:
  Taught[HasPeriods[d,cnt],r,c] <= CourseSchedule[c,d];

subto ctSurveyWorkingDays2:   
  forall <c> in Courses:
  forall <d> in Days:
  sum <r> in Rooms:
  sum <cnt> in PeriodsPerDay:
  Taught[HasPeriods[d,cnt],r,c] >= CourseSchedule[c,d];
  
subto ctCountWorkingDays:
  forall <c> in Courses: 
  sum <d> in Days:
  CourseSchedule[c,d] >= CourseHasMindays[c] - CourseMinDayViolations[c];    

subto ctPenalisePatterns:
  forall <cu> in Curricula:
  forall <penalty, x1, x2, x3, x4, minus> in BannedCurriculumPatterns:
  forall <d, p1, p2, p3, p4> in HasNamedPeriods:
  penalty * (
  x1*(sum <cnt> in ListCourses with cnt <= CurriculumHasCoursesCount[cu]: 
      sum <r> in Rooms: 
      Taught[p1,r,CurriculumHasCourses[cu,cnt]]) + 
  x2*(sum <cnt> in ListCourses with cnt <= CurriculumHasCoursesCount[cu]: 
      sum <r> in Rooms: 
      Taught[p2,r,CurriculumHasCourses[cu,cnt]]) +
  x3*(sum <cnt> in ListCourses with cnt <= CurriculumHasCoursesCount[cu]: 
      sum <r> in Rooms: 
      Taught[p3,r,CurriculumHasCourses[cu,cnt]]) +
  x4*(sum <cnt> in ListCourses with cnt <= CurriculumHasCoursesCount[cu]: 
      sum <r> in Rooms: 
      Taught[p4,r,CurriculumHasCourses[cu,cnt]]) - 
  minus)
  <= PatternPenalties[cu,d];
