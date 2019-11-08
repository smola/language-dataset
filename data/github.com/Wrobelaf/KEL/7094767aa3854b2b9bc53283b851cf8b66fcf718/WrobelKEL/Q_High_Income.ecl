//HPCC Systems KEL Compiler Version 0.11.2
IMPORT KEL011 AS KEL;
IMPORT E_Person FROM WrobelKEL;
IMPORT * FROM KEL011.Null;
EXPORT Q_High_Income := MODULE
  SHARED TYPEOF(E_Person.__Result) __E_Person := E_Person.__Result;
  SHARED __EE2099 := __E_Person;
  SHARED __EE2024 := __EE2099._income_;
  SHARED __EE2681 := __EE2099;
  SHARED __EE2721 := NORMALIZE(__EE2681,__T(LEFT._income_),TRANSFORM(E_Person._income_Layout,SELF:=RIGHT));
  SHARED __EE2726 := __EE2721;
  __JC2885(E_Person._income_Layout __EE2024) := __T(__OP2(__EE2024._income_,>,KEL.Aggregates.AveN(__EE2726,__EE2726._income_)));
  SHARED __EE2886 := __EE2099(EXISTS(__CHILDJOINFILTER(__EE2024,__JC2885)));
  SHARED __ST2488_Layout := RECORD
    KEL.typ.nuid UID;
    KEL.typ.ndataset(E_Person._name_Layout) _name_;
    KEL.typ.nstr _sex_;
    KEL.typ.ndataset(E_Person._bdate_Layout) _bdate_;
    KEL.typ.ndataset(E_Person._age_Layout) _age_;
    KEL.typ.ndataset(E_Person._income_Layout) _income_;
    KEL.typ.ndataset(E_Person._address_Layout) _address_;
    KEL.typ.nstr _fname_;
    KEL.typ.nstr _mname_;
    KEL.typ.nstr _lname_;
    KEL.typ.int __RecordCount := 0;
  END;
  __ST2488_Layout __JT2798(E_Person.Layout __l, KEL.typ.int __c) := TRANSFORM
    __r := (__T(__l._name_))[__c];
    SELF.__RecordCount := __r.__RecordCount;
    SELF := __l;
    SELF := __r;
  END;
  SHARED __EE2799 := NORMALIZE(__EE2886,MAX(1,COUNT(__T(LEFT._name_))),__JT2798(LEFT,COUNTER));
  SHARED __ST381_Layout := RECORD
    KEL.typ.nuid UID;
    KEL.typ.nstr _fname_;
    KEL.typ.nstr _lname_;
    KEL.typ.ndataset(E_Person._income_Layout) _income_;
    KEL.typ.nfloat A_V_E__income_;
    KEL.typ.int __RecordCount := 0;
  END;
  SHARED __ST381_Layout __ND2871__Project(__ST2488_Layout __PP2800) := TRANSFORM
    SELF.A_V_E__income_ := KEL.Aggregates.AveN(__EE2721,__EE2721._income_);
    SELF := __PP2800;
  END;
  SHARED __ST381_Layout __ND2871__Rollup(__ST381_Layout __r, DATASET(__ST381_Layout) __recs) := TRANSFORM
    SELF.__RecordCount := SUM(__recs,__RecordCount);
    SELF := __r;
  END;
  EXPORT Res0 := __UNWRAP(ROLLUP(GROUP(DISTRIBUTE(PROJECT(__EE2799,__ND2871__Project(LEFT)),HASH(UID)),UID,_fname_,_lname_,A_V_E__income_,LOCAL,ALL),GROUP,__ND2871__Rollup(LEFT, ROWS(LEFT))));
END;
