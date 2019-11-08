/**
*  Roxie Query
*  
*  - Target Roxie and Compile
*  - Publish via ECLWatch
*/

IMPORT $.DataDictionary as DD;

EXPORT Step01b() := FUNCTION
    INTEGER3 personID_value := -1 : stored('personID');

    AcctsRecord := RECORD
        unsigned3 personid;
        string20 account;
        string8 opendate;
        string2 industrycode;
        string1 accttype;
        string1 acctrate;
        unsigned1 code1;
        unsigned1 code2;
        unsigned4 highcredit;
        unsigned4 balance;
    END;

    File := DATASET('~progguide::exampledata::accounts', AcctsRecord, FLAT);
    FilePlus := DATASET('~progguide::exampledata::accounts', {AcctsRecord, UNSIGNED8 RecPos{VIRTUAL(fileposition)}}, FLAT);
    personAccountsIdx := INDEX(FilePlus, {personid, RecPos}, '~progguide::exampledata::keys::accounts.personid');
    fetched := FETCH(FilePlus, personAccountsIdx(personID=personID_value), RIGHT.RecPos);
    RETURN OUTPUT(CHOOSEN(fetched, 2000), NAMED('Accounts'), OVERWRITE);
END;
