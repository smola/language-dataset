IMPORT ML;
 
/*************************************************************************** 

                           BUNDLE DECLARATION

**************************************************************************/
EXPORT Bundle := MODULE(Std.BundleBase)
  EXPORT Name := 'Substitution_cipher';
  EXPORT Description := 'Replaces cipher text with a number pattern and compares the pattern to words in a dictionary';
  EXPORT Authors := ['Gavin Witz'];
  EXPORT License := 'http://www.apache.org/licenses/LICENSE-2.0';
  EXPORT Copyright := 'Copyright (C) 2013 HPCC Systems';
  EXPORT DependsOn := [];
  EXPORT Version := '1.0.0';
  EXPORT PlatformVersion := '4.0.0';
END;
 
/*************************************************************************** 

Given cipher text, the following ECL code decrypts the longest length word. 
Once the longest word has been discovered, the resulting letter can be used 
for the rest of the text

One very fast trick is to find a word that is greater than six
characters and determine it's pattern. For each unique letter in the word it is 
assigned a sequential number. Example cipher text QWRGTRYTRQT = CONVENIENCE
												                          12345365315   12345365315
**************************************************************************/

layout_line := RECORD
	STRING line;
END;

dSentences:=DATASET([
{1,'HEWFNUXEYHC XS AHNOLFOB WYH ZURNFU XSAROTFWXRS FEELOFSDH DHOWXAXDFWXRS '+
'ZXFD QORZOFT XE F OXZRORLE QORZOFT CHEXZSHC WR HSELOH WYFW EHDLOXWB '+
'QORAHEEXRSFUE THHW F TXSXTLT EWFSCFOC RA HIDHUUHSDH XS WYH VSRJUHCZH '+
'FSC EVXUUE WYHB QREEHEE WYHOH XE F DOXWXDFU EYROWFZH RA XSAROTFWXRS '+
'EHDLOXWB QORAHEEXRSFUE XS WYH XSCLEWOB WRCFB TFSB RA WYREH HSWOLEWHC '+
'JXWY EHDLOXWB OHEQRSEXNXUXWXHE YFKH SRW OHDHXKHC WYH WOFXSXSZ SHDHEEFOB '+
'WR CR WYHXO MRNE ZXFD DHOWXAXDFWXRS HSFNUHE WYREH XS WYH EHDLOXWB '+
'XSCLEWOB WR CHTRSEWOFWH WYH CHQWY RA WYHXO FNXUXWB FSC FEELOH DLOOHSW ' +
'RO QOREQHDWXKH HTQURBHOE WYFW WYH DHOWXAXHC XSCXKXCLFU YFE WYH FNXUXWB ' +
'WR ELDDHHC'}
],ML.Docs.Types.Raw);


/*************************************************************************** 
Using ML to tokenize dSetences, and then sort according to length in descending order
**************************************************************************/

dSequenced:=ML.Docs.Tokenize.Enumerate(dSentences);
dCleaned:=ML.Docs.Tokenize.Clean(dSentences);
dSplit:=ML.Docs.Tokenize.Split(dCleaned); 

LongestWord := SORT(dSplit,-length(word));

ds_dic := DATASET('~thor::in::dictionary',layout_line,csv(terminator('\n'),separator(','), quote('')));

ds_cipher := DATASET([LongestWord[1].word], layout_line);

/*************************************************************************** 
    Returns the dictionary word if found or No Result
    param: dictionary word.
    return: dictionary word if its sequence matches that of the cipher text.
***************************************************************************/

STRING get_pattern(STRING dictionary_txt) := FUNCTION  

layout_p2 := RECORD
	STRING out2;
END;

PATTERN Alpha := PATTERN('[A-Za-z]');

ds_dictionaryText := DATASET([''], layout_line);

layout_line temp(layout_line L):= transform 
    self.line := dictionary_txt;
END;

Proj_dictionaryText := project(ds_dictionaryText, temp(left));

ps2 := RECORD
out2 := MATCHTEXT(Alpha);
END;

cipher := PARSE(ds_cipher, line, Alpha, ps2, BEST, MANY, NOCASE); 
EN_DIC := PARSE(Proj_dictionaryText, line, Alpha, ps2, BEST, MANY, NOCASE);

rec_UtilStateCount := record
    integer5 seqid;
	  string letter;
	  unsigned cnt := 1;
end;

Layout_pattern := record
	 integer5 seqid;
	 unsigned cnt := 1;
end;

rec_UtilStateCount xfm(layout_p2 L,integer cnt) := transform
    self.seqid := cnt;
	  self.letter := L.out2;
	  self.cnt := 1;
end;

Projcipher := project(cipher, xfm(left,counter));
ProjEN_DIC := project(EN_DIC, xfm(left,counter));

Sortcipher := sort(Projcipher, letter);
SortEN_DIC := sort(ProjEN_DIC, letter);

rec_UtilStateCount xfm_Utilcnt(rec_UtilStateCount L, rec_UtilStateCount R) := transform
   self.seqid := L.seqid;
	 self.letter := L.letter;
	 self.cnt := L.cnt + 1;
end;

f_cipher := rollup(Sortcipher, 
				left.letter = right.letter, 
				xfm_Utilcnt(left, right));

f_EN_DIC := rollup(SortEN_DIC, 
				left.letter = right.letter, 
				xfm_Utilcnt(left, right));				
				

Layout_pattern transPattern(rec_UtilStateCount L) := transform
	 self.seqid := L.seqid;	
	 self.cnt := L.cnt;
end;

Proj_cipher := project(f_cipher, transPattern(left));
Proj_EN_DIC := project(f_EN_DIC, transPattern(left));


retcipher := sort(Proj_cipher,seqid);
retEN_DIC := sort(Proj_EN_DIC,seqid);

//if the datasets are equal their count will be 0, resulting in the dictionary_txt
ret := if(count(retcipher - retEN_DIC) = 0,dictionary_txt,'no result');

return ret;
END;

layout_line  t3(layout_line L) := TRANSFORM
    tmp := if(LENGTH(L.line) = LENGTH(ds_cipher[1].line),get_pattern(L.line),'');  //Only get_pattern if the dictionary word length equals the longest cipher word length  
		SELF.line := tmp;  
	SELF := L;
END;

y3 := PROJECT(ds_dic, t3(LEFT));

OUTPUT(dedup(y3(line NOT IN ['','no result'])),all);

