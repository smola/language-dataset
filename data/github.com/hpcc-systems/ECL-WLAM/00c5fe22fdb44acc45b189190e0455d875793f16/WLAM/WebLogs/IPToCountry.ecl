EXPORT IPToCountry := MODULE
IMPORT Std;
IMPORT * FROM WLAM.WebLogs;
/*

Downloaded from http://software77.net/geo-ip/

# IP FROM      IP TO        REGISTRY  ASSIGNED   CTRY CNTRY COUNTRY
# "1346797568","1346801663","ripencc","20010601","il","isr","Israel"
#
# IP FROM & : Numerical representation of IP address.
# IP TO       Example: (from Right to Left)
#             1.2.3.4 = 4 + (3 * 256) + (2 * 256 * 256) + (1 * 256 * 256 * 256)
#             is 4 + 768 + 13,1072 + 16,777,216 = 16,909,060
#
# REGISTRY  : apcnic, arin, lacnic, ripencc and afrinic
#             Also included as of April 22, 2005 are the IANA IETF Reserved
#             address numbers. These are important since any source claiming
#             to be from one of these IPs must be spoofed.
#
# ASSIGNED  : The date this IP or block was assigned. (In Epoch seconds)
#             NOTE: Where the allocation or assignment has been transferred from
#                   one registry to another, the date represents the date of first
#                   assignment or allocation as received in from the original RIR.
#                   It is noted that where records do not show a date of first
#                   assignment, the date is given as "0".
#
# CTRY      : 2 character international country code
#             NOTE: ISO 3166 2-letter code of the organisation to which the
#             allocation or assignment was made, and the enumerated variances of:
#                  AP - non-specific Asia-Pacific location
#                  CS - Serbia and Montenegro
#                  YU - Serbia and Montenegro (Formally Yugoslavia) (Being phased out)
#                  EU - non-specific European Union location
#                  FX - France, Metropolitan
#                  PS - Palestinian Territory, Occupied
#                  UK - United Kingdom (standard says GB)
#                * ZZ - IETF RESERVED address space.
#
# NOTE: Although CS is not an ISO-3166 code, it appears to be a colloquial term
# used by registries and appears in the RIR (Regional Internet registry) database.
#
#             These values are not defined in ISO 3166 but are widely used.
#           * IANA Reserved Address space
#
# CNTRY     : Country Abbreviation. Usually 3 Character representation
#
# COUNTRY   : Country Name. Full Country Name.
#
*/
export Layout_IPToCountry := record
unsigned4 IP_From;
unsigned4 IP_To;
string Registry;
string Assigned;
string2 ctry;
string5 cntry;
string country;
end;

r0 := RECORD
  STRING l;
	END;

// ECL has built in very elegant ways to deal with CSV, quotes and headers
// However due to the variable (and wackly) nature of this file it was deemed better to do things the hard way
f1 := dataset('~.::IPToCountry',r0,CSV(SEPARATOR(''),TERMINATOR('\n')))(l[1]<>'#');

NQ(STRING s) := S[2..LENGTH(TRIM(S))-1];
Layout_IPToCountry Into(f1 le) := TRANSFORM
  SET OF STRING ft := Std.Str.SplitWords(le.l,',');
	SELF.IP_From := (UNSIGNED4)NQ(ft[1]);
	SELF.IP_To := (UNSIGNED4)NQ(ft[2]);
	SELF.Registry := NQ(ft[3]);
	SELF.Assigned := NQ(ft[4]);
	SELF.ctry := NQ(ft[5]);
	SELF.cntry := NQ(ft[6]);
	SELF.country := NQ(ft[7]);
  END;

f0 := PROJECT(f1,Into(LEFT));

fs := SORT(f0,ip_from);

b1(unsigned s) := (unsigned)s DIV (256*256*256);

layout_iptocountry rlp(fs le,fs ri) := TRANSFORM
  SELF.ip_to := ri.ip_to;
  SELF := le;
  end;

f := ROLLUP(fs,b1(left.ip_from)=b1(right.ip_from) and left.ctry=right.ctry and (unsigned)left.ip_to=(unsigned)right.ip_from-1,rlp(left,right));

r := record
  f;
  unsigned1 low_ip1 := b1(f.ip_from);
  unsigned1 low_ip2 := ((unsigned)f.ip_from DIV (256*256)) % 256;
  unsigned1 low_ip3 := ((unsigned)f.ip_from DIV (256)) % 256;
  unsigned1 low_ip4 := ((unsigned)f.ip_from) % 256;
  unsigned1 high_ip1 := b1(f.ip_to);
  unsigned1 high_ip2 := ((unsigned)f.ip_to DIV (256*256)) % 256;
  unsigned1 high_ip3 := ((unsigned)f.ip_to DIV (256)) % 256;
  unsigned1 high_ip4 := ((unsigned)f.ip_to) % 256;
  end;

export Txt := table(f,r);

// Infile - the file needing the country
// Infield - the IP field (as an integer)
// tfield - the field the COUNTRY is to be placed in
// ofile - the output file
export MAC_AppendCountry(infile,infield,tfield,ofile) := MACRO
	IMPORT WLAM.WebLogs;
	#uniquename(r)
	%r% := record
	  infile.infield;
		infile.tfield;
	end;
	#uniquename(ips)
	%ips% := dedup( table( infile, %r% ), infield, all );
	#uniquename(lf)
	%lf% := WebLogs.IPToCountry.Txt;
	#uniquename(lf_easy)
	%lf_easy% := %lf%(ip_from % (256*256*256) = 0,ip_to % (256*256*256) = 256*256*256-1);
	#uniquename(lf_easy_set)
	%lf_easy_set% := set(%lf_easy%,ip_from div (256*256*256));
	
	#uniquename(t)
%r%	%t%(%ips% le,%lf% ri) := transform
  self.tfield := ri.cntry;
  self := le;
  end;
  #uniquename(ofile0)
%ofile0% := join(%ips%(infield div (256*256*256) NOT IN %lf_easy_set%),%lf%,left.infield div (256*256*256) = right.ip_from div (256*256*256) and
                          left.infield >= right.ip_from and
														left.infield <= right.ip_to,%t%(left,right),left outer,lookup);
  #uniquename(ofile1)

%ofile1% := join(%ips%(infield div (256*256*256) IN %lf_easy_set%),%lf_easy%,left.infield div (256*256*256) = right.ip_from div (256*256*256),%t%(left,right),left outer,lookup);

#uniquename(prejoin)
%prejoin% := %ofile0%+%ofile1%;

#uniquename(add_back)
typeof(infile) %add_back%(infile le, %prejoin% ri) := transform
  self.tfield := ri.tfield;
  self := le;
  end;
	
ofile := join(infile,%prejoin%,left.infield=right.infield,%add_back%(left,right),lookup);	

  ENDMACRO;
	END;