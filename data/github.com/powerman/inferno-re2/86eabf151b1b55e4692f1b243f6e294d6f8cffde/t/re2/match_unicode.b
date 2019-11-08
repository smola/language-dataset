implement T;

include "opt/powerman/tap/module/t.m";
include "re2.m";
	re2: Re2;
	re, RE: import re2;

test()
{
	re2 = load Re2 Re2->PATH;
	if(re2 == nil)
		bail_out("fail to load Re2");
	plan(47);

	match: array of string;

	match = re2->match("аб", re("(а)(б)"));
	ok(match != nil, "match != nil");
	eq(match[0], "а", "match[0] == а");
	eq(match[1], "б", "match[1] == б");

	match = re2->match("abc абв ABC 123456", re("(b).*(б).*(B)"));
	ok(match != nil, "match != nil");
	eq_int(len match, 3, "len match == 3");
	eq(match[0], "b", "match[0] == b");
	eq(match[1], "б", "match[1] == б");
	eq(match[2], "B", "match[2] == B");

	match = re2->match("абракадабра", re("((абр)()а(к(ад)а(бра)))"));
	ok(match != nil, "match != nil");
	eq_int(len match, 6, "len match == 6");
	eq(match[0], "абракадабра", "match[0] == абракадабра");
	eq(match[1], "абр",	"match[1] == абр");
	eq(match[2], "",	"match[2] == ");
	eq(match[3], "кадабра", "match[3] == кадабра");
	eq(match[4], "ад",	"match[4] == ад");
	eq(match[5], "бра",	"match[5] == бра");

	match = re2->match("abrakadabra", re("((abr)(a)(k(ad)a(bra)))"));
	ok(match != nil, "match != nil");
	eq_int(len match, 6, "len match == 6");
	eq(match[0], "abrakadabra", "match[0] == abrakadabra");
	eq(match[1], "abr",	"match[1] == abr");
	eq(match[2], "a",	"match[2] == a");
	eq(match[3], "kadabra", "match[3] == kadabra");
	eq(match[4], "ad",	"match[4] == ad");
	eq(match[5], "bra",	"match[5] == bra");

	match = re2->match("абракадабра", re("(X)?(X)?(X?)((абр)(а)(X)?(X)?(к(ад)а(бра)))(X)?(X)?"));
	ok(match != nil, "match != nil");
	eq_int(len match, 6+7, "len match == 6+7");
	eq(match[0], nil,	"match[0] == nil");
	eq(match[1], nil,	"match[1] == nil");
	eq(match[2], nil,	"match[2] == nil");
	eq(match[3], "абракадабра", "match[3] == абракадабра");
	eq(match[4], "абр",	"match[4] == абр");
	eq(match[5], "а",	"match[5] == а");
	eq(match[6], nil,	"match[6] == nil");
	eq(match[7], nil,	"match[7] == nil");
	eq(match[8], "кадабра", "match[8] == кадабра");
	eq(match[9], "ад",	"match[9] == ад");
	eq(match[10], "бра",	"match[10] == бра");
	eq(match[11], nil,	"match[11] == nil");
	eq(match[12], nil,	"match[12] == nil");

	match = re2->match("абrаkадабrа", re("((абr)(а)(k(ад)а(бrа)))"));
	ok(match != nil, "match != nil");
	eq_int(len match, 6, "len match == 6");
	eq(match[0], "абrаkадабrа", "match[0] == абrаkадабrа");
	eq(match[1], "абr",	"match[1] == абr");
	eq(match[2], "а",	"match[2] == а");
	eq(match[3], "kадабrа", "match[3] == kадабrа");
	eq(match[4], "ад",	"match[4] == ад");
	eq(match[5], "бrа",	"match[5] == бrа");
}

