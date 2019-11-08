Lyricutils: module {
	PATH:	con "/dis/lib/lyricd/lyricutils.dis";

	BY_URL, BY_TITLE, BY_ARTIST: con (1<<iota);

	Lyric: adt {
		site, url, text: string;
		score:	int;

		mk:	fn(s, u, t: string, score: int): ref Lyric;
	};

	Link: adt {
		artist, title, url, site: string;
		score:	int;

		mk:	fn(a, t, u, s: string, score: int): ref Link;
		cmp:	fn(a, b: ref Link): int;
	};

	init:	fn();

	httpget:	fn(urlstr, enc: string): (string, string);
	conv:		fn(enc: string, a: array of byte): string;
	find:		fn(rstr, body: string): array of string;
	findall:	fn(rstr, body: string): array of array of string;
	htmlstrip:	fn(s: string): string;
	urlallow:	fn(url: string, urls: array of string): int;
	sanitize:	fn(s: string): string;
	htmlfmt:	fn(s: string): string;
	l2a:		fn[T](l: list of T): array of T;
	a2l:		fn[T](a: array of T): list of T;
	rev:		fn[T](l: list of T): list of T;
	join:		fn(l: list of string, sep: string): string;
	hasterms:	fn(s: string, l: list of string): int;
	rate:		fn(links: array of ref Link, how: int, title, artist: list of string): list of ref Link;
	googlesearch:	fn(domain: string, title, artist: list of string): array of (string, string);
	score:		fn(s: string, words: list of string): int;

	split:		fn(s, splitstr: string): list of string;
	splitcl:	fn(s, splitcl: string): list of string;
	append:		fn[T](l1, l2: list of T): list of T;
	replace:	fn(s, src, dst: string): string;
	infix:		fn(instr, s: string): int;
};


Site: module {
	name: string;

	init:	fn();
	search:	fn(title, artist: list of string): (list of ref Lyricutils->Link, string);
	get:	fn(url: string): (ref Lyricutils->Lyric, string);
};
