# the parser is line-based.  Lf helps (un)getting the next line.
# the parser raises a string error on parse error.
# Lf then provides the line number for the error message.

implement Nopointlib;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
	Display, Image, Rect, Point: import draw;
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "lists.m";
	lists: Lists;
include "freetype.m";
include "nopoint.m";


display: ref Display;

init(disp: ref Display)
{
	sys = load Sys Sys->PATH;
	draw = load Draw Draw->PATH;
	bufio = load Bufio Bufio->PATH;
	bufio->open("/dev/null", Bufio->OREAD);
	str = load String String->PATH;
	lists = load Lists Lists->PATH;
	display = disp;
}


Lf: adt {
	b:	ref Iobuf;
	l:	string;
	havel:	int;
	lineno:	int;

	new:	fn(b: ref Iobuf): ref Lf;
	next:	fn(lf: self ref Lf): (int, string);
	unget:	fn(lf: self ref Lf, l: string);
};

Lf.new(b: ref Iobuf): ref Lf
{
	return ref Lf (b, nil, 0, 0);
}

Lf.next(lf: self ref Lf): (int, string)
{
	if(lf.havel) {
		l := lf.l;
		lf.l = nil;
		lf.havel = 0;
		lf.lineno++;
		return (0, l);
	}
	l := lf.b.gets('\n');
	if(l == nil)
		return (1, nil);
	if(l[len l-1] == '\n')
		l = l[:len l-1];
	lf.lineno++;
	return (0, l);
}

Lf.unget(lf: self ref Lf, l: string)
{
	if(lf.havel)
		raise "double unget";
	lf.l = l;
	lf.havel = 1;
	lf.lineno--;
}


error(s: string)
{
	raise "parse:"+s;
}

colorstr(s: string): ref Image
{
	case s {
	"white" =>	return display.white;
	"black" =>	return display.black;
	"grey" =>	return display.color(Draw->Grey);
	"red" =>	return display.color(Draw->Red);
	"blue" =>	return display.color(Draw->Blue);
	"green" =>	return display.color(Draw->Green);
	"cyan" =>	return display.color(Draw->Cyan);
	"magenta" =>	return display.color(Draw->Magenta);
	"yellow" =>	return display.color(Draw->Yellow);
	"darkred" =>	return display.color(int 16r880000ff);
	"darkblue" =>	return display.color(int 16r0000bbff);
	"darkgreen" =>	return display.color(int 16r007700ff);
	}
	(clrv, rem) := str->tobig(s, 16);
	if(rem != nil)
		error(sprint("bad color %#q", s));
	return display.color(int clrv);
}

wordize(s: string): ref Text
{
	f := -1;
	clr: ref Image;
	st: list of (int, ref Image);  # option stack
	l: list of ref Word;
	i := 0;
More:
	for(;;) {
		if(i >= len s)
			break More;
		w := "";
	Word:
		for(;;) {
			if(i >= len s)
				break Word;
			case c := s[i] {
			' ' =>
				end := i+len str->take(s[i:], " \t");
				l = ref Word (f, clr, 1, s[i:end])::ref Word (f, clr, 0, w)::l;
				i = end;
				continue More;
			'{' =>
				i++;
				if(i >= len s)
					error("{ at end");
				if(s[i] == '{' || s[i] == '}') {
					w[len w] = s[i];
					i++;
					continue Word;
				}
				if(i+1 >= len s)
					error("{ at end");
				st = (f, clr)::st;
				if(s[i] == '#') {
					n := len str->take(s[i:], "^ ");
					v := s[i+1:i+n];
					clr = colorstr(v);
					i += n+1;
				} else {
					case s[i:i+2] {
					"r " =>	f = Regular;
					"b " =>	f = Bold;
					"i " =>	f = Italic;
					"m " =>	f = Monospace;
					* =>
						error(sprint("unrecognized inline style %#q", s[i:i+2]));
					}
					i += 2;
				}

			'}' =>
				i++;
				if(st == nil)
					error("} without matching {");
				if(w != nil)
					l = ref Word (f, clr, 0, w)::l;
				(f, clr) = hd st;
				st = tl st;
				continue More;
			* =>
				w[len w] = c;
				i++;
			}
		}
		if(w != nil)
			l = ref Word (f, clr, 0, w)::l;
	}
	if(st != nil)
		error(sprint("unbalanced }'s"));
	return ref Text(lists->reverse(l));
}

isblank(l: string): int
{
	return str->drop(l, " \t\r\n") == nil;
}

iscomment(l: string): int
{
	return str->prefix(".#", l);
}

iscmd(l: string): int
{
	return str->prefix(".", l) && !str->prefix("..", l);
}

cmd(l: string): (string, string)
{
	(c, arg) := str->splitstrl(l[1:], " ");
	if(arg != nil)
		arg = arg[1:];
	return (c, arg);
}

text(l: string): string
{
	if(str->prefix("..", l))
		return l[1:];
	return l;
}

islistitem(l: string): int
{
	l = str->drop(l, " \t");
	return l != nil && str->in(l[0], "*•-");
}

getitem(l: string): ref (int, ref Text)
{
	depth := len str->take(l, " \t");
	t := wordize(str->drop(l, " \t*•-"));
	return ref (depth, t);
}

needslide(l: string, sl: ref Slide)
{
	if(sl == nil)
		error(sprint("command %q, but no slide opened yet", l));
}

parsereal(s, op: string): real
{
	ss := s;
	if(str->prefix(".", s))
		ss = "0"+s;
	(r, rem) := str->toreal(ss, 10);
	if(rem != nil)
		error(sprint("bad real argument %#q for command %#q", s, op));
	return r;
}

slidecmd(lf: ref Lf, sl: ref Slide, l: string, st, style, defaultstyle, titlestyle: ref Style, basepath: string): (ref Elem, ref Style)
{
	e: ref Elem;
	forslide := st != defaultstyle && st != titlestyle;
	(op, arg) := cmd(l);
	case op {
	"version" =>
		if(arg != "0")
			error(sprint("unsupported version %#q", arg));
	"title" =>
		needslide(l, sl);
		sl.title = wordize(arg);
		sl.notitle = 0;
	"notitle" =>
		needslide(l, sl);
		sl.title = nil;
		sl.notitle = 1;
	"image" =>
		needslide(l, sl);
		t := sys->tokenize(arg, " \t").t1;
		if(len t != 2 && len t != 3)
			error(sprint("bad .image"));
		path := hd t;
		if(!str->prefix("/", path) && !str->prefix("#", path))
			path = basepath+path;
		t = tl t;
		height := width := real hd t;
		t = tl t;
		keepratio := 1;
		if(t != nil) {
			height = real hd t;
			keepratio = 0;
		}
		fd := sys->open(path, Sys->OREAD);
		if(fd == nil)
			error(sprint("open %q: %r", path));
		e = ref Elem.Image (fd, path, width, height, keepratio, nil);
	"table" =>
		needslide(l, sl);
		widths: list of ref (int, real);
		aligns: list of ref (int, int);
		rows: list of array of ref Text;
		ncols := 0;
	Line:
		for(;;) {
			eof: int;
			(eof, l) = lf.next();
			if(eof)
				break Line;
			if(iscomment(l))
				continue;
			if(!iscmd(l)) {
				lf.unget(l);
				break Line;
			}
			(op, arg) = cmd(l);
			case op {
			"colalign" =>
				t := str->unquoted(arg);
				if(len t != 1 && len t != 2)
					error(sprint("bad arguments for .colalign %#q", arg));
				align := Left;
				case hd t {
				"left" =>	align = Left;
				"center" =>	align = Center;
				"right" =>	align = Right;
				* =>	error(sprint("bad align for .colalign %#q", hd t));
				}
				pos := -1;
				if(len t == 2)
					pos = int hd tl t;
				aligns = ref (pos, align)::aligns;
			"colwidth" =>
				t := str->unquoted(arg);
				if(len t != 1 && len t != 2)
					error(sprint("bad arguments for .colwidth %#q", arg));
				r := parsereal(hd t, "colwidth scale");
				pos := -1;
				if(len t == 2)
					pos = int hd tl t;
				widths = ref (pos, r)::widths;
			"row" =>
				row: list of ref Text;
				for(t := str->unquoted(arg); t != nil; t = tl t)
					row = wordize(hd t)::row;
				ncols = max(ncols, len row);
				rows = l2a(lists->reverse(row))::rows;
			}
		}

		pad := 1.0/30.0;
		nrows := l2a(lists->reverse(rows));
		nwidths := array[ncols] of {* => (1.0-pad*real ncols)/real ncols};
		for(t := lists->reverse(widths); t != nil; t = tl t) {
			(pos, r) := *hd t;
			if(pos == -1)
				nwidths = array[ncols] of {* => r};
			else if(pos < len nwidths)
				nwidths[pos] = r;
		}
		naligns := array[ncols] of {* => Left};
		for(u := lists->reverse(aligns); u != nil; u = tl u) {
			(pos, align) := *hd u;
			if(pos == -1)
				naligns = array[ncols] of {* => align};
			else if(pos < len naligns)
				naligns[pos] = align;
		}
		e = ref Elem.Table (nwidths, naligns, nrows);

	"vspace" =>
		needslide(l, sl);
		r := 1.0;
		if(arg != nil)
			r = parsereal(arg, op);
		e = ref Elem.Vspace (r);
	"color" =>
		c := defaultstyle.fgc;
		if(arg != nil)
			c = colorstr(arg);
		st.fgc = c;
		if(forslide)
			e = ref Elem.Color (c);
	"bgcolor" =>
		c := defaultstyle.bgc;
		if(arg != nil)
			c = colorstr(arg);
		st.bgc = c;
		if(forslide) {
			needslide(l, sl);
			sl.style.bgc = c;
			e = ref Elem.Bgcolor (c);
		}
	"font" =>
		ft: int;
		case arg {
		"regular" =>	ft = Regular;
		"bold" =>	ft = Bold;
		"italic" =>	ft = Italic;
		"monospace" =>	ft = Monospace;
		"" =>		ft = defaultstyle.ft;
		* =>	error(sprint("bad default font %#q", arg));
		}
		st.ft = ft;
		if(forslide)
			e = ref Elem.Font (ft);
	"fontsize" =>
		r := defaultstyle.ftsize;
		if(arg != nil)
			r = parsereal(arg, op);
		st.ftsize = r;
		if(forslide)
			e = ref Elem.Fontsize (r);
	"align" =>
		a: int;
		case arg {
		"left" =>	a = Left;
		"center" =>	a = Center;
		"right" =>	a = Right;
		"" =>		a = defaultstyle.align;
		* =>	error(sprint("bad align %#q", arg));
		}
		st.align = a;
		if(forslide)
			e = ref Elem.Align (a);
	"valign" =>
		a: int;
		case arg {
		"top" =>	a = Top;
		"middle" =>	a = Middle;
		"bottom" =>	a = Bottom;
		"" =>		a = defaultstyle.valign;
		* =>	error(sprint("bad valign %#q", arg));
		}
		st.valign = a;
		if(forslide) {
			sl.style.valign = a;
			e = ref Elem.Valign (a);
		}
	"indent" =>
		st.indent = r := parsereal(arg, op);
		if(forslide)
			e = ref Elem.Indent (r);
	"style" =>
		case arg {
		"default" =>	st = defaultstyle;
		"title" =>	st = titlestyle;
		"" =>		st = style;
		* =>	error(sprint("bad style %#q", arg));
		}
	* =>
		error(sprint("bad command %#q", op));
	}
	if(e != nil)
		needslide(l, sl);
	return (e, st);
}

slidetext(lf: ref Lf, l: string): ref Elem
{
	if(!islistitem(l))
		return ref Elem.Text (wordize(l));

	el := ref Elem.List;
	el.l = getitem(l)::el.l;
Item:
	for(;;) {
		eof: int;
		(eof, l) = lf.next();
		if(eof || !islistitem(l)) {
			lf.unget(l);
			break Item;
		}
		el.l = getitem(l)::el.l;
	}
	el.l = lists->reverse(el.l);
	return el;
}

readslide(lf: ref Lf, st, style, defaultstyle, titlestyle: ref Style, basepath: string): (ref Slide, ref Style)
{
	sl: ref Slide;
	vspace := 0;
Slide:
	for(;;) {
		(eof, l) := lf.next();
		if(eof)
			break Slide;
		if(iscomment(l))
			continue;
		if(isblank(l)) {
			vspace = 1;
			continue;
		}
		if(iscmd(l) && cmd(l).t0 == "slide") {
			if(sl == nil) {
				sl = ref Slide (-1, ref *style, titlestyle, nil, 0, nil);
				continue Slide;
			} else {
				lf.unget(l);
				break Slide;
			}
		}

		e: ref Elem;
		if(iscmd(l))
			(e, st) = slidecmd(lf, sl, l, st, style, defaultstyle, titlestyle, basepath);
		else if(sl == nil)
			error("text but no slide opened yet");
		else
			e = slidetext(lf, text(l));

		if(e != nil) {
			if(vspace && sl.e != nil)
				sl.e = ref Elem.Vspace (0.75)::sl.e;
			sl.e = e::sl.e;
		}
		vspace = 0;
	}
	if(sl != nil)
		sl.e = lists->reverse(sl.e);
	return (sl, st);
}

parse(path: string, fd: ref Sys->FD): (array of ref Slide, string)
{
	basepath := str->splitstrr(sys->fd2path(fd), "/").t0;
	b := bufio->fopen(fd, Bufio->OREAD);
	lf := Lf.new(b);

	style := ref Style (display.black, display.white, Regular, 1.0, Left, Middle, nil, 0, 0.0, 0);
	defaultstyle := ref *style;
	titlestyle := ref Style (display.black, display.white, Monospace, 1.2, Right, Top, nil, 0, 0.0, 0);
	st := style;  # style currently selected with .style
	l: list of ref Slide;
	index := 0;
	{
		for(;;) {
			sl: ref Slide;
			(sl, st) = readslide(lf, st, style, defaultstyle, titlestyle, basepath);
			if(sl == nil)
				break;
			sl.index = index++;
			l = sl::l;
		}
	} exception e {
	"parse:*" =>
		return (nil, sprint("%q:%d: %s", path, lf.lineno, e[len "parse:":]));
	}
	return (l2a(lists->reverse(l)), nil);
}

l2a[T](l: list of T): array of T
{
	a := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		a[i++] = hd l;
	return a;
}

max(a, b: int): int
{
	if(a >= b)
		return a;
	return b;
}
