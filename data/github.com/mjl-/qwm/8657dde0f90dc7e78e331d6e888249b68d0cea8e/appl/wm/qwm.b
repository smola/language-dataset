implement Qwm;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
	Wmcontext, Context, Display, Image, Screen, Point, Pointer, Rect, Font: import draw;
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "devpointer.m";
	devpointer: Devpointer;
include "keyboard.m";
	kb: Keyboard;
include "sh.m";
	sh: Sh;
include "tables.m";
	tables: Tables;
	Strhash: import tables;

Qwm: module {
	init:	fn(nil: ref Context, args: list of string);
};

program := "wm/run";
deftag: con "Hide wm/run";

Tagfgcolor: con Draw->White;
Tagbgcolor: con Draw->Greyblue;
Tagselbgcolor: con Draw->Palegreyblue;
Boxcolor: con Draw->Palegreyblue;
Colbgcolor: con Draw->Grey;

Modchar: con 'q';
Mod: con -16r60;	# against lower case only

Winmin: con 16;	# desired min height of window
Colmin: con 50;	# desired min width of column

dflag := 0;
fontname: con "qwm./fonts/pelm/unicode.8.font";  # qwm.-prefix is for fontsrv

Mstack, Msingle: con iota;	# Col.mode
modes := array[] of {"Stack", "Single"};

Skbd, Sptr, Scontrol: con 1<<iota;	# Win.started
start := array[] of {"kbd", "ptr", "control"};
Win: adt {
	index:	int;
	colindex:	int;
	tag:	string;		# given on wmctl, requested through Draw->Context
	fid:	int;		# of wmctl
	wm:	ref Wmcontext;
	nbwm:	ref Wmcontext;	# non-blocking wm, has different kbd,ptr,ctl,images chans that don't block
	img:	ref Image;	# nil initially
	started:	int;
	pids:	list of int;	# buffer pids and such
	wantr:	Rect;		# desired rect for img.  for Msingle cols, wins have col.r
	haver:	Rect;		# rect of img.  unlike img.r, which always starts at 0.0
	resizing:	int;	# whether !size msg sent or will be sent soon
	fixedorigin:	int;
	tagtab:		ref Strhash[ref (ref Image, Rect)];  # non-"." windows/tags
	dirty:		int;	# whether needs to be redrawn
	ptroff:		Point;	# to add to pointers for window
};
wingen := 1;	# tag is "w"+wingen

Col: adt {
	index:	int;			# index in cols[]
	visindex:	int;		# index in vis[] or <0
	mode:	int;
	r:	Rect;			# excluding tagr
	fr:	Rect;			# including tagr
	win:	ref Win;		# current win
	wins:	array of ref Win;	# all wins in col
	tagr:	Rect;			# desired tagr
	tagwins:	array of Rect;
	moder:	Rect;
	s:	string;			# editable text
	sr:	Rect;
	sp:	Pos;
};

Pos: adt {
	i0:	int;			# index of char after cursor
	i1:	int;			# start of selection.  i0 when no selection.
};

Cfg: adt {
	c:	array of ref Col;
	w:	array of int;
	col:	ref Col;
	tag:	string;
};

cols,				# all columns, non-visible have width 0
vis: array of ref Col;		# visible cols only, in order
col: ref Col;			# col with focus (never nil).  col.win may be nil.
tagimg: ref Image;		# double buffer for tag bars
ptrprev: ref Pointer;
ptrdown: ref Pointer;		# ptr of last change from no buttons to any button.  nil if no button down
ptrwarp: Point;			# location of ptr before last warp
otherwin: string;		# tag of "other win", either previous with focus, or last unhidden
othercfg: ref Cfg;		# previous column configuration

fg, bg, selbg, boxc, colbg: ref Image;

# currently moving window (dragging) with pointer
moving: ref (ref Pointer, ref Win, chan of (int, string), array of byte, Point);

wctlc: chan of (ref Win, string);	# demux wmcontext wctl chans
kbdc: chan of int;

drawctxt: ref Context;
ptrfd: ref Sys->FD;
B1, B2, B3: con 1<<iota;
cursorfd: ref Sys->FD;

tagfont: ref Font;
fd2: ref Sys->FD;
zeropt: Point;
zerorect: Rect;

init(ctxt: ref Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	draw = load Draw Draw->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	devpointer = load Devpointer Devpointer->PATH;
	devpointer->init();
	sh = load Sh Sh->PATH;
	tables = load Tables Tables->PATH;

	arg->init(args);
	arg->setusage(arg->progname()+" [-d] [profile]");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	dflag++;
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args > 1)
		arg->usage();
	if(args != nil)
		profile := hd args;

	if(ctxt != nil)
		fail("already have draw context");

	sys->bind("#s", "/chan", Sys->MBEFORE);
	fiorect := sys->file2chan("/chan", "wmrect");
	if(fiorect == nil)
		fail(sprint("/chan/wmrect: %r"));
	fioctl := sys->file2chan("/chan", "wmctl");
	if(fioctl == nil)
		fail(sprint("/chan/wmctl: %r"));

	disp := Display.allocate(nil);
	if(disp == nil)
		fail(sprint("disp.allocate: %r"));
	scr := Screen.allocate(disp.image, disp.color(draw->Nofill), 0);
	if(scr == nil)
		fail(sprint("screen.allocate: %r"));
	colbg = disp.color(Colbgcolor);
	scr.image.draw(scr.image.r, colbg, nil, zeropt);

	wmc := chan of (string, chan of (string, ref Wmcontext));
	drawctxt = ref Context(disp, scr, wmc);

	if(profile != nil)
		run(list of {"sh", "-n", profile});

	cols = array[9] of ref Col;
	tagfont = Font.open(disp, fontname);
	if(tagfont == nil)
		tagfont = Font.open(disp, "*default*");
	tagsr := scr.image.r;
	tagsr.max.y = tagsr.min.y+tagfont.height+2;
	tagimg = disp.newimage(tagsr, disp.image.chans, 0, draw->Nofill);
	fg = drawctxt.display.color(Tagfgcolor);
	bg = drawctxt.display.color(Tagbgcolor);
	selbg = drawctxt.display.color(Tagselbgcolor);
	boxc = drawctxt.display.color(Boxcolor);

	for(i := 0; i < len cols; i++) {
		fr := r := scr.image.r;
		tagr := tagsr;
		if(i != 0)
			fr.max.x = r.max.x = tagr.max.x = r.min.x;
		r.min.y = tagr.max.y;
		cols[i] = ref Col(i, -1, Mstack, r, fr, nil, nil, tagr, nil, zerorect, deftag, zerorect, Pos(len deftag, len deftag));
	}
	vis = array[] of {cols[0], cols[1]};
	cols[0].visindex = 0;
	cols[1].visindex = 1;
	dx := drawctxt.screen.image.r.dx();
	dx0 := 3*dx/5;
	setviswidths(array[] of {dx0, dx-dx0});
	col = cols[1];
	drawtags();

	cmdc := chan of int;
	kbdc = chan of int;
	ptrc := chan of ref Pointer;
	wctlc = chan of (ref Win, string);

	spawn kbd(cmdc, pidc := chan of (int, string));
	(nil, kerr) := <-pidc;
	if(kerr != nil)
		fail("kbd: "+kerr);

	ptrfd = sys->open("/dev/pointer", Sys->ORDWR);
	if(ptrfd == nil)
		fail(sprint("open /dev/pointer: %r"));
	spawn ptr(ptrc, ppidc := chan of int);
	<-ppidc;

	cursorfd = sys->open("/dev/cursor", sys->OWRITE);
	if(cursorfd == nil)
		warn(sprint("open /dev/cursor: %r"));

	ptrprev = ref Pointer;
	ptrprev.xy = ptrboxpt(col);
	ptrset(ptrprev.xy);

	for(;;)
	alt {
	(w, s) := <-wctlc =>
		say(sprint("wctl from %q: %q!?", w.tag, s));
		# not a single inferno program/library seems to use this.
		# they all go through /chan/wmctl.

	x := <-cmdc =>
		key(x);
		drawtags();

	x := <-kbdc =>
		if(col.win == nil || col.tagr.contains(ptrprev.xy)) {
			tagkey(col, x);
		} else if(col.win != nil && col.win.started&Skbd)
			alt {
			col.win.nbwm.kbd <-= x =>
				{}
			* =>
				{} # key lost, no big deal
			}
	p := <-ptrc =>
		mouse(p);
		ptrprev = p;

	(tag, rc) := <-drawctxt.wm =>
		say("wmc: "+tag);
		(nil, w) := winfindtag(tag);
		if(w == nil)
			err := sprint("no window %#q", tag);
		else
			# give clone, at least necessary that we don't hold on to connfd,
			# the fileio to our wmctl.
			wm := ref *w.wm;
		spawn sendwm(rc, (err, wm));

	(off, count, nil, rc) := <-fiorect.read =>
		if(rc != nil) {
			buf := array of byte r2s(drawctxt.screen.image.r);
			s := min(len buf, off);
			e := min(len buf, off+count);
			rc <-= (buf[s:e], nil);
		}

	(nil, buf, nil, wc) := <-fiorect.write =>
		if(wc != nil) {
			say(sprint("fiorect.write buf %q", string buf));
			wc <-= (-1, "no writes");
		}

	(off, count, fid, rc) := <-fioctl.read =>
		if(rc != nil) {
			w := winfindfid(fid);
			if(w == nil) {
				w = winmk(fid);
				drawtags();
			}
			buf := array of byte w.tag;
			s := min(len buf, off);
			e := min(len buf, off+count);
			say(sprint("fioctl.read, returning %q", string buf[s:e]));
			rc <-= (buf[s:e], nil);
		}

	(nil, buf, fid, wc) := <-fioctl.write =>
		if(wc == nil) {
			w := winfindfid(fid);
			windrop(w);
		} else {
			w := winfindfid(fid);
			if(w == nil) {
				s := string buf;
				err: string;
				(cmd, rem) := str->splitstrl(s, " ");
				rem = str->drop(rem, " ");
				case cmd {
				"program" =>	program = rem;
				"tagfg" =>	(fg, err) = parsecolor(rem, fg);
				"tagbg" =>	(bg, err) = parsecolor(rem, bg);
				"tagselbg" =>	(selbg, err) = parsecolor(rem, selbg);
				"tagbox" =>	(boxc, err) = parsecolor(rem, boxc);
				"colbg" =>
					(colbg, err) = parsecolor(rem, colbg);
					if(err == nil)
						for(j := 0; j < len vis; j++)
							if(len vis[j].wins == 0)
								drawctxt.screen.image.draw(vis[j].r, colbg, nil, zeropt);
				"cols" =>	colsnonempty();
				"showcol" =>
						j := int rem-1;
						if(j >= 0 && j < len cols && cols[j].visindex < 0)
							coltoggle(cols[j], 1);
				* =>		err = "bad request";
				}
				drawtags();
				wc <-= (len buf, err);
			} else
				{
					ctlwrite(w, buf, wc);
				} exception ex {
				"error:*" =>
					ex = ex[len "error:":];
					say(sprint("fio.ctlwrite: %s", ex));
					wc <-= (-1, ex);
				}
		}
		drawtags();
	}
}

parsecolor(s: string, oi: ref Image): (ref Image, string)
{
	(v, rv) := str->toint(s, 16);
	if(rv != nil)
		return (oi, "bad color");
	i := drawctxt.display.color(v);
	if(i == nil)
		return (oi, sprint("new color: %r"));
	return (i, nil);
}

wctlfwd(w: ref Win, pidc: chan of int)
{
	pidc <-= pid();
	for(;;)
		wctlc <-= (w, <-w.wm.wctl);
}


List: adt[T] {
	first,
	last:	ref Link[T];

	empty:	fn(l: self ref List): int;
	take:	fn(l: self ref List): T;
	add:	fn(l: self ref List, e: T);
};

Link: adt[T] {
	e:	T;
	next:	cyclic ref Link[T];
};

List[T].empty(l: self ref List): int
{
	return l.first == nil;
}

List[T].take(l: self ref List): T
{
	e := l.first.e;
	if(l.first == l.last)
		l.first = l.last = nil;
	else
		l.first = l.first.next;
	return e;
}

List[T].add(l: self ref List, e: T)
{
	nl := ref Link[T](e, nil);
	if(l.first == nil)
		l.first = l.last = nl;
	else {
		l.last.next = nl;
		l.last = l.last.next;
	}
}


# buffer T's from 'fc' to 'tc', so we can also send on fc
# when sending to tc would block.
chanbuf[T](fc, tc: chan of T, pidc: chan of int)
{
	pidc <-= pid();
	l := ref List[T];
	e: T;
	bogusc := chan of T;
	cc := bogusc;
	for(;;)
	alt {
	cc <-= e =>
		if(!l.empty())
			e = l.take();
		else {
			e = nil;
			cc = bogusc;
		}
	ne := <-fc =>
		if(e == nil) {
			e = ne;
			cc = tc;
		} else
			l.add(ne);
	}
}


ptr(ptrc: chan of ref Pointer, pidc: chan of int)
{
	pidc <-= pid();
	buf := array[Devpointer->Size] of byte;
	for(;;) {
		n := sys->read(ptrfd, buf, len buf);
		if(n != len buf)
			return warn(sprint("ptr read gave %d", n));
		ptrc <-= devpointer->bytes2ptr(buf);
	}
}

ptrboxpt(c: ref Col): Point
{
	b := colbox(c);
	return b.min.add((b.dx()/2, b.dy()/2));
}

ptrsetbox(c: ref Col)
{
	ptrset(ptrboxpt(c));
}

ptrsettag(c: ref Col)
{
	(nil, i1) := order(c.sp.i0, c.sp.i1);
	x := min(c.r.max.x-1, c.sr.min.x+tagfont.width(c.s[:i1]));
	y := c.sr.min.y+c.sr.dy()/2;
	ptrset(Point(x, y));
}

ptrset(xy: Point)
{
	ptrwarp = ptrprev.xy;
	p := ref Pointer;
	p.xy = xy;
	buf := devpointer->ptr2bytes(p);
	n := sys->write(ptrfd, buf, len buf);
	if(n != len buf)
		warn(sprint("write pointer: %r"));
}

kbd(cmdc: chan of int, pidc: chan of (int, string))
{
	b := bufio->open("/dev/keyboard", Sys->OREAD);
	if(b == nil) {
		pidc <-= (-1, sprint("/dev/keyboard: %r"));
		return;
	}
	pidc <-= (pid(), nil);

	mod := 0;
	for(;;)
	case c := b.getc() {
	bufio->EOF =>
		return warn("eof on /dev/keyboard");
	bufio->ERROR =>
		return warn(sprint("error on /dev/keyboard: %r"));
	* =>
		if(mod) {
			if(c == Modchar)
				kbdc <-= Mod+Modchar;
			else
				cmdc <-= c;
			mod = 0;
		} else if(c == Mod+Modchar)
			mod = 1;
		else
			kbdc <-= c;
	}
}

keysend(v: int)
{
	kbdc <-= v;
}


run(argv: list of string)
{
	sys->pctl(Sys->NEWPGRP, nil);
	sh->run(drawctxt, argv);
}

mouse(p: ref Pointer)
{
	pp := p;
	if(ptrdown != nil)
		pp = ptrdown;

	if(moving != nil) {
		if(p.buttons == 0) {
			ptrmoving(p);
			drawtags();
		}
	} else if(tagimg.r.contains(pp.xy)) {
		ptrtag(p);
	} else {
		if(!col.r.contains(pp.xy) || col.win != nil && !col.win.wantr.contains(pp.xy)) {
			(c, w) := winfindpt(pp.xy);
			if(c != nil) {
				focus(c, w, 0);
				drawtags();
			}
		}
		if(col.win != nil && col.win.started&Sptr) {
			mp := ref *p;
			mp.xy = mp.xy.add(col.win.ptroff);
			col.win.nbwm.ptr <-= mp;
		}
	}
	if(!ptrprev.buttons && p.buttons)
		ptrdown = p;
	else if(!p.buttons)
		ptrdown = nil;
}

ptrmoving(p: ref Pointer)
{
	(optr, w, wc, buf, pt) := *moving;
	moving = nil;
	c := cols[w.colindex];
	(oc, ow) := winfindpt(p.xy);
	if(oc == nil) {
		# nothing
	} else if(abs(p.xy.x-pt.x) < 10 && abs(p.xy.y-pt.y) < 10) {
		case optr.buttons {
		B1 =>	winbigger(c, w, pt, 1, c.r.dy()/6);
		}
	} else if(c != oc) {
		winmovecol(w, c, oc, p.xy.y-tagimg.r.dy());
		ptrensure(c, w);
	} else if(w == ow || (ow != nil && ow.index+1 == w.index)) {
		if(w.index != 0 && c.mode == Mstack) {
			if(w == ow)
				ow = c.wins[w.index-1];
			# move top of win up or down
			dy := p.xy.y-pt.y;
			ow.wantr.max.y += dy;
			w.wantr.min.y += dy;
			resize();
		}
	} else if(ow != nil && c.mode == Mstack) {
		# move past other win
		oh := getheights(c);
		nh: array of int;
		(c.wins, nh) = movepast(c.wins, oh, w.index, ow.index, p.xy.y-tagimg.r.dy());
		renumber(c.wins);
		setheights(c, nh);
		resize();
	}
	err := reshape(w);
	if(err != nil)
		error(err);
	wc <-= (len buf, nil);
	w.nbwm.images <-= w.img;
}

ptrtag(p: ref Pointer)
{
	if(ptrdown == nil) {
		# first buttons down, or just movement
		if(!col.fr.contains(p.xy)) {
			(c, w) := winfindpt(p.xy);
			if(w == nil && len c.wins != 0)
				w = c.wins[0];
			focus(c, w, 0);
			drawtags();
		} else if((i := tagindex(col, p.xy.x)) >= 0) {
			(i0, i1) := order(col.sp.i0, col.sp.i1);
			if(p.buttons == (B1<<8|B1) && i == i0 && i == i1) {
				(col.sp.i0, col.sp.i1) = textexpand(col.s, i);
				drawtag(col);
			} else if(p.buttons == B1 || p.buttons == B2 && (i < i0 || i > i1)) {
				col.sp.i0 = col.sp.i1 = i;
				drawtag(col);
			}
		}
		return;
	}
	if((ptrdown.buttons^p.buttons) == 0) {
		# no buttons changed
		if(ptrdown.buttons & (B1|B2) && (i := tagindex(col, ptrdown.xy.x)) >= 0 && (j := tagindex(col, p.xy.x)) >= 0 && i != j) {
			col.sp.i0 = i;
			col.sp.i1 = j;
			drawtag(col);
		}
		return;
	}
	if(p.buttons & ~ptrdown.buttons) {
		# more buttons pressed, mark as cancelled
		ptrdown.buttons = ~0;
		return;
	}
	if(p.buttons == 0 && ptrdown.buttons == ~0) {
		# buttons up, but this was cancelled
		return;
	}

	say(sprint("release of %#x at %d,%d", ptrdown.buttons, ptrdown.xy.x, ptrdown.xy.y));
	(c, nil) := winfindpt(ptrdown.xy);
	(y, nil) := winfindpt(p.xy);
	if(y == nil) {
		# nothing
	} else if(colbox(c).contains(ptrdown.xy)) {
		if(c == y || c.visindex == y.visindex+1) {
			if(colbox(c).contains(p.xy))
				case ptrdown.buttons {
				B1 =>	colbigger(c, 1);
				B2 =>	colmax(c, 1);
				B3 =>	colsingle(c);
				}
			else {
				colsetleft(c, p.xy.x);
				ptrsetbox(c);
			}
		} else if(ptrdown.buttons == B1) {
			ow := getviswidths();
			(nc, nw) := movepast(vis, ow, c.visindex, y.visindex, p.xy.x);
			visset(nc, nw);
		}
	} else if(c.moder.contains(ptrdown.xy)) {
		case ptrdown.buttons {
		B1 =>	modetoggle();
		}
	} else if((j := rectindex(c.tagwins, ptrdown.xy)) >= 0) {
		w := c.wins[j];
		if(c == y) {
			if(col != c || c.win != w || c.mode == Msingle)
				focus(c, w, 0);
			else
				case ptrdown.buttons {
				B1 =>	winbigger(c, w, zeropt, 0, c.r.dy()/6);
				B2 =>	winmax(c, w);
				B3 =>	winsingle(c, w);
				}
		} else if(ptrdown.buttons == B1) {
			winmovecol(w, c, y, p.xy.y-tagimg.r.dy());
			ptrensure(c, w);
		}
	} else if(p.xy.x >= c.sr.min.x) {
		case ptrdown.buttons {
		B2 =>
			(i0, i1) := order(c.sp.i0, c.sp.i1);
			cmd(c.s, i0, i1);
		}
	}
	drawtags();
}

tagindex(c: ref Col, x: int): int
{
	if(x < c.sr.min.x)
		return -1;
	x -= c.sr.min.x;
	s := c.s;
	n := len s;
	o := 0;
	for(i := 0; i < n; i++) {
		no := o+tagfont.width(s[i:i+1]);
		if(no > x)
			break;
		o = no;
	}
	return i;
}

textexpand(s: string, i: int): (int, int)
{
	oi := i;
	if(i >= len s)
		i = len s-1;
	if(str->in(s[i], " \t\n"))
		return (oi, oi);
	for(b := i; b-1 >= 0 && !str->in(s[b-1], " \t\n"); b--)
		{}
	for(e := i; e < len s && !str->in(s[e], " \t\n"); e++)
		{}
	return (b, e);
}

cmd(s: string, b, e: int)
{
	if(b == e)
		(b, e) = textexpand(s, b);
	s = s[b:e];
	case s {
	"" =>
		{}
	"Hide" =>
		coltoggle(col, 1);
	"Cols" =>
		colsnonempty();
	* =>
		pre: string;
		if(str->prefix(pre="Showcol ", s)) {
			i := int str->drop(s[len pre:], " \t\n")-1;
			if(i >= 0 && i < len cols && cols[i].visindex < 0)
				coltoggle(cols[i], 1);
		} else if(str->prefix(pre="Killwin ", s)) {
			i := int str->drop(s[len pre:], " \t\n");
			if(i >= 0 && i < len col.wins && col.win != nil)
				windrop(col.wins[i]);
		} else if(str->prefix(pre="Program ", s)) {
			program = str->drop(s[len pre:], " \t\n");
		} else
			spawn run(list of {"sh", "-nc", s});
	}
}

Ctl: con -16r60;
tagkey(c: ref Col, x: int)
{
	s := c.s;
	i := c.sp.i0;

	case x {
	kb->Left =>
		if(i > 0)
			c.sp.i0--;
	kb->Right =>
		if(i+1 <= len s)
			c.sp.i0++;
	Ctl+'a' =>
		c.sp.i0 = 0;
	Ctl+'e' =>
		c.sp.i0 = len s;
	* =>
		i1 := c.sp.i1;
		if(i != i1) {
			(i, i1) = order(i, i1);
			s = c.s = c.s[:i]+c.s[i1:];
			c.sp.i0 = i;
		}
	}

	case x {
	kb->Left or
	kb->Right or 
	Ctl+'a' or
	Ctl+'e' =>
		{}
	Ctl+'h' =>
		if(i > 0) {
			c.sp.i0--;
			c.s = s[:i-1]+s[i:];
		}
	Ctl+'u' =>
		c.sp.i0 = 0;
		c.s = "";
	Ctl+'w' =>
		Break: con "^a-zA-Z0-9";
		for(b := i; b-1 >= 0 && str->in(s[b-1], Break); b--)
			{}
		for(; b-1 >= 0 && !str->in(s[b-1], Break); b--)
			{}
		c.sp.i0 = b;
		c.s = s[:b]+s[i:];
	* =>
		c.sp.i0++;
		ns := "";
		ns[len ns] = x;
		c.s = s[:i]+ns+s[i:];
	}
	c.sp.i1 = c.sp.i0;
	drawtag(c);
}

key(x: int)
{
	say(sprint("cmd %c", x));
	if(x < 16r20)
		x -= Mod;
	case x {
	* =>
		# shift 1-9
		ops := array[] of {"!", "@", "#", "$", "%", "^", "&", "*", "("};
		ci := findstr(ops, sprint("%c", x));
		if(ci >= 0)
			coltoggle(cols[ci], 1);
		else
			say(sprint("unknown key %c/%#x", x, x));
	Modchar =>
		cfg := cfgget();
		colswitch();
		cfgset(cfg);
	'0' =>
		colsnonempty();
	'1' to '9' =>
		cfg := cfgget();
		colsingle(cols[x-'1']);
		cfgset(cfg);
		ptrensure(col, col.win);
	'f' =>
		modetoggle();
	'F' =>
		cfg := cfgget();
		colsingle(col);
		cfgset(cfg);
		modesingle();
	'r' =>
		ptrset(ptrwarp);
	'h' or
	'l' =>
		c := coladj(x=='l');
		if(c != nil)
			focus(c, c.win, 1);
	'H' or
	'L' =>
		c := coladj(x=='L');
		if(c != nil && col.win != nil) {
			winmovecol(col.win, col, c, ptrprev.xy.y-tagimg.r.dy());
			ptrensure(col, col.win);
		}
	'K' or
	'J' =>
		w := winadj(x=='J');
		if(col.mode != Mstack || w == nil)
			return;
		h := getheights(col);
		cwi := col.win.index;
		col.win.dirty = 1;
		w.dirty = 1;
		(h[w.index], h[cwi]) = (h[cwi], h[w.index]);
		(col.wins[w.index], col.wins[cwi]) = (col.wins[cwi], col.wins[w.index]);
		renumber(col.wins);
		setheights(col, h);
		resize();
		ptrensure(col, col.win);
	'k' or
	'j' =>
		w := winadj(x=='j');
		if(w != nil)
			focus(col, w, 1);
	'o' =>
		colbigger(col, 0);
	'O' =>
		colmax(col, 0);
	'i' =>
		if(col.mode == Mstack && col.win != nil) {
			winbigger(col, col.win, zeropt, 0, col.r.dy()/4);
			ptrensure(col, col.win);
		}
	'u' =>
		if(col.win != nil)
			winmax(col, col.win);
	'x' =>
		if(col.win != nil)
			winctl(col.win, "exit");
	'X' =>
		if(col.win != nil)
			windrop(col.win);
	'c' =>
		spawn run(list of {"sh", "-nc", program});
	'd' =>
		dflag = !dflag;
	'D' =>
		dump();
	}
}

dump()
{
	for(i := 0; i < len cols; i++) {
		c := cols[i];
		if(col.win == nil)
			s := "nil";
		else
			s = sprint("index %d", col.win.index);
		warn(sprint("col %d, visindex=%d, r %s, fr %s, mode %s, %s", c.index, c.visindex, r2s(c.r), r2s(c.fr), modes[c.mode], s));
		for(j := 0; j < len c.wins; j++) {
			w := c.wins[j];
			r := "nil";
			if(w.img != nil)
				r = r2s(w.haver);
			warn(sprint("\twin, index %d, tag %q, fid %d, resizing %d, fixedorigin %d, wantr %s, haver %s", w.index, w.tag, w.fid, w.resizing, w.fixedorigin, r2s(w.wantr), r));
		}
	}
}

colsnonempty()
{
	cfg := cfgget();
	colnonempty();
	cfgset(cfg);
	ptrensure(col, col.win);
}

winadj(after: int): ref Win
{
	w := col.win;
	if(w == nil)
		return nil;
	i := w.index-1;
	if(after)
		i = w.index+1;
	if(i >= 0 && i < len col.wins)
		return col.wins[i];
	return nil;
}

# return column adjacent to 'col', on right side (after), or left side (!after)
coladj(after: int): ref Col
{
	ni := col.visindex-1;
	if(after)
		ni = col.visindex+1;
	if(ni >= 0 && ni < len vis)
		return vis[ni];
	return nil;
}

sendwm(rc: chan of (string, ref Wmcontext), t: (string, ref Wmcontext))
{
	rc <-= t;
}

send[T](c: chan of T, e: T)
{
	c <-= e;
}

winctl(w: ref Win, s: string)
{
	say(sprint("ctl to tag %s, win.index %d: %s", w.tag, w.index, s));
	w.nbwm.ctl <-= s;
}

winkbd(w: ref Win, v: int)
{
	winctl(w, sprint("haskbdfocus %d", v));
}


# for use in ctlwrite
error(s: string)
{
	raise "error:"+s;
}

narg(cmd: string, want, have: int)
{
	if(want != have)
		raise sprint("error:%#q needs %d args, saw %d", cmd, want, have);
}

ctlwrite(w: ref Win, buf: array of byte, wc: chan of (int, string))
{
	s := string buf;
	say(sprint("ctlwrite %q: %s", w.tag, s));
	a := l2a(str->unquoted(s));
	if(len a == 0)
		error("bad request");
	cmd := a[0];
	a = a[1:];

	doimg := str->prefix("!", cmd);
	rimg: ref Image;

	c := cols[w.colindex];
	case cmd {
	"!reshape" =>
		# !reshape tag reqid minx miny maxx maxy [how] 
		if(len a != 6 && len a != 7)
			error(sprint("reshape needs 6 or 7 params, saw %d", len a));
		tag := a[0];
		if(tag == ".") {
			err := reshape(w);
			if(err != nil)
				error(err);
			rimg = w.img;
			break;
		}
		if(w.img == nil)
			error("reshape of non-\".\" without image");
		r := Rect((int a[2], int a[3]), (int a[4], int a[5]));
		ni := drawctxt.screen.newwindow(r, draw->Refnone, Draw->Nofill);
		if(ni == nil)
			error(sprint("new window: %r"));
		#ni.flush(draw->Flushoff);
		w.tagtab.del(tag);
		w.tagtab.add(tag, ref (ni, r));
		rimg = ni;
	"!move" =>
		# !move tag reqid startx starty
		# dragging window started, ignore reqid, startx & starty are start locations
		narg(cmd, 4, len a);
		pt := Point(int a[2], int a[3]);
		pt = pt.sub(w.ptroff);
		moving = ref (ptrprev, w, wc, buf, pt);
		w.resizing = 1;
		return;
	"!size" =>
		# !size tag reqid startx starty
		# resize window, we sneakily send "!size ..." to wmctxt.ctl, hoping they forward it to tkclient's wmctl, which sends us this !size, so we can give it a new image with the size we want.
		narg(cmd, 4, len a);
		err := reshape(w);
		if(err != nil)
			error(err);
		rimg = w.img;
	"start" =>
		narg(cmd, 1, len a);
		i := findstr(start, a[0]);
		if(i < 0)
			error(sprint("cannot start %q", a[0]));
		bit := 1<<i;
		if(w.started & bit)
			error(sprint("%q already started", a[0]));
		if(bit == Skbd && col.win == w)
			winkbd(w, 1);
		w.started |= bit;
	"key" =>
		# incoming simulated key
		narg(cmd, 1, len a);
		(v, rem) := str->toint(a[0], 10);
		if(rem != nil)
			error(sprint("key not decimal: %#q", a[0]));
		spawn keysend(v);
	"delete" =>
		narg(cmd, 1, len a);
		tag := a[0];
		if(tag == ".")
			break;
		t := w.tagtab.find(tag);
		w.tagtab.del(tag);
		if(t == nil) {
			warn("missing tag?");
			break;
		}
		(nil, r) := *t;

		# force redraws for windows we were over
		for(i := 0; i < len vis; i++) {
			cc := vis[i];
			for(j := 0; j < len cc.wins; j++) {
				ww := cc.wins[j];
				if(!ww.resizing && r.clip(ww.wantr).t1) {
					ww.resizing = 1;
					winctl(ww, "!size . -1 0 0");
				}
			}
		}
	"fixedorigin" =>
		# don't change origin when moving, give new image entirely
		narg(cmd, 0, len a);
		w.fixedorigin = 1;
	"kbdfocus" =>
		if(len a != 0 && len a != 1)
			error(sprint("kbdfocus needs 0 or 1 arguments, saw %d", len a));
		focus := 0;
		if(len a == 1) {
			(v, rem) := str->toint(a[0], 10);
			if(rem != nil)
				error(sprint("bad kbdfocus arg %#q", a[0]));
			focus = v;
		}
		if(w == col.win) {
			nw := col.wins[(w.index+1)%len col.wins];
			winkbd(nw, 1);
			colsetwin(col, nw);
			ptrensure(col, col.win);
		}
	"lower" or
	"task" =>
		if(w.wantr.dy() > Winmin)
			winmin(c, w);
	"raise" or
	"unhide" =>
		winunhide(c, w);
		otherwin = w.tag;
	"max" =>
		winmax(c, w);
	"full" =>
		winsingle(c, w);
	"ptr" =>
		if(len a != 2)
			error("bad ptr request");
		ptrset(Point(int a[0], int a[1]));
	"cursor" =>
		if(len a == 0)
			d := array[0] of byte;
		else if(len a != 5)
			error("bad cursor request");
		else {
			hotx := int a[0];
			hoty := int a[1];
			dx := int a[2];
			dy := int a[3];
			n := dx/8*dy;
			if(2*n != len a[4])
				error("bad cursor image");
			d = array[4*4+n] of byte;
			o := 0;
			o = p32l(d, o, hotx);
			o = p32l(d, o, hoty);
			o = p32l(d, o, dx);
			o = p32l(d, o, dy);
			d[o:] = unhex(a[4]);
		}
		if(sys->write(cursorfd, d, len d) != len d)
			error(sprint("write cursor: %r"));
	* =>
		error(sprint("unrecognized request %#q", cmd));
	}

	wc <-= (len buf, nil);
	if(doimg) {
		say(sprint("sending image, r %s, wantr %s", r2s(rimg.r), r2s(w.wantr)));
		w.nbwm.images <-= rimg;
	}
}

reshape(w: ref Win): string
{
	if(w.img == nil)
		say(sprint("reshape: newimg, r %s, img nil", r2s(w.wantr)));
	else
		say(sprint("reshape: newimg, r %s, img.r %s", r2s(w.wantr), r2s(w.img.r)));
	i := drawctxt.screen.newwindow(w.wantr, draw->Refnone, draw->Nofill);
	if(i == nil)
		return sprint("newwindow %s: %r", r2s(w.wantr));
	w.ptroff = zeropt;
	w.img = i;
	w.haver = w.wantr;
	w.resizing = 0;
	w.dirty = 0;
	return nil;
}

winmk(fid: int): ref Win
{
	kbd := chan[128] of int;
	ptr := chan of ref Pointer;
	ctl := chan of string;
	wctl := chan of string;
	images := chan of ref Image;
	wm := ref Wmcontext(kbd, ptr, ctl, wctl, images, nil, drawctxt);

	nbwm := ref *wm;
	nbwm.kbd = kbd;
	nbwm.ptr = chan of ref Pointer;
	nbwm.ctl = chan of string;
	nbwm.images = chan of ref Image;

	tagtab: ref Strhash[ref (ref Image, Rect)];
	tagtab = tagtab.new(1, nil);
	w := ref Win(-1, -1, sprint("w%d", wingen++), fid, wm, nbwm, nil, 0, nil, zerorect, zerorect, 0, 0, tagtab, 0, zeropt);

	pidc := chan of int;
	spawn chanbuf(nbwm.ptr, wm.ptr, pidc);
	spawn chanbuf(nbwm.ctl, wm.ctl, pidc);
	spawn chanbuf(nbwm.images, wm.images, pidc);
	spawn wctlfwd(w, pidc);
	w.pids = list of {<-pidc, <-pidc, <-pidc, <-pidc};

	otherwin = w.tag;
	c := colnewwin();
	winadd(c, w);
	resize();
	focus(c, w, 1);
	return w;
}

# return col in which to place new win
colnewwin(): ref Col
{
	nc := col;
	if(col.visindex-1 >= 0 && 2*vis[col.visindex-1].r.dx() > 3*nc.r.dx() && len nc.wins != 0)
		nc = vis[col.visindex-1];
	if(col.visindex+1 < len vis && 2*vis[col.visindex+1].r.dx() > 3*nc.r.dx() && len nc.wins != 0)
		nc = vis[col.visindex+1];
	return nc;
}

# add w to c at appropriate place
winadd(c: ref Col, w: ref Win)
{
	w.colindex = c.index;

	if(c == col)
		fw := col.win;

	i := 0;
	h: array of int;

	if(c.mode == Msingle) {
		if(col.win != nil)
			i = col.win.index;
		w.wantr = c.r;
	} else {
		h = getheights(c);
		bw := biggest(c, fw);

		if(bw != nil)
			i = bw.index+1;
		dy := c.r.dy();
		if(bw == nil) {
			if(len c.wins == 0)
				h = array[] of {dy};
			else
				h = array[] of {2*dy/3, dy-2*dy/3};
		} else if(h[bw.index] >= 2*Winmin) {
			nh := h[bw.index]-Winmin;
			h[bw.index] = Winmin;
			h = concati(concati(h[:bw.index+1], array[] of {nh}), h[bw.index+1:]);
		} else if(fw != nil && fw.wantr.dy() >= 4*Winmin) {
			oh := h[fw.index];
			h[fw.index] = oh/2;
			h = concati(concati(h[:fw.index], array[] of {oh-oh/2}), h[fw.index:]);
			i = fw.index;
		} else {
			n := len c.wins+1;
			h = array[n] of {* => dy/n};
			h[bw.index+1] += dy-h[0]*n;
		}
	}

	c.wins = concat(concat(c.wins[:i], array[] of {w}), c.wins[i:]);
	renumber(c.wins);
	if(h != nil)
		setheights(c, h);
}

# return biggest window in c, ignore igw
biggest(c: ref Col, igw: ref Win): ref Win
{
	bw: ref Win;
	a := -1;
	for(i := 0; i < len c.wins; i++) {
		w := c.wins[i];
		na := w.wantr.dx()*w.wantr.dy();
		if(w != igw && na > a)
			(bw, a) = (w, na);
	}
	return bw;
}

renumber(w: array of ref Win)
{
	for(i := 0; i < len w; i++)
		w[i].index = i;
}

windrop(w: ref Win)
{
	if(w == nil)
		return;
	say(sprint("dropping win %s, otherwin %s", w.tag, otherwin));
	killall(w.pids);
	hadfocus := col.win==w;
	windel(cols[w.colindex], w);

	if(col.win == nil)
		(oc, ow) := winfindtag(otherwin);
	if(ow != nil)
		focus(oc, ow, 1);
	else if(col.win == nil && ow == nil) {
		for(i := 1; i < len vis; i++) {
			l := col.visindex-i;
			r := col.visindex+i;
			if(l >= 0 && l < len vis && vis[l].win != nil)
				oc = vis[l];
			else if(r >= 0 && r < len vis && vis[r].win != nil)
				oc = vis[r];
			else
				continue;
			focus(oc, oc.win, 1);
			break;
		}
	} else {
		focus(col, col.win, 1);
		if(hadfocus && col.win != nil)
			winkbd(col.win, 1);
	}
	resize();
}

# delete w from c.  set new c.win.
# caller is responsible for telling focus changes to win, and resize.
windel(c: ref Col, w: ref Win)
{
	i := w.index;
	if(find(c.wins, w) != i)
		raise "inconsistency";
	c.wins = concat(c.wins[:i], c.wins[i+1:]);
	renumber(c.wins);

	say(sprint("windel, i %d, w.index %d, len c.wins %d", i, w.index, len c.wins));
	if(len c.wins == 0)
		return colsetwin(c, nil);
	if(i < len c.wins && (i-1 < 0 || w.wantr.dy() == Winmin)) {
		# give to top of win below the removed win
		nw := c.wins[i];
		if(c.mode == Mstack)
			nw.wantr.min.y -= w.wantr.dy();
		colsetwin(c, nw);
	} else if(i-1 >= 0) {
		# give to bottom of win above removed win
		nw := c.wins[i-1];
		if(c.mode == Mstack)
			nw.wantr.max.y += w.wantr.dy();
		colsetwin(c, nw);
	}
}

getviswidths(): array of int
{
	a := array[len vis] of int;
	for(i := 0; i < len a; i++)
		a[i] = vis[i].r.dx();
	return a;
}

setviswidths(a: array of int)
{
	ocols := array[len cols] of ref Col;
	ocols[:] = cols;

	p := scrpt();
	x := 0;
	for(i := 0; i < len a; i++) {
		c := vis[i];
		ocols[c.index] = nil;  # skip later
		c.tagr.min.x = c.r.min.x = c.fr.min.x = x;
		x += a[i];
		c.tagr.max.x = c.r.max.x = c.fr.max.x = x;
		for(j := 0; j < len c.wins; j++) {
			w := c.wins[j];
			if(c.mode == Msingle) {
				if(w != c.win && w.wantr.min.x < p.x)
					w.wantr = w.wantr.addpt(p);
				else if(w == c.win)
					w.wantr = c.r;
			} else {
				w.wantr.min.x = c.r.min.x;
				w.wantr.max.x = c.r.max.x;
			}
		}
	}
	# move other columns off-screen
	for(i = 0; i < len ocols; i++) {
		c := ocols[i];
		if(c == nil || c.r.min.x >= p.x)
			continue;
		c.tagr = c.tagr.addpt(p);
		c.r = c.r.addpt(p);
		c.fr = c.fr.addpt(p);
		for(j := 0; j < len c.wins; j++) {
			w := c.wins[j];
			w.wantr = w.wantr.addpt(p);
		}
	}
}

getheights(c: ref Col): array of int
{
	if(c.mode == Msingle)
		raise "getheights for Msingle";

	a := array[len c.wins] of int;
	for(i := 0; i < len a; i++)
		a[i] = c.wins[i].wantr.dy();
	return a;
}

setheights(c: ref Col, a: array of int)
{
	if(c.mode == Msingle)
		raise "setheights for Msingle";
	y := c.r.min.y;
	for(i := 0; i < len c.wins; i++) {
		w := c.wins[i];
		w.wantr = Rect((c.r.min.x, y), (c.r.max.x, y+a[i]));
		y += a[i];
	}
}

renumbervis()
{
	for(i := 0; i < len cols; i++)
		cols[i].visindex = -1;
	for(i = 0; i < len vis; i++)
		vis[i].visindex = i;
}

visset(n: array of ref Col, w: array of int)
{
	vis = n;
	renumbervis();
	setviswidths(w);
	resize();
}


colsetwin(c: ref Col, w: ref Win)
{
	c.win = w;
	if(w == nil || c.mode != Msingle)
		return;

	p := scrpt();
	for(i := 0; i < len c.wins; i++) {
		ww := c.wins[i];
		if(ww.img == nil)
			continue;
		if(ww != w && ww.wantr.min.x < p.x)
			ww.wantr = ww.wantr.addpt(p);
		else if(ww == w)
			ww.wantr = c.r;
	}
}

resize()
{
	p := scrpt();
	for(j := 0; j < len cols; j++) {
		c := cols[j];
		for(i := 0; i < len c.wins; i++) {
			w := c.wins[i];
			if(w.resizing || w.img == nil || (w.wantr.eq(w.haver) && !w.dirty))
				continue;

			if(!w.dirty)
			if(!w.fixedorigin)
			if(w.wantr.dx() == w.haver.dx())
			if(w.wantr.dy() == w.haver.dy())
			if(c.mode == Mstack && w.wantr.min.x == w.haver.min.x || c.mode == Msingle && w.wantr.min.x >= p.x) {
				if(w.img.origin(w.img.r.min, w.wantr.min) != 1)
					warn(sprint("setting origin: %r"));
				else {
					w.ptroff = w.ptroff.add(w.haver.min.sub(w.wantr.min));
					w.haver = w.wantr;
				}
				continue;
			}

			w.resizing = 1;
			winctl(w, "!size . -1 0 0");
		}
		if(len c.wins == 0 && c.visindex >= 0)
			drawctxt.screen.image.draw(c.r, colbg, nil, zeropt);
	}
}


# xi was dragged onto yi, released at off.
movepast[T](oa: array of T, osz: array of int, xi, yi, off: int): (array of T, array of int)
{
	na: array of T;
	nsz: array of int;

	xt := array[] of {oa[xi]};
	if(xi < yi) {
		ndy := sum(osz[:yi+1])-off;
		szd := ndy-osz[xi];
		osz[xi] += szd;
		osz[yi] -= szd;
		xsz := array[] of {osz[xi]};
		na = concat(oa[:xi], concat(oa[xi+1:yi+1], concat(xt, oa[yi+1:])));
		nsz = concati(osz[:xi], concati(osz[xi+1:yi+1], concati(xsz, osz[yi+1:])));
	} else {
		szd := sum(osz[:yi+1])-off;
		osz[yi] -= szd;
		osz[xi] += szd;
		xsz := array[] of {osz[xi]};
		na = concat(oa[:yi+1], concat(xt, concat(oa[yi+1:xi], oa[xi+1:])));
		nsz = concati(osz[:yi+1], concati(xsz, concati(osz[yi+1:xi], osz[xi+1:])));
	}
	return (na, nsz);
}

colsetleft(c: ref Col, x: int)
{
	i := c.visindex;
	if(i == 0)
		return;
	w := getviswidths();
	n := c.r.min.x-x;
	w[i] += n;
	w[i-1] -= n;
	setviswidths(w);
	resize();
}


colbox(c: ref Col): Rect
{
	min := c.tagr.min.add((3,3));
	d := c.tagr.dy()-2*3;
	max := min.add((d, d));
	return Rect((min, max));
}

drawtags()
{
	for(i := 0; i < len cols; i++)
		drawtag(cols[i]);
}

drawtag(c: ref Col)
{
	if(c.visindex < 0)
		return;

	t := tagimg;
	t.draw(t.r, bg, nil, zeropt);

	box := colbox(c);
	t.draw(box, boxc, nil, zeropt);

	b := bg;
	if(col == c)
		b = selbg;
	p := Point(box.max.x+3, t.r.min.y+1);
	p = t.text(p, fg, zeropt, tagfont, " ");
	p = t.textbg(p, fg, zeropt, tagfont, sprint("%d ", c.index+1), b, zeropt);
	c.moder = Rect((p.x, c.tagr.min.y), (0, c.tagr.max.y));
	m := modes[c.mode];
	p = t.textbg(p, fg, zeropt, tagfont, m, b, zeropt);
	p.x += (7-len m)*tagfont.width(" ");
	c.moder.max.x = p.x;
	c.tagwins = array[len c.wins] of Rect;
	slackwidth := tagfont.width(" ")/2;
	for(i := 0; i < len c.wins; i++) {
		b = bg;
		if(c.win == c.wins[i])
			b = selbg;
		p = t.textbg(p, fg, zeropt, tagfont, " ", bg, zeropt);
		c.tagwins[i] = c.tagr;
		c.tagwins[i].min.x = p.x-slackwidth;
		p = t.textbg(p, fg, zeropt, tagfont, sprint("%d", i), b, zeropt);
		c.tagwins[i].max.x = p.x+slackwidth;
	}
	p = t.textbg(p, fg, zeropt, tagfont, " ", bg, zeropt);

	c.sr.min = Point(p.x, c.tagr.min.y);
	(i0, i1) := (c.sp.i0, c.sp.i1);
	if(i0 == i1) {
		p = t.textbg(p, fg, zeropt, tagfont, c.s, bg, zeropt);

		# draw cursor
		miny := c.tagr.min.y;
		maxy := c.tagr.max.y;
		cx := c.sr.min.x+tagfont.width(c.s[:i0]);
		cr := Rect((cx, miny), (cx+1, maxy));
		cbr0 := Rect((cx-1, miny), (cx+2, miny+2));
		cbr1 := cbr0.addpt(Point(0, c.tagr.dy()-2));
		t.draw(cr, fg, nil, zeropt);
		t.draw(cbr0, fg, nil, zeropt);
		t.draw(cbr1, fg, nil, zeropt);
	} else {
		(i0, i1) = order(i0, i1);
		l := list of {
		(c.s[:i0], bg),
		(c.s[i0:i1], selbg),
		(c.s[i1:], bg)
		};
		for(; l != nil; l = tl l) {
			(s, sbg) := hd l;
			if(s != nil)
				p = t.textbg(p, fg, zeropt, tagfont, s, sbg, zeropt);
		}
	}
	c.sr.max = Point(p.x, c.tagr.max.y);

	drawctxt.display.image.draw(c.tagr, t, nil, c.tagr.min);
}

# place focus on col/win where ptr is
ptrfocus()
{
	(c, w) := winfindpt(ptrprev.xy);
	if(w == nil && len c.wins != 0)
		w = c.wins[0];
	focus(c, w, 0);
}

# if ptr is not on w, move it there.  if win is nil, make sure ptr is on c.
ptrensure(c: ref Col, w: ref Win)
{
	if(w == nil)
		return ptrsettag(c);
	(x, y) := ptrprev.xy;
	if(w.wantr.contains((x,y)))
		return;
	(nx, ny) := w.wantr.min.add((w.wantr.dx()/2, 10));
	opts := array[] of {(x,ny), (nx,y), (nx,ny)};
	for(i := 0; i < len opts; i++)
		if(w.wantr.contains(opts[i]))
			return ptrset(opts[i]);
	ptrset(rectmid(c.tagr));
}

# move focus to w in c.  or just c if w is nil.
# sets col.win.
# also sends haskbdfocus messages.
# if c is not focused, shows it too.
focus(c: ref Col, w: ref Win, ensptr: int)
{
	if(w == nil && len c.wins != 0)
		raise "bad focus";
	if(col.win != w) {
		if(col.win != nil) {
			winkbd(col.win, 0);
			otherwin = col.win.tag;
		}
		if(w != nil) {
			if(c.mode == Mstack && w.wantr.dy() < Winmin) {
				h := getheights(c);
				want := Winmin-w.wantr.dy();
				taken := 0;
				taken += scavenge(want-taken, Winmin, 0, w.index, 1, h);
				taken += scavenge(want-taken, Winmin, w.index+1, len c.wins, 1, h);
				taken += scavenge(want-taken, 0, 0, w.index, 1, h);
				taken += scavenge(want-taken, 0, w.index+1, len c.wins, 1, h);
				h[w.index] += taken;
				setheights(c, h);
			}
			winkbd(w, 1);
		}
	}
	col = c;
	colsetwin(c, w);
	resize();
	if(ensptr)
		ptrensure(col, w);
}


# scavange up to 'want' in total, keeping 'keep' per entry, starting at index 's',
# ending at index 'e' (exclusive), width 'delta' as direction (1 or -1).
scavenge(want, keep, s, e, delta: int, sz: array of int): int
{
	if(delta > 0 && s > e || delta < 0 && s < e)
		return 0;
	taken := 0;
	while(want > taken && s >= 0 && s < len sz) {
		take := min(want-taken, max(0, sz[s]-keep));
		sz[s] -= take;
		taken += take;
		s += delta;
		if(s == e)
			break;
	}
	return taken;
}

# move w from oc to nc, at destination y
winmovecol(w: ref Win, oc: ref Col, nc: ref Col, y: int)
{
	windel(oc, w);
	col = nc;

	h: array of int;
	if(nc.mode == Msingle) {
		nc.wins = concat(nc.wins, array[] of {w});
		w.wantr = nc.r;
	} else {
		if(y < nc.r.min.y)
			y = nc.r.min.y;
		(nil, ow) := winfindpt(Point(nc.r.min.x, y));
		if(ow == nil && len nc.wins != 0)
			ow = nc.win;
		if(ow != nil) {
			h = getheights(nc);
			nc.wins = concat(concat(nc.wins[:ow.index+1], array[] of {w}), nc.wins[ow.index+1:]);
			dh := ow.wantr.max.y-y;
			h[ow.index] -= dh;
			h = concati(concati(h[:ow.index+1], array[] of {dh}), h[ow.index+1:]);
			ensurewinmin(h);
		} else {
			nc.wins = array[] of {w};
			h = array[] of {nc.r.dy()};
		}
	}
	w.colindex = nc.index;
	renumber(nc.wins);
	colsetwin(nc, w);
	if(nc.mode != Msingle)
		setheights(nc, h);
	resize();
}

# ensure each window has Winmin where possible.
# start taking space from the bottom windows upwards.
ensurewinmin(h: array of int)
{
	need := 0;
	avail := 0;
	for(i := 0; i < len h; i++) {
		need += max(0, Winmin-h[i]);
		avail += max(0, h[i]-Winmin);
	}
	need = min(need, avail);
	taken := 0;
	for(i = 0; i < len h && taken < need; i++) {
		n := min(max(0, Winmin-h[i]), need-taken);
		h[i] += n;
		taken += n;
	}
	for(i = len h-1; i >= 0 && taken > 0; i --) {
		n := min(max(0, h[i]-Winmin), taken);
		h[i] -= n;
		taken -= n;
	}
}

winbigger(c: ref Col, w: ref Win, pt: Point, canmove: int, want: int)
{
	if(c.mode == Msingle)
		return;
	oy := w.wantr.min.y;
	say(sprint("bigger win %d", w.index));
	h := getheights(c);
	taken := scavenge(want, Winmin, w.index+1, len c.wins, 1, h);
	taken += scavenge(want-taken, Winmin, 0, w.index, 1, h);
	h[w.index] += taken;
	setheights(c, h);
	resize();
	focus(c, w, 0);

	if(canmove && oy != w.wantr.min.y)
		ptrset(pt.add((0, w.wantr.min.y-oy)));
}

winmax(c: ref Col, w: ref Win)
{
	if(c.mode == Msingle)
		return;
	say(sprint("biggest win %d", w.index));
	h := getheights(c);
	left := c.r.dy();
	nh := array[len h] of int;
	nh[:] = h;
	nh[w.index] = left/2;
	left -= nh[w.index];
	for(i := 0; i < len h; i++)
		if(i != w.index) {
			nh[i] = min(left, Winmin);
			left -= nh[i];
		}
	nh[w.index] += left;
	setheights(c, nh);
	resize();
	focus(c, w, 0);
}

winmin(c: ref Col, w: ref Win)
{
	say(sprint("min win %d", w.index));
	if(len c.wins == 1 || w.wantr.dy() <= Winmin)
		return;
	ni := (w.index-1+len c.wins)%len c.wins;
	if(c.mode == Msingle) {
		nw := c.wins[ni];
		focus(c, nw, 0);
		resize();
	} else {
		h := getheights(c);
		take := max(0, w.wantr.dy()-Winmin);
		# find first win (first up, than down) that isn't already minimized
		for(i := w.index-1; i >= 0 && h[i] <= Winmin; i--)
			{}
		if(i < 0)
			for(i = w.index+1; i < len c.wins && h[i] <= Winmin; i++)
				{}
		if(i < 0 || i >= len c.wins)
			i = ni;
		h[w.index] -= take;
		h[i] += take;
		nw := c.wins[i];
		setheights(c, h);
		resize();
		focus(c, nw, 0);
		ptrset((ptrprev.xy.x, nw.wantr.min.y+10));
	}
}

winunhide(c: ref Col, w: ref Win)
{
	say(sprint("unhide, c.index %d, c.visindex %d", c.index, c.visindex));
	if(c.visindex < 0) {
		say("toggling col");
		coltoggle(c, 0);
	}
	if(c.mode == Msingle) {
		winsingle(c, w);
	} else if(w.wantr.dy() <= 2*Winmin) {
		want := c.r.dy()/4;
		h := getheights(c);
		taken := scavenge(want, Winmin, w.index+1, len c.wins, 1, h);
		taken += scavenge(want-taken, Winmin, 0, w.index, 1, h);
		h[w.index] += taken;
		setheights(c, h);
		resize();
	}
	ptrensure(col, col.win);
}

winsingle(c: ref Col, w: ref Win)
{
	say(sprint("single win %d", w.index));
	if(c.mode == Mstack)
		modesingle();
	else {
		resize();
		focus(c, w, 0);
	}
}

# make col bigger.
# if it is rightmost, start taking space from the left.
# otherwise, start taking from the right.
colbigger(c: ref Col, warp: int)
{
	ox := c.tagr.min.x;
	want := tagimg.r.dx()/8;
	keep := Colmin;
	w := getviswidths();
	i := c.visindex;
	if(i == len vis-1)
		taken := scavenge(want, keep, 0, i, 1, w);
	else {
		taken = scavenge(want, keep, len vis-1, i, -1, w);
		taken += scavenge(want-taken, keep, 0, i, 1, w);
	}
	w[i] += taken;
	setviswidths(w);
	resize();
	if(warp && ox != c.tagr.min.x)
		ptrsetbox(c);
}

# make col as big as possible, leaving max Colmin for the others
colmax(c: ref Col, warp: int)
{
	ox := c.tagr.min.x;

	w := getviswidths();
	j := c.visindex;
	given := 0;
	for(i := 0; i < len w; i++)
		if(i != j) {
			w[i] = min(w[i], Colmin);
			given += w[i];
		}
	w[j] = tagimg.r.dx()-given;
	setviswidths(w);
	resize();
	if(warp && ox != c.tagr.min.x)
		ptrsetbox(c);
}

cfgget(): ref Cfg
{
	a := array[len vis] of ref Col;
	a[:] = vis;
	tag := "";
	if(col.win != nil)
		tag = col.win.tag;
	return ref Cfg(a, getviswidths(), col, tag);
}

cfgset(cfg: ref Cfg)
{
	if(othercfg == nil || !configeq(othercfg, cfg))
		othercfg = cfg;
}

configeq(a, b: ref Cfg): int
{
	if(len a.c != len b.c)
		return 0;
	for(i := 0; i < len a.c; i++)
		if(a.c[i] != b.c[i])
			return 0;
	return 1;
}

# switch to previous col configuration
colswitch()
{
	if(othercfg == nil || configeq(cfgget(), othercfg))
		return;

	(c, w) := winfindtag(othercfg.tag);
	if(w != nil && find(othercfg.c, c) < 0)
		w = nil;
	if(c == nil || w == nil && len c.wins != 0) {
		# othercfg.tag probably quit
		othercfg = nil;
		return;
	}
	visset(othercfg.c, othercfg.w);
	focus(othercfg.col, w, 1);
}

coltoggle(c: ref Col, givefocus: int)
{
	cfg := cfgget();
	if(coltoggle0(c))
		cfgset(cfg);
	if(c.visindex >= 0 && givefocus) {
		focus(c, c.win, 1);
		if(c.win != nil) {
			r := c.win.wantr;
			r.max.y = r.min.y+10;
			ptrset(rectmid(r));
		}
	}
}

# toggle visibility of c
# return true if visibility of col has changed
coltoggle0(c: ref Col): int
{
	say(sprint("toggle col %d", c.index));
	i := c.visindex;
	if(i < 0) {
		# add c
		ci := col.visindex;
		w := getviswidths();
		avail := tagimg.r.dx()/(len vis+1);
		keep := min(avail, Colmin);
		take := scavenge(avail, keep, ci+1, len vis, 1, w);
		take += scavenge(avail-take, keep, 0, ci+1, 1, w);
		nv := concat(vis[:ci+1], concat(array[] of {c}, vis[ci+1:]));
		nw := concati(w[:ci+1], concati(array[] of {take}, w[ci+1:]));
		visset(nv, nw);
		ptrensure(col, col.win);
		return 1;
	}
	if(len vis == 1)
		return 0;  # cannot remove last col

	# remove col
	w := getviswidths();
	ni := i+1;
	if(i+1 >= len w)
		ni = i-1;
	w[ni] += w[i];
	nv := concat(vis[:i], vis[i+1:]);
	nw := concati(w[:i], w[i+1:]);
	visset(nv, nw);
	ptrfocus();
	return 1;
}

# show non-empty columns only
colnonempty()
{
	n, a: list of ref Col;
	for(i := 0; i < len cols; i++) {
		c := cols[i];
		if(len c.wins == 0)
			continue;
		if(c.visindex < 0)
			a = c::a;
		else
			n = c::n;
	}
	if(len n == len vis && a == nil)
		return;
	if(n == nil && a == nil)
		a = cols[0]::nil;
	ow := getviswidths();
	nw := array[len n+len a] of int;
	nv := l2a(n);
	left := tagimg.r.dx();
	for(i = 0; i < len nv; i++) {
		nw[i] = ow[nv[i].visindex];
		left -= nw[i];
	}
	need := len a*Colmin;
	keep := min(tagimg.r.dx()/(len n+len a), Colmin);
	left += scavenge(need-left, keep, 0, len nv, 1, nw[:len nv]);
	nv = concat(nv, l2a(a));

	# give space to new cols
	for(; i < len nv; i++)
		left -= nw[i] = left/len a;

	# give remaining space to current or new col
	i = find(nv, col);
	if(i < 0)
		i = 0;
	nw[i] += left;
	visset(nv, nw);
	if(col.visindex < 0)
		focus(vis[0], vis[0].win, 0);
}

colsingle(c: ref Col)
{
	visset(array[] of {c}, array[] of {tagimg.r.dx()});
	focus(c, c.win, 0);
}

scrpt(): Point
{
	return Point(drawctxt.screen.image.r.max.x, 0);
}

modesingle()
{
	col.mode = Msingle;
	p := scrpt();
	for(i := 0; i < len col.wins; i++) {
		w := col.wins[i];
		if(w == col.win)
			w.wantr = col.r;
		else if(w.wantr.min.x < p.x)
			w.wantr = w.wantr.addpt(p);
	}
	resize();
}

modestack()
{
	col.mode = Mstack;
	
	n := len col.wins;
	if(n == 0)
		h := array[0] of int;
	else if(n == 1)
		h = array[] of {col.r.dy()};
	else {
		miny := max(2*Winmin, col.r.dy()-Winmin*(n-1));
		h = array[len col.wins] of {* => (col.r.dy()-miny)/(n-1)};
		h[col.win.index] = col.r.dy()-h[0]*(n-1);
	}
	setheights(col, h);
	resize();
	if(col.win != nil)
		ptrensure(col, col.win);
}

modetoggle()
{
	case col.mode {
	Mstack =>	modesingle();
	Msingle =>	modestack();
	}
}


# return col & win ptr is over.  win may be nil.
winfindpt(p: Point): (ref Col, ref Win)
{
	for(j := 0; j < len cols; j++) {
		c := cols[j];
		ww := c.wins;
		w: ref Win;
		for(i := 0; i < len ww; i++)
			if(ww[i].wantr.contains(p) && (w == nil || c.win == ww[i]))
				w = ww[i];
		if(w != nil)
			return (c, w);
		if(c.fr.contains(p))
			return (c, nil);
	}
	return (nil, nil);
}

winfindtag(tag: string): (ref Col, ref Win)
{
	for(j := 0; j < len cols; j++) {
		wins := cols[j].wins;
		for(i := 0; i < len wins; i++)
			if(wins[i].tag == tag)
				return (cols[j], wins[i]);
	}
	return (nil, nil);
}

winfindfid(fid: int): ref Win
{
	for(j := 0; j < len cols; j++) {
		wins := cols[j].wins;
		for(i := 0; i < len wins; i++)
			if(wins[i].fid == fid)
				return wins[i];
	}
	return nil;
}

rectmid(r: Rect): Point
{
	return r.min.add((r.dx()/2, r.dy()/2));
}

rectindex(a: array of Rect, pt: Point): int
{
	for(i := 0; i < len a; i++)
		if(a[i].contains(pt))
			return i;
	return -1;
}

r2s(r: Rect): string
{
	return sprint("%d %d %d %d", r.min.x, r.min.y, r.max.x, r.max.y);
}

p2s(p: Point): string
{
	return sprint("%d %d", p.x, p.y);
}

l2a[T](l: list of T): array of T
{
	a := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		a[i++] = hd l;
	return a;
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

find[T](a: array of T, e: T): int
{
	for(i := 0; i < len a; i++)
		if(a[i] == e)
			return i;
	return -1;
}

findstr(a: array of string, s: string): int
{
	for(i := 0; i < len a; i++)
		if(a[i] == s)
			return i;
	return -1;
}

sum(a: array of int): int
{
	v := 0;
	for(i := 0; i < len a; i++)
		v += a[i];
	return v;
}

order(a, b: int): (int, int)
{
	if(a < b)
		return (a, b);
	return (b, a);
}

concat[T](a, b: array of T): array of T
{
	n := array[len a+len b] of T;
	n[:] = a;
	n[len a:] = b;
	return n;
}

concati(a, b: array of int): array of int
{
	n := array[len a+len b] of int;
	n[:] = a;
	n[len a:] = b;
	return n;
}

p32l(d: array of byte, o: int, v: int): int
{
	d[o++] = byte v>>0;
	d[o++] = byte v>>8;
	d[o++] = byte v>>16;
	d[o++] = byte v>>24;
	return o;
}

unhexc(c: int): int
{
	case c {
	'0' to '9' =>	return c-'0';
	'a' to 'f' =>	return c-'a'+10;
	'A' to 'F' =>	return c-'A'+10;
	}
	return 0;
}

unhex(s: string): array of byte
{
	n := len s;
	d := array[n/2] of byte;
	i := 0;
	for(j := 0; j < n; j += 2)
		d[i++] = byte unhexc(s[j])<<4 | byte unhexc(s[j+1]);
	return d;
}

killall(l: list of int)
{
	for(; l != nil; l = tl l)
		kill(hd l);
}

kill(pid: int)
{
	sys->fprint(sys->open(sprint("/prog/%d/ctl", pid), Sys->OWRITE), "kill");
}

abs(i: int): int
{
	if(i < 0)
		i = -i;
	return i;
}

min(a, b: int): int
{
	if(a < b) return a;
	return b;
}

max(a, b: int): int
{
	if(a > b) return a;
	return b;
}

pid(): int
{
	return sys->pctl(0, nil);
}

warn(s: string)
{
	if(fd2 == nil)
		fd2 = sys->fildes(2);
	sys->fprint(fd2, "%s\n", s);
}

say(s: string)
{
	if(dflag)
		warn(s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
