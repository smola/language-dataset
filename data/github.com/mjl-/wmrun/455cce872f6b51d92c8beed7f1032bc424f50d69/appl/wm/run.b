implement WmRun;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "keyboard.m";
	kb: Keyboard;
include "names.m";
	names: Names;
include "plumbmsg.m";
	plumbmsg: Plumbmsg;
	Msg: import plumbmsg;
include "readdir.m";
	readdir: Readdir;
include "sh.m";
	sh: Sh;
	Listnode, Context: import sh;
include "tk.m";
	tk: Tk;
include "tkclient.m";
	tkclient: Tkclient;
include "wait.m";
	wait: Wait;

WmRun: module {
	init:	fn(ctxt: ref Draw->Context, argv: list of string);
};
Cmd: type WmRun;

dflag: int;
autoscroll := 1;
markup := 1;
font: string;

Histsize: con 50000;
showcomplete: int;
plumbed: int;

exc: chan of string;
statusc: chan of string;

Einsert, Evi, Ectlx, Eraw: con iota;
editmode := Einsert;
ctlv: int;
rawfids: list of int;

Ctl: con -16r60;	# offset for lower case control

Read: adt {
	n:	int;
	c:	sys->Rread;
};

Job: adt {
	pid:	int;
	cmd:	string;
};

Str: adt {
	s:	string;
	i:	int;
	si,
	ei:	int;  # for motion commands, start and end for motion (ordered)

	more:		fn(x: self ref Str): int;
	char:		fn(x: self ref Str): int;
	xget:		fn(x: self ref Str): int;
	in:		fn(x: self ref Str, cl: string): int;
	readrep:	fn(x: self ref Str, def: int): (int, string);
	previn:		fn(x: self ref Str, cl: string): int;
	skipcl:		fn(x: self ref Str, cl: string): int;
	rskipcl:	fn(x: self ref Str, cl: string): int;
	findcl:		fn(x: self ref Str, cl: string): int;
	rfindcl:	fn(x: self ref Str, cl: string): int;
	text:		fn(x: self ref Str): string;
};

Link: adt[T] {
	prev,
	next:	cyclic ref Link;
	e:	T;
};

List: adt[T] {
	first,
	last:	ref Link[T];

	new:	fn(): ref List;
	add:	fn(l: self ref List, e: T);
	take:	fn(l: self ref List): T;
	empty:	fn(l: self ref List): int;
};

reads: ref List[ref Read];
input: ref List[array of byte];
history: ref List[string];	# history.last is unwritten text
histcur: ref Link[string];	# non-nil while navigating history, elem in history
job: ref Job;

vikeys: ref Str;	# keys for current command
viprevkeys: ref Str;	# for repeat
vibuf: string;		# for yank/deletions & pasting
viorig: ref Str;	# before vi command, for undo
viundo,
viredo: list of ref Str;

focus := ".out";	# focused window, for pgup/pgdown etc commands

Tprompt, Tcmd, Tin, Tout, Terr, Tstatus, Tex, Texit, Tok: con iota;
tagstrs := array[] of {
"cmd", "cmd", "in", "out", "err", "status", "status", "status", "ok",
};

shctxt: ref Sh->Context;
drawctxt: ref Draw->Context;

top: ref Tk->Toplevel;
wmctl: chan of string;

tkcmds0 := array[] of {
"frame	.t",
"frame	.t.out",
"text	.out		-fg white -bg black -selectforeground black -selectbackground white -yscrollcommand {.t.out.sy set}",
"scrollbar .t.out.sy	-command {.out yview}",

"pack .t.out.sy		-fill y -side left",
"pack .out -in .t.out	-fill both -expand 1",
"pack .t.out		-fill both -expand 1",

"bind .out	<Control-t> {focus .e.input}",

"frame .c",
"text .c.complete	-yscrollcommand {.c.s set} -fg black -bg white",
"scrollbar .c.s		-command {.c.complete yview}",
"pack .c.s		-fill y -side left",
"pack .c.complete	-fill both -expand 1 -side right",

"frame	.e",
"label	.e.io -text { } -width 1w",
"entry	.e.input",

"pack .e.io		-side left",
"pack .e.input		-fill x",

"pack .t	-fill both -expand 1",
"pack .e	-fill x",

"menu	.m",
# keep indices in sync with tkscrollset & tkmarkupset
".m add command -label clear -command {send cmd clear}",
".m add command -label noscroll -command {send cmd scroll}",
".m add command -label nomarkup -command {send cmd markup}",
".m add command -label send -command {send cmd send}",

"focus .e.input",
"pack propagate . 0",
". configure -width 80w -height 24h",
};

init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	if(ctxt == nil)
		fail("no window context");
	drawctxt = ctxt;
	draw = load Draw Draw->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	names = load Names Names->PATH;
	plumbmsg = load Plumbmsg Plumbmsg->PATH;
	readdir = load Readdir Readdir->PATH;
	str = load String String->PATH;
	sh = load Sh Sh->PATH;
	sh->initialise();
	shctxt = Context.new(ctxt);
	tk = load Tk Tk->PATH;
	tkclient = load Tkclient Tkclient->PATH;
	wait = load Wait Wait->PATH;
	wait->init();

	sys->pctl(Sys->NEWPGRP|Sys->FORKNS, nil);

	plumbed = plumbmsg->init(1, nil, 512) >= 0;

	arg->init(args);
	arg->setusage(arg->progname()+" [-dMS] [-f font] [cmd ...]");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	dflag++;
		'f' =>	font = arg->earg();
		'M' =>	markup = 0;
		'S' =>	autoscroll = 0;
		* =>	arg->usage();
		}
	args = arg->argv();

	spawn main(args);
}

main(args: list of string)
{
	tkclient->init();
	(top, wmctl) = tkclient->toplevel(drawctxt, "", "run "+workdir(), Tkclient->Appl);

	cmdc := chan of string;
	keyc := chan of string;
	tptrc := chan of string;
	eptrc := chan of string;
	tk->namechan(top, cmdc, "cmd");
	tk->namechan(top, keyc, "key");
	tk->namechan(top, tptrc, "tptr");
	tk->namechan(top, eptrc, "eptr");
	for(i := 0; i < len tkcmds0; i++)
		tkcmd(tkcmds0[i]);
	tktags(markup);
	if(font != nil) {
		for(l := list of {".out", ".e.input", ".e.io"}; l != nil; l = tl l)
			tkcmd(hd l+" configure -font "+font);
	}
	tkscrollset(autoscroll);
	tkmarkupset(markup);

	keys := list of {kb->Home, kb->End, kb->Del, 16r7f};
	tkcmd("bind .e.input <Key> {send key %K}");
	ctls := list of {'d', 'x', 'v'};
	for(l := keys; l != nil; l = tl l)
		tkcmd(sprint("bind .e.input <Key-%c> {send key %%K}", hd l));
	for(l = ctls; l != nil; l = tl l)
		tkcmd(sprint("bind .e.input {<Control-%c>} {send key %%K}", hd l));

	for(b := list of {1, 2, 3}; b != nil; b = tl b) {
		tkcmd(sprint("bind .out <ButtonPress-%d> +{send tptr %%s %%W @%%x,%%y %%X %%Y}", hd b));
		tkcmd(sprint("bind .out <ButtonRelease-%d> +{send tptr %%s %%W @%%x,%%y %%X %%Y}", hd b));

		tkcmd(sprint("bind .e.input <ButtonPress-%d> +{send eptr %%s @%%x,%%y}", hd b));
		tkcmd(sprint("bind .e.input <ButtonRelease-%d> +{send eptr %%s @%%x,%%y}", hd b));
	}

	tkclient->onscreen(top, nil);
	tkclient->startinput(top, "kbd"::"ptr"::nil);

	if(sys->bind("#s", "/chan", sys->MBEFORE) < 0
	|| sys->bind("#s", "/dev", sys->MBEFORE) < 0)
		fail(sprint("bind #s: %r"));

	cons := sys->file2chan("/chan", "cons");
	consctl := sys->file2chan("/chan", "consctl");
	conserr := sys->file2chan("/dev", "conserr");
	if(cons == nil || consctl == nil || conserr == nil)
		fail(sprint("file2chan: %r"));

	if(sys->bind("/chan/cons", "/dev/cons", sys->MREPL) < 0
	|| sys->bind("/chan/consctl", "/dev/consctl", sys->MREPL) < 0)
		fail(sprint("bind cons & consctl: %r"));

	waitfd := sys->open(sprint("/prog/%d/wait", pid()), sys->OREAD);
	if(waitfd == nil)
		fail(sprint("open: %r"));
	(nil, waitc) := wait->monitor(waitfd);
	waitfd = nil;

	exc = chan of string;
	statusc = chan of string;

	reads = reads.new();
	input = input.new();
	history = history.new();
	history.add("");
	histcur = history.last;
	text(Tprompt, "");

	if(args != nil)
		runinit(args);

	for(;;)
	alt {
	s := <-top.ctxt.kbd =>
		tk->keyboard(top, s);

	s := <-top.ctxt.ptr =>
		tk->pointer(top, *s);

	s := <-top.ctxt.ctl or
	s = <-top.wreq or
	s = <-wmctl =>
		scroll := str->prefix("!", s) && tkcmd(".out dlineinfo {end -1c linestart}") != nil;
		tkclient->wmctl(top, s);
		if(scroll) {
			tkcmd(".out scan mark 0 0; .out scan dragto -10000 -10000");
			tkup();
		}

	s := <-tptrc =>
		tptr(s);
		tkup();

	s := <-eptrc =>
		eptr(s);
		tkup();

	s := <-keyc =>
		(cc, nil) := str->toint(s, 16);
		key(cc);
		tkup();

	s := <-cmdc =>
		cmd(s);
		tkup();

	s := <-exc =>
		finished(Tex, s);
		tkup();

	s := <-statusc =>
		if(s == nil)
			finished(Tok, s);
		else
			finished(Tstatus, s);
		tkup();

	(pid, mod, err) := <-waitc =>
		if(dflag) warn(sprint("wait: %d, %s, %s", pid, mod, err));
		if(pid <= 0) {
			warn("waiter quit: "+err);
			continue;
		}
		if(err != nil && job != nil && job.pid == pid) {
			if(err == nil)
				finished(Tok, "");
			else
				finished(Texit, mod+": "+err);
		} else if(err != nil)
			finished(Texit, sprint("%d \"%s\":%s", pid, mod, err));
		tkup();

	(nil, n, nil, rc) := <-cons.read =>
		# stdin
		if(rc == nil)
			continue;
		reads.add(ref Read(n, rc));
		io();
		tkup();

	(nil, buf, nil, rc) := <-cons.write =>
		# stdout
		if(rc == nil)
			continue;
		textio(Tout, buf);
		tkup();
		rc <-= (len buf, nil);

	(nil, buf, nil, rc) := <-conserr.write =>
		# stderr
		if(rc == nil)
			continue;
		textio(Terr, buf);
		tkup();
		rc <-= (len buf, nil);

	(nil, buf, fid, rc) := <-consctl.write =>
		# consctl
		if(rc == nil)
			rawfids = del(rawfids, fid);
		else
			case string buf {
			"rawon" =>
				rawfids = fid::del(rawfids, fid);
			"rawoff" =>
				rawfids = del(rawfids, fid);
			* =>
				rc <-= (-1, "bad ctl");
				continue;
			}
		if(editmode == Eraw && rawfids == nil)
			tksetedit(Einsert);
		if(editmode != Eraw && rawfids != nil) {
			tksetedit(Eraw);
			input = input.new();
		}
		if(rc != nil)
			rc <-= (len buf, nil);

	(nil, nil, nil, rc) := <-consctl.read or
	(nil, nil, nil, rc) = <-conserr.read =>
		if(rc != nil)
			rc <-= (nil, "permission denied");
	}
}

lastb: int;
bspecial: int;
tptr(m: string)
{
	(nil, l) := sys->tokenize(m, " ");
	b := int hd l;
	w := hd tl l;
	coord := hd tl tl l;

	l = tl tl tl l;
	x := hd l;
	y := hd tl l;
	pos := tkcmd(sprint("%s index %s", w, coord));
if(dflag) warn(sprint("lastb %x, b %d, widget %s, coord %s, pos %s", lastb, b, w, coord, pos));
	if(str->prefix("!", pos))
		return;

	b1, b2, b3: con 1<<iota;
	isb1 := b&b1;
	isb2 := b&b2;
	isb3 := b&b3;
	wasb1 := lastb&b1;
	wasb2 := lastb&b2;
	wasb3 := lastb&b3;

	if(isb1 && isb2 && !wasb2 && ~bspecial&b2) {
		s := tktextsel(w);
		if(s != nil) {
			tkclient->snarfput(s);
			tkcmd(sprint("%s delete sel.first sel.last", w));
		}
		bspecial |= b2;
	}
	else if(isb1 && isb3 && !wasb3 && ~bspecial&b3) {
		s := tktextsel(w);
		if(s != nil)
			tkcmd(sprint("%s delete sel.first sel.last", w));
		t := tkclient->snarfget();
		tkcmd(sprint("%s insert insert '%s", w, t));
		tkcmd(sprint("%s tag add sel {insert -%dc} insert", w, len t));
		bspecial |= b3;
	}
	else if(!isb1 && isb2) {
		tkcmd(sprint(".m post %d %d", int x-10, int y-5));
	}
	else if(wasb3 && !isb3 && !(isb1|isb2|wasb1|wasb2)) {
		path := tktextsel(w);
		if(path == nil) {
			t := tkcmd(w+sprint(" get {%s linestart} {%s lineend}", pos, pos));
			o := int str->splitstrr(pos, ".").t1;
			Break: con " \t\n{}()<>";
			for(si := o; si > 0 && !str->in(t[si-1], Break); si--)
				{}
			for(ei := o; ei < len t && !str->in(t[ei], Break); ei++)
				{}
			path = t[si:ei];
		}
		plumbpath(path);
	}
	else if(isb3 && !(isb1|isb2|wasb1|wasb2)) {
		if(!wasb3) {
			r := tkcmd(w+" tag ranges sel");
			if(r != nil)
				tkcmd(w+" tag remove sel "+r);
			tkcmd(sprint("%s mark set insert %s", w, pos));
		} else
			tkcmd(sprint("%s tkTextSelectTo %s %s", w, x, y));  # evil, using internal libtk function
	}

	if(b == 0)
		bspecial = 0;

	lastb = b;
}

eprevb: int;
especial: int;
eptr(m: string)
{
	l := sys->tokenize(m, " ").t1;
	b := int hd l;
	coord := hd tl l;
	pos := tkcmd(".e.input index "+coord);
	if(dflag) (sprint("eptr, b %d, coord %s, pos %s", b, coord, pos));
	if(str->prefix("!", pos))
		return;

	b1, b2, b3: con 1<<iota;
	isb1 := b&b1;
	isb2 := b&b2;
	isb3 := b&b3;

	if(isb1 && isb2 && (~eprevb&b2) && (~especial&b2)) {
		viorig = recordundo();
		tksetedit(Einsert);

		t: string;
		if(tkcmd(".e.input selection present") == "1") {
			t = tkcmd(".e.input get");
			s := tkcmd(".e.input index sel.first");
			e := tkcmd(".e.input index sel.last");
			t = t[int s:int e];
			tkcmd(".e.input delete sel.first sel.last");
		}
		tkclient->snarfput(t);
		especial |= b2;
	}
	else if(isb1 && isb3 && (~eprevb&b3) && (~especial&b3)) {
		viorig = recordundo();
		tksetedit(Einsert);

		t := tkclient->snarfget();
		if(tkcmd(".e.input selection present") != "1")
			s := e := int tkcmd(".e.input index insert");
		else {
			s = int tkcmd(".e.input index sel.first");
			e = int tkcmd(".e.input index sel.last");
			if(s > e)
				(s, e) = (e, s);
			tkcmd(sprint(".e.input delete %d %d", s, e));
		}
		tkcmd(sprint(".e.input insert %d '%s", s, t));
		tkcmd(sprint(".e.input selection range %d {%d +%dc}", s, s, len t));
		tkcmd(sprint(".e.input icursor %d", s));
		especial |= b3;
	}
	else if(!especial && !isb1 && (eprevb&b1))
		tkcmd(sprint(".e.input icursor %s", pos));

	eprevb = b;
	if(b == 0)
		especial = 0;
}

key(c: int)
{
	if(!ctlv) {
		done := 1;
		case c {
		kb->Pgup =>
			tkcmd(focus+" yview scroll -1 pages");
		kb->Pgdown =>
			tkcmd(focus+" yview scroll  1 pages");
		kb->Home =>
			tkcmd(focus+" see 1.0");
		kb->End =>
			tkcmd(sprint("%s scan mark 0 0; %s scan dragto -10000 -10000", focus, focus));
		Ctl+'v' =>
			ctlv = 1;
		16r7f or
		kb->Del =>
			tkcomplete(0);
			if(job != nil) {
				killgrp(job.pid);
				input = input.new();
				job = nil;
				io();
			}
		Ctl+'d' =>
			tkcomplete(0);
			input.add(array[0] of byte);
			io();
		* =>
			done = 0;
		}
		if(done)
			return;
	}
	case editmode {
	Einsert =>	keyins(c);
	Evi =>		keyvi(c);
	Ectlx =>	keyctlx(c);
	Eraw =>		keyraw(c);
	}
	ctlv = 0;
}

keyraw(c: int)
{
	input.add(sys->aprint("%c", c));
	io();
}

keyins(c: int)
{
if(dflag) warn(sprint("keyins %c/%#x", c, c));

	if(ctlv)
		return tkinsert(c);

	if(showcomplete)
		case c {
		kb->Esc =>
			tkcomplete(0);
			return;
		'\n' or
		Ctl+'x' or
		kb->Up or
		kb->Down =>
			tkcomplete(0);
		}

	case c {
	kb->Esc =>
		if(showcomplete)
			return tkcomplete(0);
		vikeys = nil;
		tksetedit(Evi);
	'\n' =>
		s := tkcmd(".e.input get");
		tkcmd(".e.input delete 0 end");
		histput(s);
		input.add(array of byte (s+"\n"));
		io();
	'\t' =>
		e := tkget();
		Break: con " \t\n{}()<>";
		while(e.si > 0 && !str->in(e.s[e.si-1], Break))
			e.si--;
		while(e.ei < len e.s && !str->in(e.s[e.ei], Break))
			e.ei++;
		complete(e.s, e.si, e.ei);
	Ctl+'x' =>
		tksetedit(Ectlx);
	kb->Up =>
		histprev();
	kb->Down =>
		histnext();
	* =>
		tkinsert(c);
	}
}

R: adt {
	change:		int;
	isrepeat:	int;
};

keyvi(c: int)
{
if(dflag) warn(sprint("keyvi %c/%#x", c, c));

	if(vikeys == nil)
		vikeys = ref Str;
	vikeys.s[len vikeys.s] = c;
	vikeys.i = 0;
	viorig = tkget();

	{
		r := ref R(1, 0);
		keyvi0(vikeys, ref *viorig, r);
		if(!r.isrepeat)
			viprevkeys = vikeys;
		if(r.change)
			recordundo();
		vikeys = nil;
	} exception ex {
	"more:*" =>
		{}
	"bad:*" =>
if(dflag) warn("bad: "+ex);
		vikeys = nil;
	}
}

keyvi0(k: ref Str, e: ref Str, r: ref R)
{
	k.i = 0;
if(dflag) warn(sprint("keyvi0, k %s, e %s", k.text(), e.text()));
	(rep1, rep1str) := k.readrep(1);
	case x := k.xget() {
	#R
	#~
	# '/' '?' 'n' 'N'
	'\n' =>
		s := tkcmd(".e.input get");
		tkcmd(".e.input delete 0 end");
		histput(s);
		input.add(array of byte (s+"\n"));
		io();
		tksetedit(Einsert);
	'i' or
	'I' or
	'a' or
	'A' or
	's' or
	'S' or
	'C' =>
		# repeat for insertions is not supported
		tksetedit(Einsert);
		case x {
		'I' =>	move("^", e);
		'a' =>	move("l", e);
		'A' =>	move("$", e);
		's' =>	vibuf = tkedit(e.i, e.i+rep1, "", e.i);
		'S' =>	vibuf = tkedit(0, len e.s, "", 0);
		'C' =>	vibuf = tkedit(e.i, len e.s, "", e.i);
		}
		r.change = x=='s'||x=='S'||x=='C';
	'c' =>
		if(motion(k, e, rep1, 'c'))
			vibuf = tkedit(e.si, e.ei, "", e.si);
		else {
			move("^", e);
			vibuf = tkedit(e.i, len e.s, "", e.i);
		}
		tksetedit(Einsert);
	'r' =>
		y := k.xget();
		n := min(rep1, len e.s-e.i);
		s: string;
		for(i := 0; i < n; i++)
			s[len s] = y;
		vibuf = tkedit(e.i, e.i+n, s, e.i+n);
	'y' =>
		if(motion(k, e, rep1, 'y'))
			vibuf = e.s[e.si:e.ei];
		else
			vibuf = e.s;
	'Y' =>
		vibuf = e.s[e.i:];
	'p' =>
		while(rep1--) {
			tkedit(e.i, e.i, vibuf, e.i);
			e = tkget();
		}
	'P' =>
		while(rep1--) {
			tkedit(e.i, e.i, vibuf, e.i+len vibuf);
			e = tkget();
		}
	'.' =>
		# for simpler implementation, only commands can be repeated, not insertions
		if(viprevkeys != nil)
			while(rep1--) {
				keyvi0(viprevkeys, e, r);
				e = tkget();
			}
		r.isrepeat = 1;
	'u' =>
		while(rep1-- && viundo != nil) {
			viredo = e::viredo;
			ee := hd viundo;
			viundo = tl viundo;
			tkedit(0, len e.s, ee.s, ee.i);
			e = ee;
		}
		r.change = 0;
	'r'-16r60 => # ^r
		while(rep1-- && viredo != nil) {
			viundo = e::viundo;
			ee := hd viredo;
			viredo = tl viredo;
			tkedit(0, len e.s, ee.s, ee.i);
			e = ee;
		}
		r.change = 0;
	'j' =>
		while(rep1-- && histnext())
			{}
	'k' =>
		while(rep1-- && histprev())
			{}
	'g' or
	'G' =>
		if(x == 'g')
			case k.xget() {
			'g' =>	{} # handled below
			* =>	raise "bad:bad 'g'";
			}

		n: ref Link[string];
		if(rep1str == nil) {
			if(x == 'g')
				n = history.first;
			else
				n = history.last;
		} else {
			for(l := history.first; l != nil && rep1-- > 0; l = l.next)
				{}
			n = l;
		}
		if(n != nil) {
			histcur = n;
			tkedit(0, len e.s, n.e, 0);
		}
		r.change = 0;
	'D' =>
		vibuf = tkedit(e.i, len e.s, "", e.i);
	'd' =>
		if(motion(k, e, rep1, 'd'))
			vibuf = tkedit(e.si, e.ei, "", e.si);
		else
			vibuf = tkedit(0, len e.s, "", 0);
	'x' =>
		vibuf = tkedit(e.i, min(len e.s, e.i+rep1), "", e.i);
	'X' =>
		i := max(0, e.i-rep1);
		vibuf = tkedit(i, e.i, "", i);
	* =>
		k.i = 0;
		motion(k, e, 1, 0);
		tkedit(0, 0, "", e.i);
		r.change = 0;
	}
	if(k.more())
		raise "remaining chars in keys";
}

Nonword: con "\u0001-\u0008\u000b-\u001f!-/:-@[-`{-\u007f";  # without whitespace
Word: con "^\u0001-/:-@[-`{-\u007f";  # without whitespace
Whitespace: con " \t\n";
Nonwhitespace := "^"+Whitespace;

movexword(e: ref Str)
{
	if(e.in(Word))
		e.skipcl(Word);
	else if(e.in(Nonword))
		e.skipcl(Nonword);
}

rmovexword(e: ref Str)
{
	if(e.in(Word))
		e.rskipcl(Word);
	else
		e.rskipcl(Nonword);
}

move(s: string, e: ref Str)
{
	k := ref Str (s, 0, 0, 0);
	motion(k, e, 1, 0);
	tkedit(0, 0, "", e.i);
}

motion(k: ref Str, e: ref Str, rep1, cmdchar: int): int
{
if(dflag) warn (sprint("motion, k %s, e %s, rep1 %d", k.text(), e.text(), rep1));
	(rep2, nil) := k.readrep(1);
	rep := rep1*rep2;

	case x := k.xget() {
	'^' =>	e.i = 0; e.findcl(Nonwhitespace);
	'0' =>	e.i = 0;
	'$' =>	e.i = len e.s;
	'w' =>
		while(rep--) {
			movexword(e);
			e.skipcl(Whitespace);
		}
	'W' =>
		while(rep--) {
			e.skipcl(Nonwhitespace);
			e.skipcl(Whitespace);
		}
	'e' =>
		while(rep--) {
			e.skipcl(Whitespace);
			movexword(e);
		}
	'E' =>
		while(rep--) {
			e.skipcl(Whitespace);
			e.skipcl(Nonwhitespace);
		}
	'b' =>
		while(rep--) {
			e.rskipcl(Whitespace);
			if(e.i > 0)
				e.i--;
			rmovexword(e);
		}
	'B' =>
		while(rep--) {
			e.rskipcl(Whitespace);
			e.rskipcl(Nonwhitespace);
		}
	'h' =>	e.i = max(0, e.i-rep);
	' ' or
	'l'=>	e.i = min(len e.s, e.i+rep);
	'|' =>	e.i = max(len e.s, rep);
	'%' =>
		pat: con "[{(]})";
		orig := e.i;
		if(!e.findcl(pat) && !e.rfindcl(pat)) {
			e.i = orig;
			warn("char not found");
			break;
		}
		c := e.char();
		for(i := 0; i < len pat && pat[i] != c; i++)
			{}
		oc := pat[(i+3)%6];
		delta := 1;
		if(i > 2)
			delta = -1;
		e.i += delta;
		n := 1;
		while(e.i >= 0 && e.i < len e.s) {
			y := e.char();
			if(y < 0)
				break;
			else if(y == c)
				n++;
			else if(y == oc)
				n--;
			if(n == 0)
				break;
			e.i += delta;
		}
		if(n != 0)
			e.i = orig;
	* =>
		if(cmdchar == x)
			return 0;
		raise "bad:not a motion command";
	}
	e.ei = e.i;
	if(e.si > e.ei)
		(e.si, e.ei) = (e.ei, e.si);
	return 1;
}

keyctlx(c: int)
{
if(dflag) warn(sprint("keyctlx %c/%#x", c, c));
	Ctl: con -16r60;
	case c {
	Ctl+'x' =>
		{} # xxx think of something useful
	'p' =>
		s := tkget().s;
		if(plumb(s)) {
			histput(s);
			tkedit(0, len s, "", 0);
		}
	'\n' =>
		spawn wmrun(tkget().s);
		s := tkget().s;
		histput(s);
		tkedit(0, len s, "", 0);
	'x' =>
		dflag = !dflag;
	'.' =>
		if(job == nil)
			run(sprint("{echo ..; ls -F} | mc -c %d\n", tktextwidth()));
	'F' =>
		if(job == nil)
			run(sprint("find -/f . | mc -c %d\n", tktextwidth()));
	'f' =>
		if(job == nil)
			run(sprint("find -/f -T .hg -N '*.dis' -N '*.sbl' . | mc -c %d\n", tktextwidth()));
	'd' =>
		if(job == nil)
			run(sprint("find -/F -T .hg . | mc -c %d\n", tktextwidth()));
	}
	tksetedit(Einsert);
}

tkinsert(c: int)
{
if(dflag) warn(sprint("tkinsert %c/%#x", c, c));
	e := tkget();
	i := ei := e.i;
	if(tkcmd(".e.input selection present") == "1") {
		i = int tkcmd(".e.input index sel.first");
		ei = int tkcmd(".e.input index sel.last");
	}
	tkedit(i, ei, sprint("%c", c), i+1);
}

cmd(s: string)
{
	case s {
	"clear" =>
		tkcmd(focus+" delete 1.0 end");
	"scroll" =>
		tkscrollset(!autoscroll);
	"markup" =>
		tkmarkupset(!markup);
	"send" =>
		r := tkcmd(focus+" tag ranges sel");
		if(r != nil)
			r = tkcmd(focus+" get "+r);
		else
			r = tkclient->snarfget();
		histput(r);
		ss := ref Str(r, 0, 0, 0);
		if(!ss.findcl("\n"))
			r += "\n";
		input.add(array of byte r);
		io();
	}
}

io()
{
	for(;;) {
		if(input.empty())
			break;

		if(job == nil) {
			d := input.take();
			if(len d == 0) {
				killgrp(pid());
				raise "fail:eof";
			}
			run(string d);
		} else if(!reads.empty()) {
			r := reads.take();
			d := input.first.e;
			if(r.n < len d) {
				input.first.e = d[r.n:];
				r.c <-= (d=d[:r.n], nil);
			} else
				r.c <-= (d=input.take(), nil);
			textio(Tin, d);
		} else
			break;
	}
	fg := "white";
	bg: string;
	if(job == nil)
		(fg, bg) = ("black", "#dddddd");
	else if(input.empty() && reads.empty())
		bg = "green";
	else if(!reads.empty())
		bg = "blue";
	else
		bg = "red";
	tkcmd(sprint(".e.io configure -fg %s -bg %s ", fg, bg));
}

runinit(argv: list of string)
{
	if(job != nil)
		raise "already running";

	s := str->quoted(argv);
	histput(s);
	text(Tcmd, s);
	l: list of ref Listnode;
	for(; argv != nil; argv = tl argv)
		l = ref Listnode(nil, hd argv)::l;
	l = rev(l);
	spawn new(l, nil, pidc := chan[1] of int);
	job = ref Job(<-pidc, s);
}

run(s: string)
{
	if(job != nil)
		raise "already running";

	(n, err) := sh->parse(s);
	if(err == nil && n == nil)
		return;

	text(Tcmd, s);

	spawn new(list of {ref Listnode(n, nil)}, err, pidc := chan[1] of int);
	job = ref Job(<-pidc, s);
}

new(args: list of ref sh->Listnode, err: string, pidc: chan of int)
{
	sys->pctl(Sys->NEWFD|sys->NEWPGRP, nil);
	pidc <-= pid();
	if(err != nil) {
		statusc <-= err;
		return;
	}
	fd0 := sys->open("/dev/cons", sys->OREAD);
	fd1 := sys->open("/dev/cons", sys->OWRITE);
	fd2 := sys->open("/dev/conserr", sys->OWRITE);
	if(fd0 == nil || fd1 == nil || fd2 == nil) {
		exc <-= sprint("open stdin/out/err: %r");
		return;
	}
	{
		nsh := shctxt.copy(1);
		err = nsh.run(args, 0);
		shctxt = nsh;
		statusc <-= err;
	} exception x {
	"fail:*" =>
		exc <-= x;
	}
}

wmrun(s: string)
{
	argv := "wm/run"::"--"::str->unquoted(s);
	wmrunv(argv);
}

wmrunv(argv: list of string)
{
	sh->run(drawctxt, argv);
}

plumb(s: string): int
{
	if(!plumbed)
		return 0;
	m := ref Msg("WmRun", "", workdir(), "text", "", array of byte s);
	return m.send() >= 0;
}

plumbpath(p: string)
{
	if(p == nil)
		return;
	(ok, d) := sys->stat(p);
	if(ok >= 0 && (d.mode & Sys->DMDIR)) {
		if(job != nil)
			spawn wmrunv(list of {"wm/run", "sh", "-c", sprint("load std; cd %q && {echo ..; ls -F} | mc", p)});
		else
			run(sprint("load std; cd %q && {echo ..; ls -F} | mc -c %d\n", p, tktextwidth()));
	} else {
		if(!plumb(p) && p[len p-1] == ':')
			plumb(p[:len p-1]);
	}
}

finished(t: int, s: string)
{
	text(t, s);
	text(Tprompt, "");
	tkclient->settitle(top, "run "+workdir());
	job = nil;
	io();
}

tags := array[] of {"in", "out", "err", "cmd", "status", "ok"};
tagcolors := array[] of {"#0000ff", "white", "orange", "lime", "red", "yellow"};
tktags(on: int)
{
	for(i := 0; i < len tags; i++) {
		c := "white";
		if(on)
			c = tagcolors[i];
		tkcmd(sprint(".out tag configure %s -fg %s -bg black", tags[i], c));
	}
	tkcmd(".out tag raise sel");
}

# text, all but stdin,stdout,stderr
textpre := array[] of {
"% ", "", "", "", "", "# ", "# ", "# ", "",
};
text(t: int, s: string)
{
	if(!markup && t > Terr)
		return;
	if(markup || t == Tprompt)
		s = textpre[t]+s;
	if(t != Tprompt && t != Tcmd && t != Tok)
		s += "\n";
	tktext(t, s);
}

# only stdin,stdout,stderr
leftovers := array[3] of array of byte;
textio(t: int, buf: array of byte)
{
	if(t == Tin && editmode == Eraw)
		return;
	i := t-Tin;
	if(len leftovers[i] == 0)
		s := string buf;
	else
		(s, leftovers[i]) = utf(leftovers[i], buf);
	tktext(t, s);
}

# a is leftover, b is new data.  make utf-8 string, and return remaining trailing data
utf(a, b: array of byte): (string, array of byte)
{
	if(len a == 0)
		c := b;
	else {
		c = array[len a+len b] of byte;
		c[:] = a;
		c[len a:] = b;
	}
	n := sys->utfbytes(c, len c);
	s := string c[:n];
	r := c[n:];
	if(len r == 0)
		r = nil;
	return (s, r);
}

tktext(t: int, s: string)
{
	scroll := autoscroll && tkcmd(".out dlineinfo {end -1c linestart}") != nil;
	tkcmd(".out insert end '"+s);

	tag := tagstrs[t];
	otags := tkcmd(sprint(".out tag names {end -%dc}", len s));
	if(otags != tag) {
		for(l := sys->tokenize(otags, " ").t1; l != nil; l = tl l)
			if((hd l) != "sel")
				tkcmd(sprint(".out tag remove %s {end -%dc} end", (hd l), len s));
		tkcmd(sprint(".out tag add %s {end -%dc} end", tag, len s));
	}

	n := int tkcmd(".out index end")-Histsize;
	if(0 && n > 0)
		tkcmd(sprint(".out delete 1.0 {%d.0-1c}", n));

	if(scroll)
		tkcmd(".out scan mark 0 0; .out scan dragto -10000 -10000");
}

histput(s: string)
{
	history.last.e = s;
	history.add("");
	histcur = history.last;
}

histprev(): int
{
	if(histcur.prev == nil)
		return 0;
	if(histcur == history.last)
		history.last.e = tkget().s;
	histcur = histcur.prev;
	tkcmd(".e.input delete 0 end; .e.input insert 0 '"+histcur.e);
	tkcmd(".e.input icursor 0");
	return 1;
}

histnext(): int
{
	if(histcur.next == nil)
		return 0;
	histcur = histcur.next;
	tkcmd(".e.input delete 0 end; .e.input insert 0 '"+histcur.e);
	c := "0";
	if(histcur.next == nil)
		c = "end";
	tkcmd(".e.input icursor "+c);
	return 1;
}

tkscrollset(on: int)
{
	autoscroll = on;
	l := "scroll";
	if(autoscroll)
		l = "no"+l;
	tkcmd(".m entryconfigure 1 -label "+l);
}

tkmarkupset(on: int)
{
	markup = on;
	tktags(markup);
	l := "markup";
	if(markup)
		l = "no"+l;
	tkcmd(".m entryconfigure 2 -label "+l);
}

editmodes: con " [xr";
tksetedit(m: int)
{
	editmode = m;
	tkcmd(sprint(".e.io configure -text {%c}", editmodes[m]));
}

tktextwidth(): int
{
	w := int tkcmd(".out cget -actwidth");
	cw := int tkcmd(".e.io cget -actwidth");
	return w/cw;
}

tktextsel(w: string): string
{
	r := tkcmd(w+" tag ranges sel");
	if(r != nil)
		r = tkcmd(w+" get "+r);
	return r;
}

tkedit(s, e: int, ns: string, ins: int): string
{
	r := tkcmd(sprint(".e.input get %d %d", s, e));
	tkcmd(sprint(".e.input delete %d %d; .e.input insert %d '%s", s, e, s, ns));
	tkcmd(sprint(".e.input icursor %d; .e.input see insert", ins));
	return r;
}

tkget(): ref Str
{
	e := tkcmd(".e.input get");
	i := int tkcmd(".e.input index insert");
	return ref Str(e, i, i, i);
}

tkcomplete(on: int)
{
	if(on && !showcomplete)
		(o, n, f) := (".t", ".c", ".c.complete");
	else if(!on && showcomplete) {
		(o, n, f) = (".c", ".t", ".out");
		tkcmd(".c.complete delete 1.0 end");
	} else
		return;
	tkcmd(sprint("pack %s -before %s -fill both -expand 1", n, o));
	tkcmd(sprint("pack forget %s", o));
	focus = f;
	showcomplete = on;
}

recordundo(): ref Str
{
	e := tkget();
	if(viorig != nil && e.s != viorig.s) {
		viundo = viorig::viundo;
		viredo = nil;
	}
	return e;
}

complete(s: string, si, ei: int)
{
	w := s[si:ei];
	if(str->prefix("'", w))
		w = w[1:];
	dir := names->dirname(w);
	file := names->basename(w, nil);
if(dflag) warn(sprint("completing in dir %q, for file prefix %q", dir, file));

	r: string;
	l := filematches(dir, file);
	if(dir != nil && dir[len dir-1] != '/')
		dir[len dir] = '/';
	if(l == nil)
		return;
	else if(len l == 1) {
		r = sprint("%q", dir+hd l);
		if(r[len r-1] != '/')
			r += " ";
		tkcomplete(0);
	} else {
		pre := hd l;
		hits := pre+"\n";
		for(l = tl l; l != nil; l = tl l) {
			mm := hd l;
			hits += mm+"\n";
			for(i := 0; i < len pre && i < len mm && pre[i] == mm[i]; i++)
				{}
			pre = pre[:i];
		}
		r = sprint("%q", dir+pre);
		tkcmd(".c.complete delete 1.0 end");
		tkcmd(".c.complete insert 1.0 '"+mc(hits));
		tkcomplete(1);
	}
	if(si != ei)
		tkedit(si, ei, r, si+len r);
}

filematches(p, f: string): list of string
{
	if(p == nil)
		p = ".";
	(a, n) := readdir->init(p, Readdir->NAME|Readdir->DESCENDING);
	if(n < 0)
		return nil;
	l: list of string;
	for(i := 0; i < len a; i++)
		if(str->prefix(f, a[i].name)) {
			s := a[i].name;
			if(a[i].mode & Sys->DMDIR)
				s += "/";
			l = s::l;
		}
	return l;
}

mc(s: string): string
{
	p0 := sys->pipe(fd0 := array[2] of ref Sys->FD);
	p1 := sys->pipe(fd1 := array[2] of ref Sys->FD);
	if(p0 < 0 || p1 < 0) {
		warn(sprint("pipe: %r"));
		return s;
	}

	pidc := chan[2] of int;
	spawn mcinit(list of {"mc", "-c", string tktextwidth()}, fd0[0], fd1[0], pidc);
	spawn mcwrite(fd0[1], s, pidc);
	fdx := fd1[1];
	fd0 = fd1 = nil;

	b := bufio->fopen(fdx, bufio->OREAD);
	r: string;
more:
	for(;;)
	case c := b.getc() {
	bufio->EOF =>
		break more;
	bufio->ERROR =>
		warn(sprint("read: %r"));
		r = s;
		break more;
	* =>
		r[len r] = c;
	}
	kill(<-pidc);
	kill(<-pidc);
	return r;
}

mcinit(args: list of string, fd0, fd1: ref Sys->FD, pidc: chan of int)
{
	pidc <-= pid();
	sys->pctl(sys->NEWFD, list of {fd0.fd, fd1.fd, 2});
	sys->dup(fd0.fd, 0);
	sys->dup(fd1.fd, 1);
	m := load Cmd "/dis/mc.dis";
	m->init(drawctxt, args);
}

mcwrite(fd: ref Sys->FD, s: string, pidc: chan of int)
{
	pidc <-= pid();
	sys->pctl(sys->NEWFD, list of {fd.fd, 2});
	if(sys->write(fd, d := array of byte s, len d) != len d)
		warn(sprint("mc write: %r"));
}


Str.more(x: self ref Str): int
{
	return x.i < len x.s;
}

Str.char(x: self ref Str): int
{
	if(!x.more())
		return -1;
	return x.s[x.i];
}

Str.xget(x: self ref Str): int
{
	if(!x.more())
		raise "more:";
	return x.s[x.i++];
}

Str.in(x: self ref Str, cl: string): int
{
	return x.more() && str->in(x.char(), cl);
}

Str.readrep(x: self ref Str, def: int): (int, string)
{
	if(!x.in("1-9"))
		return (def, "");
	s: string;
	s[len s] = x.xget();
	while(x.in("0-9"))
		s[len s] = x.xget();
	return (int s, s);
}

Str.previn(x: self ref Str, cl: string): int
{
	if(x.i <= 0)
		return 0;
	return str->in(x.s[x.i-1], cl);
}

Str.skipcl(x: self ref Str, cl: string): int
{
	while(x.in(cl))
		x.i++;
	return x.i;
}

Str.rskipcl(x: self ref Str, cl: string): int
{
	while(x.previn(cl))
		x.i--;
	return x.i;
}

Str.findcl(x: self ref Str, cl: string): int
{
	while(x.char() >= 0 && !x.in(cl))
		x.i++;
	return x.in(cl);
}

Str.rfindcl(x: self ref Str, cl: string): int
{
	while(x.i > 0 && !x.previn(cl))
		x.i--;
	return x.in(cl);
}

Str.text(x: self ref Str): string
{
	return sprint("Str(s %q, i %d, si %d, ei %d)", x.s, x.i, x.si, x.ei);
}


List[T].new(): ref List
{
	return ref List[T] (nil, nil);
}

List[T].add(l: self ref List, e: T)
{
	if(l.first == nil) {
		l.first = l.last = ref Link[T] (nil, nil, e);
	} else {
		l.last.next = ref Link[T] (l.last, nil, e);
		l.last = l.last.next;
	}
}

List[T].take(l: self ref List): T
{
	e := l.first.e;
	if(l.first == l.last)
		l.first = l.last = nil;
	else {
		l.first = l.first.next;
		l.first.prev = nil;
	}
	return e;
}

List[T].empty(l: self ref List): int
{
	return l.first == nil;
}


tkcmd(s: string): string
{
	r := tk->cmd(top, s);
	if(dflag > 1 || r != nil && r[0] == '!')
		warn(sprint("tkcmd: %q -> %q", s, r));
	return r;
}

tkup()
{
	tkcmd("update");
}

workdir(): string
{
	return sys->fd2path(sys->open(".", sys->OREAD));
}

del(l: list of int, v: int): list of int
{
	r: list of int;
	for(; l != nil; l = tl l)
		if(hd l == v) {
			l = tl l;
			break;
		} else
			r = hd l::r;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

min(a, b: int): int
{
	if(a < b)
		return a;
	return b;
}

max(a, b: int): int
{
	if(a > b)
		return a;
	return b;
}

pid(): int
{
	return sys->pctl(0, nil);
}

progctl(pid: int, s: string)
{
	sys->fprint(sys->open(sprint("/prog/%d/ctl", pid), sys->OWRITE), "%s", s);
}

kill(pid: int)
{
	progctl(pid, "kill");
}

killgrp(pid: int)
{
	progctl(pid, "killgrp");
}

warn(s: string)
{
	sys->fprint(sys->fildes(2), "%s\n", s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
