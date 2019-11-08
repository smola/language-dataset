Synergy: module
{
	PATH:	con "/dis/lib/synergy.dis";

	MAJOR:	con 1;
	MINOR:	con 3;

	init:	fn();

	Ttext, Tbmp, Thtml, Tmax: con iota;

	Msg: adt {
		pick {
		Thello =>
			major, minor: int;
		Rhello =>
			major, minor: int;
			name: string;
		Noop or Close =>
		Enter =>
			x, y, seq, mod:	int;
		Leave =>
		Grabclipboard =>
			id, seq:	int;
		Screensaver =>
			started:	int;
		Resetoptions or Infoack or Keepalive =>
		Keydown =>
			id, modmask, key:	int;
		Keydown_10 =>
			id, modmask:	int;
		Keyrepeat =>
			id, modmask, repeats, key:	int;
		Keyrepeat_10 =>
			id, modmask, repeats:	int;
		Keyup =>
			id, modmask, key:	int;
		Keyup_10 =>
			id, modmask:	int;
		Mousedown or Mouseup =>
			id:	int;
		Mousemove or Mouserelmove =>
			x, y:	int;
		Mousewheel =>
			x, y:	int;
		Mousewheel_10 =>
			y:	int;
		Clipboard =>
			id, seq:	int;
			l:	list of ref (int, array of byte);
		Info =>
			topx, topy, width, height: int;
			warpsize:	int;  # obsolete
			x, y:	int;
		Setoptions =>
			options:	int;	# xxx
		Getinfo =>
		Incompatible =>
			major, minor:	int;
		Busy or Unknown or Bad =>
		}

		packedsize:	fn(m: self ref Msg): int;
		pack:		fn(m: self ref Msg, d: array of byte): string;
		unpack:		fn(d: array of byte): (ref Msg, string);
		text:		fn(m: self ref Msg): string;
	};

	Session: adt {
		fd:	ref Sys->FD;
		major, minor:	int;

		new:	fn(fd: ref Sys->FD, name: string): (ref Session, string);
		readmsg:	fn(s: self ref Session): (ref Msg, string);
		writemsg:	fn(s: self ref Session, m: ref Msg): string;
	};
};
