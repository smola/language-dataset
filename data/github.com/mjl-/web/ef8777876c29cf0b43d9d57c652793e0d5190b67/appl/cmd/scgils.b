implement Scgils;

include "sys.m";
	sys: Sys;
	print, sprint: import sys;
include "draw.m";
include "env.m";
	env: Env;
include "string.m";
	str: String;
include "template.m";
	template: Template;
	Form, Vars: import template;

Scgils: module {
	modinit:	fn(): string;
	init:	fn(nil: ref Draw->Context, nil: list of string);
};

modinit(): string
{
	sys = load Sys Sys->PATH;
	env = load Env Env->PATH;
	str = load String String->PATH;
	template = load Template Template->PATH;
	if(template == nil)
		return Template->PATH;
	template->init();
	return nil;
}

init(nil: ref Draw->Context, nil: list of string)
{
	if(sys == nil)
		modinit();

	path := env->getenv("PATH_INFO");
	if(infix("/../", path) || str->prefix("../", path) || suffix("/..", path) || path == ".." || path == "/")
		error("bad path");

	if(path != nil && path[0] != '/')
		path = "/"+path;
	
	vacpath := "/n/vac"+path;
	fd := sys->open(vacpath, Sys->OREAD);
	if(fd == nil)
		error(sprint("opening: %r"));
	(ok, d) := sys->fstat(fd);
	if(ok != 0)
		error(sprint("fstat: %r"));
	if(d.mode&sys->DMDIR) {
		if(path == nil || path[len path-1] != '/') {
			print("Status: 301 Moved permanently\r\nLocation: %s/\r\n\r\n", env->getenv("REQUEST_URI"));
			return;
		}
		form := ref Form("scgils");
		vars := ref Vars(("score", path[1:41])::("path", path[41:])::mkvars(d), nil);
		form.printv("header", vars);
		up := d;
		up.name = "..";
		form.print("entry", mkvars(up));
		for(;;) {
			(n, ds) := sys->dirread(fd);
			if(n < 0)
				error(sprint("dirread: %r"));
			if(n == 0)
				break;
			for(i := 0; i < n; i++)
				form.print("entry", mkvars(ds[i]));
		}
		form.printv("trailer", vars);
	} else {
		print("Status: 200 OK\r\n");
		ct := ctype(d.name);
		if(str->prefix("text/", ct))
			ct += "; charset=utf-8";
		print("Content-Type: %s\r\n", ct);
		print("Cache-control: max-age=86400\r\n\r\n");
		sys->stream(fd, sys->fildes(1), sys->ATOMICIO);
	}
}

ctype(name: string): string
{
	l := list of {
	(".tar.gz",	"application/x-tgz"),
	(".gz", 	"application/x-gzip"),
	(".tgz",	"application/x-tgz"),
	(".tar",	"application/x-tar"),
	(".gif",	"image/gif"),
	(".jpg",	"image/jpeg"),
	(".jpeg",	"image/jpeg"),
	(".png",	"image/png"),
	(".css",	"text/css"),
	(".html",	"text/html"),
	(".htm",	"text/html"),
	(".js",	"text/javascript"),
	(".asc",	"text/plain"),
	(".b",	"text/plain"),
	(".c",	"text/plain"),
	(".m",	"text/plain"),
	(".py",	"text/plain"),
	(".sh",	"text/plain"),
	(".cpp",	"text/plain"),
	(".log",	"text/plain"),
	(".conf",	"text/plain"),
	(".text",	"text/plain"),
	(".txt",	"text/plain"),
	(".diff",	"text/plain"),
	(".patch",	"text/plain"),
	(".bz2",	"application/x-bzip"),
	(".1",	"text/plain"),
	(".2",	"text/plain"),
	(".3",	"text/plain"),
	(".4",	"text/plain"),
	(".5",	"text/plain"),
	(".6",	"text/plain"),
	(".7",	"text/plain"),
	(".8",	"text/plain"),
	(".9",	"text/plain"),
	(".10",	"text/plain"),
	};

	for(; l != nil; l = tl l) {
		(ext, ctype) := hd l;
		if(suffix(ext, name))
			return ctype;
	}
	return "application/octet-stream";
}

mkvars(d: Sys->Dir): list of (string, string)
{
	name := d.name;
	if(d.mode&Sys->DMDIR)
		name += "/";
	return list of {
		("name", name),
		("uid", d.uid),
		("gid", d.gid),
		("mode", modestr(d.mode)),
		("mtime", string d.mtime),
		("length", string d.length),
	};
}

permstr(perm: int): string
{
	chars := array[] of {"r", "w", "x"};
	s := "";
	for(i := 2; i >= 0; i--)
		if(1 & (perm>>i))
			s += chars[2-i];
		else
			s += "-";
	return s;
}

modestr(mode: int): string
{
	s := "";
	case mode & (Sys->DMDIR|Sys->DMAUTH|Sys->DMAPPEND) {
	Sys->DMDIR =>		s += "d";
	Sys->DMAUTH =>		s += "A";
	Sys->DMAPPEND =>	s += "a";
	* =>			s += "-";
	}
	if(mode & Sys->DMEXCL)
		s += "l";
	else
		s += "-";
	s += permstr(mode>>6);
	s += permstr(mode>>3);
	s += permstr(mode);
	return s;
}

suffix(suf, s: string): int
{
	if(len suf > len s)
		return 0;
	for(i := 0; i < len suf; i++)
		if(s[len s - i - 1] != suf[len suf - i - 1])
			return 0;
	return 1;
}

infix(instr, s: string): int
{
	for(i := 0; i < len s - len instr + 1; i++)
		if(str->prefix(instr, s[i:]))
			return 1;
	return 0;
}

error(s: string)
{
	sys->fprint(sys->fildes(2), "%s\n", s);
	print("Status: 200 OK\r\n");
	print("Content-Type: text/plain\r\n\r\n");
	print("%s\n", s);
	raise "fail:"+s;
}
