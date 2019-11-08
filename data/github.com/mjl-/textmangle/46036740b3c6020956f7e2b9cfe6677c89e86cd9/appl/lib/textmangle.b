implement Textmangle;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "lists.m";
	lists: Lists;
include "string.m";
	str: String;
include "textmangle.m";


init()
{
	sys = load Sys Sys->PATH;
	bufio = load Bufio Bufio->PATH;
	lists = load Lists Lists->PATH;
	str = load String String->PATH;
}


read(fd: ref Sys->FD): (list of string, string)
{
	b := bufio->fopen(fd, Bufio->OREAD);
	if(b == nil)
		return (nil, sprint("fopen: %r"));

	lines: list of string;
	for(;;) {
		l := b.gets('\n');
		if(l == nil)
			break;
		if(l[len l-1] == '\n')
			l = l[:len l-1];
		lines = l::lines;
	}
	lines = lists->reverse(lines);
	return (lines, nil);
}

toc(m: ref Mark): list of ref Mark.Head
{
	l := toc0(m, nil);
	return lists->reverse(l);
}

toc0(mm: ref Mark, l: list of ref Mark.Head): list of ref Mark.Head
{
	pick m := mm {
	Seq or List =>
		for(ml := m.l; ml != nil; ml = tl ml)
			l = toc0(hd ml, l);
		return l;
	Head =>
		return m::l;
	Quote =>
		return toc0(m.m, l);
	Text =>
		return l;
	Descr =>
		for(ml := m.l; ml != nil; ml = tl ml)
			l = toc0((hd ml).s, l);
		return l;
	};
}


parse(l: list of string): ref Mark.Seq
{
	return parseseq(l, 1);
}

parseseq(l: list of string, allowhead: int): ref Mark.Seq
{
	ml: list of ref Mark;
	while(l != nil) {
		s := hd l;
		if(s == nil) {
			l = tl l;
			continue;

		} else if(allowhead && ishead(l)) {
			level := len str->take(s, "#");
			s = str->drop(s[level:], " ");
			ml = ref Mark.Head(level, s)::ml;
			l = tl l;
			while(l != nil && hd l == nil)
				l = tl l;

		} else if(islisting(l)) {
			lm := ref Mark.List(nil);

			while(islisting(l)) {
				first := (hd l)[2:];
				l = tl l;

				if(islistitemindent(l)) {
					s: string;
					(l, s) = readlistitem(l);
					m := ref Mark.Text(first+"\n"+s);
					lm.l = m::lm.l;
					continue;
				}

				il: list of string;
				(l, il) = readindent(l, 0);
				if(il == nil) {
					# single-line text
					m := ref Mark.Text(first+"\n");
					lm.l = m::lm.l;
				} else {
					# complex item
					il = first::""::il;
					m := parseseq(il, 0);
					lm.l = m::lm.l;
				}
			}

			lm.l = lists->reverse(lm.l);
			ml = lm::ml;

		} else if(isquote(l)) {
			il: list of string;
			(l, il) = readindent(l, 1);

			m := parseseq(il, 0);
			ml = ref Mark.Quote(m)::ml;

		} else {
			if(isdescr(l)) {
				m := ref Mark.Descr(nil);
				while(l != nil && isdescr(l)) {
					name := hd l;
					l = tl l;

					il: list of string;
					(l, il) = readindent(l, 1);

					if(len il == 1) {
						# single line
						d := ref Descr(name, ref Mark.Text(hd il+"\n"));
						m.l = d::m.l;
					} else {
						# multi line
						dm := parseseq(il, 0);
						d := ref Descr(name, dm);
						m.l = d::m.l;
					}
				}
				m.l = lists->reverse(m.l);
				ml = m::ml;
				continue;
			}

			m := ref Mark.Text("");
			while(l != nil && hd l != nil) {
				m.s += (hd l)+"\n";
				l = tl l;
			}
			ml = m::ml;
		}
	}
	return ref Mark.Seq(lists->reverse(ml));
}

ishead(l: list of string): int
{
	if(l == nil)
		return 0;
	s := hd l;
	return str->prefix("#", s) && str->prefix(" ", str->drop(s, "#"));
}

isquote(l: list of string): int
{
	return l != nil && str->prefix("\t", hd l);
}

islisting(l: list of string): int
{
	return l != nil && (str->prefix("- ", hd l) || str->prefix("* ", hd l) || str->prefix("• ", hd l));
}

isdescr(l: list of string): int
{
	return l != nil && tl l != nil && str->prefix("\t", hd tl l);
}

islistitemindent(l: list of string): int
{
	r := l != nil && str->prefix("  ", hd l);
	return r;
}

readindent(l: list of string, allowfirstempty: int): (list of string, list of string)
{
	il: list of string;

	if(l != nil && hd l == "" && !allowfirstempty)
		return (l, nil);

	while(l != nil && (hd l == nil || (hd l)[0] == '\t')) {
		s := hd l;
		if(s != nil)
			s = s[1:];
		il = s::il;
		l = tl l;
	}
	if(il != nil && hd il == "")
		il = tl il;
	return (l, lists->reverse(il));
}

readlistitem(l: list of string): (list of string, string)
{
	s := "";
	for(; l != nil && str->prefix("  ", hd l); l = tl l)
		s += (hd l)[2:]+"\n";
	return (l, s);
}

split(s: string, sep: string): list of string
{
	l: list of string;
	e: string;
	while(s != nil) {
		(e, s) = str->splitstrl(s, sep);
		l = e::l;
		if(s != nil)
			s = s[len sep:];
	}
	l = lists->reverse(l);
	return l;
}

prefix(w, s: string, notrailnl: int): string
{
	r := "";
	for(l := split(s, "\n"); l != nil && !(notrailnl && len l == 1 && hd l == ""); l = tl l)
		if(hd l == "")
			r += "\n";
		else
			r += w+hd l+"\n";
	return r;
}

totext(mm: ref Mark): string
{
	pick m := mm {
	Seq =>
		s := "";
		for(l := m.l; l != nil; l = tl l)
			s += totext(hd l)+"\n";
		return s;
	Head =>
		s := "";
		for(i := 0; i < m.level; i++)
			s += "#";
		s += " "+m.s+"\n";
		return s;
	Quote =>
		return prefix("\t", totext(m.m), 1);
	Text =>
		return m.s;
	List =>
		s := "";
		for(l := m.l; l != nil; l = tl l)
			pick e := hd l {
			Text =>
				ll := split(e.s, "\n");
				s += "- "+hd ll+"\n";
				if(len ll > 1)
					for(ll = tl ll; ll != nil; ll = tl ll)
						s += "  "+hd ll+"\n";
			* =>
				t := totext(hd l);
				first: string;
				(first, t) = str->splitstrl(t, "\n");
				if(t != nil)
					t = t[1:];
				t = prefix("\t", t, 0);
				s += "- "+first+t;
			}
		return s;
	Descr =>
		s := "";
		for(l := m.l; l != nil; l = tl l)
			s += (hd l).name+"\n"+prefix("\t", totext((hd l).s), 0);
		return s;
	};
}

# xxx too unreadable for now
tostruct(mm: ref Mark): string
{
	pick m := mm {
	Seq =>
		s := "(seq\n";
		for(l := m.l; l != nil; l = tl l)
			s += "\t(item "+tostruct(hd l)+"\t)\n";
		s += ")\n";
		return s;
	Head =>
		return sprint("(head level %d %q)\n", m.level, m.s);
	Quote =>
		return sprint("(quote\n%s\n)\n", tostruct(m.m));
	Text =>
		return sprint("(text s %q)\n", m.s);
	List =>
		s := "(list\n";
		for(l := m.l; l != nil; l = tl l)
			s += "\t(listitem "+tostruct(hd l)+"\t)\n";;
		s += ")\n";
		return s;
	Descr =>
		s := "(descr\n";
		for(l := m.l; l != nil; l = tl l)
			s += sprint("\t(descritem %s\n\t%s)\n", (hd l).name, tostruct((hd l).s));
		s += ")\n";
		return s;
	};
}

htmlesc(s: string): string
{
	r := "";
	for(i := 0; i < len s; i++)
		case s[i] {
		'<' =>	r += "&lt;";
		'>' =>	r += "&gt;";
		'&' =>	r += "&amp;";
		'"' =>	r += "&quot;";
		* =>	r += s[i:i+1];
		}
	return r;
}

tohtml(m: ref Mark): string
{
	return tohtml0(m, 0, 0);
}

tohtmlpre(m: ref Mark): string
{
	return tohtml0(m, 0, 1);
}

tohtml0(mm: ref Mark, inline, pre: int): string
{
	pick m := mm {
	Seq =>  
		s := "";
		for(l := m.l; l != nil; l = tl l) {
			s += tohtml0(hd l, inline, pre)+"\n";
			inline = 0;
		}
		return s;
	Head =>
		s := htmlesc(m.s);
		if(pre)
			s = "<tt>"+s+"</tt>";
		return sprint("<h%d>%s</h%d>\n", m.level, s, m.level);
	Quote =>
		return sprint("<blockquote>\n%s</blockquote>\n", tohtml0(m.m, 0, pre));
	Text => 
		s := htmlesc(m.s);
		if(pre) {
			if(inline && s != nil && s[len s-1] == '\n' && len str->splitstrl(s, "\n").t1 <= 1) {
				s = "<tt>"+s[:len s-1]+"</tt>";
			} else
				s = "<pre>"+s+"</pre>";
		}
		
		if(inline)
			return s;
		if(!pre)
			s = sprint("<p>\n%s</p>\n", s);
		return s;
	List => 
		s := "<ul>\n";
		for(l := m.l; l != nil; l = tl l)
			s += sprint("\t<li>%s</li>\n", tohtml0(hd l, 1, pre));
		s += "</ul>\n";
		return s;
	Descr =>
		s := "<dl>\n";
		for(l := m.l; l != nil; l = tl l) {
			name := htmlesc((hd l).name);
			if(pre)
				name = "<tt>"+name+"</tt>"; 
			s += sprint("\t<dt>%s</dt>\n\t<dd>\n%s\n\t</dd>\n", name, tohtml0((hd l).s, 1, pre));
		}
		s += "</dl>\n";
		return s;
	}
}

# xxx
# need to convert . at start of lines (commands).  probably more chars.
troffesc(s: string): string
{
	return s;
}

totroff(mm: ref Mark): string
{
	return totroff0(mm, 0);
}

# xxx
# does not handle nested quotes/lists/descriptions
totroff0(mm: ref Mark, inline: int): string
{
	pick m := mm {
	Seq =>  
		s := "";
		for(l := m.l; l != nil; l = tl l)
			s += +totroff0(hd l, 0);
		return s;
	Head =>
		return sprint(".NH %d\n%s\n", m.level, troffesc(m.s));
	Quote =>
		return sprint(".RS\n%s.RE\n", totroff0(m.m, 0));
	Text => 
		if(inline)
			return troffesc(m.s);
		return sprint(".LP\n%s", troffesc(m.s));
	List => 
		s := "";
		for(l := m.l; l != nil; l = tl l) {
			pick e := hd l {
			Text =>	s += sprint(".IP \\(bu\n%s", totroff0(hd l, 1));
			* =>	s += sprint(".IP \\(bu\n.RS\n%s.RE\n", totroff0(hd l, 0));  # xxx in case of first elem as Text, need to show it without indent.
			}
		}
		return s;
	Descr =>
		s := "";
		for(l := m.l; l != nil; l = tl l)
			s += sprint(".IP \"%s\"\n.RS\n%s.RE\n", troffesc((hd l).name), totroff0((hd l).s, 1));
		return s;
	}
}

# xxx probably not complete...
latexesc(s: string): string
{
	esc := "$&%#_{}~^\\";
	needmath := "<>";

	r: string;
	for(i := 0; i < len s; i++) {
		if(str->in(s[i], needmath)) {
			r[len r] = '$';
			r[len r] = s[i];
			r[len r] = '$';
		} else {
			if(str->in(s[i], esc))
				r[len r] = '\\';
			r[len r] = s[i];
		}
	}
	return r;
}

tolatex(m: ref Mark): string
{
	return tolatexr(m);
}

tolatexr(mm: ref Mark): string
{
	pick m := mm {
	Seq =>  
		s := "";
		for(l := m.l; l != nil; l = tl l)
			s += tolatexr(hd l)+"\n";
		return s;
	Head =>
		# xxx level determines section/subsection, etc.
		# xxx need to check previous level.  generate \end's
		return sprint("\\section{%s}\n", latexesc(m.s));
	Quote =>
		# xxx which latex construct to use?
		return sprint("%s\n", tolatexr(m.m));
	Text => 
		return latexesc(m.s)+"\n\n";
	List => 
		s := "\\begin{itemize}\n";
		for(l := m.l; l != nil; l = tl l)
			s += sprint("\t\\item{} %s\n", tolatexr(hd l));
		s += "\\end{itemize}\n";
		return s;
	Descr =>
		s := "\\begin{description}\n";
		for(l := m.l; l != nil; l = tl l)
			s += sprint("\t\\item[%s] %s\n", (hd l).name, tolatexr((hd l).s));
		s += "\\end{description}\n";
		return s;
	}
}

say(s: string)
{
	if(dflag)
		sys->fprint(sys->fildes(2), "%s\n", s);
}
