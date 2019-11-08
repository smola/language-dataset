#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "perliol.h"
#include "ppport.h"

#define UTF8_MAX_BYTES 4

static const U8 xs_utf8_sequence_len[0x100] = {
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x00-0x0F */
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x10-0x1F */
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x20-0x2F */
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x30-0x3F */
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x40-0x4F */
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x50-0x5F */
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x60-0x6F */
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0x70-0x7F */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 0x80-0x8F */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 0x90-0x9F */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 0xA0-0xAF */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 0xB0-0xBF */
    0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2, /* 0xC0-0xCF */
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, /* 0xD0-0xDF */
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, /* 0xE0-0xEF */
    4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0, /* 0xF0-0xFF */
};


typedef enum { STRICT_UTF8=0, ALLOW_SURROGATES=1, ALLOW_NONCHARACTERS=2, ALLOW_NONSHORTEST=4 } utf8_flags;


static STRLEN skip_sequence(const U8 *cur, const STRLEN len) {
	STRLEN i, n = xs_utf8_sequence_len[*cur];

	if (n < 1 || len < 2)
		return 1;

	switch (cur[0]) {
		case 0xE0: if ((cur[1] & 0xE0) != 0xA0) return 1; break;
		case 0xED: if ((cur[1] & 0xE0) != 0x80) return 1; break;
		case 0xF4: if ((cur[1] & 0xF0) != 0x80) return 1; break;
		case 0xF0: if ((cur[1] & 0xF0) == 0x80) return 1; /* FALLTROUGH */
		default:   if ((cur[1] & 0xC0) != 0x80) return 1; break;
	}

	if (n > len)
		n = len;
	for (i = 2; i < n; i++)
		if ((cur[i] & 0xC0) != 0x80)
			break;
	return i;
}

static void report_illformed(pTHX_ const U8 *cur, STRLEN len, bool eof) {
	static const char *hex = "0123456789ABCDEF";
	const char *fmt;
	char seq[UTF8_MAX_BYTES * 3];
	char *d = seq;

	if (eof)
		fmt = "Can't decode ill-formed UTF-8 octet sequence <%s> at end of file";
	else
		fmt = "Can't decode ill-formed UTF-8 octet sequence <%s>";

	while (len-- > 0) {
		const U8 c = *cur++;
		*d++ = hex[c >> 4];
		*d++ = hex[c & 15];
		if (len)
			*d++ = ' ';
	}
	*d = 0;
	Perl_croak(aTHX_ fmt, seq);
}

static void report_noncharacter(pTHX_ UV usv) {
	static const char *fmt = "Can't interchange noncharacter code point U+%"UVXf;
	Perl_croak(aTHX_ fmt, usv);
}

static STRLEN validate(pTHX_ const U8 *buf, const U8 *end, const int flags, PerlIO* handle) {
	const bool eof = PerlIO_eof(handle);
	const U8 *cur = buf;
	const U8 *end4 = end - UTF8_MAX_BYTES;
	STRLEN skip = 0;
	U32 v;

	while (cur < end4) {
		while (cur < end4 && *cur < 0x80)
			cur++;

	  check:
		switch (xs_utf8_sequence_len[*cur]) {
			case 0:
				goto illformed;
			case 1:
				cur += 1;
				break;
			case 2:
				/* 110xxxxx 10xxxxxx */
				if ((cur[1] & 0xC0) != 0x80)
					goto illformed;
				cur += 2;
				break;
			case 3:
				v = ((U32)cur[0] << 16)
				  | ((U32)cur[1] <<  8)
				  | ((U32)cur[2]);
				/* 1110xxxx 10xxxxxx 10xxxxxx */
				if ((v & 0x00F0C0C0) != 0x00E08080 ||
					/* Non-shortest form */
					v < 0x00E0A080)
					goto illformed;
				/* Surrogates U+D800..U+DFFF */
				if (!(flags & ALLOW_SURROGATES) && (v & 0x00EFA080) == 0x00EDA080)
					goto illformed;
				/* Non-characters U+FDD0..U+FDEF, U+FFFE..U+FFFF */
				if (!(flags & ALLOW_NONCHARACTERS) && v >= 0x00EFB790 && (v <= 0x00EFB7AF || v >= 0x00EFBFBE))
					goto noncharacter;
				cur += 3;
				break;
			case 4:
				v = ((U32)cur[0] << 24)
				  | ((U32)cur[1] << 16)
				  | ((U32)cur[2] <<  8)
				  | ((U32)cur[3]);
				/* 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx */
				if ((v & 0xF8C0C0C0) != 0xF0808080 ||
					/* Non-shortest form */
					v < 0xF0908080 ||
					/* Greater than U+10FFFF */
					v > 0xF48FBFBF)
					goto illformed;
				/* Non-characters U+nFFFE..U+nFFFF on plane 1-16 */
				if (!(flags & ALLOW_NONCHARACTERS) && (v & 0x000FBFBE) == 0x000FBFBE)
					goto noncharacter;
				cur += 4;
				break;
		}
	}
	
	if (cur < end) {
		if (cur + xs_utf8_sequence_len[*cur] <= end)
			goto check;
		skip = skip_sequence(cur, end - cur);
		if (eof || cur + skip < end)
			goto illformed;
	}
	return cur - buf;

  illformed:
	if (!skip)
		skip = skip_sequence(cur, end - cur);
	PerlIOBase(handle)->flags |= PERLIO_F_ERROR;
	report_illformed(aTHX_ cur, skip, eof);

  noncharacter:
	if (v < 0xF0808080)
		v = (v & 0x3F) | (v & 0x3F00) >> 2 | (v & 0x0F0000) >> 4;
	else
		v = (v & 0x3F) | (v & 0x3F00) >> 2 | (v & 0x3F0000) >> 4 | (v & 0x07000000) >> 6;
	PerlIOBase(handle)->flags |= PERLIO_F_ERROR;
	report_noncharacter(aTHX_ v);
}

typedef struct {
	PerlIOBuf buf;
	STDCHAR leftovers[UTF8_MAX_BYTES];
	size_t leftover_length;
	utf8_flags flags;
} PerlIOUnicode;

static struct {
	const char* name;
	size_t length;
	utf8_flags value;
} map[] = {
	{ STR_WITH_LEN("allow_surrogates"), ALLOW_SURROGATES },
	{ STR_WITH_LEN("allow_noncharacters"), ALLOW_NONCHARACTERS },
	{ STR_WITH_LEN("allow_nonshortest"), ALLOW_NONSHORTEST },
	{ STR_WITH_LEN("strict"), 0 },
	{ STR_WITH_LEN("loose"), ALLOW_SURROGATES | ALLOW_NONCHARACTERS | ALLOW_NONSHORTEST },
};

static utf8_flags lookup_parameter(pTHX_ const char* ptr, size_t len) {
	unsigned i;
	for (i = 0; i < sizeof map / sizeof *map; ++i) {
		if (map[i].length == len && memcmp(ptr, map[i].name, len) == 0)
			return map[i].value;
	}
	Perl_croak(aTHX_ "Unknown argument to :utf8_strict: %*s", (int)len, ptr);
}
static utf8_flags parse_parameters(pTHX_ SV* param) {
	STRLEN len;
	const char *begin, *delim;
	if (!param || !SvOK(param))
		return 0;

	begin = SvPV(param, len);
	delim = strchr(begin, ',');
	if(delim) {
		utf8_flags ret = 0;
		const char* end = begin + len;
		do {
			ret |= lookup_parameter(aTHX_ begin, delim - begin);
			begin = delim + 1;
			delim = strchr(begin, ',');
		} while (delim);
		if (begin < end)
			ret |= lookup_parameter(aTHX_ begin, end - begin);
		return ret;
	}
	else {
		return lookup_parameter(aTHX_ begin, len);
	}
}

#define line_buffered(flags) ((flags & (PERLIO_F_LINEBUF | PERLIO_F_CANWRITE)) == (PERLIO_F_LINEBUF | PERLIO_F_CANWRITE))

void PerlIOBase_flush_linebuf(pTHX) {
#ifdef dVAR
	dVAR;
#endif
	PerlIOl **table = &PL_perlio;
	PerlIOl *f;
	while ((f = *table)) {
		int i;
		table = (PerlIOl **) (f++);
		for (i = 1; i < 64; i++) {
			if (f->next && line_buffered(PerlIOBase(&(f->next))->flags))
				PerlIO_flush(&(f->next));
			f++;
		}
	}
}

static IV PerlIOUnicode_pushed(pTHX_ PerlIO* f, const char* mode, SV* arg, PerlIO_funcs* tab) {
	utf8_flags flags = parse_parameters(aTHX_ arg);
	if (PerlIOBuf_pushed(aTHX_ f, mode, arg, tab) == 0) {
		PerlIOBase(f)->flags |= PERLIO_F_UTF8;
		PerlIOSelf(f, PerlIOUnicode)->flags = flags;
		return 0;
	}
	return -1;
}

static IV PerlIOUnicode_fill(pTHX_ PerlIO* f) {
	PerlIOUnicode * const u = PerlIOSelf(f, PerlIOUnicode);
	PerlIOBuf * const b = &u->buf;
	PerlIO *n = PerlIONext(f);
	SSize_t avail;
	Size_t read_bytes = 0;
	STDCHAR *end;
	SSize_t fit;

	if (PerlIO_flush(f) != 0)
		return -1;
	if (PerlIOBase(f)->flags & PERLIO_F_TTY)
		PerlIOBase_flush_linebuf(aTHX);

	if (!b->buf)
		PerlIO_get_base(f);

	assert(b->buf);

	if (u->leftover_length) {
		Copy(u->leftovers, b->buf, u->leftover_length, STDCHAR);
		b->end = b->buf + u->leftover_length;
		read_bytes = u->leftover_length;
		u->leftover_length = 0;
	}
	else {
		b->ptr = b->end = b->buf;
	}
	fit = (SSize_t)b->bufsiz - (b->end - b->buf);

	if (!PerlIOValid(n)) {
		PerlIOBase(f)->flags |= PERLIO_F_EOF;
		return -1;
	}

	if (PerlIO_fast_gets(n)) {
		/*
		 * Layer below is also buffered. We do _NOT_ want to call its
		 * ->Read() because that will loop till it gets what we asked for
		 * which may hang on a pipe etc. Instead take anything it has to
		 * hand, or ask it to fill _once_.
		 */
		avail = PerlIO_get_cnt(n);
		if (avail <= 0) {
			avail = PerlIO_fill(n);
			if (avail == 0)
				avail = PerlIO_get_cnt(n);
			else {
				if (!PerlIO_error(n) && PerlIO_eof(n))
					avail = 0;
			}
		}
		if (avail > 0) {
			STDCHAR *ptr = PerlIO_get_ptr(n);
			const SSize_t cnt = avail;
			if (avail > fit)
				avail = fit;
			Copy(ptr, b->end, avail, STDCHAR);
			PerlIO_set_ptrcnt(n, ptr + avail, cnt - avail);
			read_bytes += avail;
		}
	}
	else {
		avail = PerlIO_read(n, b->end, fit);
		if (avail > 0)
			read_bytes += avail;
	}
	if (avail <= 0) {
		if (avail < 0 || (read_bytes == 0 && PerlIO_eof(n))) {
			PerlIOBase(f)->flags |= (avail == 0) ? PERLIO_F_EOF : PERLIO_F_ERROR;
			return -1;
		}
	}
	end = b->buf + read_bytes;
	b->end = b->buf + validate(aTHX_ (const U8 *)b->buf, (const U8 *)end, u->flags, n);
	if (b->end < end) {
		size_t len = b->buf + read_bytes - b->end;
		Copy(b->end, u->leftovers, len, char);
		u->leftover_length = len;
	}
	PerlIOBase(f)->flags |= PERLIO_F_RDBUF;
	
	return 0;
}

PERLIO_FUNCS_DECL(PerlIO_utf8_strict) = {
	sizeof(PerlIO_funcs),
	"utf8_strict",
	sizeof(PerlIOUnicode),
	PERLIO_K_BUFFERED|PERLIO_K_UTF8,
	PerlIOUnicode_pushed,
	PerlIOBuf_popped,
	PerlIOBuf_open,
	PerlIOBase_binmode,
	NULL,
	PerlIOBase_fileno,
	PerlIOBuf_dup,
	PerlIOBuf_read,
	PerlIOBuf_unread,
	PerlIOBuf_write,
	PerlIOBuf_seek,
	PerlIOBuf_tell,
	PerlIOBuf_close,
	PerlIOBuf_flush,
	PerlIOUnicode_fill,
	PerlIOBase_eof,
	PerlIOBase_error,
	PerlIOBase_clearerr,
	PerlIOBase_setlinebuf,
	PerlIOBuf_get_base,
	PerlIOBuf_bufsiz,
	PerlIOBuf_get_ptr,
	PerlIOBuf_get_cnt,
	PerlIOBuf_set_ptrcnt,
};

MODULE = PerlIO::utf8_strict

PROTOTYPES: DISABLE

BOOT:
	PerlIO_define_layer(aTHX_ &PerlIO_utf8_strict);

