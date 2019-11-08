% This file is part of GOWEB Version 0.82 - September 2013
% Author Alexander Sychev
% GOWEB is based on program CWEB Version 3.64 - February 2002,
% Copyright (C) 1987, 1990, 1993, 2000 Silvio Levy and Donald E. Knuth
% It is distributed WITHOUT ANY WARRANTY, express or implied.
% Copyright (C) 2013 Alexander Sychev


% Permission is granted to make and distribute verbatim copies of this
% document provided that the copyright notice and this permission notice
% are preserved on all copies.

% Permission is granted to copy and distribute modified versions of this
% document under the conditions for verbatim copying, provided that the
% entire resulting derived work is given a different name and distributed
% under the terms of a permission notice identical to this one.

\def\v{\char'174} % vertical in typewriter font

@** Introduction in common code.  Next few sections contain code common
to both \.{GOTANGLE} and \.{GOWEAVE}, which roughly concerns the following
problems: character uniformity, input routines, error handling and
parsing of command line. 

@c
const (
@<Common constants@>
) @#

@<Definitions that should agree with \.{GOTANGLE} and \.{GOWEAVE}@> @#
@<Other definitions@> @#

@ \.{GOWEAVE} operates in three phases: First it inputs the source
file and stores cross-reference data, then it inputs the source once again and
produces the \TEX/ output file, and finally it sorts and outputs the index.
Similarly, \.{GOTANGLE} operates in two phases.
The global variable |phase| tells which phase we are in.

@<Other...@>= var phase int /* which phase are we in? */

@ There's an initialization procedure that gets both \.{GOTANGLE} and
\.{GOWEAVE} off to a good start. We will fill in the details of this
procedure later.

@c
func common_init() {
  @<Initialize pointers@>
  @<Set the default options common to \.{GOTANGLE} and \.{GOWEAVE}@>
  @<Scan arguments and open output files@>
}


@ A few character pairs are encoded internally as single characters,
using the definitions below. These definitions are consistent with
an extension of ASCII code originally developed at MIT and explained in
Appendix~C of {\sl The \TEX/book\/}; thus, users who have such a
character set can type things like \.{\char'32} and \.{\char'4} instead
of \.{!=} and \.{\&\&}. (However, their files will not be too portable
until more people adopt the extended code.). Actually, for \.{GOWEB} these 
codes is not significant, because \.{GOWEB} operates with UTF8 encoded sources.


@ @<Common constants@>=
and_and rune = 04 /* `\.{\&\&}'\,; corresponds to MIT's {\tentex\char'4} */
lt_lt rune = 020 /* `\.{<<}'\,;  corresponds to MIT's {\tentex\char'20} */
gt_gt rune = 021 /* `\.{>>}'\,;  corresponds to MIT's {\tentex\char'21} */
plus_plus rune = 0200 /* `\.{++}'\,;  corresponds to MIT's {\tentex\char'13} */
minus_minus rune = 0201 /* `\.{--}'\,;  corresponds to MIT's {\tentex\char'1} */
col_eq rune = 0207 /* `\.{:=}'\, */
not_eq rune = 032 /* `\.{!=}'\,;  corresponds to MIT's {\tentex\char'32} */
lt_eq rune = 034 /* `\.{<=}'\,;  corresponds to MIT's {\tentex\char'34} */
gt_eq rune = 035 /* `\.{>=}'\,;  corresponds to MIT's {\tentex\char'35} */
eq_eq rune = 036 /* `\.{==}'\,;  corresponds to MIT's {\tentex\char'36} */
or_or rune = 037 /* `\.{\v\v}'\,;  corresponds to MIT's {\tentex\char'37} */
dot_dot_dot rune = 0202 /* `\.{...}' */
begin_comment rune = '\t' /* tab marks will not appear */
and_not rune = 010  /*`\.{\&\^}'\,;*/
direct rune = 0203 /*`\.{<-}'\,;*/
begin_short_comment rune = 031 /* short comment */

@ Input routines.  The lowest level of input to the \.{GOWEB} programs
is performed by |input_ln|, which must be told which file to read from.
The return value of |input_ln| is nil if the read is successful and not nil 
otherwise (generally this means the file has ended). The |buffer| always contains
whole string without ending newlines.

@ @<Definitions...@>=
var buffer []rune /* where each line of input goes */
var loc int = 0 /* points to the next character to be read from the buffer */
var section_text[] rune /* name being sought for */
var id []rune /* slice pointed to the current identifier */

@ @<Import packages@>=
"io"
"bytes"

@
@c
/* copies a line into |buffer| or returns error */
func input_ln(fp *bufio.Reader) error {
	var prefix bool
	var err error
	var buf []byte
	var b []byte
	buffer = nil
	for buf, prefix, err = fp.ReadLine() 
		err == nil && prefix
		b, prefix, err = fp.ReadLine(){
		buf = append(buf, b...)
	}
	if len(buf) > 0 {
		buffer = bytes.Runes(buf)
	} 
	if err == io.EOF && len(buffer) != 0 {
		return nil
	}
	if err == nil && len(buffer) == 0 {
		buffer = append(buffer, ' ')
	}
	return err
}

@ Now comes the problem of deciding which file to read from next.
Recall that the actual text that \.{GOWEB} should process comes from two
|bufio.Reader|: a |file[0]|, which can contain possibly nested include
commands \.{@@i}, and a |change_file|, which might also contain
includes.  The |file[0]| together with the currently open include
files form a stack |file|, whose names are stored in a parallel stack
|file_name|.  The boolean |changing| tells whether or not we're reading
from the |change_file|.

The line number of each open file is also kept for error reporting and
for the benefit of \.{GOTANGLE}.

@ @<Import packages@>=
"bufio"

@ @<Definitions...@>=
var include_depth int /* current level of nesting */
var file [] *bufio.Reader /* stack of non-change files */
var change_file *bufio.Reader /* change file */
var file_name []string
  /* stack of non-change file names */
var change_file_name string = "/dev/null" /* name of change file */
var alt_file_name string /* alternate name to try */
var line[]int /* number of current line in the stacked files */
var change_line int/* number of current line in change file */
var change_depth int /* where \.{@@y} originated during a change */
var input_has_ended bool /* if there is no more input */
var changing bool /* if the current line is from |change_file| */

@ When |changing==false|, the next line of |change_file| is kept in
|change_buffer|, for purposes of comparison with the next
line of |file[include_depth]|. After the change file has been completely input, we
set |change_limit=0|, so that no further matches will be made.

@<Other...@>=
var change_buffer[]rune /* next line of |change_file| */

@ Procedure |prime_the_change_buffer|
sets |change_buffer| in preparation for the next matching operation.
Since blank lines in the change file are not used for matching, we have
|(change_limit==0 && !changing)| if and only if
the change file is exhausted. This procedure is called only when
|changing| is true; hence error messages will be reported correctly.

@c
func prime_the_change_buffer() {
	change_buffer = nil
	@<Skip over comment lines in the change file; |return| if end of file@>
	@<Skip to the next nonblank line; |return| if end of file@>
	@<Move |buffer| to |change_buffer|@>
}

@ @<Import packages@>=
"unicode"

@ While looking for a line that begins with \.{@@x} in the change file, we
allow lines that begin with \.{@@}, as long as they don't begin with \.{@@y},
\.{@@z}, or \.{@@i} (which would probably mean that the change file is fouled up).

@<Skip over comment lines in the change file...@>=
for true {
	change_line++
	if err := input_ln(change_file); err != nil { 
		return 
	}
	if len(buffer)<2 {
		continue
	}
	if buffer[0]!='@@' {
		continue
	}
	if unicode.IsUpper(buffer[1]) {
		buffer[1]=unicode.ToLower(buffer[1])
	}
	if buffer[1]=='x' {
		break
	}
	if buffer[1]=='y' || buffer[1]=='z' || buffer[1]=='i' {
		loc=2
		err_print("! Missing @@x in change file")
		@.Missing @@x...@>
	}
}

@ Here we are looking at lines following the \.{@@x}.

@<Skip to the next nonblank line...@>=
for true {
	change_line++
	if err := input_ln(change_file); err != nil {
		err_print("! Change file ended after @@x")
		@.Change file ended...@>
		return
	}
	if len(buffer) != 0 {
		break
	}
} 

@ @<Move |buffer| to |change_buffer|@>=
{
	change_buffer = buffer
	buffer = nil
}

@ The following procedure is used to see if the next change entry should
go into effect; it is called only when |changing| is false.
The idea is to test whether or not the current
contents of |buffer| matches the current contents of |change_buffer|.
If not, there's nothing more to do; but if so, a change is called for:
All of the text down to the \.{@@y} is supposed to match. An error
message is issued if any discrepancy is found. Then the procedure
prepares to read the next line from |change_file|.

When a match is found, the current section is marked as changed unless
the first line after the \.{@@x} and after the \.{@@y} both start with
either |'@@*'| or |'@@ '| (possibly preceded by whitespace).

This procedure is called only when the current line is nonempty.

@c
func if_section_start_make_pending(b bool) {
	for loc = 0; loc < len(buffer) && unicode.IsSpace(buffer[loc]); loc++ {}
	if len(buffer) >= 2 && buffer[0]=='@@' && (unicode.IsSpace(buffer[1]) || buffer[1]=='*') {
		change_pending=b
	}
}

@ We need a function to compare buffers of runes. It behaves like the classic |strcmp| function:
it returns -1, 0 or 1 if a left buffer is less, equal or more of a right buffer.
@c
func compare_runes(l []rune, r []rune) int{
	i := 0
	for ; i < len(l) && i < len(r) && l[i] == r[i]; i++ {}
	if i==len(r) {
		if i==len(l) { 
			return 0
		} else {
			return -1
		}
	} else {
		if i==len(l) {
			return 1
		} else if l[i]<r[i] {
			return -1 
		} else {
			return 1
		}
	}
	return  0
}

@
@c
/* switches to |change_file| if the buffers match */
func check_change() {
	n := 0 /* the number of discrepancies found */
	if compare_runes(buffer, change_buffer) != 0 { 
		return
	}
	change_pending=false
	if !changed_section[section_count]  {
		if_section_start_make_pending(true)
		if !change_pending {
			changed_section[section_count]=true
		}
	}
	for true {
		changing=true
		print_where=true
		change_line++
		if err := input_ln(change_file); err != nil {
			err_print("! Change file ended before @@y")
			@.Change file ended...@>
			change_buffer = nil
			changing=false
			return
		}
		if len(buffer)>1 && buffer[0]=='@@' {
			var xyz_code rune 
			if unicode.IsUpper(buffer[1]) {
				xyz_code = unicode.ToLower(buffer[1]) 
			} else {
				xyz_code = buffer[1]
			}
			@<If the current line starts with \.{@@y},
			report any discrepancies and |return|@>
		}
		@<Move |buffer| to |change_buffer|@>
		changing=false
		line[include_depth]++
		for input_ln(file[include_depth]) != nil { /* pop the stack or quit */
			if include_depth==0 {
				err_print("! GOWEB file ended during a change")
				@.GOWEB file ended...@>
				input_has_ended=true
				return
			}
			include_depth--
			line[include_depth]++
		}
		if compare_runes(buffer, change_buffer) != 0 {
			n++
		}
	}
}

@ @<Import...@>=
	"fmt"

@ @<If the current line starts with \.{@@y}...@>=
if xyz_code=='x' || xyz_code=='z' {
	loc=2
	err_print("! Where is the matching @@y?")
	@.Where is the match...@>
} else if xyz_code=='y' {
	if n>0 {
		loc=2
		err_print("! Hmm... %d of the preceding lines failed to match",n)
		@.Hmm... n of the preceding...@>
	}
	change_depth=include_depth
	return
}

@ The |reset_input| procedure, which gets \.{GOWEB} ready to read the
user's \.{GOWEB} input, is used at the beginning of phase one of \.{GOTANGLE},
phases one and two of \.{GOWEAVE}.

@c
func reset_input(){
	loc=0
	file = file[:0]
	@<Open input files@>
	include_depth=0
	line = line[:0]
	line = append(line, 0)
	change_line=0
	change_depth=include_depth
	changing=true
	prime_the_change_buffer()
	changing=!changing
	loc=0
	input_has_ended=false
}

@ The following code opens the input files.

@<Open input files@>=
if wf, err := os.Open(file_name[0]); err != nil {
	file_name[0] = alt_file_name
	if wf, err = os.Open(file_name[0]); err != nil {
		fatal("! Cannot open input file ", file_name[0])
		@.Cannot open input file@>
	} else {
		file = append(file, bufio.NewReader(wf))
	}
} else {
	file = append(file, bufio.NewReader(wf))
}
if cf, err := os.Open(change_file_name); err != nil {
	fatal("! Cannot open change file ", change_file_name)
	@.Cannot open change file@>
} else {
	change_file = bufio.NewReader(cf)
}

@ The |get_line| procedure is called when |loc>=len(buffer)|; it puts the next
line of merged input into the buffer and updates the other variables
appropriately.

This procedure returns |!input_has_ended| because we often want to
check the value of that variable after calling the procedure.

If we've just changed from the |file[include_depth]| to the |change_file|, or if
the |file[include_depth]| has changed, we tell \.{GOTANGLE} to print this
information in the \GO/ file by means of the |print_where| flag.

@<Common constants@>=
max_sections = 2000 /* number of identifiers, strings, section names;
  must be less than 10240 */


@ @<Defin...@>=
var section_count int32 /* the current section number */
var changed_section[max_sections] bool /* is the section changed? */
var change_pending bool /* if the current change is not yet recorded in
  |changed_section[section_count]| */
var print_where bool = false /* should \.{GOTANGLE} print line and file info? */

@
@c
func get_line() bool { /* inputs the next line */
restart:
	if changing && include_depth==change_depth {
		@<Read from |change_file| and maybe turn off |changing|@>
	}
	if !changing || include_depth>change_depth {
		@<Read from |file[include_depth]| and maybe turn on |changing|@>
		if changing && include_depth==change_depth {
			goto restart
		}
	}
	if input_has_ended {
		return false
	}
	loc=0
	if len(buffer) >= 2 && buffer[0]=='@@' && (buffer[1]=='i' || buffer[1]=='I') {
		loc=2
		for loc < len(buffer) && unicode.IsSpace(buffer[loc]) {
			loc++
		}
		if loc>=len(buffer) {
			err_print("! Include file name not given")
			@.Include file name ...@>
			goto restart
    	}

    	include_depth++ /* push input stack */
    	@<Try to open include file, abort push if unsuccessful, go to |restart|@>
	}
	return true
}

@ When an \.{@@i} line is found in the |file[include_depth]|, we must temporarily
stop reading it and start reading from the named include file.  The
\.{@@i} line should give a complete file name with or without
double quotes.
If the environment variable \.{GOWEBINPUTS} is set
\.{GOWEB} will look for include files in the colon-separated directories thus named, if
it cannot find them in the current directory. 
The remainder of the \.{@@i} line after the file name is ignored.

@<Import...@>=
"os" 
"strings"

@ @<Try to open inc...@>= {
	l := loc
	if buffer[loc]=='"' {
		loc++
		l++
		for loc<len(buffer) && buffer[loc]!='"' {
			loc++
		}
	} else {
		for loc < len(buffer) && !unicode.IsSpace(buffer[loc]) {
			loc++
		}
	}

	file_name = append(file_name, string(buffer[l:loc]))

	@.Include file name ...@>
	if f, err := os.Open(file_name[include_depth]); err == nil {
  		file = append(file, bufio.NewReader(f))
		line = append(line, 0)
		print_where=true
    	goto restart /* success */
	} 
	temp_file_name:=os.Getenv("GOWEBINPUTS")
	if len(temp_file_name)!=0 {
		@.Include file name ...@>
		for _, fn := range strings.Split(temp_file_name, ":") {
			file_name[include_depth] = fn + "/" + file_name[include_depth]
			if f, err := os.Open(file_name[include_depth]); err == nil {
				file = append(file, bufio.NewReader(f))
				line = append(line, 0)
				print_where=true
				goto restart /* success */
			}
		}
	}
	file_name = file_name[:include_depth]
	file = file[:include_depth]
	line = line[:include_depth]
	include_depth--
	err_print("! Cannot open include file")
	goto restart
}

@ @<Read from |file[include_depth]|...@>= {
	line[include_depth]++
	for input_ln(file[include_depth]) != nil { /* pop the stack or quit */
		print_where=true
		if include_depth==0 {
			input_has_ended=true
			break
		} else {
			file[include_depth] = nil
			file_name = file_name[:include_depth]
			file = file[:include_depth]
			line = line[:include_depth]
			include_depth--
			if changing && include_depth==change_depth {
				break
			}
			line[include_depth]++
		}
	}
	if !changing && !input_has_ended {
		if len(buffer)==len(change_buffer) {
			if buffer[0]==change_buffer[0] {
				if len(change_buffer)>0 {
					check_change()
				}
			}
		}
	}
}

@ @<Read from |change_file|...@>= {
	change_line++
	if input_ln(change_file) != nil {
		err_print("! Change file ended without @@z")
		@.Change file ended...@>
		buffer = append(buffer, []rune("@@z")...)
	}
	if len(buffer)>0 { /* check if the change has ended */
		if change_pending {
			if_section_start_make_pending(false)
			if change_pending {
				changed_section[section_count]=true
				change_pending=false
			}
  		}
		if len(buffer) >= 2 && buffer[0]=='@@' {
			if unicode.IsUpper(buffer[1]) {
				 buffer[1]=unicode.ToLower(buffer[1])
			}
			if buffer[1]=='x' || buffer[1]=='y' {
				loc=2
				err_print("! Where is the matching @@z?")
				@.Where is the match...@>
			} else if buffer[1]=='z' {
				prime_the_change_buffer() 
				changing=!changing 
				print_where=true
			}
		}
	}
}

@ At the end of the program, we will tell the user if the change file
had a line that didn't match any relevant line in |file[0]|.

@c
func check_complete(){
	if len(change_buffer) > 0 { /* |changing| is false */
   		buffer = change_buffer
		change_buffer = nil
		changing=true
		change_depth=include_depth
		loc=0
		err_print("! Change file entry did not match")
		@.Change file entry did not match@>
	}
}

@* Storage of names and strings.
Both \.{GOWEAVE} and \.{GOTANGLE} store identifiers, section names and
other strings in a large array |name_dir|, whose
elements are structures of type |name_info|, containing a slice of runes
with text information and other data.

@ @<Definitions that...@>=
type name_info struct{
  name []rune 
  @<More elements of |name_info| structure@>
} /* contains information about an identifier or section name */
type name_index int /* index into array of |name_info|s */
var name_dir[]name_info /* information about names */
var name_root int32

@ The names of identifiers are found by computing a hash address |h| and
then looking at strings of bytes signified by the indexes
|name_dir[hash[h]]|, |name_dir[hash[h]].llink|, |name_dir[name_dir[hash[h]].llink].llink|, \dots,
until either finding the desired name or encountering -1.

@<More elements of |name...@>=
llink int32

@ The hash table itself
consists of |hash_size| indexes, and is
updated by the |id_lookup| procedure, which finds a given identifier
and returns the appropriate index. The matching is done by the
function |names_match|, which is slightly different in
\.{GOWEAVE} and \.{GOTANGLE}.  If there is no match for the identifier,
it is inserted into the table.

@<Common constants@>=
hash_size = 353 /* should be prime */

@ @<Defini...@>=
var  hash[hash_size] int32 /* heads of hash lists */
var  h int32 /* index into hash-head array */

@ @<Initialize po...@>=
for i, _ := range hash {
	hash[i] = -1
}

@ Here is the main procedure for finding identifiers:

@c
/* looks up a string in the identifier table */
func id_lookup(@t\1@>@/
	id []rune,  /* string with id */
	t int32 @t\2@>/* the |ilk|; used by \.{GOWEAVE} only */) int32 { 
	@<Compute the hash code |h|@>
	@<Compute the name location |p|@>
	if p==-1 {
		@<Enter a new name into the table at position |p|@>
	}
	return p
}

@ A simple hash code is used: If the sequence of
character codes is $c_1c_2\ldots c_n$, its hash value will be
$$(2^{n-1}c_1+2^{n-2}c_2+\cdots+c_n)\,\bmod\,|hash_size|.$$

@<Compute the hash...@>=
h:=id[0]
for i := 1; i<len(id); i++ {
	h=(h+h+id[i]) % hash_size
}

@ If the identifier is new, it will be placed in the end of |name_dir|,
otherwise |p| will point to its existing location.

@<Compute the name location...@>=
p:=hash[h]
for p != -1 && !names_match(p,id, t) {
	p=name_dir[p].llink
}
if p==-1 {
	p:=int32(len(name_dir)) /* the current identifier is new */
	name_dir = append(name_dir, name_info{})
	name_dir[p].llink = -1
	init_node(p)
	name_dir[p].llink=hash[h]
	hash[h]=p /* insert |p| at beginning of hash list */
}

@ The information associated with a new identifier must be initialized
in a slightly different way in \.{GOWEAVE} than in \.{GOTANGLE}; both should
implement the \.{Initialization of a new identifier} section.

@ @<Enter a new name...@>= 
p=int32(len(name_dir)-1)
name_dir[p].name = append(name_dir[p].name, id...)
@<Initialization of a new identifier@>

@ The names of sections are stored in |name_dir| together
with the identifier names, but a hash table is not used for them because
\.{GOTANGLE} needs to be able to recognize a section name when given a prefix of
that name. A conventional binary search tree is used to retrieve section names,
with fields called |llink| and |rlink| . The root of this tree is stored 
in |name_root|.

@<More elements of |name...@>=
	ispref	bool /* prefix flag*/
	rlink	int32  /* right link in binary search tree for section names */


@ @<Initialize po...@>=
name_root=-1 /* the binary search tree starts out with nothing in it */

@ If |p| is a |name_dir| index variable, as we have seen,
|name_dir[p].name| is the area where the name
corresponding to |p| is stored.  However, if |p| refers to a section
name, the name may need to be stored in chunks, because it may
``grow'': a prefix of the section name may be encountered before
the full name.  Furthermore we need to know the length of the shortest
prefix of the name that was ever encountered.

We solve this problem by inserting |int32| at |name_dir[p].name|,
representing the length of the shortest prefix, when |p| is a
section name. Furthermore, the |ispref| field will be true
if |p| is a prefix. In the latter case, the name pointer
|p+1| will allow us to access additional chunks of the name:
The second chunk will begin at the name pointer |name_dir[p+1].llink|,
and if it too is a prefix (ending with blank) its |llink| will point
to additional chunks in the same way. Null links are represented by -1.

@c
func get_section_name(p int32) (dest []rune, complete bool) {
	q := p+1
	for p!=-1 {
		dest = append(dest,name_dir[p].name[1:]...)
		if name_dir[p].ispref {
			p=name_dir[q].llink
			q=p
		} else {
			p=-1
			q=-2
		}
	}
	complete=true
	if q != -2 {
		complete=false/* complete name not yet known */
	}	
	return
}

@
@c
func sprint_section_name(p int32) string {
	s,c:=get_section_name(p)
	str:=string(s)
	if !c {
		str+="..."/* complete name not yet known */
	}
	return str
}

@
@c
func print_prefix_name(p int32) (str string) {
	l := name_dir[p].name[0]
	str=fmt.Sprint(string(name_dir[p].name[1:]))
	if int(l)<len(name_dir[p].name) {
		str+="..."
	}
	return
}

@ When we compare two section names, we'll need a function 
to looking for prefixes and extensions too.

@<Common constants@>=
less = 0 /* the first name is lexicographically less than the second */
equal = 1 /* the first name is equal to the second */
greater = 2 /* the first name is lexicographically greater than the second */
prefix = 3 /* the first name is a proper prefix of the second */
extension = 4 /* the first name is a proper extension of the second */

@
@c
/* fuller comparison than |strcmp| */
func web_strcmp(@t\1@>@/
	j []rune, /* first string */
	k []rune @t\2@>/* second string */ ) int {
	i := 0
	for ; i < len(j) && i < len(k) && j[i] == k[i]; i++ {}
	if i==len(k) {
		if i==len(j) { 
			return equal
		} else {
			return extension
		}
	} else {
		if i==len(j) {
			return prefix
		} else if j[i]<k[i] {
			return less 
		} else {
			return greater
		}
	}
	return equal
}

@ Adding a section name to the tree is straightforward if we know its
parent and whether it's the |rlink| or |llink| of the parent.  As a
special case, when the name is the first section being added, we set the
``parent'' to |-1|.  When a section name is created, it has only one
chunk, which however may be just a prefix; the full name will
hopefully be unveiled later.  Obviously, prefix length starts
out as the length of the first chunk, though it may decrease later.

The information associated with a new node must be initialized
differently in \.{GOWEAVE} and \.{GOTANGLE}; hence the
|init_node| procedure, which is defined differently in \.{goweave.w}
and \.{gotangle.w}.

@c 
/* install a new node in the tree */ 
func add_section_name(@t\1@>@/
	par int32, /* parent of new node */
	c int, /* right or left? */
	name []rune, /* section name */
	ispref  bool @t\2@>/* are we adding a prefix or a full name? */) int32 {
	p:=int32(len(name_dir)) /* new node */
	name_dir = append(name_dir, name_info{})
	name_dir[p].llink = -1
	init_node(p)
	if ispref {
		name_dir = append(name_dir, name_info{})
		name_dir[p+1].llink = -1
		init_node(p+1)
	}
	name_dir[p].ispref = ispref
	name_dir[p].name = append(name_dir[p].name, int32(len(name))) /* length of section name */
	name_dir[p].name = append(name_dir[p].name, name...)
	name_dir[p].llink=-1
	name_dir[p].rlink=-1
	init_node(p)
	if par == -1 {
		name_root=p
	} else {
		if c == less {
			name_dir[par].llink=p
		} else {
			name_dir[par].rlink=p
		}
	}
	return p
}

@
@c
func extend_section_name(@t\1@>@/
	p int32, /* index name to be extended */
	text []rune, /* extension text */
	ispref bool @t\2@>/* are we adding a prefix or a full name? */) {
	q:=p+1
	for name_dir[q].llink!=-1 {
		q=name_dir[q].llink
	}
	np := int32(len(name_dir))
	name_dir[q].llink=np
	name_dir = append(name_dir, name_info{})
	name_dir[np].llink = -1 
	init_node(np)
	name_dir[np].name = append(name_dir[np].name, int32(len(text))) /* length of section name */
	name_dir[np].name = append(name_dir[np].name, text...)
	name_dir[np].ispref = ispref 
	
}

@ The |section_lookup| procedure is supposed to find a
section name that matches a new name, installing the new name if
it doesn't match an existing one. A ``match'' means that the new name
exactly equals or is a prefix or extension of a name in the tree.

@c
/* find or install section name in tree */
func section_lookup (@t\1@>@/
	name []rune, /* new name */
	ispref bool @t\2@>/* is the new name a prefix or a full name? */) int32 {
	c:=less/* comparison between two names*/
	p:=name_root /* current node of the search tree */
	var q int32 =-1 /* another place to look in the tree */
	var r int32 =-1 /* where a match has been found */
	var par int32 =-1 /* parent of |p|, if |r| is |NULL|; otherwise parent of |r| */
	name_len:=len(name)
  @<Look for matches for new name among shortest prefixes, complaining
        if more than one is found@>
  @<If no match found, add new name to tree@>
  @<If one match found, check for compatibility and return match@>
	return -1
}

@ A legal new name matches an existing section name if and only if it
matches the shortest prefix of that section name.  Therefore we can
limit our search for matches to shortest prefixes, which eliminates
the need for chunk-chasing at this stage.

@<Look for matches for new name among...@>=
for p != -1 { /* compare shortest prefix of |p| with new name */
	c=web_strcmp(name, name_dir[p].name[1:])
	if c==less || c==greater { /* new name does not match |p| */
		if r==-1 {/* no previous matches have been found */
			par=p
		}
		if c==less {
			p=name_dir[p].llink
		} else {
			p=name_dir[p].rlink
		}
	} else { /* new name matches |p| */
		if r!=-1 {  /* and also |r|: illegal */
			err_print("! Ambiguous prefix: matches <%s>\n and <%s>", print_prefix_name(p), print_prefix_name(r))
			@.Ambiguous prefix ... @>
			return 0 /* the unsection */
		}
    	r=p /* remember match */
    	p=name_dir[p].llink /* try another */
    	q=name_dir[r].rlink /* we'll get back here if the new |p| doesn't match */
	}
	if p==-1 {
		p=q
		q=-1 /* |q| held the other branch of |r| */
	}
}

@ @<If no match ...@>=
	if r==-1 { /* no matches were found */
		return add_section_name(par,c,name,ispref)
	}

@ Although error messages are given in anomalous cases, we do return the
unique best match when a discrepancy is found, because users often
change a title in one place while forgetting to change it elsewhere.

@<If one match found, check for compatibility and return match@>=
first, cmp := section_name_cmp(name,r)
switch cmp {
	/* compare all of |r| with new name */
	case prefix:
		if !ispref {
			err_print("! New name is a prefix of <%s>", sprint_section_name(r))
			@.New name is a prefix...@>
		} else if name_len<int(name_dir[r].name[0]){
			name_dir[r].name[0]=int32(len(name) - first)
		}
		fallthrough
	case equal: 
		return r
	case extension: 
		if !ispref || first<len(name) {
			extend_section_name(r,name[first:],ispref)
		}
		return r
	case bad_extension:
		err_print("! New name extends <%s>",sprint_section_name(r))
		@.New name extends...@>
		return r
	default: /* no match: illegal */
		err_print("! Section name incompatible with <%s>,\n which abbreviates <%s>", print_prefix_name(r), sprint_section_name(r))
		@.Section name incompatible...@>
		return r
}

@ The return codes of |section_name_cmp|, which compares a string with
the full name of a section, are those of |web_strcmp| plus
|bad_extension|, used when the string is an extension of a
supposedly already complete section name.  This function has a side
effect when the comparison string is an extension: It advances the
address of the first character of the string by an amount equal to
the length of the known part of the section name.

The name \.{@@<foo...@@>} should be an acceptable ``abbreviation''
for \.{@@<foo@@>}. If such an abbreviation comes after the complete
name, there's no trouble recognizing it. If it comes before the
complete name, we simply append a null chunk. This logic requires
us to regard \.{@@<foo...@@>} as an ``extension'' of itself.

@<Common constants@>=
bad_extension = 5

@
@c 
func section_name_cmp(@t\1@>@/
	name []rune, /* comparison string */
	r int32 @t\2@> /* section name being compared */) (int, int) {
	q:=r+1 /* access to subsequent chunks */
	var ispref bool /* is chunk |r| a prefix? */
	first := 0
	for true {
		if name_dir[r].ispref {
			ispref=true
			q=name_dir[q].llink
		} else {
			ispref=false
			q=-1
		}
		c:=web_strcmp(name,name_dir[r].name[1:])
		switch c {
			case equal: 
				if q==-1 {
					if ispref {
						return first + len(name_dir[r].name[1:]), extension /* null extension */
					} else {
						return first, equal
					}
				} else {
					if compare_runes(name_dir[q].name,name_dir[q+1].name) == 0 {
						return first, equal 
					} else { 
						return first, prefix
					}
				}
			case extension:
				if !ispref {
					return first, bad_extension
				}
				first += len(name_dir[r].name[1:])
				if q!=-1 {
					name = name[len(name_dir[r].name[1:]):]
					r=q
					continue
				}
				return first, extension
			default:
				return first, c
		}
	}
	return -2, -1
}

@* Reporting errors to the user.
A global variable called |history| will contain one of four values
at the end of every run: |spotless| means that no unusual messages were
printed; |harmless_message| means that a message of possible interest
was printed but no serious errors were detected; |error_message| means that
at least one error was found; |fatal_message| means that the program
terminated abnormally. The value of |history| does not influence the
behavior of the program; it is simply computed for the convenience
of systems that might want to use such information.

@<Common constants@>=
spotless = 0 /* |history| value for normal jobs */
harmless_message = 1 /* |history| value when non-serious info was printed */
error_message = 2 /* |history| value when an error was noted */
fatal_message = 3 /* |history| value when we had to stop prematurely */

@
@c 
func mark_harmless () {
	if history==spotless { 
		history=harmless_message 
	}
}

@
@c 
func mark_error() {
	history=error_message
}

@ @<Definit...@>=
var history int = spotless /* indicates how bad this run was */

@ The command `|err_print("! Error message")|' will report a syntax error to
the user, by printing the error message at the beginning of a new line and
then giving an indication of where the error was spotted in the source file.
Note that no period follows the error message, since the error routine
will automatically supply a period. A newline is automatically supplied
if the string begins with |"!"|.

@c
/* prints `\..' and location of error message */
func err_print(s string, a ...interface{}) { 
	var l int /* pointers into |buffer| */
	if len(s) > 0 && s[0] == '!' {
		fmt.Fprintf(os.Stdout,"\n\n"+s, a...)
	} else {
		fmt.Fprintf(os.Stdout,"\n"+s, a...)
	}
	if len(file)>0 && file[0]!=nil {
		@<Print error location based on input buffer@>
	}
	os.Stdout.Sync()
	mark_error()
}

@ The command `|warn_print("! Warning message")|' will report a warning to
the user, by printing the warning message at the beginning of a new line.
A newline is automatically supplied if the string begins with |"!"|.

@c
func warn_print(s string, a ...interface{}) { 
	if len(s) > 0 && s[0] == '!' {
		fmt.Fprintf(os.Stdout, "\n\n"+s, a...)
	} else {
		fmt.Fprintf(os.Stdout, "\n"+s, a...)
	}
	os.Stdout.Sync()
	mark_harmless()
}



@ The error locations can be indicated by using the global variables
|loc|, |line[include_depth]|, |file_name[include_depth]| and |changing|,
which tell respectively the first
unlooked-at position in |buffer|, the current line number, the current
file, and whether the current line is from |change_file| or |file[include_depth]|.
This routine should be modified on systems whose standard text editor
has special line-numbering conventions.
@^system dependencies@>

@<Print error location based on input buffer@>=
{
	if changing && include_depth==change_depth {
		fmt.Printf(". (change file %s:%d)\n", change_file_name, change_line)
	} else if include_depth==0 && len(line) > 0 { 
		fmt.Printf(". (%s:%d)\n", file_name[include_depth], line[include_depth])
	} else if len(line) > include_depth{
		fmt.Printf(". (include file %s:%d)\n", file_name[include_depth], line[include_depth])
	}
	l = len(buffer)
	if loc < l {
		l = loc
	}
	if l>0 {
		for k:=0; k<l; k++ {
			if buffer[k]=='\t' {
				fmt.Print(" ")
			} else { 
				fmt.Printf("%c", buffer[k]) // print the characters already read 
			}
		}
		
		fmt.Println()
		fmt.Printf("%*c", l, ' ')
	}
	fmt.Println(string(buffer[l:]))
	if len(buffer) > 0 && buffer[len(buffer)-1]=='|' {
		fmt.Print("|") /* end of \GO/ text in section names */
	}
	fmt.Print(" ") /* to separate the message from future asterisks */
}

@ When no recovery from some error has been provided, we have to wrap
up and quit as graciously as possible.  This is done by calling the
function |wrap_up| at the end of the code.

\.{GOTANGLE} and \.{GOWEAVE} have their own notions about how to
print the job statistics.


@ Some implementations may wish to pass the |history| value to the
operating system so that it can be used to govern whether or not other
programs are started. Here, for instance, we pass the operating system
a status of 0 if and only if only harmless messages were printed.
@^system dependencies@>

@c
func wrap_up() int {
	fmt.Print("\n")
	if show_stats() {
		print_stats() /* print statistics about memory usage */
	}
	@<Print the job |history|@>
	if history > harmless_message {
		return 1
	}
	return 0
}

@ @<Print the job |history|@>=
switch history {
	case spotless: 
		if show_happiness() { 
			fmt.Printf("(No errors were found.)\n")
		} 
	case harmless_message:
		fmt.Printf("(Did you see the warning message above?)\n")
	case error_message:
		fmt.Printf("(Pardon me, but I think I spotted something wrong.)\n")
	case fatal_message: 
		fmt.Printf("(That was a fatal error, my friend.)\n")
} /* there are no other cases */

@ When there is no way to recover from an error, the |fatal| subroutine is
invoked.

The two parameters to |fatal| are strings that are essentially
concatenated to print the final error message.

@c 
func fatal(s string, t string) {
	if len(s) != 0 {
		fmt.Print(s)
	}
	err_print(t)
	history=fatal_message
	os.Exit(wrap_up())
}

@* Command line arguments.
The user calls \.{GOWEAVE} and \.{GOTANGLE} with arguments on the command line.
These are either file names or flags to be turned off (beginning with |"-"|)
or flags to be turned on (beginning with |"+"|).
The following functions are for communicating the user's desires to the rest
of the program. The various file name variables contain strings with
the names of those files. Most of the 128 flags are undefined but available
for future extensions.

@
@c
func show_banner() bool {
	return flags['b'] /* should the banner line be printed? */
}

@
@c
func show_progress() bool {
	return flags['p'] /* should progress reports be printed? */
}

@
@c
func show_stats() bool {
	return flags['s'] /* should statistics be printed at end of run? */
}

@
@c
func show_happiness() bool { 
	return flags['h'] /* should lack of errors be announced? */
}

@ @<Defin...@>=
var go_file_name string /* name of |go_file| */
var tex_file_name string /* name of |tex_file| */
var idx_file_name string /* name of |idx_file| */
var scn_file_name string /* name of |scn_file| */
var flags[128]bool /* an option for each 7-bit code */

@ The |flags| will be initially zero. Some of them are set to~1 before
scanning the arguments; if additional flags are 1 by default they
should be set before calling |common_init|.

@<Set the default options common to \.{GOTANGLE} and \.{GOWEAVE}@>=
flags['b']=true
flags['h']=true
flags['p']=true

@ We now must look at the command line arguments and set the file names
accordingly.  At least one file name must be present: the \.{GOWEB}
file.  It may have an extension, or it may omit the extension to get |".w"| or
|".web"| added.  The \TEX/ output file name is formed by replacing the \.{GOWEB}
file name extension by |".tex"|, and the \GO/ file name by replacing
the extension by |".go"|, after removing the directory name (if any).

If there is a second file name present among the arguments, it is the
change file, again either with an extension or without one to get |".ch"|.
An omitted change file argument means that |"/dev/null"| should be used,
when no changes are desired.
@^system dependencies@>

If there's a third file name, it will be the output file.

@c
func scan_args() {
	dot_pos := -1 /* position of |'.'| in the argument */
	name_pos := 0 /* file name beginning, sans directory */
	found_web:=false
	found_change:=false
	found_out:=false
             /* have these names been seen? */
	flag_change := false

	for i := 1; i < len(os.Args); i++  {
		arg := os.Args[i]
		if (arg[0]=='-' || arg[0]=='+') && len(arg) > 1 {
			@<Handle flag argument@>
		} else {
			name_pos=0
			dot_pos=-1
			for j := 0; j < len(arg); j++ {
				if arg[j]=='.' { 
					dot_pos=j
				} else if arg[j]=='/' {
					dot_pos=-1
					name_pos=j+1
				} 
			}
			if !found_web {
				@<Make |file_name[0]|, |tex_file_name|, and |go_file_name|@>
			} else if !found_change {
				@<Make |change_file_name| from |fname|@>
			} else if !found_out {
				@<Override |tex_file_name| and |go_file_name|@>
			} else {
				@<Print usage error message and quit@>
			}
		}
	}
	if !found_web { 
		@<Print usage error message and quit@>
	}
}

@ We use all of |arg| for the |file_name[0]| if there is a |'.'| in it,
otherwise we add |".w"|. If this file can't be opened, we prepare an
|alt_file_name| by adding |"web"| after the dot.
The other file names come from adding other things
after the dot.  We must check that there is enough room in
|file_name[0]| and the other arrays for the argument.

@<Make |file_name[0]|...@>=
{
	if dot_pos==-1 {
		file_name = append(file_name, fmt.Sprintf("%s.w",arg))
	} else {
		file_name = append(file_name, arg)
		arg = arg[:dot_pos]/* string now ends where the dot was */
	}
	alt_file_name = fmt.Sprintf("%s.web", arg)
	tex_file_name = fmt.Sprintf("%s.tex",arg[name_pos:]) /* strip off directory name */
	idx_file_name = fmt.Sprintf("%s.idx",arg[name_pos:])
	scn_file_name = fmt.Sprintf("%s.scn",arg[name_pos:])
	go_file_name = fmt.Sprintf("%s.go",arg[name_pos:])
	found_web=true
}

@ @<Make |change_file_name|...@>=
{
	if arg[0] == '-' {
		found_change=true
	} else {
		if dot_pos==-2 {
			change_file_name = fmt.Sprintf("%s.ch",arg)
		} else {
			change_file_name = arg
		}
		found_change=true
	}
}

@ @<Override...@>=
{
	if dot_pos==-1 {
		tex_file_name = fmt.Sprintf("%s.tex",arg)
		idx_file_name = fmt.Sprintf("%s.idx",arg)
		scn_file_name = fmt.Sprintf("%s.scn",arg)
		go_file_name = fmt.Sprintf("%s.go",arg)
	} else {
		tex_file_name = arg
		go_file_name = arg
		if flags['x'] { /* indexes will be generated */
			dot_pos= -1
			idx_file_name = fmt.Sprintf("%s.idx",arg)
			scn_file_name = fmt.Sprintf("%s.scn",arg)
		}
	}
	found_out=true
}

@ @<Handle flag...@>=
{
	if arg[0]=='-' {
		flag_change=false
	} else  {
		flag_change=true
	}
	for i:=1;i<len(arg); i++ {
		flags[arg[i]]=flag_change
	}
}

@* Output. Here is the code that opens the output file:

@<Defin...@>=
var go_file io.WriteCloser /* where output of \.{GOTANGLE} goes */
var tex_file io.WriteCloser /* where output of \.{GOWEAVE} goes */
var idx_file io.WriteCloser /* where index from \.{GOWEAVE} goes */
var scn_file io.WriteCloser /* where list of sections from \.{GOWEAVE} goes */
var active_file io.WriteCloser /* currently active file for \.{GOWEAVE} output */

@ @<Scan arguments and open output files@>=
scan_args()
@<Try to open output file@>

@ |xisxdigit| checks for hexdecimal digits, that is, 
one of 0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F.

@c
func xisxdigit(r rune) bool {
	if unicode.IsDigit(r) {
		return true
	}
	if !unicode.IsLetter(r) {
		return false
	}
	r = unicode.ToLower(r)
	if r >= 'a' && r <='f' {
		return true
	}
	return false
}

@ The following code assigns values to the combinations \.{++},
\.{--}, \.{->}, \.{>=}, \.{<=}, \.{==}, \.{<<}, \.{>>}, \.{!=}, \.{||} and
\.{\&\&}, \.{...}.
The compound assignment operators (e.g., \.{+=}) are
treated as separate tokens.

@<Compress two-symbol operator@>=
switch c {
	case '/': 
		if nc=='*' {
			l := loc
			loc++
			if l <=len(buffer) {
				return begin_comment
			}
		} else if nc=='/' { 
			l := loc
			loc++
			if l <=len(buffer) {
				return begin_short_comment
			}
		}
	case '+': 
		if nc=='+' {
			l := loc
			loc++
			if l <=len(buffer) {
				return plus_plus
			}
		}	
	case '-': 
		if nc=='-' {
			l := loc
			loc++
			if l <=len(buffer) {
				return minus_minus
			}
		}
	case '.': 
		if nc=='.' && loc+1<len(buffer) && buffer[loc+1]=='.' {
			loc++
			l := loc
			loc++
			if l <=len(buffer) {
				return dot_dot_dot
			}
		}
	case '=': 
		if nc=='=' {
			l := loc
			loc++
			if l <=len(buffer) {
				return eq_eq
			}
		}
	case '>': 
		if nc=='=' {
			l := loc
			loc++
			if l <=len(buffer) {
				return gt_eq
			}
		} else if nc=='>' {
			l := loc
			loc++
			if l <=len(buffer) {
				return gt_gt
			}
		}
	case '<': 
		if nc=='<' {
			l := loc
			loc++
			if l <=len(buffer) {
				return lt_lt
			}
		} else if nc=='-' {
			l := loc
			loc++
			if l <=len(buffer) {
				return direct
			}
		} else if nc=='=' {
			l := loc
			loc++
			if l <=len(buffer) {
				return lt_eq
			}
		}
	case '&': 
		if nc=='&' {
			l := loc
			loc++
			if l <=len(buffer) {
				return and_and
			}
		} else if nc=='^' {
			l := loc
			loc++
			if l <=len(buffer) {
				return and_not
			}
		}
		
	case '|': 
		if nc=='|' {
			l := loc
			loc++
			if l <=len(buffer) {
				return or_or
			}
		}
	case '!':
		if nc=='=' {
			l := loc
			loc++
			if l <=len(buffer) {
				return not_eq
			}
		}
	case ':':
		if nc=='=' {
			l := loc
			loc++
			if l <=len(buffer) {
				return col_eq
			}
		}
}
