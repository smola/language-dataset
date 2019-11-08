use v6;
use File::Spec::Unix;
class File::Spec::Win32 is File::Spec::Unix;

# Some regexes we use for path splitting
my $driveletter = regex { <[A..Z a..z]> ':' }
my $slash	= regex { '/' | '\\' }
my $UNCpath     = regex { [<$slash> ** 2] <-[\\\/]>+  <$slash>  [<-[\\\/]>+ | $] }
my $volume_rx   = regex { $<driveletter>=<$driveletter> | $<UNCpath>=<$UNCpath> }


method canonpath ($path)     { canon-cat($path)    }

method catdir(*@dirs) {
	return "" unless @dirs;
	return canon-cat( "\\", |@dirs ) if @dirs[0] eq "";
	canon-cat(|@dirs);
}
method splitdir($dir)        { $dir.split($slash)  }

method catfile(|c)           { self.catdir(|c)     }
method devnull               { 'nul'               }
method rootdir               { '\\'                }

method tmpdir {
	state $tmpdir;
	return $tmpdir if $tmpdir.defined;
	$tmpdir = self._firsttmpdir(
		%*ENV<TMPDIR>,
		%*ENV<TEMP>,
		%*ENV<TMP>,
		'SYS:/temp',
		'C:\system\temp',
		'C:/temp',
		'/tmp',
		'/',
		self.curdir
	);
}

method path {
	my @path = split(';', %*ENV<PATH>);
	@path».=subst(:global, q/"/, '');
	@path = grep *.chars, @path;
	unshift @path, ".";
	return @path;
}

method file-name-is-absolute ($path) {
	# As of right now, this returns 2 if the path is absolute with a
	# volume, 1 if it's absolute with no volume, 0 otherwise.
	given $path {
		when /^ [<$driveletter> <$slash> | <$UNCpath>]/ { 2 }
		when /^ <$slash> /                              { 1 }
		default 					{ 0 }
	}   #/
}

method split ($path as Str is copy) { 
	$path ~~ s[ <$slash>+ $] = ''                       #=
		unless $path ~~ /^ <$driveletter>? <$slash>+ $/;

	$path ~~ 
	    m/^ ( <$volume_rx> ? )
		( [ .* <$slash> ]? )
		(.*)
	     /;
	my ($volume, $directory, $file) = (~$0, ~$1, ~$2);
        $directory ~~ s/ <?after .> <$slash>+ $//;


	if all($directory, $file) eq '' && $volume ne '' {
		$directory = $volume ~~ /^<$driveletter>/
		             ?? '.' !! '\\';
	}
	$file = '\\'      if $directory eq any('/', '\\') && $file eq '';
	$directory = '.'  if $directory eq ''             && $file ne '';

	return ($volume,$directory,$file);
}

method join ($volume, $directory is copy, $file) { 
	$directory = '' if all($directory, $file) eq any('/','\\')
                        or $directory eq '.' && $file.chars;
	self.catpath($volume, $directory, $file);
}

method splitpath($path as Str, $nofile as Bool = False) { 

	my ($volume,$directory,$file) = ('','','');
	if ( $nofile ) {
		$path ~~ 
		    /^ (<$volume_rx>?) (.*) /;
		$volume    = ~$0;
		$directory = ~$1;
	}
	else {
		$path ~~ 
		    m/^ ( <$volume_rx> ? )
			( [ .* <$slash> [ '.' ** 1..2 $]? ]? )
			(.*)
		     /;
		$volume    = ~$0;
		$directory = ~$1;
		$file      = ~$2;
	}

	return ($volume,$directory,$file);
}

method catpath($volume is copy, $directory, $file) {

	# Make sure the glue separator is present
	# unless it's a relative path like A:foo.txt
	if $volume ne ''
	   and $volume !~~ /^<$driveletter>/
	   and $volume !~~ /<$slash> $/
	   and $directory !~~ /^ <$slash>/
		{ $volume ~= '\\' }
	if $file ne '' and $directory ne ''
	   and $directory !~~ /<$slash> $/
		{ $volume ~ $directory ~ '\\' ~ $file; }
	else 	{ $volume ~ $directory     ~    $file; }
}

method rel2abs ($path is copy, $base? is copy) {

	my $is_abs = self.file-name-is-absolute($path);

	# Check for volume (should probably document the '2' thing...)
	return self.canonpath( $path ) if $is_abs == 2;

	if $is_abs {
		# It's missing a volume, add one
		my $vol;
		$vol = self.splitpath($base)[0] if $base.defined;
		$vol ||= self.splitpath($*CWD)[0];
		return self.canonpath( $vol ~ $path );
	}

	if not defined $base {
	# TODO: implement _getdcwd call ( Windows maintains separate CWD for each volume )
	# See: http://msdn.microsoft.com/en-us/library/1e5zwe0c%28v=vs.80%29.aspx
		$base = Cwd::getdcwd( (self.splitpath: $path)[0] ) if defined &Cwd::getdcwd ;
		$base //= $*CWD ;
	}
	elsif ( !self.file-name-is-absolute( $base ) ) {
		$base = self.rel2abs( $base );
	}
	else {
		$base = self.canonpath( $base );
	}

	my ($path_directories, $path_file) = self.splitpath( $path, False )[1..2] ;

	my ($base_volume, $base_directories) = self.splitpath( $base, True ) ;

	$path = self.catpath( 
				$base_volume, 
				self.catdir( $base_directories, $path_directories ), 
				$path_file
				) ;

	return self.canonpath( $path ) ;
}


sub canon-cat ( $first is copy, *@rest ) {

	my $volumematch =
	     $first ~~ /^ ([   <$driveletter> <$slash>?
			    | <$UNCpath>
			    | [<$slash> ** 2] <-[\\\/]>+
			    | <$slash> ])?
			   (.*)
			/;
	my $volume = ~$volumematch[0];
	$first =     ~$volumematch[1];

	$volume.=subst(:g, '/', '\\');
	if $volume ~~ /^<$driveletter>/ {
		$volume.=uc;
	}
	elsif $volume.chars && $volume !~~ / '\\' $/ {
		$volume ~= '\\';
	}

	my $path = join "\\", $first, @rest.flat;

	$path ~~ s:g/ <$slash>+ /\\/;    #:: xx/yy --> xx\yy & xx\\yy --> xx\yy

	$path ~~ s:g/[ ^ | '\\']   '.'  '\\.'*  [ '\\' | $ ]/\\/;  #:: xx/././yy --> xx/yy

	if $*OS ne "MSWin32" {
		#netware or symbian ... -> ../..
		#unknown if .... or higher is supported
		$path ~~ s:g/ <?after ^ | '\\'> '...' <?before '\\' | $ > /..\\../; #::
	}

	#Perl 5 File::Spec does " xx\yy\..\zz --> xx\zz" here, but Win >= Vista
	# does symlinks linux style, so we're taking that out.

	$path ~~ s/^ '\\'+ //;		# \xx --> xx  NOTE: this is *not* root
	$path ~~ s/ '\\'+ $//;		# xx\ --> xx


	if ( $volume ~~ / '\\' $ / ) {
						# <vol>\.. --> <vol>\ 
		$path ~~ s/ ^			# at begin
			    '..'
			    '\\..'*		# and more
			    [ '\\' | $ ]	# at end or followed by slash
			 //;
	}

	if $path eq '' {		# \\HOST\SHARE\ --> \\HOST\SHARE
		$volume ~~ s/<?before '\\\\' .*> '\\' $ //;
		return $volume;
	}

	return $path ne "" || $volume ?? $volume ~ $path !! ".";
}
