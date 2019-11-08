
// Xenofarm Pike result parser
// By Martin Nilsson

inherit "../../result_parser.pike";

constant prog_id = "Xenofarm Pike result parser\n";

string result_dir = "/space/www/pikefarm/in/";
string work_dir = "/space/www/pikefarm/in_work/";
string web_format = "/space/www/pikefarm/results/%B/";
bool multi_project = true;

string sqlurl = "mysql://pikefarm@/pikefarm";
Sql.Sql xfdb = Sql.Sql(sqlurl);

void create() {
  foreach(ignored_warnings, string w)
    if(lower_case(w)!=w)
      werror("Warning %O not lower cased.\n");
  foreach(removed_warnings, string w)
    if(lower_case(w)!=w)
      werror("Warning %O not lower cased.\n");
}

// These warnings will not be counted as real warnings.
array(string) ignored_warnings = ({
  "configure: warning: found bash as /*.",
  "configure: warning: defaulting to --with-poll since the os is *.",
  "checking for irritating if-if-else-else warnings... *",
  "configure: warning: no login-related functions",
  "configure: warning: defaulting to unsigned int.",
  "configure: warning: configure script has been "
    "generated with autoconf 2.50 or later.",
  "configure: warning: cleaning the environment from autoconf 2.5x pollution",
  "configure: warning: found bash as /bin/bash.",
  " warning: failed to find the gtk gl widget.  ",
  "configure: warning: rntcc/rntcl/rnticl/rntecl detected.",
  "configure: warning: enabling dynamic modules for win32",
  "configure: warning: no gl or mesagl libraries, disabling gl support.",
  "cc: 1501-245 warning: hard ulimit has been reduced to less than "
    "rlim_infinity.  there may not be enough space to complete the "
    "compilation.",
  "configure: warning: debug malloc requires rtldebug. enabling rtldebug.",
  "warning added -lpthread to $libs!",
  "configure: warning: gnome 1.0.x not supported",
  "makefile:*: warning: overriding commands for target `depend'",
  "makefile:*: warning: ignoring old commands for target `depend'",
});

constant removed_warnings = ({
  "configure: warning: cleaning the environment from autoconf 2.5x pollution",
  "cc: 1501-245 warning: hard ulimit has been reduced to less than "
    "rlim_infinity.  there may not be enough space to complete the "
    "compilation.",
  "cc1: warning: -fpic ignored (all code is position independent)",
});

void parse_build_id(string fn, mapping res) {
  //TODO: pelix had a local hack to limit this to 65k  
  string file = Stdio.read_file(fn);
  if(!file || !sizeof(file)) {
    debug("No %s in result package.\n", fn);
    return;
  }

  int build_time;

  if( sscanf(file, "%*stime:%d", build_time)!=2 )
    return;

  Sql.Sql db = get_db();

  array err = catch {
    res->build = (int)db->query("SELECT id FROM build WHERE time=%d",
				build_time)[0]->id;
  };

  if(err) {
    debug("Build %d not found.\n", build_time);
    if(verbose)
      write(describe_backtrace(err));
    else
      werror(describe_backtrace(err));
  }
}

void parse_log(string fn, mapping res)
{
  ::parse_log(fn, res);

  // We sometimes get empty _core-files.
  Stdio.Stat st = file_stat("_core.txt");
  if(st && !st->size)
    rm("_core.txt");
  else
    mv("_core.txt", "core.txt");

  if(!res->tasks) return;
  foreach(res->tasks, array task) {

    if(task[0]=="post_build/verify") {
      // We don't consider verify passed if there was a leak.
      Stdio.File f = Stdio.File();
      if (f->open("verifylog.txt", "r")) {
	foreach(f->line_iterator();; string line) {
          if(has_value(line, "==LEAK==")) {
	    task[1] = "FAIL";
	    return;
	  }
	}
      }
      f->close();
    }

  }
}

int count_warnings(string fn) {

  // Highlight warnings.
  if(file_stat("compilelog.txt")) {
    Stdio.File f = Stdio.File();
    if (!f->open("compilelog.txt", "r")) return 0;

    Stdio.File out = Stdio.File();
    if (!out->open("makelog.html", "twc")) {
      f->close();
      return ::count_warnings(fn);
    }
    out->write("<pre><a href='#bottom'>Bottom of file</a>\n");

  newline:
    foreach(f->line_iterator(1); int n; string line) {
      string lc_line=lower_case(line);
      line = _Roxen.html_encode_string(line);
      
      if(has_value(lc_line, "warning") ||
	 has_value(lc_line, "(W)")) {
	foreach(removed_warnings, string remove)
	  if(glob(remove,lc_line)) {
	    continue newline;
	  }
	int ignore_warn;
	foreach(ignored_warnings, string ignore)
	  if(glob(ignore,lc_line)) {
	    ignore_warn = 1;
	    break;
	  }
	if (!ignore_warn) {
	  line = "<font style='background: #ff8080'>" + line + "</font>";
	}
      }
      out->write(line + "\n");
    }
    f->close();
    out->write("<a name='bottom'></a></pre>\n");
    out->close();
  }

  return ::count_warnings(fn);
}
