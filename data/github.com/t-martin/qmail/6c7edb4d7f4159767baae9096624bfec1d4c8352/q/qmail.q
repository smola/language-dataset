// utility for sending HTML emails from kdb+_
// wraps around the linux 'sendmail' utility

// ===========================
// Sendmail wrapper
// ===========================
.mail.utilityexists:{not 0b~@[system;"which ",x," 2>/dev/null";{0b}]};
.mail.checkfile:{if[not x~key x:hsym x;'"file not found: ",.mail.hsym2str x]}

.mail.send:{[frm;to;sub;body;att]
  if[not .mail.utilityexists "sendmail"; '"'sendmail' not found"];
  if[not att~"";if[10h=type att;att:enlist att]];
  fn:hsym`$first system"mktemp qmail.XXXXXXXXXX";
  mail:.mail.template[frm;to;sub;body;att];
  fn 0: mail;
  @[system;"sendmail -t < ",1_string fn;{[x;y]hdel y;'"qmail error"}[;fn]];
  hdel fn;
  };

.mail.template0:{[frm;to;sub;body]
  enlist["From: ",frm],
  enlist["To: ",to],
  enlist["Subject: ",sub],
  enlist["Content-Type: text/html"],
  enlist["MIME-Version: 1.0"],
  .mail.header[],
  body,
  .mail.footer
  };

.mail.template:{[frm;to;sub;body;att]
  if[not count att where not null att,:();:.mail.template0[frm;to;sub;body]];
  boundary:"====",string[rand 0Ng],"====";
  enlist["From: ",frm],
  enlist["To: ",to],
  enlist["Subject: ",sub],
  enlist["Content-Type: multipart/mixed; boundary=\"",boundary,"\""],
  enlist["MIME-Version: 1.0"],
  enlist[""],
  enlist["--",boundary],
  enlist["Content-Type: text/html"],
  enlist[""],
  .mail.header[],
  body,
  .mail.footer,
  (raze {[att;boundary]
    enlist[""],
    enlist["--",boundary],
    enlist["Content-Transfer-Encoding: base64"],
    enlist["Content-Type: ",.mail.mimetype[att],"; name=",fn],
    enlist["Content-Disposition: attachment; filename=",fn:last "/"vs .mail.hsym2str att],
    enlist[""],
    .mail.encodefile[att]
  }[;boundary] each att),
  enlist["--",boundary,"--"]
  };


.mail.header:{[]
  raze(enlist "<html>";
  enlist "<body style=\"width:100%; marging:0; padding:0; font-size:15px;\">")
  };

.mail.footer:("</body>";"</html>");

.mail.base64encode:$[.z.K >= 3.6;76 cut .Q.btoa@;{
  c:count[x]mod 3;
  pc:count p:(0x;0x0000;0x00)c;
  b:.Q.b6 2 sv/: 6 cut raze 0b vs/: x,p;
  76 cut(neg[pc] _ b),pc#"="}];

.mail.encodefile:{
    .mail.checkfile x;
    X:read1 x;
    .mail.base64encode X
    };

.mail.mimetype:{[a]
  if[not .mail.utilityexists "file"; :"text/plain"];
  .mail.checkfile a;
  trim last ":" vs first @[system;"file --mime-type ",.mail.hsym2str a;{enlist ": text/plain"}]
  };

.mail.hsym2str:{[x] $[":"=first s:string x;1_s;s]}

// =========================
// HTML tag wrappers 
// =========================

.mail.string:{$[10h=abs type x;x;(type[x] in 0 98 99h) or (100h<type x) or 0h<type x;.Q.s1 x;string x]};

.mail.wrap:{"<",x,">",y,"</",(first " "vs (),x),">"};
.mail.ewrap:{enlist["<",x,">"],y,enlist"</",(first " "vs (),x),">"};

.mail.heading:{.mail.wrap[.mail.addstyle["h",x;`body];y]};
.mail.bold:{.mail.wrap[.mail.addstyle["b";`body];.mail.string x]};
.mail.italic:{.mail.wrap[.mail.addstyle["i";`body];.mail.string x]};

.mail.ifins:{$[""~y;y;x,":",y," "]};
.mail.colors:{[color;bg;size;text] 
  styledict:(`$("color";"background-color";"font-size";"display"))!(color;bg;size,"px";"inline");
  styledict:#[;styledict]where not ""~/:styledict;
  style:.mail.dict2css .mail.css.body[],styledict;
  .mail.wrap["p style=\"",style,"\"";.mail.string[text]]};

.mail.addcolor:{.mail.colors[x;"";"";y]};
.mail.size:{.mail.colors["";"";x;y]};
.mail.bgcolor:{.mail.colors["";x;"";y]};

.mail.url:{[url;txt] .mail.wrap[.mail.addstyle["a href=\"",url,"\"";`body];txt]};
.mail.setbookmark:{[id] "<a name=\"",id,"\"></a>"};
.mail.getbookmark:{[id;txt] .mail.url["#",id;txt]};

.mail.row:{.mail.ewrap["tr";.mail.wrap[x]each .mail.string each y]};
.mail.table0:{[t;alt]
  h:.mail.row[.mail.addstyle["th";`table`header];cols t];
  b:raze .mail.row'[.mail.addstyle["td"] each`table`row,/:$[alt;?[1=til[count t]mod 2;`odd;`even];count[t]#`even];flip value flip 0!t];
  .mail.ewrap[.mail.addstyle["table";`table`all];h,b]
  };

.mail.ztable:{.mail.table0[x;1b]};
.mail.table:{.mail.table0[x;0b]};

.mail.dict0:{[d;alt]
  b:raze .mail.row'[.mail.addstyle["td"] each`table`row,/:$[alt;?[1=til[count d]mod 2;`odd;`even];count[d]#`even];flip(key;value)@\:d];   
  .mail.ewrap["table class=\"altrowstable\"";b]
  };

.mail.dict:{.mail.dict0[x;0b]};
.mail.zdict:{.mail.dict0[x;1b]};

// ======================
// Colour scales
// ======================

///
//normalise numerics between 0 and 1 based on min/max value
.mail.color.normalize:{[low;high;x]0f | 1f & (x - low)%(high - low)};

///
//convert q hex value to html representation
.mail.color.hex2html:{"#",raze string x};

.mail.color.hsv2rgb:{[h;s;v]
  //hsv is easier to deal with but convert to RGB
  //for compatibility
  C:v*s;
  H:(h mod 360f)%60f;
  X:C * 1 - abs -1f + H mod 2;
  m:v-C;
  D:`s#0 1 2 3 4 5 6f!
    (1 2 0;2 1 0;0 1 2;0 2 1;2 0 1;1 0 2;0 0 0);
  RGB:m + (0f;C;X)D H;
  `byte$255*RGB
  };

.mail.color.hue_map:(!). flip (
  (`red; 0);
  (`orange; 30);
  (`yellow; 60);
  (`lime; 90);
  (`green; 120);
  (`turquoise; 150);
  (`cyan; 180);
  (`blue; 240);
  (`purple; 270);
  (`pink; 300);
  (`violet;330)
  );

.mail.color.colorize_mono:{[color;min_val;max_val;x]
  //fix the h + v and vary the saturation only
  s_values:.mail.color.normalize[min_val;max_val;x];
  .mail.color.hsv2rgb[$[-11h=type color;.mail.color.hue_map[color];color];;1f]each s_values
  };

.mail.color.colorize_mono_auto:{[color;x]
  .mail.color.colorize_mono[color;min x;max x;x]
  };

.mail.color.colorize_stereo:{[color_min;color_max;min_val;max_val;pivot_val;x]
  low:x<pivot_val;
  high_indices:where not low;
  low_indices:where low;
  low_colors:.mail.color.colorize_mono[color_min;pivot_val;min_val;x low_indices];
  high_colors:.mail.color.colorize_mono[color_max;pivot_val;max_val;x high_indices];
  @[;high_indices;:;high_colors] @[;low_indices;:;low_colors] count[x]#enlist 0x000000
  };

.mail.color.colorize_stereo_auto:{[color_min;color_max;x]
  //pivots around zero
  .mail.color.colorize_stereo[color_min;color_max;min x;max x;0f;x]
  };

// =======================
// CSS
// ========================
.mail.getstyle:{(.mail.css . (),x)[]}
.mail.addstyle:{x," style=\"",(.mail.dict2css .mail.getstyle[y]),"\""}
.mail.getcustomstyle:{$[99h;$[`qmailstylemeta in key x;x`qmailstylemeta;()!()];()!()]};
.mail.addtext:{.mail.wrap[.mail.addstyle["p";`body];x]}

.mail.css.body:{
  (!) . flip 2 cut(
  `$"font-family";"Verdana, Geneva, Sans-Serif";
  `$"color";"#2f4a5c")
  };

.mail.css.table.all:{
  .mail.css.body[],
  (!) . flip 2 cut(
  `$"font-family";"Verdana, Geneva, Sans-Serif";
  `$"font-size";"15px";
  `margin;"0 ";
  `padding;"3px";
  //`width;"100%";
  `$"line-height";"100%";
  `$"text-align";"left";
  `color;"#069";
  `$"border-width";"2px";
  `$"border-collapse";"collapse";
  `$"background-color";"#ffffff";
  `$"border-color";"#ffffff")
  };
.mail.css.table.header:{
  .mail.css.table.all[],
  (!) . flip 2 cut(
  `$"border-style"; "solid"; 
  `$"background-color";"#5473bf"; 
  `color;"#ffffff";  
  `$"border-color";"#ffffff"; 
  `$"border-width";"2px")
  };
.mail.css.table.row.all:{
  .mail.css.table.all[],
  (!) . flip 2 cut(
  `$"border-style";"solid"; 
  `$"border-width";"2px"; 
  `$"border-color";"#ffffff")
  };
.mail.css.table.row.odd:{
  .mail.css.table.row.all[],
  enlist[`$"background-color"]!enlist "#e6e6ff"
  };
.mail.css.table.row.even:{
  .mail.css.table.row.all[],
  enlist[`$"background-color"]!enlist "#ffffff"
  };

.mail.dict2css:{";"sv":"sv'flip(string@key@;value)@\:x}
