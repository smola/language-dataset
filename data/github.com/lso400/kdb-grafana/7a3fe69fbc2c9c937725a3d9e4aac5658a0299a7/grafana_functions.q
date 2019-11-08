gf.metrics:`$();
gf.ms:00:00:00.001000000;
gf.timeserie:([]timestamp:`s#`timestamp$(); metric:`g#`$(); val:`float$());
timeserie:update date:`date$() from gf.timeserie;

k).gf.pctile:{avg x(<x)@_y*-1 0+#x,:()}
.gf.agg:`min`max`mean`median`count`sum`pct25`pct50`pct75`pct90`pct95`pct99!(min;max;avg;med;count;sum;.gf.pctile[;0.25];med;.gf.pctile[;0.75];.gf.pctile[;0.90];.gf.pctile[;0.95];.gf.pctile[;0.99])
.gf.unixms:{`long$(x-1970.01.01D)%`long$gf.ms}

.gf.hy:{[x]"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: close\r\nAccess-Control-Allow-Origin: *\r\nContent-Length: ",string[count x],"\r\n\r\n",x}

.z.ph:{[f;x] $[""~x[0]; :.gf.hy[""]; f[x]]}.z.ph

.z.pp:{
  if[x[0] like "query *";  :.gf.query[x 0]];
  if[x[0] like "search *"; :.gf.search[x 0]];
 }

.gf.parse:{[x]
  target: gf.delimiter vs  x;
  $[(`$last[target]) in key .gf.agg; 
    [agg:`$last[target]; target:-1_target];
     agg:`$()
  ];
  if[not "*"~last last target; target[-1+count target],:"*"];
  metric: gf.delimiter vs' string gf.metrics;
  (gf.metrics where all each(count[target]#'metric) like'\: target; agg)
 }

.gf.search:{[x]
  x:.j.k 7_x;
  .gf.hy[.j.j first .gf.parse x`target]
 }

.gf.query:{[x]
  x:.j.k 6_x;
  opt:()!();
  opt[`range]:         "Z"$x[`range;`from`to];
  opt[`intervalMs]:    x`intervalMs;
  opt[`maxDataPoints]: `long$x`maxDataPoints;
  res: raze .gf.query2[opt;]each x[`targets];
  .gf.hy[.j.j res] 
 }

.gf.query2:{[x;y]
  metAgg:.gf.parse y[`target];
  metrics: first metAgg;
  r:.gf.select[x;metrics];
  .gf.aggregate[r;x;last metAgg;] each metrics
 }

.gf.select:{[x;m]
  disk:delete date from select from timeserie    where date within `date$x[`range], metric in m, timestamp within x`range;
  mem :                 select from gf.timeserie where                              metric in m, timestamp within x`range;
  disk,mem
 }

.gf.aggregate:{[r;x;agg;m]
  r:$[agg~`$(); 
    select [x`maxDataPoints]                                                                     from r where metric=m;
    select [x`maxDataPoints] .gf.agg[agg] val by (`timespan$(x`intervalMs)*gf.ms) xbar timestamp from r where metric=m
  ];
  `target`datapoints!(` sv m,agg;flip exec (val;.gf.unixms timestamp) from r)
 }

.gf.addMetric:{[x]gf.metrics:asc gf.metrics union x}

.gf.upd:{[x;y]
  `gf.timeserie insert (.z.p;x;y);
  .gf.addMetric x
 }

.gf.end:{[]
  .gf.save each exec distinct timestamp.date from gf.timeserie;
  update `s#timestamp, `g#metric from delete from `gf.timeserie;
  .gf.reload[]
 }

.gf.save:{[x]
  new:.Q.en[`:.;select from gf.timeserie where timestamp.date=x];
  old:delete date from select from timeserie where date=x;
  data:update `p#metric from `metric`timestamp xasc old,new;
  (` sv `:.,(`$string x),`timeserie`) set data
 }

.gf.reload:{[]
  system"l .";
  .gf.addMetric exec @[value;metric;`$()] from select distinct metric from timeserie
 }