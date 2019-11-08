.util.LoadDir `:lib/socket;
.util.Load `:lib/ipc/kdb.q;
.util.Load `:lib/cfg/cfg.q;
.util.Load `:lib/fix/socket.q;
.util.Load `:lib/timer/timer.q;
.util.Load `:lib/stp/bitstamp/trading.q;
.util.Load `:lib/stp/bitstamp/md.q;

SetupLogs:{[]
  prefix:":",.cfg.Config.LogPath,"/",string[.z.d],"_",.fix.SenderCompID,"-",.fix.TargetCompID;
  .fix.inboundLogFile: `$ prefix,"-IN.txt";
  .fix.outboundLogFile: `$ prefix,"-OUT.txt";
  .fix.inbound:hopen .fix.inboundLogFile;
  .fix.outbound:hopen .fix.outboundLogFile
  };

Start:{[]

  .fix.SenderCompID:.cfg.Config.SenderCompID;
  .fix.TargetCompID:.cfg.Config.TargetCompID;

  SetupLogs[]; / open logfiles

  if[not .socket.Connect[.cfg.Config.Host;"I"$.cfg.Config.Port];
    .log.Err "Connection failed, exitting";
    exit 1
    ];
  //
  .log.Hlt "Connect succeeded";
  //
  .fix.IsConnected:1b;
  // send logon
  Send.LOGON[map (553h;.cfg.Config.Username;
                  554h;.cfg.Config.Password;
                  108h;.cfg.Config.HeartBeatInterval;
                  98h; "0";  // EncryptMethod = None
                  141h;"Y")] // ResetSeqNumFlag = Yes
  // subscribe to crosses
  .cfg.Config.Subscriptions:`$ "," vs .cfg.Config.Subscriptions;
  Subscribe each .cfg.Config.Subscriptions
  };

init:{[ARGS]
  opts::.Q.opt ARGS;
  if[not `config in key opts;
    .log.Err "stp_init.q -config <path/to/config.cfg>";
    exit 1
  ];
  .cfg.LoadConfig `$":",first opts`config;

  system "p ",.cfg.Config.ListenPort;  / start listening

  .timer.AddIn[`Start;0D00:00:07] / Start in 7 seconds
  };

if[`stp_init.q=last `$"/" vs string .z.f;init[.z.x]];