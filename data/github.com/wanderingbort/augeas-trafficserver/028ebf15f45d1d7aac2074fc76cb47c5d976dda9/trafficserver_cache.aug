module Trafficserver_cache =
  autoload cache_xfm

  let eol = ( Util.eol | Util.comment_eol )
  let indent = Util.indent
  let spc = Util.del_ws_spc
  let value_re = /([^ \"\t\n#]([^ \t\n#]*)?|\"[^\\\"\n#]*(\\\.[^\\\"\n#]*)*\")/

  let cache_filter = incl "/etc/trafficserver/cache.config"

  (* primary dest types *)
  let primary_dest = "dest_domain" | "dest_host" | "dest_ip" | "url_regex"

  (* secondary dest types *)
  let secondary_spec = "port" | "scheme" | "prefix" | "suffix" | "method" | "time" | "src_ip" 

  (* directives *)
  let action_re = "never-cache" | "ignore-no-cache" | "ignore-client-no-cache" | "ignore-server-no-cache"
  let time_format_re = /([0-9]+[dhms])*[0-9]*/

  let timed_directive = [ key ( "pin-in-cache" | "revalidate" | "ttl-in-cache" ) . del /=/ "=" . store time_format_re ]
  let action_directive = [ key /action/ . del /=/ "=". store action_re ] 
  let cookie_directive = [ key /cache-responses-to-cookies/ . del /=/ "=" . store /[0-9]/ ] 
  let directive_entry = ( action_directive | timed_directive | cookie_directive )

  (* basic entry *)
  let directive_list = ( spc . directive_entry ) +
  let optional_secondary = ( spc . [ key secondary_spec . del /=/ "=" . store value_re ] ) ?
  let cache_entry = [ indent . key primary_dest . del /=/ "=" . store  value_re . optional_secondary . directive_list . eol  ]

  (* lns xfm *)
  let cache_lns = ( Util.empty | Util.comment | cache_entry ) *
  let cache_xfm = transform cache_lns cache_filter
