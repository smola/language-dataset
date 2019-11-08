module Controller {
  function start(url) {
    match (url) {
    case {path:[] ...}:
      View.show_root();
    case {path: ["ana", id, t] ...}:
      match(t){
      case "cfg":
        View.show_analysis(id, {cfg});
      default:
        View.show_analysis(id, {src});
      }
    /*case {path: ["test", "parser"] ...}:
      Model.debug_parser();
      View.show_root();*/
    case {...}: Resource.raw_status({bad_request});
    }
  }
}

resources = @static_resource_directory("resources")

Server.start(Server.http, [
  { register:
    [ { doctype: { html5 } },
      { js: [ "/resources/lib/d3.min.js", "/resources/lib/dagre.min.js",
        "/resources/lib/dagre-d3.min.js", "/resources/lib/graphlib-dot.min.js",
        "/resources/lib/prettify.js"] },
      { css: [ "/resources/css/layout.css", "/resources/css/prettify.css", "/resources/css/titatoggle.min.css"] },
      { favicon: [ Favicon.make({ico},"/resources/favicon.ico")]}
    ]
  },
  { ~resources },
  { dispatch: Controller.start }
])