namespace Drupal;

final class Bootstrap 
{
	private static conf_path_conf = "";
	private static variable_conf;

	public static final function conf_path(boolean require_settings = TRUE, boolean reset = FALSE, string drupal_root, var serverarray) {
	  if self::conf_path_conf && !reset {
	    return self::conf_path_conf;
	  }

	  string confdir = "sites", dir = "";
	  array sites = [];
	  var uri, server;
	  int i, j;

	  if file_exists(drupal_root . "/" . confdir . "/sites.php") {
	  	include(drupal_root . "/" . confdir . "/sites.php");
	  }
	  if isset serverarray["SCRIPT_NAME"] {
	  	let uri = ZephirHelper::explode("/", serverarray["SCRIPT_NAME"]);
	  }
	  else {
	  	let uri = ZephirHelper::explode("/", serverarray["SCRIPT_FILENAME"]);
	  }

	  let server = ZephirHelper::explode(".", join(".", array_reverse(ZephirHelper::explode(":", ZephirHelper::rtrim(serverarray["HTTP_HOST"], ".")))));
	  for i in range(ZephirHelper::count(uri), 0) {
	  	for j in range(ZephirHelper::count(server), 0) {
	  		let dir = join(".", ZephirHelper::array_slice(server, -j)) . join(".", ZephirHelper::array_slice(uri, 0, i));
	  		if isset sites[dir] && file_exists(drupal_root . "/" . confdir . "/" . sites[dir]) {
	  			let dir = sites[dir];
	  		}
	  		if file_exists(drupal_root . "/" . confdir . "/" . dir . "/settings.php") || require_settings && file_exists(drupal_root . "/" . confdir . "/" . dir) {
	  			let self::conf_path_conf = confdir . "/" . dir;
	  			return self::conf_path_conf;
	  		}
	  	}
	  }
	  let self::conf_path_conf = confdir . "/default";
	  return self::conf_path_conf;
	}

	public static final function check_plain(string text) {
		char ch;
		string output = "";

		for ch in text { 
			switch ch {
				case '\'':
					let output .= "&#039;";
					break;
				case '"':
					let output .= "&quot;";
					break;
				case '&':
					let output .= "&amp;";
					break;
				case '<':
					let output .= "&lt;";
					break;
				case '>':
					let output .= "&gt;";
					break;										
				default:
					let output .= ch;
					break;
			}
		}
		return output;
	}

	public static function variable_initialize(array conf = []) {
		var cached, variables, value, kname;
		string name;

		let cached = Cache::cache_get("variables", "cache_bootstrap");
	  	if typeof cached == "object" {
	    	let variables = cached->data;
	 	}
	  	else {
		    let name = "variable_init";
		    if !lock_acquire(name, 1) {
				lock_wait(name);
				return self::variable_initialize(conf);
		    }
		    else {
				let variables = array_map("unserialize", db_query("SELECT name, value FROM {variable}")->fetchAllKeyed());
				cache_set("variables", variables, "cache_bootstrap");
				lock_release(name);
		    }
		}

		for kname, value in conf {
			let variables[name] = value;
		}

	  	return variables;
	}

	public static final function variable_get(string name, var defaultvalue = "") {
		if isset self::variable_conf[name] {
			return self::variable_conf[name];
		}

		return defaultvalue;
	}

	public static final function variable_set(string name, var value) {
		db_merge("variable")->key(["name": name])->fields(["value": serialize(value)])->execute();

		cache_clear_all("variables", "cache_bootstrap");

		let self::variable_conf[name] = value;
	}

	public static final function variable_del(string name) {
		db_delete("variable")->condition("name", name)->execute();
  		cache_clear_all("variables", "cache_bootstrap");

  		unset(self::variable_conf[name]);
	}

	public static final function _drupal_bootstrap_variables(string drupal_root) {
	  require drupal_root . "/" . self::variable_get("lock_inc", "includes/lock.inc");
	  lock_initialize();

	  let self::variable_conf = self::variable_initialize();
	  
	  require drupal_root . "/includes/module.inc";
	  module_load_all(true);
	}	
}
