namespace Cake\Utility;

class Hash {

	public static function get(data, path, defaultValue = null) {
		var parts, key, loopData;
		bool isString;

		if empty data {
			return defaultValue;
		}

		let isString = (bool)is_string(path);
		if isString && strpos(path, ".") === false {
			return isset data[path] ? data[path] : defaultValue;
		}

		if isString || is_numeric(path) {
			let parts = explode(".", (string)path);
		} else {
			let parts = path;
		}

		let loopData = data;
		for key in parts {
			if !fetch loopData, loopData[key] {
				return defaultValue;
			}
		}
		return loopData;
	}

	public static function extract(array data, string path) -> array {
		var tokens, token, conditions, tmp, item, k, v, next, filter, context = [];
		string key = "__set_item__";

		if empty path {
			return data;
		}

		if !memstr(path, "{") && !memstr(path, "[") {
			let tmp = self::get(data, path);
			if typeof tmp === "array" {
				return tmp;
			} else {
				if typeof tmp === "null" {
					return [];
				}
				return [tmp];
			}
		}

		if strpos(path, "[") === false {
			let tokens = explode(".", path);
		} else {
			let tokens = String2::tokenize(path, ".", "[", "]");
		}

		let context[key] = [data];
		for token in tokens {
			let next = [];

			let tmp = self::_splitConditions(token);
			let token = tmp[0];
			let conditions = tmp[1];

			for item in context[key] {
				if !is_array(item) {
					let item = is_null(item) ? [] : [item];
				}
				for k, v in item {
					if self::_matchToken(k, token) {
						let next[] = v;
					}
				}
			}

			// Filter for attributes.
			if conditions {
				let filter = [];
				for item in next {
					if is_array(item) && self::_matches(item, conditions) {
						let filter[] = item;
					}
				}
				let next = filter;
			}
			let context[key] = next;
		}
		return context[key]; 
	}

	protected static function _splitConditions(token) -> array {
		var conditions, position;

		let conditions = false;
		let position = strpos(token, "[");
		if position !== false {
			let conditions = substr(token, position);
			let token = substr(token, 0, position);
		}

		return [token, conditions];
	}

	protected static function _matchToken(key, token) -> bool {
		if token === "{n}" {
			return is_numeric(key);
		}
		if token === "{s}" {
			return is_string(key);
		}
		if is_numeric(token) {
			return key == token;
		}
		return key === token;
	}

	protected static function _matches(data, selector) -> bool {
		var conditions = null, cond, attr, op, val, prop;

		preg_match_all(
			"/(\\[ (?P<attr>[^=><!]+?) (\\s* (?P<op>[><!]?[=]|[><]) \\s* (?P<val>(?:\\/.*?\\/ | [^\\]]+)) )? \\])/x",
			selector,
			conditions,
			PREG_SET_ORDER
		);

		for cond in conditions {
			let attr = cond["attr"];
			let op = isset cond["op"] ? cond["op"] : null;
			let val = isset cond["val"] ? cond["val"] : null;

			// Presence test.
			if empty op && empty val && !isset data[attr] {
				return false;
			}

			// Empty attribute = fail.
			if !(isset data[attr] || array_key_exists(attr, data)) {
				return false;
			}

			let prop = null;
			if isset data[attr] {
				let prop = data[attr];
			}
			if prop === true || prop === false {
				let prop = prop ? "true" : "false";
			}

			// Pattern matches and other operators.
			if op === "=" && val && substr(val, 0, 1) === "/" {
				if !preg_match(val, prop) {
					return false;
				}
			} else {
				if (
					(op === "=" && prop != val) ||
					(op === "!=" && prop == val) ||
					(op === ">" && prop <= val) ||
					(op === "<" && prop >= val) ||
					(op === ">=" && prop < val) ||
					(op === "<=" && prop > val)
				) {
					return false;
				}
			}
		}
		return true;
	}

	public static function insert(array data, string path, values = null) -> array {
		bool noTokens;
		var tokens, token, nextPath, tmp, conditions, k, v;

		let noTokens = !memstr(path, "[");
		if noTokens && !memstr(path, ".") {
			let data[path] = values;
			return data;
		}

		if noTokens {
			let tokens = explode(".", path);
		} else {
			let tokens = String2::tokenize(path, ".", "[", "]");
		}

		if noTokens && !memstr(path, "{") {
			return self::_simpleOp("insert", data, tokens, values);
		}

		let token = array_shift(tokens);
		let nextPath = implode(".", tokens);

		let tmp = self::_splitConditions(token);
		let token = tmp[0];
		let conditions = tmp[1];

		for k, v in data {
			if self::_matchToken(k, token) {
				if conditions && self::_matches(v, conditions) {
					let data[k] = array_merge(v, values);
					continue;
				}
				if !conditions {
					let data[k] = self::insert(v, nextPath, values);
				}
			}
		}
		return data;
	}

	/**
	 * This method is different from CakePHP because Zephir doesn't
	 * support variables by reference
	 */
	protected static function _simpleOp(string op, data, path, values = null) -> array {
		int count;
		var key;

		let count = count(path);
		if count === 0 {
			return data;
		}
		let key = array_shift(path);

		if op === "insert" {
			if !isset data[key] || typeof data[key] !== "array" {
				let data[key] = [];
			}
			if count === 1 {
				let data[key] = values;
			} else {
				let data[key] = self::_simpleOp(op, data[key], path, values);
			}
			return data;
		} else {
			if op === "remove" {
				if count === 1 {
					unset data[key];
					return data;
				}
				if !isset data[key] {
					return data;
				}
				let data[key] = self::_simpleOp(op, data[key], path, values);
			}
		}
		return [];
	}

	public static function remove(data, string path) -> array {
		bool noTokens, noExpansion;
		var tokens, token, nextPath, tmp, conditions, k, v, match, newData;

		let noTokens = !memstr(path, "[");
		let noExpansion = !memstr(path, "{");

		if noExpansion && noTokens && !memstr(path, ".") {
			unset data[path];
			return data;
		}

		let tokens = noTokens ? explode(".", path) : String2::tokenize(path, ".", "[", "]");

/**
 * Similar of extract method, this code is causing the PHP to crash,
 * but it is also only for performance and can be skipped for now.
 *
		if noExpansion && noTokens {
			return self::_simpleOp("remove", data, tokens, null);
		}
*/

		let token = array_shift(tokens);
		let nextPath = implode(".", tokens);

		let tmp = self::_splitConditions(token);
		let token = tmp[0];
		let conditions = tmp[1];

		let newData = data;
		for k, v in data {
			let match = self::_matchToken(k, token);
			if match && typeof v === "array" {
				if conditions && self::_matches(v, conditions) {
					unset newData[k];
					continue;
				}
				let newData[k] = self::remove(v, nextPath);
				if empty newData[k] {
					unset newData[k];
				}
			} else {
				if match {
					unset newData[k];
				}
			}
		}
		return newData;
	}

	public static function combine(data, keyPath, valuePath = null, groupPath = null) -> array {
		var format, keys, vals = null, group, out = [], i;

		if empty data {
			return [];
		}

		if typeof keyPath === "array" {
			let format = array_shift(keyPath);
			let keys = self::format(data, keyPath, format);
		} else {
			let keys = self::extract(data, keyPath);
		}
		if empty keys {
			return [];
		}

		if !empty valuePath && typeof valuePath === "array" {
			let format = array_shift(valuePath);
			let vals = self::format(data, valuePath, format);
		} else {
			if !empty valuePath {
				let vals = self::extract(data, valuePath);
			}
		}
		if empty vals {
			let vals = array_fill(0, count(keys), null);
		}

		if count(keys) !== count(vals) {
			throw new \RuntimeException(
				"Hash::combine() needs an equal number of keys + values."
			);
		}

		if groupPath !== null {
			let group = self::extract(data, groupPath);
			if !empty group {
				for i in range(0, count(keys) - 1) {
					if !isset group[i] {
						let group[i] = 0;
					}
					let out[group[i]][keys[i]] = vals[i];
				}
				return out;
			}
		}
		if empty vals {
			return [];
		}
		return array_combine(keys, vals);
	}

	public static function format(data, paths, format) -> array {
		var extracted = [], count, i, j, out = [], countTwo, args = [];

		let count = count(paths);
		if !count {
			return [];
		}

		for i in range(0, count - 1) {
			let extracted[] = self::extract(data, paths[i]);
		}
		let data = extracted;
		let count = count(data[0]);

		let countTwo = count(data);
		for j in range (0, count - 1) {
			let args = [];
			for i in range (0, countTwo - 1) {
				if isset data[i][j] {
					let args[] = data[i][j];
				}
			}
			let out[] = vsprintf(format, args);
		}
		return out;
	}

	public static function contains(data, needle) -> bool {
		var stack = [], key, val, next, tmp;

		if empty data || empty needle {
			return false;
		}

		while !empty needle {
			let key = key(needle);
			let val = needle[key];
			unset needle[key];

			if isset data[key] && typeof val === "array" {
				let next = data[key];
				unset data[key];

				if !empty val {
					let stack[] = [val, next];
				}
			} else {
				if !isset data[key] || data[key] != val {
					return false;
				}
			}

			if empty needle && !empty stack {
				let tmp = array_pop(stack);
				let needle = tmp[0];
				let data = tmp[1];
			}
		}
		return true;
	}

	public static function check(data, path) -> bool {
		var results;

		let results = self::extract(data, path);
		if typeof results !== "array" {
			return false;
		}
		return !empty results;
	}

	public static function filter(data, callback = null) -> array {
		var k, v;

		if callback === null {
			let callback = ["Cake\\Utility\\Hash", "_filter"];
		}

		for k, v in data {
			if typeof v === "array" {
				let data[k] = self::filter(v, callback);
			}
		}
		return array_filter(data, callback);
	}

/**
 * A little different from CakePHP implementation because the
 * different type treatment on comparisons
 *
 * Also, this method is supposed to be protected, but zephir seems
 * to not treat local callbacks very well. Maybe it is a limitation on
 * PHP extensions, but either way setting it to public for while
 */
	public static function _filter(value) {
		if typeof value === "boolean" && value == false {
			return false;
		}
		return
			(typeof value === "integer" && value === 0) ||
			(typeof value === "string" && value === "0") ||
			!empty(value);
	}

	public static function flatten(data, separator = ".") -> array {
		var result = [], stack = [], path = "", key, element, tmp;

		reset(data);
		while !empty data {
			let key = key(data);
			let element = data[key];
			unset data[key];

			if typeof element === "array" && !empty element {
				if !empty data {
					let stack[] = [data, path];
				}
				let data = element;
				reset(data);
				let path = path . key . separator;
			} else {
				let result[path . key] = element;
			}

			if empty data && !empty stack {
				let tmp = array_pop(stack);
				let data = tmp[0];
				let path = tmp[1];
				reset(data);
			}
		}
		return result;
	}

	public static function expand(data, separator = ".") {
		var result = [], flat, value, keys, child = [], tmp, k;

		for flat, value in data {
			let keys = explode(separator, (string)flat);
			let keys = array_reverse(keys);
			let child = [];
			let child[keys[0]] = value;
			array_shift(keys);
			for k in keys {
				let tmp = child;
				let child = [];
				let child[k] = tmp;
			}
			let result = self::merge(result, child);
		}
		return result;
	}

	public static function merge(data, merge, m2 = null, m3 = null, m4 = null, m5 = null, m6 = null, m7 = null, m8 = null, m9 = null, m10 = null) {
		var args, result, arg, tmp, key, val;

		let args = func_get_args();
		let result = current(args);

		let arg = next(args);
		while (arg !== false) {
			if typeof arg === "null" {
				let arg = next(args);
				continue;
			}
			let tmp = typeof arg === "array" ? arg : [arg];
			for key, val in tmp {
				if isset result[key] && typeof result[key] === "array" && typeof val === "array" {
					let result[key] = self::merge(result[key], val);
				} else {
					if is_int(key) && isset result[key] {
						let result[] = val;
					} else {
						let result[key] = val;
					}
				}
			}
			let arg = next(args);
		}
		return result;
	}

	public static function numeric(data) -> bool {
		if empty data {
			return data;
		}
		return data === array_filter(data, "is_numeric");
	}

	public static function dimensions(data) -> int {
		var depth = 1, elem;

		if empty data {
			return 0;
		}
		let elem = array_shift(data);
		while elem {
			if typeof elem !== "array" {
				break;
			}
			let depth++;
			let data = elem;
			let elem = array_shift(data);
		}
		return depth;
	}

	public static function maxDimensions(data) -> int {
		var depth = [], value;
		if typeof data === "array" && reset(data) !== false {
			for value in data {
				let depth[] = self::dimensions(typeof value === "array" ? value : [value]) + 1;
			}
		}
		return max(depth);
	}

	public static function map(data, path, func) -> array {
		var values;

		let values = self::extract(data, path);
		if typeof values !== "array" {
			let values = [values];
		}
		return array_map(func, values);
	}

	public static function reduce(data, path, func) {
		var values;

		let values = self::extract(data, path);
		if typeof values !== "array" {
			let values = [values];
		}
		return array_reduce(values, func);
	}

	public static function apply(data, path, func) {
		var values;

		let values = self::extract(data, path);
		if typeof values !== "array" {
			let values = [values];
		}
		return call_user_func(func, values);
	}

	public static function sort(data, path, dir, type = "regular") -> array {
		var originalKeys, numeric, sortValues, sortCount, dataCount, result, keys, values, sorted = [], k, typeValue;

		if empty data {
			return [];
		}

		let originalKeys = array_keys(data);
		let numeric = is_numeric(implode("", originalKeys));
		if numeric {
			let data = array_values(data);
		}
		let sortValues = self::extract(data, path);
		let sortCount = count(sortValues);
		let dataCount = count(data);

		if sortCount < dataCount {
			let sortValues = array_pad(sortValues, dataCount, null);
		}
		let result = self::_squash(sortValues);
		let keys = self::extract(result, "{n}.id");
		let values = self::extract(result, "{n}.value");

		let dir = strtolower(dir) == "asc" ? SORT_ASC : SORT_DESC;
		let type = strtolower(type);

		if type === "numeric" {
			let typeValue = SORT_NUMERIC;
		} else {
			if type === "string" {
				let typeValue = SORT_STRING;
			} else {
				if type === "natural" {
					let typeValue = SORT_NATURAL;
				} else {
					let typeValue = SORT_REGULAR;
				}
			}
		}
		array_multisort(values, dir, typeValue, keys, dir, typeValue);
		let keys = array_unique(keys);

		for k in keys {
			if numeric {
				let sorted[] = data[k];
				continue;
			}
			if isset originalKeys[k] {
				let sorted[originalKeys[k]] = data[originalKeys[k]];
			} else {
				let sorted[k] = data[k];
			}
		}
		return sorted;
	}

	protected static function _squash(data, key = null) -> array {
		var stack = [], k, r, id;

		for k, r in data {
			let id = k;
			if key !== null {
				let id = key;
			}
			if typeof r === "array" && !empty r {
				let stack = array_merge(stack, self::_squash(r, id));
			} else {
				let stack[] = ["id": id, "value": r];
			}
		}
		return stack;
	}

	public static function diff(data, compare) -> array {
		var key, intersection;

		if empty data {
			return compare;
		}
		if empty compare {
			return data;
		}
		let intersection = array_intersect_key(data, compare);
		let key = key(intersection);
		while key !== null {
			if data[key] == compare[key] {
				unset data[key];
				unset compare[key];
			}
			next(intersection);
			let key = key(intersection);
		}
		return data + compare;
	}

	public static function mergeDiff(data, compare) -> array {
		var key, value;

		if empty data && !empty compare {
			return compare;
		}
		if empty compare {
			return data;
		}

		for key, value in compare {
			if !isset data[key] {
				let data[key] = value;
			} else {
				if typeof value == "array" {
					let data[key] = self::mergeDiff(data[key], compare[key]);
				}
			}
		}
		return data;
	}

	public static function normalize(data, assoc = true) -> array {
		var keys, count, numeric = true, newList = [], i;

		let keys = array_keys(data);
		let count = count(keys);

		if !assoc {
			for i in range(0, count - 1) {
				if !is_int(keys[i]) {
					let numeric = false;
					break;
				}
			}
		}
		if !numeric || assoc {
			for i in range(0, count - 1) {
				if is_int(keys[i]) {
					let newList[data[keys[i]]] = null;
				} else {
					let newList[keys[i]] = data[keys[i]];
				}
			}
			let data = newList;
		}
		return data;
	}

	public static function nest(data, options = []) -> array {
		var alias, defaultOptions, output = [], newOutput, idMap = [], ids, idKeys, parentKeys, result, tmp, root, id, parentId, i;

		if !data {
			return data;
		}

		let alias = key(current(data));
		let defaultOptions = [
			"idPath": "{n}." . alias . ".id",
			"parentPath": "{n}." . alias . ".parent_id",
			"children": "children",
			"root": null
		];
		let options = array_merge(defaultOptions, options);

		let ids = self::extract(data, options["idPath"]);
		let idKeys = explode(".", options["idPath"]);
		array_shift(idKeys);
		let parentKeys = explode(".", options["parentPath"]);
		array_shift(parentKeys);

		for result in data {
			let result[options["children"]] = [];
			let id = self::get(result, idKeys);
			let parentId = self::get(result, parentKeys);

			if isset idMap[id][options["children"]] {
				let idMap[id] = array_merge(result, typeof idMap[id] === "array" ? idMap[id] : [idMap[id]]);
			} else {
				let tmp = [];
				let tmp[options["children"]] = [];
				let idMap[id] = array_merge(result, tmp);
			}
			if !parentId || !in_array(parentId, ids) {
				let output[] = idMap[id];
			} else {
				let idMap[parentId][options["children"]][] = idMap[id];
			}
		}

		if options["root"] {
			let root = options["root"];
		} else {
			if !output {
				return [];
			} else {
				let root = self::get(output[0], parentKeys);
			}
		}

		let newOutput = output;
		for i, result in output {
			let id = self::get(result, idKeys);
			let parentId = self::get(result, parentKeys);
			if id !== root && parentId != root {
				unset newOutput[i];
			}
		}
		return array_values(newOutput);
	}

}
