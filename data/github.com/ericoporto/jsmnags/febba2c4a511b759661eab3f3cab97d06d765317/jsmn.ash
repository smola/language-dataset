// new module header

/**
 * JSON type identifier. Basic types are:
 * 	o Object
 * 	o Array
 * 	o String
 * 	o Other primitive: number, boolean (true/false) or null
 */
 enum jsmntype {
  eJSMN_UNDEFINED = 0,
  eJSMN_OBJECT = 1,
  eJSMN_ARRAY = 2,
  eJSMN_STRING = 3,
  eJSMN_PRIMITIVE = 4
};

enum jsmnerr {
  /* Not enough tokens were provided */
  eJSMN_ERROR_NOMEM = -1,
  /* Invalid character inside JSON string */
  eJSMN_ERROR_INVAL = -2,
  /* The string is not a full JSON packet, more bytes expected */
  eJSMN_ERROR_PART = -3
};

/**
 * JSON token description.
 * type		type (object, array, string etc.)
 * start	start position in JSON data string
 * end		end position in JSON data string
 */
managed struct jsmntok {
  jsmntype type;
  int start;
  int end;
  int size;
  int parent;
};

/**
 * JSON parser. Contains an array of token blocks available. Also stores
 * the string being parsed now and current position in that string.
 */
managed struct jsmn_parser{
  int pos;     /* offset in the JSON string */
  int toknext; /* next token to allocate */
  int toksuper;         /* superior token node, e.g. parent object or array */
};

/**
 * Create JSON parser over an array of tokens
 */
import void jsmn_init(jsmn_parser *parser);

/**
 * Run JSON parser. It parses a JSON data string into and array of tokens, each
 * describing
 * a single JSON object.
 */
import int jsmn_parse(jsmn_parser *parser, String js, int len, jsmntok *tokens[], int num_tokens);