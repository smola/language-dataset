PROGRAM_NAME='jsmn-util'
#if_not_defined __JSMN_UTIL_
#define __JSMN_UTIL_


include 'jsmn'


DEFINE_CONSTANT

integer JSMN_MAX_RETURN_SIZE = 2048;


/**
 * Tokenise a JSON string
 */
define_function slong json_tokenise(char js[], jsmn_token tokens[]) {
    stack_var jsmn_parser parser;
    jsmn_init(parser);
    return jsmn_parse(parser, js, tokens);
}


/**
 * Gets the contents of a passed token
 */
define_function char[JSMN_MAX_RETURN_SIZE] json_get_token(char js[], jsmn_token t) {
    return mid_string(js, t.start, t.end - t.start);
}


/**
 * Compare a string to a JSON token
 */
define_function char json_token_str_eq(char js[], jsmn_token t, char s[]) {
    return (mid_string(js, t.start, t.end - t.start) == s);
}


#end_if

