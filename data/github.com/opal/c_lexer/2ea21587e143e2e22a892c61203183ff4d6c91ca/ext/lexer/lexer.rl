#include <ruby.h>
#include <ruby/encoding.h>
#include <ruby/re.h>

#include <stdint.h>
#include <stdio.h>

#include "stack_state/stack.h"
#include "stack_state/stack_state.h"
#include "lexer.h"

#define GET_LEXER(self) Data_Get_Struct(self, Lexer, lexer)
#define STATIC_ENV_DECLARED(name) \
  lexer->static_env != Qnil && RTEST(rb_funcall(lexer->static_env, rb_intern("declared?"), 1, name))
#define NUMPARAM_MAX 100

#include "stack_state/cmdarg.h"
#include "stack_state/cond.h"

#include "literal/methods.h"
#include "emit_tables.h"

%%machine lex;
%%write data;

static VALUE lexer_alloc(VALUE klass)
{
  Lexer *lexer = xmalloc(sizeof(Lexer));

  lexer->cs = lexer->p = lexer->pe = 0;
  lexer->paren_nest = 0;

  lexer->cs_stack      = xmalloc(4 * sizeof(int));
  lexer->cs_stack_top  = 0;
  lexer->cs_stack_size = 4;

  lexer->source_buffer = Qnil;
  lexer->source        = Qnil;
  lexer->source_pts    = Qnil;
  lexer->token_queue   = Qnil;
  lexer->static_env    = Qnil;
  lexer->lambda_stack  = Qnil;
  lexer->diagnostics   = Qnil;
  lexer->tokens        = Qnil;
  lexer->comments      = Qnil;
  lexer->encoding      = Qnil;
  lexer->escape        = Qnil;
  lexer->max_numparam_stack = Qnil;

  ss_stack_init(&lexer->cond_stack);
  ss_stack_init(&lexer->cmdarg_stack);
  lit_stack_init(&lexer->literal_stack);

  return Data_Wrap_Struct(klass, lexer_mark, lexer_dealloc, lexer);
}

static void lexer_mark(void *ptr)
{
  Lexer *lexer = ptr;
  rb_gc_mark(lexer->source_buffer);
  rb_gc_mark(lexer->source);
  rb_gc_mark(lexer->source_pts);
  rb_gc_mark(lexer->token_queue);
  rb_gc_mark(lexer->static_env);
  rb_gc_mark(lexer->lambda_stack);
  rb_gc_mark(lexer->diagnostics);
  rb_gc_mark(lexer->tokens);
  rb_gc_mark(lexer->comments);
  rb_gc_mark(lexer->encoding);
  rb_gc_mark(lexer->escape);
  rb_gc_mark(lexer->max_numparam_stack);

  for (literal *lit = lexer->literal_stack.bottom; lit < lexer->literal_stack.top; lit++) {
    rb_gc_mark(lit->buffer);
    rb_gc_mark(lit->start_tok);
    rb_gc_mark(lit->start_delim);
    rb_gc_mark(lit->end_delim);
    rb_gc_mark(lit->delimiter);
  }
}

static void lexer_dealloc(void *ptr)
{
  Lexer *lexer = ptr;
  ss_stack_dealloc(&lexer->cond_stack);
  ss_stack_dealloc(&lexer->cmdarg_stack);
  lit_stack_dealloc(&lexer->literal_stack);
  xfree(ptr);
}

static VALUE lexer_initialize(VALUE self, VALUE version)
{
  Lexer* lexer = GET_LEXER(self);

  lexer->version = NUM2INT(version);
  rb_iv_set(self, "@context", Qnil);

  return lexer_reset(0, NULL, self);
}

static VALUE lexer_reset(int argc, VALUE *argv, VALUE self)
{
  Lexer* lexer = GET_LEXER(self);

  VALUE reset_state;
  rb_scan_args(argc, argv, "01", &reset_state);
  if (NIL_P(reset_state))
    reset_state = Qtrue;

  if (RTEST(reset_state)) {
    lexer->cs = lex_en_line_begin;

    lexer->cond   = 0;
    lexer->cmdarg = 0;
    ss_stack_clear(&lexer->cond_stack);
    ss_stack_clear(&lexer->cmdarg_stack);
  }

  lexer->force_utf32 = 0;

  lexer->source       = Qnil;
  lexer->source_pts   = Qnil;
  lexer->encoding     = Qnil;

  lexer->p            = 0;
  // @ts is a local variable
  // @te is a local variable
  // @act is a local variable

  // @stack is handled on prepush
  // @top is handled on prepush

  // Lexer state
  lexer->token_queue  = rb_ary_new();
  lit_stack_clear(&lexer->literal_stack);

  lexer->eq_begin_s   = 0;
  // @sharp_s is a local variable

  lexer->newline_s    = 0;

  // @num_base is a local variable
  // @num_digits_s is a local variable
  // @num_suffix_s is a local variable
  // @num_xfrm is a local variable

  lexer->escape_s     = 0;
  lexer->escape       = Qnil;

  lexer->herebody_s   = 0;

  lexer->paren_nest   = 0;
  lexer->lambda_stack = rb_ary_new();

  lexer->dedent_level = -1;

  lexer->command_start = 1;

  lexer->in_kwarg     = 0;

  lexer->cs_before_block_comment = lex_en_line_begin;

  lexer->max_numparam_stack = rb_class_new_instance(0, NULL, max_numparam_stack_klass) ;

  return self;
}

static VALUE lexer_set_source_buffer(VALUE self, VALUE buffer)
{
  Lexer* lexer = GET_LEXER(self);

  lexer->source_buffer = buffer;

  if (RTEST(buffer)) {
    lexer->source = rb_funcall(buffer, rb_intern("source"), 0);
    lexer->encoding = rb_obj_encoding(lexer->source);

    if (lexer->encoding == utf8_encoding) {
      lexer->source_pts = rb_funcall(lexer->source, rb_intern("unpack"), 1, rb_str_new2("U*"));
    } else {
      lexer->source_pts = rb_funcall(lexer->source, rb_intern("unpack"), 1, rb_str_new2("C*"));
    }

    lexer->pe = RARRAY_LEN(lexer->source_pts) + 2; /* pretend there is a null at the end */

    VALUE source_pt = rb_ary_entry(lexer->source_pts, 0);
    if (source_pt != Qnil && NUM2INT(source_pt) == 0xfeff) {
      lexer->p = 1;
    }
  } else {
    lexer->source = Qnil;
    lexer->source_pts = Qnil;
    lexer->encoding = Qnil;
    lexer->pe = 0;
  }

  return self;
}

static VALUE lexer_get_state(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);

  switch (lexer->cs) {
  case lex_en_line_begin:    return ID2SYM(rb_intern("line_begin"));
  case lex_en_expr_dot:      return ID2SYM(rb_intern("expr_dot"));
  case lex_en_expr_fname:    return ID2SYM(rb_intern("expr_fname"));
  case lex_en_expr_value:    return ID2SYM(rb_intern("expr_value"));
  case lex_en_expr_beg:      return ID2SYM(rb_intern("expr_beg"));
  case lex_en_expr_mid:      return ID2SYM(rb_intern("expr_mid"));
  case lex_en_expr_arg:      return ID2SYM(rb_intern("expr_arg"));
  case lex_en_expr_cmdarg:   return ID2SYM(rb_intern("expr_cmdarg"));
  case lex_en_expr_end:      return ID2SYM(rb_intern("expr_end"));
  case lex_en_expr_endarg:   return ID2SYM(rb_intern("expr_endarg"));
  case lex_en_expr_endfn:    return ID2SYM(rb_intern("expr_endfn"));
  case lex_en_expr_labelarg: return ID2SYM(rb_intern("expr_labelarg"));

  case lex_en_interp_string: return ID2SYM(rb_intern("interp_string"));
  case lex_en_interp_words:  return ID2SYM(rb_intern("interp_words"));
  case lex_en_plain_string:  return ID2SYM(rb_intern("plain_string"));
  case lex_en_plain_words:   return ID2SYM(rb_intern("plain_words"));
  default:
    rb_raise(rb_eRuntimeError, "Lexer state variable is borked");
  }
}

static VALUE lexer_set_state(VALUE self, VALUE state_sym)
{
  Lexer* lexer = GET_LEXER(self);
  const char *state_name = rb_id2name(SYM2ID(state_sym));

  if (strcmp(state_name, "line_begin") == 0)
    lexer->cs = lex_en_line_begin;
  else if (strcmp(state_name, "expr_dot") == 0)
    lexer->cs = lex_en_expr_dot;
  else if (strcmp(state_name, "expr_fname") == 0)
    lexer->cs = lex_en_expr_fname;
  else if (strcmp(state_name, "expr_value") == 0)
    lexer->cs = lex_en_expr_value;
  else if (strcmp(state_name, "expr_beg") == 0)
    lexer->cs = lex_en_expr_beg;
  else if (strcmp(state_name, "expr_mid") == 0)
    lexer->cs = lex_en_expr_mid;
  else if (strcmp(state_name, "expr_arg") == 0)
    lexer->cs = lex_en_expr_arg;
  else if (strcmp(state_name, "expr_cmdarg") == 0)
    lexer->cs = lex_en_expr_cmdarg;
  else if (strcmp(state_name, "expr_end") == 0)
    lexer->cs = lex_en_expr_end;
  else if (strcmp(state_name, "expr_endarg") == 0)
    lexer->cs = lex_en_expr_endarg;
  else if (strcmp(state_name, "expr_endfn") == 0)
    lexer->cs = lex_en_expr_endfn;
  else if (strcmp(state_name, "expr_labelarg") == 0)
    lexer->cs = lex_en_expr_labelarg;

  else if (strcmp(state_name, "interp_string") == 0)
    lexer->cs = lex_en_interp_string;
  else if (strcmp(state_name, "interp_words") == 0)
    lexer->cs = lex_en_interp_words;
  else if (strcmp(state_name, "plain_string") == 0)
    lexer->cs = lex_en_plain_string;
  else if (strcmp(state_name, "plain_words") == 0)
    lexer->cs = lex_en_plain_words;
  else
    rb_raise(rb_eArgError, "Invalid state: %s", state_name);

  return state_sym;
}

static VALUE lexer_push_cmdarg(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  ss_stack_push(&lexer->cmdarg_stack, lexer->cmdarg);
  lexer->cmdarg = 0;
  return Qnil;
}

static VALUE lexer_pop_cmdarg(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  lexer->cmdarg = ss_stack_pop(&lexer->cmdarg_stack);
  return Qnil;
}

static VALUE lexer_push_cond(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  ss_stack_push(&lexer->cond_stack, lexer->cond);
  lexer->cond = 0;
  return Qnil;
}

static VALUE lexer_pop_cond(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  lexer->cond = ss_stack_pop(&lexer->cond_stack);
  return Qnil;
}

static VALUE lexer_get_in_kwarg(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  return lexer->in_kwarg ? Qtrue : Qfalse;
}

static VALUE lexer_set_in_kwarg(VALUE self, VALUE val)
{
  Lexer* lexer = GET_LEXER(self);
  lexer->in_kwarg = RTEST(val) ? 1 : 0;
  return val;
}

static VALUE lexer_max_numparam_stack(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  return lexer->max_numparam_stack;
}

static VALUE lexer_max_numparam(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  return rb_funcall(lexer->max_numparam_stack, rb_intern("top"), 0);
}

static VALUE lexer_get_dedent_level(VALUE self)
{
  Lexer* lexer = GET_LEXER(self);
  int result = lexer->dedent_level;
  lexer->dedent_level = -1;
  if (result == -1)
    return Qnil;
  else
    return INT2NUM(result);
}

static VALUE lexer_set_force_utf32(VALUE self, VALUE arg)
{
  return arg;
}

static VALUE lexer_advance(VALUE self)
{
  int cs, act = 0, top, cmd_state;
  int num_base = 0;
  long p, pe, eof, ts = 0, te = 0, tm = 0, sharp_s = 0, heredoc_e = 0;
  long num_digits_s = 0, num_suffix_s = 0;
  void (*num_xfrm)(Lexer*, VALUE, long, long); /* numeric suffix-induced transformation */
  Lexer *lexer;
  int *stack;
  VALUE ident_tok = Qnil;
  long ident_ts = 0, ident_te = 0;
  long numeric_s = 0;
  Data_Get_Struct(self, Lexer, lexer);
  VALUE diag_msg = Qnil;

  if (RARRAY_LEN(lexer->token_queue) > 0)
    return rb_ary_shift(lexer->token_queue);

  cs = lexer->cs;
  p = lexer->p;
  pe = eof = lexer->pe;
  stack = lexer->cs_stack;
  top = lexer->cs_stack_top;

  cmd_state = lexer->command_start;
  lexer->command_start = 0;

  %%{
    write exec;
  }%%

  lexer->p = p;
  lexer->cs = cs;
  lexer->cs_stack_top = top;

  if (RARRAY_LEN(lexer->token_queue) > 0) {
    return rb_ary_shift(lexer->token_queue);
  } else if (cs == lex_error) {
    VALUE info  = rb_ary_new3(2, rb_str_new2("$error"), range(lexer, p - 1, p));
    VALUE token = rb_ary_new3(2, Qfalse, info);
    return token;
  } else {
    VALUE info  = rb_ary_new3(2, rb_str_new2("$eof"), range(lexer, eof - 2, eof - 2));
    VALUE token = rb_ary_new3(2, Qfalse, info);
    return token;
  }
}

static inline void force_encoding(VALUE str, VALUE enc)
{
  rb_enc_associate(str, rb_to_encoding(enc));
}

static void emit_token(Lexer *lexer, VALUE type, VALUE value, long start, long end)
{
  VALUE info  = rb_ary_new3(2, value, range(lexer, start, end));
  VALUE token = rb_ary_new3(2, type, info);

  rb_ary_push(lexer->token_queue, token);

  if (lexer->tokens != Qnil)
    rb_ary_push(lexer->tokens, token);
}

static void emit_comment(Lexer *lexer, long start, long end)
{
  VALUE rng = Qnil;

  if (lexer->tokens != Qnil) {
    rng = range(lexer, start, end);

    VALUE info  = rb_ary_new3(2, tok(lexer, start, end), rng);
    VALUE token = rb_ary_new3(2, tCOMMENT, info);
    rb_ary_push(lexer->tokens, token);
  }

  if (lexer->comments != Qnil) {
    if (rng == Qnil)
      rng = range(lexer, start, end);
    VALUE comment = rb_class_new_instance(1, &rng, comment_klass);
    rb_ary_push(lexer->comments, comment);
  }
}

static void emit_do(Lexer *lexer, int do_block, long ts, long te)
{
  if (stack_state_active(&lexer->cond))
    emit(kDO_COND);
  else if (stack_state_active(&lexer->cmdarg) || do_block)
    emit(kDO_BLOCK);
  else
    emit(kDO);
}

static VALUE tok(Lexer *lexer, long start, long end)
{
  return rb_str_substr(lexer->source, start, end - start);
}

static VALUE range(Lexer *lexer, long start, long end)
{
  VALUE args[3];
  args[0] = lexer->source_buffer;
  args[1] = INT2NUM(start);
  args[2] = INT2NUM(end);
  return rb_class_new_instance(3, args, range_klass);
}

static void diagnostic(Lexer *lexer, VALUE type, VALUE reason,
                       VALUE arguments, VALUE loc, VALUE hilights)
{
  VALUE args[5];
  args[0] = type;
  args[1] = reason;
  args[2] = arguments;
  args[3] = loc;
  args[4] = hilights;
  VALUE diagnostic = rb_class_new_instance(5, args, diagnostic_klass);
  rb_funcall(lexer->diagnostics, rb_intern("process"), 1, diagnostic);
}

static int get_codepoint(Lexer *lexer, long p)
{
  if (p >= RARRAY_LEN(lexer->source_pts))
    return 0;
  else
    return NUM2INT(rb_ary_entry(lexer->source_pts, p));
}

static int arg_or_cmdarg(int cmd_state)
{
  if (cmd_state) {
    return lex_en_expr_cmdarg;
  } else {
    return lex_en_expr_arg;
  }
}

static int is_nthref(VALUE str)
{
  char c;
  char *p = RSTRING_PTR(str);

  if (*p++ != '$') return 0;

  c = *p++;
  if (c < '1' || c > '9') return 0;

  while ((c = *p++)) {
    if (c < '0' || c > '9') return 0;
  }

  return 1;
}

static int is_backref(VALUE str)
{
  char c;
  char *p = RSTRING_PTR(str);

  if (*p++ != '$') return 0;

  c = *p++;
  if (c != '&' && c != '`' && c != '\'' && c != '+') return 0;

  return *p == '\0'; /* are we at end of string? */
}

static int is_capitalized(VALUE str)
{
  char *p = RSTRING_PTR(str);
  return *p >= 'A' && *p <= 'Z';
}

static int is_regexp_metachar(VALUE str)
{
  char c = *RSTRING_PTR(str);
  return c == '\\' || c == '$' || c == '(' || c == ')' || c == '*' ||
         c == '+'  || c == '.' || c == '<' || c == '>' || c == '?' ||
         c == '['  || c == ']' || c == '^' || c == '{' || c == '|' ||
         c == '}';
}

static int eof_codepoint(int codepoint)
{
  return codepoint == 0x04 || codepoint == 0x1a || codepoint == 0x00;
}

static VALUE find_unknown_options(VALUE str)
{
  char c, *p = RSTRING_PTR(str);
  VALUE result = Qnil;

  while ((c = *p++)) {
    if (c != 'i' && c != 'm' && c != 'x' && c != 'o' && c != 'u' && c != 'e' &&
        c != 's' && c != 'n') {
      if (result == Qnil) {
        result = rb_str_new(&c, 1);
      } else {
        rb_str_concat(result, rb_str_new(&c, 1));
      }
    }
  }

  return result;
}

static int bad_cvar_name(VALUE str)
{
  char *p = RSTRING_PTR(str);

  if (*p++ != '@') return 0;
  if (*p++ != '@') return 0;
  return *p >= '0' && *p <= '9';
}

static int bad_ivar_name(VALUE str)
{
  char *p = RSTRING_PTR(str);

  if (*p++ != '@') return 0;
  return *p >= '0' && *p <= '9';
}

static int find_8_or_9(VALUE str)
{
  int idx = 0;
  char *p = RSTRING_PTR(str);

  while (*p) {
    if (*p == '8' || *p == '9')
      return idx;
    idx++;
    p++;
  }

  return -1;
}

static void emit_int(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tINTEGER, val, start, end);
}

static void emit_rational(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tRATIONAL, rb_Rational1(val), start, end);
}

static void emit_complex(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tIMAGINARY, rb_Complex(Qzero, val), start, end);
}

static void emit_complex_rational(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tIMAGINARY, rb_Complex(Qzero, rb_Rational1(val)), start, end);
}

static void emit_float(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tFLOAT, rb_Float(val), start, end);
}

static void emit_complex_float(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tIMAGINARY, rb_Complex(Qzero, rb_Float(val)), start, end);
}

static void emit_int_followed_by_if(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tINTEGER, val, start, end);
}

static void emit_int_followed_by_rescue(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tINTEGER, val, start, end);
}

static void emit_float_followed_by_if(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tFLOAT, rb_Float(val), start, end);
}
static void emit_float_followed_by_rescue(Lexer *lexer, VALUE val, long start, long end)
{
  emit_token(lexer, tFLOAT, rb_Float(val), start, end);
}

static int next_state_for_literal(literal *lit) {
  if (literal_words_p(lit) && literal_backslash_delimited_p(lit)) {
    if (lit->interpolate) {
      return lex_en_interp_backslash_delimited_words;
    } else {
      return lex_en_plain_backslash_delimited_words;
    }
  } else if (literal_words_p(lit) && !literal_backslash_delimited_p(lit)) {
    if (lit->interpolate) {
      return lex_en_interp_words;
    } else {
      return lex_en_plain_words;
    }
  } else if (!literal_words_p(lit) && literal_backslash_delimited_p(lit)) {
    if (lit->interpolate) {
      return lex_en_interp_backslash_delimited;
    } else {
      return lex_en_plain_backslash_delimited;
    }
  } else {
    if (lit->interpolate) {
      return lex_en_interp_string;
    } else {
      return lex_en_plain_string;
    }
  }
}

static int push_literal(Lexer *lexer, VALUE str_type, VALUE delimiter,
                        long str_s, long heredoc_e, int indent, int dedent_body,
                        int label_allowed)
{
  literal lit;
  literal_init(&lit, lexer, str_type, delimiter, str_s, heredoc_e, indent,
               dedent_body, label_allowed);
  lit_stack_push(&lexer->literal_stack, lit);

  return next_state_for_literal(&lit);
}

static int pop_literal(Lexer *lexer)
{
  literal old_literal = lit_stack_pop(&lexer->literal_stack);

  lexer->dedent_level = old_literal.dedent_level;

  if (old_literal.start_tok == tREGEXP_BEG) {
    return lex_en_regexp_modifiers;
  } else {
    return lex_en_expr_end;
  }
}

static VALUE array_last(VALUE array)
{
  long len = RARRAY_LEN(array);
  if (len == 0)
    return Qnil;
  else
    return rb_ary_entry(array, len - 1);
}

static VALUE unescape_char(char c)
{
  switch (c) {
  case 'a': return rb_str_new("\a", 1);
  case 'b': return rb_str_new("\b", 1);
  case 'e': return rb_str_new("\e", 1);
  case 'f': return rb_str_new("\f", 1);
  case 'n': return rb_str_new("\n", 1);
  case 'r': return rb_str_new("\r", 1);
  case 's': return rb_str_new(" ", 1);
  case 't': return rb_str_new("\t", 1);
  case 'v': return rb_str_new("\v", 1);
  default:  return Qnil;
  }
}

static VALUE escape_char(VALUE str)
{
  char c = *RSTRING_PTR(str);

  switch (c) {
  case '\f': return rb_str_new("\\f", 2);
  case '\n': return rb_str_new("\\n", 2);
  case '\r': return rb_str_new("\\r", 2);
  case ' ':  return rb_str_new("\\s", 2);
  case '\t': return rb_str_new("\\t", 2);
  case '\v': return rb_str_new("\\v", 2);
  default:   return Qnil;
  }
}

// assumes that pattern contains only one char
static inline int str_start_with_p(VALUE str, const char *pattern)
{
  if (RSTRING_LEN(str) == 0) {
    return 0;
  }
  return (memcmp(RSTRING_PTR(str), pattern, 1) == 0);
}

// assumes that pattern contains only one char
static inline int str_end_with_p(VALUE str, const char *pattern)
{
  if (RSTRING_LEN(str) == 0) {
    return 0;
  }
  return (memcmp(RSTRING_PTR(str) + RSTRING_LEN(str) - 1, pattern, 1) == 0);
}

def_lexer_attribute(diagnostics);
def_lexer_attribute(static_env);
def_lexer_attribute(tokens);
def_lexer_attribute(comments);
def_lexer_attribute(encoding);

def_lexer_attr_reader(source_buffer);

void Init_lexer()
{
  init_symbol(k__ENCODING__);
  init_symbol(k__FILE__);
  init_symbol(k__LINE__);
  init_symbol(kALIAS);
  init_symbol(kAND);
  init_symbol(kBEGIN);
  init_symbol(klBEGIN);
  init_symbol(kBREAK);
  init_symbol(kCASE);
  init_symbol(kCLASS);
  init_symbol(kDEF);
  init_symbol(kDEFINED);
  init_symbol(kDO);
  init_symbol(kDO_BLOCK);
  init_symbol(kDO_COND);
  init_symbol(kDO_LAMBDA);
  init_symbol(kELSE);
  init_symbol(kELSIF);
  init_symbol(kEND);
  init_symbol(klEND);
  init_symbol(kENSURE);
  init_symbol(kFALSE);
  init_symbol(kFOR);
  init_symbol(kIF);
  init_symbol(kIF_MOD);
  init_symbol(kIN);
  init_symbol(kMODULE);
  init_symbol(kNEXT);
  init_symbol(kNIL);
  init_symbol(kNOT);
  init_symbol(kOR);
  init_symbol(kREDO);
  init_symbol(kRESCUE);
  init_symbol(kRESCUE_MOD);
  init_symbol(kRETRY);
  init_symbol(kRETURN);
  init_symbol(kSELF);
  init_symbol(kSUPER);
  init_symbol(kTHEN);
  init_symbol(kTRUE);
  init_symbol(kUNDEF);
  init_symbol(kUNLESS);
  init_symbol(kUNLESS_MOD);
  init_symbol(kUNTIL);
  init_symbol(kUNTIL_MOD);
  init_symbol(kWHEN);
  init_symbol(kWHILE);
  init_symbol(kWHILE_MOD);
  init_symbol(kYIELD);

  init_symbol(tAMPER);
  init_symbol(tAMPER2);
  init_symbol(tANDDOT);
  init_symbol(tANDOP);
  init_symbol(tAREF);
  init_symbol(tASET);
  init_symbol(tASSOC);
  init_symbol(tBACK_REF);
  init_symbol(tBACK_REF2);
  init_symbol(tBANG);
  init_symbol(tBDOT2);
  init_symbol(tBDOT3);
  init_symbol(tCARET);
  init_symbol(tCHARACTER);
  init_symbol(tCMP);
  init_symbol(tCOLON);
  init_symbol(tCOLON2);
  init_symbol(tCOLON3);
  init_symbol(tCOMMA);
  init_symbol(tCOMMENT);
  init_symbol(tCONSTANT);
  init_symbol(tCVAR);
  init_symbol(tDIVIDE);
  init_symbol(tDOT);
  init_symbol(tDOT2);
  init_symbol(tDOT3);
  init_symbol(tDSTAR);
  init_symbol(tEH);
  init_symbol(tEQ);
  init_symbol(tEQL);
  init_symbol(tEQQ);
  init_symbol(tFID);
  init_symbol(tFLOAT);
  init_symbol(tGEQ);
  init_symbol(tGT);
  init_symbol(tGVAR);
  init_symbol(tIDENTIFIER);
  init_symbol(tIMAGINARY);
  init_symbol(tINTEGER);
  init_symbol(tIVAR);
  init_symbol(tLABEL);
  init_symbol(tLABEL_END);
  init_symbol(tLAMBDA);
  init_symbol(tLAMBEG);
  init_symbol(tLBRACE);
  init_symbol(tLBRACE_ARG);
  init_symbol(tLBRACK);
  init_symbol(tLBRACK2);
  init_symbol(tLCURLY);
  init_symbol(tLEQ);
  init_symbol(tLPAREN);
  init_symbol(tLPAREN2);
  init_symbol(tLPAREN_ARG);
  init_symbol(tLSHFT);
  init_symbol(tLT);
  init_symbol(tMATCH);
  init_symbol(tMETHREF);
  init_symbol(tMINUS);
  init_symbol(tNEQ);
  init_symbol(tNL);
  init_symbol(tNMATCH);
  init_symbol(tNTH_REF);
  init_symbol(tNUMPARAM);
  init_symbol(tOP_ASGN);
  init_symbol(tOROP);
  init_symbol(tPERCENT);
  init_symbol(tPIPE);
  init_symbol(tPLUS);
  init_symbol(tPOW);
  init_symbol(tQSYMBOLS_BEG);
  init_symbol(tQWORDS_BEG);
  init_symbol(tRATIONAL);
  init_symbol(tRBRACK);
  init_symbol(tRCURLY);
  init_symbol(tREGEXP_BEG);
  init_symbol(tREGEXP_OPT);
  init_symbol(tRPAREN);
  init_symbol(tRSHFT);
  init_symbol(tSEMI);
  init_symbol(tSPACE);
  init_symbol(tSTAR);
  init_symbol(tSTAR2);
  init_symbol(tSTRING);
  init_symbol(tSTRING_BEG);
  init_symbol(tSTRING_CONTENT);
  init_symbol(tSTRING_DBEG);
  init_symbol(tSTRING_DEND);
  init_symbol(tSTRING_DVAR);
  init_symbol(tSTRING_END);
  init_symbol(tSYMBEG);
  init_symbol(tSYMBOL);
  init_symbol(tSYMBOLS_BEG);
  init_symbol(tTILDE);
  init_symbol(tUMINUS);
  init_symbol(tUNARY_NUM);
  init_symbol(tUPLUS);
  init_symbol(tWORDS_BEG);
  init_symbol(tXSTRING_BEG);

  severity_error = ID2SYM(rb_intern("error"));
  rb_gc_register_address(&severity_error);
  init_symbol(fatal);
  init_symbol(warning);

  init_symbol(ambiguous_literal);
  init_symbol(ambiguous_prefix);
  init_symbol(bare_backslash);
  init_symbol(character);
  init_symbol(cvar_name);
  init_symbol(embedded_document);
  init_symbol(empty_numeric);
  init_symbol(escape_eof);
  init_symbol(heredoc_id_ends_with_nl);
  init_symbol(heredoc_id_has_newline);
  init_symbol(incomplete_escape);
  init_symbol(invalid_escape);
  init_symbol(invalid_escape_use);
  init_symbol(invalid_hex_escape);
  init_symbol(invalid_octal);
  init_symbol(invalid_unicode_escape);
  init_symbol(ivar_name);
  init_symbol(leading_zero_in_numparam);
  init_symbol(no_dot_digit_literal);
  init_symbol(numparam_outside_block);
  init_symbol(ordinary_param_defined);
  init_symbol(prefix);
  init_symbol(regexp_options);
  init_symbol(string_eof);
  init_symbol(too_large_numparam);
  init_symbol(trailing_in_number);
  init_symbol(unexpected);
  init_symbol(unexpected_percent_str);
  init_symbol(unicode_point_too_large);
  init_symbol(unterminated_heredoc_id);
  init_symbol(unterminated_unicode);

  VALUE m_Parser = rb_define_module("Parser");
  VALUE c_Lexer  = rb_define_class_under(m_Parser, "CLexer", rb_cObject);

  rb_define_alloc_func(c_Lexer, lexer_alloc);

  rb_define_method(c_Lexer, "initialize", lexer_initialize,    1);
  rb_define_method(c_Lexer, "advance",    lexer_advance, 0);
  rb_define_method(c_Lexer, "reset",      lexer_reset,   -1);

  rb_define_method(c_Lexer, "push_cmdarg", lexer_push_cmdarg, 0);
  rb_define_method(c_Lexer, "pop_cmdarg",  lexer_pop_cmdarg,  0);
  rb_define_method(c_Lexer, "push_cond",   lexer_push_cond,   0);
  rb_define_method(c_Lexer, "pop_cond",    lexer_pop_cond,    0);

  rb_define_method(c_Lexer, "push_cmdarg_state",   lexer_push_cmdarg_state, 1);
  rb_define_method(c_Lexer, "pop_cmdarg_state",    lexer_pop_cmdarg_state, 0);
  rb_define_method(c_Lexer, "lexpop_cmdarg_state", lexer_lexpop_cmdarg_state, 0);
  rb_define_method(c_Lexer, "clear_cmdarg_state",  lexer_clear_cmdarg_state, 0);
  rb_define_method(c_Lexer, "cmdarg_state_empty?", lexer_cmdarg_state_empty_p, 0);
  rb_define_method(c_Lexer, "cmdarg_state_value",  lexer_cmdarg_state_value, 0);
  rb_define_method(c_Lexer, "set_cmdarg_state",    lexer_set_cmdarg_state, 1);

  rb_define_method(c_Lexer, "push_cond_state",   lexer_push_cond_state, 1);
  rb_define_method(c_Lexer, "pop_cond_state",    lexer_pop_cond_state, 0);
  rb_define_method(c_Lexer, "lexpop_cond_state", lexer_lexpop_cond_state, 0);
  rb_define_method(c_Lexer, "clear_cond_state",  lexer_clear_cond_state, 0);
  rb_define_method(c_Lexer, "cond_state_empty?", lexer_cond_state_empty_p, 0);
  rb_define_method(c_Lexer, "cond_state_value",  lexer_cond_state_value, 0);
  rb_define_method(c_Lexer, "set_cond_state",    lexer_set_cond_state, 1);

  rb_define_method(c_Lexer, "state",          lexer_get_state,       0);
  rb_define_method(c_Lexer, "state=",         lexer_set_state,       1);
  rb_define_method(c_Lexer, "in_kwarg",       lexer_get_in_kwarg,    0);
  rb_define_method(c_Lexer, "in_kwarg=",      lexer_set_in_kwarg,    1);
  rb_define_method(c_Lexer, "diagnostics",    lexer_get_diagnostics, 0);
  rb_define_method(c_Lexer, "diagnostics=",   lexer_set_diagnostics, 1);
  rb_define_method(c_Lexer, "static_env",     lexer_get_static_env,  0);
  rb_define_method(c_Lexer, "static_env=",    lexer_set_static_env,  1);
  rb_define_method(c_Lexer, "tokens",         lexer_get_tokens,      0);
  rb_define_method(c_Lexer, "tokens=",        lexer_set_tokens,      1);
  rb_define_method(c_Lexer, "comments",       lexer_get_comments,    0);
  rb_define_method(c_Lexer, "comments=",      lexer_set_comments,    1);
  rb_define_method(c_Lexer, "encoding",       lexer_get_encoding,    0);
  rb_define_method(c_Lexer, "encoding=",      lexer_set_encoding,    1);
  rb_define_method(c_Lexer, "dedent_level",   lexer_get_dedent_level,  0);
  rb_define_method(c_Lexer, "source_buffer",  lexer_get_source_buffer, 0);
  rb_define_method(c_Lexer, "source_buffer=", lexer_set_source_buffer, 1);
  rb_define_method(c_Lexer, "force_utf32=",   lexer_set_force_utf32,   1);

  rb_define_method(c_Lexer, "max_numparam_stack", lexer_max_numparam_stack, 0);
  rb_define_method(c_Lexer, "max_numparam",       lexer_max_numparam,    0);

  rb_define_attr(c_Lexer, "context", 1, 1);

  VALUE m_Source   = rb_const_get(m_Parser, rb_intern("Source"));
  comment_klass    = rb_const_get(m_Source, rb_intern("Comment"));
  diagnostic_klass = rb_const_get(m_Parser, rb_intern("Diagnostic"));
  range_klass      = rb_const_get(m_Source, rb_intern("Range"));
  VALUE lexer_class = rb_const_get(m_Parser, rb_intern("Lexer"));
  max_numparam_stack_klass = rb_const_get(lexer_class, rb_intern("MaxNumparamStack"));

  empty_array = rb_obj_freeze(rb_ary_new2(0));
  rb_gc_register_address(&empty_array);
  blank_string = rb_obj_freeze(rb_str_new2(""));
  rb_gc_register_address(&blank_string);
  newline = rb_obj_freeze(rb_str_new2("\n"));
  rb_gc_register_address(&newline);
  escaped_newline = rb_obj_freeze(rb_str_new2("\\\n"));
  rb_gc_register_address(&escaped_newline);
  slash_r = rb_obj_freeze(rb_str_new2("\r"));
  rb_gc_register_address(&slash_r);

  if (rb_const_defined(rb_cObject, rb_intern("Encoding"))) {
    VALUE encoding = rb_const_get(rb_cObject, rb_intern("Encoding"));
    utf8_encoding  = rb_const_get(encoding, rb_intern("UTF_8"));
    rb_gc_register_address(&utf8_encoding);
  } else {
    utf8_encoding = Qnil;
  }

  VALUE regex_str = rb_str_new2("\\r.*$");
  cr_then_anything_to_eol = rb_class_new_instance(1, &regex_str, rb_cRegexp);
  rb_gc_register_address(&cr_then_anything_to_eol);
  regex_str = rb_str_new2("\\r+$");
  crs_to_eol = rb_class_new_instance(1, &regex_str, rb_cRegexp);
  rb_gc_register_address(&crs_to_eol);
}

%%{
  alphtype int;
  getkey (get_codepoint(lexer, p));

  prepush {
    /* grow the state stack as needed */
    if (lexer->cs_stack_top == lexer->cs_stack_size) {
      int *new_stack = xmalloc(lexer->cs_stack_size * 2 * sizeof(int));
      memcpy(new_stack, lexer->cs_stack, lexer->cs_stack_size * sizeof(int));
      xfree(lexer->cs_stack);
      stack = lexer->cs_stack = new_stack;
      lexer->cs_stack_size = lexer->cs_stack_size * 2;
    }
  }

  action do_nl { lexer->newline_s = p; }

  c_nl       = '\n' $ do_nl;
  c_space    = [ \t\r\f\v];
  c_space_nl = c_space | c_nl;

  c_eof      = 0x04 | 0x1a | 0 | zlen; # ^D, ^Z, \0, EOF
  c_eol      = c_nl | c_eof;
  c_any      = any - c_eof;

  c_nl_zlen  = c_nl | zlen;
  c_line     = any - c_nl_zlen;

  c_unicode  = c_any - 0x00..0x7f;
  c_upper    = [A-Z];
  c_lower    = [a-z_]  | c_unicode;
  c_alpha    = c_lower | c_upper;
  c_alnum    = c_alpha | [0-9];

  action do_eof { fhold; fbreak; }

  operator_fname      = '[]' | '[]=' | '`'  | '-@' | '+@' | '~@'  | '!@' ;
  operator_arithmetic = '&'  | '|'   | '&&' | '||' | '^'  | '+'   | '-'  |
                        '*'  | '/'   | '**' | '~'  | '<<' | '>>'  | '%'  ;
  operator_rest       = '=~' | '!~' | '==' | '!=' | '!'   | '===' |
                        '<'  | '<=' | '>'  | '>=' | '<=>' | '=>'  ;

  punctuation_begin   = '-'  | '+'  | '::' | '('  | '['  |
                        '*'  | '**' | '&'  ;
  punctuation_end     = ','  | '='  | '->' | '('  | '['  | ']'   |
                        '::' | '?'  | ':'  | '.'  | '..' | '...' ;

  keyword_modifier    = 'if'     | 'unless' | 'while'  | 'until' | 'rescue' ;
  keyword_with_arg    = 'yield'  | 'super'  | 'not'    | 'defined?' ;
  keyword_with_fname  = 'def'    | 'undef'  | 'alias'  ;
  keyword_with_value  = 'else'   | 'case'   | 'ensure' | 'module' | 'elsif' | 'then'  |
                        'for'    | 'in'     | 'do'     | 'when'   | 'begin' | 'class' |
                        'and'    | 'or'     ;
  keyword_with_mid    = 'rescue' | 'return' | 'break'  | 'next'   ;
  keyword_with_end    = 'end'    | 'self'   | 'true'   | 'false'  | 'retry'    |
                        'redo'   | 'nil'    | 'BEGIN'  | 'END'    | '__FILE__' |
                        '__LINE__' | '__ENCODING__';

  keyword             = keyword_with_value | keyword_with_mid |
                        keyword_with_end   | keyword_with_arg |
                        keyword_with_fname | keyword_modifier ;

  constant       = c_upper c_alnum*;
  bareword       = c_alpha c_alnum*;

  call_or_var    = c_lower c_alnum*;
  class_var      = '@@' bareword;
  instance_var   = '@' bareword;
  global_var     = '$'
      ( bareword | digit+
      | [`'+~*$&?!@/\\;,.=:<>"] # '
      | '-' c_alnum
      )
  ;

  class_var_v    = '@@' c_alnum+;
  instance_var_v = '@' c_alnum+;

  label          = bareword [?!]? ':';

  int_hex  = ( xdigit+ '_' )* xdigit* '_'? ;
  int_dec  = ( digit+ '_' )* digit* '_'? ;
  int_bin  = ( [01]+ '_' )* [01]* '_'? ;

  flo_int  = [1-9] [0-9]* ( '_' digit+ )* | '0';
  flo_frac = '.' ( digit+ '_' )* digit+;
  flo_pow  = [eE] [+\-]? ( digit+ '_' )* digit+;

  int_suffix =
    ''       % { num_xfrm = emit_int;                    numeric_s = ts;         }
  | 'r'      % { num_xfrm = emit_rational;               numeric_s = ts;         }
  | 'i'      % { num_xfrm = emit_complex;                numeric_s = ts;         }
  | 'ri'     % { num_xfrm = emit_complex_rational;       numeric_s = ts;         }
  | 'if'     % { num_xfrm = emit_int_followed_by_if;     numeric_s = ts; p -= 2; }
  | 'rescue' % { num_xfrm = emit_int_followed_by_rescue; numeric_s = ts; p -= 6; } ;

  flo_pow_suffix =
    ''   % { num_xfrm = emit_float;                numeric_s = ts;         }
  | 'i'  % { num_xfrm = emit_complex_float;        numeric_s = ts;         }
  | 'if' % { num_xfrm = emit_float_followed_by_if; numeric_s = ts; p -= 2; };

  flo_suffix =
    flo_pow_suffix
  | 'r'      % { num_xfrm = emit_rational;                 numeric_s = ts;         }
  | 'ri'     % { num_xfrm = emit_complex_rational;         numeric_s = ts;         }
  | 'rescue' % { num_xfrm = emit_float_followed_by_rescue; numeric_s = ts; p -= 6; };

  escaped_nl = "\\" c_nl;

  action unicode_points {
    lexer->escape = rb_str_new2("");

    VALUE codepoints = tok(lexer, lexer->escape_s + 2, p - 1);
    long codepoint_s = lexer->escape_s + 2;

    VALUE regexp;

    if (lexer->version < 24) {
      if (str_start_with_p(codepoints, " ") || str_start_with_p(codepoints, "\t")) {
        diagnostic(lexer, severity_error, invalid_unicode_escape, Qnil,
                     range(lexer, lexer->escape_s + 2, lexer->escape_s + 3), empty_array);
      }

      regexp = rb_reg_regcomp(rb_str_new2("[ \\t]{2}"));
      VALUE space_p = rb_funcall(codepoints, rb_intern("index"), 1, regexp);

      if (RTEST(space_p)) {
        diagnostic(lexer, severity_error, invalid_unicode_escape, Qnil,
                     range(lexer, codepoint_s + NUM2INT(space_p) + 1, codepoint_s + NUM2INT(space_p) + 1), empty_array);
      }

      if (str_end_with_p(codepoints, " ") || str_end_with_p(codepoints, "\t")) {
        diagnostic(lexer, severity_error, invalid_unicode_escape, Qnil,
                     range(lexer, p - 1, p), empty_array);
      }
    }

    regexp = rb_reg_regcomp(rb_str_new2("([0-9a-fA-F]+)|([ \t]+)"));

    VALUE matches = rb_funcall(codepoints, rb_intern("scan"), 1, regexp);
    long len = RARRAY_LEN(matches);

    for (long i = 0; i < len; i++) {
      VALUE match = RARRAY_AREF(matches, i);
      VALUE codepoint_str = RARRAY_AREF(match, 0);
      VALUE spaces = RARRAY_AREF(match, 1);

      if (RTEST(spaces)) {
        codepoint_s += RSTRING_LEN(spaces);
      } else {
        VALUE codepoint = rb_str_to_inum(codepoint_str, 16, 0);
        if (NUM2INT(codepoint) >= 0x110000) {
          diagnostic(lexer, severity_error, unicode_point_too_large, Qnil,
                     range(lexer, codepoint_s, codepoint_s + RSTRING_LEN(codepoint_str)), empty_array);
          break;
        }

        codepoint = rb_funcall(codepoint, rb_intern("chr"), 1, utf8_encoding);
        lexer->escape = rb_str_plus(lexer->escape, codepoint);
        codepoint_s += RSTRING_LEN(codepoint_str);
      }
    }
  }

  action unescape_char {
    char c = NUM2INT(rb_ary_entry(lexer->source_pts, p - 1));
    lexer->escape = unescape_char(c);

    if (lexer->escape == Qnil) {
      VALUE codepoint = rb_funcall(lexer->source_buffer, rb_intern("slice"), 1, INT2NUM(p - 1));
      lexer->escape = codepoint;
      force_encoding(codepoint, lexer->encoding);
    }
  }

  action invalid_complex_escape {
    diagnostic(lexer, fatal, invalid_escape, Qnil, range(lexer, ts, te),
               empty_array);
  }

  action read_post_meta_or_ctrl_char {
    VALUE codepoint = rb_funcall(lexer->source_buffer, rb_intern("slice"), 1, INT2NUM(p - 1));
    lexer->escape = rb_funcall(codepoint, rb_intern("chr"), 0);
    int codepoint_i = FIX2INT(rb_funcall(codepoint, rb_intern("ord"), 0));

    if (
      lexer->version >= 27 && (
        (codepoint_i >= 0 && codepoint_i <= 8) ||
        (codepoint_i >= 14 && codepoint_i <= 31)
      )
    ) {
      diagnostic(lexer, fatal, invalid_escape, Qnil, range(lexer, ts, te), empty_array);
    }
  }

  action slash_c_char {
    char c = *RSTRING_PTR(lexer->escape) & 0x9f;
    lexer->escape = rb_str_new(&c, 1);
    force_encoding(lexer->escape, lexer->encoding);
  }

  action slash_m_char {
    char c = *RSTRING_PTR(lexer->escape) | 0x80;
    lexer->escape = rb_str_new(&c, 1);
    force_encoding(lexer->escape, lexer->encoding);
  }

  maybe_escaped_char = (
        '\\' c_any      %unescape_char
    | ( c_any - [\\] )  %read_post_meta_or_ctrl_char
  );

  maybe_escaped_ctrl_char = (
        '\\' c_any      %unescape_char %slash_c_char
    |   '?'             % { lexer->escape = rb_str_new2("\x7f"); }
    | ( c_any - [\\?] ) %read_post_meta_or_ctrl_char %slash_c_char
  );

  escape = (
      [0-7]{1,3} % {
        VALUE token = tok(lexer, lexer->escape_s, p);
        char c = NUM2INT(rb_str_to_inum(token, 8, 0));
        c = c % 0x100;
        lexer->escape = rb_str_new(&c, 1);
        force_encoding(lexer->escape, lexer->encoding);
      }

    | 'x' xdigit{1,2} % {
        VALUE token = tok(lexer, lexer->escape_s + 1, p);
        char c = NUM2INT(rb_str_to_inum(token, 16, 0));
        lexer->escape = rb_str_new(&c, 1);
        force_encoding(lexer->escape, lexer->encoding);
      }

    | 'x' ( c_any - xdigit )
      % {
        diagnostic(lexer, fatal, invalid_hex_escape, Qnil,
                   range(lexer, lexer->escape_s - 1, p + 2), empty_array);
      }

    | 'u' xdigit{4} % {
        VALUE token = tok(lexer, lexer->escape_s + 1, p);
        int i = NUM2INT(rb_str_to_inum(token, 16, 0));
        lexer->escape = rb_enc_uint_chr(i, rb_to_encoding(utf8_encoding));
      }

    | 'u' xdigit{0,3} % {
        diagnostic(lexer, fatal, invalid_unicode_escape, Qnil,
                   range(lexer, lexer->escape_s - 1, p), empty_array);
      }

    | 'u{' ( c_any - xdigit - [ \t}] )* '}' % {
        diagnostic(lexer, fatal, invalid_unicode_escape, Qnil,
                   range(lexer, lexer->escape_s - 1, p), empty_array);
      }

    | 'u{' [ \t]* ( xdigit{1,6} [ \t]+ )*
      (
        ( xdigit{1,6} [ \t]* '}'
          %unicode_points
        )
        |
        ( xdigit* ( c_any - xdigit - [ \t}] )+ '}'
          | ( c_any - [ \t}] )* c_eof
          | xdigit{7,}
        ) % {
          diagnostic(lexer, fatal, invalid_unicode_escape, Qnil,
                   range(lexer, p - 1, p), empty_array);
        }
      )

    | ( 'C-' | 'c' ) escaped_nl?
      maybe_escaped_ctrl_char

    | 'M-' escaped_nl?
      maybe_escaped_char
      %slash_m_char

    | ( ( 'C-'   | 'c' ) escaped_nl?   '\\M-'
      |   'M-\\'         escaped_nl? ( 'C-'   | 'c' ) ) escaped_nl?
      maybe_escaped_ctrl_char
      %slash_m_char

    | 'C' c_any %invalid_complex_escape
    | 'M' c_any %invalid_complex_escape
    | ( 'M-\\C' | 'C-\\M' ) c_any %invalid_complex_escape

    | ( c_any - [0-7xuCMc] ) %unescape_char

    | c_eof % {
        diagnostic(lexer, fatal, escape_eof, Qnil, range(lexer, p - 1, p),
                   empty_array);
      }
  );

  e_bs = '\\' % {
    lexer->escape_s = p;
    lexer->escape = Qnil;
  };

  e_heredoc_nl = c_nl % {
    if (lexer->herebody_s) {
      p = lexer->herebody_s;
      lexer->herebody_s = 0;
    }
  };

  action extend_string {
    VALUE string = tok(lexer, ts, te);
    VALUE lookahead = Qnil;

    if (lexer->version >= 22 && !stack_state_active(&lexer->cond)) {
      lookahead = tok(lexer, te, te + 2);
    }

    literal *current_literal = lit_stack_top(&lexer->literal_stack);

    if (!current_literal->heredoc_e &&
         literal_nest_and_try_closing(current_literal, string, ts, te, lookahead)) {
      VALUE token = array_last(lexer->token_queue);
      if (rb_ary_entry(token, 0) == tLABEL_END) {
        p += 1;
        pop_literal(lexer);
        fnext expr_labelarg;
      } else {
        fnext *pop_literal(lexer);
      }

      fbreak;
    } else {
      literal_extend_string(current_literal, string, ts, te);
    }
  }

  action extend_string_escaped {
    literal *current_literal = lit_stack_top(&lexer->literal_stack);
    VALUE escaped_char = rb_str_substr(lexer->source, lexer->escape_s, 1);

    if (literal_munge_escape_p(current_literal, escaped_char)) {
      if (literal_regexp_p(current_literal) && is_regexp_metachar(escaped_char)) {
        literal_extend_string(current_literal, tok(lexer, ts, te), ts, te);
      } else {
        literal_extend_string(current_literal, escaped_char, ts, te);
      }
    } else {
      if (literal_squiggly_heredoc_p(current_literal) && newline_char_p(escaped_char)) {
        literal_extend_string(current_literal, tok(lexer, ts, te), ts, te);
      } else if (literal_supports_line_continuation_via_slash_p(current_literal) && newline_char_p(escaped_char)) {
        VALUE token = tok(lexer, ts, te);
        rb_funcall(token, rb_intern("gsub!"), 2, escaped_newline, blank_string);
        literal_extend_string(current_literal, token, ts, te);
      } else if (literal_regexp_p(current_literal)) {
        VALUE token = tok(lexer, ts, te);
        rb_funcall(token, rb_intern("gsub!"), 2, escaped_newline, blank_string);
        literal_extend_string(current_literal, token, ts, te);
      } else if (lexer->escape == Qnil) {
        literal_extend_string(current_literal, tok(lexer, ts, te), ts, te);
      } else {
        literal_extend_string(current_literal, lexer->escape, ts, te);
      }
    }
  }

  action extend_string_eol {
    literal *current_literal = lit_stack_top(&lexer->literal_stack);
    long str_s = current_literal->str_s;

    if (te == pe) {
      diagnostic(lexer, fatal, string_eof, Qnil,
                 range(lexer, str_s, str_s + 1), empty_array);
    }

    if (literal_heredoc_p(current_literal)) {
      VALUE line = tok(lexer, lexer->herebody_s, ts);
      rb_funcall(line, rb_intern("gsub!"), 2, crs_to_eol, blank_string);

      if (lexer->version >= 18 && lexer->version <= 20) {
        rb_funcall(line, rb_intern("gsub!"), 2, cr_then_anything_to_eol, blank_string);
      }

      if (literal_nest_and_try_closing(current_literal, line, lexer->herebody_s, ts, Qnil)) {
        lexer->herebody_s = te;
        p = current_literal->heredoc_e - 1;
        fnext *pop_literal(lexer); fbreak;
      } else {
        literal_infer_indent_level(current_literal, line);
        lexer->herebody_s = te;
      }
    } else {
      if (literal_nest_and_try_closing(current_literal, tok(lexer, ts, te), ts, te, Qnil)) {
        fnext *pop_literal(lexer); fbreak;
      }

      if (lexer->herebody_s) {
        p = lexer->herebody_s - 1;
        lexer->herebody_s = 0;
      }
    }

    if (literal_words_p(current_literal) && !eof_codepoint(get_codepoint(lexer, p))) {
      literal_extend_space(current_literal, ts, te);
    } else {
      literal_extend_string(current_literal, tok(lexer, ts, te), ts, te);
      literal_flush_string(current_literal);
    }
  }

  action extend_string_space {
    literal *current_literal = lit_stack_top(&lexer->literal_stack);
    literal_extend_space(current_literal, ts, te);
  }

  interp_var = '#' ( global_var | class_var_v | instance_var_v );

  action extend_interp_var {
    literal *current_literal = lit_stack_top(&lexer->literal_stack);
    literal_flush_string(current_literal);
    literal_extend_content(current_literal);

    emit_token(lexer, tSTRING_DVAR, Qnil, ts, ts + 1);

    p = ts;
    fcall expr_variable;
  }

  interp_code = '#{';

  e_lbrace = '{' % {
    stack_state_push(&lexer->cond, 0);
    stack_state_push(&lexer->cmdarg, 0);

    literal *current_literal = lit_stack_top(&lexer->literal_stack);
    if (current_literal != NULL) {
      literal_start_interp_brace(current_literal);
    }
  };

  e_rbrace = '}' % {
    literal *current_literal = lit_stack_top(&lexer->literal_stack);
    if (current_literal != NULL) {
      if (literal_end_interp_brace_and_try_closing(current_literal)) {
        if (lexer->version == 18 || lexer->version == 19) {
          emit_token(lexer, tRCURLY, rb_str_new2("}"), p - 1, p);
          stack_state_pop(&lexer->cond);
          stack_state_pop(&lexer->cmdarg);
        } else {
          emit_token(lexer, tSTRING_DEND, rb_str_new2("}"), p - 1, p);
        }

        if (current_literal->herebody_s) {
          lexer->herebody_s = current_literal->herebody_s;
        }

        fhold;
        fnext *next_state_for_literal(current_literal);
        fbreak;
      }
    }
  };

  action extend_interp_code {
    literal *current_literal = lit_stack_top(&lexer->literal_stack);
    literal_flush_string(current_literal);
    literal_extend_content(current_literal);

    emit_token(lexer, tSTRING_DBEG, rb_str_new2("#{"), ts, te);

    if (current_literal->heredoc_e) {
      current_literal->herebody_s = lexer->herebody_s;
      lexer->herebody_s = 0;
    }

    literal_start_interp_brace(current_literal);
    lexer->command_start = 1;
    fnext expr_value;
    fbreak;
  }

  interp_words := |*
      interp_code => extend_interp_code;
      interp_var  => extend_interp_var;
      e_bs escape => extend_string_escaped;
      c_space+    => extend_string_space;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  interp_string := |*
      interp_code => extend_interp_code;
      interp_var  => extend_interp_var;
      e_bs escape => extend_string_escaped;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  plain_words := |*
      e_bs c_any  => extend_string_escaped;
      c_space+    => extend_string_space;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  plain_string := |*
      '\\' c_nl   => extend_string_eol;
      e_bs c_any  => extend_string_escaped;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  interp_backslash_delimited := |*
      interp_code => extend_interp_code;
      interp_var  => extend_interp_var;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  plain_backslash_delimited := |*
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  interp_backslash_delimited_words := |*
      interp_code => extend_interp_code;
      interp_var  => extend_interp_var;
      c_space+    => extend_string_space;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  plain_backslash_delimited_words := |*
      c_space+    => extend_string_space;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  regexp_modifiers := |*
      [A-Za-z]+
      => {
        VALUE unknown_options = find_unknown_options(tok(lexer, ts, te));

        if (unknown_options != Qnil) {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, ID2SYM(rb_intern("options")), unknown_options);
          diagnostic(lexer, severity_error, regexp_options, hash,
                     range(lexer, ts, te), empty_array);
        }

        emit(tREGEXP_OPT);
        fnext expr_end;
        fbreak;
      };

      any
      => {
        emit_token(lexer, tREGEXP_OPT, tok(lexer, ts, te - 1), ts, te - 1);
        fhold;
        fgoto expr_end;
      };
  *|;

  w_space =
      c_space+
    | '\\' e_heredoc_nl
    ;

  w_comment =
      '#'     %{ sharp_s = p - 1; }
      c_line* %{ emit_comment(lexer, sharp_s, p == pe ? p - 2 : p); }
    ;

  w_space_comment =
      w_space
    | w_comment
    ;

  w_newline =
      e_heredoc_nl;

  w_any =
      w_space
    | w_comment
    | w_newline
    ;

  ambiguous_fid_suffix =
    [?!]     %{ tm = p; }      |
    [?!]'='  %{ tm = p - 2; }
  ;

  ambiguous_ident_suffix =
    ambiguous_fid_suffix    |
    '='   %{ tm = p; }      |
    '=='  %{ tm = p - 2; }  |
    '=~'  %{ tm = p - 2; }  |
    '=>'  %{ tm = p - 2; }  |
    '===' %{ tm = p - 3; }
  ;

  ambiguous_symbol_suffix =
    ambiguous_ident_suffix |
    '==>' %{ tm = p - 2; }
  ;

  ambiguous_const_suffix =
    '::'  %{ tm = p - 2; }
  ;

  e_lbrack = '[' % {
    stack_state_push(&lexer->cond, 0);
    stack_state_push(&lexer->cmdarg, 0);
  };

  e_lparen = '(' % {
    stack_state_push(&lexer->cond, 0);
    stack_state_push(&lexer->cmdarg, 0);
    lexer->paren_nest += 1;

    if (lexer->version == 18) {
      lexer->command_start = 1;
    }
  };

  e_rparen = ')' % {
    lexer->paren_nest -= 1;
  };

  action local_ident {
    VALUE str = tok(lexer, ts, te);
    emit(tIDENTIFIER);

    if (STATIC_ENV_DECLARED(str)) {
      fnext expr_endfn; fbreak;
    } else {
      fnext *arg_or_cmdarg(cmd_state); fbreak;
    }
  }

  expr_variable := |*
      global_var => {
        VALUE str = tok(lexer, ts, te);

        if (is_nthref(str)) {
          VALUE integer = rb_str_to_inum(tok(lexer, ts + 1, te), 10, 0);
          emit_token(lexer, tNTH_REF, integer, ts, te);
        } else if (is_backref(str)) {
          emit(tBACK_REF);
        } else {
          emit(tGVAR);
        }

        fnext *stack[--top]; fbreak;
      };

      class_var_v => {
        VALUE str = tok(lexer, ts, te);

        if (bad_cvar_name(str)) {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, ID2SYM(rb_intern("name")), str);
          diagnostic(lexer, severity_error, cvar_name, hash, range(lexer, ts, te), empty_array);
        }

        emit(tCVAR);
        fnext *stack[--top]; fbreak;
      };

      '@' [0-9]+
      => {
        VALUE token = tok(lexer, ts, te);

        if (lexer->version < 27) {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, ID2SYM(rb_intern("name")), token);
          diagnostic(lexer, severity_error, ivar_name, hash, range(lexer, ts, te), empty_array);
        }

        VALUE value = rb_funcall(token, rb_intern("[]"), 1, rb_range_new(INT2NUM(1), INT2NUM(-1), 0));
        VALUE int_value = rb_funcall(value, rb_intern("to_i"), 0);

        if (*RSTRING_PTR(value) == '0') {
          diagnostic(lexer, severity_error, leading_zero_in_numparam, Qnil, range(lexer, ts, te), empty_array);
        }

        if (FIX2INT(int_value) > NUMPARAM_MAX) {
          diagnostic(lexer, severity_error, too_large_numparam, Qnil, range(lexer, ts, te), empty_array);
        }

        VALUE context = rb_iv_get(self, "@context");
        int in_block  = RTEST(rb_funcall(context, rb_intern("in_block?"), 0));
        int in_lambda = RTEST(rb_funcall(context, rb_intern("in_lambda?"), 0));

        if (!in_block && !in_lambda) {
          diagnostic(lexer, severity_error, numparam_outside_block, Qnil, range(lexer, ts, te), empty_array);
        }

        VALUE max_numparam_stack = lexer->max_numparam_stack;
        int can_have_numparams = RTEST(rb_funcall(max_numparam_stack, rb_intern("can_have_numparams?"), 0));
        if (!can_have_numparams) {
          diagnostic(lexer, severity_error, ordinary_param_defined, Qnil, range(lexer, ts, te), empty_array);
        }

        rb_funcall(max_numparam_stack, rb_intern("register"), 1, int_value);

        emit_token(lexer, tNUMPARAM, tok(lexer, ts + 1, te), ts, te);
        fnext *stack[--top]; fbreak;
      };

      instance_var_v => {
        VALUE str = tok(lexer, ts, te);

        if (bad_ivar_name(str)) {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, ID2SYM(rb_intern("name")), str);
          diagnostic(lexer, severity_error, ivar_name, hash, range(lexer, ts, te), empty_array);
        }

        emit(tIVAR);
        fnext *stack[--top]; fbreak;
      };
  *|;

  expr_fname := |*
      keyword
      => { emit_table_KEYWORDS_BEGIN(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_endfn; fbreak; };

      constant        => { emit(tCONSTANT); fnext expr_endfn; fbreak; };

      bareword [?=!]? => { emit(tIDENTIFIER); fnext expr_endfn; fbreak; };

      global_var => { p = ts - 1; fnext expr_end; fcall expr_variable; };

      operator_fname      |
      operator_arithmetic |
      operator_rest
      => { emit_table_PUNCTUATION(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_endfn; fbreak; };

      '::' => { fhold; fhold; fgoto expr_end; };

      ':' => { fhold; fgoto expr_beg; };

      '%s' c_any
      => {
        if (lexer->version == 23) {
          VALUE type = rb_str_substr(lexer->source, ts, te - ts - 1);
          VALUE delimiter = rb_str_substr(lexer->source, te - 1, 1);
          if (delimiter == Qnil)
            delimiter = blank_string;

          fgoto *push_literal(lexer, type, delimiter, ts, 0, 0, 0, 0);
        } else {
          p = ts - 1;
          fgoto expr_end;
        }
      };

      w_any;

      c_any => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  expr_endfn := |*
      label ( any - ':' ) => {
        emit_token(lexer, tLABEL, tok(lexer, ts, te - 2), ts, te - 1);
        fhold; fnext expr_labelarg; fbreak;
      };

      w_space_comment;

      c_any => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  expr_dot := |*
      constant
      => { emit(tCONSTANT);
           fnext *arg_or_cmdarg(cmd_state); fbreak; };

      call_or_var
      => { emit(tIDENTIFIER);
           fnext *arg_or_cmdarg(cmd_state); fbreak; };

      bareword ambiguous_fid_suffix
      => { emit_token(lexer, tFID, tok(lexer, ts, tm), ts, tm);
           fnext *arg_or_cmdarg(cmd_state); p = tm - 1; fbreak; };

      operator_fname      |
      operator_arithmetic |
      operator_rest
      => { emit_table_PUNCTUATION(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_arg; fbreak; };

      w_any;

      c_any
      => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  expr_arg := |*
      w_space+ e_lparen => {
        if (lexer->version == 18) {
          emit_token(lexer, tLPAREN2, rb_str_new2("("), te - 1, te);
          fnext expr_value; fbreak;
        } else {
          emit_token(lexer, tLPAREN_ARG, rb_str_new2("("), te - 1, te);
          fnext expr_beg; fbreak;
        }
      };

      e_lparen => { emit(tLPAREN2); fnext expr_beg; fbreak; };

      w_space+ e_lbrack => {
        emit_token(lexer, tLBRACK, rb_str_new2("["), te - 1, te);
        fnext expr_beg; fbreak;
      };

      w_space* e_lbrace => {
        VALUE val = array_last(lexer->lambda_stack);
        if (val != Qnil && NUM2INT(val) == lexer->paren_nest) {
          rb_ary_pop(lexer->lambda_stack);
          emit_token(lexer, tLAMBEG, rb_str_new2("{"), te - 1, te);
        } else {
          emit_token(lexer, tLCURLY, rb_str_new2("{"), te - 1, te);
        }
        lexer->command_start = 1;
        fnext expr_value; fbreak;
      };

      '?' c_space_nl => { p = ts - 1; fgoto expr_end; };

      w_space* '?' => { fhold; fgoto expr_beg; };

      w_space+ %{ tm = p; }
      ( [%/] ( c_any - c_space_nl - '=' ) | '<<' ) => {
        if (NUM2INT(rb_ary_entry(lexer->source_pts, tm)) == '/') {
          diagnostic(lexer, warning, ambiguous_literal, Qnil,
                     range(lexer, tm, tm + 1), empty_array);
        }

        p = tm - 1;
        fgoto expr_beg;
      };

      w_space+ %{ tm = p; } ( '+' | '-' | '*' | '&' | '**' ) => {
        VALUE hash = rb_hash_new();
        VALUE str  = tok(lexer, tm, te);
        rb_hash_aset(hash, prefix, str);
        diagnostic(lexer, warning, ambiguous_prefix, hash, range(lexer, tm, te),
                   empty_array);

        p = tm - 1;
        fgoto expr_beg;
      };

      w_space+ '::' => { fhold; fhold; fgoto expr_beg; };

      w_space* ':' => { fhold; fgoto expr_beg; };

      w_space+ label => { p = ts - 1; fgoto expr_beg; };

      w_space+ %{ tm = p; } '?' c_space_nl => { p = tm - 1; fgoto expr_end; };

      w_space* operator_arithmetic
                  ( '=' | c_space_nl )?    |
      w_space* keyword_modifier            |
      w_space* '&.'                        |
      w_space* punctuation_end
      => {
        p = ts - 1;
        fgoto expr_end;
      };

      w_space;

      w_comment => { fgoto expr_end; };

      w_newline => { fhold; fgoto expr_end; };

      c_any => { fhold; fgoto expr_beg; };

      c_eof => do_eof;
  *|;

  expr_cmdarg := |*
      w_space+ e_lparen
      => {
        emit_token(lexer, tLPAREN_ARG, rb_str_new2("("), te - 1, te);
        if (lexer->version == 18) {
          fnext expr_value; fbreak;
        } else {
          fnext expr_beg; fbreak;
        }
      };

      w_space* 'do'
      => {
        if (stack_state_active(&lexer->cond)) {
          emit_token(lexer, kDO_COND, rb_str_new2("do"), te - 2, te);
        } else {
          emit_token(lexer, kDO, rb_str_new2("do"), te - 2, te);
        }
        fnext expr_value; fbreak;
      };

      c_any             |
      w_space* bareword |
      w_space* label
      => { p = ts - 1;
           fgoto expr_arg; };

      c_eof => do_eof;
  *|;

  expr_endarg := |*
      e_lbrace => {
        VALUE val = array_last(lexer->lambda_stack);
        if (val != Qnil && NUM2INT(val) == lexer->paren_nest) {
          rb_ary_pop(lexer->lambda_stack);
          emit_token(lexer, tLAMBEG, rb_str_new2("{"), te - 1, te);
        } else {
          emit_token(lexer, tLBRACE_ARG, rb_str_new2("{"), te - 1, te);
        }
        lexer->command_start = 1;
        fnext expr_value; fbreak;
      };

      'do' => { emit_do(lexer, 1, ts, te); fnext expr_value; fbreak; };

      w_space_comment;

      c_any
      => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  expr_mid := |*
      keyword_modifier
      => { emit_table_KEYWORDS(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_beg; fbreak; };

      bareword => { p = ts - 1; fgoto expr_beg; };

      w_space_comment;

      w_newline => { fhold; fgoto expr_end; };

      c_any => { fhold; fgoto expr_beg; };

      c_eof => do_eof;
  *|;

  expr_beg := |*
      [+\-] w_any* [0-9] => {
        emit_token(lexer, tUNARY_NUM, tok(lexer, ts, ts + 1), ts, ts + 1);
        fhold; fnext expr_end; fbreak;
      };

      '*' => { emit(tSTAR); fbreak; };

      '/' c_any => {
        VALUE delimiter = rb_str_substr(lexer->source, ts, 1);
        fhold; fgoto *push_literal(lexer, delimiter, delimiter, ts, 0, 0, 0, 0);
      };

      '%' ( any - [A-Za-z] ) => {
        VALUE type = rb_str_substr(lexer->source, ts, 1);
        VALUE delimiter = rb_str_substr(lexer->source, te - 1, 1);
        if (delimiter == Qnil)
          delimiter = blank_string;

        fgoto *push_literal(lexer, type, delimiter, ts, 0, 0, 0, 0);
      };

      '%' [A-Za-z]+ c_any => {
        VALUE type = rb_str_substr(lexer->source, ts, te - ts - 1);
        VALUE delimiter = rb_str_substr(lexer->source, te - 1, 1);
        if (delimiter == Qnil)
          delimiter = blank_string;

        fgoto *push_literal(lexer, type, delimiter, ts, 0, 0, 0, 0);
      };

      '%' c_eof => {
        diagnostic(lexer, fatal, string_eof, Qnil,
                   range(lexer, ts, ts + 1), empty_array);
      };

      '<<' [~\-]?
        ( '"' ( any - '"' )* '"'
        | "'" ( any - "'" )* "'"
        | "`" ( any - "`" )* "`"
        | bareword ) % { heredoc_e = p; }
        c_line* c_nl % { if (!lexer->herebody_s) lexer->herebody_s = p; } => {

        VALUE heredoc = tok(lexer, ts, heredoc_e);
        VALUE type;
        char *cp = RSTRING_PTR(heredoc);
        int indent = 0, dedent_body = 0;
        long rng_s = ts, rng_e = heredoc_e;

        if (cp[2] == '-') {
          indent = 1;
          cp += 3;
          rng_s += 3;
        } else if (cp[2] == '~') {
          dedent_body = indent = 1;
          cp += 3;
          rng_s += 3;
        } else {
          cp += 2;
          rng_s += 2;
        }

        if (*cp == '"' || *cp == '\'' || *cp == '`') {
          char type_str[3];
          type_str[0] = '<';
          type_str[1] = '<';
          type_str[2] = *cp;

          cp += 1;
          rng_s += 1;
          rng_e -= 1;

          type = rb_str_new(type_str, 3);
        } else {
          type = rb_str_new2("<<\"");
        }

        VALUE delimiter = tok(lexer, rng_s, rng_e);

        if (lexer->version >= 27) {
          int newlines_count = NUM2INT(rb_funcall(delimiter, rb_intern("count"), 1, newline));
          int slash_r_count = NUM2INT(rb_funcall(delimiter, rb_intern("count"), 1, slash_r));

          if (newlines_count > 0 || slash_r_count > 0) {
            diagnostic(lexer, severity_error, unterminated_heredoc_id, Qnil,
                   range(lexer, ts, ts + 1), empty_array);
          }
        } else if (lexer->version >= 24) {
          if (NUM2INT(rb_funcall(delimiter, rb_intern("count"), 1, newline)) > 0) {
            if (str_end_with_p(delimiter, "\n")) {
              diagnostic(lexer, warning, heredoc_id_ends_with_nl, Qnil,
                   range(lexer, ts, ts + 1), empty_array);

              delimiter = rb_funcall(delimiter, rb_intern("rstrip"), 0);
            } else {
              diagnostic(lexer, fatal, heredoc_id_has_newline, Qnil,
                   range(lexer, ts, ts + 1), empty_array);
            }
          }
        }

        if (dedent_body && lexer->version >= 18 && lexer->version <= 22) {
          emit_token(lexer, tLSHFT, rb_str_new2("<<"), ts, ts + 2);
          p = ts + 1;
          fnext expr_beg; fbreak;
        } else {
          fnext *push_literal(lexer, type, delimiter, ts, heredoc_e, indent,
                              dedent_body, 0);
          p = lexer->herebody_s - 1;
        }
      };

      # Escaped unterminated heredoc start
      # <<'END  | <<"END  | <<`END  |
      # <<-'END | <<-"END | <<-`END |
      # <<~'END | <<~"END | <<~`END
      #
      # If the heredoc is terminated the rule above should handle it
      '<<' [~\-]?
        ('"' (any - c_nl - '"')*
        |"'" (any - c_nl - "'")*
        |"`" (any - c_nl - "`")
        )
      => {
        diagnostic(lexer, severity_error, unterminated_heredoc_id, Qnil, range(lexer, ts, ts + 1), empty_array);
      };

      ':' ('&&' | '||') => {
        fhold; fhold;
        emit_token(lexer, tSYMBEG, tok(lexer, ts, ts + 1), ts, ts + 1);
        fgoto expr_fname;
      };

      ':' ['"] => { /* ' */
        VALUE type = tok(lexer, ts, te);
        VALUE delimiter = tok(lexer, te - 1, te);
        fgoto *push_literal(lexer, type, delimiter, ts, 0, 0, 0, 0);
      };

      ':' [!~] '@'
      => {
        emit_token(lexer, tSYMBOL, tok(lexer, ts + 1, ts + 2), ts, te);
        fnext expr_end; fbreak;
      };

      ':' bareword ambiguous_symbol_suffix => {
        emit_token(lexer, tSYMBOL, tok(lexer, ts + 1, tm), ts, tm);
        p = tm - 1;
        fnext expr_end; fbreak;
      };

      ':' ( bareword | global_var | class_var | instance_var |
            operator_fname | operator_arithmetic | operator_rest ) => {
        emit_token(lexer, tSYMBOL, tok(lexer, ts + 1, te), ts, te);
        fnext expr_end; fbreak;
      };

      ':' ( '@'  %{ tm = p - 1; diag_msg = ivar_name; }
          | '@@' %{ tm = p - 2; diag_msg = cvar_name; }
          ) [0-9]*
      => {
        if (lexer->version >= 27) {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, ID2SYM(rb_intern("name")), tok(lexer, tm, te));
          diagnostic(lexer, severity_error, diag_msg, hash, range(lexer, tm, te), empty_array);
        } else {
          emit_token(lexer, tCOLON, tok(lexer, ts, ts + 1), ts, ts + 1);
          p = ts;
        }

        fnext expr_end; fbreak;
      };


      '?' ( e_bs ( escape - ( '\u{' (xdigit+ [ \t]+)+ xdigit+ '}' ))
          | (c_any - c_space_nl - e_bs) % { lexer->escape = Qnil; }
          ) => {
        VALUE value = lexer->escape;
        if (value == Qnil)
          value = tok(lexer, ts + 1, te);

        if (lexer->version == 18)
          emit_token(lexer, tINTEGER, rb_funcall(value, rb_intern("getbyte"), 1, INT2NUM(0)), ts, te);
        else
          emit_token(lexer, tCHARACTER, value, ts, te);

        fnext expr_end; fbreak;
      };

      '?' c_space_nl => {
        VALUE escape = escape_char(rb_str_subseq(lexer->source, ts + 1, 1));
        VALUE hash = rb_hash_new();
        rb_hash_aset(hash, ID2SYM(rb_intern("escape")), escape);
        diagnostic(lexer, warning, invalid_escape_use, hash,
                   range(lexer, ts, te), empty_array);

        p = ts - 1;
        fgoto expr_end;
      };

      '?' c_eof => {
        diagnostic(lexer, fatal, incomplete_escape, Qnil,
                   range(lexer, ts, ts + 1), empty_array);
      };

      '?' [A-Za-z_] bareword => { p = ts - 1; fgoto expr_end; };

      e_lbrace => {
        VALUE val = array_last(lexer->lambda_stack);
        if (val != Qnil && NUM2INT(val) == lexer->paren_nest) {
          rb_ary_pop(lexer->lambda_stack);
          lexer->command_start = 1;
          emit(tLAMBEG);
        } else {
          emit(tLBRACE);
        }
        fbreak;
      };

      e_lbrack => {
        emit_token(lexer, tLBRACK, tok(lexer, ts, te), ts, te);
        fbreak;
      };

      e_lparen => {
        emit_token(lexer, tLPAREN, tok(lexer, ts, te), ts, te);
        fbreak;
      };

      punctuation_begin
      => { emit_table_PUNCTUATION_BEGIN(lexer, tok(lexer, ts, te), ts, te);
           fbreak; };

      'rescue' %{ tm = p; } '=>'? => {
        emit_token(lexer, kRESCUE, tok(lexer, ts, tm), ts, tm);
        p = tm - 1;
        fnext expr_mid; fbreak;
      };

      keyword_modifier
      => { emit_table_KEYWORDS_BEGIN(lexer, tok(lexer, ts, te), ts, te);
           lexer->command_start = 1;
           fnext expr_value; fbreak; };

      label ( any - ':' )
      => {
        fhold;

        if (lexer->version == 18) {
          VALUE ident = tok(lexer, ts, te - 2);

          emit_token(lexer, is_capitalized(ident) ? tCONSTANT : tIDENTIFIER,
               ident, ts, te - 2);
          fhold;

          if (STATIC_ENV_DECLARED(ident)) {
            fnext expr_end;
          } else {
            fnext *arg_or_cmdarg(cmd_state);
          }
        } else {
          emit_token(lexer, tLABEL, tok(lexer, ts, te - 2), ts, te - 1);
          fnext expr_labelarg;
        }

        fbreak;
      };

      # RUBY 2.7 BEGINLESS RANGE

      '..'
      => {
        if (lexer->version >= 27) {
          emit(tBDOT2);
        } else {
          emit(tDOT2);
        }

        fnext expr_beg; fbreak;
      };

      '...'
      => {
        if (lexer->version >= 27) {
          emit(tBDOT3);
        } else {
          emit(tDOT3);
        }

        fnext expr_beg; fbreak;
      };

      bareword ambiguous_ident_suffix | keyword => { p = ts - 1; fgoto expr_end; };

      call_or_var => local_ident;

      (call_or_var - keyword)
        % { ident_tok = tok(lexer, ts, te); ident_ts = ts; ident_te = te; }
      w_space+ '('
      => {
        emit_token(lexer, tIDENTIFIER, ident_tok, ident_ts, ident_te);
        p = ident_te - 1;

        if (STATIC_ENV_DECLARED(ident_tok) && lexer->version < 25) {
          fnext expr_endfn;
        } else {
          fnext expr_cmdarg;
        }

        fbreak;
      };

      w_any;

      e_heredoc_nl '=begin' ( c_space | c_nl_zlen ) => {
        p = ts - 1;
        lexer->cs_before_block_comment = lexer->cs;
        fgoto line_begin;
      };

      operator_arithmetic '='    |
      operator_rest              |
      punctuation_end            |
      c_any
      => { p = ts - 1; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  expr_labelarg := |*
      w_space_comment;

      w_newline => {
        if (lexer->in_kwarg) {
          fhold; fgoto expr_end;
        } else {
          fgoto line_begin;
        }
      };

      c_any => { fhold; fgoto expr_beg; };

      c_eof => do_eof;
  *|;

  expr_value := |*
      label (any - ':') => { p = ts - 1; fgoto expr_end; };

      ['"] => { /* ' */
        VALUE type = tok(lexer, ts, te);
        fgoto *push_literal(lexer, type, type, ts, 0, 0, 0, 0);
      };

      w_space_comment;

      w_newline => { fgoto line_begin; };

      c_any => { fhold; fgoto expr_beg; };

      c_eof => do_eof;
  *|;

  expr_end := |*
      '->' => {
        emit_token(lexer, tLAMBDA, tok(lexer, ts, ts + 2), ts, ts + 2);
        rb_ary_push(lexer->lambda_stack, INT2NUM(lexer->paren_nest));
        fnext expr_endfn; fbreak;
      };

      e_lbrace => {
        VALUE val = array_last(lexer->lambda_stack);
        if (val != Qnil && NUM2INT(val) == lexer->paren_nest) {
          rb_ary_pop(lexer->lambda_stack);
          emit(tLAMBEG);
        } else {
          emit(tLCURLY);
        }
        lexer->command_start = 1;
        fnext expr_value; fbreak;
      };

      'do' => {
        VALUE val = array_last(lexer->lambda_stack);
        if (val != Qnil && NUM2INT(val) == lexer->paren_nest) {
          rb_ary_pop(lexer->lambda_stack);
          emit(kDO_LAMBDA);
        } else {
          emit_do(lexer, 0, ts, te);
        }
        lexer->command_start = 1;
        fnext expr_value; fbreak;
      };

      keyword_with_fname
      => { emit_table_KEYWORDS(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_fname; fbreak; };

      'class' w_any* '<<'
      => { emit_token(lexer, kCLASS, rb_str_new2("class"), ts, ts + 5);
           emit_token(lexer, tLSHFT, rb_str_new2("<<"),    te - 2, te);
           fnext expr_value; fbreak; };

      keyword_modifier
      => { emit_table_KEYWORDS(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_beg; fbreak; };

      keyword_with_value
      => { emit_table_KEYWORDS(lexer, tok(lexer, ts, te), ts, te);
           lexer->command_start = 1;
           fnext expr_value; fbreak; };

      keyword_with_mid
      => { emit_table_KEYWORDS(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_mid; fbreak; };

      keyword_with_arg
      => {
        VALUE keyword = tok(lexer, ts, te);
        emit_table_KEYWORDS(lexer, keyword, ts, te);

        if (lexer->version == 18 && strcmp(RSTRING_PTR(keyword), "not") == 0) {
          fnext expr_beg; fbreak;
        } else {
          fnext expr_arg; fbreak;
        }
      };

      '__ENCODING__' => {
        if (lexer->version == 18) {
          VALUE str = tok(lexer, ts, te);
          emit(tIDENTIFIER);

          if (STATIC_ENV_DECLARED(str)) {
            fnext expr_end;
          } else {
            fnext *arg_or_cmdarg(cmd_state);
          }
        } else {
          emit(k__ENCODING__);
        }
        fbreak;
      };

      keyword_with_end
      => { emit_table_KEYWORDS(lexer, tok(lexer, ts, te), ts, te);
           fbreak; };

      ( '0' [Xx] %{ num_base = 16; num_digits_s = p; } int_hex
      | '0' [Dd] %{ num_base = 10; num_digits_s = p; } int_dec
      | '0' [Oo] %{ num_base = 8;  num_digits_s = p; } int_dec
      | '0' [Bb] %{ num_base = 2;  num_digits_s = p; } int_bin
      | [1-9] digit* '_'? %{ num_base = 10; num_digits_s = ts; } int_dec
      | '0'   digit* '_'? %{ num_base = 8;  num_digits_s = ts; } int_dec
      ) %{ num_suffix_s = p; } int_suffix
      => {
        int invalid_idx;
        VALUE digits = tok(lexer, num_digits_s, num_suffix_s);

        if (NUM2INT(rb_ary_entry(lexer->source_pts, num_suffix_s - 1)) == '_') {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, character, rb_str_new2("_"));
          diagnostic(lexer, severity_error, trailing_in_number, hash,
                     range(lexer, te - 1, te), empty_array);
        } else if (RSTRING_LEN(digits) == 0 && num_base == 8 && lexer->version == 18) {
          digits = rb_str_new2("0");
        } else if (RSTRING_LEN(digits) == 0) {
          diagnostic(lexer, severity_error, empty_numeric, Qnil,
                     range(lexer, ts, te), empty_array);
        } else if (num_base == 8 && (invalid_idx = find_8_or_9(digits)) != -1) {
          long invalid_s = num_digits_s + invalid_idx;
          diagnostic(lexer, severity_error, invalid_octal, Qnil,
                     range(lexer, invalid_s, invalid_s + 1), empty_array);
        }

        VALUE integer = rb_str_to_inum(digits, num_base, 0);
        if (lexer->version >= 18 && lexer->version <= 20) {
          emit_token(lexer, tINTEGER, integer, numeric_s, num_suffix_s);
          p = num_suffix_s - 1;
        } else {
          num_xfrm(lexer, integer, numeric_s, te);
        }

        fbreak;
      };

      flo_frac flo_pow?
      => {
        diagnostic(lexer, severity_error, no_dot_digit_literal, Qnil,
                   range(lexer, ts, te), empty_array);
      };

      flo_int [eE]
      => {
        if (lexer->version >= 18 && lexer->version <= 20) {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, character, tok(lexer, te - 1, te));
          diagnostic(lexer, severity_error, trailing_in_number, hash,
                     range(lexer, te - 1, te), empty_array);
        } else {
          VALUE integer = rb_str_to_inum(tok(lexer, ts, te - 1), 10, 0);
          emit_token(lexer, tINTEGER, integer, ts, te - 1);
          fhold; fbreak;
        }
      };

      flo_int flo_frac [eE]
      => {
        if (lexer->version >= 18 && lexer->version <= 20) {
          VALUE hash = rb_hash_new();
          rb_hash_aset(hash, character, tok(lexer, te - 1, te));
          diagnostic(lexer, severity_error, trailing_in_number, hash,
                     range(lexer, te - 1, te), empty_array);
        } else {
          VALUE fval = rb_funcall(tok(lexer, ts, te - 1), rb_intern("to_f"), 0);
          emit_token(lexer, tFLOAT, fval, ts, te - 1);
          fhold; fbreak;
        }
      };

      flo_int
      ( flo_frac? flo_pow %{ num_suffix_s = p; } flo_pow_suffix
      | flo_frac          %{ num_suffix_s = p; } flo_suffix
      )
      => {
        VALUE digits = tok(lexer, ts, num_suffix_s);

        if (lexer->version >= 18 && lexer->version <= 20) {
          VALUE fval = rb_Float(digits);
          emit_token(lexer, tFLOAT, fval, ts, num_suffix_s);
          p = num_suffix_s - 1;
        } else {
          num_xfrm(lexer, digits, ts, te);
        }
        fbreak;
      };

      '`' | ['"] => { /* ' */
        VALUE type = tok(lexer, ts, te);
        VALUE delimiter = tok(lexer, te - 1, te);
        fgoto *push_literal(lexer, type, delimiter, ts, 0, 0, 0, 1);
      };

      constant => { emit(tCONSTANT); fnext *arg_or_cmdarg(cmd_state); fbreak; };

      constant ambiguous_const_suffix => {
        emit_token(lexer, tCONSTANT, tok(lexer, ts, tm), ts, tm);
        p = tm - 1;
        fbreak;
      };

      global_var | class_var_v | instance_var_v
      => { p = ts - 1; fcall expr_variable; };

      '.:' w_space+
      => { emit_token(lexer, tDOT, tok(lexer, ts, ts + 1), ts, ts + 1);
           emit_token(lexer, tCOLON, tok(lexer, ts + 1, ts + 2), ts + 1, ts + 2);
           p = p - (te - ts) + 2;
           fnext expr_dot; fbreak; };

      '.:' | '.' | '&.' | '::'
      => { emit_table_PUNCTUATION(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_dot; fbreak; };

      call_or_var => local_ident;

      bareword ambiguous_fid_suffix => {
        if (tm == te) {
          emit(tFID);
        } else {
          emit_token(lexer, tIDENTIFIER, tok(lexer, ts, tm), ts, tm);
          p = tm - 1;
        }
        fnext expr_arg; fbreak;
      };

      '*' | '=>' => {
        emit_table_PUNCTUATION(lexer, tok(lexer, ts, te), ts, te);
        fgoto expr_value;
      };

      ( operator_arithmetic | operator_rest ) - ( '|' | '~' | '!' | '*' )
      => {
        emit_table_PUNCTUATION(lexer, tok(lexer, ts, te), ts, te);
        fnext expr_value; fbreak;
      };

      ( e_lparen | '|' | '~' | '!' )
      => { emit_table_PUNCTUATION(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_beg; fbreak; };

      e_rbrace => {
        emit(tRCURLY);

        if (lexer->version < 24) {
          stack_state_lexpop(&lexer->cond);
          stack_state_lexpop(&lexer->cmdarg);
        } else {
          stack_state_pop(&lexer->cond);
          stack_state_pop(&lexer->cmdarg);
        }

        if (lexer->version >= 25) {
          fnext expr_end;
        } else {
          fnext expr_endarg;
        }

        fbreak;
      };

      e_rparen => {
        emit(tRPAREN);

        if (lexer->version < 24) {
          stack_state_lexpop(&lexer->cond);
          stack_state_lexpop(&lexer->cmdarg);
        } else {
          stack_state_pop(&lexer->cond);
          stack_state_pop(&lexer->cmdarg);
        }

        fbreak;
      };

      ']' => {
        emit(tRBRACK);

        if (lexer->version < 24) {
          stack_state_lexpop(&lexer->cond);
          stack_state_lexpop(&lexer->cmdarg);
        } else {
          stack_state_pop(&lexer->cond);
          stack_state_pop(&lexer->cmdarg);
        }

        if (lexer->version >= 25) {
          fnext expr_end;
        } else {
          fnext expr_endarg;
        }

        fbreak;
      };

      operator_arithmetic '='
      => { emit_token(lexer, tOP_ASGN, tok(lexer, ts, te - 1), ts, te);
           fnext expr_beg; fbreak; };

      '?' => { emit(tEH); fnext expr_value; fbreak; };

      e_lbrack => { emit(tLBRACK2); fnext expr_beg; fbreak; };

      punctuation_end
      => { emit_table_PUNCTUATION(lexer, tok(lexer, ts, te), ts, te);
           fnext expr_beg; fbreak; };

      w_space_comment;

      w_newline => { fgoto leading_dot; };

      ';'
      => { emit(tSEMI);
           lexer->command_start = 1;
           fnext expr_value; fbreak; };

      '\\' c_line {
        diagnostic(lexer, severity_error, bare_backslash, Qnil,
                   range(lexer, ts, ts + 1), empty_array);
        fhold;
      };

      c_any
      => {
        VALUE hash = rb_hash_new();
        VALUE str  = rb_str_inspect(tok(lexer, ts, te));
        rb_hash_aset(hash, character, rb_str_substr(str, 1, NUM2INT(rb_str_length(str)) - 2));
        diagnostic(lexer, fatal, unexpected, hash, range(lexer, ts, te), empty_array);
      };

      c_eof => do_eof;
  *|;

  leading_dot := |*
      c_space* %{ tm = p; } ('.' | '&.') => { p = tm - 1; fgoto expr_end; };

      any => {
        emit_token(lexer, tNL, Qnil, lexer->newline_s, lexer->newline_s + 1);
        fhold; fnext line_begin; fbreak;
      };
  *|;

  line_comment := |*
      '=end' c_line* c_nl_zlen => {
        emit_comment(lexer, lexer->eq_begin_s, te);
        fgoto *lexer->cs_before_block_comment;
      };

      c_line* c_nl;

      c_line* zlen => {
        diagnostic(lexer, fatal, embedded_document, Qnil,
                   range(lexer, lexer->eq_begin_s, lexer->eq_begin_s + 6),
                   empty_array);
      };
  *|;

  line_begin := |*
      w_any;

      '=begin' ( c_space | c_nl_zlen ) => {
        lexer->eq_begin_s = ts;
        fgoto line_comment;
      };

      '__END__' ( c_eol - zlen ) => { p = pe - 3; };

      c_any => { cmd_state = 1; fhold; fgoto expr_value; };

      c_eof => do_eof;
  *|;
}%%
