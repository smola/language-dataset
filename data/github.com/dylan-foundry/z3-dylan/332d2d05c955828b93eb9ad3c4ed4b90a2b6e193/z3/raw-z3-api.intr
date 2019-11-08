module: %z3-api
synopsis: bindings for the Z3 prover
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define interface
  #include {
      "z3_macros.h",
      "z3_api.h",
      "z3_algebraic.h",
      "z3_polynomial.h",
      "z3_rcf.h",
      "z3_interp.h",
      "z3_fpa.h"
    },
    map: {"Z3_bool" => <C-boolean>},
    import: all,
    inline-functions: inline-only,
    exclude: {
      "Z3_get_error_msg",
      "Z3_mk_injective_function",
      "Z3_set_logic",
      "Z3_push",
      "Z3_pop",
      "Z3_get_num_scopes",
      "Z3_persist_ast",
      "Z3_assert_cnstr",
      "Z3_check_and_get_model",
      "Z3_check",
      "Z3_check_assumptions",
      "Z3_get_implied_equalities",
      "Z3_del_model",
      "Z3_soft_check_cancel",
      "Z3_get_search_failure",
      "Z3_mk_label",
      "Z3_get_relevant_labels",
      "Z3_get_relevant_literals",
      "Z3_get_guessed_literals",
      "Z3_del_literals",
      "Z3_get_num_literals",
      "Z3_get_label_symbol",
      "Z3_get_literal",
      "Z3_disable_literal",
      "Z3_block_literals",
      "Z3_get_model_num_constants",
      "Z3_get_model_constant",
      "Z3_get_model_num_funcs",
      "Z3_get_model_func_decl",
      "Z3_eval_func_decl",
      "Z3_is_array_value",
      "Z3_get_array_value",
      "Z3_get_model_func_else",
      "Z3_get_model_func_num_entries",
      "Z3_get_model_func_entry_num_args",
      "Z3_get_model_func_entry_arg",
      "Z3_get_model_func_entry_value",
      "Z3_eval",
      "Z3_eval_decl",
      "Z3_context_to_string",
      "Z3_statistics_to_string",
      "Z3_get_context_assignment"
    };

  function "Z3_get_version",
    output-argument: 1,
    output-argument: 2,
    output-argument: 3,
    output-argument: 4;

  function "Z3_check_and_get_model",
    output-argument: 2;

  function "Z3_mk_add" => %Z3-mk-add;
  function "Z3_mk_and" => %Z3-mk-and;
  function "Z3_mk_app" => %Z3-mk-app;
  function "Z3_mk_distinct" => %Z3-mk-distinct;
  function "Z3_mk_enumeration_sort" => %Z3-mk-enumeration-sort;
  function "Z3_mk_mul" => %Z3-mk-mul;
  function "Z3_mk_or" => %Z3-mk-or;
  function "Z3_mk_set_intersect" => %Z3-mk-set-intersect;
  function "Z3_mk_set_union" => %Z3-mk-set-union;
  function "Z3_mk_sub" => %Z3-mk-sub;
end interface;
