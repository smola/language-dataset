//! Built-in middlewares

//! Create a thunk middleware. The result of calling this can be an argument to
//! @[Redux.apply_middleware()].
//!
//! @note
//!  Instead of calling this explicitly you probably want to use
//!  @[Redux.thunk] instead which is an instance of a call to this function.
function create_thunk(void|mixed extra_arg) {
  return lambda (mapping(string:function) wrapper) {
    function dispatch = wrapper->dispatch;
    function get_state = wrapper->get_state;

    // Next is a dispatch function
    return lambda (function next) {
      // action can either be a dispatch action or a create_store function.
      return lambda (mixed action) {
        if (functionp(action)) {
          return action(dispatch, get_state, extra_arg);
        }

        return next(action);
      };
    };
  };
}

/*
  Author: Pontus Östlund <https://github.com/poppa>

  Permission to copy, modify, and distribute this source for any legal
  purpose granted as long as my name is still attached to it. More
  specifically, the GPL, LGPL and MPL licenses apply to this software.
*/
