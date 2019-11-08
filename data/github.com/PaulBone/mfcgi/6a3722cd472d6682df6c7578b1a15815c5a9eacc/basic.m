% Mercury-fcgi library binding
% Contains basic interface
%
% This binding is a simple wrapping of the C interface.
%
% Copyright (C) 2014 Ebrahim Azarisooreh
% Copyright (C) 2014 Paul Bone
% Distributed under the BSD 3-clause license, see LICENSE.
%

:- module mfcgi.basic.

:- interface.

:- import_module mfcgi.streams.

:- import_module bool.
:- import_module io.
:- import_module maybe.
:- import_module string.

%--------------------------------------------------------------------------%
% Basic interface.
%--------------------------------------------------------------------------%

:- type is_fcgi
    --->    is_fcgi
    ;       is_cgi.

    % Tests whether application is CGI or FCGI
    %
:- pred fcgx_is_cgi(is_fcgi::out, io::di, io::uo) is det.

    % Accept a request from the client
    %
    % If "no" is returned the caller should exit.
    %
:- pred fcgx_accept(bool::out, io::di, io::uo) is det.

    % Finish the current request.
    %
:- pred fcgx_finish(io::di, io::uo) is det.

%--------------------------------------------------------------------------%

    % Input output and error streams are associated with each connection.
    % The stream returned by any of these calls is invalid after a call to
    % fcgx_accept or fcgx_finish.
    %
:- pred fcgx_get_input_stream(fcgi_stream::out, io::di, io::uo) is det.
:- pred fcgx_get_output_stream(fcgi_stream::out, io::di, io::uo) is det.
:- pred fcgx_get_error_stream(fcgi_stream::out, io::di, io::uo) is det.

    % Try to get a parameter from the environment array.
    %
:- pred fcgx_get_param(string::in, maybe(string)::uo, io::di, io::uo) is det.

%
% Note that the filter related functions are not implemented.
%

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
   #include \"fcgiapp.h\"
").

%--------------------------------------------------------------------------%

fcgx_is_cgi(IsCGI, !IO) :-
    is_cgi(IsCGI0, !IO),
    (
        IsCGI0 = yes,
        IsCGI = is_cgi
    ;
        IsCGI0 = no,
        IsCGI = is_fcgi
    ).

:- pred is_cgi(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", is_cgi(Result::out, _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury, tabled_for_io, thread_safe],
"
   Result = FCGX_IsCGI() ? MR_YES : MR_NO;
").		

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_accept(Success::out, _IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int result;

    result = FCGX_Accept(&in, &out, &err, &envp);

    Success = result == 0 ? MR_YES : MR_NO;
").

%--------------------------------------------------------------------------%

:- pragma foreign_proc("C", fcgx_finish(_IO0::di, _IO::uo),
  [promise_pure, will_not_call_mercury, tabled_for_io],
"
   FCGX_Finish();
").

%--------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    static FCGX_Stream *in, *out, *err;
    static FCGX_ParamArray envp;
").

:- pragma foreign_proc("C",
    fcgx_get_input_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
     will_not_throw_exception],
    "Stream = in;").
:- pragma foreign_proc("C",
    fcgx_get_output_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
     will_not_throw_exception],
    "Stream = out;").
:- pragma foreign_proc("C",
    fcgx_get_error_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
     will_not_throw_exception],
    "Stream = err;").

%--------------------------------------------------------------------------%

fcgx_get_param(Name, MaybeParam, !IO) :-
    get_param(Name, String, Result, !IO),
    (
        Result = yes,
        MaybeParam = yes(String)
    ;
        Result = no,
        MaybeParam = no
    ).

:- pred get_param(string::in, string::uo, bool::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_param(Name::in, String::uo, Result::uo, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    char *temp;

    temp = FCGX_GetParam(Name, envp);
    if (temp != NULL) {
        Result = MR_YES;
        /*
         * Duplicate the string so we can use it after calling fcgi_finish()
         */
        MR_make_aligned_string_copy(String, temp);
    } else {
        Result = MR_NO;
        String = NULL;
    }
").

