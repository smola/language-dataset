(defmodule logjam-util
  (export all))

(defun version ()
  (lutil:get-app-version 'logjam))

(defun versions ()
  (++ (lutil:get-versions)
      `(#(logjam ,(version)))))

(defun check ()
  (let ((caller (logjam:caller)))
    (logjam:info `#(c ,caller) "Checking all log levels ...")
    (lists:foreach
      (lambda (x)
        (call 'logjam x `#(c ,caller)  (++ "Testing log output of "
                                           (atom_to_list x)
                                           " with args: ~s, ~s, and ~s ...")
                '(apple banana cranberry)))
      '(debug info notice warning error critical alert emergency))
    (logjam:info `#(c ,caller) "Check complete.")
    (timer:sleep 500)
    'ok))

(defun check (level)
  (let ((caller (logjam:caller)))
    (logjam:info `#(c ,caller) "Checking log-level ~p ..." `(,level))
    (call 'logjam level `#(c ,caller) (++ "Testing log output of "
                                          (atom_to_list level)
                                          " ... "))
    (logjam:info `#(c ,caller) "Checked log-level ~p." `(,level))))

(defun erl-version ()
  (list_to_integer (erlang:system_info 'otp_release)))

(defun make-printable
  ((a) (when (is_atom a))
    (atom_to_list a))
  ((p) (when (is_pid p))
    (pid_to_list p))
  ((l) (when (orelse (is_list l) (is_binary l)))
    l)
  ((other)
    (io_lib:format "~p" `(,other))))
