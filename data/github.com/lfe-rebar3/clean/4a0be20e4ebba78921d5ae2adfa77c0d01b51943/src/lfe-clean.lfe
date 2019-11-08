(defmodule lfe-clean
  (behaviour provider)
  (export all))

(defun namespace () 'lfe)
(defun provider-name () 'clean)
(defun short-desc () "The LFE rebar3 clean plugin.")
(defun deps () '(#(default app_discovery)))
(defun items-to-clean ()
  '(#(files ("rebar.lock" "erl_crash.dump" "ebin/*.beam" "*.beam"))
    #(dirs ("_build" "deps" ".rebar" ".rebar3"))))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Public API
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun init (state)
  (let* ((opts `(#(name ,(provider-name))        ; The 'user friendly' name
                 #(module ,(MODULE))             ; The module implementation
                 #(namespace ,(namespace))       ; Plugin namespace
                 #(opts ())                      ; List of plugin options
                 #(deps ,(deps))                 ; The list of dependencies
                 #(example "rebar3 lfe clean")   ; How to use the plugin
                 #(short_desc ,(short-desc))     ; A one-line description
                 #(desc ,(info (short-desc)))    ; A longer description
                 #(bare true)))                  ; The task can be run by user
         (provider (providers:create opts)))
    `#(ok ,(rebar_state:add_provider state provider))))

(defun do (state)
  (rebar_api:info "Cleaning up files and directories" '())
  (lr3-cln-util:clean state (items-to-clean))
  `#(ok ,state))

(defun format_error (reason)
  (io_lib:format "~p" `(,reason)))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Internal functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun info (desc)
  (io_lib:format
    (++ "~n~s~n~n"
        "Delete files not removed by 'rebar3 clean'. ~n"
        "This operation is destructive! It will delete files~n"
        "and recursively remove the configured directories!~n~n")
    `(,desc)))
