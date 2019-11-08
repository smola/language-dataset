#|
@doc
  Telegram low-level API library for LFE
@end
|#

(defmodule telex
  ;; API
  (export (request 2)
          (request 3)))

;;; API

(defun request [token method]
  (request token method #M()))

(defun request [token method opts]
  (let [(req-opts (parse-keyboard opts))]
    (if (has-files? req-opts)
      (do-request token method [] (multipart-body req-opts))
      (do-request token method (json-headers) (jsone:encode req-opts)))))

;;; Internal functions

(defun do-request [token method headers payload]
  (let [(url (create-url token method))]
    (case (hackney:request 'post url headers payload [])
      (`#(ok ,status ,_ ,ref) (when (progn (>= status 200) (< status 500)))
       (parse-response ref))
      (error error))))

(defun json-headers []
  `[#(#"content-type" #"application/json")])

(defun multipart-body [opts]
  (multipart-meta
   (maps:get 'chat_id opts)
   (which-type opts)
   (file-path opts)))

(defun multipart-meta [chat-id type file]
  (let [(filename (filename:basename file))]
    `#(multipart
       [#(#"chat_id" ,(erlang:integer_to_binary chat-id))
        #(file
          ,(erlang:list_to_binary file)
          #("form-data"
            [#("name" ,(erlang:atom_to_binary type 'utf8))
             #("filename" ,filename)])
          [])])))

(defun parse-response [ref]
  (let [(`#(ok ,response) (hackney:body ref))]
    (format-reply (jsone:decode response))))

(defun format-reply [reply]
  (if (is-ok? reply)
    `#(ok ,(get-result reply))
    `#(error ,(error-description reply))))

(defun with-files [opts fn]
  (lists:foldr
   (lambda [key acc]
     (if (maps:is_key key opts)
       (funcall fn key opts)
       acc))
   'undefined
   (default-media-types)))

(defun which-type [opts]
  (with-files opts (lambda [key _map] key)))

(defun file-path [opts]
  (with-files opts (lambda [key map] (maps:get key map))))

(defun has-files? [opts]
  (clj:any?
   (lambda [item] (lists:member item (default-media-types)))
   (maps:keys opts)))

(defun parse-keyboard [opts]
  opts)

(defun default-media-types []
  '[photo voice audio document video video_note media])

(defun is-ok? [response]
  (maps:get #"ok" response))

(defun get-result [response]
  (maps:get #"result" response))

(defun error-description [error]
  (maps:get #"description" error))

(defun create-url [token method]
  (erlang:iolist_to_binary
   `[#"https://api.telegram.org/bot" ,token #"/" ,method]))
