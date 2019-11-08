(defmodule ljson
  (export all))

(defun pairs ()
  (dict:new))

(defun pairs (data)
  (dict:from_list data))

(defun encode (data)
  (clj:-> data
          (convert)
          (mochijson2:encode)
          (unicode:characters_to_binary)))

(defun decode (data)
  (clj:-> data
          (unicode:characters_to_binary)
          (jsx:decode)
          (deconvert)))

(defun convert (data)
  (cond
    ((pairs? data)
      (pairs->list data))
    ((clj:tuple? data)
      (list data))
    ('true data)))

(defun deconvert
  ((`(,data)) (when (is_tuple data))
   data)
  ((data)
   data))

(defun pairs? (data)
  (clj:dict? data))

(defun pairs->list (pairs)
  (dict:to_list pairs))

(defun get (data keys options)
  (cond
    ((orelse (clj:string? data) (clj:binary? data) (== options #(json)))
      (encode
        (ljson-util:get-in (decode data) keys)))
    ('true
      (ljson-util:get-in data keys))))

(defun get (data keys)
  (get data keys '()))

(defun prettify (data)
  (print-str (jsx:prettify data)))

(defun minify (data)
  (jsx:minify data))

(defun print (data)
  (lfe_io:format "~p~n" (list data)))

(defun print-str (data)
  (lfe_io:format "~s~n" (list data)))

(defun read (filename)
  (case (file:read_file filename)
    (`#(ok ,data) (decode data))
    (err err)))

