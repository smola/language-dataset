;; The code below was translated from the Erlang ring benchmark hosted
;; at http://benchmarksgame.alioth.debian.org, written by Jiri Isa and
;; optimized by Shun Shino:
;;  http://goo.gl/5YOjg3
;;
;; The LFE version split the logic of the above-mentioned Erlang code
;; into more functions for increased clarity.
;;
(defmodule ring
  (export
    (main 1)
    (roundtrip 2)))

(defun main (args)
  "Call with the following:
     $ lfec ring.lfe
     $ lfe -smp disable -noshell -run ring main 503 50000000
  "
  (apply
    #'start-ring/2
    (lists:map #'list_to_integer/1 args)))

(defun start-ring (process-count traversal-count)
  (let ((batch (make-processes process-count traversal-count)))
    (! batch traversal-count)
    (roundtrip 1 batch)))

(defun make-processes (process-count traversal-count)
  (lists:foldl
    #'make-process/2
    (self)
    (lists:seq process-count 2 -1)))

(defun make-process (id pid)
  (spawn 'ring 'roundtrip (list id pid)))

(defun roundtrip (id pid)
  (receive
    (1
      (io:fwrite '"Result: ~b~n" (list id))
      (erlang:halt))
    (data
      (! pid (- data 1))
      (roundtrip id pid))))
      
      