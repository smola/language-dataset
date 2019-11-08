(defmodule echo-sup
  "The root supervisor of the `echo` application."
  (behaviour supervisor)
  (export (start_link 0) (init 1)))

(defun start_link ()
  "Starts the supervisor process."
  (supervisor:start_link
    (tuple 'local 'echo-sup) 'echo-sup (list)))

(defun init (args)
  "
  Initializes the supervisor process, by providing it with a specification.

  This supervisor has only one worker process - the `echo-worker`.
  "
  (let* ((flags (tuple 'one_for_one 1000 3600))
         (specs (list
                 (tuple 'echo-worker
                        (tuple 'echo-worker 'start_link (list))
                        'permanent
                        2000
                        'worker
                        (list 'echo-worker)))))
    (tuple 'ok (tuple flags specs))))
