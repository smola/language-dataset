(defmodule lcfg-deps
  (export all))

(defun clone-deps ()
  (lists:foreach
    (lambda (x)
      (io:format "~s~s~n" (list (lcfg-const:out-prompt) x)))
    (do-clone-deps)))

(defun do-clone-deps ()
  (do-clone-deps (get-clone-cmds)))

(defun do-clone-deps
  (('())
    `(,(lcfg-const:no-deps)))
  (('no-repo)
    `(,(lcfg-const:no-deps)))
  ((commands)
    (lists:map #'do-cmd/1 commands)))

(defun do-cmd (command)
  (clean-cmd (os:cmd command)))

(defun clean-cmd (result)
  (string:strip
    (re:replace result "^fatal:" "git:" '(#(return list)))
    'right
    (lcfg-const:newline)))

(defun get-clone-cmds ()
  (lists:map
    #'get-clone-cmd/1
    (get-projects-deps)))

(defun get-clone-cmd
  ((`(,org ,name ,branch))
    (++ "git clone "
        (get-branch-option branch)
        (lcfg-const:github)
        (filename:join (list org name))
        ".git "
        (filename:join (lcfg-const:deps-dir) name)))
  ((_)
    'no-repo))

(defun get-branch-option
  (('false)
    "")
  ((branch)
    (++ "-b " branch " ")))

(defun get-projects-deps ()
  (lists:map
    #'parse-dep/1
    (merge-deps)))

(defun parse-dep
  "Parse an element of the deps list.

  Returns a list of '(user-or-org repo branch). If no branch was given,
  branch gets the value of 'false."
  ;; suport the dep format of '#("user-or-org/repo" "branch")
  ((`#(,dep-element ,branch))
    (parse-dep dep-element branch))
  ;; suport the dep format of '("user-or-org/repo")
  ((dep-element)
    (parse-dep dep-element 'false)))

(defun parse-dep (dep-element branch)
  (++ (string:tokens dep-element "/") `(,branch)))

(defun merge-deps ()
  (merge-deps (lcfg-file:read-global)
                      (lcfg-file:read-local)))

(defun merge-deps (config-1 config-2)
  (select-deps
    (get-deps (lcfg-proj:get-project config-1))
    (get-deps (lcfg-proj:get-project config-2))))

(defun select-deps (deps-1 deps-2)
  "This function takes two lists of dependencies where each dependency may be
  either a list, e.g. 'rvirding/lfe' or a tuple, e.g. #('rvirding/lfe' 'master')
  The deps are compared at the org/repo level only, not at the branch level.

  The convention here is that secondary additions override initial additions.
  As such, if you want one particular set of deps to take precedence over
  another, be sure to pass them second."
  (++ (lists:filter
        (lambda (x)
          (let ((repo (get-repo x)))
            (not (or (lists:keymember repo 1 deps-2)
                     (lists:member repo deps-2)))))
        deps-1)
      deps-2))

(defun get-repo
  ((dep) (when (is_tuple dep))
    (element 1 dep))
  ((dep)
    dep))

(defun get-deps
  (('())
    '())
  ((project)
    (proplists:get_value 'deps project '())))
