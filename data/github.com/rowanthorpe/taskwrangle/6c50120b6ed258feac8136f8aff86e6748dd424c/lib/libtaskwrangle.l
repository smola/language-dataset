##    Copyright © 2016-2017 Rowan Thorpe, initially based entirely on a "taskdb"
##    demo-app posted by Joe Bogner at http://picolisp.com/wiki/?taskdb on
##    24 August 2016, and has been developed extensively from there.
##
##    This file is part of Taskwrangle.
##
##    Taskwrangle is free software: you can redistribute it and/or modify
##    it under the terms of the GNU Affero General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.
##
##    Taskwrangle is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU Affero General Public License for more details.
##
##    You should have received a copy of the GNU Affero General Public License
##    along with Taskwrangle.  If not, see <http://www.gnu.org/licenses/>.

(load "@lib/misc.l" "@lib/btree.l") # "@lib/debug.l" "@lib/pilog.l"

(symbols 'taskwrangle 'pico)

#### PRIVATE HELPER FUNCTIONS

(de error (Error)
   (nil (prinl (pack "ERROR: " Error))) )

(de known-status (Status)
   (case Status
      # "NEW" status can never be manually set, only by creating the task
      (("OPEN" "DONE" "ACTION" "WAITFOR" "DELEGATED") T)
      (T (error "Status is not one of OPEN, DONE, ACTION, WAITFOR, DELEGATED")) ) )

(de val-or-tasknum (Item)
   (if (lst? Item)
      (mapcar val-or-tasknum Item)
      (if (isa '+Task Item)
         (with Item (: number))
         Item ) ) )

(de get-or-set (Number Field Val)
   (if Val
      (modify T Number Field Val)
      (get (db 'number '+Task Number) Field) ) )

#### PUBLIC CORE FUNCTIONS

(de add (Taskname Priority Tags Due Parent Children Blocks Blockedby Comment)
   "(add Taskname, [Priority, Tags, Due, Parent, Children, Blocks, Blockedby, Comment])"
   "Adds a task. Uses default priority of 1000 if not specified"
   "(add \"call home\"), (add \"call home\" 1000 \"work\")"

   (when Taskname
      (new! '(+Task)
         'number (genKey 'number '+Task)
         'name Taskname
         'status "NEW"
         'priority (or Priority 1000)
         'tags Tags
         'due Due
         'parent Parent
         'children Children
         'blocks Blocks
         'blockedby Blockedby
         'comment Comment ) ) )

(de destroy (Number)
   "(destroy Tasknumber)"
   "Destroys a task"
   "(destroy 1)"

   (lose!> (db 'number '+Task Number)) )

(de modify (SetVal Number . @)
   "(modify T/NIL=add/remove, Tasknumber, [FieldName, ...])"
   "Modifies a task (first arg =T to add/set value, =NIL to remove/clear)"
   "(modify T 2 'status \"WAITFOR\"), (modify NIL 1 'blocks 3)"

   (if SetVal
      (case (arg 1)
         ("status" (when (known-status (arg 2)) (put!> (db 'number '+Task Number) (arg 1) (arg 2))))
         (("name" "priority" "comment") (put!> (db 'number '+Task Number) (arg 1) (arg 2)))
         ("tags" (put!> (db 'number '+Task Number) (arg 1) (if (lst? (arg 2)) (append (get (db 'number '+Task Number) (arg 1)) (arg 2)) (arg 2))))
         ("due" (let (Date ($dat (arg 2) *DateSep)) (if (== NIL Date) (error "Unrecognised due-date format") (put!> (db 'number '+Task Number) (arg 1) Date))))
         ("parent" (put!> (db 'number '+Task Number) (arg 1) (db 'number '+Task (arg 2))))
         (("children" "blocks" "blockedby") (put!> (db 'number '+Task Number) (arg 1) (if (lst? (arg 2)) (append (get (db 'number '+Task Number) (arg 1)) (mapcar '((Y) (db 'number '+Task Y)) (arg 2))) (db 'number '+Task (arg 2)))))
         (T (nil (prinl "Operation not allowed"))) )
      (case (arg 1)
         ("tags" (if (lst? (arg 2)) (mapcar '((X) (del!> (db 'number '+Task Number) (arg 1) X)) (arg 2)) (del!> (db 'number '+Task Number) (arg 1) (arg 2))))
         (("comment" "due" "parent") (nil (put!> (db 'number '+Task Number) (arg 1))))
         (("children" "blocks" "blockedby") (if (lst? (arg 2)) (mapcar '((X) (del!> (db 'number '+Task Number) (arg 1) (db 'number '+Task X))) (arg 2)) (del!> (db 'number '+Task Number) (arg 1) (arg 2))))
         (T (nil (prinl "Operation not allowed"))) ) ) )

(de retrieve (Objects RetFields . @)
   "(retrieve Objects (list or symbol), [RetFields (list or symbol)])"
   "Retrieve properties of a task(s) from an external symbol(s), optionally specifying field(s) to retrieve (default all)"
   "(retrieve {4}), (retrieve '({2} {4}) 'name 'status) (retrieve '({2} {4}) '(name status))"

   (mapcar
      '((Ob)
        (if (lst? Ob)
           (apply retrieve (append (list Ob Retfields) (rest)))
           (mapcan
              '((Field) (list (get Ob Field)))
              (if RetFields
                 (if (lst? RetFields)
                    RetFields
                    (list RetFields) )
                 '(number name status priority tags due parent children blocks blockedby comment) ) ) ) )
      (if (lst? Objects)
         Objects
         (list Objects) ) ) )

(de ind @
   "(ind [[FieldName, arg1/start, [arg2/end], ...] | Equivalent-as-a-list])"
   "Returns an index of all tasks matching criteria"
   "(ind), (ind 'priority 1000 2000), (ind 'tags \"mytag\" 'due \"2016-08-16\"), (ind '(tags \"mytag\" 'due \"2016-08-16\"))"

   (sect
      (let (Var (next))
         (if (and Var (lst? Var))
            (apply ind Var)
            (let (Val1 (next) Val2 (if (index Var '("priority" "due" "notpriority" "notdue")) (next)))
               (if (and Var Val1)
                  (if (= "not" (pack (head 3 (chop Var))))
                     (diff
                        (collect 'number '+Task)
                        (apply ind
                           (list (intern (pack (tail -3 (chop Var)))) Val1 Val2) ) )
                     (case Var
                        (("name" "status" "tags" "parent" "children" "blocks" "blockedby" "comment")
                           (collect Var '+Task Val1) )
                        ("priority"
                           (collect Var '+Task Val1 (or Val2 Val1)) )
                        ("due"
                           (collect Var '+Task ($dat Val1 *DateSep) ($dat (or Val2 Val1) *DateSep)) )
                        (T
                           (nil (prinl "Operation not allowed")) ) ) )
                  (collect 'number '+Task) ) ) ) )
      (if (rest)
         (apply ind (rest))
         (collect 'number '+Task) ) ) )

# TODO: use this with an accumulating "already used" list to avoid circular loops killing the retrieval
(de exclude (Index . @)
   "(exclude Tree (list), [ExcludeField(s) (list)])"
   "Filters the specified external symbol(s) out of the specified tree"
   "(exclude (ind) '{2} '{4})"

   (filter '((X) (not (memq X (rest)))) Index) )

(de dosort (Objects SortFields)
   "(dosort Objects (list or symbol), [SortField(s) (list or symbol)])"
   "Sorts an index of task(s) by specified field(s)"
   "(dosort '({2} {4}) 'priority), (dosort '({2} {4}) '(priority due name)), (dosort '({2} {4}) 'priority 'due 'name)"

   (if SortFields
      (let (SortField (if (lst? SortFields) (car SortFields) SortFields))
         (by
            '((X)
              (let (Y (if (lst? X) (car X) X))
                 (case SortField
                    (("number" "name" "priority" "tags" "parent" "comment") (get Y SortField))
                    (("children" "blocks" "blockedby") (length (get Y SortField)))
                    ("due" ($dat (get Y SortField))) ) ) )
            sort
            Objects ) )
      Objects ) )

(de dorevsort @
   "(dorevsort Objects (list or symbol), [SortField(s) (list or symbol)])"
   "Reverse-sorts an index of task(s) by specified field(s)"
   "(dorevsort '({2} {4}) 'priority), (dorevsort '({2} {4}) '(priority due name)), (dorevsort '({2} {4}) 'priority 'due 'name)"

   (reverse (pass dosort)) )

(de return-raw (Criteria SortFields RetFields)
   "(return-raw '([[FieldName, arg1/start, [arg2/end]], ...]), SortField(s) (list or symbol), RetField(s) (list or symbol))"
   "Returns all tasks matching criteria, without expanding linked DB IDs, optionally specifying field(s) to sort by (default none), and to return (default all)"
   "(return-raw), (return-raw '(priority 1000 2000)), (return-raw '(tags \"mytag\" due \"2016-08-16\") 'name '(name priority)), (return-raw '(tags \"mytag\" due \"2016-08-16\") '(name) 'name 'priority)"

   (retrieve (dosort (ind Criteria) SortFields) RetFields) )

(de return @
   "(return '([[FieldName, arg1/start, [arg2/end]], ...]), SortField(s) (list or symbol), RetField(s) (list or symbol))"
   "Returns all tasks matching criteria, expanding linked DB IDs, optionally specifying field(s) to sort by (default none), and to return (default all)"
   "(return), (return '(priority 1000 2000)), (return '(tags \"mytag\" due \"2016-08-16\") 'name '(name priority)), (return '(tags \"mytag\" due \"2016-08-16\") '(name) 'name 'priority)"

   (mapcar '((Task)
      (mapcar '((TaskEntry)
         (if (and (not (== NIL TaskEntry)) (lst? TaskEntry))
            (mapcar val-or-tasknum TaskEntry)
            (val-or-tasknum TaskEntry) ))
         Task ))
      (pass return-raw) ) )

(de display-raw @
   "(display-raw '([[FieldName, arg1/start, [arg2/end]], ...]), SortField(s) (list or symbol), RetField(s) (list or symbol))"
   "Displays all tasks matching criteria, one per line, without expanding linked DB IDs, optionally specifying field(s) to sort by (default none), and to return (default all)"
   "(display-raw), (display-raw '(priority 1000 2000)), (display-raw '(tags \"mytag\" due \"2016-08-16\") 'name '(name priority)), (display-raw '(tags \"mytag\" due \"2016-08-16\") 'name 'name 'priority)"

   (nil (mapcar println (pass return-raw))) )

(de rev-display-raw @
   "(rev-display-raw '([[FieldName, arg1/start, [arg2/end]], ...]), SortField(s) (list or symbol), RetField(s) (list or symbol))"
   "Displays all tasks matching criteria, one per line, without expanding linked DB IDs, optionally specifying field(s) to reverse-sort by (default none), and to return (default all)"
   "(rev-display-raw), (rev-display-raw '(priority 1000 2000)), (rev-display-raw '(tags \"mytag\" due \"2016-08-16\") 'name '(name priority)), (rev-display-raw '(tags \"mytag\" due \"2016-08-16\") 'name 'name 'priority)"

   (nil (mapcar println (reverse (pass return-raw)))) )

(de display @
   "(display '([[FieldName, arg1/start, [arg2/end]], ...]), SortField(s) (list or symbol), RetField(s) (list or symbol))"
   "Displays all tasks matching criteria, one per line, expanding linked DB IDs, optionally specifying field(s) to sort by (default none), and to return (default all)"
   "(display), (display '(priority 1000 2000)), (display '(tags \"mytag\" due \"2016-08-16\") 'name '(name priority)), (display '(tags \"mytag\" due \"2016-08-16\") 'name 'name 'priority)"

   (nil (mapcar println (pass return))) )

(de rev-display @
   "(rev-display '([[FieldName, arg1/start, [arg2/end]], ...]), SortField(s) (list or symbol), RetField(s) (list or symbol))"
   "Displays all tasks matching criteria, one per line, expanding linked DB IDs, optionally specifying field(s) to reverse-sort by (default none), and to return (default all)"
   "(rev-display), (rev-display '(priority 1000 2000)), (rev-display '(tags \"mytag\" due \"2016-08-16\") 'name '(name priority)), (rev-display '(tags \"mytag\" due \"2016-08-16\") 'name 'name 'priority)"

   (nil (mapcar println (reverse (pass return)))) )

(de license ()
   "(license)"
   "License/Copyright output"
   "(license)"

   (nil (prinl "Taskwrangle

Copyright © 2016-2017 Rowan Thorpe, initially based entirely on a \"taskdb\"
demo-app posted by Joe Bogner at http://picolisp.com/wiki/?taskdb on
24 August 2016, and has been developed extensively from there.

Taskwrangle is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Taskwrangle is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with Taskwrangle.  If not, see <http://www.gnu.org/licenses/>.")) )

(de source ()
   "(source)"
   "Source output (AGPL compliance)"
   "(source)"

   (nil
      (prinl "****** taskwrangle executable")
      (prinl)
      (in "__EXECDIR__/taskwrangle" (prinl (till NIL T)))
      (prinl "****** taskwrangle config")
      (prinl)
      (in "__CONFDIR__/taskwrangle.l" (prinl (till NIL T)))
      (prinl "****** libtaskwrangle.l")
      (prinl)
      (in "__LIBDIR__/libtaskwrangle.l" (prinl (till NIL T))) ) )

(de gethelp @
   "(gethelp [FuncName, ...])"
   "Build help output"
   "(gethelp), (gethelp add modify)"

   (mapcar
      '(
        (X)
        (make
           (when (str? (cadr X))
              (link (cadr X))
              (when (str? (caddr X))
                 (link (caddr X))
                 (when (str? (cadddr X))
                    (link (cadddr X)) ) ) ) ) )
      (or
         (rest)
         (list add destroy modify retrieve ind exclude dosort dorevsort return-raw return display-raw rev-display-raw display rev-display license source gethelp help status isdone isopen setdone setopen rename priority tag untag due undue parent unparent child unchild block unblock comment uncomment todo connectdb) ) ) ) # ...webserve

(de help @
   "(help [FuncName, ...])"
   "Print help output"
   "(help), (help add modify)"

   (nil
      (mapcar
         '(
           (X)
           (when X
              (when (car X) (prinl "Usage:       " (car X)))
              (when (cadr X) (prinl "Description: " (cadr X)))
              (when (caddr X) (prinl "Example:     " (caddr X)))
              (prinl) ) )
         (pass gethelp) ) ) )

#### PUBLIC WRAPPER FUNCTIONS

(de status (Number Status)
   "(status Number Status)"
   "Get or set task-status"
   "(status 3 \"OPEN\")"

   (get-or-set Number 'status Status) )

(de isdone (Number)
   "(isdone Number)"
   "Return whether a task is \"DONE\""
   "(isdone 3)"

   (= "DONE" (status Number)) )

(de isopen (Number)
   "(isopen Number)"
   "Return whether a task is \"OPEN\""
   "(isopen 3)"

   (= "OPEN" (status Number)) )

(de setdone (Number)
   "(setdone Number)"
   "Set a task as \"DONE\""
   "(setdone 3)"

   (status Number "DONE") )

(de setopen (Number)
   "(setopen Number)"
   "Set a task as \"OPEN\""
   "(setopen 3)"

   (status Number "OPEN") )

(de rename (Number Name)
   "(rename Number Name)"
   "Rename a task"
   "(rename 3 \"other task name\")"

   (get-or-set Number 'name Name) )

(de priority (Number Priority)
   "(priority Number Priority)"
   "Change a task's priority"
   "(priority 3 4000)"

   (get-or-set Number 'priority Priority) )

(de tag (Number Tag)
   "(tag Number Tag)"
   "Report or change a task's tag(s)"
   "(tag 3), (tag 3 \"work\") , (tag 3 '(\"work\", \"foreign\"))"

   (get-or-set Number 'tags Tag) )

(de untag (Number Tag)
   "(untag Number Tag)"
   "Remove a task's tag(s)"
   "(untag 3 \"work\")"

   (modify NIL Number 'tags Tag) )

(de due (Number Due)
   "(due Number Due)"
   "Report or change a task's due-date"
   "(due 3), (due 3 \"20161010\")"

   (get-or-set Number 'due Due) )

(de undue (Number)
   "(undue Number)"
   "Remove a task's due-date"
   "(undue 3)"

   (modify NIL Number 'due) )

(de parent (Number PNumber)
   "(parent Number PNumber)"
   "Report or change a task's parent-task"
   "(parent 3), (parent 3 4)"

   (get-or-set Number 'parent PNumber) )

(de unparent (Number)
   "(unparent Number)"
   "Remove a task's parent-task"
   "(unparent 3)"

   (modify NIL Number 'parent) )

(de child (Number CNumber)
   "(child Number CNumber)"
   "Report or add a task's child-task(s)"
   "(child 3), (child 3 4), (child 3 '(4 5))"

   (get-or-set Number 'children CNumber) )

(de unchild (Number CNumber)
   "(unchild Number CNumber)"
   "Remove a task's child-task(s)"
   "(unchild 3 4), (unchild 3 '(4 5))"

   (modify NIL Number 'children CNumber) )

(de block (Number BNumber)
   "(block Number BNumber)"
   "Report or add a task's blockee(s)"
   "(block 3), (block 3 4), (block 3 '(4 5))"

   (get-or-set Number 'blocks BNumber) )

(de unblock (Number BNumber)
   "(unblock Number BNumber)"
   "Remove a task's blockee(s)"
   "(unblock 3 4), (unblock 3 '(4 5))"

   (modify NIL Number 'blocks BNumber) )

(de comment (Number Comment)
   "(comment Number Comment)"
   "Report or change a task's comment"
   "(comment 3), (comment 3 \"this is a comment\")"

   (get-or-set Number 'comment Comment) )

(de uncomment (Number)
   "(uncomment Number)"
   "Remove a task's comment"
   "(uncomment 3)"

   (modify NIL Number 'comment) )

(de todo @
   "(todo [Tag, ...])"
   "Show all incomplete tasks (optionally matching specified tags) in reverse-priority order"
   "(todo), (todo 'work 'devel)"

   (rev-display
      (append
         '(notstatus "DONE")
         (mapcar '((X) (list 'tags X)) (rest)) )
      'priority ) )

#### PUBLIC FRONTEND FUNCTIONS

(de connectdb ()
   "(connectdb)"
   "Connect to the configured task-database"
   "(connectdb)"

   (pool *TaskDBFile) ) ## initial database (consider block size)

#(de webserve ()
#   "(webserve)"
#   "Serve a web-interface"
#   "(webserve)"
#
#   (connectdb)
#   (server 8081 "!work") )
