(defmodule sample-app-routes
  (export all))

(include-lib "exemplar/include/html-macros.lfe")
(include-lib "lfest/include/lfest-routes.lfe")

(defroutes
  ('GET "/"
    (sample-app-content:get-sidebar-content arg-data))
  ('GET "/content/:id"
    (sample-app-content:get-content id arg-data))
  ('GET "/relation/:userid/:accountid"
    (sample-app-content:get-content userid accountid arg-data))
  ('GET "/version"
    (sample-app-content:get-version-content arg-data))
  ;; When nothing matches, do this
  ('NOTFOUND
   (let* ((joined-path (++ "/" (string:join path "/")))
          (msg (++ "Unmatched route!~n"
                   "\tPath info: ~p~n"
                   "\tJoined path: ~p~n"
                   "\tArg data: ~p"))
          (msg-args `(,path ,joined-path ,arg-data)))
    (logjam:error msg msg-args)
    (sample-app-content:four-oh-four
      (++ (strong "Unmatched Route: ") joined-path)))))
