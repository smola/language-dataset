(mac objv args
  `(obj ,@(mappend [list _ _] args)))

(redef handle-request-thread (i o ip)
  (with (nls 0 lines nil line nil responded nil t0 (msec))
    (after
      (whilet c (unless responded (readc i))
        (if srv-noisy* (pr c))
        (if (is c #\newline)
            (if (is (++ nls) 2) 
                (with ((type op args n cooks) (parseheader (rev lines))
                       headers (headerlines (rev lines)))
                  (let t1 (msec)
                    (case type
                      get  (build-response o op args cooks ip headers)
                      post (handle-post i o op args n cooks ip headers)
                           (respond-err o "Unknown request: " (car lines)))
                    (log-request type op args cooks ip t0 t1)
                    (set responded)))
                (do (push (string (rev line)) lines)
                    (wipe line)))
            (unless (is c #\return)
              (push c line)
              (= nls 0))))
      (close i o)))
  (harvest-fnids))

(def build-response2 (req)
  (w/stdout req!str
    (respond req)))

(redef handle-post (i o op args n cooks ip headers)
  (if srv-noisy* (pr "Post Contents: "))
  (if (no n)
      (respond-err o "Post request without Content-Length.")
      (let is-form (begins (alref headers "Content-Type")
                           "application/x-www-form-urlencoded")
        (let line nil
          (whilet c (and (> n 0) (readc i))
            (if srv-noisy* (pr c))
            (-- n)
            (push c line)) 
          (if srv-noisy* (pr "\n\n"))
          (let post (string (rev line))
            (build-response2
             (obj op      op
                  args    (+ (if is-form (parseargs post)) args)
                  cooks   cooks
                  ip      ip
                  str     o
                  headers headers
                  post    (unless is-form post))))))))

(= nfheader* "HTTP/1.0 404 Not Found
Content-Type: text/html; charset=utf-8
Connection: close")

(def build-response (str op args cooks ip headers)
  (w/stdout str
    (respond (inst 'request 'args args 'cooks cooks 'ip ip 'op op 'str str 'headers headers))))

(def static-file (op)
  (file-exists (string staticdir* op)))

(redef respond (req)
  (iflet f (srvops* req!op)
         (if (redirector* req!op)
               (do (prn rdheader*)
                   (prn "Location: " (f req!str req))
                   (prn))
               (do (prn header*)
                   (awhen (max-age* req!op)
                     (prn "Cache-Control: max-age=" it))
                   (f req!str req)))
         (let filetype (static-filetype req!op)
           (aif (and filetype (static-file req!op))
                (do (prn (type-header* filetype))
                    (awhen static-max-age*
                      (prn "Cache-Control: max-age=" it))
                    (prn)
                    (w/infile i it
                      (whilet b (readb i)
                        (writeb b))))
                (respond-not-found req)))))

(def respond-not-found (req)
  (prn nfheader*)
  (prn)
  (prn unknown-msg*))

(def headerlines (lines)
  (trues [only.cdr ((scheme regexp-match) "^([-a-zA-Z]+)[ \t]*:[ \t]*(.*)" _)]
         cdr.lines))
