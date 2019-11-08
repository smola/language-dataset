; Toy swank.

; Author: Takeshi Banse <takebi@laafc.net>
; Licence: Public Domain

; Only a few things may work.

($:xdef getpid getpid)
(require "lib/dynvar.arc")
(require "lib/ppr.arc")

(def swank ((o port 4005)) (swank-server port nil))

(wipe swankpo*)

(def swankp args
  (do1 car.args
    (awhen swankpo*
      (w/stdout it
        (apply ppr args)
        (prn)
        (flushout)))))

(def swank-server (port portfile)
  (w/socket s port
    (prn "Listening on " port)
    (flushout)
    (awhen portfile (writefile port it))
    (let (i o _p) (socket-accept s)
      (swank-serve i o))))

(def swank-serve (i o)
  (after (swank-loop i o)
    (force-close i o)))

(dynvars swanki* swanko*)

(wipe swanktl*)

(def swank-loop (i o)
  (with* (swanki* i swanko* o)
    (while t
      (ccc [(= swanktl* _)
            (w/stdin swanki*
              (swank-dispatch:swank-read-packet))]))))

(def swank-dispatch (event)
  (let (x . xs) swankp.event
    (case x
      :emacs-rex (apply swank-emacs-rex xs)
      (err:+ "Unknown event: " event))))

(dynvar swankc*)

(def swank-emacs-rex (form pkg _th id)
  (let (x . xs) form
    (on-err [let* swankc* _ (sldb swankc*)]
            (fn () (swank-respond x xs pkg id)))))

(= swankfns* (table))

(def swank-respond (x xs pkg id)
  (with (ok nil result nil)
    (after (aif swankfns*.x
                (= result (apply it xs) ok t)
                (do (warn "Not implemented" x)
                    (flushout)))
      (swank-send-to-emacs
        `(:return ,(if ok `(:ok ,result) `(:abort)) ,id)))))

(def sldb (c)
  (swankp:details c)
  (swank-send-to-emacs (+ '(:debug 0 1) (sldb-info c 0 1)))
  (after (sldb-loop c)
    (swank-send-to-emacs '(:debug-return 0 1 nil))))

(def sldb-info (c start end)
  `((,(details c) " [Error]" nil)
    ,(sldb-restarts c)
    ,(sldb-backtrace c start end)
    nil))

(def sldb-loop (c)
  (while t (swank-dispatch:swank-read-packet)))

(def sldb-restarts (_c) '(("quit" "SLIME top-level.")))

(def sldb-backtrace (c start end)
  ((fn (xs)
     (with (i (max start 0) end (min len.xs end))
       (let noend [~<= end _]
         (drain (do1 (noend&xs i)
                  (++ i))))))
   `((0 ,(details c))
     (1 "Arc"))))

(def swank-send-to-emacs (form)
  (w/stdout swanko* (swank-write-packet form)))

(def swank-write-packet (sexp)
  (withs (payload (tostring:write sexp)
          hex (($ number->string) len.payload 16)
          pad (newstring (- 6 len.hex) #\0))
    (disp:+ pad hex payload)
    (flushout)))

(def swank-read-packet ()
  (let len (int (swank-readchunk 6) 16)
    (swank-read-sexp-from-string:swank-readchunk len)))

(def swank-readchunk (size)
  (let s (swank-readchunk-1 size)
    (do1 s
      (unless (is len.s size)
        (err:+ "Short read: expected " size " != " (len s) " [" s "]")))))

(def swank-readchunk-1 (size)
  (tostring
    (repeat size
      (awhen (readc)
        (pr (coerce it 'char))))))

(def swank-read-sexp-from-string (s) read.s)

(wipe swank-protocolver*)

(def swank-beforeinit (version)
  (= swank-protocolver* version))

(mac defsn (name parms . body)
  (let syn (sym:+ "swank:" name)
    `(= (swankfns* ',syn) (fn ,parms ,@body))))

(defsn connection-info ()
  `(,@'()
    :pid ,(getpid)
    :package (:name "arc" :prompt "arc")
    :lisp-implementation (:type "Arc" :name "arc")
    :version ,swank-protocolver*))

(defsn swank-require _args)

(defsn autodoc _args ':not-available)

(defsn operator-arglist (s p)
  (zap sym s)
  (awhen (sig ([_ s] (if ssyntax.s last:dedup:flat:ssexpand idfn)))
    (cons s it)))

(def swank-eval (s) (eval:read s))

(def swank-formatvalue (v)
  (if no.v "; No value"
      (+ "=> " (tostring:ppr v))))

(defsn interactive-eval (s) (swank-formatvalue:swank-eval s))
(defsn interactive-eval-region (s) (swank-formatvalue:swank-eval s))

(defsn listener-eval (s) `(:values ,(tostring:ppr:swank-eval s)))

(defsn throw-to-toplevel () (swanktl*))

(defsn backtrace (start end) (sldb-backtrace swankc* start end))

(defsn invoke-nth-restart-for-emacs (_ __) (swanktl*))

(defsn quit-lisp () (quit))
  
(defsn create-repl (_target) '("arc" "arc"))

(def swank-describesym (s)
  (zap sym s)
  (tostring
    (each x ([_ s] (if ssyntax.s dedup:flat:ssexpand flat))
      (pr #\* x #\newline helpstr.x #\newline))))

(defsn describe-symbol (s) swank-describesym.s)
(defsn describe-function (s) swank-describesym.s)
(defsn describe-definition-for-emacs (s _k) swank-describesym.s)
(defsn documentation-symbol (s . _) swank-describesym.s)

(def swank-applymacex (expander s) (tostring:ppr:expander:read s))

(defsn swank-macroexpand-1 (s) (swank-applymacex macex1 s))
(defsn swank-macroexpand (s) (swank-applymacex macex s))

(defsn swank-macroexpand-all (s)
  (($ (lambda (x)
        (with-output-to-string
          (lambda ()
            (pretty-print (ac (ac-denil x) ()))))))
   read.s))

(defsn buffer-first-change (_filename))
