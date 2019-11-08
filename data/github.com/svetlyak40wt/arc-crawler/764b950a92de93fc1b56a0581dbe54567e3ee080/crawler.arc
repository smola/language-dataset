; Получить список коммитеров
; https://api.github.com/repos/svetlyak40wt/dotfiler/commits

; Получить список репозиториев коммитера
; https://api.github.com/users/svetlyak40wt/repos

; Получить список файлов репозитория
; https://api.github.com/repos/svetlyak40wt/dotfiler/git/trees/master?recursive=1
; Если есть .arc файлы, добавить в список и перейти к шагу 1

; тупо через поиск гитхаба
; https://api.github.com/search/repositories?q=language:arc&sort=stars&order=desc
; но работает плохо, например тут всего 1 .arc файл, да и то там про arc нет ничего
; При этом GitHub показывает что код на 80% из Arc
; https://github.com/bixo/bixo

; http://developer.github.com/v3/search/#search-repositories

; Интересное
; https://github.com/sacado/arc2c - Arc Lisp to C compiler
; https://github.com/awwx/ar - заброшенная попытка реализовать платформу для создания языков, подобных Arc
; https://github.com/evanrmurphy/lava-script - заброшенная попытка сделать Arc, компилирующийся в JavaScript
; https://github.com/arclanguage/rainbow-js - вариант Arc для запуска в браузере (2 года не обновлялся)
; https://github.com/akkartik/arc - развивающийся форк Arc3.1
; https://github.com/hasenj/sehm - s-expression based html markup for arc
; https://github.com/esden/arc/blob/2686877db06809d1ba1280fe417c3ff02ef02e58/lib/module/python.arc - форк Arc для встраиваемых систем (и у них есть система модулей)
; https://github.com/hchbaw/swank-arc - попытка сделать swank сервер для arc (более 4 лет не обновлялось)
; https://github.com/shader/metagame какая-то вебовая игрушка на arc (2 года)
; https://github.com/mgiken японская компания, использующая arc. У них много библиотечек
; https://github.com/smihica/glaze - реализация Arc на C.

; GitHub Paging:
; Link: <https://api.github.com/repositories/3935/commits?top=master&last_sha=fbd3354db3bb4f5598222a78544ecf30e66a14b2>; rel="next", <https://api.github.com/repositories/3935/commits?sha=master>; rel="first"

($ (require (file "lib/json.ss")))
($ (xdef read-json read-json))
($ (xdef write-json write-json))

($ (require racket/date))
($ (xdef find-seconds find-seconds))
($ (xdef current-seconds current-seconds))


(= num-requests* 0)

; parses date in format "2014-01-16T12:56:47Z"
; and returns seconds since epoch
(def parse-date (dt (o format "(\\d+)-(\\d+)-(\\d+)T(\\d+):(\\d+):(\\d+)Z"))
  (let match (re-match-pat format dt)
    (if match
        (let parsed (map [coerce _ 'int] (rev (cdr match)))
          (apply find-seconds parsed)))))


(def months-since (t)
  (/ (since t) (* 60 60 24 31)))


(def years-since (t)
  (/ (since t) (* 60 60 24 365)))


(def humanize-since (t)
  (with (days (coerce (days-since t) 'int)
              months (coerce (months-since t) 'int)
              years (coerce (years-since t) 'int))
    (if (< days 31) (+ "" days " day(s)")
        (< days 365) (+ "" months " month(s)")
        (> years 0) (+ "" years " year(s)"))))


(= oauth-token* "f619c243b2fb7492e5da8abb89dfd75cf5812b29")


(require "lib/re.arc")
(require "lib/web.arc")

(def get-next-link (headers)
  (caar (rem not
        (map [re-match "Link: .*<(.*?)>; rel=\"next\".*" _]
             headers))))


; helper to extract X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset
(def get-header (headers name)
  (let regex (+ "^" name ": (.*)$" )
    (cadar (rem not
          (map [re-match-pat regex _]
               headers)))))


(def parse-response (response)
  (w/instring f (cadr response) (read-json f)))


(def get-login (item)
  (let author (item 'author)
    (if author
        (author 'login))))


(def get-repo (item)
  (item 'full_name))


(def get-file (item)
  (obj path (item 'path)
       size (item 'size)))

(= cache* (table))
(= cache-hits* 0)
(= cache-misses* 0)
(= ratelimit-remaining* nil)
(= ratelimit-reset* 0)
(= http-times* nil)

(def gh-get (url)
  (++ num-requests*)
  (let data (cache* url)
    (if data
        (do
          (++ cache-hits*)
          data)
        
        (do
          (when (and ratelimit-remaining*
                     (< ratelimit-remaining* (* (len workers*) 4)))
            (let seconds-to-wait (+ (- ratelimit-reset* (seconds)) 60)
                 (prn "Damn ratelimit! Will sleep for " seconds-to-wait " seconds")
                 (sleep seconds-to-wait)))
          
            (withs (before (msec)
                    data (mkreq url nil "GET" nil (list (+ "Authorization: token " oauth-token*)))
                    after (msec))
                   
                   (push (- after before) http-times*)
                   
                   (= (cache* url) data)
                   (= ratelimit-remaining* (coerce (get-header (car data) "X-RateLimit-Remaining") 'int))
                   (= ratelimit-reset* (coerce (get-header (car data) "X-RateLimit-Reset") 'int))
                   (++ cache-misses*)
                   data)))))


(mac defget (name url-form process-form)
  `(def ,name (param)
     (let url ,url-form
       (letf work (url)
             (if url
                 (do
                   ;(prn "Downloading " url)
                   (withs (response (gh-get url)
                                    next-link (get-next-link (car response))
                                    parsed-response (parse-response response))
                     (+ ,process-form
                        (work next-link)))))
             (rem nil (dedup (work url)))))))

(defget files
        (+ "https://api.github.com/repos/" param "/git/trees/HEAD?recursive=1")
        (map get-file (parsed-response 'tree)))

(defget repos
        (+ "https://api.github.com/users/" param "/repos")
        (map get-repo parsed-response))

(defget commiters
        (+ "https://api.github.com/repos/" param "/commits")
        (map get-login parsed-response))

(defget watchers
        (+ "https://api.github.com/repos/" param "/subscribers")
        (map [_ 'login] parsed-response))


(def is-arc-project (files)
  (with (total-size 0 count 0)
        (each file files
          (if (endmatch ".arc" (file 'path))
              (do (++ count)
                  (++ total-size (file 'size)))))
        (if (> count 0) 
            (< (/ total-size count) 10000))))


(def calculate-score (repo)
  (withs (response (gh-get (+ "https://api.github.com/repos/" repo))
                   data (parse-response response)
                   stars (data 'stargazers_count)
                   watchers (data 'watchers_count)
                   pushed-at (parse-date (data 'pushed_at))
                   since-push (coerce (days-since pushed-at) 'int))
    (list repo
          (coerce (+ since-push
                     (/ 365 (max 1 (+ watchers stars))))
                  'int)
          pushed-at stars watchers)))


(= queue* nil)
(= workers* nil)
(= still-working* nil)


(def pop-job ()
  (atomic (pop queue*)))


(= debug-jobs* nil)

(mac start-job (name . body)
  `(if debug-jobs*
       (do ,@body)
       ; we put pairs (name, callback) to the queue
       (atomic (++ queue* (list (list ,name (fn () ,@body)))))))


(def start-workers (n)
  (for i 1 n
       (push (thread
               (while t
                      (let (name job) (pop-job)
                        (if job
                            (do (push name still-working*)
                                (on-err (fn (err) (prn "Error: " err)) job)
                                (pull name still-working*))
                            (sleep 1)))))
             workers*)))

 
(def stop-workers ()
  (each worker workers*
    (kill-thread worker))
  (= workers* nil))


(= processed-reps* (table))
(= processed-logins* (table))
(= results* nil)


(def process-repo (name depth)
  (when (and (> depth 0)
             (not (processed-reps* name)))
    
    (prn "Processing repo: " name " " depth)
    (when (is-arc-project (files name))
      (push name results*))
    
    (each login (+ (commiters name)
                   (watchers name))
          (start-job (+ "process-login:" login)
                     (process-login login (- depth 1))))
    
    (set (processed-reps* name))))


(def process-login (login depth)
  (when (and (> depth 0)
             (not (processed-logins* login)))
    (prn "Processing login: " login " " depth)
    (each repo (repos login)
          (start-job (+ "process-repo:" repo)
                     (process-repo repo
                                   (- depth 1))))
    (set (processed-logins* login))))


(def main ((o num-workers 10) (o depth 3))
  (do
   (= num-requests* 0)
   (= processed-reps* (table))
   (= processed-logins* (table))
   (= results* nil)
   (= queue* nil)
   (= cache-hits* 0)
   (= cache-misses* 0)
   (= still-working* nil)
   (= http-times* nil)
  
   (stop-workers)
   (start-workers num-workers)
   (start-job "process-root-repo" (process-repo "arclanguage/anarki" depth))

   (let start (seconds)
     (while (or (> (len queue*) 0)
                (> (len still-working*) 0))
       (prn (len still-working*) " jobs currently processes by workers:")
       (prn:string:intersperse ", " still-working*)
       (sleep 1))
     (prn "GOING TO RESULTS")
    
     (if results*
       (withs (scored-reps (sort (compare < cadr)
                                 (map calculate-score results*))
                           total-time (- (seconds) start)
                           rps (/ num-requests* (max total-time 1)))
         (prn #\newline "Results:" #\newline "========")
         (each (name score pushed-at stars watchers) scored-reps
           (prn name
                ", last push: " (humanize-since pushed-at)
                ", stars: " stars
                ", watchers: " watchers))
         (prn #\newline "Found reps: " (len scored-reps))
         (prn "Total time: " total-time)
         (prn "Num requests: " num-requests*)
         (prn "Cache hits: " cache-hits*)
         (prn "Cache misses: " cache-misses*)
         (prn "RPS: " rps)
         (prn "Avg HTTP response time: " (real (/ (apply + http-times*) (len http-times*)))))

       nil))))
