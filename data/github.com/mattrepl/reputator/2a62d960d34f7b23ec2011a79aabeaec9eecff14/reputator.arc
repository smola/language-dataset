; start of something neat...

(= title* "reputator")
(= datadir* "data/") 

(def datadir (file)
  (string datadir* "/" file))

(deftem point
  to nil
  from nil
  val nil
  date nil
  note nil)

(= pointfile* (datadir "points"))
(= points* (temloadall 'point pointfile*))

(= users*
   (sort < (flat (map [keys _]
                      (load-tables "arc/hpw")))))

(def user? (name)
  (mem name users*))

(mac rep-page body
  `(whitepage
     (center
       (widtable 600
         (tag b (link title* "about"))
         (br 3)
         ,@body
         (br 3)
         (w/bars (link "scoreboard")
                 (link "about"))))))

(defopl user req
  (aif (user? (arg req "name"))
       (user-page (reduce 'string it))
       (not-found)))

(defop login req 
  (login-page 'login "your reputation proceeds you."
              (list nilfn "scoreboard")))

(def user-page (name)
  (rep-page
    (pr name "   " "(" (score name) ")")
    (br 2)
    (aform (fn (req)
             (let user (get-user req)
               (add-point name
                          user
                          (if (is (arg req "val") "+1") 1 -1)
                          (arg req "note")))
             (user-page name))
           (tab
             (row "+/-" (menu "val" '("+1" "-1")))
             (row "note" (input "note" "" 60))
             (row ""      (submit))))
    (sptab
      (each pt (user-points name)
        (row pt!val pt!from pt!date pt!note)))))

(def user-points (name)
  (trues (fn (_) (if (is _!to name) _)) points*))

(def save-points ()
  (w/outfile of pointfile* (each x points* (write-table x of))))

(def add-point (to from val (o note nil) (o date (date)))
  (= points* (cons (inst 'point 'to to 'from from 'val val 'date date 'note note) points*))
  (save-points))

(def not-found ()
  (rep-page 
    (pr "no such user.")))

; Calculate score for user
(def score (user)
  (reduce + (map [_ 'val] (user-points user))))

(defopl scoreboard req
  (rep-page
    (pr "scoreboard")
    (br 2)
    (sptab
      (each u users*
        (tr (td (w/link (user-page u) (pr u)))
            (td (pr (score u))))))))

(defop about req
  (rep-page
    (pr "about")))

(def rep-start ((o port 8080))
  (ensure-dir datadir*)
  (= srv* (asv port)))

(def rep-stop ()
  (= quitsrv* t))