(global db) ; sticking close to PCL
(setv db [])

(defn make-cd [title artist rating ripped]
  (do
    (setv cd {})
    (assoc cd "title" title)
    (assoc cd "artist" artist)
    (assoc cd "rating" rating)
    (assoc cd "ripped" ripped))
  cd)

(defn add-record [cd]
  (.append db cd))

; helper
(defn print-dict [d]
  (for [key (.keys d)]
       (print (+ key ": " (str (get d key))))))

(defn dump-db []
  (for [cd db]
       (print-dict cd)
       (print)))

(defn y-or-n-p [prompt]
  (do
    (setv answer (get (.lower (raw_input prompt)) 0))
    (if (in answer (, "y" "n"))
      answer
      (y-or-n-p prompt)
    )))

(defn prompt-for-cd []
  (make-cd
    (raw_input "Title: ")
    (raw_input "Artist: ")
    (raw_input "Rating: ")
    (y-or-n-p "Ripped [y/n]: ")))

; straying from PCL using while here
(defn add-cds []
    (do
      (add-record (prompt-for-cd))
      (while (= "y" (y-or-n-p "Another? [y/n]: "))
             (add-record (prompt-for-cd)))))

(import json) ; for context...
(defn save-db [fname]
  (with [[f (open fname "w")]] (.write f (json.dumps db))))

(defn load-db [fname]
  (with [[f (open fname)]] (setv db (json.loads (.read f)))))
