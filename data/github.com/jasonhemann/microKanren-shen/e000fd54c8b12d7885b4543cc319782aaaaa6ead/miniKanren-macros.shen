(defmacro inverse-eta-delay-macro
  [inverse-eta-delay G] -> (let TEMP (intern (str (gensym s/c)))
                             [lambda TEMP [freeze [G TEMP]]]))

(defmacro disj-macro
  [disj G] -> G
  [disj G0 G1 G2 | G*] -> [disj G0 [disj G1 G2 | G*]])

(defmacro conj-macro
  [conj G] -> G
  [conj G0 G1 G2 | G*] -> [conj G0 [conj G1 G2 | G*]])

(defmacro conde-macro
  [conde | CL*] -> [inverse-eta-delay [disj | (map (lambda CL (cons conj CL)) CL*)]])

(defmacro fresh-macro
  [fresh [] | G*] -> [conj | G*]
  [fresh [X | X*] | G* ] -> [call/fresh [lambda X [fresh X* | G*]]])

(defmacro run-macro
  [run N [Q] | G*] -> [mK-reify [take N [call/empty-state [fresh [Q] | G*]]]])

  