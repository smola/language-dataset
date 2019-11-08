;    $c 0 + = -> ( ) term wff |- $.
($c + = -> term wff TT)


;    $v t r s P Q $.
($v u r s P Q)

;    tt $f term t $.
($f tu term u)
($f tr term r)
($f ts term s)
;    wp $f wff P $.
($f wp wff P)
($f wq wff Q)


;    tze $a term 0 $.
($a tze nil nil (term 0))
($a tpl nil nil (term (+ u r)))
($a weq nil nil (wff (= u r)))
($a wim nil nil (wff (-> P Q)))
($a a1 nil nil (TT (-> (= u r) (-> (= u s) (= r s)))))
($a a2 nil nil (TT (= (+ u 0) u)))
($a mp (min (TT P) maj (TT (-> P Q))) nil (TT Q))


($p th1 nil nil (TT (= u u))
    (tu tze tpl tu weq tu tu weq tu a2 tu tze tpl
     tu weq tu tze tpl tu weq tu tu weq wim tu a2
     tu tze tpl tu tu a1 mp mp))

($p th2 nil nil (TT (= u (+ u 0)))
    (tu tze tpl tu tze tpl weq tu tu tze tpl weq
     tu tze tpl th1 
     tu tze tpl tu weq 
     tu tze tpl tu tze tpl weq tu tu tze tpl weq wim
     tu a2 tu tze tpl tu tu tze tpl a1 mp mp))

;
;(TT (= u (+ u 0)))
;(TT (-> (= (+ u 0) u) (-> (= (+ u 0) (+ u 0)) (= u (+ u 0)))))
;(TT (-> (= (+ u 0) (+ u 0)) (= u (+ u 0))))   mp
;
;(TT (= u (u + 0)))

