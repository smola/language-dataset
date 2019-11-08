;; appel de la méthode println sur un objet de type Text
"Hello" println

;; 2 appels sur la même ligne
"hello" println. "world" println.

;; création d'une méthode
foo = method("Hello" println)
foo()

;; une méthode est un objet de type "DefaultMethod"
method() kind println

;; calcul du factoriel du nombre 10
fact = method(n, if((0..1) include?(n), 1, n * fact(n-1)))
fact(10) println

;; création d'un nouveau type
Foo = Origin mimic
Foo kind println

;; création d'une instance
foo = Foo mimic
foo kind println

;; obtenir des singletons en surchargeant la méthode de sous-typage
Foo = Origin mimic
Foo mimic = Foo
if(Foo == Foo mimic, "Foo is a singleton", "Foo is a normal type") println