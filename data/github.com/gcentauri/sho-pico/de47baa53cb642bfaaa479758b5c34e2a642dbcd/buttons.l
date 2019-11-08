# 2019-08-08
# shoshin

(load "@lib/http.l" "@lib/xhtml.l" "@lib/form.l" "@lib/ps.l")

# Game
##----

(default *Bounds '(1 . 100))

(de small ()
   (car *Bounds) )

(de big ()
   (cdr *Bounds) )

(de guess-my-number ()
   (if (> (small) (big))
      "you cheated!"
      (/ (+ (small) (big)) 2) ) )

(de smaller ()
   (unless (str? (guess-my-number))
      (set '*Bounds (cons (small) (dec (guess-my-number)))) )
   (guess-my-number) )

(de bigger ()
   (unless (str? (guess-my-number))
      (set '*Bounds (cons (inc (guess-my-number)) (big))) )
   (guess-my-number) )

# Gui
#-----

(de guess ()
   (app)
   (action
      (html 0 "Guess My Number" "@sho-pico/buttons.css" NIL
         (form NIL
            (<div> 'container
               (<div> 'main
                  (gui '(+Style +View +TextField) "guess" '(pack (guess-my-number)))
                  (<div> 'buttons
                     (gui '(+JS +Button) "SMALLER" '(smaller))
                     (gui '(+JS +Button) "BIGGER" '(bigger)) ) ) ) ) ) ) )

(de go ()
   (server 8081 "!guess") )
