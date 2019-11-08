;;;;;
;;;;;
;;;;;
(require marx.language)
(import wheatley.utils)


(defmacro wheatley-spawn [&rest args]
  `(do (import wheatley.utils)
       (go (wheatley.utils.wheatley-simple-launch docker [~@args]))))


(defmacro daemon [&rest args]
  `(do (disown
       (import wheatley.utils)
       (go (wheatley.utils.wheatley-daemon docker [~@args])))))


(defmacro job [&rest args]
  (setv every (:every (wheatley.utils.group-map keyword? args)))
  `(run-every ~@every
    (disown (import wheatley.utils)
            (go (wheatley.utils.wheatley-job docker [~@args])))))


(defmacro wheatley [&rest forms]
  `(marx 
    ; bringup code.
    (print "
              .,-:;//;:=,
          . :H@@@MM@M#H/.,+%;,
       ,/X+ +M@@M@MM%=,-%HMMM@X/,
     -+@MM; $M@@MH+-,;XMMMM@MMMM@+-  Good news: that is NOT a docking station.
    ;@M@@M- XM@X;. -+XXXXXHHH@M@M#@/.  So there's one mystery solved. I'm going
  ,%MM@@MH ,@%=             .---=-=:=,.  to attempt a manual override on this
  =@#@@@MX.,                -%HX$$%%%:;   wall. Could get a bit technical!
 =-./@M@M$                   .;@MMMM@MM:
 X@/ -$MM/                    . +MM@@@M$            Hold on!!
,@M@H: :@:                    . =X#@@@@-
,@@@MMX, .                    /H- ;@M@M=
.H@@@@M@+,                    %MM+..%#$.
 /MMMM@MMH/.                  XM@MH; =;
  /%+%$XHH@$=              , .H@@@@MX,
   .=--------.           -%H.,@@@@@MX,
   .%MM@@@HHHXX$$$%+- .:$MMX =M@@MM%.
     =XMMM@MM@MM#H;,-+HMM@M+ /MMMX=
       =%@M@M#@$-.=$@MM@@@M; %M%=
         ,:+$+-,/H#MMMMMMM@= =,
               =++%%%%+/:-.")
    ~@forms))
