;Функция задания вопроса
(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
; преобразование 
(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE 
       else FALSE))
;состояние здоровья
(defrule health-state ""
   (not (health-state complaint ?))
   (not (repair ?))
   =>
   (if (yes-or-no-p "У вас есть жалобы на здоровье (yes/no)? ") 
    then 	(assert (health-state disease))                 
   else (assert (repair "Вы здоровы!!!Удачного вам дня"))))
;состояние головы
(defrule disease-state-head ""
   (health-state disease)
   (not ( disease-state-head?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас  болит голова (yes/no)? ")
       then	(assert (disease-state headache yes))                     
    else (assert (disease-state headache no)) 
  ))
  ;живота
  (defrule disease-state-pain""
   (health-state disease)
   (disease-state headache no)
   (not ( disease-state-pain?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас  болит живот (yes/no)? ")
       then	(assert (disease-state pain yes))                     
    else (assert (disease-state pain no)) 
  ))
  ;состояние сердца
  (defrule disease-state-heart""
   (health-state disease)
   (disease-state headache no)
   (disease-state pain no)
   (not ( disease-state-heart?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас есть боль в области сердца (yes/no)? ")
       then	(assert (disease-state heart yes))                     
    else (assert (disease-state heart no)) 
  ))
;Head
(defrule disease-state-head-temperature ""
   (disease-state headache yes)
   (not ( disease-state-head-temperature?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас высокая температура (yes/no)? ")
       then	(assert (head-state temperature yes))                     
    else (assert (head-state temperature no)) 
  ))
  (defrule disease-state-head-sneezing ""
   (disease-state headache yes)
   (head-state temperature no)
   (not ( disease-state-head-sneezing?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "Вы чихаете (yes/no)? ")
       then	(assert (head-state sneezing yes))                     
    else (assert (head-state sneezing no)) 
  ))
  (defrule disease-state-head-throat ""
   (disease-state headache yes)
   (head-state temperature yes)
   (not ( disease-state-head-throat?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас есть заложенность носа или боль в горле (yes/no)? ")
       then	(assert (head-state throat yes))                     
    else (assert (head-state throat no)) 
  ))

(defrule head-state-help-temperature ""
   (head-state throat yes)
   (not (repair ?))
   =>
   (if  (yes-or-no-p "У вас температура выше 39 градусов (yes/no)? ")
      then (assert (repair "По всей вероятности у вас ГРИПП. Необходимо обратиться в больницу!!! "))
      else (assert (repair "По всей вероятности у вас ОРВИ. Необходимо обратиться в больницу!!! "))))

(defrule head-state-turbulent ""
(declare (salience 15))
(head-state sneezing yes)
   (not (repair ?))
   =>
   (if  (yes-or-no-p "Ваша болезнь бурно развивалась? (yes/no)? ")
      then (assert (repair "По всей вероятности у вас ОРВИ. Необходимо обратиться в больницу!!! "))
    else  (assert (repair "Вы простудились!!!рекомендуем пить чай с медом и лимоном, а так же можно принять римантадин.Будьте здоровы!!!"))))
(defrule head-state-sneezing ""
(declare (salience 10))
(head-state sneezing yes)
   (not (repair ?))
   =>
   (if  (yes-or-no-p "Проблемы со здоровьем начались не давно(2-3 дня) (yes/no)? ")
    then (assert (repair "Вы простудились!!!рекомендуем пить чай с медом и лимоном, а так же можно принять римантадин.Будьте здоровы!!!"))))
;End HEAD
(defrule disease-state-pain-sharp ""
   (disease-state pain yes)
   (not ( disease-state-pain-sharp?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "Какая у вас боль в животе резкая (yes/no)? ")
       then	(assert (pain-state sharp yes))                     
    else
      (if (yes-or-no-p "Какая у вас боль в животе острая (yes/no)? ")
      then	(assert (pain-state acute yes))
      else  (assert (pain-state unknow)) )
  ))
(defrule disease-state-pain-help1 ""
   (pain-state acute yes)
   (not ( disease-state-pain-help?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас есть изжога (yes/no)? ")
       then	(assert (repair "У вас по всей вероятности язва !!!Необходимо срочно обратиться к врачу!!!"))                     
    else (assert (repair "У вас по всей видимости аппендицит !!!Необходимо срочно обратиться к врачу!!!"))     
  ))
  (defrule disease-state-pain-help ""
   (pain-state sharp yes)
   (not ( disease-state-pain-help?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас есть изжога (yes/no)? ")
       then	(assert (repair "У вас по всей вероятности химический ожог слизистых оболочек или отравление!!!Примите активированное угля!!!"))                          
  ))
;End pain

  ;состояние heart
  (defrule disease-state-heart-arterial-pressure ""
   (disease-state heart yes)
   (not ( disease-state-heart?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас давление выше 140 (yes/no)? ")
       then	(assert (heart-arterial-pressure yes))                     
    else (assert (heart-arterial-pressure no)) 
  ))
 (defrule disease-state-heart-arterial-pressure-high ""
  (heart-arterial-pressure yes)
   (not ( disease-state-heart-arterial-pressure-high?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас есть покраснение глаз (yes/no)? ")
       then	(assert (heart-eyes  yes))                     
    else (assert (heart-eyes  no)) 
  ))
 (defrule disease-state-heart-weakness ""
  (heart-arterial-pressure no)
   (not ( disease-state-heart-weakness?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас есть слабость в теле (yes/no)? ")
       then	(assert (heart-weakness  yes))                     
    else (assert (heart-weakness  no)) 
  ))
   (defrule disease-state-heart-dizziness ""
  (heart-weakness  yes)
   (not ( disease-state-heart-dizziness?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас есть отеки или головокружения (yes/no)? ")
       then	(assert (heart-dizziness  yes))                     
    else (assert (heart-dizziness  no)) 
  ))
(defrule disease-state-heart-help ""
  (heart-weakness  yes)
  (heart-dizziness  yes)
   (not ( disease-state-heart-help?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас плохой сон (yes/no)? ")
       then	(assert (repair "У вас по всей вероятности проблемы с сердцем!!!Выпейте зеленого чая, успокойтесь, полежите на кровати!!!Когда полегчает обратитесь к кардиологу"))                      
  ))
  (defrule disease-state-heart-help1 ""
  (heart-eyes  yes)
   (not ( disease-state-heart-help1?))
   (not (repair ?))   
   =>
   (if (yes-or-no-p "У вас плохой сон (yes/no)? ")
       then	(assert (repair "У вас по всей вероятности проблемы с сердцем!!!Выпейте зеленого чая, успокойтесь, полежите на кровати!!!Когда полегчает обратитесь к кардиологу"))                      
  ))
;Enters
  (defrule no-repairs ""
  (declare (salience -10))
  (not (repair ?))
  =>
  (assert (repair "К сожалению мы не смогли определить чем вы больны, пожалуйста обратитесь в больницу!!!")))
  (defrule print-repair ""
  (declare (salience 10))
  (repair ?item)
  =>
  (printout t crlf crlf)
  (printout t "Рекомендации по болезни:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))
(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Система Health Help")
  (printout t crlf crlf)
)