
;;;======================================================
;;;     Automotive Expert System
;;;
;;;     This expert system does a proper diagnoses of some of the problems associated with 
;;;     engine,tyre,brake,headlight,steering and suspension.
;;;
;;;     PROJECT BY :
;;;     
;;;     SWAPNANEEL NANDY (IIT2014111)
;;;     UTKARSH SRIVASTAVA (IIT2014507)
;;;     AMIT VIJAY (IIT2014110)
;;;     MOHD ABDULLAH (ISM2014004)
;;;     SHIVAM BERI (IIT2014159)
;;;     
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

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

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))


;;;***************
;;;* QUERY RULES *
;;;***************


(defrule determine-problem-type ""
   (not (problem-type ?))
   (not (repair ?))
   =>
   (assert (problem-type
      (ask-question "What is the problem type (engine/tyres/suspension/headlight/brake)? "
                    engine tyres suspension headlight brake))))


;;;*************************
;;;* QUERY RULES FOR ENGINE*
;;;*************************


(defrule determine-engine-condition ""
   (problem-type engine)
   (not (engine-starting ?))
   (not (repair ?))
   =>
   (assert (engine-starting (yes-or-no-p "Does the engine starts properly  (yes/no)? "))))
   
(defrule determine-runs-properly ""
   (problem-type engine)
   (engine-starting yes)
   (not (repair ?))
   =>
   (assert (runs-properly (yes-or-no-p "Does the engine run normally (yes/no)? "))))

(defrule determine-rotation-of-engine ""
   (problem-type engine)
   (engine-starting no)
   (not (repair ?))   
   =>
   (assert (engine-rotating-properly (yes-or-no-p "Does the engine rotate (yes/no)? "))))
   
(defrule determine-sluggishness ""
   (problem-type engine)
   (runs-properly no)
   (not (repair ?))
   =>
   (assert (engine-sluggish-state (yes-or-no-p "Is the engine sluggish (yes/no)? "))))
   
(defrule determine-misfiring ""
   (problem-type engine)
   (runs-properly no)
   (not (repair ?))
   =>
   (assert (engine-misfires-state (yes-or-no-p "Does the engine misfire (yes/no)? "))))

(defrule determine-knocking ""
   (problem-type engine)
   (runs-properly no)
   (not (repair ?))
   =>
   (assert (engine-knocks-state (yes-or-no-p "Does the engine knock (yes/no)? "))))

(defrule determine-output-state ""
   (problem-type engine)
   (runs-properly no)
   (not (repair ?))
   =>
   (assert (engine-output-low-high
               (yes-or-no-p "Is the output of the engine low (yes/no)? "))))

(defrule determine-level-of-fuel ""
   (problem-type engine)
   (engine-starting no)
   (engine-rotating-properly yes)
   (not (repair ?))
   =>
   (assert (tank-has-fuel-filled
              (yes-or-no-p "Does the tank have any fuel in it (yes/no)? "))))

(defrule determine-charged-battery-status ""
   (problem-type engine)
   (engine-rotating-properly no)
   (not (repair ?))
   =>
   (assert (battery-charge-status-not-empty
              (yes-or-no-p "Is the battery charged (yes/no)? "))))

(defrule determine-point-surface-state ""
   (problem-type engine)
   (or (and (engine-starting no)      
            (engine-rotating-properly yes))
       (engine-output-low-high yes))
   (not (repair ?))
   =>
   (assert (point-surface-state
      (ask-question "What is the surface state of the points (normal/burned/contaminated)? "
                    normal burned contaminated))))

(defrule determine-electrical-conduction-status ""
   (problem-type engine)
   (engine-starting no)      
   (engine-rotating-properly no)
   (battery-charge-status-not-empty yes)
   (not (repair ?))
   =>
   (assert (electrical-conduction-status-success
              (yes-or-no-p "Is the conductivity test for the ignition coil positive (yes/no)? "))))
              
              
;;;*************************
;;;* QUERY RULES FOR TYRES *
;;;*************************


(defrule determine-tyre-state ""
   (problem-type tyres)
   (not (tyre-inflated ?))
   (not (repair ?))
   =>
   (assert (tyres-inflated (yes-or-no-p "Are the tyres inflated (yes/no)? "))))
   
(defrule determine-check-puncture ""
   (problem-type tyres)
   (tyres-inflated no)
   (not (repair ?))
   =>
   (assert (tyres-puncture (yes-or-no-p "Are the tyres punctured (yes/no)? "))))
   
(defrule determine-check-alignment ""
   (problem-type tyres)
   (tyres-inflated yes)
   (not (repair ?))
   =>
   (assert (tyres-alignment (yes-or-no-p "Are the tyres aligned (yes/no)? "))))
   
(defrule determine-check-movement ""
   (problem-type tyres)
   (tyres-inflated yes)
   (tyres-alignment yes)
   (not (repair ?))
   =>
   (assert (tyres-movement (yes-or-no-p "Are the tyres moving freely (yes/no)? "))))
   
(defrule determine-check-vibration ""
   (problem-type tyres)
   (tyres-inflated yes)
   (tyres-alignment yes)
   (tyres-movement yes)
   (not (repair ?))
   =>
   (assert (tyres-vibration (yes-or-no-p "Are the tyres vibrating (yes/no)? "))))
   
 
(defrule determine-check-mud ""
   (problem-type tyres)
   (tyres-inflated yes)
   (tyres-alignment yes)
   (tyres-movement yes)
   (tyres-vibration yes)
   (not (repair ?))
   =>
   (assert (tyres-mud-present (yes-or-no-p "Is mud or dirt present in rims (yes/no)? "))))
   
(defrule determine-check-tyre-noise ""
   (problem-type tyres)
   (tyres-inflated yes)
   (tyres-alignment yes)
   (tyres-movement yes)
   (tyres-vibration yes)
   (tyres-mud-present no)
   (not (repair ?))
   =>
   (assert (tyres-noise-present (yes-or-no-p "Is there excessive play or noise from the wheel bearings (yes/no)? "))))



;;;*****************************
;;;* QUERY RULES FOR HEADLIGHT *
;;;*****************************



(defrule determine-headlight-type ""
   (problem-type headlight)
   (not (headlight-type ?))
   (not (repair ?))
   =>
   (assert (headlight-type
      (ask-question "Is the headlight (fluctuating/dim/always_off)? "
                    fluctuating dim always_off))))
   
(defrule determine-check-alternator ""
   (problem-type headlight)
   (headlight-type fluctuating)
   (not (repair ?))
   =>
   (assert (headlight-connections-tight (yes-or-no-p "Are the connections tight (yes/no)? "))))
   
(defrule determine-dim-type ""
   (problem-type headlight)
   (headlight-type dim)
   (not (repair ?))
   =>
   (assert (dim-type
      (ask-question "Is the headlight always dim or when something like AC is turned ON (always/not_always)? "
                    always not_always))))
   
(defrule determine-alternator ""
   (problem-type headlight)
   (headlight-type dim)
   (dim-type not_always)
   (not (repair ?))
   =>
   (assert (check-alternator (yes-or-no-p "Is the alternator old (yes/no)? "))))
   
   
   
;;;**************************
;;;* QUERY RULES FOR BRAKES *
;;;**************************


(defrule determine-hard-brake ""
   (problem-type brake)
   (not (hard-brake ?))
   (not (repair ?))
   =>
   (assert (hard-brake (yes-or-no-p "Are the brake pedals hard (yes/no)? "))))
   
(defrule determine-car-unused ""
   (problem-type brake)
   (hard-brake yes)
   (not (repair ?))
   =>
   (assert (brake-car-unused (yes-or-no-p "Has the car been unused for a long time (yes/no)? "))))
   
(defrule determine-brake-squishy ""
   (problem-type brake)
   (hard-brake no)
   (not (repair ?))
   =>
   (assert (brake-squishy (yes-or-no-p "Does the brake pedal feels Squishy (yes/no)? "))))
   
(defrule determine-brake-master-cylinder ""
   (problem-type brake)
   (hard-brake no)
   (brake-squishy yes)
   (not (repair ?))
   =>
   (assert (brake-master-cylinder (yes-or-no-p "Is there a sign of leakage of fluid in the master cylinder (yes/no)? "))))
   
(defrule determine-brake-side-pull ""
   (problem-type brake)
   (hard-brake no)
   (brake-squishy no)
   (not (repair ?))
   =>
   (assert (brake-side-pull (yes-or-no-p "Does the car pulls to one side when braking (yes/no)? "))))
  
(defrule determine-brake-pedal-pulse ""
   (problem-type brake)
   (hard-brake no)
   (brake-squishy no)
   (brake-side-pull no)
   (not (repair ?))
   =>
   (assert (brake-pedal-pulse (yes-or-no-p "Does the brake pedal pulses Up/Down (yes/no)? "))))
   
(defrule determine-rotor-status ""
   (problem-type brake)
   (hard-brake no)
   (brake-squishy no)
   (brake-side-pull no)
   (brake-pedal-pulse yes)
   (not (repair ?))
   =>
   (assert (brake-rotors-thick (yes-or-no-p "Are the rotors still thick (yes/no)? "))))
   
   
;;;*******************************************
;;;* QUERY RULES FOR STEERING AND SUSPENSION *
;;;*******************************************


(defrule determine-porpoising-over-bumps ""
   (problem-type suspension)
   (not (repair ?))
   =>
   (assert (porpoising-over-bumps (yes-or-no-p "Is the vehicle porpoising over bumps or uneven roads (yes/no)? "))))

(defrule determine-suspension-worn-shocks ""
   (problem-type suspension)
   (porpoising-over-bumps yes)
   (not (repair ?))
   =>
   (assert (suspension-worn-shocks (yes-or-no-p "Are the shocks worn (yes/no)? "))))

(defrule determine-suspension-pulling-one-side ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (not (repair ?))
   =>
   (assert (suspension-pulling-one-side (yes-or-no-p "Is the vehicle pulling to one side while driving (yes/no)? "))))

(defrule determine-suspension-tyre-inflation ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (suspension-pulling-one-side yes)
   (not (repair ?))
   =>
   (assert (suspension-tyre-inflation-solved (yes-or-no-p "Check tyre inflation. Is the problem solved (yes/no)? "))))

(defrule determine-crack-rod-steering-rack ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (suspension-pulling-one-side yes)
   (suspension-tyre-inflation-solved no)
   (not (repair ?))
   =>
   (assert (crack-rod-steering-rack (yes-or-no-p "Are there cracks in the rod/steering rack (yes/no)? "))))

(defrule determine-flip-flop-wheel ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (suspension-pulling-one-side no)
   (not (repair ?))
   =>
   (assert (flip-flop-wheel (yes-or-no-p "Is the flip flop wheel shimmy (yes/no)? "))))

(defrule determine-suspension-wheel-balance ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (suspension-pulling-one-side no)
   (flip-flop-wheel yes)
   (not (repair ?))
   =>
   (assert (suspension-wheel-balance (yes-or-no-p "Check tyre inflation and wheel balance. Is the problem solved (yes/no)? "))))


(defrule determine-clunking-over-bumps ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (suspension-pulling-one-side no)
   (flip-flop-wheel no)
   (not (repair ?))
   =>
   (assert (clunking-over-bumps (yes-or-no-p "Is the vehicle clunking over bumps (yes/no)? "))))

(defrule determine-loose-steering ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (suspension-pulling-one-side no)
   (flip-flop-wheel no)
   (clunking-over-bumps no)
   (not (repair ?))
   =>
   (assert (loose-steering (yes-or-no-p "Is the vehicle having loose steering (yes/no)? "))))

(defrule determine-steering-vibrations ""
   (problem-type suspension)
   (porpoising-over-bumps no)
   (suspension-pulling-one-side no)
   (flip-flop-wheel no)
   (clunking-over-bumps no)
   (loose-steering no)
   (not (repair ?))
   =>
   (assert (steering-vibrations (yes-or-no-p "Is the steering vibrating at high speeds (yes/no)? "))))


;;;***************************
;;;* REPAIR RULES FOR ENGINE *
;;;***************************


(defrule engine-decisions ""
   (problem-type engine)
   (runs-properly yes)
   (not (repair ?))
   =>
   (assert (repair "No repair needed.")))

(defrule engine-sluggish-state ""
   (problem-type engine)
   (engine-sluggish-state yes)
   (not (repair ?))
   =>
   (assert (repair "Clean the fuel line."))) 

(defrule engine-misfires-state ""
   (problem-type engine)
   (engine-misfires-state yes)
   (not (repair ?))
   =>
   (assert (repair "Point gap adjustment.")))     

(defrule engine-knocks-state ""
   (problem-type engine)
   (engine-knocks-state yes)
   (not (repair ?))
   =>
   (assert (repair "Timing adjustment.")))

(defrule tank-filled-or-empty ""
   (problem-type engine)
   (tank-has-fuel-filled no)
   (not (repair ?))
   =>
   (assert (repair "Add fuel.")))

(defrule battery-notnormal-state ""
   (problem-type engine)
   (battery-charge-status-not-empty no)
   (not (repair ?))
   =>
   (assert (repair "Charge the battery.")))

(defrule point-surface-state-burned ""
   (problem-type engine)
   (point-surface-state burned)
   (not (repair ?))
   =>
   (assert (repair "Replace the points.")))

(defrule point-surface-state-contaminated ""
   (problem-type engine)
   (point-surface-state contaminated)
   (not (repair ?))
   =>
   (assert (repair "Clean the points.")))

(defrule electrical-conduction-status-success-yes ""
   (problem-type engine)
   (electrical-conduction-status-success yes)
   (not (repair ?))
   =>
   (assert (repair "Repair the distributor lead wire.")))

(defrule electrical-conduction-status-success-no ""
   (problem-type engine)
   (electrical-conduction-status-success no)
   (not (repair ?))
   =>
   (assert (repair "Replace the ignition coil.")))
   
   
;;;***************************
;;;* REPAIR RULES FOR TYRES  *
;;;***************************

(defrule normal-tyre-state-conclusions ""
   (problem-type tyres)
   (tyres-inflated yes)
   (tyres-alignment yes)
   (tyres-movement yes)
   (tyres-vibration no)
   (not (repair ?))
   =>
   (assert (repair "No repair needed.")))

   
(defrule tyres-punctured-yes ""
   (problem-type tyres)
   (tyres-inflated no)
   (tyres-puncture yes)
   (not (repair ?))
   =>
   (assert (repair "Get Puncture Repaired.")))
   
   
(defrule tyres-punctured-no ""
   (problem-type tyres)
   (tyres-inflated yes)
   (tyres-puncture no)
   (not (repair ?))
   =>
   (assert (repair "Get Tyres Inflated.")))

(defrule tyres-aligned-no ""
   (problem-type tyres)
   (tyres-inflated yes)
   (tyres-alignment no)
   (not (repair ?))
   =>
   (assert (repair "Get Tyres Aligned.")))
   
(defrule tyres-movement-no ""
   (problem-type tyres)
   (tyres-movement no)
   (not (repair ?))
   =>
   (assert (repair "Apply oil in the axle.")))
   
(defrule tyres-mud-yes ""
   (problem-type tyres)
   (tyres-mud-present yes)
   (not (repair ?))
   =>
   (assert (repair "Clean mud or dirt packed in the back of the rim.")))
   
(defrule tyres-noise-yes ""
   (problem-type tyres)
   (tyres-noise-present yes)
   (not (repair ?))
   =>
   (assert (repair "Change Loose, worn or damaged wheel bearings.")))
   
   
;;;*******************************
;;;* REPAIR RULES FOR HEADLIGHTS *
;;;*******************************


(defrule headlight-connection-tight ""
   (problem-type headlight)
   (headlight-type fluctuating)
   (headlight-connections-tight yes)
   (not (repair ?))
   =>
   (assert (repair "Disconnect the battery and clean all contact points using a anti corossion liquid."))) 

(defrule headlight-fix-loose-connections ""
   (problem-type headlight)
   (headlight-type fluctuating)
   (headlight-connections-tight no)
   (not (repair ?))
   =>
   (assert (repair "Fix all loose connections.")))     

(defrule headlight-replace-bulb ""
   (problem-type headlight)
   (headlight-type dim)
   (dim-type always)
   (not (repair ?))
   =>
   (assert (repair "Replace the bulb.")))
   
(defrule replace-alternator ""
   (problem-type headlight)
   (headlight-type dim)
   (dim-type not_always)
   (check-alternator yes)
   (not (repair ?))
   =>
   (assert (repair "Replace alternator with high amperage.")))

(defrule repair-alternator ""
   (problem-type headlight)
   (headlight-type dim)
   (dim-type not_always)
   (check-alternator no)
   (not (repair ?))
   =>
   (assert (repair "Repair alternator.")))

(defrule repair-connector ""
   (problem-type headlight)
   (headlight-type always_off)
   (not (repair ?))
   =>
   (assert (repair "Inspect the electrical connector on back. If corroded, remove corrosion. Else replace connector.")))
   
   
;;;*******************************
;;;* REPAIR RULES FOR BRAKES     *
;;;*******************************


(defrule brake-car-unused-yes ""
   (problem-type brake)
   (hard-brake yes)
   (brake-car-unused yes)
   (not (repair ?))
   =>
   (assert (repair "Check for rusting near pedals."))) 
   
(defrule brake-car-unused-no ""
   (problem-type brake)
   (hard-brake yes)
   (brake-car-unused no)
   (not (repair ?))
   =>
   (assert (repair "Leaky vaccum or defective brake booster."))) 
   
(defrule brake-master-leakage-yes ""
   (problem-type brake)
   (brake-squishy yes)
   (brake-master-cylinder yes)
   (not (repair ?))
   =>
   (assert (repair "Fix the leakage."))) 

(defrule brake-master-leakage-no ""
   (problem-type brake)
   (brake-squishy yes)
   (brake-master-cylinder no)
   (not (repair ?))
   =>
   (assert (repair "Check for internal damamge. Replace the cylinder")))
   
(defrule brake-car-pull-yes ""
   (problem-type brake)
   (brake-side-pull yes)
   (not (repair ?))
   =>
   (assert (repair "Possibility of frozen caliper.")))  
   
(defrule brake-rotors-thick-yes ""
   (problem-type brake)
   (brake-rotors-thick yes)
   (not (repair ?))
   =>
   (assert (repair "Resurface Rotors.")))  
   
(defrule brake-rotors-thick-no ""
   (problem-type brake)
   (brake-rotors-thick no)
   (not (repair ?))
   =>
   (assert (repair "Replace the pads.")))  
   
   
;;;********************************************
;;;* REPAIR RULES FOR STEERING AND SUSPENSION *
;;;********************************************


(defrule suspension-worn-shocks-yes ""
   (problem-type suspension)
   (suspension-worn-shocks yes)
   (not (repair ?))
   =>
   (assert (repair "Replace shocks."))) 

(defrule suspension-worn-shocks-no ""
   (problem-type suspension)
   (suspension-worn-shocks no)
   (not (repair ?))
   =>
   (assert (repair "Replace leaf springs.")))

;;; check tyre  inflation yes suspension wheel balance yes

(defrule suspension-tyre-inflation-solved-yes ""
   (problem-type suspension)
   (or (suspension-tyre-inflation-solved yes)
       (suspension-wheel-balance yes))
   (not (repair ?))
   =>
   (assert (repair "Problem Solved.")))

(defrule crack-rod-steering-rack-yes ""
   (problem-type suspension)
   (crack-rod-steering-rack yes)
   (not (repair ?))
   =>
   (assert (repair "Replace rods.")))

(defrule crack-rod-steering-rack-no ""
   (problem-type suspension)
   (crack-rod-steering-rack no)
   (not (repair ?))
   =>
   (assert (repair "Replace brake caliper.")))

(defrule suspension-wheel-balance-no ""
   (problem-type suspension)
   (suspension-wheel-balance no)
   (not (repair ?))
   =>
   (assert (repair "Inspect tyres and replace them in pairs. Inspect tie rods and repair if needed.")))

(defrule suspension-clunking-over-bumps-yes ""
   (problem-type suspension)
   (clunking-over-bumps yes)
   (not (repair ?))
   =>
   (assert (repair "Replace worn shacks, worn bearings and warn ball joints."))) 
   
(defrule suspension-loose-steering-yes ""
   (problem-type suspension)
   (loose-steering yes)
   (not (repair ?))
   =>
   (assert (repair "Check power steering fluid, worn bearing and broken rack mounts.")))
   
(defrule suspension-steering-vibrations-yes ""
   (problem-type suspension)
   (steering-vibrations yes)
   (not (repair ?))
   =>
   (assert (repair "Check wheel balance, loose wheel bolts and warped brake rotors."))) 


;;;******************************
;;;* REPAIRS NOT MENTIONED      *
;;;******************************
   
(defrule no-repairs ""
  (declare (salience -10))
  (not (repair ?))
  =>
  (assert (repair "Take your car to a mechanic.")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The Engine Diagnosis Expert System")
  (printout t crlf crlf))

(defrule print-repair ""
  (declare (salience 10))
  (repair ?item)
  =>
  (printout t crlf crlf)
  (printout t "Suggested Repair:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))

