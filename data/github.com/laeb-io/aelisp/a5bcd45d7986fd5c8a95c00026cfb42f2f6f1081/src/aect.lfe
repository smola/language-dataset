(defmodule aect
	(export all))

(include-lib "../include/aelisp.macro")

(func: run_suites (Suites) 
	(~ ct:run_test `(#(dir "ebin") #(suite ,Suites) #(auto_compile false)
		#(logdir "test/log") #(logopt no_src))))

(func: test_module (M)
	(test_modules `(,M)))

(func: test_modules (Modules) (->
	Suites = (fmap (\X. (lta! (++ (atl X) "_SUITE"))) Modules)
	(foreach #'reload/1 Suites)
	(run_suites Suites)))

(func: reload (M) (->
	(~ code:purge M)	
	(~ code:load_file M)))

(func: repeat () (->*
	Modified = (~ code:modified_modules)
	Suites = (filter (\X. (~ lists:suffix "_SUITE" (atl X))) Modified)
	(foreach #'reload/1 Modified)
	(run_suites Suites)))
