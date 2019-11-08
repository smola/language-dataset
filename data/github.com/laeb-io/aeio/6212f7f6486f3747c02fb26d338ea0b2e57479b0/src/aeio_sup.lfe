(module: aeio_sup
	(behaviour supervisor)
	(export (start_link 0) (init 1)))

(include: "aelisp/include/aelisp.macro")

(func: start_link ()
	(~ supervisor:start_link (MODULE) ()))

(func: init _ (->*
	SupFlags = #M(strategy one_for_one intensity 1 period 5)
	ChildSpecs = `(
		#M(id spawner
			start #(spawner start_link ())
			restart permanent
			shutdown brutal_kill
			type worker
			modules (spawner))
		#M(id aeio
			start #(aeio start_link ())
			restart permanent
			shutdown brutal_kill
			type worker
			modules (aeio)))
	`#(ok #(,SupFlags ,ChildSpecs))))
	
