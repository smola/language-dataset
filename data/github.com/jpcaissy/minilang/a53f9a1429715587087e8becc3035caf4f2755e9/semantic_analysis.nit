module semantic_analysis

import minilang_test_parser

class SemanticAnalysis
	super Visitor

	var methods = new ArrayMap[String, Int]

	redef fun visit(n) do n.accept_semantic(self)
end

redef class Node
	fun accept_semantic(v: SemanticAnalysis) do visit_children(v)
end


redef class Ndef
	redef fun accept_semantic(v) do
		if v.methods.has_key(n_id.text) then
			print "Cannot redeclared method"
			exit(1)
		end
		v.methods[n_id.text] = n_params.number_of_children
	end
end

redef class Ncall
	redef fun accept_semantic(v) do
		super
		if not v.methods.has_key(n_id.text) then
			print "Method {n_id.text} was not declared"
			exit(1)
		end
		var nb_arguments = n_arguments.number_of_children
		if nb_arguments != v.methods[n_id.text] then
			print "Method {n_id.text} signature does not match"
			exit(1)
		end
	end
end
