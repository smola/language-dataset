foldingFunction=method(sum, x,
	case(x,
		"+", [sum pop! + sum pop!],
		"*", [sum pop! * sum pop!],
		"-", [sum pop! - sum pop!],
		"/", [sum pop! / sum pop!],
		[x toRational]
	) + sum
)
"8 1 2 + 5 * +" split fold([], sum, x, foldingFunction(sum, x)) println

List foldingFunction2 = method(x, 
	append!(case(x,
		"+", pop! + pop!,
		"*", pop! * pop!,
		"-", pop! - pop!,
		"/", pop! / pop!,
		x toRational
		)
	)
)
"8 1 2 + 5 * +" split prepend!([]) fold(foldingFunction2) println

List foldingFunction2 = method(x, 
	append!(case(x,
		or("+", "*", "-", "/"), Origin eval("#{pop!} #{x} #{pop!}"),
		x toRational
		)
	)
)
"8 1 2 + 5 * +" split prepend!([]) fold(foldingFunction2) println

List foldingFunction3 = method(x, 
	append!(case(x,
		or("+", "*", "-", "/"), Message fromText(x) appendArgument(pop!) sendTo(pop!),
		x toRational
		)
	)
)
"8 1 2 + 5 * +" split prepend!([]) fold(foldingFunction3) println


List foldingFunction3 = method(x, 
	append!(case(x,
		or("+", "*", "-", "/"), Message fromText(x) appendArgument(pop!) sendTo(pop!),
		x toRational
		)
	)
)
"8 1 2 + 5 * +" split prepend!([]) fold(foldingFunction3) println

e = method(str,
	str split fold([], s, x, s append!(case(x,
		or("+", "*", "-", "/"), Message fromText(x) appendArgument(s pop!) sendTo(s pop!),
		x toRational
		)
	))
)

e("8 1 2 + 5 * +") println

rpn1=fn(e,e split fold([],s,x,s append!(case(x,or("+","*","-","/"),Origin eval("#{s pop!}#{x}#{s pop!}"),x toRational))))
r=fn(e,e split fold([],s,x,s append!(if(x !~(#/[-\+\*\/]/), x toRational,Origin eval("#{s pop!}#{x}#{s pop!}")))))

r("8 1 2 + 5 * +") println