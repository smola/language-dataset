import ceylon.language.meta.model {
	Type
}
import ceylon.language.meta {
	type,
	typeLiteral
}

"Converts provided data into other specific type. This is core API for providing logic into Codamo. 
 It act's as a kind of controller, for many (most of simple) conversions, it is only required component,
  which needs to be implemented and registered."
by ("Wojciech Potiopa")
shared abstract class Converter<Source, Result, ResultType=Type<Result>>()
		satisfies Conversion<Source,Result,ResultType> & Registrable
		given ResultType satisfies Type<Result> {
	
	shared class Matcher(
		shared Boolean predicate(Source source, ResultType resultType),
		shared Integer priority) {
	}
	
	shared default Matcher? matcher = null;
	
	shared formal actual Result convert(Delegator delegator, Source source, ResultType resultType);
	
	shared actual Anything visitAdapter(Registrable.Adapter visitor) => visitor.conversion(this);
	
	string => "``type(this).declaration.name`` ``typeLiteral<Source>()`` -> ``typeLiteral<Result>()`` ";
}
