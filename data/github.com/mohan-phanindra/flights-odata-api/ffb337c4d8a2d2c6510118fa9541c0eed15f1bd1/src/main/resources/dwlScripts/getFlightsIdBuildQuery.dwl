%dw 2.0
output application/java


// APIkit Odata Service creates a variable that contains the fields of your entity. It's a list of string (List<String>)
var entityFields : Array<String> = vars.odata.fields match {
	case fields is Array<String> -> fields
	else -> []
}


// APIkit Odata Service creates a variable that contains the keys of your entity
var keys : String = vars.odata.keyNames match {
	case keyNames is String -> keyNames
	else -> ""
}

// APIkit Odata Service creates a variable that contains the table's name 
var remoteEntityName = vars.odata.remoteEntityName match {
	case remoteEntityName is String -> remoteEntityName
	else -> ""	
}


// APIkit Odata Service puts your oData filters into the queryParams
var filters = attributes.queryParams

var select : String = filters.select match {
	case select is String -> select
	else -> ""
}

var id = attributes.uriParams.ID match {
	case id is String -> id
	else -> ""	
}

// Generate the fields you need in the query. 
// It checks for a select function in case you need less filters that you're actually exposing. 
// If there is no select present, it just returns your fields defined in your metadata
var generateSqlFields = (select) -> ((if (select != "" )( (select splitBy ",") -- (keys splitBy ",") ++ (keys splitBy ",")) else entityFields)  map "`$`" ) joinBy ", "
---

"SELECT " ++ generateSqlFields(select) ++ " FROM $remoteEntityName WHERE $keys = '$id'"
