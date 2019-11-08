true asJson := method(buffer,
    buffer ifNilEval(Sequence clone) appendSeq("true")
)

false asJson := method(buffer,
    buffer ifNilEval(Sequence clone) appendSeq("false")
)

nil asJson := method(buffer,
    buffer ifNilEval(Sequence clone) appendSeq("null")
)

Sequence asJson := method(buffer,
    buffer ifNilEval(Sequence clone) appendSeq("\"", self asMutable escape, "\"")
)

Number asJson := method(buffer,
    buffer ifNilEval(Sequence clone) appendSeq(self)
)

List asJson := method(buffer,
    buffer = buffer ifNilEval(Sequence clone) appendSeq("[")
    first := true
    self foreach(x, if(first, first = false, buffer appendSeq(", ")); x asJson(buffer))
    buffer appendSeq("]")
)

Object asJson := method(buffer,
    buffer = buffer ifNilEval(Sequence clone) appendSeq("{")
    first := true
    self slotNames sort foreach(name,
        if (first, first = false , buffer appendSeq(", "))
        name asJson(buffer)
        buffer appendSeq(":")
        self getSlot(name) asJson(buffer)
    )
    buffer appendSeq("}")
)

Map asJson := method(buffer,
    buffer = buffer ifNilEval(Sequence clone) appendSeq("{")
    first := true
    self keys sort foreach(name,
        if (first, first = false , buffer appendSeq(", "))
        name asJson(buffer)
        buffer appendSeq(":")
        self at(name) asJson(buffer)
    )
    buffer appendSeq("}")
)

JsonObject := Object clone do(
    type := "JsonObject"

    protected := method(
        self protected = Map clone
    )

    addSlot := method(name, value,
        if(protectedSlotNames contains(name),
            protected atPut(name, value)
        ,
            self setSlot(name, value)
        )
        self
    )

    asJson := method(buffer,
        buffer = buffer ifNilEval(Sequence clone) appendSeq("{")
        first := true
        keys sort foreach(name,
            if (first, first = false , buffer appendSeq(", "))
            name asJson(buffer)
            buffer appendSeq(":")
            protected at(name) ifNilEval(self getSlot(name)) asJson(buffer)
        )
        buffer appendSeq("}")
    )

    at := method(name,
        if (protectedSlotNames contains(name),
            protected at(name)
        ,
            self getSlot(name)
        )
    )

    atPut := method(name, value,
        if (protectedSlotNames contains(name),
            protected atPut(name, value)
        ,
            self setSlot(name, value)
        )
    )

    keys := method(
        names := self slotNames selectInPlace(n, protectedSlotNames contains(n) not)
        names appendSeq(protected keys)
    )

    protectedSlotNames := slotNames append("protectedSlotNames", "protected", "protos", "slotNames", "getSlot", "setSlot", "", "type")
)

JsonParser := Object clone do (
    type := "JsonParser"
    newSlot("index", 0)
    newSlot("buffer")

    parseJson := method(
        whitespace
        object
    )


    current := method(buffer at(index))
    currentIsDigit := method(current isDigit)

    currentIs := method(string,
        string foreach(i, char, if(buffer at(index + i) != char, return false))
        true
    )

    inc := method(amount, index = index + (amount ifNilEval(1)))

    whitespace := method(
        while(index < buffer size and current isSpace, inc)
    )

    useLiteralIfPresent := method(string,
        if(currentIs(string), inc(string size) true, false)
    )

    optionalLiteral := method(string,
        if(useLiteralIfPresent(string),
            whitespace
            true
        ,
            false
        )
    )

    literal := method(string, error,
        optionalLiteral(string) ifFalse(
            Exception raise ("JsonParser " .. error)
        )
    )

    object := method(
        result := JsonObject clone
        literal("{", "Expected a '{'.")
        if(isString,
            result addSlot(string, literal(":", "Expected a ':'."); value)
            while(optionalLiteral(","),
                result addSlot(string, literal(":", "Expected a ':'."); value)
            }
            literal("}", "Expected a '}', or ','.")
        ,
            literal("}", "Expected a '}', or quoted string.")
        )
        result
    )

    array := method(
        result := List clone
        literal("[", "Expected a '['.");

        if(isValue,
            result append(value)
            while(optionalLiteral(","),
                result append(value)
            )
            literal("]", "Expected a ']', or ','.");
        ,
            literal("]", "Expected a ']', or a value.");
        )

        result
    )

    isObject := method(currentIs("{"))
    isArray := method(currentIs("["))
    isString := method(currentIs("\""))
    isNumber := method(currentIs("-") or current isDigit)

    isTrue := method(currentIs("true"))
    isFalse := method(currentIs("false"))
    isNull := method(currentIs("null"))

    isValue := method(
        isObject or isArray or isString or isNumber or isTrue or isFalse or isNull
    )

    value := method(
        if(isObject, return object)
        if(isArray, return array)
        if(isString, return string)
        if(isNumber, return number)

        if(optionalLiteral("true"), return true)
        if(optionalLiteral("false"), return false)
        if(optionalLiteral("null"), return nil)

        Exception raise("JsonParser Expected a value and instead found '" .. current asCharacter .. "'. A value is an object('{'), array('['), string('\"'), number(-,digit), true, false or null. " .. buffer exSlice(index))
    )

    string := method(
        result := Sequence clone
        quote := "\""
        slash := "\\"
        if(currentIs(quote),
            inc
        ,
            Expected raise("JsonParser Expected a '\"'.")
        )
        while(currentIs(quote) not,
            if(currentIs(slash),
                result append(current)
                inc
                if(currentIs("u"),
                    inc(2)
                    result append(buffer exSlice(index, index + 2) fromBase(16))
                    inc(2)
                )
            )
            result append(current)
            inc
        )
        if(currentIs(quote),
            inc
        ,
            Expected raise("JsonParser Expected a '\"'.")
        )
        whitespace
        result unescape
        result
    )

    number := method(
        buf := Sequence clone

        start := index

        useLiteralIfPresent("-")
        if(currentIs("0"),
            inc
        ,
            while(currentIsDigit, inc)
        )

        if(useLiteralIfPresent("."),
            while(currentIsDigit, inc)
        )

        if(useLiteralIfPresent("e") or useLiteralIfPresent("E"),
            useLiteralIfPresent("-") or useLiteralIfPresent("+")
            while(currentIsDigit, inc)
        )

        result := buffer exSlice(start, index) asNumber

        whitespace
        result
    )
)

Sequence parseJson := method(JsonParser clone setBuffer(self) parseJson)
File parseJson := method(asBuffer parseJson)

#x := Object clone
#x a := 1
#x b := 2
#x list := list(1,2,3,4)
#x t := true
#x f := false
#x n := nil
#x s := "seq"
#x o := Object clone do( x := 13; y := 19 )
#x m := Map clone atPut("x", 13) atPut("y", 19)
#x mut := "\"" asMutable
#x hex := 19 asCharacter # Doesn't work as the character is stripped out becuase Io doesn't support escaping.
#x jobj := JsonObject clone addSlot("setSlot", 13) addSlot("addSlot", "hehe")
#
#json := x asJson
#writeln(json)
#
#y := json parseJson
#writeln(y asJson)
