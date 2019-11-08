import structs/[HashBag, HashMap, ArrayList]

import optparse/Parser

Option: abstract class {
    key: String
    help := ""
    metaVar := ""

    init: func (=key) {}

    help: func (=help) {}
    metaVar: func (=metaVar) {}

    storeValue: func <T> (parser: Parser, value: T) {
        parser values put(key, value)
    }
    
    storeDefault: abstract func (parser: Parser)
    activate: abstract func (parser: Parser, reader: CommandLineReader) -> Bool
    createHelp: abstract func -> String
}

SimpleOption: abstract class extends Option {
    longName := ""
    shortName := ""

    init: func ~noarg {} // heaven knows

    activate: func (parser: Parser, reader: CommandLineReader) -> Bool {
        token := reader peek()
        longNameTemplate := "--" + longName
        shortNameTemplate := "-" + shortName
        if((!longName empty?() && token == longNameTemplate) \
            || (!shortName empty?() && token == shortNameTemplate)) {
            reader skip()
            activate2(parser, reader)
            return true
        }
        return false
    }

    createHelp: func -> String {
        buf := Buffer new()
        buf append(" ")
        // has short name.
        if(!shortName empty?()) {
            buf append("-%s" format(shortName))
            if(!longName empty?()) {
                buf append(", ")
            }
        }
        // has long name.
        if(!longName empty?()) {
            buf append("--%s" format(longName))
        }
        // metavar!
        if(!metaVar empty?()) {
            buf append(' ') .append(metaVar)
        } else {
            buf append("\t")
        }
        // tab!
        buf append("\t\t")
        // description!
        if(!help empty?()) {
            buf append(help)
        }
        buf append('\n')
        buf toString()
    }

    longName: func (=longName) {}
    shortName: func (=shortName) {}

    activate2: abstract func (parser: Parser, reader: CommandLineReader)
}

ToggleOption: class extends SimpleOption {
    store := true
    defaultValue := false

    init: func ~doggl (=key) {}

    activate2: func (parser: Parser, reader: CommandLineReader) {
        storeValue(parser, store)
    }

    storeDefault: func (parser: Parser) {
        storeValue(parser, defaultValue)
    }

    store: func (=store) {}
    defaultValue: func (=defaultValue) {}
}

StringOption: class extends SimpleOption {
    defaultValue := ""

    init: func ~s (=key) {}

    activate2: func (parser: Parser, reader: CommandLineReader) {
        storeValue(parser, reader get())
    }

    storeDefault: func (parser: Parser) {
        storeValue(parser, defaultValue)
    }

    defaultValue: func (=defaultValue) {}
}

ListOption: class extends SimpleOption {
    init: func ~ichLiebeWurstsalat (=key) {}

    activate2: func (parser: Parser, reader: CommandLineReader) {
        parser values get(key, ArrayList<String>) add(reader get())
    }

    storeDefault: func (parser: Parser) {
        storeValue(parser, ArrayList<String> new())
    }
}

MapOption: class extends SimpleOption {
    init: func ~leckerWurstsalat (=key) {}

    activate2: func (parser: Parser, reader: CommandLineReader) {
        token := reader get()
        key := ""
        value := ""
        if(token contains?('=')) {
            key = token substring(0, token indexOf('='))
            value = token substring(token indexOf('=') + 1)
        } else {
            key = token
            value = reader get()
        }
        parser values get(this key, HashMap<String, String>) put(key, value)
    }

    storeDefault: func (parser: Parser) {
        storeValue(parser, HashMap<String, String> new())
    }
}
