
// sdk
import structs/[ArrayList]
import text/StringTokenizer

// Shove stuff, send it over the wire,
// then pull back out stuff

ZBag: class {

    stuff: ArrayList<String>

    readonly := false

    init: func ~write {
        stuff = ArrayList<String> new()
    }

    init: func ~fromStuff (=stuff) {
        readonly = true
    }

    _check: func (str: String) {
        if (readonly) {
            complain("Trying to shove stuff in a readonly zbag.")
        }

        if (str contains?('\n')) {
            complain("Trying to shove a newline in a zbag. Escape your shit, yo.")
        }
    }

    preshove: func (str: String) {
        _check(str)
        stuff add(0, str)
    }

    shove: func (str: String) {
        _check(str)
        stuff add(str)
    }

    shoveInt: func (value: Int) {
        shove(value toString())
    }

    shoveFloat: func (value: Float) {
        shove(value toString())
    }

    pull: func -> String {
        stuff removeAt(0)
    }

    pullInt: func -> Int {
        pull() toInt()
    }

    pullFloat: func -> Int {
        pull() toFloat()
    }

    pullCheck: func (value: String) {
        recv := pull()
        if (recv != value) {
            complain("Network error: expected %s, got %s" format(recv, value))
        }
    }

    // utility func

    complain: static func (message: String) {
        raise(message)
    }

    extract: static func (message: String) -> This {
        tokens := message split('\n')
        This new(tokens)
    }

    pack: func -> String {
        stuff join("\n")
    }

    make: static func (first: String, args: ...) -> This {
        bag := This new()
        bag shove(first)

        args each(|arg|
            match arg {
                case s: String => bag shove(s)
                case i: Int =>    bag shoveInt(i)
                case f: Float =>  bag shoveFloat(f)
                case => complain("What are you trying to shove in the bag?")
            }
        )
        bag
    }

    first: func -> String {
        stuff first()
    }

}

