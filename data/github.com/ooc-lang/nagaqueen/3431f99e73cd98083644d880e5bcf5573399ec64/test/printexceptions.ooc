
use printexceptions, nagaqueen
import nagaqueen/OocListener

MyListener: class extends OocListener {

    init: func

    parse: func (path: String) {
        try {
            super(path)
        } catch (e: Exception) {
            "Got exception: #{e message}" println()
        }
    }

}


main: func (argc: Int, argv: CString*) {

    if(argc > 1) {
        "Parsing %s" printfln(argv[1])
        MyListener new() parse(argv[1] toString())
    } else {
        "Usage: %s FILE.ooc" printfln(argv[0])
    }
    
}
