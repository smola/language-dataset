import os/Time

uuid_parse: extern func(in: Char*, uu: UUID)
uuid_unparse: extern func(uu: UUID, out: Char*)

UUID_STRING_SIZE := const 36

UUID: cover from Pointer { // TODO: yeah that's a bit evil
    clear: extern(uuid_clear) func
    copy: extern(uuid_copy) func (dst: UUID)
    compare: extern(uuid_compare) func (second: UUID) -> Int
    generate: extern(uuid_generate) func
    generateRandom: extern(uuid_generate_random) func
    generateTime: extern(uuid_generate_time) func
    generateTimeSafe: extern(uuid_generate_time_safe) func
    isNull: extern(uuid_is_null) func -> Bool
    parse: func(in: String) {
        uuid_parse(in toCString(), this)
    }
    unparse: func -> String {
        chars := gc_malloc(Char size * UUID_STRING_SIZE) as Char*
        uuid_unparse(this, chars)
        String new(chars, UUID_STRING_SIZE)
    }
    // TODO: unparse_upper / unparse_lower
    time: extern(uuid_time) func(retTv: TimeVal*) -> TimeT

    new: static func -> This {
        gc_malloc(This size) as This
    }
}

generateUUID: func -> String {
    uuid := UUID new()
    uuid generate()
    uuid unparse()
}
