import ceylon.interop.java {
    JavaList
}

import java.lang {
    JLong=Long,
    JString=String,
    Types {
        nativeString
    }
}
import java.util {
    HashMap
}

//TODO: move this generally-useful function into a library!
HashMap<JString,Object> velocityContext({<String->Anything>*} model) {
    value map = HashMap<JString,Object>();
    for (key -> item in model) {
        if (exists item) {
            map.put(nativeString(key),
                if (is Integer item)
                    then JLong(item)
                else if (is String item)
                    then nativeString(item)
                else if (is List<Anything> item)
                    then JavaList(item)
                else item);
        }
    }
    return map;
}
