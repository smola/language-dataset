// *** Symbols loading ****

NSTemporaryDirectory_ = dlsym(RTLD_DEFAULT, "NSTemporaryDirectory");
NSTemporaryDirectory = new Functor(NSTemporaryDirectory_, "@");

// *** Constants ****

let commonStructsMap = {
    "CGRect": "CGRect",
    "CGPoint": "CGPoint",
    "CGSize": "CGSize",
    "CFDictionary": "NSDictionary *"
};

let commonTypes = {
    "d": "double",
    "i": "int",
    "f": "float",
    "c": "BOOL",
    "s": "short",
    "I": "unsigned int",
    "l": "long",
    "q": "long long",
    "L": "unsigned long",
    "C": "unsigned char",
    "S": "unsigned short",
    "Q": "unsigned long long",
    "B": "BOOL",
    "b": "unsigned int",
    "v": "void",
    "*": "char*",
    ":": "SEL",
    "#": "Class",
    "@": "id",
    "@?": "id",
    "Vv": "void",
    "rv": "const void*",
};

function range(n) {
    return Array.apply(null, Array(n)).map(function (_, i) { return i; });
}

// *** Protocols ****

function dumpProtocolMethods(protocol, isRequiredMethod) {
    let body = isRequiredMethod ? "@required\n" : "@optional\n";
    var totalMethods = 0;

    for (isInstanceMethod of [true, false]) {
        var methodsCount = new int;
        let methods = protocol_copyMethodDescriptionList(protocol,
            isRequiredMethod, isInstanceMethod, methodsCount);

        totalMethods += *methodsCount;
        let methodPrefix = isInstanceMethod ? "-" : "+";
        for (var i = 0; i < *methodsCount; i++) {
            let selectors = methods[i][0].toString().split(":");
            let sign = [NSMethodSignature
                        signatureWithObjCTypes:methods[i][1]];
            let returnType = typeForAttribute([sign methodReturnType], "");
            var params = [];
            if ([sign numberOfArguments] < 3) {
                params.push(selectors[0]);
            }

            for (var j = 0; j < [sign numberOfArguments] - 2; j++) {
                let argument = [sign getArgumentTypeAtIndex:j + 2];
                let type = typeForAttribute(argument, "", 0);
                params.push(selectors[j] + ":(" + type + ")arg" + j);
            }

            let method = params.join(" ");
            body += methodPrefix + "(" + returnType + ")" + method + ";\n";
        }

        free(methods);
    }

    return totalMethods > 0 ? body : "";
}

function dumpProtocol(protocol) {
    let header = "@protocol " + protocol_getName(protocol) + "\n";

    var propertiesCount = new int;
    let list = protocol_copyPropertyList(protocol, propertiesCount);
    let properties = range(*propertiesCount).map(i => dumpProperty(list[i]));

    free(list);

    return header + properties.join("\n") + "\n" +
        dumpProtocolMethods(protocol, false) +
        dumpProtocolMethods(protocol, true) + "@end";
}

function dumpProtocolsFromClass(Class, outputdir) {
    var protocolsCount = new int;
    let protocols = class_copyProtocolList(Class, protocolsCount);
    var protocolNames = [];
    for (var i = 0; i < *protocolsCount; i++) {
        if (class_conformsToProtocol(Class, protocols[i])) {
            let protocolBody = dumpProtocol(protocols[i]) + "\n";
            let protocolName = protocol_getName(protocols[i]);
            let path = outputdir + protocolName + ".h";
            [protocolBody writeToFile:path atomically:YES];

            protocolNames.push(protocolName);
        }
    }
    free(protocols);

    return protocolNames;
}

// *** Properties and variable dumping ****

function typeForAttribute(attribute, name) {
    let postfix = (name.length > 0 ? " " + name : "");

    if (attribute[0] == "^") {
        attribute = attribute.slice(1);
    }

    // Check if it's an objc object
    if (attribute.slice(0, 2) == "@\"") {
        // Check if it's a property with an enforced protocol.
        if (attribute[2] == "<") {
            return "id" + attribute.slice(2, -1) + postfix;
        }

        return attribute.slice(2, -1) + " *" + name;

    } else if (attribute[0] == "{") {
        // First try to match known structs.
        for (struct in commonStructsMap) {
            let replace = commonStructsMap[struct];
            if (attribute.search(struct) >= 0) {
                return replace + postfix;
            }
        }

        let n = (attribute[1] == "?") ? 4 : 3;
        let values = attribute.slice(n, -1).split("\"");
        let types = [for (i in values) if (i % 2 != 0) values[i]]
        let names = [for (i in values) if (i % 2 == 0) values[i]]
        let members = names.map(function(name, i) {
            if (types[i] === undefined || types[i][0] == undefined) {
                return "";
            }

            return "        " + typeForAttribute(types[i][0], name);
        });

        return "struct {\n" + members.join(";\n") + "\n    } " + name;
    }

    let attrs = commonTypes[attribute] || attribute;
    return attrs + postfix;
}

function dumpProperty(property) {
    let name = property_getName(property);
    let attributes = property_getAttributes(property).split(",");
    let typeAndName = typeForAttribute(attributes[0].slice(1), name);

    var properties = [];
    for each (attribute in attributes.slice(1)) {
        if (attribute.slice(0, 2) == "V_") {
            attribute = attribute.slice(2)
        }

        if (attribute.length == 1) {
            let modifierMap = {
                "R": "readonly",
                "C": "copy",
                "&": "retain",
                "N": "nonatomic",
                "D": "@dynamic",
                "W": "__weak",
                "P": "t<encoding>"
            };
            properties.unshift(modifierMap[attribute] || "")
        }

        if (attribute[0] == "G" || attribute[0] == "S") {
            let modifier = attribute[0] == "G" ? "getter" : "setter";
            let property = modifier + "=" + attribute.slice(1);
            properties.unshift(property);
        }
    }

    let modifiers = properties.length > 0 ? properties.join(", ") : "assign";
    return "@property (" + modifiers + ") " + typeAndName + ";";
}

function dumpPropertiesFromClass(Class) {
    var count = new int;
    let properties = class_copyPropertyList(Class, count);
    let lines = range(*count).map(i => dumpProperty(properties[i]));
    free(properties);

    return lines.join("\n");
}

function dumpIvarsFromClass(Class) {
    var count = new int;
    let ivarList = class_copyIvarList(Class, count);
    let ivars = range(*count).map(function(i) {
        let ivarName = ivar_getName(ivarList[i]);
        let ivarType = ivar_getTypeEncoding(ivarList[i]);
        return "    " + typeForAttribute(ivarType, ivarName) + ";";
    });

    free(ivarList);

    return ivars.join("\n");
}

function dumpMethod(method, isInstance) {
    var signature = method_getName(method);
    let charType = method_copyReturnType(method);
    let returnType = typeForAttribute(String.fromCharCode(*charType), "");

    let methodParts = signature.toString().split(":");
    if (methodParts.length > 1) {
        let methods = range(methodParts.length - 1).map(function(i) {
            let type = method_copyArgumentType(method, i + 2);
            let typeName = typeForAttribute(String.fromCharCode(*type), "");
            free(type);

            return methodParts[i] + ":(" + typeName + ")arg" + (i + 1);
        });

        signature = methods.join(" ");
    }

    free(charType);
    let prefix = isInstance ? "-" : "+";
    return prefix + " (" + returnType + ")" + signature + ";";
}

function dumpMethodsFromClass(Class) {
    var count = new int;
    var classCount = new int;
    let methods = class_copyMethodList(Class, count);
    let classMethods = class_copyMethodList(object_getClass(Class), classCount);

    let lines = range(*count).map(i => dumpMethod(methods[i], true))
    let classLines = range(*classCount).map(i => dumpMethod(classMethods[i]));

    free(methods);
    free(classMethods);

    return lines.concat(classLines).join("\n");
}

// *** General dump (main methods) ****

function dumpBundle(bundle, prefix) {
    prefix = prefix == undefined ? nil : prefix
    try {
        [FZBundler class];
    } catch (e) {
        loadFZBundler();
    }

    [objc_getClass("FZBundler") dumpBundle:bundle prefix:prefix];
}

function _classIsInternal(name) {
    return [name hasPrefix:@"NS"] || [name hasPrefix:@"_"] ||
        [name hasPrefix:@"MF"] || [name hasPrefix:@"UI"] ||
        [name hasPrefix:@"AV"];
}

function loadFZBundler() {
    @implementation FZBundler : NSObject {}
    + (id)dumpBundle:(NSBundle *)bundle prefix:(NSString *)prefix {
        var enumerator = [ObjectiveC.classes keyEnumerator];
        while ((name = [enumerator nextObject])) {
            let hasPrefix = prefix == null || [name hasPrefix:prefix];
            let isBundle = [[NSBundle bundleForClass:objc_getClass(name)]
                            isEqual:bundle];

            if (isBundle && hasPrefix && !_classIsInternal(name)) {
                [self performSelectorInBackground:@selector(dump:)
                                       withObject:name];
            }
        }
    }

    + (void)dump:(NSString *)name {
        try {
            classDump(objc_getClass(name.toString()));
        }
        catch (e) {
        }
    }
    @end
}

function classDump(Class) {
    let outputdir = NSTemporaryDirectory();
    let path = @(outputdir + [Class description] + ".h");

    if ([[NSFileManager defaultManager] fileExistsAtPath:path isDirectory:false]) {
        NSLog("[classdump] Class already dumped " + Class);
        return path
    }

    NSLog("[classdump] Dumping class %@ into %@", Class, path);

    // Protocol(s) dump
    let protocols = dumpProtocolsFromClass(Class, outputdir);
    let imports = protocols.map(name => "#import <" + name + ".h>").join("\n");
    protocols = protocols.length ? " <" + protocols.join(", ") + ">" : "";

    let interface = "@interface " + Class;
    if ([[Class superclass] className] !== undefined) {
        // Add parent class to interface (as needed)
        interface += " : " + [Class superclass];
    }
    interface += " " + protocols;

    let ivars = dumpIvarsFromClass(Class);
    let properties = dumpPropertiesFromClass(Class);
    let methods = dumpMethodsFromClass(Class);
    let body = imports + "\n\n" + interface + "{\n" + ivars + "\n}\n\n" +
        properties + "\n" + methods + "\n\n@end\n";

    // Save class dump into .h file.
    [body writeToFile:path atomically:YES];

    return path;
}
