Character testIfExists() {
    if (exists c = "test".first) {
        return c;
    } else if (!exists c = "".first) {
        return 0.character;
    } else {
        return 0.character;
    }
}
Character testIfNonempty() {
    if (nonempty args = process.arguments) {
        return 'a';
    } else if (!nonempty args = process.arguments) {
        return 'e';
    } else {
        return 0.character;
    }
}
Character testIf() {
    if ("hello" == "world") {
        return #7F.character;
    }
}
