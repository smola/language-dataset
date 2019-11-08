import text/StringTokenizer
import mustang/Value


Context: class extends HashValue {
    init: func ~fromHashValue(hash: HashValue) {
        super(hash hash())
    }
    init: func ~emptyContext { super() }

    resolve: func(expression: String) -> Value {
        offset := expression indexOf(' ')

        // If just a simple root hash access, return value quickly
        if(offset == -1) {
            return getValue(expression)
        }

        // For complex, chained accesses, tokenize and resolve each one
        current: HashValue = this as HashValue
        next: Value = null
        for(name: String in StringTokenizer new(expression, ' ')) {
            next = current getValue(name)
            if(!next || next instanceOf(HashValue)) break
            else current = next as HashValue
        }

        return next
    }
}
