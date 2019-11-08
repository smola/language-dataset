namespace Zhandlersocket;

/**
 * A simple collection of static methods to compose HS commands
 */

class Command {

    const DELIMITER = "\t";
    /**
     * compose open_index command
     */
    static public function open(<Index> index) -> string {
        var tokens = [
            "P",
            index->getNum(),
            index->getDbname(),
            index->getTable(),
            index->getIdx(),
            index->getColsAsString()
        ];
        if index->hasFcols() {
            let tokens[] = index->getFcolsAsString();
        }
        return self::compose(tokens);
    }
    /**
     * compose insert command
     */
    static public function insert(<Index> index, values) -> string {
        var tokens = [index->getNum(), "+", count(values)];
        var colValues = index->mapValues(values);
        let tokens = array_merge(tokens, colValues);
        return self::compose(tokens);
    }
    /**
     * compose update command
     */
    static public function update(<Index> index, <WhereClause> wc, values) -> string {
        var tokens = array_merge([index->getNum()], wc->toArray(), ["U"], index->mapValues(values));
        return self::compose(tokens);
    }
    /**
     * compose increment command
     */
    static public function increment(<Index> index, <WhereClause> wc, values) -> string {
        var tokens = array_merge([index->getNum()], wc->toArray(), ["+"], index->mapValues(values));
        return self::compose(tokens);
    }
    /**
     * compose delete command
     */
    static public function delete(<Index> index, <WhereClause> wc) -> string {
        var tokens = array_merge([index->getNum()], wc->toArray(), ["D"]);
        return self::compose(tokens);
    }
    /**
     * encode command tokens and compose a string
     */
    static public function encode(array tokens) -> string {
        var encodedTokens = [];
        var tok;
        for tok in tokens {
            let encodedTokens[] = Encoder::encode(tok);
        }
        return join(self::DELIMITER, encodedTokens);
    }
    /**
     * encode command tokens and compose a string
     */
    static public function decode(string line) -> array {
        var tokens = explode("\t", line);
        var decodedTokens = [];
        var tok;
        for tok in tokens {
            let decodedTokens[] = Encoder::decode(tok);
        }
        return decodedTokens;
    }
    /**
     * compose a command
     */
    static public function compose(array tokens) -> string {
        var ret = self::encode(tokens);
        return ret . "\n";
    }
}