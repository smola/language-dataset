import ceylon.ast.core {
    IntegerLiteral,
    PrimaryType,
    SequentialType
}
import ceylon.ast.redhat {
    RedHatTransformer,
    sequentialTypeToCeylon,
    parseSequentialType
}
import org.eclipse.ceylon.compiler.typechecker.tree {
    Tree {
        JSequenceType=SequenceType
    }
}

shared object sequentialType satisfies ConcreteTest<SequentialType,JSequenceType> {
    
    String->SequentialType construct(String->PrimaryType elem, <String->IntegerLiteral>? length = null)
            => "``elem.key``[`` length?.key else "" ``]" -> SequentialType(elem.item, length?.item);
    
    shared String->SequentialType stringSequentialType = construct(baseType.stringType);
    shared String->SequentialType iterableOfStringSequentialType = construct(baseType.iterableOfStringType);
    shared String->SequentialType byteSequentialType = construct(baseType.booleanType, integerLiteral._8IntegerLiteral);
    
    parse = parseSequentialType;
    fromCeylon = RedHatTransformer.transformSequentialType;
    toCeylon = sequentialTypeToCeylon;
    codes = [stringSequentialType, iterableOfStringSequentialType, byteSequentialType];
}
