import org.eclipse.ceylon.model.typechecker.model {
    Declaration,
    FunctionOrValue,
    Function,
    Value,
    ParameterList,
    Parameter,
    Scope,
    Unit
}

class ParameterListData([ParameterData*] parameters) {
    shared ParameterList toParameterList(Module mod, Unit unit,
            Declaration container) {
        value ret = ParameterList();

        for (parameter in parameters) {
            ret.parameters.add(parameter.toParameter(mod, unit, container));
        }

        return ret;
    }
}

class ParameterType {
    shared new normal {}
    shared new zeroOrMore {}
    shared new oneOrMore {}
}

class ParameterData(name, hidden, defaulted, parameterType, parameters,
        type, annotations) {
    shared String name;
    shared Boolean hidden;
    shared Boolean defaulted;
    shared ParameterType parameterType;
    shared [ParameterListData*] parameters;
    shared TypeData type;
    shared AnnotationData annotations;

    shared Parameter toParameter(Module mod, Unit unit, Declaration container) {
        value ret = Parameter();

        ret.name = name;
        ret.declaration = container;
        ret.hidden = hidden;
        ret.defaulted = defaulted;
        ret.sequenced = parameterType != ParameterType.normal;
        ret.atLeastOne = parameterType == ParameterType.oneOrMore;

        FunctionOrValue f;

        if (nonempty parameters) {
            f = Function();
            assert(is Function f);
            applyParametersToFunction(mod, unit, f, parameters);
        } else {
            f = Value();
        }

        ret.model = f;
        f.initializerParameter = ret;
        f.name = name;
        f.unit = unit;

        if (is Scope container) {
            f.container = container;
        }

        f.type = type.toType(mod, unit, f);
        annotations.apply(f);

        return ret;
    }
}
