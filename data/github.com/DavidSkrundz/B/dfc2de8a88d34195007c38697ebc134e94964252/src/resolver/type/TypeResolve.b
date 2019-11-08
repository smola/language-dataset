var types: Type**;

func registerType(type: Type*) {
	Buffer_append((Void***)&types, (Void*)type);
};

func findTypeByName(name: String*): Type* {
	var symbol = findSymbol(name);
	if (symbol != NULL && symbol->isType) {
		return symbol->type;
	};
	return NULL;
};

func findTypeByBase(base: Type*): Type* {
	var i = 0;
	while (i < Buffer_getCount((Void**)types)) {
		if (types[i]->kind == .Pointer) {
			var pointer = (TypePointer*)types[i]->type;
			if (pointer->base == base) { return types[i]; };
		};
		i = i + 1;
	};
	return NULL;
};

func resolveTypePointer(base: Type*): Type* {
	var type = findTypeByBase(base);
	if (type != NULL) { return type; };
	type = newType();
	type->kind = .Pointer;
	type->type = (Void*)TypePointer_init(base);
	registerType(type);
	return type;
};

func resolveTypeIdentifier(name: String*): Type* {
	var type = findTypeByName(name);
	if (type != NULL) { return type; };
	var i = 0;
	while (i < Buffer_getCount((Void**)_declarations)) {
		if (_declarations[i]->name->string == name) {
			var chainStash = stashContextChainToRoot();
			var contextStash = stashContextToRoot();
			resolveDeclaration(_declarations[i]);
			restoreContextFromRoot(contextStash);
			restoreContextChainFromRoot(chainStash);
		};
		i = i + 1;
	};
	return findTypeByName(name);
};

func isPointer(type: Type*): Bool {
	return type->kind == .Pointer;
};

func getPointerBase(type: Type*): Type* {
	if (type->kind == .Identifier) {
		ProgrammingError("called getPointerBase on a .Identifier");
	} else if (type->kind == .Pointer) {
		var pointer = (TypePointer*)type->type;
		return pointer->base;
	} else if (type->kind == .Function) {
		ProgrammingError("called getPointerBase on a .Function");
	} else if (type->kind == .Invalid) {
		ProgrammingError("called getPointerBase on a .Invalid");
	} else {
		ProgrammingError("called getPointerBase on a .Invalid");
	};;;;
	return NULL;
};

func createTypeIdentifier(name: Token*): Type* {
	if (findTypeByName(name->string) != NULL) {
		ResolverError(name->pos, "duplicate definition of type '", name->string->string, "'");
	};
	var type = newType();
	type->kind = .Identifier;
	type->type = (Void*)TypeIdentifier_init(name->string);
	registerType(type);
	return type;
};

func resolveTypeFunction(returnType: Type*, argumentTypes: Type**, isVariadic: Bool): Type* {
	var i = 0;
	while (i < Buffer_getCount((Void**)types)) {
		var type = types[i];
		if (type->kind == .Function) {
			var funcType = (TypeFunction*)type->type;
			if (funcType->isVariadic == isVariadic) {
				if (funcType->returnType == returnType) {
					if (Buffer_getCount((Void**)funcType->argumentTypes) == Buffer_getCount((Void**)argumentTypes)) {
						var isMatch = true;
						var j = 0;
						while (j < Buffer_getCount((Void**)funcType->argumentTypes)) {
							if (funcType->argumentTypes[j] != argumentTypes[j]) {
								isMatch = false;
							};
							j = j + 1;
						};
						if (isMatch) { return type; };
					};
				};
			};
		};
		i = i + 1;
	};
	return NULL;
};

func createTypeFunction(returnType: Type*, argumentTypes: Type**, isVariadic: Bool): Type* {
	if (resolveTypeFunction(returnType, argumentTypes, isVariadic) != NULL) {
		ProgrammingError("attempting to create duplicate function type");
	};
	var funcType = newTypeFunction();
	funcType->returnType = returnType;
	funcType->argumentTypes = argumentTypes;
	funcType->isVariadic = isVariadic;
	var type = newType();
	type->kind = .Function;
	type->type = (Void*)funcType;
 	registerType(type);
 	return type;
 };
