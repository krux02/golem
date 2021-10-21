package main

// Printf is literally the only use case for real varargs that I
// know. Therefore the implementation for varargs will be strictly
// tied to printf for now. A general concept for varargs will be
// specified out as soon as it becomes necessary.
var BuiltinPrintf *TcProcDef = &TcProcDef{
	Name: "printf",
	// TODO support argument list
	printfargs: true,
	ResultType: TypeVoid,
}

var builtinScope Scope = &ScopeImpl{
	Parent: nil,
	Types: map[string]Type{
		"int":      TypeInt,
		"float":    TypeFloat,
		"string":   TypeString,
		"void":     TypeVoid,
		"noreturn": TypeNoReturn,
	},
	// these are builtin procedures, therefore their Impl is nil
	Procedures: map[string]*TcProcDef{
		"printf": BuiltinPrintf,
	},
}

func registerBuiltin(name string, args []Type, result Type) {
	procDef := &TcProcDef{
		Name:       name,
		Args:       make([]TcSymbol, len(args)),
		ResultType: result,
	}
	for i, arg := range args {
		procDef.Args[i].Typ = arg
	}
	builtinScope.Procedures[name] = procDef
}

func init() {
	// currently overloading isn't supported
	registerBuiltin("+", []Type{TypeInt, TypeInt}, TypeInt)
	registerBuiltin("-", []Type{TypeInt, TypeInt}, TypeInt)
	registerBuiltin("*", []Type{TypeInt, TypeInt}, TypeInt)

	// NOTE, there is something written about weirdo in ideos.org
	registerBuiltin("/", []Type{TypeFloat, TypeFloat}, TypeFloat)

	// this has no structure, just made to make the example compile
	registerBuiltin("==", []Type{TypeChar, TypeChar}, TypeBoolean)
	registerBuiltin("<", []Type{TypeInt, TypeInt}, TypeBoolean)
	registerBuiltin(">", []Type{TypeInt, TypeInt}, TypeBoolean)

	registerBuiltin("+=", []Type{TypeInt, TypeInt}, TypeVoid)
	registerBuiltin("-=", []Type{TypeInt, TypeInt}, TypeVoid)
	registerBuiltin("=", []Type{TypeInt, TypeInt}, TypeVoid)
}
