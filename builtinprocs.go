package main

type BuiltinType struct {
	name string
}

type ArrayType struct {
	Len  int
	Elem Type
}

func (typ *BuiltinType) Source() string {
	// should this panic?
	return ""
}

func (typ *BuiltinType) Name() string {
	return typ.name
}

func (typ ArrayType) Source() string {
	// should this panic?
	return ""
}

func (typ ArrayType) Name() string {
	panic("not implemented")
}

// **** Constants ****

// These type names are by no means final, there are just to get
// something working.
var TypeBoolean = &BuiltinType{"bool"}
var TypeInt = &BuiltinType{"int"}
var TypeFloat = &BuiltinType{"float"}
var TypeString = &BuiltinType{"string"}
var TypeChar = &BuiltinType{"char"}
var TypeVoid = &BuiltinType{"void"}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn"}

// this type is the internal representation when no type has been
// specified. It is not a type by its own.
var TypeUnspecified = &BuiltinType{"<unspecified>"}

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
