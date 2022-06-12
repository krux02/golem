package main

import (
	"fmt"
	"strings"
)

type BuiltinType struct {
	name         string
	internalName string // name for C code generation
	mangleChar   rune
}

var _ Type = &BuiltinType{}

type TypeGroup struct {
	name  string
	items []Type
}

var _ Type = &TypeGroup{}

func (typ *BuiltinType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.mangleChar)
}

func (typ *TypeGroup) ManglePrint(builder *strings.Builder) {
	panic("not a resolved type")
}

func (typ *TypeGroup) Source() string {
	panic("type group does not have source")
}

func (typ TypeGroup) prettyPrint(builder *AstPrettyPrinter) {
	for i, typ := range typ.items {
		if i != 0 {
			builder.WriteString(" | ")
		}
		typ.prettyPrint(builder)
	}
}

func (typ TypeGroup) typenode() {}

type ArrayType struct {
	Len  int64
	Elem Type
}

func (typ *ArrayType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('A')
	builder.WriteString(fmt.Sprintf("A%d", typ.Len))
	typ.Elem.ManglePrint(builder)
}

func (typ *TcStructDef) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('S')
	builder.WriteString(typ.Name)
	builder.WriteRune('_')
}

func (typ *BuiltinType) Source() string {
	// should this panic?
	return ""
}

func (typ *BuiltinType) typenode() {}

func (typ *ArrayType) Source() string {
	// should this panic?
	return ""
}

func (typ *ArrayType) typenode() {}

// **** Constants ****

// These type names are by no means final, there are just to get
// something working.

var TypeBoolean = &BuiltinType{"bool", "bool", 'b'}
var TypeInt8 = &BuiltinType{"int8", "int8_t", 'm'}
var TypeInt16 = &BuiltinType{"int16", "int16_t", 's'}
var TypeInt32 = &BuiltinType{"int32", "int32_t", 'i'}
var TypeInt64 = &BuiltinType{"int64", "int64_t", 'l'}
var TypeFloat32 = &BuiltinType{"float32", "float", 'f'}
var TypeFloat64 = &BuiltinType{"float64", "double", 'd'}
var TypeString = &BuiltinType{"string", "string", 'S'}
var TypeChar = &BuiltinType{"char", "char", 'c'}
var TypeVoid = &BuiltinType{"void", "void", 'v'}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn", "void", '-'}

// this type is the internal representation when no type has been
// specified. It is not a type by its own.
var TypeUnspecified = &BuiltinType{"???", "<unspecified>", ','}

var TypeAnyInt = &TypeGroup{name: "AnyInt", items: []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64}}
var TypeAnyFloat = &TypeGroup{name: "AnyFloat", items: []Type{TypeFloat32, TypeFloat64}}

// Printf is literally the only use case for real varargs that I
// know. Therefore the implementation for varargs will be strictly
// tied to printf for now. A general concept for varargs will be
// specified out as soon as it becomes necessary.
var BuiltinPrintf *TcProcDef = &TcProcDef{
	Name:        "printf",
	MangledName: "printf",
	// Support for type checking arguments for printf is currently a language
	// feature.
	printfargs: true,
	ResultType: TypeVoid,
}

var builtinScope Scope = &ScopeImpl{
	Parent:     nil,
	Types:      make(map[string]Type),
	Procedures: make(map[string]([]*TcProcDef)),
	Variables:  make(map[string]TcSymbol),
}

func registerBuiltinType(typ *BuiltinType) {
	builtinScope.Types[typ.name] = typ
}

func registerTypeGroup(typ *TypeGroup) {
	builtinScope.Types[typ.name] = typ
}

func registerBuiltin(name, builtinName string, isOperator bool, args []Type, result Type) {
	procDef := &TcProcDef{
		Name:               name,
		Args:               make([]TcSymbol, len(args)),
		ResultType:         result,
		generateAsOperator: isOperator,
		MangledName:        builtinName,
	}
	for i, arg := range args {
		procDef.Args[i].Typ = arg
		procDef.Args[i].Kind = SkProcArg
	}
	builtinScope.Procedures[name] = append(builtinScope.Procedures[name], procDef)
}

func registerConstant(name string, typ Type) {
	// TODO this is wrong, a constant isn't a variable
	_ = builtinScope.NewSymbol(name, SkConst, typ)
}

func init() {
	registerBuiltinType(TypeBoolean)
	registerBuiltinType(TypeInt8)
	registerBuiltinType(TypeInt16)
	registerBuiltinType(TypeInt32)
	registerBuiltinType(TypeInt64)
	registerBuiltinType(TypeFloat32)
	registerBuiltinType(TypeFloat64)
	registerBuiltinType(TypeString)
	registerBuiltinType(TypeVoid)
	registerBuiltinType(TypeNoReturn)

	registerTypeGroup(TypeAnyFloat)
	registerTypeGroup(TypeAnyInt)

	builtinScope.Procedures["printf"] = append(builtinScope.Procedures["printf"], BuiltinPrintf)
	// this has no structure, just made to make the example compile
	for _, typ := range []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64, TypeFloat32, TypeFloat64} {
		registerBuiltin("+", "+", true, []Type{typ, typ}, typ)
		registerBuiltin("-", "-", true, []Type{typ, typ}, typ)
		registerBuiltin("*", "*", true, []Type{typ, typ}, typ)
		registerBuiltin("/", "/", true, []Type{typ, typ}, typ)

		registerBuiltin("==", "==", true, []Type{typ, typ}, TypeBoolean)
		registerBuiltin("<", "<", true, []Type{typ, typ}, TypeBoolean)
		registerBuiltin("<=", "<=", true, []Type{typ, typ}, TypeBoolean)
		registerBuiltin(">", ">", true, []Type{typ, typ}, TypeBoolean)
		registerBuiltin(">=", ">=", true, []Type{typ, typ}, TypeBoolean)
		registerBuiltin("!=", "!=", true, []Type{typ, typ}, TypeBoolean)

		registerBuiltin("+=", "+=", true, []Type{typ, typ}, TypeVoid)
		registerBuiltin("-=", "-=", true, []Type{typ, typ}, TypeVoid)
		registerBuiltin("*=", "*=", true, []Type{typ, typ}, TypeVoid)
		registerBuiltin("/=", "/=", true, []Type{typ, typ}, TypeVoid)
		registerBuiltin("=", "=", true, []Type{typ, typ}, TypeVoid)

		registerBuiltin("int8", "(int8_t)", false, []Type{typ}, TypeInt8)
		registerBuiltin("int16", "(int16_t)", false, []Type{typ}, TypeInt16)
		registerBuiltin("int32", "(int32_t)", false, []Type{typ}, TypeInt32)
		registerBuiltin("int64", "(int64_t)", false, []Type{typ}, TypeInt64)

		registerBuiltin("float32", "(float)", false, []Type{typ}, TypeFloat32)
		registerBuiltin("float64", "(double)", false, []Type{typ}, TypeFloat64)
	}

	registerBuiltin("=", "=", true, []Type{TypeString, TypeString}, TypeVoid)
	registerBuiltin("==", "==", true, []Type{TypeChar, TypeChar}, TypeBoolean)
	registerBuiltin("!=", "!=", true, []Type{TypeChar, TypeChar}, TypeBoolean)
	registerBuiltin("==", "==", true, []Type{TypeBoolean, TypeBoolean}, TypeBoolean)
	registerBuiltin("!=", "!=", true, []Type{TypeBoolean, TypeBoolean}, TypeBoolean)

	registerBuiltin("and", "&&", true, []Type{TypeBoolean, TypeBoolean}, TypeBoolean)
	registerBuiltin("or", "||", true, []Type{TypeBoolean, TypeBoolean}, TypeBoolean)

	registerBuiltin("assert", "assert", false, []Type{TypeBoolean}, TypeVoid)

	registerConstant("true", TypeBoolean)
	registerConstant("false", TypeBoolean)
}
