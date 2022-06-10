package main

import (
	"fmt"
	"strings"
)

type BuiltinType struct {
	name       string
	mangleChar rune
}

func (typ *BuiltinType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.mangleChar)
}

type ArrayType struct {
	Len  int
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

var TypeBoolean = &BuiltinType{"bool", 'b'}
var TypeInt8 = &BuiltinType{"int8_t", 'm'}
var TypeInt16 = &BuiltinType{"int16_t", 's'}
var TypeInt32 = &BuiltinType{"int32_t", 'i'}
var TypeInt64 = &BuiltinType{"int64_t", 'l'}
var TypeFloat32 = &BuiltinType{"float", 'f'}
var TypeFloat64 = &BuiltinType{"double", 'd'}
var TypeString = &BuiltinType{"string", 'S'}
var TypeChar = &BuiltinType{"char", 'c'}
var TypeVoid = &BuiltinType{"void", 'v'}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn", '-'}

// this type is the internal representation when no type has been
// specified. It is not a type by its own.
var TypeUnspecified = &BuiltinType{"<unspecified>", ','}

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
	Parent: nil,
	Types: map[string]Type{
		"bool":     TypeBoolean,
		"int8":     TypeInt8,
		"int16":    TypeInt16,
		"int32":    TypeInt32,
		"int64":    TypeInt64,
		"float32":  TypeFloat32,
		"float64":  TypeFloat64,
		"string":   TypeString,
		"void":     TypeVoid,
		"noreturn": TypeNoReturn,
	},
	// these are builtin procedures, therefore their Impl is nil
	Procedures: map[string]([]*TcProcDef){
		"printf": []*TcProcDef{BuiltinPrintf},
	},
	Variables: map[string]TcSymbol{},
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

	// lessOpDef := &TcProcDef{
	// 	Name:               "<",
	// 	Args:               []TcSymbol{TcSymbol{Name: "", Kind: SkProcArg, Typ: TypeUnspecified}},
	// 	ResultType:         TypeBoolean,
	// 	generateAsOperator: true,
	// 	builtinName:        "<",
	// }

	// builtinScope.Procedures["<"] = lessOpDef
}
