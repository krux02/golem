package main

import (
	"fmt"
	"strings"
)

type BuiltinType struct {
	Name         string
	InternalName string // name for C code generation
	MangleChar   rune
}

type TypeGroup struct {
	Name  string
	Items []Type
}

type EnumType struct {
	Impl *TcEnumDef

	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this type as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

type StructType struct {
	Impl *TcStructDef

	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this type as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

type ArrayType struct {
	Len  int64
	Elem Type

	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this type as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

var _ Type = &BuiltinType{}
var _ Type = &TypeGroup{}
var _ Type = &EnumType{}
var _ Type = &StructType{}
var _ Type = &ArrayType{}

func (typ *BuiltinType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.MangleChar)
}

func (typ *TypeGroup) ManglePrint(builder *strings.Builder) {
	panic("not a resolved type")
}

func (typ *TypeGroup) PrettyPrint(builder *AstPrettyPrinter) {
	if typ.Name != "" {
		builder.WriteString(typ.Name)
		return
	}
	for i, typ := range typ.Items {
		if i != 0 {
			builder.WriteString(" | ")
		}
		typ.PrettyPrint(builder)
	}
}

type EnumSetType struct {
	Elem *EnumType
}

func (typ *ArrayType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('A')
	builder.WriteString(fmt.Sprintf("%d", typ.Len))
	typ.Elem.ManglePrint(builder)
}

func (typ *StructType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('S')
	builder.WriteString(typ.Impl.Name)
	builder.WriteRune('_')
}

func (typ *EnumType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('E')
	builder.WriteString(typ.Impl.Name)
	builder.WriteRune('_')
}

func (typ *EnumSetType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('X')
	typ.Elem.ManglePrint(builder)
	builder.WriteRune('_')
}

func (typ *TcGenericTypeParam) ManglePrint(builder *strings.Builder) {
	panic("illegal not a concrete type")
}

func (typ *TypeGroup) GetSource() string {
	panic("type group does not have source")
}

// **** Constants ****
// These type names are by no means final, there are just to get
// something working.

var TypeBoolean = &BuiltinType{"bool", "bool", 'b'}
var TypeInt8 = &BuiltinType{"i8", "int8_t", 'm'}
var TypeInt16 = &BuiltinType{"i16", "int16_t", 's'}
var TypeInt32 = &BuiltinType{"i32", "int32_t", 'i'}
var TypeInt64 = &BuiltinType{"i64", "int64_t", 'l'}
var TypeFloat32 = &BuiltinType{"f32", "float", 'f'}
var TypeFloat64 = &BuiltinType{"f64", "double", 'd'}
var TypeString = &BuiltinType{"string", "string", 'S'}
var TypeChar = &BuiltinType{"char", "char", 'c'}
var TypeVoid = &BuiltinType{"void", "void", 'v'}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn", "void", '-'}

// this type is the internal representation when no type has been specified. It
// is not a type by its own.
//
// TODO, maybe this should be a different Type that isn't ~BuiltinType~,
// ~BuiltinType~ is used only for concrete types that actually can be
// instantiated. It behaves very differently is is always the exception to be checked for.
var TypeUnspecified = &BuiltinType{"???", "<unspecified>", ','}

// this type is the internal representation when the type checker fails to
// resolve the type. Expressions with this type cannot be further processed in
// code generation.
var TypeError = &BuiltinType{"???", "<error>", ','}

var TypeAnyInt = &TypeGroup{Name: "AnyInt", Items: []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64}}
var TypeAnyFloat = &TypeGroup{Name: "AnyFloat", Items: []Type{TypeFloat32, TypeFloat64}}
var TypeAnyNumber = &TypeGroup{Name: "AnyNumber", Items: []Type{TypeFloat32, TypeFloat64, TypeInt8, TypeInt16, TypeInt32, TypeInt64}}

// types for C wrappers
var TypeCString = &BuiltinType{"cstring", "char const*", 'x'}

func (typ *BuiltinType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	if typ == TypeUnspecified {
		tc.ReportErrorf(context, "variable definitions statements must have at least one, a type or a value expression")
	} else if typ == TypeNoReturn {
		tc.ReportErrorf(context, "a default value of no retrun does not exist")
	} else if typ == TypeFloat32 || typ == TypeFloat64 {
		return FloatLit{Type: typ}
	} else if typ == TypeChar {
		return CharLit{}
	} else if typ == TypeInt8 || typ == TypeInt16 || typ == TypeInt32 || typ == TypeInt64 {
		return IntLit{Type: typ}
	} else if typ == TypeBoolean {
		panic("not implemented bool default value")
	} else if typ == TypeVoid {
		panic("not implemented void default value")
	} else {
		panic(fmt.Errorf("not implemented %+v", typ))
	}
	return nil
}

func (typ *TypeGroup) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	panic(fmt.Errorf("no default value for abstract type group: %s", typ.Name))
}

func (typ *ArrayType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return TcArrayLit{ElemType: typ.Elem}
}

func (typ *StructType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return TcStructLit{Type: typ}
}

func (typ *EnumType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return typ.Impl.Values[0]
}

func (typ *EnumSetType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return TcEnumSetLit{ElemType: typ.Elem}
}

func (typ *TcGenericTypeParam) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	panic("not implemented")
}

var builtinScope Scope = &ScopeImpl{
	Parent:     nil,
	Types:      make(map[string]Type),
	Procedures: make(map[string][]ProcSignature),
	Variables:  make(map[string]TcSymbol),
}

func registerBuiltinType(typ *BuiltinType) {
	_, ok := builtinScope.Types[typ.Name]
	if ok {
		panic("internal error double definition of builtin type")
	}
	builtinScope.Types[typ.Name] = typ
}

func registerTypeGroup(typ *TypeGroup) {
	builtinScope.Types[typ.Name] = typ
}

func registerGenericBuiltin(name, prefix, infix, postfix string, genericParams []Type, args []Type, result Type, checker PostResolveValidator) {
	signature := ProcSignature{
		GenericParams: genericParams,
		Params:        make([]TcSymbol, len(args)),
		ResultType:    result,
		Varargs:       checker != nil,
		Validator:     checker,
	}
	procDef := &TcProcDef{
		Name:    name,
		Prefix:  prefix,
		Infix:   infix,
		Postfix: postfix,
	}
	for i, arg := range args {
		signature.Params[i].Type = arg
		signature.Params[i].Kind = SkProcArg
	}
	signature.Impl = procDef
	procDef.Signature = signature
	builtinScope.Procedures[name] = append(builtinScope.Procedures[name], signature)
}

func registerBuiltin(name, prefix, infix, postfix string, args []Type, result Type) {
	registerGenericBuiltin(name, prefix, infix, postfix, []Type{}, args, result, nil)
}

type PostResolveValidator func(tc *TypeChecker, scope Scope, call TcCall) TcCall

func registerBuiltinVararg(name, prefix, infix, postfix string, args []Type, result Type, checker PostResolveValidator) {
	// TODO actually do something with PostResolveValidator
	registerGenericBuiltin(name, prefix, infix, postfix, []Type{}, args, result, checker)
}

func registerConstant(name string, typ Type) {
	// TODO this is wrong, a constant isn't a variable
	var ident Ident
	ident.Source = name
	_ = builtinScope.NewSymbol(nil, ident, SkConst, typ)
}

func ValidatePrintfCall(tc *TypeChecker, scope Scope, call TcCall) (result TcCall) {

	formatExpr := call.Args[0]
	// format string must be a string literal
	formatStrLit, ok := formatExpr.(StrLit)
	if !ok {
		// TODO this needs a func Test
		tc.ReportErrorf(formatExpr, "format string must be a string literal")
		return
	}
	formatStr := formatStrLit.Value
	formatStrC := &strings.Builder{}
	for j, i := 0, 1; j < len(formatStr); j++ {
		c1 := formatStr[j]
		formatStrC.WriteByte(c1)
		if c1 != '%' {
			continue
		}
		j++
		if j == len(formatStr) {
			tc.ReportErrorf(formatExpr, "incomplete format expr at end of format string")
			break
		}
		c2 := formatStr[j]
		var typeExpectation Type = TypeUnspecified
		switch c2 {
		case '%':
			formatStrC.WriteRune('%')
			continue
		case 's':
			typeExpectation = TypeString
		case 'd':
			typeExpectation = TypeAnyInt
		case 'f':
			typeExpectation = TypeAnyFloat
		case 'v':
			typeExpectation = TypeUnspecified
		default:
			tc.ReportErrorf(formatExpr, "invalid format expr %%%c in %s", c2, AstFormat(formatExpr))
			return
		}
		if i == len(call.Args) {
			tc.ReportErrorf(formatExpr, "not enough arguments for %s", AstFormat(formatExpr))
			return
		}

		argType := call.Args[i].GetType()
		tc.ExpectType(call.Args[i], argType, typeExpectation)
		switch argType {
		case TypeInt8:
			formatStrC.WriteString("hhd")
		case TypeInt16:
			formatStrC.WriteString("hd")
		case TypeInt32:
			formatStrC.WriteString("d")
		case TypeInt64:
			formatStrC.WriteString("ld")
		case TypeCString:
			formatStrC.WriteString("s")
		case TypeString:
			formatStrC.WriteString("*s")
		case TypeFloat32:
			formatStrC.WriteString("f")
		case TypeFloat64:
			formatStrC.WriteString("f")
		case TypeError:
			formatStrC.WriteString("<error>")
		}
		i++
	}

	result = call
	result.Args[0] = CStrLit{Source: formatStrLit.Source, Value: formatStrC.String()}
	return result
}

func init() {
	arrayTypeMap = make(map[ArrayTypeMapKey]*ArrayType)
	enumSetTypeMap = make(map[*EnumType]*EnumSetType)

	// Printf is literally the only use case for real varargs that I actually see as
	// practical. Therefore the implementation for varargs will be strictly tied to
	// printf for now. A general concept for varargs will be specified out as soon
	// as it becomes necessary, but right now it is not planned.
	registerBuiltinVararg("printf", "printf(", ", ", ")", []Type{TypeString}, TypeVoid, ValidatePrintfCall)

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
	registerTypeGroup(TypeAnyNumber)

	// this has no structure, just made to make the example compile
	for _, typ := range []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64, TypeFloat32, TypeFloat64} {
		registerBuiltin("+", "(", "+", ")", []Type{typ, typ}, typ)
		registerBuiltin("-", "(", "-", ")", []Type{typ, typ}, typ)
		registerBuiltin("*", "(", "*", ")", []Type{typ, typ}, typ)
		registerBuiltin("/", "(", "/", ")", []Type{typ, typ}, typ)

		registerBuiltin("<", "(", "<", ")", []Type{typ, typ}, TypeBoolean)
		registerBuiltin("<=", "(", "<=", ")", []Type{typ, typ}, TypeBoolean)
		registerBuiltin(">", "(", ">", ")", []Type{typ, typ}, TypeBoolean)
		registerBuiltin(">=", "(", ">=", ")", []Type{typ, typ}, TypeBoolean)

		registerBuiltin("+=", "(", "+=", ")", []Type{typ, typ}, TypeVoid)
		registerBuiltin("-=", "(", "-=", ")", []Type{typ, typ}, TypeVoid)
		registerBuiltin("*=", "(", "*=", ")", []Type{typ, typ}, TypeVoid)
		registerBuiltin("/=", "(", "/=", ")", []Type{typ, typ}, TypeVoid)

		registerBuiltin("i8", "(int8_t)(", "", ")", []Type{typ}, TypeInt8)
		registerBuiltin("i16", "(int16_t)(", "", ")", []Type{typ}, TypeInt16)
		registerBuiltin("i32", "(int32_t)(", "", ")", []Type{typ}, TypeInt32)
		registerBuiltin("i64", "(int64_t)(", "", ")", []Type{typ}, TypeInt64)

		registerBuiltin("f32", "(float)(", "", ")", []Type{typ}, TypeFloat32)
		registerBuiltin("f64", "(double)(", "", ")", []Type{typ}, TypeFloat64)
	}

	{
		// TODO: has no line information
		T := &TcGenericTypeParam{Name: "T", Constraint: TypeUnspecified}
		registerGenericBuiltin("=", "(", "=", ")", []Type{T}, []Type{T, T}, TypeVoid, nil)
	}

	{
		// TODO: has no line information
		T := &TcGenericTypeParam{Name: "T", Constraint: TypeUnspecified}
		registerGenericBuiltin("==", "(", "==", ")", []Type{T}, []Type{T, T}, TypeBoolean, nil)
	}

	{
		// TODO: has no line information
		T := &TcGenericTypeParam{Name: "T", Constraint: TypeUnspecified}
		registerGenericBuiltin("!=", "(", "!=", ")", []Type{T}, []Type{T, T}, TypeBoolean, nil)
	}

	registerBuiltin("and", "(", "&&", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean)
	registerBuiltin("or", "(", "||", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean)

	registerBuiltin("assert", "assert(", "", ")", []Type{TypeBoolean}, TypeVoid)

	// TODO this one needs a generic solution
	// T := &TcGenericTypeParam{Source: "T"}
	// registerBuiltin("=", "(", "=", ")", []Type{T, T}, TypeVoid)

	registerBuiltin("[", "", ".arr[", "]", []Type{GetArrayType(TypeInt64, 7), TypeInt64}, TypeInt64)
	registerBuiltin("[", "", ".arr[", "]", []Type{GetArrayType(TypeInt32, 20), TypeInt64}, TypeInt32)
	registerBuiltin("[", "", ".arr[", "]", []Type{GetArrayType(GetArrayType(TypeInt32, 20), 3), TypeInt64}, GetArrayType(TypeInt32, 20))

	registerConstant("true", TypeBoolean)
	registerConstant("false", TypeBoolean)
}
