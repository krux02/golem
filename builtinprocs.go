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

type ErrorType struct{}

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

// the type that represets types as arguments. Example:
//
//	sizeof(type int)
//	max(type int)
//	min(type int)
//	alignof(type int)
type TypeType struct {
	Type Type
}

var _ Type = &BuiltinType{}
var _ Type = &TypeGroup{}
var _ Type = &EnumType{}
var _ Type = &EnumSetType{}
var _ Type = &StructType{}
var _ Type = &ArrayType{}
var _ Type = &IntLit{}
var _ Type = &ErrorType{}
var _ Type = &TypeType{}
var _ Type = &PtrType{}
var _ Type = &OpenGenericType{}
var _ Type = &GenericTypeSymbol{}

func (typ *BuiltinType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.MangleChar)
}

func (typ *ErrorType) ManglePrint(builder *strings.Builder) {
	panic("internal error, an error type should not be here")
}

func (typ *TypeGroup) ManglePrint(builder *strings.Builder) {
	panic("not a resolved type")
}

func (typ *TypeType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteNode(typ.Type)
}

type EnumSetType struct {
	Elem *EnumType
}

type PtrType struct {
	Target Type
}

type OpenGenericType struct {
	// incomplete type that still needs Generic symbols to be substituted becfore it can be used.
	Type        Type
	OpenSymbols []*GenericTypeSymbol
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
func (typ *PtrType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('P')
	typ.Target.ManglePrint(builder)
	builder.WriteRune('_')
}

func (typ *IntLit) ManglePrint(builder *strings.Builder) {
	fmt.Fprintf(builder, "%d_", typ.Value)
}

func (typ *GenericTypeSymbol) ManglePrint(builder *strings.Builder) {
	panic("illegal not a concrete type")
}

func (typ *OpenGenericType) ManglePrint(builder *strings.Builder) {
	panic("illegal not a concrete type")
}

func (typ *TypeGroup) GetSource() string {
	panic("type group does not have source")
}

func (typ *TypeType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('T')
}

// **** Constants ****
// These type names are by no means final, there are just to get
// something working.

var TypeBoolean = &BuiltinType{"bool", "bool", 'b'}
var TypeInt8 = &BuiltinType{"i8", "int8_t", 'm'}
var TypeInt16 = &BuiltinType{"i16", "int16_t", 's'}
var TypeInt32 = &BuiltinType{"i32", "int32_t", 'i'}
var TypeInt64 = &BuiltinType{"i64", "int64_t", 'l'}
var TypeUInt8 = &BuiltinType{"u8", "uint8_t", 'M'}
var TypeUInt16 = &BuiltinType{"u16", "uint16_t", 'S'}
var TypeUInt32 = &BuiltinType{"u32", "uint32_t", 'I'}
var TypeUInt64 = &BuiltinType{"u64", "uint64_t", 'L'}
var TypeFloat32 = &BuiltinType{"f32", "float", 'f'}
var TypeFloat64 = &BuiltinType{"f64", "double", 'd'}
var TypeString = &BuiltinType{"string", "string", 'R'}
var TypeChar = &BuiltinType{"char", "char", 'c'}
var TypeVoid = &BuiltinType{"void", "void", 'v'}
var TypeNilPtr = &BuiltinType{"nilptr", "void*", 'n'}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn", "void", '-'}

// this type is the internal representation when no type has been specified. It
// is not a type by its own.
//
// TODO, maybe this should be a different Type that isn't ~BuiltinType~,
// ~BuiltinType~ is used only for concrete types that actually can be
// instantiated. It behaves very differently is is always the exception to be checked for.
var TypeUnspecified = &BuiltinType{"?unspecified?", "<unspecified>", ','}

// this type is the internal representation when the type checker fails to
// resolve the type. Expressions with this type cannot be further processed in
// code generation. This should probably be a different type, not BuiltinType
var TypeError = &ErrorType{}

var TypeAnyUInt = &TypeGroup{Name: "AnyUInt", Items: []Type{TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64}}
var TypeAnyInt = &TypeGroup{Name: "AnyInt", Items: []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64}}
var TypeAnyFloat = &TypeGroup{Name: "AnyFloat", Items: []Type{TypeFloat32, TypeFloat64}}
var TypeAnyNumber = &TypeGroup{Name: "AnyNumber", Items: []Type{
	TypeFloat32, TypeFloat64,
	TypeInt8, TypeInt16, TypeInt32, TypeInt64,
	TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64,
}}

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
		return &IntLit{Type: typ, Value: 0} // TODO no Source set
	} else if typ == TypeBoolean {
		// TODO this is weird
		tc.LookUpLetSym(builtinScope, Ident{Source: "false"}, TypeBoolean)
	} else if typ == TypeVoid {
		panic("not implemented void default value")
	} else {
		panic(fmt.Errorf("not implemented %+v", typ))
	}
	return nil
}

func (typ *ErrorType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
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

func (typ *PtrType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return NilLit{Type: typ}
}

func (typ *EnumSetType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return TcEnumSetLit{ElemType: typ.Elem}
}

func (typ *IntLit) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return typ // and int lit is its own default value
}

func (typ *TypeType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	panic("no default value for types")
}

func (typ *GenericTypeSymbol) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	panic("not implemented")
}

func (typ *OpenGenericType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
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

func extractGenericTypeSymbols(typ Type) (result []*GenericTypeSymbol) {
	switch typ := typ.(type) {
	case *BuiltinType:
		return nil
	case *TypeGroup:
		panic("internal error")
	case *EnumType:
		return nil
	case *StructType:
		for _, field := range typ.Impl.Fields {
			result = append(result, extractGenericTypeSymbols(field.Type)...)
		}
		return result
	case *ArrayType:
		return extractGenericTypeSymbols(typ.Elem)
	case *IntLit:
		return nil
	case *ErrorType:
		panic("internal error")
	case *TypeType:
		return extractGenericTypeSymbols(typ.Type)
	case *PtrType:
		return extractGenericTypeSymbols(typ.Target)
	case *OpenGenericType:
		panic("interanl error, OpenGenericType should not be created around another OpenGenericType")
	case *GenericTypeSymbol:
		return []*GenericTypeSymbol{typ}
	}
	panic("should be dead code")
}

func SymSubset(setA, setB []*GenericTypeSymbol) bool {
OUTER:
	for _, symA := range setA {
		for _, symB := range setB {
			if symA == symB {
				continue OUTER
			}
		}
		// symA not in setB
		return false
	}
	// found all
	return true
}

func registerGenericBuiltin(name, prefix, infix, postfix string, genericParams []*GenericTypeSymbol, args []Type, result Type, checker PostResolveValidator) {
	if len(genericParams) > 0 {
		for i, arg := range args {
			syms := extractGenericTypeSymbols(arg)
			if len(syms) > 0 {
				if !SymSubset(syms, genericParams) {
					panic("not a true subset")
				}
				args[i] = &OpenGenericType{arg, syms}
			}
		}
		syms := extractGenericTypeSymbols(result)
		if len(syms) > 0 {
			if !SymSubset(syms, genericParams) {
				panic("not a true subset")
			}
			result = &OpenGenericType{result, syms}
		}
	}

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
	registerGenericBuiltin(name, prefix, infix, postfix, nil, args, result, nil)
}

func registerSimpleTemplate(name string, args []Type, result Type, substitution TcExpr) {
	templateDef := &TcTemplateDef{
		// TODO set Source
		Name: name,
		Signature: ProcSignature{
			Params:     make([]TcSymbol, len(args)),
			ResultType: result,
		},
		Body: substitution,
	}
	sigParams := templateDef.Signature.Params
	for i, arg := range args {
		sigParams[i].Type = arg
		sigParams[i].Kind = SkProcArg
	}
	templateDef.Signature.TemplateImpl = templateDef
	builtinScope.Procedures[name] = append(builtinScope.Procedures[name], templateDef.Signature)
}

type PostResolveValidator func(tc *TypeChecker, scope Scope, call TcCall) TcCall

func registerBuiltinVararg(name, prefix, infix, postfix string, args []Type, result Type, checker PostResolveValidator) {
	// TODO actually do something with PostResolveValidator
	registerGenericBuiltin(name, prefix, infix, postfix, nil, args, result, checker)
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
	i := 1
	for j := 0; j < len(formatStr); j++ {
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
		default:
			panic(fmt.Errorf("internal error: %s", AstFormat(argType)))
		}
		i++
	}

	if i != len(call.Args) {
		tc.ReportErrorf(call.Args[i], "too many arguments for %s", AstFormat(formatExpr))
		return call
	}

	result = call
	result.Args[0] = CStrLit{Source: formatStrLit.Source, Value: formatStrC.String()}
	return result
}

func init() {
	arrayTypeMap = make(map[ArrayTypeMapKey]*ArrayType)
	enumSetTypeMap = make(map[*EnumType]*EnumSetType)
	ptrTypeMap = make(map[Type]*PtrType)
	typeTypeMap = make(map[Type]*TypeType)

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
	registerBuiltinType(TypeUInt8)
	registerBuiltinType(TypeUInt16)
	registerBuiltinType(TypeUInt32)
	registerBuiltinType(TypeUInt64)
	registerBuiltinType(TypeFloat32)
	registerBuiltinType(TypeFloat64)
	registerBuiltinType(TypeString)
	registerBuiltinType(TypeCString)
	registerBuiltinType(TypeVoid)
	registerBuiltinType(TypeNoReturn)
	registerBuiltinType(TypeNilPtr)

	registerTypeGroup(TypeAnyFloat)
	registerTypeGroup(TypeAnyInt)
	registerTypeGroup(TypeAnyUInt)
	registerTypeGroup(TypeAnyNumber)

	for _, typ := range TypeAnyNumber.Items {
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

	registerSimpleTemplate("high", []Type{GetTypeType(TypeInt8)}, TypeInt8, &IntLit{Value: 127, Type: TypeInt8})
	registerSimpleTemplate("high", []Type{GetTypeType(TypeInt16)}, TypeInt16, &IntLit{Value: 32767, Type: TypeInt16})
	registerSimpleTemplate("high", []Type{GetTypeType(TypeInt32)}, TypeInt32, &IntLit{Value: 2147483647, Type: TypeInt32})
	registerSimpleTemplate("high", []Type{GetTypeType(TypeInt64)}, TypeInt64, &IntLit{Value: 9223372036854775807, Type: TypeInt64})
	registerSimpleTemplate("low", []Type{GetTypeType(TypeInt8)}, TypeInt8, &IntLit{Value: -128, Type: TypeInt8})
	registerSimpleTemplate("low", []Type{GetTypeType(TypeInt16)}, TypeInt16, &IntLit{Value: -32768, Type: TypeInt16})
	registerSimpleTemplate("low", []Type{GetTypeType(TypeInt32)}, TypeInt32, &IntLit{Value: -2147483648, Type: TypeInt32})
	registerSimpleTemplate("low", []Type{GetTypeType(TypeInt64)}, TypeInt64, &IntLit{Value: -9223372036854775808, Type: TypeInt64})

	{
		// TODO: has no line information
		T := &GenericTypeSymbol{Name: "T", Constraint: TypeUnspecified}
		registerGenericBuiltin("addr", "&(", "", ")", []*GenericTypeSymbol{T}, []Type{T}, GetPtrType(T), nil)
		registerGenericBuiltin("[", "*(", "", ")", []*GenericTypeSymbol{T}, []Type{GetPtrType(T)}, T, nil)
		registerGenericBuiltin("=", "(", "=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeVoid, nil)
		registerGenericBuiltin("==", "(", "==", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean, nil)
		registerGenericBuiltin("!=", "(", "!=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean, nil)
	}

	registerBuiltin("and", "(", "&&", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean)
	registerBuiltin("or", "(", "||", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean)

	registerBuiltin("assert", "assert(", "", ")", []Type{TypeBoolean}, TypeVoid)

	registerConstant("true", TypeBoolean)
	registerConstant("false", TypeBoolean)

}
