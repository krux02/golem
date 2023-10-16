package main

import (
	"fmt"
	"strings"
)

type BuiltinType struct {
	Name         string
	InternalName string // name in code generation
	MangleChar   rune
}

type BuiltinIntType struct {
	Name         string
	InternalName string
	MangleChar   rune
	MinValue     int64
	MaxValue     uint64
}

type ErrorType struct{}
type UnspecifiedType struct{}

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

type IntLitType struct {
	Value int64
}

type MutableType struct {
	target Type
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
var _ Type = &BuiltinIntType{}
var _ Type = &TypeGroup{}
var _ Type = &EnumType{}
var _ Type = &EnumSetType{}
var _ Type = &StructType{}
var _ Type = &ArrayType{}
var _ Type = &IntLitType{}
var _ Type = &ErrorType{}
var _ Type = &TypeType{}
var _ Type = &PtrType{}
var _ Type = &OpenGenericType{}
var _ Type = &GenericTypeSymbol{}

func (typ *BuiltinType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.MangleChar)
}

func (typ *BuiltinIntType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.MangleChar)
}

func (typ *UnspecifiedType) ManglePrint(builder *strings.Builder) {
	panic("internal error, an unspecifed type should not be here")
}

func (typ *ErrorType) ManglePrint(builder *strings.Builder) {
	// this should not fail, as it is used during type checking phase, when an
	// error previously occured. It should not write anything that could actually
	// be used in the C compilation backend.
	builder.WriteString("?ErrorType?")
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

func (typ *MutableType) ManglePrint(builder *strings.Builder) {
	// mutability has no affect on name mangling. This will probably cause
	// problems later, but for now it can be used to explore the idea that
	// overloading a function, both mutable and non-mutable does not exist.
	typ.target.ManglePrint(builder)
}

func (typ *IntLitType) ManglePrint(builder *strings.Builder) {
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

// ManglePrint
func (typ *TypeType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune('T')
}

// **** Constants ****
// These type names are by no means final, there are just to get
// something working.

var TypeBoolean = &BuiltinType{"bool", "bool", 'b'}
var TypeInt8 = &BuiltinIntType{"i8", "int8_t", 'm', -0x80, 0x7f}
var TypeInt16 = &BuiltinIntType{"i16", "int16_t", 's', -0x8000, 0x7fff}
var TypeInt32 = &BuiltinIntType{"i32", "int32_t", 'i', -0x80000000, 0x7fffffff}
var TypeInt64 = &BuiltinIntType{"i64", "int64_t", 'l', -0x8000000000000000, 0x7fffffffffffffff}
var TypeUInt8 = &BuiltinIntType{"u8", "uint8_t", 'M', 0, 0xff}
var TypeUInt16 = &BuiltinIntType{"u16", "uint16_t", 'S', 0, 0xffff}
var TypeUInt32 = &BuiltinIntType{"u32", "uint32_t", 'I', 0, 0xffffffff}
var TypeUInt64 = &BuiltinIntType{"u64", "uint64_t", 'L', 0, 0xffffffffffffffff}
var TypeFloat32 = &BuiltinType{"f32", "float", 'f'}
var TypeFloat64 = &BuiltinType{"f64", "double", 'd'}
var TypeStr = &BuiltinType{"str", "string", 'R'}
var TypeChar = &BuiltinType{"char", "char", 'c'}
var TypeVoid = &BuiltinType{"void", "void", 'v'}
var TypeNilPtr = &BuiltinType{"nilptr", "void*", 'n'}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn", "void", '-'}

// this type is the internal representation when no type has been specified. It
// is not a type by its own.
var TypeUnspecified = &UnspecifiedType{}

// this type is the internal representation when the type checker fails to
// resolve the type. Expressions with this type cannot be further processed in
// code generation. This should probably be a different type, not BuiltinType
var TypeError = &ErrorType{}

var TypeAnyUInt = &TypeGroup{Name: "AnyUInt", Items: []Type{TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64}}
var TypeAnySInt = &TypeGroup{Name: "AnyInt", Items: []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64}}
var TypeAnyInt = &TypeGroup{Name: "AnyInt", Items: []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64, TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64}}
var TypeAnyFloat = &TypeGroup{Name: "AnyFloat", Items: []Type{TypeFloat32, TypeFloat64}}
var TypeAnyNumber = &TypeGroup{Name: "AnyNumber", Items: []Type{
	TypeFloat32, TypeFloat64,
	TypeInt8, TypeInt16, TypeInt32, TypeInt64,
	TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64,
}}

// types for C wrappers
var TypeCString = &BuiltinType{"cstring", "char const*", 'x'}

// builtin proc signatures
var builtinCPrintf ProcSignature
var builtinAddr ProcSignature
var builtinDeref ProcSignature

func (typ *BuiltinType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	if typ == TypeNoReturn {
		ReportErrorf(tc, context, "a default value of no retrun does not exist")
		return TcErrorNode{}
	} else if typ == TypeFloat32 || typ == TypeFloat64 {
		return FloatLit{Type: typ}
	} else if typ == TypeChar {
		return CharLit{}
	} else if typ == TypeBoolean {
		// TODO this is weird
		return LookUpLetSym(tc, builtinScope, Ident{Source: "false"}, TypeBoolean)
	} else if typ == TypeVoid {
		panic("not implemented void default value")
	} else {
		panic(fmt.Errorf("not implemented %+v", typ))
	}
}

func (typ *BuiltinIntType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return IntLit{Type: typ, Value: 0} // TODO no Source set
}

func (typ *UnspecifiedType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	ReportErrorf(tc, context, "variable definitions statements must have at least one, a type or a value expression")
	return TcErrorNode{}
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

func (typ *IntLitType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return IntLit{Type: typ, Value: typ.Value}
}

func (typ *MutableType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return typ.target.DefaultValue(tc, context)
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
	Parent:         nil,
	CurrentPackage: nil,
	CurrentProc:    nil,
	Types:          make(map[string]Type),
	Procedures:     make(map[string][]ProcSignature),
	Variables:      make(map[string]TcSymbol),
}

func registerBuiltinType(typ *BuiltinType) {
	_, ok := builtinScope.Types[typ.Name]
	if ok {
		panic("internal error double definition of builtin type")
	}
	builtinScope.Types[typ.Name] = typ
}

func registerBuiltinIntType(typ *BuiltinIntType) {
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
	case *IntLitType:
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

func registerGenericBuiltin(name, prefix, infix, postfix string, genericParams []*GenericTypeSymbol, args []Type, result Type) ProcSignature {
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

	procDef := &TcBuiltinGenericProcDef{
		Name: name,
		Signature: ProcSignature{
			GenericParams: genericParams,
			Params:        make([]TcSymbol, len(args)),
			ResultType:    result,
		},
		Prefix:  prefix,
		Infix:   infix,
		Postfix: postfix,
	}
	for i, arg := range args {
		procDef.Signature.Params[i].Type = arg
		procDef.Signature.Params[i].Kind = SkProcArg
	}
	procDef.Signature.Impl = procDef
	builtinScope.Procedures[name] = append(builtinScope.Procedures[name], procDef.Signature)
	return procDef.Signature
}

func registerBuiltinMacro(name string, varargs bool, args []Type, result Type, macroFunc BuiltinMacroFunc) {
	macroDef := &TcBuiltinMacroDef{
		Name: name,
		Signature: ProcSignature{
			Params:     make([]TcSymbol, len(args)),
			ResultType: result,
			Varargs:    varargs,
		},
		MacroFunc: macroFunc,
	}
	for i, arg := range args {
		macroDef.Signature.Params[i].Type = arg
		macroDef.Signature.Params[i].Kind = SkProcArg
	}
	macroDef.Signature.Impl = macroDef
	builtinScope.Procedures[name] = append(builtinScope.Procedures[name], macroDef.Signature)
}

func registerBuiltin(name, prefix, infix, postfix string, args []Type, result Type) ProcSignature {
	return registerGenericBuiltin(name, prefix, infix, postfix, nil, args, result)
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
	for i, arg := range args {
		templateDef.Signature.Params[i].Type = arg
		templateDef.Signature.Params[i].Kind = SkProcArg
	}
	templateDef.Signature.Impl = templateDef
	builtinScope.Procedures[name] = append(builtinScope.Procedures[name], templateDef.Signature)
}

type BuiltinMacroFunc func(tc *TypeChecker, scope Scope, call TcCall) TcExpr

func registerConstant(name string, value TcExpr) {
	var ident Ident
	ident.Source = name
	_ = builtinScope.NewConstSymbol(nil, ident, value)
}

func ValidatePrintfCall(tc *TypeChecker, scope Scope, call TcCall) TcExpr {
	formatExpr := call.Args[0]
	formatStrLit, ok := formatExpr.(StrLit)
	if !ok {
		ReportErrorf(tc, formatExpr, "format string must be a string literal")
		return call
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
			ReportErrorf(tc, formatExpr, "incomplete format expr at end of format string")
			break
		}
		c2 := formatStr[j]
		var typeExpectation Type = TypeUnspecified
		switch c2 {
		case '%':
			formatStrC.WriteRune('%')
			continue
		case 's':
			typeExpectation = TypeStr
		case 'd':
			typeExpectation = TypeAnyInt
		case 'f':
			typeExpectation = TypeAnyFloat
		case 'v':
			typeExpectation = TypeUnspecified
		default:
			ReportErrorf(tc, formatExpr, "invalid format expr %%%c in %s", c2, AstFormat(formatExpr))
			return call
		}
		if i == len(call.Args) {
			ReportErrorf(tc, formatExpr, "not enough arguments for %s", AstFormat(formatExpr))
			return call
		}

		argType := call.Args[i].GetType()
		ExpectType(tc, call.Args[i], argType, typeExpectation)
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
		case TypeStr:
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
		ReportErrorf(tc, call.Args[i], "too many arguments for %s", AstFormat(formatExpr))
		return call
	}

	result := call

	result.Sym = TcProcSymbol{
		Source: call.Sym.Source,
		Impl:   builtinCPrintf.Impl,
	}
	result.Args[0] = CStrLit{Source: formatStrLit.Source, Value: formatStrC.String()}
	return result
}

func BuiltinAddCFlags(tc *TypeChecker, scope Scope, call TcCall) TcExpr {
	if len(call.Args) != 1 {
		ReportErrorf(tc, call, "expect single string literal as argument")
		return TcErrorNode{call}
	}
	switch arg0 := call.Args[0].(type) {
	case StrLit:
		scope.CurrentPackage.CFlags = append(scope.CurrentPackage.CFlags, arg0.Value)
	default:
		ReportErrorf(tc, call, "expect single string literal as argument")
		return TcErrorNode{call}
	}
	return TcCodeBlock{Source: call.Source}
}

func init() {
	arrayTypeMap = make(map[ArrayTypeMapKey]*ArrayType)
	enumSetTypeMap = make(map[*EnumType]*EnumSetType)
	ptrTypeMap = make(map[Type]*PtrType)
	typeTypeMap = make(map[Type]*TypeType)
	packageMap = make(map[string]*TcPackageDef)
	intLitTypeMap = make(map[int64]*IntLitType)
	mutableTypeMap = make(map[Type]*MutableType)

	// Printf is literally the only use case for real varargs that I actually see as
	// practical. Therefore the implementation for varargs will be strictly tied to
	// printf for now. A general concept for varargs will be specified out as soon
	// as it becomes necessary, but right now it is not planned.
	builtinCPrintf = registerBuiltin("c_printf", "printf(", ", ", ")", []Type{TypeCString}, TypeVoid)
	registerBuiltinMacro("printf", true, []Type{TypeStr}, TypeVoid, ValidatePrintfCall)

	registerBuiltinMacro("addCFlags", false, []Type{TypeStr}, TypeVoid, BuiltinAddCFlags)

	registerBuiltinType(TypeBoolean)
	registerBuiltinIntType(TypeInt8)
	registerBuiltinIntType(TypeInt16)
	registerBuiltinIntType(TypeInt32)
	registerBuiltinIntType(TypeInt64)
	registerBuiltinIntType(TypeUInt8)
	registerBuiltinIntType(TypeUInt16)
	registerBuiltinIntType(TypeUInt32)
	registerBuiltinIntType(TypeUInt64)
	registerBuiltinType(TypeFloat32)
	registerBuiltinType(TypeFloat64)
	registerBuiltinType(TypeStr)
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

		if intType, isIntType := typ.(*BuiltinIntType); isIntType {
			registerSimpleTemplate("high", []Type{GetTypeType(typ)}, typ, IntLit{Value: int64(intType.MaxValue), Type: typ})
			registerSimpleTemplate("low", []Type{GetTypeType(typ)}, typ, IntLit{Value: intType.MinValue, Type: typ})
		}
	}

	{
		// TODO: has no line information
		T := &GenericTypeSymbol{Name: "T", Constraint: TypeUnspecified}
		builtinAddr = registerGenericBuiltin("addr", "&(", "", ")", []*GenericTypeSymbol{T}, []Type{T}, GetPtrType(T))
		builtinDeref = registerGenericBuiltin("[", "*(", "", ")", []*GenericTypeSymbol{T}, []Type{GetPtrType(T)}, T)
		registerGenericBuiltin("=", "(", "=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeVoid)
		registerGenericBuiltin("==", "(", "==", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean)
		registerGenericBuiltin("!=", "(", "!=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean)
	}

	registerBuiltin("and", "(", "&&", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean)
	registerBuiltin("or", "(", "||", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean)

	registerBuiltin("assert", "assert(", "", ")", []Type{TypeBoolean}, TypeVoid)

	registerConstant("true", IntLit{Type: TypeBoolean, Value: 1})
	registerConstant("false", IntLit{Type: TypeBoolean, Value: 0})

}
