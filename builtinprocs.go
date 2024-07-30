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

type BuiltinFloatType struct {
	Name         string
	InternalName string
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

type TypeTrait struct {
	Impl *TcTraitDef
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

// the type for integer literals. Each integer represented by a literal is its own type
type IntLitType struct {
	// TODO needs to be able to store all values from uint64 and int64, int640is not enough
	Value int64
}

type FloatLitType struct {
	Value float64
}

// the type that represets types as arguments. Example:
//
//	sizeof(type int)
//	max(type int)
//	min(type int)
//	alignof(type int)
type TypeType struct {
	WrappedType Type
}

var _ Type = &BuiltinType{}
var _ Type = &BuiltinIntType{}
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

var _ TypeConstraint = &TypeGroup{}
var _ TypeConstraint = &TypeTrait{}
var _ TypeConstraint = &UnspecifiedType{}

func (typ *BuiltinType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.MangleChar)
}

func (typ *BuiltinIntType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.MangleChar)
}

func (typ *BuiltinFloatType) ManglePrint(builder *strings.Builder) {
	builder.WriteRune(typ.MangleChar)
}

func (typ *ErrorType) ManglePrint(builder *strings.Builder) {
	// this should not fail, as it is used during type checking phase, when an
	// error previously occured. It should not write anything that could actually
	// be used in the C compilation backend.
	builder.WriteString("?ErrorType?")
}

func (typ *TypeType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteNode(typ.WrappedType)
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

func (typ *IntLitType) ManglePrint(builder *strings.Builder) {
	fmt.Fprintf(builder, "%d_", typ.Value)
}

func (typ *FloatLitType) ManglePrint(builder *strings.Builder) {
	panic("not implemented")
}

func (typ *GenericTypeSymbol) ManglePrint(builder *strings.Builder) {
	fmt.Fprintf(builder, "?%s?", typ.Name)
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

var TypeInt8 = &BuiltinIntType{"i8", "int8_t", 'm', -0x80, 0x7f}
var TypeInt16 = &BuiltinIntType{"i16", "int16_t", 's', -0x8000, 0x7fff}
var TypeInt32 = &BuiltinIntType{"i32", "int32_t", 'i', -0x80000000, 0x7fffffff}
var TypeInt64 = &BuiltinIntType{"i64", "int64_t", 'l', -0x8000000000000000, 0x7fffffffffffffff}

var TypeUInt8 = &BuiltinIntType{"u8", "uint8_t", 'M', 0, 0xff}
var TypeUInt16 = &BuiltinIntType{"u16", "uint16_t", 'S', 0, 0xffff}
var TypeUInt32 = &BuiltinIntType{"u32", "uint32_t", 'I', 0, 0xffffffff}
var TypeUInt64 = &BuiltinIntType{"u64", "uint64_t", 'L', 0, 0xffffffffffffffff}

var TypeFloat32 = &BuiltinFloatType{"f32", "float", 'f'}
var TypeFloat64 = &BuiltinFloatType{"f64", "double", 'd'}

var TypeBoolean = &BuiltinType{"bool", "bool", 'b'}
var TypeStr = &BuiltinType{"str", "string", 'R'}
var TypeChar = &BuiltinType{"char", "char", 'c'}
var TypeVoid = &BuiltinType{"void", "void", 'v'}
var TypeNilPtr = &BuiltinType{"nilptr", "void*", 'n'}

// vector type
var TypeFloat32x4 = &BuiltinType{"f32x4", "f32x4", '4'}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn", "void", '-'}

// this type is the internal representation when no type has been specified. It
// is not a type by its own.
var TypeUnspecified = &UnspecifiedType{}

// this type is the internal representation when the type checker fails to
// resolve the type. Expressions with this type cannot be further processed in
// code generation.
var TypeError = &ErrorType{}

var TypeAnyUInt = &TypeGroup{Name: "AnyUInt", Items: []Type{TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64}}
var TypeAnySInt = &TypeGroup{Name: "AnySInt", Items: []Type{TypeInt8, TypeInt16, TypeInt32, TypeInt64}}
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
var builtinCPrintf *Signature
var builtinAddr *Signature
var builtinDeref *Signature

func (typ *BuiltinType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	if typ == TypeNoReturn {
		ReportErrorf(tc, context, "a default value of no retrun does not exist")
		return newErrorNode(context)
	} else if typ == TypeChar {
		return CharLit{}
	} else if typ == TypeBoolean {
		// TODO this is weird
		// no source location
		sym := builtinScope.Variables["false"]
		sym.Type = TypeBoolean
		return sym
	} else if typ == TypeVoid {
		panic("not implemented void default value")
	} else {
		panic(fmt.Errorf("not implemented %+v", typ))
	}
}

func (typ *BuiltinIntType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return IntLit{Type: typ, Value: 0} // TODO no Source set
}

func (typ *BuiltinFloatType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return FloatLit{Type: typ}
}

func (typ *ErrorType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return nil
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

func (typ *FloatLitType) DefaultValue(tc *TypeChecker, context AstNode) TcExpr {
	return FloatLit{Type: typ, Value: typ.Value}
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
	Parent:          nil,
	CurrentPackage:  nil,
	CurrentProc:     nil,
	Signatures:      make(map[string][]*Signature),
	Variables:       make(map[string]TcSymbol),
	Types:           make(map[string]Type),
	TypeConstraints: make(map[string]TypeConstraint),
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

func registerBuiltinFloatType(typ *BuiltinFloatType) {
	_, ok := builtinScope.Types[typ.Name]
	if ok {
		panic("internal error double definition of builtin type")
	}
	builtinScope.Types[typ.Name] = typ
}

func registerBuiltinTypeGroup(typ *TypeGroup) {
	builtinScope.TypeConstraints[typ.Name] = typ
}

func extractGenericTypeSymbols(typ Type) (result []*GenericTypeSymbol) {
	switch typ := typ.(type) {
	case *BuiltinType:
		return nil
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
	case *BuiltinIntType:
		return nil
	case *BuiltinFloatType:
		return nil
	case *ErrorType:
		panic("internal error")
	case *TypeType:
		return extractGenericTypeSymbols(typ.WrappedType)
	case *PtrType:
		return extractGenericTypeSymbols(typ.Target)
	case *OpenGenericType:
		panic("interanl error, OpenGenericType should not be created around another OpenGenericType")
	case *GenericTypeSymbol:
		return []*GenericTypeSymbol{typ}
	}
	panic(fmt.Errorf("unhandled type: %T", typ))
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

func makeGenericSignature(name string, genericParams []*GenericTypeSymbol, args []Type, result Type, firstArgMutable bool) *Signature {
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

	signature := Signature{
		Name:          name,
		GenericParams: genericParams,
		Params:        make([]TcSymbol, len(args)),
		ResultType:    result,
	}
	for i, arg := range args {
		signature.Params[i].Type = arg
		if firstArgMutable && i == 0 {
			signature.Params[i].Kind = SkVarProcArg
		} else {
			signature.Params[i].Kind = SkProcArg
		}
	}

	return &signature
}

func registerGenericBuiltin(name, prefix, infix, postfix string, genericParams []*GenericTypeSymbol, args []Type, result Type, firstArgMutable bool) *Signature {
	procDef := &TcBuiltinGenericProcDef{
		Signature: makeGenericSignature(name, genericParams, args, result, firstArgMutable),
		Prefix:    prefix,
		Infix:     infix,
		Postfix:   postfix,
	}
	procDef.Signature.Impl = procDef
	builtinScope.Signatures[name] = append(builtinScope.Signatures[name], procDef.Signature)
	return procDef.Signature
}

func registerBuiltin(name, prefix, infix, postfix string, args []Type, result Type, firstArgMutable bool) *Signature {
	return registerGenericBuiltin(name, prefix, infix, postfix, nil, args, result, firstArgMutable)
}

func registerGenericBuiltinMacro(name string, varargs bool, genericParams []*GenericTypeSymbol, args []Type, result Type, macroFunc BuiltinMacroFunc) {
	macroDef := &TcBuiltinMacroDef{
		Signature: makeGenericSignature(name, genericParams, args, result, false),
		MacroFunc: macroFunc,
	}
	macroDef.Signature.Varargs = varargs
	macroDef.Signature.Impl = macroDef
	builtinScope.Signatures[name] = append(builtinScope.Signatures[name], macroDef.Signature)
}

func registerBuiltinMacro(name string, varargs bool, args []Type, result Type, macroFunc BuiltinMacroFunc) {
	registerGenericBuiltinMacro(name, varargs, nil, args, result, macroFunc)
}

func registerSimpleTemplate(name string, args []Type, result Type, substitution TcExpr) {
	templateDef := &TcTemplateDef{
		// TODO set Source
		Signature: &Signature{
			Name:       name,
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
	builtinScope.Signatures[name] = append(builtinScope.Signatures[name], templateDef.Signature)
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
		var typeExpectation TypeConstraint = TypeUnspecified
		switch c2 {
		case '%':
			formatStrC.WriteRune('%')
			continue
		case 's':
			typeExpectation = UniqueTypeConstraint{TypeStr}
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
		Source:    call.Sym.Source,
		Signature: builtinCPrintf,
	}
	result.Args[0] = CStrLit{Source: formatStrLit.Source, Value: formatStrC.String()}
	return result
}

func BuiltinAddLinkerFlags(tc *TypeChecker, scope Scope, call TcCall) TcExpr {
	if len(call.Args) != 1 {
		ReportErrorf(tc, call, "expect single string literal as argument")
		return newErrorNode(call)
	}
	switch arg0 := call.Args[0].(type) {
	case StrLit:
		program := scope.CurrentProgram
		program.LinkerFlags = append(program.LinkerFlags, arg0.Value)
	default:
		ReportErrorf(tc, call, "expect single string literal as argument")
		return newErrorNode(call)
	}
	return TcCodeBlock{Source: call.Source}
}

func BuiltinAddCFlags(tc *TypeChecker, scope Scope, call TcCall) TcExpr {
	if len(call.Args) != 1 {
		ReportErrorf(tc, call, "expect single string literal as argument")
		return newErrorNode(call)
	}
	switch arg0 := call.Args[0].(type) {
	case StrLit:
		scope.CurrentPackage.CFlags = append(scope.CurrentPackage.CFlags, arg0.Value)
	default:
		ReportErrorf(tc, call, "expect single string literal as argument")
		return newErrorNode(call)
	}
	return TcCodeBlock{Source: call.Source}
}

// func BuiltinSizeOf(tc *TypeChecker, scope Scope, call TcCall) TcExpr {
// 	if len(call.Args) != 1 {
// 		ReportErrorf(tc, call, "expect single string literal as argument")
// 		return newErrorNode(call)
// 	}
// 	typ := call.Args[0].GetType()
// 	size := GetTypeSize(typ)
// 	return IntLit{Source: call.Source, Type: TypeInt64, Value: size}
// }

func init() {
	arrayTypeMap = make(map[ArrayTypeMapKey]*ArrayType)
	enumSetTypeMap = make(map[*EnumType]*EnumSetType)
	ptrTypeMap = make(map[Type]*PtrType)
	typeTypeMap = make(map[Type]*TypeType)
	packageMap = make(map[string]*TcPackageDef)
	intLitTypeMap = make(map[int64]*IntLitType)
	floatLitTypeMap = make(map[float64]*FloatLitType)

	// Printf is literally the only use case for real varargs that I actually see as
	// practical. Therefore the implementation for varargs will be strictly tied to
	// printf for now. A general concept for varargs will be specified out as soon
	// as it becomes necessary, but right now it is not planned.
	builtinCPrintf = registerBuiltin("c_printf", "printf(", ", ", ")", []Type{TypeCString}, TypeVoid, false)
	builtinCPrintf.Varargs = true

	registerBuiltinMacro("printf", true, []Type{TypeStr}, TypeVoid, ValidatePrintfCall)
	registerBuiltinMacro("addCFlags", false, []Type{TypeStr}, TypeVoid, BuiltinAddCFlags)
	registerBuiltinMacro("addLinkerFlags", false, []Type{TypeStr}, TypeVoid, BuiltinAddLinkerFlags)

	// registerBuiltinMacro("sizeof", false, []Type{TypeUnspecified}, TypeInt64, BuiltinSizeOf)

	registerBuiltinType(TypeBoolean)
	registerBuiltinIntType(TypeInt8)
	registerBuiltinIntType(TypeInt16)
	registerBuiltinIntType(TypeInt32)
	registerBuiltinIntType(TypeInt64)
	registerBuiltinIntType(TypeUInt8)
	registerBuiltinIntType(TypeUInt16)
	registerBuiltinIntType(TypeUInt32)
	registerBuiltinIntType(TypeUInt64)
	registerBuiltinFloatType(TypeFloat32)
	registerBuiltinFloatType(TypeFloat64)
	registerBuiltinType(TypeStr)
	registerBuiltinType(TypeCString)
	registerBuiltinType(TypeVoid)
	registerBuiltinType(TypeNoReturn)
	registerBuiltinType(TypeNilPtr)

	registerBuiltinType(TypeFloat32x4)

	// type aliases
	builtinScope.Types["pointer"] = GetPtrType(TypeVoid)
	builtinScope.Types["int"] = TypeInt64
	builtinScope.Types["uint"] = TypeUInt64

	registerBuiltinTypeGroup(TypeAnyFloat)
	registerBuiltinTypeGroup(TypeAnyInt)
	registerBuiltinTypeGroup(TypeAnyUInt)
	registerBuiltinTypeGroup(TypeAnySInt)
	registerBuiltinTypeGroup(TypeAnyNumber)

	for _, typ := range TypeAnyNumber.Items {
		registerBuiltin("+", "(", "+", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("-", "(", "-", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("*", "(", "*", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("/", "(", "/", ")", []Type{typ, typ}, typ, false)

		registerBuiltin("<", "(", "<", ")", []Type{typ, typ}, TypeBoolean, false)
		registerBuiltin("<=", "(", "<=", ")", []Type{typ, typ}, TypeBoolean, false)
		registerBuiltin(">", "(", ">", ")", []Type{typ, typ}, TypeBoolean, false)
		registerBuiltin(">=", "(", ">=", ")", []Type{typ, typ}, TypeBoolean, false)

		for _, op := range []string{"+=", "-=", "*=", "/="} {
			registerBuiltin(op, "(", op, ")", []Type{typ, typ}, TypeVoid, true)
		}

		// TODO this should be a generic with proper working type constraint on all builtin number types
		registerBuiltin("i8", "(int8_t)(", "", ")", []Type{typ}, TypeInt8, false)
		registerBuiltin("i16", "(int16_t)(", "", ")", []Type{typ}, TypeInt16, false)
		registerBuiltin("i32", "(int32_t)(", "", ")", []Type{typ}, TypeInt32, false)
		registerBuiltin("i64", "(int64_t)(", "", ")", []Type{typ}, TypeInt64, false)
		registerBuiltin("int", "(int64_t)(", "", ")", []Type{typ}, TypeInt64, false)

		registerBuiltin("u8", "(uint8_t)(", "", ")", []Type{typ}, TypeUInt8, false)
		registerBuiltin("u16", "(uint16_t)(", "", ")", []Type{typ}, TypeUInt16, false)
		registerBuiltin("u32", "(uint32_t)(", "", ")", []Type{typ}, TypeUInt32, false)
		registerBuiltin("u64", "(uint64_t)(", "", ")", []Type{typ}, TypeUInt64, false)
		registerBuiltin("uint", "(uint64_t)(", "", ")", []Type{typ}, TypeUInt64, false)

		registerBuiltin("f32", "(float)(", "", ")", []Type{typ}, TypeFloat32, false)
		registerBuiltin("f64", "(double)(", "", ")", []Type{typ}, TypeFloat64, false)

		if intType, isIntType := typ.(*BuiltinIntType); isIntType {
			registerSimpleTemplate("high", []Type{GetTypeType(typ)}, typ, IntLit{Value: int64(intType.MaxValue), Type: typ})
			registerSimpleTemplate("low", []Type{GetTypeType(typ)}, typ, IntLit{Value: intType.MinValue, Type: typ})
		}
	}

	// TODO
	//   * test this
	//   * use a generics for this
	//   * allow varargs
	for _, typ := range TypeAnyInt.Items {
		registerBuiltin("bitand", "(", "&", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("bitor", "(", "|", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("bitnot", "~(", "", ")", []Type{typ}, typ, false)
	}

	registerBuiltin("len", "", "", ".len", []Type{TypeStr}, TypeInt64, false)
	registerBuiltin("cstring", "", "", ".data", []Type{TypeStr}, TypeCString, false)

	// vector types
	for _, typ := range []Type{TypeFloat32x4} {
		registerBuiltin("+", "(", "+", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("-", "(", "-", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("*", "(", "*", ")", []Type{typ, typ}, typ, false)
		registerBuiltin("/", "(", "/", ")", []Type{typ, typ}, typ, false)

		registerBuiltin("<", "(", "<", ")", []Type{typ, typ}, TypeBoolean, false)
		registerBuiltin("<=", "(", "<=", ")", []Type{typ, typ}, TypeBoolean, false)
		registerBuiltin(">", "(", ">", ")", []Type{typ, typ}, TypeBoolean, false)
		registerBuiltin(">=", "(", ">=", ")", []Type{typ, typ}, TypeBoolean, false)

	}

	{
		// TODO: has no line information
		T := &GenericTypeSymbol{Name: "T", Constraint: TypeUnspecified}
		// TODO mark argument as mutable
		builtinAddr = registerGenericBuiltin("addr", "&(", "", ")", []*GenericTypeSymbol{T}, []Type{T}, GetPtrType(T), true)
		builtinDeref = registerGenericBuiltin("[", "*(", "", ")", []*GenericTypeSymbol{T}, []Type{GetPtrType(T)}, T, false)
		// TODO mark first argument as mutable
		registerGenericBuiltin("=", "(", "=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeVoid, true)
		registerGenericBuiltin("==", "(", "==", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean, false)
		registerGenericBuiltin("!=", "(", "!=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean, false)

		registerGenericBuiltin("pointer", "(void*)(", "", ")", []*GenericTypeSymbol{T}, []Type{GetPtrType(T)}, GetPtrType(TypeVoid), false)
		registerGenericBuiltin("sizeof", "sizeof(", "", ")", []*GenericTypeSymbol{T}, []Type{T}, TypeInt64, false)
		registerGenericBuiltin("discard", "", "", "", []*GenericTypeSymbol{T}, []Type{T}, TypeVoid, false)
	}

	{
		// TODO: has no line information
		T := &GenericTypeSymbol{Name: "T", Constraint: TypeUnspecified}
		U := &GenericTypeSymbol{Name: "U", Constraint: TypeUnspecified}
		registerGenericBuiltin("cast", "*((", "*)(&(", ")))", []*GenericTypeSymbol{T, U}, []Type{GetTypeType(T), U}, T, false)
	}

	registerBuiltin("and", "(", "&&", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean, false)
	registerBuiltin("or", "(", "||", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean, false)
	registerBuiltin("not", "!(", "", ")", []Type{TypeBoolean}, TypeBoolean, false)

	registerBuiltin("assert", "assert(", "", ")", []Type{TypeBoolean}, TypeVoid, false)

	registerConstant("true", IntLit{Type: TypeBoolean, Value: 1})
	registerConstant("false", IntLit{Type: TypeBoolean, Value: 0})

}
