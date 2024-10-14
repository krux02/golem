package main

import (
	"fmt"
	"math/big"
	"reflect"
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

type BuiltinStringType struct {
	Name         string
	InternalName string
	MangleChar   rune
}

type BuiltinIntType struct {
	Name         string
	InternalName string
	MangleChar   rune
	MinValue     *big.Int
	MaxValue     *big.Int
}

type UntypedType struct{ Name string } // explicitly untyped ast, pass in the ast verbatim, no type checking.

type ErrorType struct{}
type UnspecifiedType struct{} // type constraint: no costraints

type TypeGroup struct {
	Name  string
	Items []Type
}

type TypeTrait struct {
	Impl *TcTraitDef
}

type EnumType struct {
	Impl *TcEnumDef
}

type StructType struct {
	Impl *TcStructDef
}

type ArrayType struct {
	Len  int64
	Elem Type
}

// types for literals

type IntLitType struct {
	Value *big.Int
}

type FloatLitType struct {
	Value *big.Float
}

type StringLitType struct {
	Value string
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

func (typ *BuiltinStringType) ManglePrint(builder *strings.Builder) {
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

func (typ *StringLitType) ManglePrint(builder *strings.Builder) {
	panic("not implemented")
}

func (typ *UntypedType) ManglePrint(builder *strings.Builder) {
	panic("untyped ast may not exist in C code generation")
}

func (typ *GenericTypeSymbol) ManglePrint(builder *strings.Builder) {
	fmt.Fprintf(builder, "?%s?", typ.Name)
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
// something working

var TypeInt8 = &BuiltinIntType{"i8", "int8_t", 'm', big.NewInt(-0x80), big.NewInt(0x7f)}
var TypeInt16 = &BuiltinIntType{"i16", "int16_t", 's', big.NewInt(-0x8000), big.NewInt(0x7fff)}
var TypeInt32 = &BuiltinIntType{"i32", "int32_t", 'i', big.NewInt(-0x80000000), big.NewInt(0x7fffffff)}
var TypeInt64 = &BuiltinIntType{"i64", "int64_t", 'l', big.NewInt(-0x8000000000000000), big.NewInt(0x7fffffffffffffff)}

var TypeUInt8 = &BuiltinIntType{"u8", "uint8_t", 'M', big.NewInt(0), big.NewInt(0xff)}
var TypeUInt16 = &BuiltinIntType{"u16", "uint16_t", 'S', big.NewInt(0), big.NewInt(0xffff)}
var TypeUInt32 = &BuiltinIntType{"u32", "uint32_t", 'I', big.NewInt(0), big.NewInt(0xffffffff)}
var TypeUInt64 = &BuiltinIntType{"u64", "uint64_t", 'L', big.NewInt(0), big.NewInt(0).SetUint64(0xffffffffffffffff)}

var TypeFloat32 = &BuiltinFloatType{"f32", "float", 'f'}
var TypeFloat64 = &BuiltinFloatType{"f64", "double", 'd'}

var TypeBoolean = &BuiltinType{"bool", "bool", 'b'}

var TypeStr = &BuiltinStringType{"str", "string", 'R'}
var TypeCStr = &BuiltinStringType{"cstr", "char const*", 'x'}

// TODO is this really best as a subtype of BuiltinStringType
var TypeChar = &BuiltinStringType{"char", "int32_t", 'c'}

// TODO maybe have something like this cchar type
//var TypeCChar = &BuiltinIntType{"cchar", "char", 'm', big.NewInt(-0x80), big.NewInt(0x7f)}

var TypeVoid = &BuiltinType{"void", "void", 'v'}
var TypeNilPtr = &BuiltinType{"nilptr", "void*", 'n'}

// vector type
var TypeFloat32x4 = &BuiltinType{"f32x4", "f32x4", '4'}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn", "void", '-'}

// this type is the internal representation when no type has been specified. It
// is not a type by its own.
var TypeUnspecified = &UnspecifiedType{}

// this type is the internal representaion when an untyped ast should be passed to a macro or template.
var TypeUntyped Type = &UntypedType{"untyped"}

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

// TODO: The char type is not included in there, even though it is implemented
// as a BuiltinStringType. This is an ugly code smell.
var TypeAnyString = &TypeGroup{Name: "AnyString", Items: []Type{TypeStr, TypeCStr}}

// builtin proc signatures
var builtinCPrintf Overloadable
var builtinAddr Overloadable
var builtinDeref Overloadable

func (typ *BuiltinType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	if typ == TypeNoReturn {
		ReportErrorf(sc, context, "a default value of no retrun does not exist")
		return newErrorNode(context)
	} else if typ == TypeBoolean {
		// TODO this is weird
		// no source location
		return builtinScope.Variables["false"]
	} else if typ == TypeVoid {
		panic("not implemented void default value")
	} else {
		panic(fmt.Errorf("not implemented %+v", typ))
	}
}

func (typ *BuiltinIntType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcIntLit{Type: typ, Value: big.NewInt(0)} // TODO no Source set
}

func (typ *BuiltinFloatType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcFloatLit{Type: typ, Value: big.NewFloat(0)}
}

func (typ *BuiltinStringType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	if typ == TypeChar {
		return &TcStrLit{Type: typ, Value: "\000"}
	}
	return &TcStrLit{Type: typ}

}

func (typ *ErrorType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return nil
}

func (typ *ArrayType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcArrayLit{ElemType: typ.Elem}
}

func (typ *StructType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcStructLit{Type: typ}
}

func (typ *EnumType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return typ.Impl.Values[0]
}

func (typ *PtrType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &NilLit{Type: typ}
}

func (typ *EnumSetType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcEnumSetLit{ElemType: typ.Elem}
}

func (typ *IntLitType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcIntLit{Type: typ, Value: typ.Value}
}

func (typ *FloatLitType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcFloatLit{Type: typ, Value: typ.Value}
}

func (typ *StringLitType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcStrLit{Type: typ, Value: typ.Value}
}

func (typ *UntypedType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	return &TcCodeBlock{}
}

func (typ *TypeType) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	panic("no default value for types")
}

func (typ *GenericTypeSymbol) DefaultValue(sc *SemChecker, context Expr) TcExpr {
	panic("this is an abstrict type, there cannot be a default value")
}

var builtinScope Scope = &ScopeImpl{
	Parent:               nil,
	CurrentPackage:       nil,
	CurrentProcSignature: nil,
	Overloadables:        make(map[string][]Overloadable),
	Variables:            make(map[string]*TcSymbol),
	Types:                make(map[string]Type),
	TypeConstraints:      make(map[string]TypeConstraint),
}

func registerBuiltinType(typ Type) {

	rValue := reflect.ValueOf(typ).Elem()

	nameField := rValue.FieldByName("Name")
	if nameField.Kind() != reflect.String {
		panic(fmt.Errorf("internal error type %s needs Name Field of type string", rValue.Type().Name()))
	}
	RegisterType(nil, builtinScope, nameField.String(), typ, nil)
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

func AstListFormat[T PrettyPrintable](syms []T) string {
	builder := &AstPrettyPrinter{}
	builder.WriteString("{")
	for i, sym := range syms {
		if i != 0 {
			builder.WriteString(", ")
		}
		sym.PrettyPrint(builder)
	}
	builder.WriteString("}")
	return builder.String()
}

func makeGenericSignature(name string, genericParams []*GenericTypeSymbol, args []Type, result Type, argMutableBitmask uint64) Signature {
	params := make([]*TcSymbol, len(args))
	for i, arg := range args {
		params[i] = &TcSymbol{Type: arg}
		if (argMutableBitmask & (1 << i)) != 0 {
			params[i].Kind = SkVarProcArg
		} else {
			params[i].Kind = SkProcArg
		}
	}

	return Signature{
		Name:          name,
		GenericParams: genericParams,
		Params:        params,
		ResultType:    result,
	}
}

func registerGenericBuiltin(name, prefix, infix, postfix string, genericParams []*GenericTypeSymbol, args []Type, result Type, argMutableBitmask uint64) Overloadable {
	procDef := &TcBuiltinProcDef{
		Signature:     makeGenericSignature(name, genericParams, args, result, argMutableBitmask),
		Prefix:        prefix,
		Infix:         infix,
		Postfix:       postfix,
		InstanceCache: NewInstanceCache(len(genericParams)),
	}
	RegisterProc(nil, builtinScope, procDef, nil)
	return procDef
}

func registerBuiltin(name, prefix, infix, postfix string, args []Type, result Type, argMutableBitmask uint64) Overloadable {
	return registerGenericBuiltin(name, prefix, infix, postfix, nil, args, result, argMutableBitmask)
}

func registerGenericBuiltinMacro(name string, varargs bool, genericParams []*GenericTypeSymbol, args []Type, result Type, macroFunc BuiltinMacroFunc) {
	macroDef := &TcBuiltinMacroDef{
		Signature: makeGenericSignature(name, genericParams, args, result, 0),
		MacroFunc: macroFunc,
	}
	macroDef.Signature.Varargs = varargs
	RegisterProc(nil, builtinScope, macroDef, nil)
}

func registerBuiltinMacro(name string, varargs bool, args []Type, result Type, macroFunc BuiltinMacroFunc) {
	registerGenericBuiltinMacro(name, varargs, nil, args, result, macroFunc)
}

func registerSimpleTemplate(name string, params []*TcSymbol, result Type, substitution Expr) {
	templateDef := &TcTemplateDef{
		// TODO set Source
		Signature: Signature{
			Name:       name,
			Params:     params,
			ResultType: result,
		},
		Body: substitution,
	}
	RegisterProc(nil, builtinScope, templateDef, nil)
}

type BuiltinMacroFunc func(sc *SemChecker, scope Scope, call *TcCall) TcExpr

func registerConstant(name string, value TcExpr) {
	ident := &Ident{Source: name}
	_ = builtinScope.NewConstSymbol(nil, ident, value)
}

func ValidatePrintfCall(sc *SemChecker, scope Scope, call *TcCall) TcExpr {

	formatExpr := call.Args[0]
	formatStrLit, ok := formatExpr.(*TcStrLit)
	if !ok {
		ReportErrorf(sc, formatExpr, "format string must be a string literal")
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
			ReportErrorf(sc, formatExpr, "incomplete format expr at end of format string")
			break
		}

		if i == len(call.Args) {
			ReportErrorf(sc, formatExpr, "not enough arguments for %s", AstFormat(formatExpr))
			return call
		}
		argType := call.Args[i].GetType()
		c2 := formatStr[j]

		switch c2 {
		case 'v':
			switch argType {
			case TypeInt8, TypeInt16, TypeInt32, TypeInt64:
				c2 = 'd'
			case TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64:
				c2 = 'u'
			case TypeCStr, TypeStr:
				c2 = 's'
			case TypeChar:
				c2 = 'c'
			case TypeFloat32, TypeFloat64:
				c2 = 'f'
			default:
				// TODO test error message

				ReportErrorf(sc, formatExpr, "type not supported for %%v: %s", AstFormat(argType))
				return call
			}
		case 'd':
			switch argType {
			case TypeUInt8, TypeUInt16, TypeUInt32, TypeUInt64:
				c2 = 'u'
			}
		}

		var typeExpectation TypeConstraint = TypeUnspecified

		switch c2 {
		case '%':
			formatStrC.WriteRune('%')
			continue
		case 's':
			typeExpectation = TypeAnyString
		case 'd':
			typeExpectation = TypeAnyInt
		case 'x', 'X', 'u':
			typeExpectation = TypeAnyUInt
		case 'f':
			typeExpectation = TypeAnyFloat
		case 'c':
			typeExpectation = UniqueTypeConstraint{TypeChar}
		default:
			ReportErrorf(sc, formatExpr, "invalid format expr %%%c in %s", c2, AstFormat(formatExpr))
			return call
		}

		ExpectType(sc, call.Args[i], argType, typeExpectation)
		switch argType {
		case TypeInt8, TypeUInt8:
			formatStrC.WriteString("hh")
			formatStrC.WriteRune(rune(c2))
		case TypeInt16, TypeUInt16:
			formatStrC.WriteString("h")
			formatStrC.WriteRune(rune(c2))
		case TypeInt32, TypeUInt32:
			formatStrC.WriteRune(rune(c2))
		case TypeInt64, TypeUInt64:
			formatStrC.WriteString("l")
			formatStrC.WriteRune(rune(c2))
		case TypeCStr:
			formatStrC.WriteString("s")
		case TypeStr:
			formatStrC.WriteString("*s")
		case TypeChar:
			// TODO: add support to print unicode runes as well
			formatStrC.WriteString("lc")
		case TypeFloat32, TypeFloat64:
			formatStrC.WriteString("f")
		default:
			panic(fmt.Errorf("internal error: %s", AstFormat(argType)))
		}
		i++
	}

	if i != len(call.Args) {
		ReportErrorf(sc, call.Args[i], "too many arguments for %s", AstFormat(formatExpr))
		return call
	}

	result := &TcCall{Source: call.Source, Braced: call.Braced}
	result.Sym = &TcProcRef{
		Source:       call.Sym.Source,
		Overloadable: builtinCPrintf,
	}
	result.Args = make([]TcExpr, 1, len(call.Args))
	result.Args[0] = &TcStrLit{Source: formatStrLit.Source, Type: TypeCStr, Value: formatStrC.String()}
	for _, arg := range call.Args[1:] {

		if arg.GetType() == TypeStr {
			// TODO this is incorrect for when the argument has side effects
			lengthExpr := SemCheckExpr(sc, scope, &Call{Callee: &Ident{Source: "i32"}, Args: []Expr{&Call{Callee: &Ident{Source: "len"}, Args: []Expr{arg}}}}, UniqueTypeConstraint{TypeInt32})
			dataExpr := SemCheckExpr(sc, scope, &Call{Callee: &Ident{Source: "cstr"}, Args: []Expr{arg}}, UniqueTypeConstraint{TypeCStr})
			result.Args = append(result.Args, lengthExpr, dataExpr)
		} else {
			result.Args = append(result.Args, arg)
		}
	}
	return result
}

func BuiltinAddLinkerFlags(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 1) {
		return newErrorNode(call)
	}
	switch arg0 := call.Args[0].(type) {
	case *TcStrLit:
		program := scope.CurrentProgram
		program.LinkerFlags = append(program.LinkerFlags, arg0.Value)
	default:
		ReportInvalidAstNode(sc, arg0, "string literal")
		return newErrorNode(call)
	}
	// return empty code block as a substitude for not returning anything
	return &TcCodeBlock{Source: call.Source}
}

func BuiltinAddCFlags(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 1) {
		return newErrorNode(call)
	}
	switch arg0 := call.Args[0].(type) {
	case *TcStrLit:
		scope.CurrentPackage.CFlags = append(scope.CurrentPackage.CFlags, arg0.Value)
	default:
		ReportInvalidAstNode(sc, arg0, "string literal")
		return newErrorNode(call)
	}
	// return empty code block as a substitude for not returning anything
	return &TcCodeBlock{Source: call.Source}
}

func BuiltinPipeTransformation(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 2) {
		return newErrorNode(call)
	}
	// TODO might crash
	lhs := call.Args[0].(*TcWrappedUntypedAst).Expr
	rhs, isCall := call.Args[1].(*TcWrappedUntypedAst).Expr.(*Call)
	if !isCall {
		ReportInvalidAstNode(sc, call.Args[1], "call")
		return newErrorNode(call)
	}

	result := &Call{
		Source: call.Source,
		Callee: rhs.Callee,
		Args:   make([]Expr, 0, len(rhs.Args)+1),
	}

	result.Args = append(result.Args, lhs)
	result.Args = append(result.Args, rhs.Args...)

	return SemCheckExpr(sc, scope, result, TypeUnspecified)
}

func BuiltinDotOperator(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 2) {
		return newErrorNode(call)
	}

	lhs := SemCheckExpr(sc, scope, call.Args[0].(*TcWrappedUntypedAst).Expr, TypeUnspecified)

	result := &TcDotExpr{
		Source: call.Source,
		Lhs:    lhs,
	}
	// fmt.Printf("lhs: %T %+v\n", result.Lhs, result.Lhs)
	typ := result.Lhs.GetType()
	switch t := typ.(type) {
	case *StructType:
		rhs, isIdent := call.Args[1].(*TcWrappedUntypedAst).Expr.(*Ident)
		if !isIdent {
			ReportErrorf(sc, call.Args[1], "right of dot operator needs to be an identifier, but it is %T", call.Args[1])
			return result
		}
		var idx int
		result.Rhs, idx = t.Impl.GetField(rhs.Source)
		if idx < 0 {
			ReportErrorf(sc, rhs, "type %s has no field %s", t.Impl.Name, rhs.GetSource())
			return newErrorNode(call)
		}
		// ExpectType(sc, rhs, result.Rhs.GetType(), expected)
		return result
	case *ErrorType:
		return newErrorNode(call)
	default:
		ReportErrorf(sc, lhs, "dot call is only supported on struct types, but got: %s", AstFormat(typ))
		return newErrorNode(call)
	}
}

func BuiltinColonExpr(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 2) {
		return newErrorNode(call)
	}
	lhs := call.Args[0].(*TcWrappedUntypedAst).Expr
	rhs := call.Args[1].(*TcWrappedUntypedAst).Expr
	typ := LookUpType(sc, scope, rhs)
	// typ = ExpectType(sc, call, typ, expected)
	return SemCheckExpr(sc, scope, lhs, UniqueTypeConstraint{typ})
}

func BuiltinEmitStmt(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 1) {
		return newErrorNode(call)
	}
	switch arg0 := call.Args[0].(type) {
	case *TcStrLit:

		result := &TcEmitExpr{
			Source: call.Source,
			Type:   TypeVoid,
		}

		emitStr := arg0.Value
		for true {
			idx := strings.IndexRune(emitStr, '`')
			if idx < 0 {
				result.EmitSource = emitStr
				return result
			}
			source := emitStr[:idx]
			emitStr = emitStr[idx+1:]
			idx2 := strings.IndexRune(emitStr, '`')
			if idx2 < 0 {
				ReportErrorf(sc, arg0, "unmatching ` in emit source")
				return newErrorNode(call)
			}
			symName := &Ident{Source: emitStr[:idx2]}
			emitStr = emitStr[idx2+1:]
			symRef := LookUpLetSym(sc, scope, symName, TypeUnspecified)
			result.SourceSymPairs = append(result.SourceSymPairs, SymSourcePair{EmitSource: source, Sym: symRef})
		}

		return result
	default:
		ReportInvalidAstNode(sc, arg0, "string literal")
		return newErrorNode(arg0)
	}

}

func BuiltinStaticExpr(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	// TODO: ensure this expression can be evaluated at compile time
	expr := call.Args[0].(*TcWrappedUntypedAst).Expr
	checkedExpr := SemCheckExpr(sc, scope, expr, TypeUnspecified)
	return EvalExpr(sc, checkedExpr, scope)
}

func BuiltinTraitDef(sc *SemChecker, scope Scope, def *TcCall) TcExpr {
	expr := def.Args[0].(*TcWrappedUntypedAst).Expr
	name, dependentTypes, signatures := ParseTraitDef(sc, expr)
	ValidNameCheck(sc, name, "trait")

	result := &TcTraitDef{}
	result.Source = def.Source
	result.Name = name.Source
	traitScope := NewSubScope(scope)
	traitScope.CurrentTrait = result
	trait := RegisterTrait(sc, scope, result, name)

	for _, typ := range dependentTypes {
		// TODO, support setting the Constraint here
		sym := NewGenericTypeSymbol(typ.Source, typ.Source, trait)
		result.DependentTypes = append(result.DependentTypes, sym)
		RegisterType(sc, traitScope, sym.Name, sym, sym)
	}

	for _, procDef := range signatures {
		tcProcDef := SemCheckProcDef(sc, traitScope, procDef)
		result.Overloadables = append(result.Overloadables, tcProcDef)
	}

	return result
}

func BuiltinCastExpr(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	arg1 := call.Args[0]
	arg2 := call.Args[1].(*TcTypeContext)
	// TODO validate that expression can actually be casted. E.g. test if they have the same size and alignment
	return &TcCastExpr{
		Source: call.Source,
		Expr:   arg1,
		Type:   arg2.WrappedType,
	}
}

func BuiltinConvExpr(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	arg1 := call.Args[0]
	arg2 := call.Args[1].(*TcTypeContext)
	// TODO validate that expression can actually be converted. e.g. both types are compatible number types
	return &TcConvExpr{
		Source: call.Source,
		Expr:   arg1,
		Type:   arg2.WrappedType,
	}
}

func BuiltinImportStmt(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 1) {
		return newErrorNode(call)
	}
	var strLit *TcStrLit
	switch arg0 := call.Args[0].(type) {
	case *TcStrLit:
		strLit = arg0
	default:
		ReportInvalidAstNode(sc, arg0, "string literal")
		return newErrorNode(arg0)
	}

	pkg, err := GetPackage(scope.CurrentProgram, scope.CurrentPackage.WorkDir, strLit.Value)
	if err != nil {
		ReportErrorf(sc, strLit, "%s", err.Error())
	} else {
		for key, value := range pkg.ExportScope.Variables {
			// TODO solution for conflicts and actually find out where the conflicting symbol comes from.
			_, hasKey := scope.Variables[key]
			if hasKey {
				ReportErrorf(sc, strLit, "name conflicts in imported variable not yet implemented: %s.%s conflicts with some other symbol", strLit.Value, key)
			}
			scope.Variables[key] = value
		}
		for key, value := range pkg.ExportScope.Types {
			_, hasKey := scope.Types[key]
			if hasKey {
				ReportErrorf(sc, strLit, "name conflicts in imported type not yet implemented: %s.%s conflicts with some other symbol", strLit.Value, key)
			}
			scope.Types[key] = value
		}
		for key, value := range pkg.ExportScope.Overloadables {
			procs, _ := scope.Overloadables[key]
			scope.Overloadables[key] = append(procs, value...)
		}
	}
	return &TcCodeBlock{Source: call.Source}
}

func BuiltinTypeExpr(sc *SemChecker, scope Scope, call *TcCall) TcExpr {
	expr := call.Args[0].(*TcWrappedUntypedAst).Expr
	return (TcExpr)(&TcTypeContext{
		Source:      call.Source,
		WrappedType: LookUpType(sc, scope, expr),
	})
}

func init() {
	openGenericsMap = make(map[Type][]*GenericTypeSymbol)
	arrayTypeMap = make(map[ArrayTypeMapKey]*ArrayType)
	enumSetTypeMap = make(map[*EnumType]*EnumSetType)
	ptrTypeMap = make(map[Type]*PtrType)
	typeTypeMap = make(map[Type]*TypeType)
	packageMap = make(map[string]*TcPackageDef)
	intLitTypeMap = make(map[string]*IntLitType)
	floatLitTypeMap = make(map[string]*FloatLitType)
	stringLitTypeMap = make(map[string]*StringLitType)

	// Printf is literally the only use case for real varargs that I actually see as
	// practical. Therefore the implementation for varargs will be strictly tied to
	// printf for now. A general concept for varargs will be specified out as soon
	// as it becomes necessary, but right now it is not planned.
	builtinCPrintf = registerBuiltin("c_printf", "printf(", ", ", ")", []Type{TypeCStr}, TypeVoid, 0)
	builtinCPrintf.GetSignature().Varargs = true

	registerBuiltinMacro("printf", true, []Type{TypeStr}, TypeVoid, ValidatePrintfCall)
	registerBuiltinMacro("addCFlags", false, []Type{TypeStr}, TypeVoid, BuiltinAddCFlags)
	registerBuiltinMacro("addLinkerFlags", false, []Type{TypeStr}, TypeVoid, BuiltinAddLinkerFlags)
	registerBuiltinMacro("emit", false, []Type{TypeStr}, TypeVoid, BuiltinEmitStmt)
	registerBuiltinMacro("import", false, []Type{TypeStr}, TypeVoid, BuiltinImportStmt)
	registerBuiltinMacro("type", false, []Type{TypeUntyped}, TypeUntyped, BuiltinTypeExpr)

	registerBuiltinMacro("|", false, []Type{TypeUntyped, TypeUntyped}, TypeUntyped, BuiltinPipeTransformation)
	registerBuiltinMacro(".", false, []Type{TypeUntyped, TypeUntyped}, TypeUntyped, BuiltinDotOperator)
	registerBuiltinMacro(":", false, []Type{TypeUntyped, TypeUntyped}, TypeUntyped, BuiltinColonExpr)

	registerBuiltinMacro("static", false, []Type{TypeUntyped}, TypeUntyped, BuiltinStaticExpr)
	registerBuiltinMacro("trait", false, []Type{TypeUntyped}, TypeVoid, BuiltinTraitDef)

	{
		// TODO: has no line information
		T := NewGenericTypeSymbol("", "T", TypeUnspecified)
		U := NewGenericTypeSymbol("", "U", TypeUnspecified)
		registerGenericBuiltinMacro("cast", false, []*GenericTypeSymbol{T, U}, []Type{T, GetTypeType(U)}, U, BuiltinCastExpr)
		registerGenericBuiltinMacro("conv", false, []*GenericTypeSymbol{T, U}, []Type{T, GetTypeType(U)}, U, BuiltinConvExpr)
	}

	// registerBuiltinMacro("sizeof", false, []Type{TypeUnspecified}, TypeInt64, BuiltinSizeOf)

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
	registerBuiltinType(TypeStr)
	registerBuiltinType(TypeCStr)
	registerBuiltinType(TypeChar)
	registerBuiltinType(TypeVoid)
	registerBuiltinType(TypeNoReturn)
	registerBuiltinType(TypeNilPtr)

	registerBuiltinType(TypeFloat32x4)

	// type aliases
	RegisterType(nil, builtinScope, "pointer", GetPtrType(TypeVoid), nil)
	RegisterType(nil, builtinScope, "int", TypeInt64, nil)
	RegisterType(nil, builtinScope, "uint", TypeUInt64, nil)

	RegisterTypeGroup(nil, builtinScope, TypeAnyFloat, nil)
	RegisterTypeGroup(nil, builtinScope, TypeAnyInt, nil)
	RegisterTypeGroup(nil, builtinScope, TypeAnyUInt, nil)
	RegisterTypeGroup(nil, builtinScope, TypeAnySInt, nil)
	RegisterTypeGroup(nil, builtinScope, TypeAnyNumber, nil)
	RegisterTypeGroup(nil, builtinScope, TypeAnyString, nil)

	for _, typ := range TypeAnyNumber.Items {
		registerBuiltin("+", "(", "+", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("-", "(", "-", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("*", "(", "*", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("/", "(", "/", ")", []Type{typ, typ}, typ, 0)

		registerBuiltin("-", " -", "", "", []Type{typ}, typ, 0)
		registerBuiltin("+", " +", "", "", []Type{typ}, typ, 0)

		registerBuiltin("<", "(", "<", ")", []Type{typ, typ}, TypeBoolean, 0)
		registerBuiltin("<=", "(", "<=", ")", []Type{typ, typ}, TypeBoolean, 0)
		registerBuiltin(">", "(", ">", ")", []Type{typ, typ}, TypeBoolean, 0)
		registerBuiltin(">=", "(", ">=", ")", []Type{typ, typ}, TypeBoolean, 0)

		for _, op := range []string{"+=", "-=", "*=", "/="} {
			registerBuiltin(op, "(", op, ")", []Type{typ, typ}, TypeVoid, 1)
		}

		// TODO this should be a generic with proper working type constraint on all builtin number types
		// maybe it should be implemented with the builtin `conv` macro that exists now.
		registerBuiltin("i8", "(int8_t)(", "", ")", []Type{typ}, TypeInt8, 0)
		registerBuiltin("i16", "(int16_t)(", "", ")", []Type{typ}, TypeInt16, 0)
		registerBuiltin("i32", "(int32_t)(", "", ")", []Type{typ}, TypeInt32, 0)
		registerBuiltin("i64", "(int64_t)(", "", ")", []Type{typ}, TypeInt64, 0)
		registerBuiltin("int", "(int64_t)(", "", ")", []Type{typ}, TypeInt64, 0)

		registerBuiltin("u8", "(uint8_t)(", "", ")", []Type{typ}, TypeUInt8, 0)
		registerBuiltin("u16", "(uint16_t)(", "", ")", []Type{typ}, TypeUInt16, 0)
		registerBuiltin("u32", "(uint32_t)(", "", ")", []Type{typ}, TypeUInt32, 0)
		registerBuiltin("u64", "(uint64_t)(", "", ")", []Type{typ}, TypeUInt64, 0)
		registerBuiltin("uint", "(uint64_t)(", "", ")", []Type{typ}, TypeUInt64, 0)

		registerBuiltin("f32", "(float)(", "", ")", []Type{typ}, TypeFloat32, 0)
		registerBuiltin("f64", "(double)(", "", ")", []Type{typ}, TypeFloat64, 0)

		if intType, isIntType := typ.(*BuiltinIntType); isIntType {
			// TODO, invalid source location
			argSym := &TcSymbol{Source: "arg", Kind: SkProcArg, Type: GetTypeType(typ)}
			registerSimpleTemplate("high", []*TcSymbol{argSym}, typ, &IntLit{Value: big.NewInt(0).Set(intType.MaxValue)})
			registerSimpleTemplate("low", []*TcSymbol{argSym}, typ, &IntLit{Value: big.NewInt(0).Set(intType.MinValue)})
		}

	}

	{
		// TODO, invalid source location
		arg1 := &TcSymbol{Source: "arg1", Kind: SkProcArg, Type: TypeUntyped}
		arg2 := &TcSymbol{Source: "arg2", Kind: SkProcArg, Type: TypeUntyped}

		inExpr := &Call{Callee: &Ident{Source: "contains"}, Args: []Expr{arg2, arg1}}
		notInExpr := &Call{Callee: &Ident{Source: "not"}, Args: []Expr{inExpr}}

		registerSimpleTemplate("in", []*TcSymbol{arg1, arg2}, TypeBoolean, inExpr)
		registerSimpleTemplate("notin", []*TcSymbol{arg1, arg2}, TypeBoolean, notInExpr)
	}

	// TODO test these bitops
	// TODO put all bitops in their own module
	//   * allow varargs
	for _, typ := range TypeAnyInt.Items {
		registerBuiltin("bitand", "(", "&", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("bitor", "(", "|", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("bitxor", "(", "^", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("bitnot", "~(", "", ")", []Type{typ}, typ, 0)
		registerBuiltin("shiftleft", "(", "<<", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("shiftright", "(", ">>", ")", []Type{typ, typ}, typ, 0)
	}

	registerBuiltin("len", "", "", ".len", []Type{TypeStr}, TypeInt64, 0)
	registerBuiltin("cstr", "", "", ".data", []Type{TypeStr}, TypeCStr, 0)

	// vector types
	for _, typ := range []Type{TypeFloat32x4} {
		registerBuiltin("+", "(", "+", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("-", "(", "-", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("*", "(", "*", ")", []Type{typ, typ}, typ, 0)
		registerBuiltin("/", "(", "/", ")", []Type{typ, typ}, typ, 0)

		registerBuiltin("<", "(", "<", ")", []Type{typ, typ}, TypeBoolean, 0)
		registerBuiltin("<=", "(", "<=", ")", []Type{typ, typ}, TypeBoolean, 0)
		registerBuiltin(">", "(", ">", ")", []Type{typ, typ}, TypeBoolean, 0)
		registerBuiltin(">=", "(", ">=", ")", []Type{typ, typ}, TypeBoolean, 0)
	}

	{
		// TODO: has no line information
		T := NewGenericTypeSymbol("", "T", TypeUnspecified)

		builtinAddr = registerGenericBuiltin("addr", "&(", "", ")", []*GenericTypeSymbol{T}, []Type{T}, GetPtrType(T), 1)

		// TODO mark argument as mutable
		builtinDeref = registerGenericBuiltin("indexOp", "*(", "", ")", []*GenericTypeSymbol{T}, []Type{GetPtrType(T)}, T, 0)

		registerGenericBuiltin("=", "(", "=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeVoid, 1)
		registerGenericBuiltin("==", "(", "==", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean, 0)
		registerGenericBuiltin("!=", "(", "!=", ")", []*GenericTypeSymbol{T}, []Type{T, T}, TypeBoolean, 0)

		registerGenericBuiltin("pointer", "(void*)(", "", ")", []*GenericTypeSymbol{T}, []Type{GetPtrType(T)}, GetPtrType(TypeVoid), 0)
		registerGenericBuiltin("sizeof", "sizeof(", "", ")", []*GenericTypeSymbol{T}, []Type{T}, TypeInt64, 0)
		registerGenericBuiltin("discard", "", "", "", []*GenericTypeSymbol{T}, []Type{T}, TypeVoid, 0)
	}

	registerBuiltin("and", "(", "&&", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean, 0)
	registerBuiltin("or", "(", "||", ")", []Type{TypeBoolean, TypeBoolean}, TypeBoolean, 0)
	registerBuiltin("not", "!(", "", ")", []Type{TypeBoolean}, TypeBoolean, 0)

	registerBuiltin("assert", "assert(", "", ")", []Type{TypeBoolean}, TypeVoid, 0)

	registerConstant("true", &TcIntLit{Type: TypeBoolean, Value: big.NewInt(1)})
	registerConstant("false", &TcIntLit{Type: TypeBoolean, Value: big.NewInt(0)})

}
