package main

import (
	"fmt"
)

type BuiltinType struct {
	name string
}

func (typ *BuiltinType) Name() string {
	return typ.name
}

// **** Constants ****

var TypeInt = &BuiltinType{"int"}
var TypeFloat = &BuiltinType{"float"}
var TypeString = &BuiltinType{"string"}
var TypeVoid = &BuiltinType{"void"}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn"}

// this type is the internal representation when no type has been
// specified. It is not a type by its own.
var TypeUnspecified = &BuiltinType{"<unspecified>"}

var BuiltinPlus *TcProcDef = &TcProcDef{
	Name: "+",
	Args: []TcLetSymbol{
		TcLetSymbol{Typ: TypeInt},
		TcLetSymbol{Typ: TypeInt},
	},
	ResultType: TypeInt,
}

var BuiltinMinus *TcProcDef = &TcProcDef{
	Name: "-",
	Args: []TcLetSymbol{
		TcLetSymbol{Typ: TypeInt},
		TcLetSymbol{Typ: TypeInt},
	},
	ResultType: TypeInt,
}

var BuiltinMult *TcProcDef = &TcProcDef{
	Name: "*",
	Args: []TcLetSymbol{
		TcLetSymbol{Typ: TypeInt},
		TcLetSymbol{Typ: TypeInt},
	},
	ResultType: TypeInt,
}

var BuiltinDivide *TcProcDef = &TcProcDef{
	Name: "/",
	Args: []TcLetSymbol{
		TcLetSymbol{Typ: TypeFloat},
		TcLetSymbol{Typ: TypeFloat},
	},
	ResultType: TypeFloat,
}

var BuiltinReturn *TcProcDef = &TcProcDef{
	Name:       "return",
	ResultType: TypeNoReturn,
}

var BuiltinPrintf *TcProcDef = &TcProcDef{
	Name: "printf",
	Args: []TcLetSymbol{
		TcLetSymbol{Name: "format", Typ: TypeString},
		// TODO support argument list
	},
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
		"+":      BuiltinPlus,
		"-":      BuiltinMinus,
		"*":      BuiltinMult,
		"/":      BuiltinDivide,
		"return": BuiltinReturn,
		"printf": BuiltinPrintf,
	},
}

// ****

// index to refere to a (currently only builtin) type
// somehow unify this mess
type ScopeImpl struct {
	Parent     Scope
	Variables  map[string]TcLetSymbol
	Procedures map[string]*TcProcDef
	Types      map[string]Type
}

type Scope = *ScopeImpl

func (scope Scope) NewSubScope() Scope {
	return &ScopeImpl{
		Parent:     scope,
		Variables:  make(map[string]TcLetSymbol),
		Procedures: make(map[string]*TcProcDef),
		Types:      make(map[string]Type),
	}
}

func (scope Scope) NewLetSym(name string, typ Type) TcLetSymbol {
	result := TcLetSymbol{Name: name, Typ: typ}
	scope.Variables[name] = result
	return result
}

func (scope Scope) LookUpType(expr TypeExpr) Type {
	// TODO really slow lookup, should really be faster
	name := expr.Ident
	for key, value := range scope.Types {
		if key == name {
			return value
		}
	}
	if scope.Parent != nil {
		return scope.Parent.LookUpType(expr)
	}
	panic(fmt.Sprintf("Type not found: %s", name))
}

func (scope Scope) LookUpProc(ident Ident) TcProcSymbol {
	if scope == nil {
		panic(fmt.Sprintf("Proc not found: %s", ident.Name))
	}

	if impl, ok := scope.Procedures[ident.Name]; ok {
		return TcProcSymbol{Name: ident.Name, Impl: impl}
	}
	return scope.Parent.LookUpProc(ident)
}

func (scope Scope) LookUpLetSym(ident Ident) TcLetSymbol {
	if scope == nil {
		panic(fmt.Sprintf("let sym not found: %s", ident.Name))
	}

	if sym, ok := scope.Variables[ident.Name]; ok {
		return sym
	}
	return scope.Parent.LookUpLetSym(ident)
}

func TypeCheckStructDef(scope Scope, def StructDef) TcStructDef {
	var result TcStructDef
	result.Name = def.Name
	for _, field := range def.Fields {
		var tcField TcStructField
		tcField.Name = field.Name
		tcField.Type = scope.LookUpType(field.TypeExpr)
		result.Fields = append(result.Fields, tcField)
	}
	return result
}

func TypeCheckProcDef(parentScope Scope, def ProcDef) TcProcDef {
	scope := parentScope.NewSubScope()
	var result TcProcDef
	result.Name = def.Name
	for _, arg := range def.Args {
		tcArg := scope.NewLetSym(arg.Name, scope.LookUpType(arg.Type))
		result.Args = append(result.Args, tcArg)
	}
	resultType := scope.LookUpType(def.ResultType)
	result.ResultType = resultType
	result.Body = TypeCheckExpr(scope, def.Body, resultType)

	// TODO this is very ugly, store a pointer to a local, return a copy
	parentScope.Procedures[result.Name] = &result
	return result
}

func ExpectType(gotten, expected Type) {
	if expected != TypeUnspecified && expected != gotten {
		// TODO print proper line information here
		panic(fmt.Sprintf("Expected type '%s' but got type '%s'", expected.Name(), gotten.Name()))
	}
}

func ExpectArgsLen(gotten, expected int) {
	if expected != gotten {
		panic(fmt.Sprintf("Expected %d arguments, but got %d.",
			expected, gotten))
	}
}

func TypeCheckCall(scope Scope, call Call, expected Type) TcCall {
	var result TcCall
	procSym := scope.LookUpProc(call.Sym)
	ExpectType(procSym.Impl.ResultType, expected)
	result.Sym = procSym
	result.Args = make([]TcExpr, 0, len(call.Args))
	expectedArgs := procSym.Impl.Args
	ExpectArgsLen(len(call.Args), len(expectedArgs))
	if len(expectedArgs) != len(call.Args) {
		// TODO print proper line information here

	}
	for i, arg := range call.Args {
		expectedType := expectedArgs[i].Typ
		tcArg := TypeCheckExpr(scope, arg, expectedType)
		result.Args = append(result.Args, tcArg)
	}

	return result
}

func TypeCheckCodeBlock(scope Scope, arg CodeBlock, expected Type) TcCodeBlock {
	var result TcCodeBlock
	N := len(arg.Items)
	if N > 0 {
		result.Items = make([]TcExpr, N)
		for i, item := range arg.Items {
			if i == N-1 {
				result.Items[i] = TypeCheckExpr(scope, item, expected)
			} else {
				result.Items[i] = TypeCheckExpr(scope, item, TypeVoid)
			}
		}
	} else {
		// empty block is type void
		ExpectType(TypeVoid, expected)
	}
	return result
}

func (expr TypeExpr) IsSet() bool {
	return expr.Ident != ""
}

func TypeCheckLetStmt(scope Scope, arg LetStmt) TcLetStmt {
	var expected Type = TypeUnspecified
	if arg.TypeExpr.IsSet() {
		expected = scope.LookUpType(arg.TypeExpr)
	}

	var result TcLetStmt
	result.Value = TypeCheckExpr(scope, arg.Value, expected)
	result.Sym = scope.NewLetSym(arg.Name, result.Value.Type())
	return result
}

func TypeCheckReturnStmt(scope Scope, arg ReturnStmt) TcReturnStmt {
	return TcReturnStmt{TypeCheckExpr(scope, arg.Value, TypeUnspecified)}
}

func (block TcCodeBlock) Type() Type {
	if len(block.Items) == 0 {
		return TypeVoid
	}
	return block.Items[len(block.Items)-1].Type()
}

func (call TcCall) Type() Type {
	return call.Sym.Impl.ResultType
}

func (lit StrLit) Type() Type {
	return TypeString
}

func (lit IntLit) Type() Type {
	return TypeInt
}

func (sym TcLetSymbol) Type() Type {
	return sym.Typ
}

func (sym TcLetStmt) Type() Type {
	return TypeVoid
}

func (returnStmt TcReturnStmt) Type() Type {
	return TypeNoReturn
}

func TypeCheckExpr(scope Scope, arg Expr, expected Type) TcExpr {
	var result TcExpr
	switch arg := arg.(type) {
	case Call:
		return (TcExpr)(TypeCheckCall(scope, arg, expected))
	case CodeBlock:
		return (TcExpr)(TypeCheckCodeBlock(scope, arg, expected))
	case Ident:
		sym := scope.LookUpLetSym(arg)
		ExpectType(sym.Typ, expected)
		return (TcExpr)(sym)
	case StrLit:
		ExpectType(TypeString, expected)
		return (TcExpr)(arg)
	case IntLit:
		ExpectType(TypeInt, expected)
		return (TcExpr)(arg)
	case ReturnStmt:
		// ignoring expected type here, because the return expression
		// never evaluates to anything.
		return (TcExpr)(TypeCheckReturnStmt(scope, arg))
	case LetStmt:
		ExpectType(TypeVoid, expected)
		return (TcExpr)(TypeCheckLetStmt(scope, arg))
	default:
		panic(fmt.Sprintf("not implemented %T", arg))
	}

	// TODO not implemented
	return result
}

func TypeCheckPackage(arg PackageDef) TcPackageDef {
	var result TcPackageDef
	scope := builtinScope.NewSubScope()
	result.Name = arg.Name
	for _, typeDef := range arg.TypeDefs {
		result.TypeDefs = append(result.TypeDefs, TypeCheckStructDef(scope, typeDef))
	}
	for _, procDef := range arg.ProcDefs {
		result.ProcDefs = append(result.ProcDefs, TypeCheckProcDef(scope, procDef))
	}
	return result
}
