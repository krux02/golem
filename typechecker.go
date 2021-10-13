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
	Args: []TcProcArgument{
		TcProcArgument{Type: TypeInt},
		TcProcArgument{Type: TypeInt},
	},
	ResultType: TypeInt,
}

var BuiltinMinus *TcProcDef = &TcProcDef{
	Name: "-",
	Args: []TcProcArgument{
		TcProcArgument{Type: TypeInt},
		TcProcArgument{Type: TypeInt},
	},
	ResultType: TypeInt,
}

var BuiltinMult *TcProcDef = &TcProcDef{
	Name: "*",
	Args: []TcProcArgument{
		TcProcArgument{Type: TypeInt},
		TcProcArgument{Type: TypeInt},
	},
	ResultType: TypeInt,
}

var BuiltinDivide *TcProcDef = &TcProcDef{
	Name: "/",
	Args: []TcProcArgument{
		TcProcArgument{Type: TypeFloat},
		TcProcArgument{Type: TypeFloat},
	},
	ResultType: TypeFloat,
}

var BuiltinReturn *TcProcDef = &TcProcDef{
	Name:       "return",
	ResultType: TypeNoReturn,
}

var BuiltinPrintf *TcProcDef = &TcProcDef{
	Name: "printf",
	Args: []TcProcArgument{
		TcProcArgument{Name: "format", Type: TypeString},
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

type ScopeImpl struct {
	Parent     Scope
	Variables  map[string]TcSymbol
	Procedures map[string]*TcProcDef
	Types      map[string]Type
}

type Scope = *ScopeImpl

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

func (scope Scope) LookUpProc(sym Symbol) TcProcSymbol {
	if scope == nil {
		panic(fmt.Sprintf("Proc not found: %s", sym.Name))
	}

	if impl, ok := scope.Procedures[sym.Name]; ok {
		return TcProcSymbol{Name: sym.Name, Impl: impl}
	}
	return scope.Parent.LookUpProc(sym)
}

func (scope Scope) LookUpSym(sym Symbol) TcSymbol {
	var result TcSymbol
	result.Name = sym.Name
	result.typ = scope.Types[sym.Name]
	return result
}

func TypeCheckStructDef(scope Scope, def StructDef) TcStructDef {
	var result TcStructDef
	result.Name = def.Name
	for _, field := range def.Fields {
		var tcField TcStructField
		tcField.Name = field.Name
		tcField.Type = scope.LookUpType(field.Type)
		result.Fields = append(result.Fields, tcField)
	}
	return result
}

func TypeCheckProcDef(scope Scope, def ProcDef) TcProcDef {
	var result TcProcDef
	result.Name = def.Name
	for _, arg := range def.Args {
		var tcArg TcProcArgument
		tcArg.Name = arg.Name
		tcArg.Type = scope.LookUpType(arg.Type)
		result.Args = append(result.Args, tcArg)
	}
	resultType := scope.LookUpType(def.ResultType)
	result.ResultType = resultType

	result.Body = TypeCheckExpr(scope, def.Body, resultType)

	return result
}

func ExpectType(gotten, expected Type) {
	if expected != gotten {
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
		expectedType := expectedArgs[i].Type
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
		if expected != TypeVoid {

		}
	}
	return result
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

func TypeCheckExpr(scope Scope, arg Expr, expected Type) TcExpr {
	var result TcExpr
	switch arg := arg.(type) {
	case Call:
		return (TcExpr)(TypeCheckCall(scope, arg, expected))
	case CodeBlock:
		return (TcExpr)(TypeCheckCodeBlock(scope, arg, expected))
	case Symbol:
		return (TcExpr)(scope.LookUpSym(arg))
	case StrLit:
		return (TcExpr)(arg)
	case IntLit:
		return (TcExpr)(arg)
	default:
		panic(fmt.Sprintf("not implemented %T", arg))
	}

	// TODO not implemented
	return result
}

func TypeCheckPackage(arg PackageDef) TcPackageDef {
	var result TcPackageDef
	scope := &ScopeImpl{Parent: builtinScope}
	result.Name = arg.Name
	for _, typeDef := range arg.TypeDefs {
		result.TypeDefs = append(result.TypeDefs, TypeCheckStructDef(scope, typeDef))
	}
	for _, procDef := range arg.ProcDefs {
		result.ProcDefs = append(result.ProcDefs, TypeCheckProcDef(scope, procDef))
	}
	return result
}
