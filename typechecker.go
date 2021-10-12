package main

import (
	"fmt"
)

type Type interface {
	Name() string
}

type BuiltinType struct {
	name string
}

func (typ *BuiltinType) Name() string {
	return typ.name
}

var TypeInt = &BuiltinType{"int"}
var TypeFloat = &BuiltinType{"float"}
var TypeString = &BuiltinType{"string"}
var TypeVoid = &BuiltinType{"void"}
var TypeNoReturn = &BuiltinType{"noreturn"}

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
		"+":      nil,
		"-":      nil,
		"*":      nil,
		"/":      nil,
		"return": nil,
		"printf": nil,
	},
}

// index to refere to a (currently only builtin) type
type TypeHandle Type

type ScopeImpl struct {
	Parent     Scope
	Variables  map[string]TcSymbol
	Procedures map[string]*TcProcDef
	Types      map[string]Type
}

type Scope = *ScopeImpl

func (scope Scope) LookUpType(expr TypeExpr) TypeHandle {
	// TODO really slow lookup, should really be faster
	name := expr.Ident
	for key, value := range scope.Types {
		if key == name {
			return TypeHandle(value)
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
	result.ResultType = scope.LookUpType(def.ResultType)

	for _, arg := range def.Args {
		var tcArg TcProcArgument
		tcArg.Name = arg.Name
		tcArg.Type = scope.LookUpType(arg.Type)
		result.Args = append(result.Args, tcArg)
	}

	result.Body = TypeCheckExpr(scope, def.Body)
	return result
}

func TypeCheckCall(scope Scope, call Call) TcCall {
	var result TcCall
	result.Sym = scope.LookUpProc(call.Sym)
	result.Args = make([]TcExpr, 0, len(call.Args))
	for _, arg := range call.Args {
		tcArg := TypeCheckExpr(scope, arg)
		result.Args = append(result.Args, tcArg)
	}
	return result
}

func TypeCheckCodeBlock(scope Scope, arg CodeBlock) TcCodeBlock {
	var result TcCodeBlock
	result.Items = make([]TcExpr, 0, len(arg.Items))
	for _, item := range arg.Items {
		result.Items = append(result.Items, TypeCheckExpr(scope, item))
	}
	return result
}

func (block TcCodeBlock) Type() TypeHandle {
	if len(block.Items) == 0 {
		return TypeVoid
	}
	return block.Items[len(block.Items)-1].Type()
}

func (call TcCall) Type() TypeHandle {
	return call.Sym.Impl.ResultType
}

func (strLit StrLit) Type() TypeHandle {
	return TypeString
}

func TypeCheckExpr(scope Scope, arg Expr) TcExpr {
	var result TcExpr
	switch arg := arg.(type) {
	case Call:
		return (TcExpr)(TypeCheckCall(scope, arg))
	case CodeBlock:
		return (TcExpr)(TypeCheckCodeBlock(scope, arg))
	case Symbol:
		return (TcExpr)(scope.LookUpSym(arg))
	case StrLit:
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
