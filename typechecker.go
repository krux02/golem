package main


type Type interface {
	Name() string
}

type BuiltinType struct {
	name string
}

func (typ *BuiltinType) Name() string {
	return typ.name
}

var builtinScope = &Scope{
	Parent: nil,
	Types: map[string]Type{
    "int": &BuiltinType{"int"},
	  "float": &BuiltinType{"float"},
	  "string": &BuiltinType{"string"},
	},
}

// index to refere to a (currently only builtin) type
type TypeHandle Type

type Scope struct {
	Parent *Scope
	Variables map[string]Symbol
	Procedures map[string]ProcDef
	Types map[string]Type
}

func (scope *Scope) LookUpType(expr TypeExpr) TypeHandle {
	// TODO really slow lookup, should really be faster
	name := expr.Ident


	for key, value := range scope.Types {
		if key == name {
			return TypeHandle(value)
		}
	}

	panic("Type not found")
}

func TypeCheckStructDef(scope *Scope, def StructDef) TcStructDef {
	var result TcStructDef
	result.Name = def.Name
	for _, field := range def.Fields {
		var tcField TcStructField
		tcField.Name = field.Name
		tcField.Type = scope.LookUpType(field.Type)
	}
	return result
}

func TypeCheckProcDef(scope *Scope, def ProcDef) TcProcDef {
	var result TcProcDef
	result.ResultType = scope.LookUpType(def.ResultType)

	for _, arg := range def.Args {
		var tcArg TcProcArgument
		tcArg.Name = arg.Name
		tcArg.Type = scope.LookUpType(arg.Type)
		result.Args = append(result.Args, tcArg)
	}
	result.Body = TypeCheckExpr(def.Body)
	return result
}

func TypeCheckExpr(arg Expr) TcExpr {
	var result TcExpr
	// TODO not implemented
	return result
}

func TypeCheckPackage(arg PackageDef) TcPackageDef {
	var result TcPackageDef
	scope := &Scope{Parent: builtinScope}
	result.Name = arg.Name
	for _, typeDef := range arg.TypeDefs {
		result.TypeDefs = append(result.TypeDefs, TypeCheckStructDef(scope, typeDef))
	}
	for _, procDef := range arg.ProcDefs {
		result.ProcDefs = append(result.ProcDefs, TypeCheckProcDef(scope, procDef))
	}
  return result
}
