package main

import (
	. "fmt"
)

func LookUpType(expr TypeExpr) TypeHandle {
	// TODO really slow lookup, should really be faster
	name := expr.Ident
	for i, typ := range builtinTypes {
		if typ.Name == name {
			return TypeHandle(i)
		}
	}
	panic(Sprintf("type not found %s", name))
	//return TypeHandle(-1)
}

func TypeCheckStructDef(def StructDef) TcStructDef {
	var result TcStructDef
	result.Name = def.Name
	for _, field := range def.Fields {
		var tcField TcStructField
		tcField.Name = field.Name
		tcField.Type = LookUpType(field.Type)
	}
	return result
}

func TypeCheckProcDef(def ProcDef) TcProcDef {
	var result TcProcDef
	result.ResultType = LookUpType(def.ResultType)

	for _, arg := range def.Args {
		var tcArg TcProcArgument
		tcArg.Name = arg.Name
		tcArg.Type = LookUpType(arg.Type)
		result.Args = append(result.Args, tcArg)
	}
	return result
}

func TypeCheckPackage(arg PackageDef) TcPackageDef {
	var result TcPackageDef
	result.Name = arg.Name
	for _, typeDef := range arg.TypeDefs {
		result.TypeDefs = append(result.TypeDefs, TypeCheckStructDef(typeDef))
	}
	for _, procDef := range arg.ProcDefs {
		result.ProcDefs = append(result.ProcDefs, TypeCheckProcDef(procDef))
	}
  return result
}
