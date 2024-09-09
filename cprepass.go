package main

import "fmt"

func newAddrExpr(arg TcExpr) TcExpr {
	sym := TcProcSymbol{
		Source:    "", // no source possible, hidden
		Signature: builtinAddr,
	}
	call := &TcCall{
		Source: "", // no source possible, hidden
		Sym:    sym,
		Args:   []TcExpr{arg},
	}
	return call
}

func maybeDerefParam(expr TcExpr, isVarParam bool) TcExpr {
	if isVarParam {
		return newAddrExpr(expr)
	}
	if _, isStruct := expr.GetType().(*StructType); isStruct {
		return newAddrExpr(expr)
	}
	return expr
}

func maybeUnrefParamSym(sym *TcSymRef) TcExpr {
	switch sym.Sym.Kind {
	case SkProcArg:
		switch sym.Type.(type) {
		case *StructType:
			hiddenSym := TcProcSymbol{
				Source:    "",
				Signature: builtinDeref,
			}
			call := &TcCall{
				Source: "", // no source possible, hidden
				Sym:    hiddenSym,
				Args:   []TcExpr{sym},
			}
			return call
		}
	}
	return sym
}

func cgenprepassSlice(expr []TcExpr) []TcExpr {
	for i, it := range expr {
		expr[i] = cgenprepass(it)
	}
	return expr
}

func cgenprepassSignature(sig *Signature) *Signature {
	for i, param := range sig.Params {
		if t, isStruct := param.Type.(*StructType); isStruct {
			newType := GetPtrType(t)
			sig.Params[i].Type = newType
		}
	}
	return sig
}

// this compilation pass should transform code for C code generation. At the
// time of writing, this pass should transform structs to pointers when passed
// down to functions, so that they are passed down by reference.
func cgenprepass(expr TcExpr) TcExpr {
	switch expr := expr.(type) {
	case *TcCall:
		if _, isBuiltin := expr.Sym.Signature.Impl.(*TcBuiltinProcDef); isBuiltin {
			return expr
		}
		sig := expr.Sym.Signature
		newArgs := make([]TcExpr, len(expr.Args))
		for i, arg := range expr.Args {
			newArg := cgenprepass(arg)
			var isVarParam bool = false
			if !sig.Varargs || i < len(sig.Params) {
				isVarParam = sig.Params[i].Kind == SkVarProcArg
			}
			newArgs[i] = maybeDerefParam(newArg, isVarParam)
		}
		expr.Args = newArgs
		return expr
	case *TcSymbol:
		return expr
	case *TcSymRef:
		return maybeUnrefParamSym(expr)
	case *TcDotExpr:
		return &TcDotExpr{Source: expr.Source,
			Lhs: cgenprepass(expr.Lhs),
			Rhs: cgenprepass(expr.Rhs).(*TcStructField),
		}
	case *TcVariableDefStmt:
		return &TcVariableDefStmt{
			Source: expr.Source,
			Sym:    expr.Sym,
			Value:  cgenprepass(expr.Value),
		}
	case *TcReturnExpr:
		return &TcReturnExpr{
			Source: expr.Source,
			Value:  cgenprepass(expr.Value),
		}
	case *TcForLoopStmt:
		return &TcForLoopStmt{
			Source:     expr.Source,
			LoopSym:    expr.LoopSym,
			Collection: cgenprepass(expr.Collection),
			Body:       cgenprepass(expr.Body),
		}
	case *TcWhileLoopStmt:
		return &TcWhileLoopStmt{
			Source:    expr.Source,
			Condition: cgenprepass(expr.Condition),
			Body:      cgenprepass(expr.Body),
		}
	case *TcIfStmt:
		return &TcIfStmt{
			Source:    expr.Source,
			Condition: cgenprepass(expr.Condition),
			Body:      cgenprepass(expr.Body),
		}
	case *TcIfElseExpr:
		return &TcIfElseExpr{
			Source:    expr.Source,
			Condition: cgenprepass(expr.Condition),
			Body:      cgenprepass(expr.Body),
			Else:      cgenprepass(expr.Else),
		}
	case *TcCodeBlock:
		return &TcCodeBlock{
			Source: expr.Source,
			Items:  cgenprepassSlice(expr.Items),
		}
	case *TcIntLit:
		return expr
	case *TcFloatLit:
		return expr
	case *TcStrLit:
		return expr
	case *NilLit:
		return expr
	case *TcArrayLit:
		return &TcArrayLit{
			Source:   expr.Source,
			Items:    cgenprepassSlice(expr.Items),
			ElemType: expr.ElemType,
		}
	case *TcEnumSetLit:
		return &TcEnumSetLit{
			Source:   expr.Source,
			Items:    cgenprepassSlice(expr.Items),
			ElemType: expr.ElemType,
		}
	case *TcProcDef:
		return &TcProcDef{
			Source:      expr.Source,
			MangledName: expr.MangledName,
			Signature:   cgenprepassSignature(expr.Signature), // TODO maybe?
			Body:        cgenprepass(expr.Body),
			Importc:     expr.Importc,
		}
	case *TcBuiltinProcDef:
		return expr
	case *TcBuiltinGenericProcDef:
		panic("i don't know what to do?")
		// return expr
	case *TcTemplateDef:
		// template should be inlined at this point in time. Their definitions are therefore superflous.
		// Technically they should just be removed, but taking them in verbatim shouldn't cause harm either.
		return expr
	case *TcStructLit:
		return &TcStructLit{
			Source: expr.Source,
			Items:  cgenprepassSlice(expr.Items),
			Type:   expr.Type,
		}
	//case TcProcSymbol:
	case *TcPackageDef:
		for i, procDef := range expr.ProcDefs {
			expr.ProcDefs[i] = cgenprepass(procDef).(*TcProcDef)
		}
		return expr
	case *TcErrorNode:
		panic("error nodes should not be passed down to a cgenprepass, compilation should already have ended")
	case *TcTypeContext:
		panic("cgenprepass should not have a type context anymore")
	case *TcStructField:
		return expr
	case nil:
		// body of "importc" procs is nil and should stay that way
		return nil
	default:
		panic(fmt.Errorf("not implemented %T", expr))
	}
}
