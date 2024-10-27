package main

import "fmt"

type Substitutions struct {
	procSubs []ProcSubstitution
	symSubs  []SymbolSubstitution
	typeSubs []TypeSubstitution
}

func InstanciateBuiltinGenericProc(proc *TcBuiltinProcDef, subs *Substitutions) *TcBuiltinProcDef {
	if len(proc.Signature.GenericParams) == 0 {
		return proc
	}
	cacheKey := ComputeInstanceCacheKey(proc.Signature.GenericParams, subs.typeSubs)
	result := proc.InstanceCache.LookUp(cacheKey)
	if result != nil {
		return result
	}
	newSig := SignatureApplyTypeSubstitution(proc.Signature, subs)
	result = &TcBuiltinProcDef{
		Signature: newSig,
		Prefix:    proc.Prefix,
		Infix:     proc.Infix,
		Postfix:   proc.Postfix,
	}
	proc.InstanceCache.Set(cacheKey, result)
	return result
}

func InstanciateGenericProc(proc *TcProcDef, subs *Substitutions) *TcProcDef {
	// fmt.Printf("instantiate proc:\n%s\n", AstFormat(proc))
	// fmt.Println(AstFormat(subs))

	if len(proc.Signature.GenericParams) == 0 {
		return proc
	}
	cacheKey := ComputeInstanceCacheKey(proc.Signature.GenericParams, subs.typeSubs)
	result := proc.InstanceCache.LookUp(cacheKey)
	if result != nil {
		return result
	}
	newSignature := SignatureApplyTypeSubstitution(proc.Signature, subs)
	newBody := recursiveInstanciateGenericBody(proc.Body, subs)
	if len(newSignature.GenericParams) > 0 {
		panic("internal error, generic proc not fully instanciated")
	}
	result = &TcProcDef{
		Source:      proc.Source,
		MangledName: MangleSignature(&newSignature),
		Signature:   newSignature,
		Body:        newBody,
	}
	proc.InstanceCache.Set(cacheKey, result)
	// fmt.Printf("instantiated proc:\n%s\n", AstFormat(result))
	return result
}

func recursiveInstanciateGenericBody(body TcExpr, subs *Substitutions) TcExpr {
	switch b := body.(type) {
	case nil:
		// The body of trait procs is nil
		return nil
	case *TcErrorNode:
		copy := *b
		return &copy
	case *TcDotExpr:
		return &TcDotExpr{
			Source: b.Source,
			Lhs:    recursiveInstanciateGenericBody(b.Lhs, subs),
			Rhs:    recursiveInstanciateGenericBody(b.Rhs, subs).(*TcStructField),
		}
	case *TcStructField:
		return &TcStructField{
			Source: b.Source,
			Name:   b.Name,
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs),
		}
	case *TcSymRef:
		return &TcSymRef{
			Source: b.Source,
			Sym:    ApplySymbolSubstitutions(b.Sym, subs.symSubs),
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs),
		}
	case *TcVariableDefStmt:
		newSym := &TcSymbol{
			Source: b.Sym.Source,
			Kind:   b.Sym.Kind,
			Type:   ApplyTypeSubstitutions(b.Sym.Type, subs.typeSubs),
		}
		subs.symSubs = append(subs.symSubs, SymbolSubstitution{b.Sym, newSym})
		return &TcVariableDefStmt{
			Source: b.Source,
			Sym:    newSym,
			Value:  recursiveInstanciateGenericBody(b.Value, subs),
		}
	case *TcReturnStmt:
		return &TcReturnStmt{
			Source: b.Source,
			Value:  recursiveInstanciateGenericBody(b.Value, subs),
		}
	case *TcTypeContext:
		return &TcTypeContext{
			Source:      b.Source,
			WrappedType: ApplyTypeSubstitutions(b.WrappedType, subs.typeSubs),
		}
	case *TcForLoopStmt:
		newSym := &TcSymbol{
			Source: b.LoopSym.Source,
			Kind:   b.LoopSym.Kind,
			Type:   ApplyTypeSubstitutions(b.LoopSym.Type, subs.typeSubs),
		}
		subs.symSubs = append(subs.symSubs, SymbolSubstitution{b.LoopSym, newSym})
		return &TcForLoopStmt{
			Source:     b.Source,
			LoopSym:    newSym,
			Collection: recursiveInstanciateGenericBody(b.Collection, subs),
			Body:       recursiveInstanciateGenericBody(b.Body, subs),
		}
	case *TcWhileLoopStmt:
		return &TcWhileLoopStmt{
			Source:    b.Source,
			Condition: recursiveInstanciateGenericBody(b.Condition, subs),
			Body:      recursiveInstanciateGenericBody(b.Body, subs),
		}
	case *TcIfStmt:
		return &TcIfStmt{
			Source:    b.Source,
			Condition: recursiveInstanciateGenericBody(b.Condition, subs),
			Body:      recursiveInstanciateGenericBody(b.Body, subs),
		}
	case *TcIfElseExpr:
		return &TcIfElseExpr{
			Source:    b.Source,
			Condition: recursiveInstanciateGenericBody(b.Condition, subs),
			Body:      recursiveInstanciateGenericBody(b.Body, subs),
			Else:      recursiveInstanciateGenericBody(b.Else, subs),
		}
	case *TcCodeBlock:
		newItems := make([]TcExpr, len(b.Items))
		for i, it := range b.Items {
			newItems[i] = recursiveInstanciateGenericBody(it, subs)
		}
		return &TcCodeBlock{
			Source: b.Source,
			Items:  newItems,
		}
	case *TcCall:
		newArgs := make([]TcExpr, len(b.Args))
		for i, it := range b.Args {
			newArgs[i] = recursiveInstanciateGenericBody(it, subs)
		}
		return &TcCall{
			Source: b.Source,
			Sym:    ApplyProcSubstitutions(b.Sym, subs.procSubs),
			Args:   newArgs,
			Braced: b.Braced,
		}
	case *TcIntLit, *TcFloatLit, *TcStrLit:
		return b
	case *TcArrayLit:
		newItems := make([]TcExpr, len(b.Items))
		for i, it := range b.Items {
			newItems[i] = recursiveInstanciateGenericBody(it, subs)
		}
		return &TcArrayLit{
			Source:   b.Source,
			Items:    newItems,
			ElemType: ApplyTypeSubstitutions(b.ElemType, subs.typeSubs),
		}
	case *TcEnumSetLit:
		newItems := make([]TcExpr, len(b.Items))
		for i, it := range b.Items {
			newItems[i] = recursiveInstanciateGenericBody(it, subs)
		}
		return &TcEnumSetLit{
			Source:   b.Source,
			Items:    newItems,
			ElemType: ApplyTypeSubstitutions(b.ElemType, subs.typeSubs).(*EnumType),
		}
	case *TcProcDef:
		panic("not implemented")
		// return &TcProcDef{
		// 	Source      string
		// 	MangledName string
		// 	Signature   *Signature
		// 	Body        TcExpr
		// 	Importc     bool
		// 	Signature: instanciateGenericBody(b.Signature, subs),
		// }
	case *TcBuiltinProcDef:
		panic("not implemented")
	case *TcTemplateDef:
		panic("not implemented")
	case *TcStructLit:
		newItems := make([]TcExpr, len(b.Items))
		for i, it := range b.Items {
			newItems[i] = recursiveInstanciateGenericBody(it, subs)
		}
		return &TcStructLit{
			Source: b.Source,
			Items:  newItems,
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs).(*StructType),
		}
	case *TcPackageDef:
		panic("package def?")
	case *TcStructDef:
		panic("not implemented")
	case *TcEnumDef:
		panic("not implemented, maybe do nothing?")
	case *TcTypeAlias:
		return &TcTypeAlias{
			Source: b.Source,
			Name:   b.Name,
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs),
		}
	case *TcTraitDef:
		panic("not implemented")
	// case *GenericTypeSymbol:
	case *TcBuiltinMacroDef:
		panic("not implemented")
	case *TcErrorProcDef:
		panic("not implemented")
	case *TcWrappedUntypedAst:
		panic("not implemented")
	case *TcEmitExpr:
		newSourceSymPairs := make([]SymSourcePair, len(b.SourceSymPairs))
		for i, it := range b.SourceSymPairs {
			newSourceSymPairs[i].EmitSource = it.EmitSource
			newSourceSymPairs[i].Sym = recursiveInstanciateGenericBody(it.Sym, subs)
		}
		return &TcEmitExpr{
			Source:         b.Source,
			Type:           ApplyTypeSubstitutions(b.Type, subs.typeSubs),
			SourceSymPairs: newSourceSymPairs,
			EmitSource:     b.EmitSource,
		}
	case *TcCastExpr:
		return &TcCastExpr{
			Source: b.Source,
			Expr:   recursiveInstanciateGenericBody(b.Expr, subs),
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs),
		}
	case *TcConvExpr:
		return &TcConvExpr{
			Source: b.Source,
			Expr:   recursiveInstanciateGenericBody(b.Expr, subs),
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs),
		}
	}
	panic(fmt.Errorf("compiler bug, node not handled in instanciate generic body %T", body))
}
