package main

type Substitutions struct {
	procSubs []ProcSubstitution
	symSubs  []SymbolSubstitution
	typeSubs []TypeSubstitution
}

func instanciateGenericBody(body TcExpr, subs *Substitutions) TcExpr {
	switch b := body.(type) {
	case *TcErrorNode:
		copy := *b
		return &copy
	case *TcDotExpr:
		return &TcDotExpr{
			Source: b.Source,
			Lhs:    instanciateGenericBody(b.Lhs, subs),
			Rhs:    instanciateGenericBody(b.Rhs, subs).(*TcStructField),
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
			Value:  instanciateGenericBody(b.Value, subs),
		}
	case *TcReturnExpr:
		return &TcReturnExpr{
			Source: b.Source,
			Value:  instanciateGenericBody(b.Value, subs),
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
			Collection: instanciateGenericBody(b.Collection, subs),
			Body:       instanciateGenericBody(b.Body, subs),
		}
	case *TcWhileLoopStmt:
		return &TcWhileLoopStmt{
			Source:    b.Source,
			Condition: instanciateGenericBody(b.Condition, subs),
			Body:      instanciateGenericBody(b.Body, subs),
		}
	case *TcIfStmt:
		return &TcIfStmt{
			Source:    b.Source,
			Condition: instanciateGenericBody(b.Condition, subs),
			Body:      instanciateGenericBody(b.Body, subs),
		}
	case *TcIfElseExpr:
		return &TcIfElseExpr{
			Source:    b.Source,
			Condition: instanciateGenericBody(b.Condition, subs),
			Body:      instanciateGenericBody(b.Body, subs),
			Else:      instanciateGenericBody(b.Else, subs),
		}
	case *TcCodeBlock:
		newItems := make([]TcExpr, len(b.Items))
		for i, it := range b.Items {
			newItems[i] = instanciateGenericBody(it, subs)
		}
		return &TcCodeBlock{
			Source: b.Source,
			Items:  newItems,
		}
	case *TcCall:
		newArgs := make([]TcExpr, len(b.Args))
		for i, it := range b.Args {
			newArgs[i] = instanciateGenericBody(it, subs)
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
			newItems[i] = instanciateGenericBody(it, subs)
		}
		return &TcArrayLit{
			Source:   b.Source,
			Items:    newItems,
			ElemType: ApplyTypeSubstitutions(b.ElemType, subs.typeSubs),
		}
	case *TcEnumSetLit:
		newItems := make([]TcExpr, len(b.Items))
		for i, it := range b.Items {
			newItems[i] = instanciateGenericBody(it, subs)
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
	case *TcGenericProcDef:
		newSignature := SignatureApplyTypeSubstitution(b.Signature, subs)

		// TODO use InstanceCache here

		if len(newSignature.GenericParams) > 0 {
			return &TcGenericProcDef{
				Source:        b.Source,
				Signature:     newSignature,
				Body:          instanciateGenericBody(b.Body, subs),
				InstanceCache: b.InstanceCache, // TODO is this even correct?
			}
		}
		return &TcProcDef{
			Source:      b.Source,
			MangledName: MangleSignature(newSignature),
			Signature:   newSignature,
			Body:        instanciateGenericBody(b.Body, subs),
		}
		// panic("not implemented")
	case *TcBuiltinProcDef:
		panic("not implemented")
	case *TcBuiltinGenericProcDef:
		panic("not implemented")
	case *TcTemplateDef:
		panic("not implemented")
	case *TcStructLit:
		newItems := make([]TcExpr, len(b.Items))
		for i, it := range b.Items {
			newItems[i] = instanciateGenericBody(it, subs)
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
			newSourceSymPairs[i].Sym = instanciateGenericBody(it.Sym, subs)
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
			Expr:   instanciateGenericBody(b.Expr, subs),
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs),
		}
	case *TcConvExpr:
		return &TcConvExpr{
			Source: b.Source,
			Expr:   instanciateGenericBody(b.Expr, subs),
			Type:   ApplyTypeSubstitutions(b.Type, subs.typeSubs),
		}
	}
	panic("compiler bug, node not handled in instanciate generic body")
}
