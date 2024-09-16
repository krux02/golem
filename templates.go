package main

type TemplateSubstitution struct {
	Sym   *TcSymbol
	Value Expr
}

func MapSliceForSyms(items []Expr, substitutions []TemplateSubstitution) (result []Expr) {
	for _, it := range items {
		result = append(result, it.RecursiveSubstituteSymbols(substitutions))
	}
	return result
}

func (arg *ExprList) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &ExprList{
		Source: arg.Source,
		Items:  MapSliceForSyms(arg.Items, substitutions),
	}
}
func (arg *CodeBlock) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &CodeBlock{
		Source: arg.Source,
		Items:  MapSliceForSyms(arg.Items, substitutions),
	}
}
func (arg *ProcDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	result := &ProcDef{
		Source: arg.Source,
		Expr:   arg.Expr.RecursiveSubstituteSymbols(substitutions),
	}
	return result
}
func (arg *TypeDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	result := &TypeDef{
		Source: arg.Source,
		Expr:   arg.Expr.RecursiveSubstituteSymbols(substitutions),
	}
	return result
}

func (arg *PrefixDocComment) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *NamedDocSection) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *Ident) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *IntLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *FloatLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *StrLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *ArrayLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &ArrayLit{
		Source: arg.Source,
		Items:  MapSliceForSyms(arg.Items, substitutions),
	}
}
func (arg *Call) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &Call{
		Source: arg.Source,
		Callee: arg.Callee.RecursiveSubstituteSymbols(substitutions),
		Args:   MapSliceForSyms(arg.Args, substitutions),
		Braced: arg.Braced,
		Prefix: arg.Prefix,
	}
}
func (arg *BracketExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	result := &BracketExpr{
		Source: arg.Source,
		Callee: arg.Callee.RecursiveSubstituteSymbols(substitutions),
		Args:   MapSliceForSyms(arg.Args, substitutions),
	}
	return result
}
func (arg *VariableDefStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	result := &VariableDefStmt{
		Source: arg.Source,
		Prefix: arg.Prefix,
		Expr:   arg.Expr.RecursiveSubstituteSymbols(substitutions),
	}
	return result
}
func (arg *ForLoopStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &ForLoopStmt{
		Source:     arg.Source,
		LoopIdent:  arg.LoopIdent.RecursiveSubstituteSymbols(substitutions).(*Ident),
		Collection: arg.Collection.RecursiveSubstituteSymbols(substitutions),
		Body:       arg.Body.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *WhileLoopStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &WhileLoopStmt{
		Source:    arg.Source,
		Condition: arg.Condition.RecursiveSubstituteSymbols(substitutions),
		Body:      arg.Body.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *IfExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &IfExpr{
		Source:    arg.Source,
		Condition: arg.Condition.RecursiveSubstituteSymbols(substitutions),
		Body:      arg.Body.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *IfElseExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &IfElseExpr{
		Source:    arg.Source,
		Condition: arg.Condition.RecursiveSubstituteSymbols(substitutions),
		Body:      arg.Body.RecursiveSubstituteSymbols(substitutions),
		Else:      arg.Else.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *ReturnExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &ReturnExpr{
		Source: arg.Source,
		Value:  arg.Value.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *VarExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &VarExpr{
		Source: arg.Source,
		Expr:   arg.Expr.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *BreakStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *ContinueStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TypeContext) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &TypeContext{
		Source: arg.Source,
		Expr:   arg.Expr.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *NilLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *EmitStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *StaticExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &StaticExpr{
		Source: arg.Source,
		Expr:   arg.Expr.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *ImportStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}

func (arg *TraitDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return &TraitDef{
		Source: arg.Source,
		Expr:   arg.Expr.RecursiveSubstituteSymbols(substitutions),
	}
}
func (arg *PackageDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	panic("package level substitutions not thought through")
}

// these are the typed ast nodes, they don't do substitudions at all, except for the `TcSymbol` node.

func (arg *TcSymbol) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	// this is the only typed ast node that is allowed to be substituted.
	// Substitutions within a typed ast is not allowed.
	for _, it := range substitutions {
		if it.Sym == arg {
			return it.Value
		}
	}
	return arg
}

func (arg *InvalidTokenExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcErrorNode) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcDotExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcStructField) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcSymRef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcProcSymbol) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcVariableDefStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcReturnExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcTypeContext) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcForLoopStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcWhileLoopStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcIfStmt) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcIfElseExpr) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcCodeBlock) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcCall) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr { return arg }
func (arg *TcIntLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcFloatLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcStrLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcArrayLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcEnumSetLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcProcDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcBuiltinProcDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
} // builtins have no source location
func (arg *TcBuiltinGenericProcDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
} // builtins have no source location
func (arg *TcTemplateDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcStructLit) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcPackageDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcStructDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcEnumDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *GenericTypeSymbol) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcBuiltinMacroDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcErrorProcDef) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *TcWrappedUntypedAst) RecursiveSubstituteSymbols(substitutions []TemplateSubstitution) Expr {
	return arg
}
