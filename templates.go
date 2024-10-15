package main

type TemplateSubstitution struct {
	SymOrIdent Expr
	Value      Expr
}

func MapSliceForSyms(items []Expr, substitutions []TemplateSubstitution) (result []Expr) {
	for _, it := range items {
		result = append(result, it.RecSubSyms(substitutions))
	}
	return result
}

func (arg *CodeBlock) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &CodeBlock{
		Source: arg.Source,
		Items:  MapSliceForSyms(arg.Items, substitutions),
	}
}
func (arg *ProcDef) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	result := &ProcDef{
		Source: arg.Source,
		Expr:   arg.Expr.RecSubSyms(substitutions),
	}
	return result
}
func (arg *TypeDef) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	result := &TypeDef{
		Source: arg.Source,
		Expr:   arg.Expr.RecSubSyms(substitutions),
	}
	return result
}

func (arg *ArrayLit) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &ArrayLit{
		Source: arg.Source,
		Items:  MapSliceForSyms(arg.Items, substitutions),
	}
}
func (arg *Call) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &Call{
		Source: arg.Source,
		Callee: arg.Callee.RecSubSyms(substitutions),
		Args:   MapSliceForSyms(arg.Args, substitutions),
		Braced: arg.Braced,
		Prefix: arg.Prefix,
	}
}
func (arg *BracketExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	result := &BracketExpr{
		Source: arg.Source,
		Callee: arg.Callee.RecSubSyms(substitutions),
		Args:   MapSliceForSyms(arg.Args, substitutions),
	}
	return result
}
func (arg *VariableDefStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	result := &VariableDefStmt{
		Source: arg.Source,
		Prefix: arg.Prefix,
		Expr:   arg.Expr.RecSubSyms(substitutions),
	}
	return result
}
func (arg *ForLoopStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &ForLoopStmt{
		Source:     arg.Source,
		LoopIdent:  arg.LoopIdent.RecSubSyms(substitutions).(*Ident),
		Collection: arg.Collection.RecSubSyms(substitutions),
		Body:       arg.Body.RecSubSyms(substitutions),
	}
}
func (arg *WhileLoopStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &WhileLoopStmt{
		Source:    arg.Source,
		Condition: arg.Condition.RecSubSyms(substitutions),
		Body:      arg.Body.RecSubSyms(substitutions),
	}
}
func (arg *IfExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &IfExpr{
		Source:    arg.Source,
		Condition: arg.Condition.RecSubSyms(substitutions),
		Body:      arg.Body.RecSubSyms(substitutions),
	}
}
func (arg *IfElseExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &IfElseExpr{
		Source:    arg.Source,
		Condition: arg.Condition.RecSubSyms(substitutions),
		Body:      arg.Body.RecSubSyms(substitutions),
		Else:      arg.Else.RecSubSyms(substitutions),
	}
}
func (arg *ReturnExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &ReturnExpr{
		Source: arg.Source,
		Value:  arg.Value.RecSubSyms(substitutions),
	}
}
func (arg *VarExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return &VarExpr{
		Source: arg.Source,
		Expr:   arg.Expr.RecSubSyms(substitutions),
	}
}
func (arg *BreakStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *ContinueStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return arg
}
func (arg *NilLit) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	return arg
}

func (arg *PackageDef) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	panic("package level substitutions not thought through")
}

func (arg *ExprList) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	// Exprlist only ever exists as part of the Error node.
	panic("not a real node")
}

// these are the typed ast nodes, they don't do substitudions at all, except for the `TcSymbol` node.

func (arg *TcSymbol) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	// this is the only typed ast node that is allowed to be substituted.
	// Substitutions within a typed ast is not allowed.
	for _, it := range substitutions {
		if it.SymOrIdent == arg {
			return it.Value
		}
	}
	return arg
}

func (arg *Ident) RecSubSyms(substitutions []TemplateSubstitution) Expr {
	for _, it := range substitutions {
		if ident, isIdent := it.SymOrIdent.(*Ident); isIdent {
			if ident.Source == arg.Source {
				return it.Value
			}
		}
	}
	return arg
}

func (arg *PrefixDocComment) RecSubSyms(substitutions []TemplateSubstitution) Expr    { return arg }
func (arg *NamedDocSection) RecSubSyms(substitutions []TemplateSubstitution) Expr     { return arg }
func (arg *IntLit) RecSubSyms(substitutions []TemplateSubstitution) Expr              { return arg }
func (arg *FloatLit) RecSubSyms(substitutions []TemplateSubstitution) Expr            { return arg }
func (arg *StrLit) RecSubSyms(substitutions []TemplateSubstitution) Expr              { return arg }
func (arg *InvalidTokenExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr    { return arg }
func (arg *TcErrorNode) RecSubSyms(substitutions []TemplateSubstitution) Expr         { return arg }
func (arg *TcDotExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr           { return arg }
func (arg *TcStructField) RecSubSyms(substitutions []TemplateSubstitution) Expr       { return arg }
func (arg *TcSymRef) RecSubSyms(substitutions []TemplateSubstitution) Expr            { return arg }
func (arg *TcProcRef) RecSubSyms(substitutions []TemplateSubstitution) Expr           { return arg }
func (arg *TcVariableDefStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr   { return arg }
func (arg *TcReturnStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr        { return arg }
func (arg *TcTypeContext) RecSubSyms(substitutions []TemplateSubstitution) Expr       { return arg }
func (arg *TcForLoopStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr       { return arg }
func (arg *TcWhileLoopStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr     { return arg }
func (arg *TcIfStmt) RecSubSyms(substitutions []TemplateSubstitution) Expr            { return arg }
func (arg *TcIfElseExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr        { return arg }
func (arg *TcCodeBlock) RecSubSyms(substitutions []TemplateSubstitution) Expr         { return arg }
func (arg *TcCall) RecSubSyms(substitutions []TemplateSubstitution) Expr              { return arg }
func (arg *TcIntLit) RecSubSyms(substitutions []TemplateSubstitution) Expr            { return arg }
func (arg *TcFloatLit) RecSubSyms(substitutions []TemplateSubstitution) Expr          { return arg }
func (arg *TcStrLit) RecSubSyms(substitutions []TemplateSubstitution) Expr            { return arg }
func (arg *TcArrayLit) RecSubSyms(substitutions []TemplateSubstitution) Expr          { return arg }
func (arg *TcEnumSetLit) RecSubSyms(substitutions []TemplateSubstitution) Expr        { return arg }
func (arg *TcProcDef) RecSubSyms(substitutions []TemplateSubstitution) Expr           { return arg }
func (arg *TcBuiltinProcDef) RecSubSyms(substitutions []TemplateSubstitution) Expr    { return arg } // builtins have no source location
func (arg *TcTemplateDef) RecSubSyms(substitutions []TemplateSubstitution) Expr       { return arg }
func (arg *TcStructLit) RecSubSyms(substitutions []TemplateSubstitution) Expr         { return arg }
func (arg *TcPackageDef) RecSubSyms(substitutions []TemplateSubstitution) Expr        { return arg }
func (arg *TcStructDef) RecSubSyms(substitutions []TemplateSubstitution) Expr         { return arg }
func (arg *TcEnumDef) RecSubSyms(substitutions []TemplateSubstitution) Expr           { return arg }
func (arg *TcTypeAlias) RecSubSyms(substitutions []TemplateSubstitution) Expr         { return arg }
func (arg *TcTraitDef) RecSubSyms(substitutions []TemplateSubstitution) Expr          { return arg }
func (arg *GenericTypeSymbol) RecSubSyms(substitutions []TemplateSubstitution) Expr   { return arg }
func (arg *TcBuiltinMacroDef) RecSubSyms(substitutions []TemplateSubstitution) Expr   { return arg }
func (arg *TcErrorProcDef) RecSubSyms(substitutions []TemplateSubstitution) Expr      { return arg }
func (arg *TcWrappedUntypedAst) RecSubSyms(substitutions []TemplateSubstitution) Expr { return arg }
func (arg *TcEmitExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr          { return arg }
func (arg *TcCastExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr          { return arg }
func (arg *TcConvExpr) RecSubSyms(substitutions []TemplateSubstitution) Expr          { return arg }
