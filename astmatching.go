package main

func ExpectArgsLen(sc *SemChecker, node AstNode, gotten, expected int) bool {
	if expected != gotten {
		ReportErrorf(sc, node, "expected %d arguments, but got %d", expected, gotten)
		return false
	}
	return true
}

func ExpectMinArgsLen(sc *SemChecker, node AstNode, gotten, expected int) bool {
	if gotten < expected {
		ReportErrorf(sc, node, "Expected at least %d arguments, but got %d.", expected, gotten)
		return false
	}
	return true
}

func MatchVariableDefStatement(sc *SemChecker, arg *VariableDefStmt) (kind SymbolKind, name *Ident, typeExpr Expr, value Expr, ok bool) {
	// String matiching
	switch arg.Prefix.Source {
	case "var":
		kind = SkVar
	case "let":
		kind = SkLet
	case "const":
		kind = SkConst
	default:
		kind = SkInvalid
	}

	var expr = arg.Expr

	if lhs, rhs, ok := MatchAssign(expr); ok {
		value = rhs
		expr = lhs
	}

	if lhs, rhs, ok := MatchColonExpr(expr); ok {
		typeExpr = rhs
		expr = lhs
	}

	name, isIdent := expr.(*Ident)
	if !isIdent {
		ReportErrorf(sc, expr, "expect identifier")
	}

	return kind, name, typeExpr, value, kind != SkInvalid && isIdent
}

func MatchColonExpr(expr Expr) (lhs Expr, rhs Expr, ok bool) {
	call, ok := expr.(*Call)
	if !ok {
		return nil, nil, false
	}
	op, ok := call.Callee.(*Ident)
	if !ok || op.Source != ":" || len(call.Args) != 2 {
		return nil, nil, false
	}
	return call.Args[0], call.Args[1], true
}

func MatchAssign(expr Expr) (lhs, rhs Expr, ok bool) {
	call, ok := expr.(*Call)
	if !ok {
		return lhs, rhs, false
	}
	op, ok := call.Callee.(*Ident)
	if !ok || op.Source != "=" || len(call.Args) != 2 {
		return lhs, rhs, false
	}
	lhs = call.Args[0]
	rhs = call.Args[1]
	return lhs, rhs, true
}

func MatchPrefixCall(expr Expr) (callee *Ident, arg Expr, ok bool) {
	call, isCall := expr.(*Call)
	if !isCall {
		return nil, nil, false
	}
	if len(call.Args) != 1 {
		return nil, nil, false
	}
	ident, isIdent := call.Callee.(*Ident)
	if !isIdent {
		return nil, nil, false
	}
	return ident, call.Args[0], true
}
