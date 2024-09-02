package main

func EvalExpr(sc *SemChecker, expr TcExpr, scope Scope) TcExpr {
	// TODO actually do something
	//
	switch lit := expr.(type) {
	case TcArrayLit:
		for i := range lit.Items {
			lit.Items[i] = EvalExpr(sc, lit.Items[i], scope)
		}
		return lit
	case TcStructLit:
		for i := range lit.Items {
			lit.Items[i] = EvalExpr(sc, lit.Items[i], scope)
		}
		return lit
	case TcStrLit:
		return lit
	case TcIntLit:
		return lit
	case TcFloatLit:
		return lit
	case TcCodeBlock:
		// TODO: this should be implemented, with a bytecode compiler or something.
		// Note: this is wrong, this is not a value that actually can be used.
		return lit
	}
	ReportErrorf(sc, expr, "eval for %T node not implemented", expr)
	return expr
}
