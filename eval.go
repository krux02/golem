package main

func EvalExpr(tc *TypeChecker, expr TcExpr, scope Scope) TcExpr {
	// TODO actually do something
	//
	switch lit := expr.(type) {
	case TcArrayLit:
		for i := range lit.Items {
			lit.Items[i] = EvalExpr(tc, lit.Items[i], scope)
		}
		return lit
	case TcStructLit:
		for i := range lit.Items {
			lit.Items[i] = EvalExpr(tc, lit.Items[i], scope)
		}
		return lit
	case StrLit:
		return lit
	case IntLit:
		return lit
	case FloatLit:
		return lit
	case TcCodeBlock:
		// TODO: this should be implemented, with a bytecode compiler or something.
		// Note: this is wrong, this is not a value that actually can be used.
		return lit
	}
	ReportErrorf(tc, expr, "eval for %T node not implemented", expr)
	return expr
}
