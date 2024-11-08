package main

func ExpectArgsLen(sc *SemChecker, node Expr, gotten, expected int) bool {
	if expected != gotten {
		ReportErrorf(sc, node, "expected %d arguments, but got %d", expected, gotten)
		return false
	}
	return true
}

func ExpectMinArgsLen(sc *SemChecker, node Expr, gotten, expected int) bool {
	if gotten < expected {
		ReportErrorf(sc, node, "Expected at least %d arguments, but got %d.", expected, gotten)
		return false
	}
	return true
}

func MatchVariableDefStatement(sc *SemChecker, callee Expr, expr Expr) (kind SymbolKind, name *Ident, typeExpr Expr, value Expr, ok bool) {
	// String matiching
	switch callee.GetSource() {
	case "var":
		kind = SkVar
	case "let":
		kind = SkLet
	case "const":
		kind = SkConst
	default:
		// this branch should be dead code
		ReportErrorf(sc, callee, "must be var/let/const")
		return SkInvalid, nil, nil, nil, false
	}

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

func MatchBinaryOperator(expr Expr, operator string) (lhs Expr, rhs Expr, ok bool) {
	call, ok := expr.(*Call)
	if !ok {
		return nil, nil, false
	}
	op, ok := call.Callee.(*Ident)
	if !ok || op.Source != operator || len(call.Args) != 2 {
		return nil, nil, false
	}
	return call.Args[0], call.Args[1], true
}

func MatchColonExpr(expr Expr) (lhs Expr, rhs Expr, ok bool) {
	return MatchBinaryOperator(expr, ":")
}

func MatchArrowExpr(expr Expr) (lhs Expr, rhs Expr, ok bool) {
	return MatchBinaryOperator(expr, "->")
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

type GenericArgument struct {
	Source    string
	Name      *Ident
	TraitName *Ident
}
type ProcArgument struct {
	Source  string
	Name    *Ident
	Mutable bool
	Type    Expr
}

func MatchVarExpr(expr Expr) (result Expr, ok bool) {
	call, ok := expr.(*Call)
	if !ok {
		return nil, false
	}
	if len(call.Args) != 1 {
		return nil, false
	}
	ident, ok := call.Callee.(*Ident)
	if !ok {
		return nil, false
	}
	if ident.Source != "var" {
		return nil, false
	}
	return call.Args[0], true
}

func MustMatchProcDef(sc *SemChecker, expr Expr) (name *Ident, body, resultType Expr, genericArgs []GenericArgument, args []ProcArgument) {

	if lhs, rhs, isAssign := MatchAssign(expr); isAssign {
		body = rhs
		expr = lhs
	}

	if lhs, rhs, isArrowExpr := MatchArrowExpr(expr); isArrowExpr {
		resultType = rhs
		expr = lhs
	}

	var argsRaw []Expr
	if call, isCall := expr.(*Call); isCall {
		argsRaw = call.Args
		expr = call.Callee
	} else {
		// TODO, test this
		ReportErrorf(sc, expr, "proc def requires an argument list")
	}

	if bracketExpr, isBracketExpr := expr.(*BracketExpr); isBracketExpr {
		expr = bracketExpr.Callee
		genericArgs = make([]GenericArgument, 0, len(bracketExpr.Args))
		// TODO do something with args
		for _, arg := range bracketExpr.Args {
			call, isCall := arg.(*Call)
			if !isCall {
				// TODO test error message
				ReportErrorf(sc, arg, "generic argument must be a call expr")
				continue
			}
			if len(call.Args) != 1 {
				ReportErrorf(sc, arg, "Trait on multiple types not yet implemented")
				continue
			}
			name, isIdent := call.Args[0].(*Ident)
			if !isIdent {
				// TODO test error message
				ReportErrorf(sc, call.Args[0], "generic argument name must be an Identifire, but it is %T", call.Args[0])
				continue
			}
			traitName, isIdent := call.Callee.(*Ident)
			if !isIdent {
				// TODO firstToken is not the right code location
				ReportErrorf(sc, call.Callee, "generic argument constraint must be an Identifire, but it is %T", call.Callee)
				continue
			}

			genericArg := GenericArgument{Source: arg.GetSource(), Name: name, TraitName: traitName}
			genericArgs = append(genericArgs, genericArg)
		}
	}

	if ident, isIdent := expr.(*Ident); isIdent {
		name = ident
	} else {
		ReportErrorf(sc, expr, "proc name must be an identifier, but it is %T", expr)
	}

	var newArgs []ProcArgument
	for _, arg := range argsRaw {

		var newArg Expr
		var typeExpr Expr
		var gotTypeExpr bool = false

		newArg, typeExpr, gotTypeExpr = MatchColonExpr(arg)
		if gotTypeExpr {
			arg = newArg
		}

		var mutable = false
		if varExpr, ok := MatchVarExpr(arg); ok {
			mutable = true
			arg = varExpr
		}
		if ident, isIdent := arg.(*Ident); isIdent {
			newArgs = append(newArgs, ProcArgument{Source: ident.Source, Name: ident, Mutable: mutable})
		} else {
			ReportErrorf(sc, arg, "expected identifier but got %T (%s)", arg, AstFormat(arg))
			// TODO decide if this should be an error node or something in the arguments list
			newArgs = append(newArgs, ProcArgument{Source: arg.GetSource(), Name: &Ident{Source: "_"}})
		}

		if gotTypeExpr {
			for i := range newArgs {
				newArgs[i].Type = typeExpr
			}
			args = append(args, newArgs...)

			newArgs = newArgs[:0]
		}
	}
	if len(newArgs) > 0 {
		// TODO test this error
		ReportErrorf(sc, newArgs[0].Name, "arguments have no type: %+v", newArgs)
	}

	ValidNameCheck(sc, name, "proc")

	return
}
