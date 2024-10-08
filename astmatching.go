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

func MustMatchProcDef(sc *SemChecker, def *ProcDef) (name *Ident, body, resultType Expr, genericArgs []GenericArgument, args []ProcArgument) {
	var expr = def.Expr

	if lhs, rhs, isAssign := MatchAssign(expr); isAssign {
		body = rhs
		expr = lhs
	}

	if lhs, rhs, isColonExpr := MatchColonExpr(expr); isColonExpr {
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
			lhs, rhs, isColonExpr := MatchColonExpr(arg)
			if !isColonExpr {
				// TODO test error message
				ReportErrorf(sc, arg, "generic argument must be a colon expr")
				continue
			}
			name, isIdent := lhs.(*Ident)
			if !isIdent {
				// TODO test error message
				ReportErrorf(sc, lhs, "generic argument name must be an Identifire, but it is %T", lhs)
				continue
			}
			traitName, isIdent := rhs.(*Ident)
			if !isIdent {
				// TODO firstToken is not the right code location
				ReportErrorf(sc, rhs, "generic argument constraint must be an Identifire, but it is %T", rhs)
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
		if varExpr, ok := arg.(*VarExpr); ok {
			mutable = true
			arg = varExpr.Expr
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

	// apply doc section
	if def.DocComment != nil {
		name.Comment = append(name.Comment, def.DocComment.BaseDoc...)

	DOCSECTIONS1:
		for _, it := range def.DocComment.NamedDocSections {
			key := it.Name
			value := it.Lines

			for i := range args {
				if args[i].Name.Source == key {
					commentRef := &args[i].Name.Comment
					*commentRef = append(*commentRef, value...)
					continue DOCSECTIONS1
				}
			}
			ReportInvalidDocCommentKey(sc, it)
		}
	}
	return
}
