package main

import (
	"fmt"
	"path"
	"strconv"
	"strings"
	"unicode/utf8"
)

// TODO this is not a function sybol table, it is just an Operator Precedence table
var OperatorPrecedence map[string]int = map[string]int{
	"*": 6,
	"/": 6,
	"+": 5,
	"-": 5,
}

func parseIdent(tokenizer *Tokenizer) (result Ident) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
	result.source = token.value
	return
}

func parseOperator(tokenizer *Tokenizer) (result Ident) {
	token := tokenizer.Next()
	tk := token.kind
	if tk != TkOperator && tk != TkAnd && tk != TkOr {
		tokenizer.formatWrongKind(token)
	}
	result.source = token.value
	return
}

func parseTypeExpr(tokenizer *Tokenizer) (result TypeExpr) {
	firstToken := tokenizer.lookAheadToken
	result.Ident = parseIdent(tokenizer)
	// TODO this must be used
	if tokenizer.lookAheadToken.kind == TkOpenBrace {
		result.ExprArgs = parseExprList(tokenizer, TkOpenBrace, TkCloseBrace)
	}
	if tokenizer.lookAheadToken.kind == TkOpenBracket {
		result.TypeArgs = parseTypeExprList(tokenizer, TkOpenBracket, TkCloseBracket)
	}
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseReturnStmt(tokenizer *Tokenizer) (result ReturnStmt) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkReturn)
	result.Value = parseExpr(tokenizer, false)
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

/* (name string, typ TypeExpr, expr Expr) */

func parseVariableDefStmt(tokenizer *Tokenizer) (result VariableDefStmt) {
	firstToken := tokenizer.Next()
	switch firstToken.kind {
	case TkLet:
		result.Kind = SkLet
	case TkVar:
		result.Kind = SkVar
	case TkConst:
		result.Kind = SkConst
	default:
		tokenizer.formatWrongIdent(firstToken)
	}

	result.Name = parseIdent(tokenizer)
	if tokenizer.lookAheadToken.kind == TkColon {
		_ = tokenizer.Next()
		result.TypeExpr = parseTypeExpr(tokenizer)
	}
	if tokenizer.lookAheadToken.kind == TkOperator {
		op := tokenizer.Next()
		tokenizer.expectOperator(op, "=")
		result.Value = parseExpr(tokenizer, false)
	}
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseBreakStmt(tokenizer *Tokenizer) (result BreakStmt) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkBreak)
	result.source = token.value
	return
}

func parseContinueStmt(tokenizer *Tokenizer) (result ContinueStmt) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkContinue)
	result.source = token.value
	return
}

func parseForLoop(tokenizer *Tokenizer) (result ForLoopStmt) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkFor)
	result.LoopIdent = parseIdent(tokenizer)
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIn)
	result.Collection = parseExpr(tokenizer, false)
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkDo)
	result.Body = parseExpr(tokenizer, false)
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseIfStmt(tokenizer *Tokenizer) Expr {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkIf)
	condition := parseExpr(tokenizer, false)
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkDo)
	body := parseExpr(tokenizer, false)
	if tokenizer.lookAheadToken.kind == TkElse {
		tokenizer.Next()
		elseExpr := parseExpr(tokenizer, false)

		result := IfElseStmt{Condition: condition, Body: body, Else: elseExpr}
		lastToken := tokenizer.token
		result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
		return result
		// return IfElseStmt{Condition: condition, Body: body, Else: elseExpr}
	}
	result := IfStmt{Condition: condition, Body: body}
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

func (tokenizer *Tokenizer) eatSemicolon() {
	for tokenizer.lookAheadToken.kind == TkSemicolon {
		tokenizer.Next()
	}
}

func parseCodeBlock(tokenizer *Tokenizer) (result CodeBlock) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkOpenCurly)
	tokenizer.eatSemicolon()
	for tokenizer.lookAheadToken.kind != TkCloseCurly {
		item := parseStmtOrExpr(tokenizer)
		result.Items = append(result.Items, item)
		tokenizer.eatSemicolon()
	}
	lastToken := tokenizer.Next()
	tokenizer.expectKind(lastToken, TkCloseCurly)
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseCharLit(tokenizer *Tokenizer) (result CharLit) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkCharLit)
	result.source = token.value

	rune1, rune1Len := utf8.DecodeRuneInString(token.value[1:])
	if rune1 == '\\' {
		rune2, _ := utf8.DecodeRuneInString(token.value[1+rune1Len:])
		switch rune2 {
		case 'a':
			result.Rune = '\a'
		case 'b':
			result.Rune = '\b'
		case 'f':
			result.Rune = '\f'
		case 'n':
			result.Rune = '\n'
		case 'r':
			result.Rune = '\r'
		case 't':
			result.Rune = '\t'
		case 'v':
			result.Rune = '\v'
		case '\\':
			result.Rune = '\\'
		case '\'':
			result.Rune = '\''
		case '"':
			result.Rune = '"'
		default:
			panic(tokenizer.formatError(token, "invalid escape \\%c in char literal", rune2))
		}
		return
	}
	result.Rune = rune1
	return
}

func parseStrLit(tokenizer *Tokenizer) (result StrLit) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkStrLit)
	result.source = token.value
	var b strings.Builder
	b.Grow(len(token.value) - 2)
	var processEscape bool
	for i, rune := range token.value {
		if processEscape {
			switch rune {
			case 'a':
				b.WriteRune('\a')
			case 'b':
				b.WriteRune('\b')
			case 'f':
				b.WriteRune('\f')
			case 'n':
				b.WriteRune('\n')
			case 'r':
				b.WriteRune('\r')
			case 't':
				b.WriteRune('\t')
			case 'v':
				b.WriteRune('\v')
			case '\\':
				b.WriteRune('\\')
			case '\'':
				b.WriteRune('\'')
			case '"':
				b.WriteRune('"')
			default:
				panic(tokenizer.formatError(token, "invalid escape \\%c in string literal", rune))
			}
			processEscape = false
			continue
		}

		if rune == '\\' {
			processEscape = true
			continue
		} else if i != 0 && i != len(token.value)-1 {
			b.WriteRune(rune)
		}
	}

	result.Value = b.String()
	return
}

func parseIntLit(tokenizer *Tokenizer) (result IntLit) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIntLit)
	result.source = token.value
	intValue, err := strconv.Atoi(token.value)
	if err != nil {
		panic("internal error invalid int token")
	}
	result.Value = intValue
	return
}

func parseFloatLit(tokenizer *Tokenizer) (result FloatLit) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkFloatLit)
	result.source = token.value
	floatValue, err := strconv.ParseFloat(token.value, 64)
	if err != nil {
		panic("internal error invalid float token")
	}
	result.Value = floatValue
	return
}

func parseInfixCall(tokenizer *Tokenizer, lhs Expr) (result Call) {
	operator := parseOperator(tokenizer)
	rhs := parseExpr(tokenizer, false)
	result = Call{Callee: operator, Args: []Expr{lhs, rhs}}

	if rhsCall, ok := rhs.(Call); ok && !rhsCall.Braced {
		rhsOperator, isIdent := rhsCall.Callee.(Ident)
		if isIdent && OperatorPrecedence[operator.source] > OperatorPrecedence[rhsOperator.source] {
			// operator precedence
			result.Args[1] = rhsCall.Args[0]
			result.source = joinSubstr(tokenizer.code, result.Args[0].Source(), result.Args[1].Source())
			rhsCall.Args[0] = result
			rhsCall.source = joinSubstr(tokenizer.code, rhsCall.Args[0].Source(), rhsCall.Args[1].Source())
			return rhsCall
		}
	}
	result.source = joinSubstr(tokenizer.code, lhs.Source(), rhs.Source())
	return
}

func parseTypeExprList(tokenizer *Tokenizer, tkOpen, tkClose TokenKind) (result []TypeExpr) {
	next := tokenizer.Next()
	tokenizer.expectKind(next, tkOpen)
	if tokenizer.lookAheadToken.kind != tkClose {
		result = append(result, parseTypeExpr(tokenizer))
		for tokenizer.lookAheadToken.kind == TkComma {
			tokenizer.Next()
			result = append(result, parseTypeExpr(tokenizer))
		}
	}
	next = tokenizer.Next()
	tokenizer.expectKind(next, tkClose)
	return
}

// comma separated list of expressions. used for call arguments and array literals
func parseExprList(tokenizer *Tokenizer, tkOpen, tkClose TokenKind) (result []Expr) {
	next := tokenizer.Next()
	tokenizer.expectKind(next, tkOpen)
	if tokenizer.lookAheadToken.kind != tkClose {
		result = append(result, parseExpr(tokenizer, false))
		for tokenizer.lookAheadToken.kind == TkComma {
			tokenizer.Next()
			result = append(result, parseExpr(tokenizer, false))
		}
	}
	next = tokenizer.Next()
	tokenizer.expectKind(next, tkClose)
	return
}

func parseCall(tokenizer *Tokenizer, callee Expr) (result Call) {
	// parse call expr
	result.Callee = callee
	result.Args = parseExprList(tokenizer, TkOpenBrace, TkCloseBrace)
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, callee.Source(), lastToken.value)
	return
}

func parseArrayLit(tokenizer *Tokenizer) (result ArrayLit) {
	firstToken := tokenizer.lookAheadToken
	result.Items = parseExprList(tokenizer, TkOpenBracket, TkCloseBracket)
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseStmtOrExpr(tokenizer *Tokenizer) (result Expr) {
	switch tokenizer.lookAheadToken.kind {
	case TkVar, TkLet, TkConst:
		return (Expr)(parseVariableDefStmt(tokenizer))
	case TkReturn:
		return (Expr)(parseReturnStmt(tokenizer))
	case TkBreak:
		return (Expr)(parseBreakStmt(tokenizer))
	case TkContinue:
		return (Expr)(parseContinueStmt(tokenizer))
	case TkFor:
		return (Expr)(parseForLoop(tokenizer))
	case TkIf:
		return (Expr)(parseIfStmt(tokenizer))
	}
	return parseExpr(tokenizer, false)
}

func parsePrefixCall(tokenizer *Tokenizer) (result Call) {
	firstToken := tokenizer.lookAheadToken
	op := parseOperator(tokenizer)
	// this would be the place to introduce negative integer literals
	// kind := tokenizer.lookAheadToken.kind
	// if op.source == "-" && (kind == TkIntLit || kind == TkFloatLit) {
	// }

	result.Callee = op
	result.Args = []Expr{parseExpr(tokenizer, true)}
	// other properties (TODO can this be removed?)
	result.Prefix = true
	result.Braced = true
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseExpr(tokenizer *Tokenizer, prefixExpr bool) (result Expr) {
	switch tokenizer.lookAheadToken.kind {
	case TkIdent:
		result = (Expr)(parseIdent(tokenizer))
	case TkOpenCurly:
		result = (Expr)(parseCodeBlock(tokenizer))
	case TkCharLit:
		result = (Expr)(parseCharLit(tokenizer))
	case TkStrLit:
		result = (Expr)(parseStrLit(tokenizer))
	case TkIntLit:
		result = (Expr)(parseIntLit(tokenizer))
	case TkFloatLit:
		result = (Expr)(parseFloatLit(tokenizer))
	case TkOpenBracket:
		result = (Expr)(parseArrayLit(tokenizer))
	case TkOperator:
		if prefixExpr {
			// do not allow prefix prefix expression?
			panic(tokenizer.formatWrongKind(tokenizer.lookAheadToken))
		} else {
			result = (Expr)(parsePrefixCall(tokenizer))
		}
	default:
		panic(tokenizer.formatWrongKind(tokenizer.lookAheadToken))
	}

	// any expression could be the start of a longer expression, this is
	// explorerd here

	lookAhead := tokenizer.lookAheadToken
	switch lookAhead.kind {
	// and and or is an operator token?
	case TkOperator, TkAnd, TkOr:
		result = (Expr)(parseInfixCall(tokenizer, result))
	case TkOpenBrace:
		result = (Expr)(parseCall(tokenizer, result))
	}

	return
}

func parseStructField(tokenizer *Tokenizer) (result StructField) {
	firstToken := tokenizer.lookAheadToken
	result.Name = parseIdent(tokenizer)
	colon := tokenizer.Next()
	tokenizer.expectKind(colon, TkColon)
	result.TypeExpr = parseTypeExpr(tokenizer)
	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseTypeDef(tokenizer *Tokenizer) (result StructDef) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkType)
	result.Name = parseIdent(tokenizer)
	token := tokenizer.Next()
	tokenizer.expectOperator(token, "=")
	token = tokenizer.Next()
	tokenizer.expectIdent(token, "struct")
	openBrace := tokenizer.Next()
	tokenizer.expectKind(openBrace, TkOpenCurly)
	tokenizer.eatSemicolon()
	for tokenizer.lookAheadToken.kind != TkCloseCurly {
		field := parseStructField(tokenizer)
		result.Fields = append(result.Fields, field)
		tokenizer.eatSemicolon()
	}

	lastToken := tokenizer.Next()
	tokenizer.expectKind(lastToken, TkCloseCurly)
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseProcArgumentGroup(tokenizer *Tokenizer) (result []ProcArgument) {
	name := parseIdent(tokenizer)
	result = append(result, ProcArgument{Name: name})
	for tokenizer.lookAheadToken.kind == TkComma {
		_ = tokenizer.Next() // throw away the comma
		name := parseIdent(tokenizer)
		result = append(result, ProcArgument{Name: name})
	}
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkColon)
	typ := parseTypeExpr(tokenizer)
	lastToken := tokenizer.token
	for i := range result {
		result[i].Type = typ
		// the code is not continuous, doing best effort here
		result[i].source = joinSubstr(tokenizer.code, result[i].Name.source, lastToken.value)
	}
	return
}

func parseProcDef(tokenizer *Tokenizer) (result ProcDef) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkProc)
	result.Name = parseIdent(tokenizer)
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkOpenBrace)

	tokenizer.eatSemicolon()
	for tokenizer.lookAheadToken.kind != TkCloseBrace {
		result.Args = append(result.Args, parseProcArgumentGroup(tokenizer)...)
		tokenizer.eatSemicolon()
	}

	token = tokenizer.Next()
	tokenizer.expectKind(token, TkCloseBrace)

	token = tokenizer.Next()
	tokenizer.expectKind(token, TkColon)
	result.ResultType = parseTypeExpr(tokenizer)
	token = tokenizer.Next()
	tokenizer.expectOperator(token, "=")

	result.Body = parseExpr(tokenizer, false)

	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parsePackage(code, filename string) (result PackageDef) {
	result.Name = path.Base(filename)
	result.source = code
	fmt.Println("processing package: ", result.Name)
	var tokenizer = NewTokenizer(code, filename)
	for true {
		switch tokenizer.lookAheadToken.kind {
		//case TkLineComment:
		//	continue
		case TkSemicolon:
			tokenizer.Next()
			continue
		case TkEof:
			return
		case TkType:
			result.TypeDefs = append(result.TypeDefs, parseTypeDef(tokenizer))
			continue
		case TkProc:
			result.ProcDefs = append(result.ProcDefs, parseProcDef(tokenizer))
			continue
		case TkVar, TkLet, TkConst:
			result.Globals = append(result.Globals, parseVariableDefStmt(tokenizer))
			continue
		}
		panic(tokenizer.formatWrongKind(tokenizer.lookAheadToken))
	}
	panic("unreachable")
}
