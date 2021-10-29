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
	tokenizer.expectKind(token, TkOperator)
	result.source = token.value
	return
}

func parseTypeExpr(tokenizer *Tokenizer) (result TypeExpr) {
	// firstToken := tokenizer.lookAheadToken
	result.Ident = parseIdent(tokenizer)
	// TODO this must be used
	if tokenizer.lookAheadToken.kind == TkOpenBrace {
		_ = parseExprList(tokenizer, TkOpenBrace, TkCloseBrace)
	}
	if tokenizer.lookAheadToken.kind == TkOpenBracket {
		_ = parseExprList(tokenizer, TkOpenBracket, TkCloseBracket)
	}
	// lastToken := tokenizer.token
	return
}

func parseReturnStmt(tokenizer *Tokenizer) (result ReturnStmt) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkReturn)
	result.Value = parseExpr(tokenizer)
	return
}

/* (name string, typ TypeExpr, expr Expr) */

func parseVariableDefStmt(tokenizer *Tokenizer) (result VariableDefStmt) {
	next := tokenizer.Next()
	switch next.kind {
	case TkLet:
		result.Kind = SkLet
	case TkVar:
		result.Kind = SkVar
	case TkConst:
		result.Kind = SkConst
	default:
		tokenizer.wrongIdent(next)
	}

	result.Name = parseIdent(tokenizer)
	next = tokenizer.Next()
	if next.kind == TkColon {
		result.TypeExpr = parseTypeExpr(tokenizer)
		next = tokenizer.Next()
	}
	tokenizer.expectKind(next, TkOperator)
	result.Value = parseExpr(tokenizer)
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
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkFor)
	result.LoopIdent = parseIdent(tokenizer)
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkIn)
	result.Collection = parseExpr(tokenizer)
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkDo)
	result.Body = parseExpr(tokenizer)
	return
}

func parseIfStmt(tokenizer *Tokenizer) (result Expr) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIf)
	condition := parseExpr(tokenizer)
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkDo)
	body := parseExpr(tokenizer)
	if tokenizer.lookAheadToken.kind == TkElse {
		tokenizer.Next()
		elseExpr := parseExpr(tokenizer)
		return IfElseStmt{Condition: condition, Body: body, Else: elseExpr}
	}
	return IfStmt{Condition: condition, Body: body}
}

func (tokenizer *Tokenizer) eatSemicolon() {
	for tokenizer.lookAheadToken.kind == TkSemicolon {
		tokenizer.Next()
	}
}

func parseCodeBlock(tokenizer *Tokenizer) (result CodeBlock) {
	startToken := tokenizer.Next()
	tokenizer.expectKind(startToken, TkOpenCurly)
	tokenizer.eatSemicolon()
	for tokenizer.lookAheadToken.kind != TkCloseCurly {
		item := parseStmtOrExpr(tokenizer)
		result.Items = append(result.Items, item)
		tokenizer.eatSemicolon()
	}
	endToken := tokenizer.Next()
	tokenizer.expectKind(endToken, TkCloseCurly)
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
			panic("illegal escale sequence")
		}

		//rune3, rune3Length = utf8.DecodeRuneInString(token.value[1+rune1Len+rune2Len:])
		return
	}

	return CharLit{Rune: rune1}
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
				panic("illegal escale sequence")
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

func parseInfixCall(tokenizer *Tokenizer, lhs Expr) (result Call) {
	operator := parseOperator(tokenizer)
	rhs := parseExpr(tokenizer)
	result = Call{Callee: operator, Args: []Expr{lhs, rhs}}

	if rhsCall, ok := rhs.(Call); ok && !rhsCall.Braced {
		rhsOperator, isIdent := rhsCall.Callee.(Ident)
		if isIdent && OperatorPrecedence[operator.source] > OperatorPrecedence[rhsOperator.source] {
			// operator precedence
			result.Args[1] = rhsCall.Args[0]
			rhsCall.Args[0] = result
			return rhsCall
		}
	}
	return
}

// comma separated list of expressions. used for call arguments and array literals
func parseExprList(tokenizer *Tokenizer, tkOpen, tkClose TokenKind) (result []Expr) {
	next := tokenizer.Next()
	tokenizer.expectKind(next, tkOpen)
	if tokenizer.lookAheadToken.kind != tkClose {
		result = append(result, parseExpr(tokenizer))
		for tokenizer.lookAheadToken.kind == TkComma {
			tokenizer.Next()
			result = append(result, parseExpr(tokenizer))
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
	return
}

func parseArrayLit(tokenizer *Tokenizer) (result ArrayLit) {
	result.Items = parseExprList(tokenizer, TkOpenBracket, TkCloseBracket)
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
	return parseExpr(tokenizer)
}

func parseExpr(tokenizer *Tokenizer) (result Expr) {
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
	case TkOpenBracket:
		result = (Expr)(parseArrayLit(tokenizer))
	default:
		panic(tokenizer.wrongKind(tokenizer.lookAheadToken))
	}

	// any expression could be the start of a longer expression, this is
	// explorerd here

	lookAhead := tokenizer.lookAheadToken
	switch lookAhead.kind {
	case TkOperator:
		result = (Expr)(parseInfixCall(tokenizer, result))
	case TkOpenBrace:
		result = (Expr)(parseCall(tokenizer, result))
	}

	return
}

func parseStructField(tokenizer *Tokenizer) (result StructField) {
	result.Name = parseIdent(tokenizer)
	colon := tokenizer.Next()
	tokenizer.expectKind(colon, TkColon)
	result.TypeExpr = parseTypeExpr(tokenizer)
	return
}

func parseTypeDef(tokenizer *Tokenizer) (result StructDef) {
	var token Token
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkType)
	result.Name = parseIdent(tokenizer)
	token = tokenizer.Next()
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

	token = tokenizer.Next()
	tokenizer.expectKind(token, TkCloseCurly)
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
	for i := range result {
		result[i].Type = typ
	}
	return
}

func parseProcDef(tokenizer *Tokenizer) (result ProcDef) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkProc)
	result.Name = parseIdent(tokenizer)
	token = tokenizer.Next()
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

	result.Body = parseExpr(tokenizer)

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
		panic(tokenizer.wrongKind(tokenizer.lookAheadToken))
	}
	panic("unreachable")
}
