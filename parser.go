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

func parseTypeExpr(tokenizer *Tokenizer) (result TypeExpr) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
	result.Ident = token.value

	if tokenizer.lookAheadToken.kind == TkOpenBracket {
		// TODO this must be used
		_ = parseArrayLit(tokenizer)
	}
	return
}

func parseReturnStmt(tokenizer *Tokenizer) (result ReturnStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "return")
	result.Value = parseExpr(tokenizer)
	return
}

/* (name string, typ TypeExpr, expr Expr) */

func parseVariableDefStmt(tokenizer *Tokenizer) (result VariableDefStmt) {
	next := tokenizer.Next()
	switch next.value {
	case "let":
		result.Kind = SkLet
	case "var":
		result.Kind = SkVar
	case "const":
		result.Kind = SkConst
	default:
		tokenizer.wrongIdent(next)
	}

	next = tokenizer.Next()
	tokenizer.expectKind(next, TkIdent)
	result.Name = next.value
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
	tokenizer.expectIdent(token, "break")
	result.source = token.value
	return
}

func parseContinueStmt(tokenizer *Tokenizer) (result ContinueStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "continue")
	result.source = token.value
	return
}

func parseForLoop(tokenizer *Tokenizer) (result ForLoopStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "for")
	result.LoopIdent = parseIdent(tokenizer)
	token = tokenizer.Next()
	tokenizer.expectOperator(token, "in")
	result.Collection = parseExpr(tokenizer)
	result.Body = parseCodeBlock(tokenizer)
	return
}

func parseIfStmt(tokenizer *Tokenizer) (result Expr) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "if")
	condition := parseExpr(tokenizer)
	body := parseCodeBlock(tokenizer)
	if tokenizer.lookAheadToken.kind == TkIdent && tokenizer.lookAheadToken.value == "else" {
		tokenizer.Next()
		elseBlock := parseCodeBlock(tokenizer)
		return IfElseStmt{Condition: condition, Body: body, Else: elseBlock}
	}
	return IfStmt{Condition: condition, Body: body}
}

func parseStmtOrExpr(tokenizer *Tokenizer) (result Expr) {
	lookAhead := tokenizer.lookAheadToken
	if lookAhead.kind == TkIdent {
		switch lookAhead.value {
		case "var", "let", "const":
			return (Expr)(parseVariableDefStmt(tokenizer))
		case "return":
			return (Expr)(parseReturnStmt(tokenizer))
		case "for":
			return (Expr)(parseForLoop(tokenizer))
		case "break":
			return (Expr)(parseBreakStmt(tokenizer))
		case "continue":
			return (Expr)(parseContinueStmt(tokenizer))
		case "if":
			return (Expr)(parseIfStmt(tokenizer))
		}
	}
	return parseExpr(tokenizer)
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
	intValue, err := strconv.Atoi(token.value)
	if err != nil {
		panic("internal error invalid int token")
	}
	result.Value = intValue
	return
}

func parseIdent(tokenizer *Tokenizer) (result Ident) {
	// this func implementation is a joke, but it should be consistent with the other parse thingies
	token := tokenizer.Next()
	result.Name = token.value
	return
}

func parseInfixCall(tokenizer *Tokenizer, lhs Expr) (result Call) {
	token := tokenizer.Next()
	operator := Ident{Name: token.value}
	rhs := parseExpr(tokenizer)
	result = Call{Sym: operator, Args: []Expr{lhs, rhs}}

	if rhsCall, ok := rhs.(Call); ok && !rhsCall.Braced {
		rhsOperator := rhsCall.Sym
		if OperatorPrecedence[operator.Name] > OperatorPrecedence[rhsOperator.Name] {
			// operator precedence
			result.Args[1] = rhsCall.Args[0]
			rhsCall.Args[0] = result
			return rhsCall
		}
	}
	return
}

func parseCall(tokenizer *Tokenizer, callee Expr) (result Call) {
	// parse call expr
	var args []Expr
	tokenizer.Next()
	// TODO this need a clean revisit
	for true {
		switch tokenizer.lookAheadToken.kind {
		case TkIdent, TkStrLit, TkIntLit:
			args = append(args, parseExpr(tokenizer))
			switch tokenizer.lookAheadToken.kind {
			case TkComma:
				tokenizer.Next()
				continue
			case TkCloseBrace:
				// TkCloseBrace is already handled in the outer switch
				// this is really ugly, why? I must be doing something wrong
				continue
			default:
			}
			panic(tokenizer.wrongKind(tokenizer.lookAheadToken))
		case TkCloseBrace:
			tokenizer.Next()
			result.Sym = callee.(Ident)
			result.Args = args
			return
		}
		panic(tokenizer.wrongKind(tokenizer.lookAheadToken))
	}
	panic("unreachable")
}

func parseArrayLit(tokenizer *Tokenizer) (result ArrayLit) {
	next := tokenizer.Next()
	tokenizer.expectKind(next, TkOpenBracket)
	if tokenizer.lookAheadToken.kind != TkCloseBracket {
		result.Items = append(result.Items, parseExpr(tokenizer))
		for tokenizer.lookAheadToken.kind == TkComma {
			tokenizer.Next()
			result.Items = append(result.Items, parseExpr(tokenizer))
		}
	}
	next = tokenizer.Next()
	tokenizer.expectKind(next, TkCloseBracket)
	return
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
		tokenizer.wrongKind(tokenizer.lookAheadToken)
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

func parseTypeDef(tokenizer *Tokenizer) (result StructDef) {
	var token Token
	token = tokenizer.Next()
	tokenizer.expectIdent(token, "type")
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
	result.Name = token.value
	token = tokenizer.Next()
	tokenizer.expectOperator(token, "=")
	token = tokenizer.Next()
	tokenizer.expectIdent(token, "struct")

	openBrace := tokenizer.Next()
	tokenizer.expectKind(openBrace, TkOpenCurly)

	token = tokenizer.Next()

	for token.kind == TkSemicolon {
		token = tokenizer.Next()
	}

	for token.kind == TkIdent {
		var structField StructField
		name := token
		tokenizer.expectKind(name, TkIdent)
		structField.Name = name.value
		colon := tokenizer.Next()
		tokenizer.expectKind(colon, TkColon)
		structField.TypeExpr = parseTypeExpr(tokenizer)
		token = tokenizer.Next()
		tokenizer.expectKind(token, TkSemicolon)
		for token.kind == TkSemicolon {
			token = tokenizer.Next()
		}
		result.Fields = append(result.Fields, structField)
	}

	tokenizer.expectKind(token, TkCloseCurly)
	return
}

func parseProcDef(tokenizer *Tokenizer) (result ProcDef) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "proc")
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
	result.Name = token.value
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkOpenBrace)
	token = tokenizer.Next()

	for token.kind == TkIdent {
		startIndex := len(result.Args)

		result.Args = append(result.Args, ProcArgument{Name: token.value})
		token = tokenizer.Next()
		for token.kind == TkComma {
			token = tokenizer.Next()
			tokenizer.expectKind(token, TkIdent)
			result.Args = append(result.Args, ProcArgument{Name: token.value})
			token = tokenizer.Next()
		}

		tokenizer.expectKind(token, TkColon)

		typ := parseTypeExpr(tokenizer)

		for i := startIndex; i < len(result.Args); i++ {
			result.Args[i].Type = typ
		}
		token = tokenizer.Next()
		if token.kind == TkSemicolon {
			token = tokenizer.Next()
		}
	}
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
		case TkIdent:
			switch tokenizer.lookAheadToken.value {
			case "type":
				result.TypeDefs = append(result.TypeDefs, parseTypeDef(tokenizer))
				continue
			case "proc":
				result.ProcDefs = append(result.ProcDefs, parseProcDef(tokenizer))
				continue
			case "let", "var", "const":
				result.Globals = append(result.Globals, parseVariableDefStmt(tokenizer))
				continue
			default:
			}
			panic(tokenizer.wrongIdent(tokenizer.lookAheadToken))
		}
		panic(tokenizer.wrongKind(tokenizer.lookAheadToken))
	}
	panic("unreachable")
}
