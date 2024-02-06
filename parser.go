package main

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

func OperatorPrecedence(op string) int {
	switch op {
	case ".":
		return 10
	case ":":
		return 9
	case "*", "/":
		return 7
	case "+", "-":
		return 6
	case ">", "<", ">=", "<=":
		return 5
	case "==", "!=":
		return 4
	case "and", "or":
		return 2
	case "=", "+=", "-=", "*=", "/=":
		return 1
	}
	// operator precedence does not exist
	return -1
}

func parseIdent(tokenizer *Tokenizer) (result Ident) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
	result.Source = token.value
	return result
}

func parseOperator(tokenizer *Tokenizer) (result Ident) {
	token := tokenizer.Next()
	tk := token.kind
	if tk != TkOperator && tk != TkAssign && tk != TkAnd && tk != TkOr {
		tokenizer.reportWrongKind(token)
	}
	// ensure operator precedence exists
	precedence := OperatorPrecedence(token.value)
	if precedence < 0 {
		tokenizer.reportError(token, "invalid operator '%s'", token.value)
	}
	result.Source = token.value

	return result
}

func parseTypeContext(tokenizer *Tokenizer) (result TypeContext) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkType)
	result.Expr = TypeExpr(parseExpr(tokenizer, false))
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

func parseReturnExpr(tokenizer *Tokenizer) (result ReturnExpr) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkReturn)
	result.Value = parseExpr(tokenizer, false)
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

func parseVarExpr(tokenizer *Tokenizer) (result VarExpr) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkVar)
	result.Expr = parseExpr(tokenizer, false)
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
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
		tokenizer.reportWrongKind(firstToken)
		// TODO what now? Create an invalid expression? this default value of result is in valid stmt.
		return result
	}
	expr := parseExpr(tokenizer, false)
	if Lhs, Rhs, ok := MatchAssign(expr); ok {
		result.Value = Rhs
		expr = Lhs
	}
	if colonExpr, ok := MatchColonExpr(expr); ok {
		result.TypeExpr = colonExpr.Rhs
		expr = colonExpr.Lhs
	}
	result.Name = expr.(Ident)
	result.Source = joinSubstr(tokenizer.code, firstToken.value, expr.GetSource())
	return result
}

func parseBreakStmt(tokenizer *Tokenizer) (result BreakStmt) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkBreak)
	result.Source = token.value
	return result
}

func parseContinueStmt(tokenizer *Tokenizer) (result ContinueStmt) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkContinue)
	result.Source = token.value
	return result
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
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

func parseWhileLoop(tokenizer *Tokenizer) (result WhileLoopStmt) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkWhile)
	result.Condition = parseExpr(tokenizer, false)
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkDo)
	result.Body = parseExpr(tokenizer, false)
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
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

		result := IfElseExpr{Condition: condition, Body: body, Else: elseExpr}
		lastToken := tokenizer.token
		result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
		return result
		// return IfElseStmt{Condition: condition, Body: body, Else: elseExpr}
	}
	result := IfExpr{Condition: condition, Body: body}
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
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
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

func parseCharLit(tokenizer *Tokenizer) (result CharLit) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkCharLit)
	quotelessValue := token.value[1 : len(token.value)-1]
	if len(quotelessValue) == 0 {
		tokenizer.reportError(token, "char token must not be empty")
	}
	result.Source = token.value
	rune1, rune1Len := utf8.DecodeRuneInString(quotelessValue)
	if rune1 == '\\' {
		rune2, rune2Len := utf8.DecodeRuneInString(quotelessValue[rune1Len:])
		if len(quotelessValue) != rune1Len+rune2Len {
			tokenizer.reportError(token, "char token with escape sequence must contain exactly two characters")
		}
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
			tokenizer.reportError(token, "invalid escape \\%c in char literal", rune2)
			result.Rune = '\u29EF'
		}
		return result
	}
	if len(quotelessValue) != rune1Len {
		tokenizer.reportError(token, "char token too long")
	}
	result.Rune = rune1
	return result
}

func parseStrLit(tokenizer *Tokenizer) (result StrLit) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkStrLit)
	result.Source = token.value
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
				tokenizer.reportError(token, "invalid escape \\%c in string literal", rune)
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
	return result
}

func parseIntLit(tokenizer *Tokenizer) (result IntLit) {
	token := tokenizer.Next()
	tokenizer.expectKind2(token, TkIntLit, TkHexLit)
	result = IntLit{Source: token.value}

	if token.kind == TkIntLit {
		intValue := Must(strconv.Atoi(token.value))
		result.Value = int64(intValue)
	} else {
		str := token.value[2:]
		var intValue uint64
		for _, c := range str {
			var nibble uint64
			switch c {
			case '0':
				nibble = 0
			case '1':
				nibble = 1
			case '2':
				nibble = 2
			case '3':
				nibble = 3
			case '4':
				nibble = 4
			case '5':
				nibble = 5
			case '6':
				nibble = 6
			case '7':
				nibble = 7
			case '8':
				nibble = 8
			case '9':
				nibble = 9
			case 'a', 'A':
				nibble = 10
			case 'b', 'B':
				nibble = 11
			case 'c', 'C':
				nibble = 12
			case 'd', 'D':
				nibble = 13
			case 'e', 'E':
				nibble = 14
			case 'f', 'F':
				nibble = 15
			default:
				panic(fmt.Errorf("internal error, %c token.value: %s", c, token.value))
			}
			intValue = (intValue << 4) | nibble
		}
		result.Value = int64(intValue)
	}

	result.Type = GetIntLitType(result.Value)
	return result
}

func parseFloatLit(tokenizer *Tokenizer) (result FloatLit) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkFloatLit)
	result.Source = token.value
	floatValue, err := strconv.ParseFloat(token.value, 64)
	if err != nil {
		panic("internal error invalid float token")
	}
	result.Value = floatValue
	return result
}

func applyOperatorPrecedenceFromLeft(tokenizerCode string, lhs Expr, op Ident, rhs Expr) Call {
	if rhsCall, ok := rhs.(Call); ok && !rhsCall.Braced {
		rhsOperator, isIdent := rhsCall.Callee.(Ident)
		if isIdent && OperatorPrecedence(op.Source) >= OperatorPrecedence(rhsOperator.Source) {
			newLhs := applyOperatorPrecedenceFromLeft(tokenizerCode, lhs, op, rhsCall.Args[0])
			newRhs := rhsCall.Args[1]
			return Call{
				Source: joinSubstr(tokenizerCode, newLhs.GetSource(), newRhs.GetSource()),
				Callee: rhsOperator,
				Args:   []Expr{newLhs, newRhs},
			}
		}
	}

	return Call{
		Source: joinSubstr(tokenizerCode, lhs.GetSource(), rhs.GetSource()),
		Callee: op,
		Args:   []Expr{lhs, rhs},
	}
}

func parseInfixCall(tokenizer *Tokenizer, lhs Expr) (result Call) {
	operator := parseOperator(tokenizer)
	rhs := parseExpr(tokenizer, false)
	// parseExpr recursively eats all follow operator calls. Therefore `lhs` is
	// the _new_ operatore that needs to be applied in the expression from the
	// left.
	result = applyOperatorPrecedenceFromLeft(tokenizer.code, lhs, operator, rhs)
	return result
}

// comma separated list of expressions. used for call arguments and array literals
func parseExprList(tokenizer *Tokenizer, tkOpen, tkClose TokenKind) (result ExprList) {
	firstToken := tokenizer.lookAheadToken
	next := tokenizer.Next()
	tokenizer.expectKind(next, tkOpen)
	if tokenizer.lookAheadToken.kind != tkClose {
		result.Items = append(result.Items, parseExpr(tokenizer, false))
		for tokenizer.lookAheadToken.kind == TkComma {
			tokenizer.Next()
			result.Items = append(result.Items, parseExpr(tokenizer, false))
		}
	}
	next = tokenizer.Next()
	tokenizer.expectKind(next, tkClose)
	result.Source = joinSubstr(tokenizer.code, firstToken.value, next.value)
	return result
}

func parseCall(tokenizer *Tokenizer, callee Expr) (result Call) {
	// parse call expr
	result.Callee = callee
	result.Args = parseExprList(tokenizer, TkOpenBrace, TkCloseBrace).Items
	result.Braced = true
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, callee.GetSource(), lastToken.value)
	return result
}

func parseBracketCall(tokenizer *Tokenizer, callee Expr) (result Call) {
	// parse call expr
	var ident Ident
	ident.Source = tokenizer.lookAheadToken.value
	result.Callee = ident
	result.Args = append(result.Args, callee)
	result.Args = append(result.Args, parseExprList(tokenizer, TkOpenBracket, TkCloseBracket).Items...)
	result.Braced = true
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, callee.GetSource(), lastToken.value)
	return result
}

func parseArrayLit(tokenizer *Tokenizer) (result ArrayLit) {
	exprList := parseExprList(tokenizer, TkOpenBracket, TkCloseBracket)
	result.Items = exprList.Items
	result.Source = exprList.Source
	return result
}

func parseStmtOrExpr(tokenizer *Tokenizer) (result Expr) {
	switch tokenizer.lookAheadToken.kind {
	case TkPrefixDocComment:
		result = (Expr)(parseDocComment(tokenizer))
	case TkVar, TkLet, TkConst:
		result = (Expr)(parseVariableDefStmt(tokenizer))
	case TkBreak:
		result = (Expr)(parseBreakStmt(tokenizer))
	case TkContinue:
		result = (Expr)(parseContinueStmt(tokenizer))
	case TkFor:
		result = (Expr)(parseForLoop(tokenizer))
	case TkWhile:
		result = (Expr)(parseWhileLoop(tokenizer))
	default:

		result = parseExpr(tokenizer, false)
	}
	return result
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
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

type DocCommentScanner struct {
	rawsource string
}

func (this *DocCommentScanner) Next() string {
	i := strings.Index(this.rawsource, "##") + 2
	if i < 0 {
		this.rawsource = ""
		return ""
	}
	this.rawsource = this.rawsource[i:]
	j := strings.IndexByte(this.rawsource, '\n')
	if j < 0 {
		return this.rawsource
	}
	return this.rawsource[:j]
}

func (this *DocCommentScanner) HasNext() bool {
	return strings.Contains(this.rawsource, "##")
}

var docCommentSectionRegex *regexp.Regexp

func init() {
	//docCommentSectionRegex = regexp.MustCompile(`\s*\([[:alpha:]][[:alnum:]]\)*\s*:`)
	docCommentSectionRegex = regexp.MustCompile(`^\s*([_[:alpha:]][_[:alnum:]]*)\s*:(.*)$`)
}

func parseDocComment(tokenizer *Tokenizer) (result DocComment) {
	if !tokenizer.expectKind(tokenizer.lookAheadToken, TkPrefixDocComment) {
		return
	}
	token := tokenizer.Next()
	result.Source = token.value
	commentScanner := &DocCommentScanner{token.value}
	for commentScanner.HasNext() {
		line := commentScanner.Next()
		if line != "" {
			if matches := docCommentSectionRegex.FindStringSubmatch(line); len(matches) > 0 {
				name := matches[1]
				value := matches[2]
				section := NamedDocSection{Name: name}
				if value != "" {
					section.Lines = append(section.Lines, value)
				}
				result.NamedDocSections = append(result.NamedDocSections, section)
			} else if len(result.NamedDocSections) == 0 {
				result.BaseDoc = append(result.BaseDoc, line)
			} else {
				idx := len(result.NamedDocSections) - 1
				result.NamedDocSections[idx].Lines = append(result.NamedDocSections[idx].Lines, line)
			}
		}
	}
	for i := range result.NamedDocSections {
		section := &result.NamedDocSections[i]
		lastIdx := len(section.Lines) - 1
		if lastIdx >= 0 {
			section.Source = joinSubstr(result.Source, section.Name, section.Lines[lastIdx])
		} else {
			section.Source = section.Name
		}
	}
	return result
}

func attachDocComment(expr Expr, target string, value string) bool {
	switch ex := expr.(type) {
	case Ident:
		if ex.Source == target {
			ex.Comment = append(ex.Comment, value)
			fmt.Printf("attaching doc comment to %s\n", ex.Source)
			fmt.Printf("%s\n", value)
			return true
		} else {
			return false
		}
	case CodeBlock:
		return false
	case StrLit:
		return false
	case IntLit:
		return false
	case FloatLit:
		return false
	case ArrayLit:
		return false
	case CharLit:
		return false
	case Call:
		result := attachDocComment(ex.Callee, target, value)
		for _, arg := range ex.Args {
			if result {
				return result
			}
			result = attachDocComment(arg, target, value)
		}
		return result
	case ColonExpr:
		return false
	case VariableDefStmt:
		return attachDocComment(ex.Name, target, value)
	case ForLoopStmt:
		return false
	case IfExpr:
		return false
	case IfElseExpr:
		return false
	case ReturnExpr:
		return false
	case BreakStmt:
		return false
	case ContinueStmt:
		return false
	}
	panic("not implemented")
}

func parseNilLit(tokenizer *Tokenizer) NilLit {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkNilLit)
	return NilLit{Source: token.value, Type: TypeNilPtr}
}

func parseExpr(tokenizer *Tokenizer, prefixExpr bool) (result Expr) {
	switch tokenizer.lookAheadToken.kind {
	case TkIdent:
		result = (Expr)(parseIdent(tokenizer))
	case TkOpenCurly:
		result = (Expr)(parseCodeBlock(tokenizer))
	case TkCharLit:
		result = (Expr)(parseCharLit(tokenizer))
	case TkIf:
		result = (Expr)(parseIfStmt(tokenizer))
	case TkStrLit:
		result = (Expr)(parseStrLit(tokenizer))
	case TkIntLit:
		result = (Expr)(parseIntLit(tokenizer))
	case TkHexLit:
		result = (Expr)(parseIntLit(tokenizer))
	case TkFloatLit:
		result = (Expr)(parseFloatLit(tokenizer))
	case TkOpenBracket:
		result = (Expr)(parseArrayLit(tokenizer))
	case TkOpenBrace:
		token := tokenizer.lookAheadToken
		exprList := parseExprList(tokenizer, TkOpenBrace, TkCloseBrace)

		if len(exprList.Items) == 1 {
			result = exprList.Items[0]
		} else {
			// TODO, this report is not on the right location. It should be on
			// `exprList`, not token.
			tokenizer.reportError(token, "braced expression must contain a single expression, but has %d", len(exprList.Items))
			result = newErrorNode(exprList)
		}
	case TkOperator:
		if prefixExpr {
			// do not allow prefix prefix expression?
			tokenizer.reportWrongKind(tokenizer.lookAheadToken)
		}
		result = (Expr)(parsePrefixCall(tokenizer))
	case TkType:
		result = (Expr)(parseTypeContext(tokenizer))
	case TkNilLit:
		result = (Expr)(parseNilLit(tokenizer))
	case TkReturn:
		result = (Expr)(parseReturnExpr(tokenizer))
	case TkVar:
		result = (Expr)(parseVarExpr(tokenizer))
	default:
		tokenizer.reportWrongKind(tokenizer.lookAheadToken)
		result = (Expr)(InvalidTokenExpr{tokenizer.Next()})
	}

	// any expression could be the start of a longer expression, this is
	// explorerd here

	for true {
		lookAhead := tokenizer.lookAheadToken
		switch lookAhead.kind {
		// TkAnd, TkOr, TkAssign is an operator token?
		case TkOperator, TkAnd, TkOr, TkAssign:
			result = (Expr)(parseInfixCall(tokenizer, result))
		case TkOpenBrace:
			result = (Expr)(parseCall(tokenizer, result))
		case TkOpenBracket:
			result = (Expr)(parseBracketCall(tokenizer, result))
		case TkPostfixDocComment:
			comment := tokenizer.Next().value
			commentScanner := &DocCommentScanner{comment}
			for commentScanner.HasNext() {
				commentLine := commentScanner.Next()
				idx := strings.IndexByte(commentLine, ':')
				if idx > 0 {
					target := commentLine[:idx]
					for target[0] == ' ' || target[0] == '\t' {
						target = target[1:]
					}
					comment := commentLine[idx+1:]
					for comment[0] == ' ' || comment[0] == '\t' {
						comment = comment[1:]
					}

					success := attachDocComment(result, target, comment)
					if !success {

						fmt.Printf("could not attach doc comment <%s> to <%s>\n", comment, result.GetSource())
						fmt.Printf("key: <%s>, value: <%s> \n", target, comment)
					}
				} else {
					panic("invalid doc comment, proper error message not implemented")
				}
			}
			return result
		default:
			return result
		}
	}

	return result
}

func parseTypeDef(tokenizer *Tokenizer) Expr {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkType)

	var annotations StrLit
	if tokenizer.lookAheadToken.kind == TkStrLit {
		annotations = parseStrLit(tokenizer)
	}
	name := parseIdent(tokenizer)
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkAssign)
	kindToken := tokenizer.Next()
	body := parseCodeBlock(tokenizer)
	source := joinSubstr(tokenizer.code, firstToken.value, body.Source)

	switch kindToken.kind {
	case TkStruct:
		result := StructDef{Name: name, Source: source, Annotations: annotations}
		result.Name = name
		for _, it := range body.Items {
			colonExpr, ok := MatchColonExpr(it)
			if !ok {
				//tokenizer.reportError(it, "expect colon expr")
				panic("expect colon expr")
			}
			result.Fields = append(result.Fields, colonExpr)
		}
		return result
	case TkEnum:
		result := EnumDef{Name: name, Source: source, Annotations: annotations}
		for _, it := range body.Items {
			ident, ok := it.(Ident)
			if !ok {
				panic("parser error handling not implemented")
			}
			result.Values = append(result.Values, ident)
		}
		return result
	default:
		tokenizer.expectKind2(kindToken, TkStruct, TkEnum)
		panic("parser error handling not implemented")
	}
}

func MatchColonExpr(expr Expr) (colonExpr ColonExpr, ok bool) {
	call, ok := expr.(Call)
	if !ok {
		return colonExpr, false
	}
	op, ok := call.Callee.(Ident)
	if !ok || op.Source != ":" || len(call.Args) != 2 {
		return colonExpr, false
	}
	colonExpr.Lhs = call.Args[0]
	colonExpr.Rhs = TypeExpr(call.Args[1])
	colonExpr.Source = call.Source
	return colonExpr, true
}

func MatchAssign(expr Expr) (lhs, rhs Expr, ok bool) {
	call, ok := expr.(Call)
	if !ok {
		return lhs, rhs, false
	}
	op, ok := call.Callee.(Ident)
	if !ok || op.Source != "=" || len(call.Args) != 2 {
		return lhs, rhs, false
	}
	lhs = call.Args[0]
	rhs = call.Args[1]
	return lhs, rhs, true
}

func MatchBracketCall(expr Expr) (lhs Expr, args []Expr, ok bool) {
	call, ok := expr.(Call)
	if !ok {
		return lhs, args, false
	}
	op, ok := call.Callee.(Ident)
	if !ok || op.Source != "[" {
		return lhs, args, false
	}
	lhs = call.Args[0]
	args = call.Args[1:]
	return lhs, args, true
}

func parseProcArgumentNoType(arg Expr) ProcArgument {
	var mutable = false
	if varExpr, ok := arg.(VarExpr); ok {
		mutable = true
		arg = varExpr.Expr
	}
	if ident, isIdent := arg.(Ident); isIdent {
		return ProcArgument{Source: ident.Source, Name: ident, Mutable: mutable}
	}
	panic(fmt.Errorf("expected identifier but got %T (%s)", arg, AstFormat(arg)))
}

func parseProcDef(tokenizer *Tokenizer) (result ProcDef) {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkProc)

	if tokenizer.lookAheadToken.kind == TkStrLit {
		result.Annotations = parseStrLit(tokenizer)
	}

	expr := parseExpr(tokenizer, false)

	if lhs, rhs, isAssign := MatchAssign(expr); isAssign {
		result.Body = rhs
		expr = lhs
	}

	colonExpr, isColonExpr := MatchColonExpr(expr)
	if !isColonExpr {
		tokenizer.reportError(firstToken, "proc def requres colon to specify return type")
		return
	}
	result.ResultType = colonExpr.Rhs

	call, isCall := colonExpr.Lhs.(Call)
	if !isCall {
		tokenizer.reportError(firstToken, "proc def requires an argument list")
		return
	}
	name, isIdent := call.Callee.(Ident)
	if !isIdent {
		tokenizer.reportError(firstToken, "proc keyword must be followed by an identifier")
		return
	}

	result.Name = name
	argsRaw := call.Args

	var newArgs []ProcArgument
	for _, arg := range argsRaw {
		if colonExpr, ok := MatchColonExpr(arg); ok {
			newArgs = append(newArgs, parseProcArgumentNoType(colonExpr.Lhs))
			for i := range newArgs {
				newArgs[i].Type = colonExpr.Rhs
			}
			result.Args = append(result.Args, newArgs...)
			newArgs = newArgs[:0]
		} else {
			newArgs = append(newArgs, parseProcArgumentNoType(arg))
		}
	}
	if len(newArgs) > 0 {
		panic(fmt.Errorf("arguments have no type: %+v", newArgs))
	}
	result.Source = joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)
	return result
}

func parseEmitStmt(tokenizer *Tokenizer) EmitStmt {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkEmit)
	strLit := parseStrLit(tokenizer)
	return EmitStmt{Source: joinSubstr(tokenizer.code, firstToken.value, strLit.Source), Value: strLit}
}

func parseStaticExpr(tokenizer *Tokenizer) StaticExpr {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkStatic)
	expr := parseExpr(tokenizer, false)
	return StaticExpr{Source: joinSubstr(tokenizer.code, firstToken.value, expr.GetSource()), Expr: expr}
}

func parseImportStmt(tokenizer *Tokenizer) ImportStmt {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkImport)
	strLit := parseStrLit(tokenizer)
	return ImportStmt{Source: joinSubstr(tokenizer.code, firstToken.value, strLit.Source), StrLit: strLit}
}

func parsePackage(tokenizer *Tokenizer) (result PackageDef, errors []ParseError) {
	result.Name = strings.TrimSuffix(filepath.Base(tokenizer.filename), ".golem")
	result.Source = tokenizer.code
	result.WorkDir = filepath.Dir(tokenizer.filename)
	for true {
		switch tokenizer.lookAheadToken.kind {
		//case TkLineComment:
		//	continue
		case TkSemicolon:
			tokenizer.Next()
			continue
		case TkEof:
			return result, tokenizer.errors
		case TkEmit:
			emitStmt := parseEmitStmt(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, emitStmt)
			continue
		case TkType:
			typeDef := parseTypeDef(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, typeDef)
			continue
		case TkProc:
			procDef := parseProcDef(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, procDef)
			continue
		case TkImport:
			importStmt := parseImportStmt(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, importStmt)
			continue
		case TkVar, TkLet, TkConst:
			varDef := parseVariableDefStmt(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, varDef)
			continue
		case TkStatic:
			expr := parseStaticExpr(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, expr)
			continue
		case TkPrefixDocComment:
			docComment := parseDocComment(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, docComment)
			continue
		default:
			tokenizer.reportWrongKind(tokenizer.Next())
			continue
		}
	}
	panic("unreachable")
}
