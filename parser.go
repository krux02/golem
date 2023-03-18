package main

import (
	"fmt"
	"path"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

// TODO this is not a function sybol table, it is just an Operator Precedence table
var OperatorPrecedence map[string]int = map[string]int{
	".": 10,
	"*": 7, "/": 7,
	"+": 6, "-": 6,
	">": 5, "<": 5, ">=": 5, "<=": 5,
	"==": 4, "!=": 4,
	"and": 3, "or": 2,
	"=": 1, "+=": 1, "-=": 1, "*=": 1, "/=": 1,
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

	_, ok := OperatorPrecedence[result.source]
	if !ok {
		panic(tokenizer.Errorf(token, "invalid operator '%s'", result.source))
	}
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

func (varDef *VariableDefStmt) ApplyDocComment(tc *TypeChecker, doc DocComment) {
	varDef.Name.Comment = append(varDef.Name.Comment, doc.BaseDoc...)
	for _, it := range doc.NamedDocSections {
		key := it.Name
		value := it.Lines

		if varDef.Name.source != key {
			tc.ReportInvalidDocCommentKey(it)
			continue
		}

		commentRef := &varDef.Name.Comment
		*commentRef = append(*commentRef, value...)
	}
}

var docCommentSectionRegex *regexp.Regexp

func init() {
	//docCommentSectionRegex = regexp.MustCompile(`\s*\([[:alpha:]][[:alnum:]]\)*\s*:`)
	docCommentSectionRegex = regexp.MustCompile(`^\s*([_[:alpha:]][_[:alnum:]]*)\s*:(.*)$`)
}

func (procDef *ProcDef) ApplyDocComment(tc *TypeChecker, doc DocComment) {
	procDef.Name.Comment = append(procDef.Name.Comment, doc.BaseDoc...)

DOCSECTIONS:
	for _, it := range doc.NamedDocSections {
		key := it.Name
		value := it.Lines

		for i := range procDef.Args {
			if procDef.Args[i].Name.source == key {
				commentRef := &procDef.Args[i].Name.Comment
				*commentRef = append(*commentRef, value...)
				continue DOCSECTIONS
			}
		}
		tc.ReportInvalidDocCommentKey(it)
	}
}

func (structDef *StructDef) ApplyDocComment(tc *TypeChecker, doc DocComment) {
	structDef.Name.Comment = append(structDef.Name.Comment, doc.BaseDoc...)
	hasDocComment := len(structDef.Name.Comment) != 0

DOCSECTIONS:
	for _, it := range doc.NamedDocSections {
		key := it.Name
		value := it.Lines

		for i := range structDef.Fields {
			if structDef.Fields[i].Name.source == key {
				commentRef := &structDef.Fields[i].Name.Comment
				*commentRef = append(*commentRef, value...)
				hasDocComment = true
				continue DOCSECTIONS
			}
		}
		tc.ReportInvalidDocCommentKey(it)
	}

	if hasDocComment {
		fmt.Printf("proc def '%s' -- '%v'\n", structDef.Name.source, structDef.Name.Comment)
		for _, field := range structDef.Fields {
			fmt.Printf("   arg '%s' -- '%v'\n", field.Name.source, field.Name.Comment)
		}
	} else {
		fmt.Printf("proc def '%s' --- no doc\n", structDef.Name.source)
	}
}

func (_ FloatLit) ApplyDocComment(tc *TypeChecker, doc DocComment)   { tc.ReportIllegalDocComment(doc) }
func (_ CharLit) ApplyDocComment(tc *TypeChecker, doc DocComment)    { tc.ReportIllegalDocComment(doc) }
func (_ IntLit) ApplyDocComment(tc *TypeChecker, doc DocComment)     { tc.ReportIllegalDocComment(doc) }
func (_ IfExpr) ApplyDocComment(tc *TypeChecker, doc DocComment)     { tc.ReportIllegalDocComment(doc) }
func (_ IfElseExpr) ApplyDocComment(tc *TypeChecker, doc DocComment) { tc.ReportIllegalDocComment(doc) }
func (_ Ident) ApplyDocComment(tc *TypeChecker, doc DocComment)      { tc.ReportIllegalDocComment(doc) }
func (_ Call) ApplyDocComment(tc *TypeChecker, doc DocComment)       { tc.ReportIllegalDocComment(doc) }

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
		panic(tokenizer.formatWrongKind(firstToken))
	}

	result.Name = parseIdent(tokenizer)
	if tokenizer.lookAheadToken.kind == TkColon {
		_ = tokenizer.Next()
		result.TypeExpr = parseTypeExpr(tokenizer)
	}
	if tokenizer.lookAheadToken.kind == TkAssign {
		tokenizer.Next()
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

		result := IfElseExpr{Condition: condition, Body: body, Else: elseExpr}
		lastToken := tokenizer.token
		result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
		return result
		// return IfElseStmt{Condition: condition, Body: body, Else: elseExpr}
	}
	result := IfExpr{Condition: condition, Body: body}
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
			panic(tokenizer.Errorf(token, "invalid escape \\%c in char literal", rune2))
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
				panic(tokenizer.Errorf(token, "invalid escape \\%c in string literal", rune))
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
	result.Value = int64(intValue)
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
	result.Braced = true
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
	case TkPrefixDocComment:
		result = (Expr)(parseDocComment(tokenizer))
	case TkVar, TkLet, TkConst:
		result = (Expr)(parseVariableDefStmt(tokenizer))
	case TkReturn:
		result = (Expr)(parseReturnStmt(tokenizer))
	case TkBreak:
		result = (Expr)(parseBreakStmt(tokenizer))
	case TkContinue:
		result = (Expr)(parseContinueStmt(tokenizer))
	case TkFor:
		result = (Expr)(parseForLoop(tokenizer))
	case TkIf:
		result = (Expr)(parseIfStmt(tokenizer))
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
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parseColonExpr(tokenizer *Tokenizer, lhs Expr) Expr {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkColon)
	rhs := parseTypeExpr(tokenizer)

	result := ColonExpr{
		Lhs: lhs,
		Rhs: rhs,
	}
	result.source = joinSubstr(tokenizer.code, lhs.Source(), rhs.Source())
	return result
}

type DocCommentScanner struct {
	rawsource string
}

func (this *DocCommentScanner) Next() (result string) {
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

func parseDocComment(tokenizer *Tokenizer) (result DocComment) {
	token := tokenizer.Next()
	if token.kind != TkPrefixDocComment {
		panic(tokenizer.formatWrongKind(token))
	}
	result.source = token.value
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
			section.source = joinSubstr(result.source, section.Name, section.Lines[lastIdx])
		} else {
			section.source = section.Name
		}
	}
	return result
}

func attachDocComment(expr Expr, target string, value string) (result bool) {
	switch ex := expr.(type) {
	case Ident:
		if ex.source == target {
			ex.Comment = append(ex.Comment, value)
			fmt.Printf("attaching doc comment to %s\n", ex.source)
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
		result = attachDocComment(ex.Callee, target, value)
		for _, arg := range ex.Args {
			if result {
				return
			}
			result = attachDocComment(arg, target, value)
		}
		return
	case ColonExpr:
		return false
	case VariableDefStmt:
		result = attachDocComment(ex.Name, target, value)
	case ForLoopStmt:
		return false
	case IfExpr:
		return false
	case IfElseExpr:
		return false
	case ReturnStmt:
		return false
	case BreakStmt:
		return false
	case ContinueStmt:
		return false
	}
	panic("not implementede")
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
	case TkOpenBrace:
		exprList := parseExprList(tokenizer, TkOpenBrace, TkCloseBrace)
		if len(exprList) != 1 {
			panic("braced expression must have a single brace")
		}
		result = exprList[0]
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

	for true {
		lookAhead := tokenizer.lookAheadToken
		switch lookAhead.kind {
		// and and or is an operator token?
		case TkOperator, TkAnd, TkOr, TkAssign:
			result = (Expr)(parseInfixCall(tokenizer, result))
		case TkOpenBrace:
			result = (Expr)(parseCall(tokenizer, result))
		case TkColon:
			result = (Expr)(parseColonExpr(tokenizer, result))
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

						fmt.Printf("could not attach doc comment <%s> to <%s>\n", comment, result.Source())
						fmt.Printf("key: <%s>, value: <%s> \n", target, comment)
					}
				} else {
					panic("invalid doc comment, proper error message not implemented")
				}
			}
			return
		default:
			return
		}
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
	tokenizer.expectKind(token, TkAssign)
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
	tokenizer.expectKind(token, TkAssign)

	result.Body = parseExpr(tokenizer, false)

	lastToken := tokenizer.token
	result.source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return
}

func parsePackage(code, filename string) (result PackageDef) {
	result.Name = path.Base(filename)
	result.source = code
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
			typeDef := parseTypeDef(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, typeDef)
			continue
		case TkProc:
			procDef := parseProcDef(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, procDef)
			continue
		case TkVar, TkLet, TkConst:
			varDef := parseVariableDefStmt(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, varDef)
			continue
		case TkPrefixDocComment:
			docComment := parseDocComment(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, docComment)
			continue
		}
		panic(tokenizer.formatWrongKind(tokenizer.lookAheadToken))
	}
	panic("unreachable")
}
