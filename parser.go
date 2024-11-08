package main

import (
	"fmt"
	"math/big"
	"path/filepath"
	"regexp"
	"strings"
	"unsafe"
)

func OperatorPrecedence(op string) int {
	switch op {
	case ".":
		return 12
	case "->":
		return 11
	case ":":
		return 10
	case "|":
		return 9
	case "*", "/":
		return 8
	case "+", "-":
		return 7
	case ">", "<", ">=", "<=":
		return 6
	case "in", "notin":
		return 5
	case "==", "!=":
		return 4
	case "and", "or":
		return 3
	case "=", "+=", "-=", "*=", "/=":
		return 2
	}
	// operator precedence does not exist
	return -1
}

func parseIdent(tokenizer *Tokenizer) *Ident {
	token := tokenizer.Next()
	tokenizer.expectKind2(token, TkIdent, TkQuotedIdent)
	return &Ident{
		Source: token.value,
	}
}

func parseInfixOperator(tokenizer *Tokenizer) *Ident {
	token := tokenizer.Next()
	tk := token.kind
	if tk != TkOperator {
		tokenizer.reportWrongKind(token)
	}
	// ensure operator precedence exists
	precedence := OperatorPrecedence(token.value)
	if precedence < 0 {
		tokenizer.reportError(token, "invalid infix operator '%s'", token.value)
	}
	return &Ident{
		Source: token.value,
	}
}

func eatSemicolons(tokenizer *Tokenizer) {
	for tokenizer.lookAheadToken.kind == TkSemicolon || tokenizer.lookAheadToken.kind == TkNewLine {
		tokenizer.Next()
	}
}

func eatNewLines(tokenizer *Tokenizer) {
	for tokenizer.lookAheadToken.kind == TkNewLine {
		tokenizer.Next()
	}
}

func parseCodeBlock(tokenizer *Tokenizer) *CodeBlock {
	result := &CodeBlock{}
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkOpenCurly)
	eatSemicolons(tokenizer)
	for tokenizer.lookAheadToken.kind != TkCloseCurly {
		item := parseStmtOrExpr(tokenizer)
		result.Items = append(result.Items, item)
		eatSemicolons(tokenizer)
	}
	lastToken := tokenizer.Next()
	tokenizer.expectKind(lastToken, TkCloseCurly)
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

// func extractRawStringValue(b *strings.Builder, tokenizer *Tokenizer, token Token) {
// 	// shave off the optional hacky newline that isn't part of the string literal
// 	str := token.value
// 	if str[len(str)-1] == '\n' {
// 		str = str[:len(str)-1]
// 	}
// 	if len(str) < 3 {
// 		return
// 	}
// 	if str[2] != ' ' {
// 		tokenizer.reportError(token, "3rd char is raw string literal must be a ' ' (space) character")
// 	}
// 	b.WriteString(str[3:])
// }

func parseRawStrLit(tokenizer *Tokenizer) *StrLit {
	firstToken := tokenizer.Next()
	var b strings.Builder
	tokenizer.expectKind(firstToken, TkRawStrLit)
	for i, line := range tokenizer.lastMultilineStrLitLines {
		if i != 0 {
			b.WriteString("\n")
		}
		b.WriteString(line)
	}
	return &StrLit{
		Source: joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value),
		Value:  b.String(),
		Raw:    true,
	}
}

func parseStrLit(tokenizer *Tokenizer) *StrLit {
	firstToken := tokenizer.Next()
	var b strings.Builder
	tokenizer.expectKind(firstToken, TkStrLit)
	b.Grow(len(firstToken.value) - 2)
	var processEscape bool
	for i, rune := range firstToken.value {
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
				tokenizer.reportError(firstToken, "invalid escape \\%c in string literal", rune)
			}
			processEscape = false
			continue
		}

		if rune == '\\' {
			processEscape = true
			continue
		} else if i != 0 && i != len(firstToken.value)-1 {
			b.WriteRune(rune)
		}
	}
	return &StrLit{
		Source: firstToken.value,
		Value:  b.String(),
		Raw:    false,
	}
}

func parseIntLit(tokenizer *Tokenizer) *IntLit {
	token := tokenizer.Next()
	tokenizer.expectKind2(token, TkIntLit, TkHexLit)
	result := &IntLit{Source: token.value}

	var ok bool
	if token.kind == TkIntLit {
		result.Value, ok = big.NewInt(0).SetString(token.value, 10)
	} else {
		result.Value, ok = big.NewInt(0).SetString(token.value[2:], 16)
	}
	if !ok {
		panic(fmt.Errorf("internal error, token.value: %s", token.value))
	}

	return result
}

func parseFloatLit(tokenizer *Tokenizer) *FloatLit {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkFloatLit)
	result := &FloatLit{Source: token.value}

	floatValue, _, err := big.ParseFloat(token.value, 10, 64, big.ToNearestEven)
	if err != nil {
		panic("internal error invalid float token")
	}
	result.Value = floatValue
	return result
}

func applyOperatorPrecedenceFromLeft(tokenizerCode string, lhs Expr, op *Ident, rhs Expr) *Call {
	if rhsCall, ok := rhs.(*Call); ok && !rhsCall.Braced {
		rhsOperator, isIdent := rhsCall.Callee.(*Ident)
		if isIdent && OperatorPrecedence(op.Source) >= OperatorPrecedence(rhsOperator.Source) {
			newLhs := applyOperatorPrecedenceFromLeft(tokenizerCode, lhs, op, rhsCall.Args[0])
			newRhs := rhsCall.Args[1]
			return &Call{
				Source: joinSubstr(tokenizerCode, newLhs.GetSource(), newRhs.GetSource()),
				Callee: rhsOperator,
				Args:   []Expr{newLhs, newRhs},
			}
		}
	}

	return &Call{
		Source: joinSubstr(tokenizerCode, lhs.GetSource(), rhs.GetSource()),
		Callee: op,
		Args:   []Expr{lhs, rhs},
	}
}

func parseInfixCall(tokenizer *Tokenizer, lhs Expr) *Call {
	operator := parseInfixOperator(tokenizer)
	rhs := parseExpr(tokenizer, false)
	// parseExpr recursively eats all follow operator calls. Therefore `lhs` is
	// the _new_ operatore that needs to be applied in the expression from the
	// left.
	return applyOperatorPrecedenceFromLeft(tokenizer.code, lhs, operator, rhs)
}

// comma separated list of expressions. used for call arguments and array literals
func parseExprList(tokenizer *Tokenizer, tkOpen, tkClose TokenKind) *ExprList {
	firstToken := tokenizer.lookAheadToken
	next := tokenizer.Next()
	tokenizer.expectKind(next, tkOpen)
	var items []Expr
	if tokenizer.lookAheadToken.kind != tkClose {
		items = append(items, parseExpr(tokenizer, false))
		for tokenizer.lookAheadToken.kind == TkComma {
			tokenizer.Next()
			items = append(items, parseExpr(tokenizer, false))
		}
	}
	eatNewLines(tokenizer)
	lastToken := tokenizer.Next()
	tokenizer.expectKind(lastToken, tkClose)
	return &ExprList{
		Source: joinSubstr(tokenizer.code, firstToken.value, lastToken.value),
		Items:  items,
	}
}

func parseCall(tokenizer *Tokenizer, callee Expr) *Call {
	// parse call expr
	result := &Call{Callee: callee}
	result.Args = parseExprList(tokenizer, TkOpenBrace, TkCloseBrace).Items
	result.Braced = true
	result.Source = joinSubstr(tokenizer.code, callee.GetSource(), tokenizer.token.value)
	return result
}

func parseCommand(tokenizer *Tokenizer, callee Expr) Expr {
	// parse call expr
	result := &Call{Callee: callee, Command: true}
	tk := &tokenizer.lookAheadToken.kind
	for *tk != TkNewLine &&
		*tk != TkSemicolon &&
		*tk != TkCloseCurly &&
		*tk != TkEof {
		result.Args = append(result.Args, parseExpr(tokenizer, false))
	}
	if len(result.Args) == 0 {
		return result.Callee
	}
	result.Source = joinSubstr(tokenizer.code, callee.GetSource(), tokenizer.token.value)
	return result
}

func parseBracketExpr(tokenizer *Tokenizer, callee Expr) *BracketExpr {
	// parse call expr
	result := &BracketExpr{
		Callee: callee,
		Args:   parseExprList(tokenizer, TkOpenBracket, TkCloseBracket).Items,
	}
	result.Source = joinSubstr(tokenizer.code, callee.GetSource(), tokenizer.token.value)
	return result
}

func parseArrayLit(tokenizer *Tokenizer) *ArrayLit {
	exprList := parseExprList(tokenizer, TkOpenBracket, TkCloseBracket)
	return &ArrayLit{
		Items:  exprList.Items,
		Source: exprList.Source,
	}
}

func parseStmtOrExpr(tokenizer *Tokenizer) Expr {
	// TODO this code wraps nil with a type, which is then not seen anymore by `expr == nil`. Could be a problem.
	switch tokenizer.lookAheadToken.kind {
	case TkDocComment:
		return parseDocComment(tokenizer)
	case TkVar, TkLet, TkConst:
		// TODO this special handling of var let const is ugly here and should not
		// be here. currently it exists here, because in parseExpr, `TkVar` will
		// treat "var" like an operator, so that `proc foo(var arg: Baz)` is
		// possible, and `var` only attaches to `arg` and nothing else.
		ident := &Ident{Source: tokenizer.Next().value}
		return parseCommand(tokenizer, ident)
	}
	result := parseExpr(tokenizer, false)
	if ident, isIdent := result.(*Ident); isIdent {
		result = parseCommand(tokenizer, ident)
	}
	return result
}

func parsePrefixCall(tokenizer *Tokenizer, preventInfixOperatorInArgument bool) Expr {
	// Currently any token  is legal as prefix. I hope this won't fall on my head.
	firstToken := tokenizer.Next()
	op := &Ident{Source: firstToken.value}
	secondToken := tokenizer.lookAheadToken
	arg := parseExpr(tokenizer, preventInfixOperatorInArgument)

	// kind := tokenizer.lookAheadToken.kind
	// Negation operator immediately followed by a number literal is collapsed into a negative literal.
	if op.Source == "-" {
		// check for spaces between the tokens
		ptr1 := uintptr(unsafe.Pointer(unsafe.StringData(firstToken.value)))
		ptr2 := uintptr(unsafe.Pointer(unsafe.StringData(secondToken.value)))
		if ptr1+uintptr(len(op.Source)) == ptr2 { // no space
			if intLit, isIntLit := arg.(*IntLit); isIntLit {
				intLit.Source = joinSubstr(tokenizer.code, op.Source, intLit.Source)
				intLit.Value.Neg(intLit.Value)
				return intLit
			}
			if floatLit, isFloatLit := arg.(*FloatLit); isFloatLit {
				floatLit.Source = joinSubstr(tokenizer.code, op.Source, floatLit.Source)
				floatLit.Value.Neg(floatLit.Value)
				return floatLit
			}
		}
	}

	source := joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)

	return &Call{
		Source: source,
		Callee: op,
		Args:   []Expr{arg},
		Prefix: true,
		Braced: true,
	}
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

func parseDocComment(tokenizer *Tokenizer) Expr {

	commentStartToken := tokenizer.Next()
	if !tokenizer.expectKind(commentStartToken, TkDocComment) {
		return &InvalidTokenExpr{commentStartToken.value, commentStartToken.kind}
	}

	token := commentStartToken
	for tokenizer.lookAheadToken.kind == TkNewLine {
		lookAhead2, _ := tokenizer.ScanTokenAt(tokenizer.lookAheadOffset)
		if lookAhead2.kind == TkDocComment {
			_ = tokenizer.Next()     // discard newline
			token = tokenizer.Next() // new doc comment token
		} else {
			break
		}
	}

	source := joinSubstr(tokenizer.code, commentStartToken.value, token.value)
	result := &PrefixDocComment{Source: source}
	commentScanner := &DocCommentScanner{source}
	for commentScanner.HasNext() {
		line := commentScanner.Next()
		if line != "" {
			if matches := docCommentSectionRegex.FindStringSubmatch(line); len(matches) > 0 {
				name := matches[1]
				value := matches[2]
				section := &NamedDocSection{Name: name}
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
		section := result.NamedDocSections[i]
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
	// TODO attach doc comments after sem checking phase, this ast has no semantics at all
	switch ex := expr.(type) {
	case *Ident:
		if ex.Source == target {
			ex.Comment = append(ex.Comment, value)
			fmt.Printf("attaching doc comment to %s\n", ex.Source)
			fmt.Printf("%s\n", value)
			return true
		} else {
			return false
		}
	case *CodeBlock:
		return false
	case *StrLit:
		return false
	case *IntLit:
		return false
	case *FloatLit:
		return false
	case *ArrayLit:
		return false
	case *Call:
		result := attachDocComment(ex.Callee, target, value)
		for _, arg := range ex.Args {
			if result {
				return result
			}
			result = attachDocComment(arg, target, value)
		}
		return result
	}
	panic("not implemented")
}

func parseNilLit(tokenizer *Tokenizer) *NilLit {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkNilLit)
	return &NilLit{Source: token.value, Type: TypeNilPtr}
}

func parseExpr(tokenizer *Tokenizer, stopAtOperator bool) (result Expr) {

	switch tk := tokenizer.lookAheadToken.kind; tk {
	case TkNewLine:
		tokenizer.Next()
		result = parseExpr(tokenizer, stopAtOperator)
	case TkIdent, TkQuotedIdent:
		ident := parseIdent(tokenizer)
		// this is a hack. This is simply to prevent the expression `if not x` to be
		// parsed as an infix operator call, but instead of a command call.
		if tk != TkQuotedIdent {
			switch ident.Source {
			case "if", "while", "for", "return", "var", "let", "const", "proc", "template", "type", "discard":
				stopAtOperator = true
			}
		}
		result = ident
	case TkOpenCurly:
		result = (Expr)(parseCodeBlock(tokenizer))
	case TkStrLit:
		result = (Expr)(parseStrLit(tokenizer))
	case TkRawStrLit:
		result = (Expr)(parseRawStrLit(tokenizer))
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
			if call, isCall := result.(*Call); isCall {
				call.Braced = true
			}
		} else {
			// TODO, this report is not on the right location. It should be on
			// `exprList`, not token.
			tokenizer.reportError(token, "braced expression must contain a single expression, but has %d", len(exprList.Items))
			result = newErrorNode(exprList)
		}
	case TkOperator, TkVar:
		result = (Expr)(parsePrefixCall(tokenizer, true))
	case TkNilLit:
		result = (Expr)(parseNilLit(tokenizer))
	default:
		tokenizer.reportWrongKind(tokenizer.lookAheadToken)
		token := tokenizer.Next()
		result = (Expr)(&InvalidTokenExpr{token.value, token.kind})
	}

	// any expression could be the start of a longer expression, this is
	// explorerd here.

	for true {
		lookAhead := tokenizer.lookAheadToken
		switch lookAhead.kind {
		case TkOperator:
			// used for:
			//   -1:f32
			//   not a or b -> (not a) or b # stops at operator
			//   discard a or b -> discard(a or b) # does not stop at operator
			if stopAtOperator {
				return result
			}
			result = (Expr)(parseInfixCall(tokenizer, result))
		case TkOpenBrace:
			result = (Expr)(parseCall(tokenizer, result))
		case TkOpenBracket:
			result = (Expr)(parseBracketExpr(tokenizer, result))
		case TkDocComment:
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
						tokenizer.reportError(lookAhead, "could not attach doc comment <%s> to <%s>\n", comment, result.GetSource())
					}
				} else {
					tokenizer.reportError(lookAhead, "invalid doc comment, proper error message not implemented")
				}
			}
			return result
		default:
			return result
		}
	}

	return result
}

func parsePackage(tokenizer *Tokenizer) (result *PackageDef, errors []ParseError) {
	result = &PackageDef{
		Name:    strings.TrimSuffix(filepath.Base(tokenizer.filename), ".golem"),
		Source:  tokenizer.code,
		WorkDir: filepath.Dir(tokenizer.filename),
	}
	for true {
		switch tokenizer.lookAheadToken.kind {
		//case TkLineComment:
		//	continue
		case TkSemicolon, TkNewLine:
			tokenizer.Next()
			continue
		case TkEof:
			return result, tokenizer.errors
		default:
			result.TopLevelStmts = append(result.TopLevelStmts, parseStmtOrExpr(tokenizer))
			continue
		}
	}
	panic("unreachable")
}
