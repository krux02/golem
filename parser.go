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
		return 10
	case ":":
		return 9
	case "*", "/":
		return 7
	case "+", "-":
		return 6
	case ">", "<", ">=", "<=":
		return 5
	case "in", "notin":
		return 4
	case "==", "!=":
		return 3
	case "and", "or":
		return 2
	case "=", "+=", "-=", "*=", "/=":
		return 1
	}
	// operator precedence does not exist
	return -1
}

func parseIdent(tokenizer *Tokenizer) *Ident {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
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
		tokenizer.reportError(token, "invalid operator '%s'", token.value)
	}
	return &Ident{
		Source: token.value,
	}
}

func parseTypeContext(tokenizer *Tokenizer) *TypeContext {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkType)
	result := &TypeContext{
		Expr: TypeExpr(parseExpr(tokenizer, false)),
	}
	result.Source = joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)
	return result
}

func parseReturnExpr(tokenizer *Tokenizer) *ReturnExpr {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkReturn)
	result := &ReturnExpr{
		Value: parseExpr(tokenizer, false),
	}
	result.Source = joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)
	return result
}

func parseVarExpr(tokenizer *Tokenizer) *VarExpr {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkVar)
	result := &VarExpr{
		Expr: parseExpr(tokenizer, true),
	}
	result.Source = joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)
	return result
}

/* (name string, typ TypeExpr, expr Expr) */

func parseVariableDefStmt(tokenizer *Tokenizer) *VariableDefStmt {
	result := &VariableDefStmt{}
	firstToken := tokenizer.Next()
	switch firstToken.kind {
	case TkLet, TkVar, TkConst:
		result.Prefix = Ident{Source: firstToken.value}
	default:
		tokenizer.reportWrongKind(firstToken)
		// TODO what now? Create an invalid expression? this default value of result is in valid stmt.
		return result
	}
	result.Expr = parseExpr(tokenizer, false)
	result.Source = joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)
	return result
}

func parseBreakStmt(tokenizer *Tokenizer) *BreakStmt {
	result := &BreakStmt{}
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkBreak)
	result.Source = token.value
	return result
}

func parseContinueStmt(tokenizer *Tokenizer) *ContinueStmt {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkContinue)
	return &ContinueStmt{Source: token.value}
}

func parseForLoop(tokenizer *Tokenizer) *ForLoopStmt {
	result := &ForLoopStmt{}
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkFor)
	result.LoopIdent = parseIdent(tokenizer)
	token := tokenizer.Next()
	tokenizer.expectOperator(token, "in")
	result.Collection = parseExpr(tokenizer, false)
	token = tokenizer.Next()
	tokenizer.expectKind(token, TkDo)
	result.Body = parseExpr(tokenizer, false)
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
}

func parseWhileLoop(tokenizer *Tokenizer) *WhileLoopStmt {
	result := &WhileLoopStmt{}
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

		result := &IfElseExpr{Condition: condition, Body: body, Else: elseExpr}
		lastToken := tokenizer.token
		result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
		return result
		// return IfElseStmt{Condition: condition, Body: body, Else: elseExpr}
	}
	result := &IfExpr{Condition: condition, Body: body}
	lastToken := tokenizer.token
	result.Source = joinSubstr(tokenizer.code, firstToken.value, lastToken.value)
	return result
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

func parseStrLit(tokenizer *Tokenizer) *StrLit {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkStrLit)
	result := &StrLit{Source: token.value}
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
	case TkPrefixDocComment:
		return (Expr)(parseDocComment(tokenizer))
	case TkVar, TkLet, TkConst:
		return (Expr)(parseVariableDefStmt(tokenizer))
	case TkBreak:
		return (Expr)(parseBreakStmt(tokenizer))
	case TkContinue:
		return (Expr)(parseContinueStmt(tokenizer))
	case TkFor:
		return (Expr)(parseForLoop(tokenizer))
	case TkWhile:
		return (Expr)(parseWhileLoop(tokenizer))
	case TkProc:
		return (Expr)(parseProcDef(tokenizer))
	}
	return parseExpr(tokenizer, false)
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

	return &Call{
		Source: joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value),
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

func parseDocComment(tokenizer *Tokenizer) *PrefixDocComment {
	if !tokenizer.expectKind(tokenizer.lookAheadToken, TkPrefixDocComment) {
		return nil
	}
	token := tokenizer.Next()
	result := &PrefixDocComment{Source: token.value}
	commentScanner := &DocCommentScanner{token.value}
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
	case *VariableDefStmt:
		return attachDocComment(ex.Expr, target, value)
	case *ForLoopStmt:
		return false
	case *IfExpr:
		return false
	case *IfElseExpr:
		return false
	case *ReturnExpr:
		return false
	case *BreakStmt:
		return false
	case *ContinueStmt:
		return false
	}
	panic("not implemented")
}

func parseNilLit(tokenizer *Tokenizer) *NilLit {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkNilLit)
	return &NilLit{Source: token.value, Type: TypeNilPtr}
}

func parseExpr(tokenizer *Tokenizer, stopAtOperator bool) (result Expr) {
	switch tokenizer.lookAheadToken.kind {
	case TkNewLine:
		tokenizer.Next()
		result = parseExpr(tokenizer, stopAtOperator)
	case TkIdent:
		result = (Expr)(parseIdent(tokenizer))
	case TkOpenCurly:
		result = (Expr)(parseCodeBlock(tokenizer))
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
			if call, isCall := result.(*Call); isCall {
				call.Braced = true
			}
		} else {
			// TODO, this report is not on the right location. It should be on
			// `exprList`, not token.
			tokenizer.reportError(token, "braced expression must contain a single expression, but has %d", len(exprList.Items))
			result = newErrorNode(exprList)
		}
	case TkOperator:
		result = (Expr)(parsePrefixCall(tokenizer, true))
	case TkDiscard, TkStruct, TkUnion, TkEnum:
		result = (Expr)(parsePrefixCall(tokenizer, false))
	case TkReturn:
		result = (Expr)(parseReturnExpr(tokenizer))
	case TkVar:
		result = (Expr)(parseVarExpr(tokenizer))
	case TkType:
		result = (Expr)(parseTypeContext(tokenizer))
	case TkNilLit:
		result = (Expr)(parseNilLit(tokenizer))
	default:
		tokenizer.reportWrongKind(tokenizer.lookAheadToken)
		result = (Expr)(&InvalidTokenExpr{tokenizer.Next()})
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

func parseTypeDef(tokenizer *Tokenizer) Expr {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkType)
	var annotations *StrLit
	if tokenizer.lookAheadToken.kind == TkStrLit {
		annotations = parseStrLit(tokenizer)
	}
	expr := parseExpr(tokenizer, false)
	source := joinSubstr(tokenizer.code, firstToken.value, expr.GetSource())
	return &TypeDef{Source: source, Expr: expr, Annotations: annotations}
}

func parseProcDef(tokenizer *Tokenizer) *ProcDef {
	result := &ProcDef{}
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkProc)
	if tokenizer.lookAheadToken.kind == TkStrLit {
		result.Annotations = parseStrLit(tokenizer)
	}
	result.Expr = parseExpr(tokenizer, false)
	result.Source = joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)
	return result
}

func parseEmitStmt(tokenizer *Tokenizer) *EmitStmt {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkEmit)
	strLit := parseStrLit(tokenizer)
	return &EmitStmt{
		Source: joinSubstr(tokenizer.code, firstToken.value, strLit.Source),
		Value:  strLit,
	}
}

func parseStaticExpr(tokenizer *Tokenizer) *StaticExpr {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkStatic)
	expr := parseExpr(tokenizer, false)
	return &StaticExpr{
		Source: joinSubstr(tokenizer.code, firstToken.value, expr.GetSource()),
		Expr:   expr,
	}
}

func parseImportStmt(tokenizer *Tokenizer) *ImportStmt {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkImport)
	strLit := parseStrLit(tokenizer)
	return &ImportStmt{
		Source: joinSubstr(tokenizer.code, firstToken.value, strLit.Source),
		Value:  strLit,
	}
}

func parseTrait(tokenizer *Tokenizer) *TraitDef {
	firstToken := tokenizer.Next()
	tokenizer.expectKind(firstToken, TkTrait)
	result := &TraitDef{}
	result.Expr = parseExpr(tokenizer, false)
	result.Source = joinSubstr(tokenizer.code, firstToken.value, tokenizer.token.value)
	return result
}

func parsePackage(tokenizer *Tokenizer) (result *PackageDef, errors []ParseError) {
	result = &PackageDef{}
	result.Name = strings.TrimSuffix(filepath.Base(tokenizer.filename), ".golem")
	result.Source = tokenizer.code
	result.WorkDir = filepath.Dir(tokenizer.filename)
	for true {
		switch tokenizer.lookAheadToken.kind {
		//case TkLineComment:
		//	continue
		case TkSemicolon, TkNewLine:
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
		case TkTrait:
			trait := parseTrait(tokenizer)
			result.TopLevelStmts = append(result.TopLevelStmts, trait)
			continue
		default:
			tokenizer.reportWrongKind(tokenizer.Next())
			continue
		}
	}
	panic("unreachable")
}
