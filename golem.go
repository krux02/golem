package main

import . "fmt"
import "os"
import "unicode/utf8"
import "strings"
import "io/ioutil"
import u "unicode"
import "path/filepath"
import "unsafe"
import "reflect"
import "path"

// (add-hook 'before-save-hook 'gofmt-before-save)

type TokenKind int16

const (
	TkInvalid TokenKind = iota
	TkIdent
	TkSemicolon
	TkComma
	TkOperator
	TkStringLit
	TkIntLit
	TkFloatLit
	TkLineComment

	TkOpenBrace    TokenKind = 100
	TkCloseBrace   TokenKind = 101
	TkOpenBracket  TokenKind = 102
	TkCloseBracket TokenKind = 103
	TkOpenCurly    TokenKind = 104
	TkCloseCurly   TokenKind = 105
	TkCount        TokenKind = 106
)

func init() {

	if TkOpenBrace^TkCloseBrace != 1 {
		panic("()")
	}
	if TkOpenBracket^TkCloseBracket != 1 {
		panic("[]")
	}
	if TkOpenCurly^TkCloseCurly != 1 {
		panic("{}")
	}
}

var TokenKindNames = [TkCount]string{
	TkInvalid:      "Invalid",
	TkIdent:        "Ident",
	TkSemicolon:    "Semicolon",
	TkComma:        "Comma",
	TkOperator:     "Operator",
	TkStringLit:    "StringLit",
	TkIntLit:       "IntLit",
	TkFloatLit:     "FloatLit",
	TkLineComment:  "LineComment",
	TkOpenBrace:    "OpenBrace",
	TkCloseBrace:   "CloseBrace",
	TkOpenBracket:  "OpenBracket",
	TkCloseBracket: "CloseBracket",
	TkOpenCurly:    "OpenCurly",
	TkCloseCurly:   "CloseCurly",
}

type Type struct {
	name string
}

var types []Type = []Type{
	{"int"},
	{"float"},
	{"string"},
}

type TypeHandle int

type Token struct {
	kind  TokenKind
	value string
}

type Tokenizer struct {
	code         string
	filename     string
	offset       int
	bracketStack []Token
	lastToken    Token
}

func NewTokenizer(code string, filename string) *Tokenizer {
	return &Tokenizer{code: code, filename: filename}
}

func (this *Tokenizer) LineColumn(offset int) (line, column int) {
	line = 1
	lineStart := 0
	for pos, rune := range this.code {
		if offset <= pos {
			column = pos - lineStart
			return
		}

		if rune == '\n' {
			line++
			lineStart = pos
		}
	}
	return -1, -1
}

func (this *Tokenizer) LineColumnToken(token Token) (line, columnStart, columnEnd int) {
	header1 := (*reflect.StringHeader)(unsafe.Pointer(&this.code))
	header2 := (*reflect.StringHeader)(unsafe.Pointer(&token.value))
	offset := int(header2.Data - header1.Data)
	line, columnStart = this.LineColumn(offset)
	columnEnd = columnStart + header2.Len
	return
}

func (this *Tokenizer) LineColumnCurrent() (line, column int) {
	return this.LineColumn(this.offset)
}

func (this *Tokenizer) LineColumnLastToken() (line, columnStart, columnEnd int) {
	line, columnStart = this.LineColumn(this.offset - len(this.lastToken.value))
	line, columnEnd = this.LineColumn(this.offset)
	return
}

func (this *Tokenizer) checkMatchingBracket(kind TokenKind) bool {
	len := len(this.bracketStack)
	if len > 0 && this.bracketStack[len-1].kind == kind^1 {
		return true
	}
	return false
}

func (this *Tokenizer) popBracketStack() {
	len := len(this.bracketStack)
	this.bracketStack = this.bracketStack[:len-1]
}

func (this *Tokenizer) pushBracketStack(arg Token) {
	this.bracketStack = append(this.bracketStack, arg)
}

func (this *Tokenizer) Next() (result Token) {
	result, this.offset = this.LookAhead()
	switch result.kind {
	case TkOpenBrace, TkOpenBracket, TkOpenCurly:
		this.pushBracketStack(result)
	case TkCloseBrace, TkCloseBracket, TkCloseCurly:
		// either match bracket or invalidat it
		if this.checkMatchingBracket(result.kind) {
			this.popBracketStack()
		} else {
			Println(this.bracketStack, result)
			result.kind = TkInvalid
		}
	}
	return
}

// LookAhead does the same as Next, but does not modify offset
func (this *Tokenizer) LookAhead() (result Token, newOffset int) {
	code := this.code[this.offset:]
	newOffset = this.offset
	// Printf("read token in:%s...\n", code[:20]);
	if len(code) == 0 {
		panic("Cannot read token in empty string")
	}

	{
		// eat whitespace, maybe emit EndLine token
		var idx = 0
		var gotNewLine = false

		rune, length := utf8.DecodeRuneInString(code)

		for u.IsSpace(rune) {
			if rune == '\n' {
				gotNewLine = true
			}
			idx += length
			rune, length = utf8.DecodeRuneInString(code[idx:])
		}

		newOffset += idx

		if gotNewLine &&
			// do not infer semicolon when within braces
			(len(this.bracketStack) == 0 || this.bracketStack[len(this.bracketStack)-1].kind != TkOpenBrace) {
			// TODOdo not infer semicolon after comments
			result.kind = TkSemicolon
			result.value = code[:idx]
			this.lastToken = result
			return
		}

		code = code[idx:]
	}

	result = Token{kind: TkInvalid, value: code[:1]}

	c, cLen := utf8.DecodeRuneInString(code)
	if c >= 128 {
		panic("currently only 7 bit ascii is supported!!!")
	}

	switch {
	case u.IsLetter(c):
		result.kind = TkIdent
		result.value = code
		for pos, rune := range code {
			//Printf("rune: %c pos: %d\n",
			//	rune, pos)

			if !u.IsDigit(rune) && !u.IsLetter(rune) {
				result.value = code[:pos]
				break
			}
		}
	case c == '#':
		result.kind = TkLineComment
		result.value = code
		for pos, rune := range code {
			if rune == '\n' {
				result.value = code[:pos]
				break
			}
		}
	case u.IsDigit(c):
		result.kind = TkIntLit
		// read number literal
		it := len(code)
		for pos, rune := range code {
			if !u.IsDigit(rune) {
				it = pos
				break
			}
		}

		if rune, runeLen := utf8.DecodeRuneInString(code[it:]); rune == '.' {
			result.kind = TkFloatLit
			it += runeLen

			rune, _ := utf8.DecodeRuneInString(code[it:])

			if !u.IsDigit(rune) {
				panic("incomplete floating point literal")
			}

			var idx2 = len(code)
			for pos, rune := range code[it:] {
				if !u.IsDigit(rune) {
					idx2 = it + pos
					break
				}
			}
			it = idx2
		}

		rune, _ := utf8.DecodeRuneInString(code[it:])
		if u.IsLetter(rune) {
			panic("invalid alpha character after literal")
		}

		result.value = code[:it]
	case c == '"':
		result.kind = TkStringLit
		var lastRune = '"'
		var scanningDone = false
		var idx2 = len(code)
		for pos, rune := range code[cLen:] {
			if scanningDone {
				idx2 = cLen + pos
				break
			}

			if lastRune == '\\' {
				switch rune {
				case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\'', '"':
				default:
					panic("illegal escale sequence")
				}
			} else if rune == '"' {
				scanningDone = true
			}
		}
		result.value = code[:idx2]
	case c == '(':
		result = Token{TkOpenBrace, code[:cLen]}
	case c == ')':
		result = Token{TkCloseBrace, code[:cLen]}
	case c == '[':
		result = Token{TkOpenBracket, code[:cLen]}
	case c == ']':
		result = Token{TkCloseBracket, code[:cLen]}
	case c == '{':
		result = Token{TkOpenCurly, code[:cLen]}
	case c == '}':
		result = Token{TkCloseCurly, code[:cLen]}
	case c == ',':
		result = Token{TkComma, code[:cLen]}
	case c == ';':
		result = Token{TkSemicolon, code[:cLen]}
	case u.IsSymbol(c) || u.IsPunct(c):
		var idx2 = len(code)
		for pos, rune := range code[cLen:] {
			if !u.IsPunct(rune) && !u.IsSymbol(rune) || strings.ContainsRune("{}[](),;", rune) {
				idx2 = cLen + pos
				break
			}
		}

		result.kind = TkOperator
		result.value = code[:idx2]
	default:
		Printf("ispunct %v\n", u.IsSymbol(c))
		panic(Sprintf("unexpected input: %c\n", c))
	}

	newOffset += len(result.value)
	this.lastToken = result
	return
}

func (this *Tokenizer) AtEnd() bool {
	return this.offset == len(this.code)
}

type TypeExpr struct {
	Ident string
}

type StructField struct {
	Name string
	Type TypeExpr
}

type StructDef struct {
	Name   string
	Fields []StructField
}

type ProcArgument struct {
	Name string
	Type TypeExpr
}

type CodeBlock struct {
	Items []Expr
}

func (block CodeBlock) expression() {}
func (block CodeBlock) statement()  {}

type Expr interface {
	prettyPrint(*strings.Builder)
	expression()
}

type Stmt interface {
	prettyPrint(*strings.Builder)
	statement()
}

type ReturnStmt struct {
	Expr Expr
}

func (stmt ReturnStmt) statement() {}

type Symbol struct {
	Value              string
	OperatorPrecedence int
}

func (sym Symbol) expression() {}

type StringLit struct {
	Val string
}

func (lit StringLit) expression() {}

var FunctionSymbolTable = []Symbol{
	{Value: "*", OperatorPrecedence: 6},
	{Value: "/", OperatorPrecedence: 6},
	{Value: "+", OperatorPrecedence: 5},
	{Value: "-", OperatorPrecedence: 5},
	{Value: "println"},
}

func LookupFunctionSymbol(value string) Symbol {
	// TODO, this is horrible lookup
	for _, sym := range FunctionSymbolTable {
		if sym.Value == value {
			return sym
		}
	}
	panic(Sprintf("Undefined Function `%s`", value))
}

type Call struct {
	Sym  Symbol
	Args []Expr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

func (call Call) expression() {}

type ProcDef struct {
	Name       string
	Args       []ProcArgument
	ResultType TypeExpr
	Body       Expr
}

type PackageDef struct {
	Name     string
	TypeDefs []StructDef
	ProcDefs []ProcDef
}

func (tokenizer *Tokenizer) wrongKind(token Token) {
	line, columnStart, columnEnd := tokenizer.LineColumnToken(token)
	panic(Sprintf("%s(%d, %d-%d) Error: unexpected Token: %v",
		tokenizer.filename, line, columnStart, columnEnd, token))
}

func (tokenizer *Tokenizer) expectKind(token Token, kind TokenKind) {
	if token.kind != kind {
		tokenizer.wrongKind(token)
	}
}

func (tokenizer *Tokenizer) expectIdent(token Token, arg string) {
	tokenizer.expectKind(token, TkIdent)
	if token.value != arg {
		panic(Sprintf("expected ident %v got %v", arg, token.value))
	}
}

func (tokenizer *Tokenizer) expectOperator(token Token, arg string) {
	tokenizer.expectKind(token, TkOperator)
	if token.value != arg {
		panic(Sprintf("expected %v got %v", arg, token.value))
	}
}

func (tokenizer *Tokenizer) parseTypeExpr() (result TypeExpr) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
	result.Ident = token.value
	return
}

func (tokenizer *Tokenizer) parseExpr() (result Expr) {
	// eat whitespace before setting startOffest
	token := tokenizer.Next()

	switch token.kind {
	case TkIdent:
		sym := Symbol{Value: token.value}

		lookAhead, _ := tokenizer.LookAhead()

		switch lookAhead.kind {
		case TkSemicolon, TkCloseBrace, TkCloseCurly:
			return (Expr)(sym)

		case TkOperator:
			token = tokenizer.Next()
			operator := LookupFunctionSymbol(token.value)
			rhs := tokenizer.parseExpr()
			call := Call{Sym: operator, Args: []Expr{sym, rhs}}

			if rhsCall, ok := rhs.(Call); ok && !rhsCall.Braced {
				rhsOperator := rhsCall.Sym
				if operator.OperatorPrecedence > rhsOperator.OperatorPrecedence {
					// operator precedence
					call.Args[1] = rhsCall.Args[0]
					rhsCall.Args[0] = call
					return (Expr)(rhsCall)
				}
			}
			return (Expr)(call)
		case TkOpenBrace:
			// parse call expr
			var args []Expr
			token = tokenizer.Next()
			for true {
				lookAhead, _ = tokenizer.LookAhead()
				switch lookAhead.kind {
				case TkIdent, TkStringLit:
					args = append(args, tokenizer.parseExpr())
					continue
				case TkCloseBrace:
					token = tokenizer.Next()
					call := Call{Sym: sym, Args: args}
					return (Expr)(call)
				}
				tokenizer.wrongKind(lookAhead)
			}
		}

		tokenizer.wrongKind(lookAhead)
	case TkOpenCurly:
		// parse block
		var block CodeBlock

		lookAhead, _ := tokenizer.LookAhead()
		for lookAhead.kind == TkSemicolon {
			tokenizer.Next()
			lookAhead, _ = tokenizer.LookAhead()
		}

		for lookAhead.kind != TkCloseCurly {
			item := tokenizer.parseExpr()
			block.Items = append(block.Items, item)

			next := tokenizer.Next()
			tokenizer.expectKind(next, TkSemicolon)
			lookAhead, _ = tokenizer.LookAhead()
		}

		endToken := tokenizer.Next()

		tokenizer.expectKind(endToken, TkCloseCurly)
		// tokenizer.expectKind(endtoken, TkCloseCurly)
		return (Expr)(block)
	case TkStringLit:
		var b strings.Builder
		b.Grow(len(token.value) - 2)
		for i, rune := range token.value {
			if rune == '\\' {
				panic("escape sequences not implemented")
			} else if i != 0 && i != len(token.value)-1 {
				b.WriteRune(rune)
			}
		}

		lit := StringLit{Val: b.String()}
		return (Expr)(lit)
	}

	tokenizer.wrongKind(token)
	panic("unreachable")
}

func parseTypeDef(tokenizer *Tokenizer) (result StructDef) {
	var token Token
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
		tokenizer.expectOperator(colon, ":")
		structField.Type = tokenizer.parseTypeExpr()
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

		tokenizer.expectOperator(token, ":")
		typ := tokenizer.parseTypeExpr()
		for i := startIndex; i < len(result.Args); i++ {
			result.Args[i].Type = typ
		}
		token = tokenizer.Next()
	}
	tokenizer.expectKind(token, TkCloseBrace)

	token = tokenizer.Next()
	tokenizer.expectOperator(token, ":")
	result.ResultType = tokenizer.parseTypeExpr()
	token = tokenizer.Next()
	tokenizer.expectOperator(token, "=")

	result.Body = tokenizer.parseExpr()

	return
}

func parsePackage(code, filename string) (result PackageDef) {
	result.Name = path.Base(filename)
	var tokenizer = NewTokenizer(code, filename)

	for !tokenizer.AtEnd() {
		token := tokenizer.Next()
		switch token.kind {
		case TkLineComment:
			continue
		case TkSemicolon:
			continue
		case TkIdent:
			switch token.value {
			case "type":
				result.TypeDefs = append(result.TypeDefs, parseTypeDef(tokenizer))
				continue
			case "proc":
				result.ProcDefs = append(result.ProcDefs, parseProcDef(tokenizer))
				continue
			}
		}

		// utrineiaurtne
		line, columnStart, columnEnd := tokenizer.LineColumnLastToken()
		msg := Sprintf("%s(%d, %d-%d) Error: unexpected Token: %v",
			filename, line, columnStart, columnEnd, token)
		panic(msg)
	}
	return
}

func main() {
	var filename string
	if len(os.Args) == 2 {
		filename = os.Args[1]
	} else {
		panic("program needs one argument only, the input file")
	}

	filename, err := filepath.Abs(filename)
	if err != nil {
		panic(err)
	}

	// base := filepath.Base(filename)
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	// tokenize(string(bytes));
	parsePackage(string(bytes), filename)
}
