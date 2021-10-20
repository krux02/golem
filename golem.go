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
import "log"
import "strconv"
import "os/exec"
import "syscall"

type TokenKind int16

const (
	TkInvalid TokenKind = iota
	TkIdent
	TkSemicolon
	TkComma
	TkOperator
	TkStrLit
	TkIntLit
	TkFloatLit
	TkCharLit
	TkDocComment

	TkOpenBrace    TokenKind = 100
	TkCloseBrace   TokenKind = 101
	TkOpenBracket  TokenKind = 102
	TkCloseBracket TokenKind = 103
	TkOpenCurly    TokenKind = 104
	TkCloseCurly   TokenKind = 105

	TkEof TokenKind = iota // end of file
	TkCount
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

var TokenKindNames = [...]string{
	TkInvalid:      "Invalid",
	TkIdent:        "Ident",
	TkSemicolon:    "Semicolon",
	TkComma:        "Comma",
	TkOperator:     "Operator",
	TkStrLit:       "StrLit",
	TkIntLit:       "IntLit",
	TkFloatLit:     "FloatLit",
	TkCharLit:      "CharLit",
	TkDocComment:   "DocComment",
	TkOpenBrace:    "OpenBrace",
	TkCloseBrace:   "CloseBrace",
	TkOpenBracket:  "OpenBracket",
	TkCloseBracket: "CloseBracket",
	TkOpenCurly:    "OpenCurly",
	TkCloseCurly:   "CloseCurly",
	TkEof:          "<EOF>",
}

type Token struct {
	kind  TokenKind
	value string
}

// this is by now far from just a tokenizer, has evolved to become the
// full parser of the language.
type Tokenizer struct {
	code                    string
	filename                string
	token, lookAheadToken   Token
	offset, lookAheadOffset int
}

func NewTokenizer(code string, filename string) (result *Tokenizer) {
	result = &Tokenizer{code: code, filename: filename}
	result.lookAheadToken, result.lookAheadOffset = result.ScanTokenAt(0)
	return
}

func (this *Tokenizer) LineColumnOffset(offset int) (line, column int) {
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
	line, columnStart = this.LineColumnOffset(offset)
	columnEnd = columnStart + header2.Len
	return
}

func (this *Tokenizer) LineColumnCurrent() (line, column int) {
	return this.LineColumnOffset(this.offset)
}

func (this *Tokenizer) Next() Token {
	this.token, this.offset = this.lookAheadToken, this.lookAheadOffset
	this.lookAheadToken, this.lookAheadOffset = this.ScanTokenAt(this.lookAheadOffset)
	return this.token
}

func (this *Tokenizer) ScanTokenAt(offset int) (result Token, newOffset int) {
	code := this.code[offset:]
	newOffset = offset

	// Printf("read token in:%s...\n", code[:20]);
	if len(code) == 0 {
		result.kind = TkEof
		return
	}

	{
		// eat whitespace, maybe emit EndLine token
		var idx int
		var gotNewLine = false

	eatWhiteSpace:
		rune, length := utf8.DecodeRuneInString(code[idx:])
		switch rune {
		case ' ':
			idx += length
			goto eatWhiteSpace
		case '\n':
			idx += length
			gotNewLine = true
			goto eatWhiteSpace
		case '#': // eat comment as part of whitespace
			//idx += length
			gotNewLine = true
			offset := strings.IndexByte(code[idx:], '\n')
			if offset != -1 {
				idx = idx + offset + 1
			} else {
				idx = len(code)
			}
			goto eatWhiteSpace
		case '\\':
			idx += length
		newlineEscape:
			rune, length := utf8.DecodeRuneInString(code[idx:])

			switch rune {
			case ' ', '\r':
				idx += length
				goto newlineEscape
			case '\n':
				idx += length
				goto eatWhiteSpace
			default:
				idx += length
				newOffset += idx
				result.kind = TkInvalid
				result.value = code[:idx]
				return
			}
		default:
		}

		if gotNewLine {
			newOffset += idx
			result.kind = TkSemicolon
			result.value = code[:idx]
			return
		}

		code = code[idx:]
		newOffset += idx
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
			if !u.IsDigit(rune) && !u.IsLetter(rune) {
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
		result.kind = TkStrLit
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
					panic("illegal escape sequence")
				}
			} else if rune == '"' {
				scanningDone = true
			}
		}
		result.value = code[:idx2]
	case c == '\'':
		result.kind = TkCharLit
		rune1, rune1Len := utf8.DecodeRuneInString(code[cLen:])
		if rune1 == '\\' {
			rune2, rune2Len := utf8.DecodeRuneInString(code[cLen+rune1Len:])
			switch rune2 {
			case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\'', '"':
			default:
				panic("illegal escape sequence")
			}
			rune3, rune3Len := utf8.DecodeRuneInString(code[cLen+rune1Len+rune2Len:])
			if rune3 != '\'' {
				panic("illegal escape sequence")
			}
			result.value = code[:cLen+rune1Len+rune2Len+rune3Len]
		} else {
			rune2, rune2Len := utf8.DecodeRuneInString(code[cLen+rune1Len:])
			if rune2 != '\'' {
				panic("illegal escape sequence")
			}
			result.value = code[:cLen+rune1Len+rune2Len]
		}
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
		panic(Sprintf("unexpected input: %c %d\n", c, c))
	}

	newOffset += len(result.value)
	return
}

func (this *Tokenizer) AtEnd() bool {
	return this.offset == len(this.code)
}

// TODO this is not a function sybol table, it is just an Operator Precedence table
var OperatorPrecedence map[string]int = map[string]int{
	"*": 6,
	"/": 6,
	"+": 5,
	"-": 5,
}

func (tokenizer *Tokenizer) wrongKind(token Token) string {
	line, columnStart, columnEnd := tokenizer.LineColumnToken(token)
	return Sprintf("%s(%d, %d-%d) Error: unexpected Token: %v",
		tokenizer.filename, line, columnStart, columnEnd, token)
}

func (tokenizer *Tokenizer) wrongIdent(token Token) string {
	line, columnStart, columnEnd := tokenizer.LineColumnToken(token)
	return Sprintf("%s(%d, %d-%d) Error: unexpected identifier: %s",
		tokenizer.filename, line, columnStart, columnEnd, token.value)
}

func (tokenizer *Tokenizer) expectKind(token Token, kind TokenKind) {
	if token.kind != kind {
		panic(tokenizer.wrongKind(token))
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

func parseTypeExpr(tokenizer *Tokenizer) (result TypeExpr) {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkIdent)
	result.Ident = token.value
	return
}

func parseReturnStmt(tokenizer *Tokenizer) (result ReturnStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "return")
	result.Value = parseExpr(tokenizer)
	return
}

func parseVarLetConstBody(tokenizer *Tokenizer) (name string, typ TypeExpr, expr Expr) {
	next := tokenizer.Next()
	tokenizer.expectKind(next, TkIdent)
	name = next.value
	next = tokenizer.Next()
	if next.kind == TkOperator && next.value == ":" {
		typ = parseTypeExpr(tokenizer)
		next = tokenizer.Next()
	}
	tokenizer.expectKind(next, TkOperator)
	expr = parseExpr(tokenizer)
	return
}

func parseLetStmt(tokenizer *Tokenizer) (result LetStmt) {
	// parse let Stmt
	next := tokenizer.Next()
	tokenizer.expectIdent(next, "let")
	result.Name, result.TypeExpr, result.Value = parseVarLetConstBody(tokenizer)
	return
}

func parseVarStmt(tokenizer *Tokenizer) (result VarStmt) {
	// parse let Stmt
	next := tokenizer.Next()
	tokenizer.expectIdent(next, "var")
	result.Name, result.TypeExpr, result.Value = parseVarLetConstBody(tokenizer)
	return
}

func parseConstStmt(tokenizer *Tokenizer) (result ConstStmt) {
	// parse let Stmt
	next := tokenizer.Next()
	tokenizer.expectIdent(next, "const")
	result.Name, result.TypeExpr, result.Value = parseVarLetConstBody(tokenizer)
	return
}

func parseBreakStmt(tokenizer *Tokenizer) (result BreakStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "break")
	result.Source = token.value
	return
}

func parseContinueStmt(tokenizer *Tokenizer) (result ContinueStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "continue")
	result.Source = token.value
	return
}

func parseForLoop(tokenizer *Tokenizer) (result ForLoopStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "for")
	result.LoopIdent = parseIdent(tokenizer)
	token = tokenizer.Next()
	tokenizer.expectIdent(token, "in")
	result.Collection = parseExpr(tokenizer)

	Println(AstFormat(result))
	Println(tokenizer.token, tokenizer.lookAheadToken)

	result.Body = parseCodeBlock(tokenizer)
	return
}

func parseIfStmt(tokenizer *Tokenizer) (result IfStmt) {
	token := tokenizer.Next()
	tokenizer.expectIdent(token, "if")
	result.Condition = parseExpr(tokenizer)
	result.Body = parseCodeBlock(tokenizer)
	return
}

func parseStmtOrExpr(tokenizer *Tokenizer) (result Expr) {
	lookAhead := tokenizer.lookAheadToken
	if lookAhead.kind == TkIdent {
		switch lookAhead.value {
		case "let":
			return (Expr)(parseLetStmt(tokenizer))
		case "var":
			return (Expr)(parseVarStmt(tokenizer))
		case "const":
			return (Expr)(parseConstStmt(tokenizer))
		case "return":
			return (Expr)(parseReturnStmt(tokenizer))
		case "for":
			return (Expr)(parseForLoop(tokenizer))
		case "break":
			return (Expr)(parseBreakStmt(tokenizer))
		case "continue":
			return (Expr)(parseBreakStmt(tokenizer))
		case "if":
			return (Expr)(parseIfStmt(tokenizer))
		}
	}
	return parseExpr(tokenizer)
}

func parseCodeBlock(tokenizer *Tokenizer) CodeBlock {
	token := tokenizer.Next()
	tokenizer.expectKind(token, TkOpenCurly)

	// parse block
	var block CodeBlock
	for tokenizer.lookAheadToken.kind == TkSemicolon {
		tokenizer.Next()
	}
	for tokenizer.lookAheadToken.kind != TkCloseCurly {
		item := parseStmtOrExpr(tokenizer)
		block.Items = append(block.Items, item)
		next := tokenizer.Next()
		tokenizer.expectKind(next, TkSemicolon)
	}
	endToken := tokenizer.Next()
	tokenizer.expectKind(endToken, TkCloseCurly)
	return block
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

func parseStrLit(tokenizer *Tokenizer) StrLit {
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

	return StrLit{Value: b.String()}
}

func parseIntLit(tokenizer *Tokenizer) IntLit {
	token := tokenizer.Next()
	intValue, err := strconv.Atoi(token.value)
	if err != nil {
		panic("internal error invalid int token")
	}
	return IntLit{Value: intValue}
}

func parseIdent(tokenizer *Tokenizer) Ident {
	// this func implementation is a joke, but it should be consistent with the other parse thingies
	token := tokenizer.Next()
	return Ident{Name: token.value}
}

func parseInfixCall(tokenizer *Tokenizer, lhs Expr) Call {
	token := tokenizer.Next()
	operator := Ident{Name: token.value}
	rhs := parseExpr(tokenizer)
	call := Call{Sym: operator, Args: []Expr{lhs, rhs}}

	if rhsCall, ok := rhs.(Call); ok && !rhsCall.Braced {
		rhsOperator := rhsCall.Sym
		if OperatorPrecedence[operator.Name] > OperatorPrecedence[rhsOperator.Name] {
			// operator precedence
			call.Args[1] = rhsCall.Args[0]
			rhsCall.Args[0] = call
			return rhsCall
		}
	}
	return call
}

func parseCall(tokenizer *Tokenizer, callee Expr) Call {
	// parse call expr
	var args []Expr
	tokenizer.Next()
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
			return Call{Sym: callee.(Ident), Args: args}
		}
		panic(tokenizer.wrongKind(tokenizer.lookAheadToken))
	}
	panic("unreachable")
}

func parseArrayLit(tokenizer *Tokenizer) (result ArrayLit) {
	next := tokenizer.Next()
	tokenizer.expectKind(next, TkOpenBracket)

	if tokenizer.lookAheadToken.kind == TkCloseBracket {
		return
	}
	result.Items = append(result.Items, parseExpr(tokenizer))

	for tokenizer.lookAheadToken.kind == TkComma {
		tokenizer.Next()
		result.Items = append(result.Items, parseExpr(tokenizer))
	}

	next = tokenizer.Next()
	tokenizer.expectKind(next, TkCloseBracket)
	return
}

func parseExpr(tokenizer *Tokenizer) Expr {
	var expr Expr
	switch tokenizer.lookAheadToken.kind {
	case TkIdent:
		expr = (Expr)(parseIdent(tokenizer))
	case TkOpenCurly:
		expr = (Expr)(parseCodeBlock(tokenizer))
	case TkCharLit:
		expr = (Expr)(parseCharLit(tokenizer))
	case TkStrLit:
		expr = (Expr)(parseStrLit(tokenizer))
	case TkIntLit:
		expr = (Expr)(parseIntLit(tokenizer))
	case TkOpenBracket:
		expr = (Expr)(parseArrayLit(tokenizer))
	default:
		tokenizer.wrongKind(tokenizer.lookAheadToken)
	}

	// any expression could be the start of a longer expression, this is
	// explorerd here

	lookAhead := tokenizer.lookAheadToken
	switch lookAhead.kind {
	case TkOperator:
		expr = (Expr)(parseInfixCall(tokenizer, expr))
	case TkOpenBrace:
		expr = (Expr)(parseCall(tokenizer, expr))
	}

	return expr
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
		tokenizer.expectOperator(colon, ":")
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

		tokenizer.expectOperator(token, ":")

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
	tokenizer.expectOperator(token, ":")
	result.ResultType = parseTypeExpr(tokenizer)
	token = tokenizer.Next()
	tokenizer.expectOperator(token, "=")

	result.Body = parseExpr(tokenizer)

	return
}

func parsePackage(code, filename string) (result PackageDef) {
	result.Name = path.Base(filename)
	Println("processing package: ", result.Name)
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
			case "let":
				result.Globals = append(result.Globals, parseLetStmt(tokenizer))
				continue
			case "var":
				panic("not implemented")
				continue
			case "const":
				panic("not implemented")
				continue
			default:
			}
			panic(tokenizer.wrongIdent(tokenizer.lookAheadToken))
		}
		panic(tokenizer.wrongKind(tokenizer.lookAheadToken))
	}
	panic("unreachable")
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
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	Println("----------------------- parsed  code -----------------------")
	pak := parsePackage(string(bytes), filename)
	Println(AstFormat(pak))
	Println("--------------------- typechecked code ---------------------")
	typedPak := TypeCheckPackage(pak)
	Println(AstFormat(typedPak))
	Println("-------------------------- C code --------------------------")
	sourceCodeC := compilePackageToC(typedPak)
	Println(sourceCodeC)
	tempDir := path.Join(os.TempDir(), "golem")
	base := filepath.Base(filename)
	if base[len(base)-6:] != ".golem" {
		panic("Input file must end on .golem")
	}
	base = base[:len(base)-6]
	fileName := Sprintf("%s.c", base)
	absFilename := path.Join(tempDir, fileName)
	err = os.MkdirAll(tempDir, os.ModePerm)
	if err != nil {
		log.Fatal(err)
	}
	err = os.WriteFile(absFilename, []byte(sourceCodeC), 0666)
	if err != nil {
		log.Fatal(err)
	}
	binaryAbsFilename := path.Join(tempDir, base)
	cmd := exec.Command("gcc", absFilename, "-o", binaryAbsFilename)
	err = cmd.Run()
	if err != nil {
		log.Fatal(err)
	}
	Println("=========================== exec ===========================")
	// exec should not return
	log.Fatal(syscall.Exec(binaryAbsFilename, nil, nil))
}
