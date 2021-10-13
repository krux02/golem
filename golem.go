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
	TkLineComment

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
	TkLineComment:  "LineComment",
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
	case TkLineComment:
		// eat automatically generated TkSemicolon at end of each line
		// (the semicolon should probably not be generated here?)
		next := tokenizer.Next()
		tokenizer.expectKind(next, TkSemicolon)
		return tokenizer.parseExpr()
	case TkIdent:
		sym := Symbol{Name: token.value}

		lookAhead := tokenizer.lookAheadToken

		switch lookAhead.kind {
		case TkSemicolon, TkCloseBrace, TkCloseCurly:
			return (Expr)(sym)

		case TkOperator:
			token = tokenizer.Next()
			operator := Symbol{Name: token.value}
			rhs := tokenizer.parseExpr()
			call := Call{Sym: operator, Args: []Expr{sym, rhs}}

			if rhsCall, ok := rhs.(Call); ok && !rhsCall.Braced {
				rhsOperator := rhsCall.Sym
				if OperatorPrecedence[operator.Name] > OperatorPrecedence[rhsOperator.Name] {
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
				lookAhead = tokenizer.lookAheadToken
				switch lookAhead.kind {
				case TkIdent, TkStrLit:
					args = append(args, tokenizer.parseExpr())
					continue
				case TkCloseBrace:
					token = tokenizer.Next()
					call := Call{Sym: sym, Args: args}
					return (Expr)(call)
				}
				panic(tokenizer.wrongKind(lookAhead))
			}
		}

		panic(tokenizer.wrongKind(lookAhead))
	case TkOpenCurly:
		// parse block
		var block CodeBlock

		for tokenizer.lookAheadToken.kind == TkSemicolon {
			tokenizer.Next()
		}

		for tokenizer.lookAheadToken.kind != TkCloseCurly {
			item := tokenizer.parseExpr()
			block.Items = append(block.Items, item)

			next := tokenizer.Next()
			tokenizer.expectKind(next, TkSemicolon)
		}

		endToken := tokenizer.Next()

		tokenizer.expectKind(endToken, TkCloseCurly)
		// tokenizer.expectKind(endtoken, TkCloseCurly)
		return (Expr)(block)
	case TkStrLit:
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

		lit := StrLit{Value: b.String()}
		return (Expr)(lit)
	case TkIntLit:
		intValue, err := strconv.Atoi(token.value)
		if err != nil {
			panic("internal error invalid int token")
		}
		lit := IntLit{Value: intValue}
		return (Expr)(lit)
	}

	panic(tokenizer.wrongKind(token))
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
	Println("processing package: ", result.Name)
	var tokenizer = NewTokenizer(code, filename)

	for true {
		token := tokenizer.Next()
		switch token.kind {
		case TkLineComment:
			continue
		case TkSemicolon:
			continue
		case TkEof:
			return
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
		panic(tokenizer.wrongKind(tokenizer.token))
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
	// tokenize(string(bytes));
	pak := parsePackage(string(bytes), filename)

	Println(pak.Name)
	for _, typ := range pak.TypeDefs {
		Println(typ.String())
	}
	for _, proc := range pak.ProcDefs {
		Println(proc.String())
	}

	checkpackage := TypeCheckPackage(pak)

	Println("------------------------------------------------------------\n")

	sourceCodeC := compilePackageToC(checkpackage)

	Println(sourceCodeC)
	tempDir := path.Join(os.TempDir(), "golem")
	fileName := Sprintf("%s.c", filepath.Base(filename))
	absFilename := path.Join(tempDir, fileName)
	Println(absFilename)

	err = os.MkdirAll(tempDir, os.ModePerm)
	if err != nil {
		log.Fatal(err)
	}
	err = os.WriteFile(absFilename, []byte(sourceCodeC), 0666)
	if err != nil {
		log.Fatal(err)
	}

}
