package main

import (
	"fmt"
	"strings"
	u "unicode"
	"unicode/utf8"
	"unsafe"
)

type TokenKind int16

const (
	TkInvalid TokenKind = iota
	TkIdent
	TkSemicolon
	TkComma
	TkAssign
	TkOperator
	TkStrLit
	TkIntLit
	TkHexLit
	TkFloatLit
	TkCharLit
	TkNilLit
	TkPrefixDocComment
	TkPostfixDocComment

	// keywords
	TkType
	TkProc
	TkVar
	TkLet
	TkConst
	TkReturn
	TkBreak
	TkContinue
	TkStatic
	TkEmit

	TkOr
	TkAnd
	TkIn
	TkIf
	TkElse
	TkFor
	TkWhile
	TkDo

	// These tokens must be convertible from Open/Close by flipping the last bit
	TkOpenBrace    TokenKind = 100
	TkCloseBrace   TokenKind = 101
	TkOpenBracket  TokenKind = 102
	TkCloseBracket TokenKind = 103
	TkOpenCurly    TokenKind = 104
	TkCloseCurly   TokenKind = 105

	TkEof TokenKind = iota // end of file
	TkCount
)

var TokenKindNames = [...]string{
	TkInvalid:           "Invalid",
	TkIdent:             "Ident",
	TkSemicolon:         "Semicolon",
	TkComma:             "Comma",
	TkAssign:            "Assign",
	TkOperator:          "Operator",
	TkStrLit:            "StrLit",
	TkIntLit:            "IntLit",
	TkFloatLit:          "FloatLit",
	TkCharLit:           "CharLit",
	TkNilLit:            "NilLit",
	TkPrefixDocComment:  "PrefixDocComment",
	TkPostfixDocComment: "PostfixDocComment",
	TkType:              "Type",
	TkProc:              "Proc",
	TkVar:               "Var",
	TkLet:               "Let",
	TkConst:             "Const",
	TkReturn:            "Return",
	TkBreak:             "Break",
	TkContinue:          "Continue",
	TkStatic:            "Static",
	TkEmit:              "Emit",
	TkOr:                "Or",
	TkAnd:               "And",
	TkIn:                "In",
	TkIf:                "If",
	TkElse:              "Else",
	TkFor:               "For",
	TkWhile:             "While",
	TkDo:                "Do",
	TkOpenBrace:         "OpenBrace",
	TkCloseBrace:        "CloseBrace",
	TkOpenBracket:       "OpenBracket",
	TkCloseBracket:      "CloseBracket",
	TkOpenCurly:         "OpenCurly",
	TkCloseCurly:        "CloseCurly",
	TkEof:               "<EOF>",
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

func LineColumnOffset(code string, offset int) (line, column int) {
	line = 1
	lineStart := 0
	for pos, rune := range code {
		if offset <= pos {
			column = pos - lineStart
			return
		}
		if rune == '\n' {
			line++
			lineStart = pos + 1
		}
	}
	return -1, -1
}

func LineColumnStr(str, substr string) (line, columnStart, columnEnd int) {
	data1 := (uintptr)(unsafe.Pointer(unsafe.StringData(str)))
	data2 := (uintptr)(unsafe.Pointer(unsafe.StringData(substr)))
	if data2 < data1 {
		panic(fmt.Sprintf("internal error, no substring, (%d %d) (%d %d)", data1, len(str), data2, len(substr)))
	}
	offset := int(data2 - data1)
	line, columnStart = LineColumnOffset(str, offset)
	columnEnd = columnStart + len(substr)
	return
}

func (this *Tokenizer) LineColumnCurrent() (line, column int) {
	return LineColumnOffset(this.code, this.offset)
}

func (this *Tokenizer) LineColumnToken(token Token) (line, columnStart, columnEnd int) {
	return LineColumnStr(this.code, token.value)
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
		var gotComment = false

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
			if rune2, _ := utf8.DecodeRuneInString(code[idx+length:]); rune2 == '#' {
				gotComment = true
			}
			offset := strings.IndexByte(code[idx:], '\n')
			if offset != -1 {
				idx += offset
			} else {
				idx = len(code)
			}
			if gotNewLine || !gotComment {
				goto eatWhiteSpace
			}
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

		if gotComment {
			newOffset += idx
			if gotNewLine {
				result.kind = TkPrefixDocComment
			} else {
				result.kind = TkPostfixDocComment
			}
			result.value = code[:idx]
			return
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
		result.value = code
		for pos, rune := range code {
			if !u.IsDigit(rune) && !u.IsLetter(rune) && rune != '_' {
				result.value = code[:pos]
				break
			}
		}
		switch result.value {

		case "type":
			result.kind = TkType
		case "proc":
			result.kind = TkProc
		case "var":
			result.kind = TkVar
		case "let":
			result.kind = TkLet
		case "const":
			result.kind = TkConst
		case "return":
			result.kind = TkReturn
		case "break":
			result.kind = TkBreak
		case "continue":
			result.kind = TkContinue
		case "static":
			result.kind = TkStatic
		case "emit":
			result.kind = TkEmit
		case "or":
			result.kind = TkOr
		case "and":
			result.kind = TkAnd
		case "in":
			result.kind = TkIn
		case "if":
			result.kind = TkIf
		case "else":
			result.kind = TkElse
		case "for":
			result.kind = TkFor
		case "while":
			result.kind = TkWhile
		case "do":
			result.kind = TkDo
		case "nil":
			result.kind = TkNilLit
		default:
			result.kind = TkIdent
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

		rune, runeLen := utf8.DecodeRuneInString(code[it:])
		if rune == '.' {
			result.kind = TkFloatLit
			it += runeLen

			rune, _ := utf8.DecodeRuneInString(code[it:])

			if !u.IsDigit(rune) {
				panic(this.Errorf(result, "incomplete floating point literal"))
			}

			var idx2 = len(code)
			for pos, rune := range code[it:] {
				if !u.IsDigit(rune) {
					idx2 = it + pos
					break
				}
			}
			it = idx2
		} else if rune == 'x' && code[:it] == "0" {
			result.kind = TkHexLit
			for pos, rune := range code[2:] {
				switch rune {
				case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F':
					continue
				}
				it = pos + 2
				break
			}
		}

		rune, runeLen = utf8.DecodeRuneInString(code[it:])
		if u.IsLetter(rune) {
			panic(fmt.Errorf("invalid alpha character %c after literal: %s", rune, code[:it]))
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
			lastRune = rune
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
				panic(fmt.Errorf("illegal escape sequence at: %s...", code[:5]))
			}
			result.value = code[:cLen+rune1Len+rune2Len]
		}
	case c == '`':
		result.kind = TkIdent
		for pos, rune := range code[cLen:] {
			if rune == '`' {
				result.value = code[cLen : cLen+pos]
				newOffset += cLen * 2
				break
			}
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
			if !u.IsPunct(rune) && !u.IsSymbol(rune) || strings.ContainsRune("{}[](),;'\"`", rune) {
				idx2 = cLen + pos
				break
			}
		}

		result.kind = TkOperator
		result.value = code[:idx2]
		if result.value == "=" {
			result.kind = TkAssign
		}
	default:
		panic(this.Errorf(result, "unexpected input: %c %d", c, c))
	}

	newOffset += len(result.value)
	return
}

func (this *Tokenizer) AtEnd() bool {
	return this.offset == len(this.code)
}

func (tokenizer *Tokenizer) Errorf(token Token, msg string, args ...interface{}) error {
	line, columnStart, columnEnd := tokenizer.LineColumnToken(token)
	return fmt.Errorf("%s(%d, %d-%d) Error: %s",
		tokenizer.filename, line, columnStart, columnEnd,
		fmt.Sprintf(msg, args...))
}

func (tokenizer *Tokenizer) formatWrongKind(token Token) error {
	return tokenizer.Errorf(token, "unexpected token: %v", token)
}

func (tokenizer *Tokenizer) formatWrongIdent(token Token) error {
	return tokenizer.Errorf(token, "unexpected identifier: %s", token.value)
}

func (tokenizer *Tokenizer) expectKind(token Token, kind TokenKind) {
	if token.kind != kind {
		panic(tokenizer.formatWrongKind(token))
	}
}

func (tokenizer *Tokenizer) expectKind2(token Token, kind1, kind2 TokenKind) {
	if token.kind != kind1 && token.kind != kind2 {
		panic(tokenizer.formatWrongKind(token))
	}
}

func (tokenizer *Tokenizer) expectIdent(token Token, ident string) {
	tokenizer.expectKind(token, TkIdent)
	if token.value != ident {
		panic(tokenizer.Errorf(token, "expected ident %v got %v", ident, token.value))
	}
}

func (tokenizer *Tokenizer) expectIdent2(token Token, ident1, ident2 string) {
	tokenizer.expectKind(token, TkIdent)
	if token.value != ident1 && token.value != ident2 {
		panic(tokenizer.Errorf(token, "expected ident %v or %v got %v", ident1, ident2, token.value))
	}
}

func (tokenizer *Tokenizer) expectOperator(token Token, arg string) {
	tokenizer.expectKind(token, TkOperator)
	if token.value != arg {
		panic(tokenizer.Errorf(token, "expected %v got %v", arg, token.value))
	}
}
