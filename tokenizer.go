package main

import (
	"fmt"
	"reflect"
	"strings"
	u "unicode"
	"unicode/utf8"
	"unsafe"
)

type TokenKind int16

const (
	TkInvalid TokenKind = iota
	TkIdent
	TkColon
	TkSemicolon
	TkComma
	TkOperator
	TkStrLit
	TkIntLit
	TkFloatLit
	TkCharLit
	TkDocComment

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
	TkInvalid:      "Invalid",
	TkIdent:        "Ident",
	TkColon:        "Colon",
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
	case c == ':':
		result = Token{TkColon, code[:cLen]}
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
		fmt.Printf("ispunct %v\n", u.IsSymbol(c))
		panic(fmt.Sprintf("unexpected input: %c %d\n", c, c))
	}

	newOffset += len(result.value)
	return
}

func (this *Tokenizer) AtEnd() bool {
	return this.offset == len(this.code)
}

func (tokenizer *Tokenizer) wrongKind(token Token) string {
	line, columnStart, columnEnd := tokenizer.LineColumnToken(token)
	return fmt.Sprintf("%s(%d, %d-%d) Error: unexpected Token: %v",
		tokenizer.filename, line, columnStart, columnEnd, token)
}

func (tokenizer *Tokenizer) wrongIdent(token Token) string {
	line, columnStart, columnEnd := tokenizer.LineColumnToken(token)
	return fmt.Sprintf("%s(%d, %d-%d) Error: unexpected identifier: %s",
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
		panic(fmt.Sprintf("expected ident %v got %v", arg, token.value))
	}
}

func (tokenizer *Tokenizer) expectOperator(token Token, arg string) {
	tokenizer.expectKind(token, TkOperator)
	if token.value != arg {
		panic(fmt.Sprintf("expected %v got %v", arg, token.value))
	}
}
