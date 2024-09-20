package main

import (
	"fmt"
	"path/filepath"
	"strings"
	u "unicode"
	"unicode/utf8"
	"unsafe"
)

type TokenKind int16

const (
	TkInvalid TokenKind = iota
	TkIdent
	TkNewLine
	TkSemicolon
	TkComma
	TkOperator
	TkStrLit
	TkIntLit
	TkHexLit
	TkFloatLit
	TkNilLit
	TkPrefixDocComment
	TkPostfixDocComment

	// keywords
	TkType
	TkProc
	TkTemplate
	TkMacro
	TkVar
	TkLet
	TkConst
	TkReturn
	TkDiscard
	TkBreak
	TkContinue
	TkStruct
	TkUnion
	TkEnum
	TkTrait

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
	TkNewLine:           "NewLine",
	TkSemicolon:         "Semicolon",
	TkComma:             "Comma",
	TkOperator:          "Operator",
	TkStrLit:            "StrLit",
	TkIntLit:            "IntLit",
	TkFloatLit:          "FloatLit",
	TkNilLit:            "NilLit",
	TkPrefixDocComment:  "PrefixDocComment",
	TkPostfixDocComment: "PostfixDocComment",
	TkType:              "Type",
	TkProc:              "Proc",
	TkTemplate:          "Template",
	TkMacro:             "Macro",
	TkVar:               "Var",
	TkLet:               "Let",
	TkConst:             "Const",
	TkReturn:            "Return",
	TkDiscard:           "Discard",
	TkBreak:             "Break",
	TkContinue:          "Continue",
	TkStruct:            "Struct",
	TkUnion:             "Union",
	TkEnum:              "Enum",
	TkTrait:             "Trait",
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

type ParseError struct {
	code Token // untokenizable code
	msg  string
}

// this is by now far from just a tokenizer, has evolved to become the
// full parser of the language.

type Tokenizer struct {
	code                    string
	filename                string
	token, lookAheadToken   Token
	offset, lookAheadOffset int
	errors                  []ParseError
	silentErrors            bool
}

func NewTokenizer(code string, filename string) (result *Tokenizer) {
	absfilename, err := filepath.Abs(filename)
	if err != nil {
		panic(err)
	}
	result = &Tokenizer{code: code, filename: absfilename}
	result.lookAheadToken, result.lookAheadOffset = result.ScanTokenAt(0)
	return result
}

func LineColumnOffset(code string, offset int) (line, column int) {
	if offset > len(code) {
		return -1, -1
	}

	line = 1
	lineStart := 0
	for pos, rune := range code {
		if offset <= pos {
			// TODO, this only works for ASCII not for multibyte runes
			column = pos - lineStart
			break
		}
		if rune == '\n' {
			line++
			lineStart = pos + 1
		}
	}
	return line, column
}

func LineColumnStr(str, substr string) (line, columnStart, columnEnd int) {
	data1 := (uintptr)(unsafe.Pointer(unsafe.StringData(str)))
	data2 := (uintptr)(unsafe.Pointer(unsafe.StringData(substr)))
	if data2 < data1 {
		return -1, -1, -1
	}
	offset := int(data2 - data1)
	line, columnStart = LineColumnOffset(str, offset)
	columnEnd = columnStart + len(substr)
	return line, columnStart, columnEnd
}

func (this *Tokenizer) LineColumnCurrent() (line, column int) {
	return LineColumnOffset(this.code, this.offset)
}

func (this *Tokenizer) LineColumnToken(token Token) (line, columnStart, columnEnd int) {
	return LineColumnStr(this.code, token.value)
}

func (this *Tokenizer) Next() Token {
	if this.token.kind == TkEof {
		// prevent an endless token stream of <EOF> tokens where parsing never stops
		panic("no token beyond <EOF>")
	}
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
		// go optimizes the empty string to have a null pointer. This breaks the
		// line information that is encoded in the pointer value. If I do something
		// about it, the go runtime will crash and complain about a bad pointer
		// value.
		result.value = code
		return result, newOffset
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
		case '\\': // very questionable implementation here
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
				result.kind = TkInvalid
				result.value = code[idx-1 : idx+length]
				idx += length
				newOffset += idx
				this.reportError(result, "invalid escape sequence \\%c", rune)
				return result, newOffset
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
			return result, newOffset
		}

		if gotNewLine {
			newOffset += idx
			result.kind = TkNewLine
			result.value = code[:idx]
			return result, newOffset
		}

		code = code[idx:]
		newOffset += idx
	}

	result = Token{kind: TkInvalid, value: code[:1]}

	c, cLen := utf8.DecodeRuneInString(code)

	// this is not strictly failsafe
	if c >= 128 {
		invalidRangeLength := 0
		for c >= 128 {
			invalidRangeLength += cLen
			c, cLen = utf8.DecodeRuneInString(code[invalidRangeLength:])
		}
		result.kind = TkInvalid
		result.value = code[:invalidRangeLength]
		newOffset += invalidRangeLength
		this.reportError(result, "currently only 7 bit ASCII is supported")
		return result, newOffset
	}

	switch {
	case u.IsLetter(c) || c == '_':
		result.value = code
		for pos, rune := range code {
			if !u.IsDigit(rune) && !u.IsLetter(rune) && rune != '_' {
				result.value = code[:pos]
				break
			}
		}
		switch result.value {
		// builtin word operators
		case "and", "or", "not", "in", "notin", "ptr", "addr":
			result.kind = TkOperator
		case "type":
			result.kind = TkType
		case "proc":
			result.kind = TkProc
		case "macro":
			result.kind = TkMacro
		case "template":
			result.kind = TkTemplate
		case "var":
			result.kind = TkVar
		case "let":
			result.kind = TkLet
		case "const":
			result.kind = TkConst
		case "return":
			result.kind = TkReturn
		case "discard":
			result.kind = TkDiscard
		case "break":
			result.kind = TkBreak
		case "continue":
			result.kind = TkContinue
		case "struct":
			result.kind = TkStruct
		case "union":
			result.kind = TkUnion
		case "enum":
			result.kind = TkEnum
		case "trait":
			result.kind = TkTrait
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
				this.reportError(result, "incomplete floating point literal")
				result.kind = TkInvalid
				result.value = code[:it]
				newOffset += it
				return result, newOffset
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
			this.reportError(result, "invalid alpha character %c after literal: %s", rune, code[:it])

			for u.IsLetter(rune) {
				it += runeLen
				rune, runeLen = utf8.DecodeRuneInString(code[it:])
			}

			result.kind = TkInvalid
			result.value = code[:it]
			newOffset += it
			return result, newOffset
		}

		result.value = code[:it]

	case c == '"':
		result.kind = TkStrLit
		delimiter := c
		lastRune := c
		scanningDone := false
		idx2 := len(code)
		for pos, rune := range code[cLen:] {
			if scanningDone {
				idx2 = cLen + pos
				break
			}
			if lastRune != '\\' && rune == delimiter {
				// checking for correct escape sequences is done parseStrLit and parseCharLit
				scanningDone = true
			}
			lastRune = rune
		}
		result.value = code[:idx2]
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
	default:
		this.reportError(result, "unexpected input: %c %d", c, c)
	}

	newOffset += len(result.value)
	return result, newOffset
}

func (this *Tokenizer) AtEnd() bool {
	return this.offset == len(this.code)
}

const fatalError = false

func (tokenizer *Tokenizer) reportError(token Token, msg string, args ...interface{}) {
	newMsg := fmt.Sprintf(msg, args...)
	error := ParseError{token, newMsg}
	tokenizer.errors = append(tokenizer.errors, error)

	if !tokenizer.silentErrors {
		line, columnStart, columnEnd := LineColumnStr(tokenizer.code, token.value)
		// LineColumnStr(tokenizer.code, token.value)
		fmt.Printf("%s(%d, %d-%d) Error: %s\n", tokenizer.filename, line, columnStart, columnEnd, newMsg)
	}
	if fatalError {
		panic("error caused here")
	}
	return
}

// 	line, columnStart, columnEnd := tokenizer.LineColumnToken(token)
// 	return fmt.Errorf("%s(%d, %d-%d) Error: %s",
// 		tokenizer.filename, line, columnStart, columnEnd,
// 		fmt.Sprintf(msg, args...))
// }

func (tokenizer *Tokenizer) reportWrongKind(token Token) {
	if token.kind != TkInvalid {
		// Skip reporting invalid tokens, as they should already be reported as an error on creating them.
		tokenizer.reportError(token, "unexpected token: %s value: '%s'", TokenKindNames[token.kind], token.value)
	}
	return
}

func (tokenizer *Tokenizer) reportWrongIdent(token Token) {
	tokenizer.reportError(token, "unexpected identifier: %s", token.value)
}

func (tokenizer *Tokenizer) expectKind(token Token, kind TokenKind) bool {
	if kind == TkInvalid {
		panic("never expect an invalid token")
	}
	if token.kind != kind {
		tokenizer.reportWrongKind(token)
		return false
	}
	return true
}

func (tokenizer *Tokenizer) expectKind2(token Token, kind1, kind2 TokenKind) bool {
	if kind1 == TkInvalid || kind2 == TkInvalid {
		panic("never expect an invalid token")
	}
	if token.kind != kind1 && token.kind != kind2 {
		tokenizer.reportWrongKind(token)
		return false
	}
	return true
}

func (tokenizer *Tokenizer) expectKind3(token Token, kind1, kind2, kind3 TokenKind) bool {
	if kind1 == TkInvalid || kind2 == TkInvalid || kind3 == TkInvalid {
		panic("never expect an invalid token")
	}
	if token.kind != kind1 && token.kind != kind2 && token.kind != kind3 {
		tokenizer.reportWrongKind(token)
		return false
	}
	return true
}

func (tokenizer *Tokenizer) expectOperator(token Token, arg string) bool {
	tokenizer.expectKind(token, TkOperator)
	if token.value != arg {
		tokenizer.reportError(token, "expected %v got %v", arg, token.value)
		return false
	}
	return true
}
