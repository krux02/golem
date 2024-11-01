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
	TkRawStrLit
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
	TkRawStrLit:         "RawStrLit",
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
	code                     string
	filename                 string
	token, lookAheadToken    Token
	offset, lookAheadOffset  int
	lastMultilineStrLitLines []string
	errors                   []ParseError
	silentErrors             bool
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

	// TODO, this only works for runes, not for grapheme clusters
	line = 1
	for pos, rune := range code {
		if offset <= pos {
			break
		}
		if rune == '\n' {
			line++
			column = 0
		} else {
			column += 1
		}
	}
	return line, column
}

func LineColumnStr(str, substr string) (line, columnStart, columnEnd int) {
	// TODO actually support multi line references
	if idxNewLine := strings.IndexByte(substr, '\n'); idxNewLine > 0 {
		substr = substr[0:idxNewLine]
	}

	data1 := (uintptr)(unsafe.Pointer(unsafe.StringData(str)))
	data2 := (uintptr)(unsafe.Pointer(unsafe.StringData(substr)))
	if data2 < data1 {
		return -1, -1, -1
	}
	offset := int(data2 - data1)
	line1, columnStart := LineColumnOffset(str, offset)
	line2, columnEnd := LineColumnOffset(str, offset+len(substr))

	if line1 == line2 {
		return line1, columnStart, columnEnd
	}

	// multiline error messages not yet supported
	return line1, columnStart, -1
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

func (this *Tokenizer) MatchMultilineRawString(code string) Token {
	var state int = 0

	this.lastMultilineStrLitLines = this.lastMultilineStrLitLines[:0]

	commentIssue := false
	lastCommentStart := -1
	lastLineEnd := -1

	for idx2, char := range code {
		switch state {
		case 0: // new beginning of a new line
			switch char {
			case '\\':
				state = 1
			case ' ':
				state = 4
			default:
			}
		case 1: // got the first backslash expect the second one
			switch char {
			case '\\':
				state = 2
			default:
				goto DONE
			}
		case 2: // skipping the first ' ' after \\
			switch char {
			case '\n': // unless it is a newline then we see it as an empty line
				this.lastMultilineStrLitLines = append(this.lastMultilineStrLitLines, "")
				lastLineEnd = idx2
				state = 0
			default:
				if char == ' ' {
					lastCommentStart = idx2 + 1
				} else {
					commentIssue = true
				}
				state = 3
			}
		case 3: // scannting till end of line, no matter what
			switch char {
			case '\n':
				if 0 <= lastCommentStart {
					line := code[lastCommentStart:idx2]
					this.lastMultilineStrLitLines = append(this.lastMultilineStrLitLines, line)
				}
				lastCommentStart = -1
				lastLineEnd = idx2
				state = 0
			}
		case 4: // eat all whitespace till \ or anything else
			switch char {
			case '\\':
				state = 1
			case ' ':
				// stay in this state
			default:
				// not part of the multiline raw string literal anymore
				goto DONE
			}
		}
	}
DONE:
	if lastLineEnd == 0 {
		fmt.Println(code)
		panic("should not happen")
	}

	result := Token{
		kind:  TkRawStrLit,
		value: code[:lastLineEnd],
	}

	if commentIssue {
		this.reportError(result, "first char after \\ in raw string literal must be a ' ' space")
	}
	return result
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
		newlineEscape:
			rune2, length2 := utf8.DecodeRuneInString(code[idx+length:])
			switch rune2 {
			case ' ', '\r':
				idx += length + length2
				goto newlineEscape
			case '\n':
				idx += length + length2
				goto eatWhiteSpace
			case '\\':
				// do nothing here
			default:
				idx += length + length2
				result.kind = TkInvalid
				result.value = code[idx-1 : idx+length+length2]
				newOffset += idx
				this.reportError(result, "invalid escape sequence \\%c", rune2)
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
		case "and", "or", "not", "in", "notin", "ptr", "addr": // , "conv", "cast":
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
	case c == '\\':

		if strings.HasPrefix(code, "\\\\") {
			result = this.MatchMultilineRawString(code)
		} else {
			// if the compiler is correct, this should be unreachable.
			// result.kind = TkInvalid
			this.reportError(result, "unexpected input: %c %d", c, c)
		}
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
