package main

import "fmt"
import "os"
import "unicode/utf8"
import "strings"
import "io/ioutil"
import u "unicode"
import "path/filepath"

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

  TkOpenBrace
  TkCloseBrace
  TkOpenBracket
  TkCloseBracket
  TkOpenCurly
  TkCloseCurly
	TkCount
);

func init() {
	if TkOpenBrace & TkCloseBrace == 1 {panic("()")}
	if TkOpenBracket & TkCloseBracket == 1 {panic("[]")}
	if TkOpenCurly & TkCloseCurly == 1 {panic("{}")}
	fmt.Println("checks complete")
}

var TokenKindNames = [TkCount]string{
  "Invalid",
  "Ident",
  "Semicolon",
  "Comma",
  "Operator",
  "StringLit",
  "IntLit",
  "FloatLit",
  "LineComment",
  "OpenBrace",
  "CloseBrace",
  "OpenBracket",
  "CloseBracket",
  "OpenCurly",
  "CloseCurly",
};

type Type struct {
  name string
};

var types []Type = []Type{
  {"int"},
	{"float"},
	{"string"},
};

type TypeHandle int;

type Token struct {
  kind TokenKind;
  value string;
};

func (this TokenKind) String() string {
	return TokenKindNames[this]
}

func (this Token) String() string {
	return fmt.Sprintf("(%s %q)", this.kind, this.value)
}

type Tokenizer struct {
	code string
	filename string
	offset int
  bracketStack []Token
  lastToken Token
};

func NewTokenizer(code string, filename string) *Tokenizer {
	return &Tokenizer{code:code, filename:filename}
}

func (this *Tokenizer) LineColumn(offset int) (line,column int) {
	line = 1
	lineStart := 0
	for pos, rune := range this.code {
		if offset <= pos {
			column = pos-lineStart
			return
		}

		if rune == '\n' {
			line++
			lineStart = pos
		}
	}
	return -1, -1
}

func (this *Tokenizer) LineColumn(token Token) (line,column int) {

}

func (this *Tokenizer) LineColumnCurrent() (line,column int) {
	return this.LineColumn(this.offset)
}

func (this *Tokenizer) LineColumnLastToken() (line,columnStart, columnEnd int) {
	fmt.Println(this.lastToken)
	line, columnStart = this.LineColumn(this.offset - len(this.lastToken.value))
	line, columnEnd = this.LineColumn(this.offset)
	return
}

func (this *Tokenizer) checkMatchingBracket(kind TokenKind) bool {
	len := len(this.bracketStack)
	if len > 0 && this.bracketStack[len-1].kind == kind ^ 1 {
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
			result.kind = TkInvalid
		}
	}
	return
}

// LookAhead does the same as Next, but does not modify offset
func (this *Tokenizer) LookAhead() (result Token, newOffset int) {
	code := this.code[this.offset:]
	newOffset = this.offset
	// fmt.Printf("read token in:%s...\n", code[:20]);
  if (len(code) == 0) {
		panic("Cannot read token in empty string");
  }

  {
    // eat whitespace, maybe emit EndLine token
    var idx = 0;
    var gotNewLine = false;

		rune, length := utf8.DecodeRuneInString(code)

    for u.IsSpace(rune) {
      if (rune == '\n') {
        gotNewLine = true;
      }
			idx += length;
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

    code = code[idx:];
  }

  result = Token{kind : TkInvalid, value : code[:1]};

  c, cLen := utf8.DecodeRuneInString(code)
  if (c >= 128) {
    panic("currently only 7 bit ascii is supported!!!")
  }

  switch {
  case u.IsLetter(c):
    result.kind = TkIdent
		result.value = code
		for pos, rune := range code {
			//fmt.Printf("rune: %c pos: %d\n",
			//	rune, pos)

			if !u.IsDigit(rune) && !u.IsLetter(rune) {
				result.value = code[:pos]
				break
			}
	  }
  case c == '#':
    result.kind = TkLineComment;
		result.value = code
		for pos, rune := range code {
			if rune == '\n' {
				result.value = code[:pos]
				break
			}
	  }
  case u.IsDigit(c):
    result.kind = TkIntLit;
    // read number literal
    it := len(code);
		for pos, rune := range code {
			if !u.IsDigit(rune) {
				it = pos
				break
		  }
		}

    if rune, runeLen := utf8.DecodeRuneInString(code[it:]); rune == '.' {
      result.kind = TkFloatLit;
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
      panic("invalid alpha character after literal");
    }

    result.value = code[:it]
  case c == '"':
    result.kind = TkStringLit;
		var lastRune = '"'
		var scanningDone = false
		var idx2 = len(code)
		for pos, rune := range code[cLen:] {
			if scanningDone {
				idx2 = cLen + pos;
				break
		  }

			if lastRune == '\\' {
				switch rune {
				case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\'', '"':
				default:
          panic("illegal escale sequence");
				}
			} else if rune == '"' {
        scanningDone = true
		  }
	  }
		result.value = code[:idx2]
  case c == '(':
    result = Token{TkOpenBrace, code[:cLen]};
  case c == ')':
		result = Token{TkCloseBrace, code[:cLen]};
  case c == '[':
    result = Token{TkOpenBracket, code[:cLen]};
  case c == ']':
		result = Token{TkCloseBracket, code[:cLen]};
  case c == '{':
		result = Token{TkOpenCurly, code[:cLen]};
  case c == '}':
		result = Token{TkCloseCurly, code[:cLen]};
  case c == ',':
		result = Token{TkComma, code[:cLen]};
  case c == ';':
		result = Token{TkSemicolon, code[:cLen]};
	case u.IsSymbol(c) || u.IsPunct(c):
		var idx2 = len(code);
		for pos, rune := range code[cLen:] {
			if !u.IsPunct(rune) && !u.IsSymbol(rune) || strings.ContainsRune("{}[](),;", rune) {
				idx2 = cLen + pos
				break
			}
		}

		result.kind = TkOperator;
		result.value = code[:idx2];
  default:
		fmt.Printf("ispunct %v\n", u.IsSymbol(c) )
		panic(fmt.Sprintf("unexpected input: %c\n", c))
  };

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
	Name string
	Fields []StructField
}

type ProcArgument struct {
	Name string
	Type TypeExpr
}

type Expr struct {
	Value string
}

type ProcDef struct {
	Name string
	Arguments []ProcArgument
	ResultType TypeExpr
	Body Expr
}


type PackageDef struct {
	Name string
	TypeDefs []StructDef
	ProcDefs []ProcDef
}

func (tokenizer *Tokenizer) expectKind(token Token, kind TokenKind) {
	if token.kind != kind {
		panic(fmt.Sprintf("%s(%d, %d-%d) Error: unexpected Token: %v",
			tokenizer.filename, line, columnStart, columnEnd, token))
	}
}

func (token Token) expectIdent(arg string) {
	token.expectKind(TkIdent)
	if token.value != arg {
		panic(fmt.Sprintf("expected ident %v got %v", arg, token.value))
	}
}

func (token Token) expectOperator(arg string) {
	token.expectKind(TkOperator)
	if token.value != arg {
		panic(fmt.Sprintf("expected %v got %v", arg, token.value))
	}
}

func (tokenizer *Tokenizer) parseTypeExpr() (result TypeExpr) {
	token := tokenizer.Next()
	token.expectKind(TkIdent)
	result.Ident = token.value
	return
}

func (tokenizer *Tokenizer) parseExpr() (result Expr) {
	// eat whitespace before setting startOffest
	token := tokenizer.Next()
	startOffset := tokenizer.offset - len(token.value)

	if token.kind == TkIdent {
		result.Value = token.value
		return
	}

	token.expectKind(TkOpenCurly)
	curlyDepth := 1

	for curlyDepth > 0 {
		token = tokenizer.Next()

		if token.kind == TkOpenCurly {
			curlyDepth++
		}
		if token.kind == TkCloseCurly {
			curlyDepth--
	  }
	}

	endOffset := tokenizer.offset
	result.Value = tokenizer.code[startOffset:endOffset]

	return
}

func parseTypeDef(tokenizer *Tokenizer) (result StructDef) {
	var token Token
	token = tokenizer.Next()
	token.expectKind(TkIdent)
	result.Name = token.value
	token = tokenizer.Next()
	token.expectOperator("=")
	token = tokenizer.Next()
	token.expectIdent("struct")

	openBrace := tokenizer.Next()
	openBrace.expectKind(TkOpenCurly)

	token = tokenizer.Next()

	for token.kind == TkSemicolon {
		token = tokenizer.Next()
	}

  for token.kind == TkIdent {
		var structField StructField
		name := token
		name.expectKind(TkIdent)
		structField.Name = name.value
		colon := tokenizer.Next()
		colon.expectOperator(":")
		structField.Type = tokenizer.parseTypeExpr()
		token = tokenizer.Next()
		token.expectKind(TkSemicolon)
		for token.kind == TkSemicolon {
			token = tokenizer.Next()
		}
		result.Fields = append(result.Fields, structField)
	}

	token.expectKind(TkCloseCurly)

  return
}

func parseProcDef(tokenizer *Tokenizer) (result ProcDef) {
	token := tokenizer.Next()
	token.expectKind(TkIdent)
	result.Name = token.value
	token = tokenizer.Next()
	token.expectKind(TkOpenBrace)
	token = tokenizer.Next()

	for token.kind == TkIdent {
		startIndex := len(result.Arguments)

		result.Arguments = append(result.Arguments, ProcArgument{Name: token.value})
		token = tokenizer.Next()
		for token.kind == TkComma {
			token = tokenizer.Next()
			token.expectKind(TkIdent)
			result.Arguments = append(result.Arguments, ProcArgument{Name: token.value})
			token = tokenizer.Next()
		}

		token.expectOperator(":")
  	typ := tokenizer.parseTypeExpr()
		for i := startIndex; i < len(result.Arguments); i++ {
			result.Arguments[i].Type = typ
		}
		token = tokenizer.Next()
	}
	token.expectKind(TkCloseBrace)

	token = tokenizer.Next()
	token.expectOperator(":")
	result.ResultType = tokenizer.parseTypeExpr()
	token = tokenizer.Next()
	token.expectOperator("=")

	result.Body = tokenizer.parseExpr()

	fmt.Printf("Body:\n%s\n", result.Body.Value)
	return
}

func parsePackage(code, packageName string) (result PackageDef) {
	result.Name = packageName
	var tokenizer = NewTokenizer(code)

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
		fmt.Println("Cant Process")
		fmt.Println(tokenizer.code[tokenizer.offset:])
		line, columnStart, columnEnd := tokenizer.LineColumnLastToken()
		msg := fmt.Sprintf("%s(%d, %d-%d) Error: unexpected Token: %v",
			packageName, line, columnStart, columnEnd, token)
		panic(msg)
	}
	return
}

func main() {
	for i, arg := range os.Args {
    fmt.Printf("%d: %s\n", i, arg);
  }

	var filename string
  if len(os.Args) == 2 {
		filename = os.Args[1]
	} else {
    panic("program needs one argument only, the input file");
  }

	filename, err := filepath.Abs(filename)
	if err != nil { panic(err) }

	// base := filepath.Base(filename)
	bytes, err := ioutil.ReadFile(filename)
	if err != nil { panic(err) }
  // tokenize(string(bytes));
	parsePackage(string(bytes), filename)
}

// Local Variables:
// compile-command: "go build && ./golem demo.golem"
// End:
