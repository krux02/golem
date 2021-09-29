package main

import "fmt"
import "os"
import "unicode/utf8"
import "strings"
import "io/ioutil"
// import "regexp"
import u "unicode"


// (project-try-vc default-directory)
// (vc-responsible-backend default-directory)
// (project-current t)
// (project-compile)
// (project-find-file)
// (project-compile)
// (project-root (project-current t))
//  default-directory
// (toggle-debug-on-error t)
// MatchString
// https://0x0.st/-Y01.el

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

func (this Token) String() string {
	return fmt.Sprintf("<%s %q>", TokenKindNames[this.kind], this.value)
}

//func print(Token token) -> void {
//  printf("%s(%.*s)", TokenKindNames[size_t(token.kind)], (int)len(token.value), token.value.data );
//}

type Tokenizer struct {
	code string
	offset int
  bracketStack []string
  lastToken Token
};

func NewTokenizer(code string) *Tokenizer {
	return &Tokenizer{code:code}
}


func (this *Tokenizer) checkLastBracket(arg string) bool {
	len := len(this.bracketStack)
	if len > 0 && this.bracketStack[len-1] == arg {
		return true
  }
	return false
}

func (this *Tokenizer) popBracketStack() {
	len := len(this.bracketStack)
	this.bracketStack = this.bracketStack[:len-1]
}

func (this *Tokenizer) pushBracketStack(arg string) {
	this.bracketStack = append(this.bracketStack, arg)
}


func readToken(context *Tokenizer, code string) (result Token, offset int) {
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

    if gotNewLine &&
        // do not infer semicolon when within braces
        (len(context.bracketStack) == 0 || context.bracketStack[len(context.bracketStack)-1] != "(") {
        // TODOdo not infer semicolon after comments
			result.kind = TkSemicolon
			result.value = code[:idx]
      return
    }

    code = code[idx:];
		offset = idx
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

		return
  case c == '#':
    result.kind = TkLineComment;
		result.value = code
		for pos, rune := range code {
			if rune == '\n' {
				result.value = code[:pos]
				break
			}
	  }
		return
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
		return
  case c == '(':
    context.pushBracketStack(code[:cLen]);
    result = Token{TkOpenBrace, code[:cLen]};
		return
  case c == ')':
    if context.checkLastBracket("(") {
			context.popBracketStack()
			result = Token{TkCloseBrace, code[:cLen]};
    }
    return
  case c == '[':
    context.pushBracketStack(code[:cLen]);
    result = Token{TkOpenBrace, code[:cLen]};
		return
  case c == ']':
    if context.checkLastBracket("[") {
			context.popBracketStack()
      result = Token{TkCloseBrace, code[:cLen]};
    }
    return;
  case c == '{':
    context.pushBracketStack(code[:cLen]);
		result = Token{TkOpenBrace, code[:cLen]};
    return
  case c == '}':
    if context.checkLastBracket("{") {
			context.popBracketStack()
			result = Token{TkCloseBrace, code[:cLen]};
    }
    return
  case c == '(':
    context.pushBracketStack(code[:cLen]);
		result = Token{TkOpenBrace, code[:cLen]};
    return
  case c == ')':
    if context.checkLastBracket("(") {
			context.popBracketStack()
      result = Token{TkCloseBrace, code[:cLen]};
    }
    return
  case c == ',':
		result = Token{TkComma, code[:cLen]};
		return
  case c == ';':
		result = Token{TkSemicolon, code[:cLen]};
		return

  default:
    if u.IsSymbol(c) || u.IsPunct(c) {

      var idx2 = len(code);
			for pos, rune := range code[cLen:] {
				if !u.IsPunct(rune) && !u.IsSymbol(rune) || strings.ContainsRune("{}[](),;", rune) {
					idx2 = cLen + pos
					break
			  }
			}

      result.kind = TkOperator;
      result.value = code[:idx2];
      return
    }
		fmt.Printf("ispunct %v\n", u.IsSymbol(c) )
		panic(fmt.Sprintf("unexpected input: %c\n", c))
  };

	fmt.Printf("ispunct %v\n", u.IsPunct(c) )
	panic(fmt.Sprintf("unexpected input: %c\n", c))
}

func (this *Tokenizer) Next() Token {
	var token, offset = readToken(this, this.code[this.offset:])
	this.offset += offset + len(token.value)
	return token
}

func (this *Tokenizer) AtEnd() bool {
	return this.offset == len(this.code)
}

func tokenize(code string) {
	var tokenizer = NewTokenizer(code)
  for !tokenizer.AtEnd() {
		token := tokenizer.Next()
		fmt.Println(token)
	}
}

func main() {
	for i, arg := range os.Args {
    fmt.Printf("%d: %s\n", i, arg);
  }

  // if (len(os.Args) < 2) {
  //   panic("no input file specified");
  // }
  // var file = fileFileToString(os.Args[1]);

	filepath := "/home/arne/proj/golem-language/prototype1/demo.golem"

	bytes, err := ioutil.ReadFile(filepath)
	if err != nil { panic(err) }
	file := string(bytes)


  fmt.Println("all file:")
	fmt.Println(file)
	fmt.Println("------------------------------------------------------------------------------");

  tokenize(file);
}

// Local Variables:
// compile-command: "go build && ./golem"
// End:
