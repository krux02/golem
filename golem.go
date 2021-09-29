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


//func print(Token token) -> void {
//  printf("%s(%.*s)", TokenKindNames[size_t(token.kind)], (int)len(token.value), token.value.data );
//}

type TokenizeContext struct {
  bracketStack []string
  lastToken Token
};

func (this *TokenizeContext) checkLastBracket(arg string) bool {
	len := len(this.bracketStack)
	if len > 0 && this.bracketStack[len-1] == arg {
		return true
  }
	return false
}

func (this *TokenizeContext) popBracketStack() {
	len := len(this.bracketStack)
	this.bracketStack = this.bracketStack[:len-1]
}

func (this *TokenizeContext) pushBracketStack(arg string) {
	this.bracketStack = append(this.bracketStack, arg)
}


func readToken(context *TokenizeContext, code string) Token {
  if (len(code) == 0){
		fmt.Println("Cannot read token in empty string");
    os.Exit(1)
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
      return Token{ kind : TkSemicolon, value : code[:idx] };
    }

    code = code[idx:];
  }

  var result = Token{kind : TkInvalid, value : code[:1]};

  c, cLen := utf8.DecodeRuneInString(code)
  if (c >= 128) {
    fmt.Println("currently only 7 bit ascii is supported!!!")
		os.Exit(1)
  }

  switch {
  case u.IsLetter(c):
    result.kind = TkIdent
		result.value = code
		for pos, rune := range code {
			if !u.IsDigit(rune) || !u.IsLetter(rune) {
				result.value = code[:pos]
			}
	  }
		return result
  case c == '#':
    result.kind = TkLineComment;
		result.value = code
		for pos, rune := range code {
			if rune == '\n' {
				result.value = code[:pos]
			}
	  }
		return result
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
      fmt.Println("invalid alpha character after literal");
			os.Exit(1)
    }

    result.value = code[:it]
    return result;
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
          fmt.Println("illegal escale sequence");
					os.Exit(1)
				}
			} else if rune == '"' {
        scanningDone = true
		  }
	  }
		result.value = code[:idx2]
  case c == '(':
    context.pushBracketStack(code[:cLen]);
    return Token{TkOpenBrace, code[:cLen]};
  case c == ')':
    if context.checkLastBracket("(") {
			context.popBracketStack()
      return Token{TkCloseBrace, code[:cLen]};
    }
    return result;
  case c == '[':
    context.pushBracketStack(code[:cLen]);
    return Token{TkOpenBrace, code[:cLen]};
  case c == ']':
    if context.checkLastBracket("[") {
			context.popBracketStack()
      return Token{TkCloseBrace, code[:cLen]};
    }
    return result;
  case c == '{':
    context.pushBracketStack(code[:cLen]);
    return Token{TkOpenBrace, code[:cLen]};
  case c == '}':
    if context.checkLastBracket("{") {
			context.popBracketStack()
      return Token{TkCloseBrace, code[:cLen]};
    }
    return result;
  case c == '(':
    context.pushBracketStack(code[:cLen]);
    return Token{TkOpenBrace, code[:cLen]};
  case c == ')':
    if context.checkLastBracket("(") {
			context.popBracketStack()
      return Token{TkCloseBrace, code[:cLen]};
    }
    return result;

  case c == ',': return Token{TkComma, code[:cLen]};
  case c == ';': return Token{TkSemicolon, code[:cLen]};

  default:
    if u.IsPunct(c) {

      var idx2 = len(code);
			for pos, rune := range code[cLen:] {
				if !u.IsPunct(rune) || strings.ContainsRune("{}[](),;", rune) {
					idx2 = cLen + pos
					break
			  }
			}

      result.kind = TkOperator;
      result.value = code[:idx2];
      return result;
    }
		panic(fmt.Sprintf("unexpected input: %c\n", c))
  };

	panic(fmt.Sprintf("unexpected input: %c\n", c))
}

func tokenize(code string) {
  for len(code) > 0 {
		var context = &TokenizeContext{}
    var token = readToken(context, code);
    if token.kind == TkSemicolon {
      fmt.Println("Semicolon(;)");
    } else {
      fmt.Println(token);
    }
		code = code[len(token.value):]
  }
}

func main() {
	for i, arg := range os.Args {
    fmt.Printf("%d: %s\n", i, arg);
  }

  if (len(os.Args) < 2) {
    panic("no input file specified");
  }

  // var file = fileFileToString(os.Args[1]);

	bytes, err := ioutil.ReadFile("/home/arne/proj/golem-language/prototype1/demo.golem")
	if err != nil { panic(err) }
	file := string(bytes)


  fmt.Println("all file:")
	fmt.Println(file)

  tokenize(file);
}

// Local Variables:
// compile-command: "go build && ./golem"
// End:
