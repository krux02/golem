package main

import (
	"strings"
	"fmt"
)

type CCodeGeneratorContext struct {
	strings.Builder
	indentation int
	pak         PackageDef
}

func (context *CCodeGeneratorContext) newlineAndIndent() {
	context.WriteString("\n")
	for i := 0; i < context.indentation; i++ {
		context.WriteString("  ")
	}
}

func (context *CCodeGeneratorContext) compileTypeExpr(typ TypeExpr) {
	context.WriteString(typ.Ident)
}

func (context *CCodeGeneratorContext) compileCall(call Call) {
	switch call.Sym.Value {
	case "+", "-", "*", "/":
		context.WriteString("(")
		for i, it := range call.Args {
			if i != 0 {
				context.WriteString(" ")
				context.WriteString(call.Sym.Value)
				context.WriteString(" ")
			}
			context.compileExpr(it)
		}
		context.WriteString(")")
  default:
		context.WriteString(call.Sym.Value)
		context.WriteString("(")
		for i, it := range call.Args {
			if i != 0 {
				context.WriteString(", ")
			}
			context.compileExpr(it)
		}
		context.WriteString(")")
	}
}

func (context *CCodeGeneratorContext) compileStrLit(lit StrLit) {
	context.WriteRune('"')
	for _, rune := range lit.Val {
		switch rune {
		case '\a':
			context.WriteString("\\a")
		case '\b':
			context.WriteString("\\b")
		case '\f':
			context.WriteString("\\f")
		case '\n':
			context.WriteString("\\n")
		case '\r':
			context.WriteString("\\r")
		case '\t':
			context.WriteString("\\t")
		case '\v':
			context.WriteString("\\v")
		case '\\':
			context.WriteString("\\\\")
		case '\'':
			context.WriteString("\\'")
		case '"':
			context.WriteString("\\\"")
		default:
			context.WriteRune(rune)
		}
	}
	//context.WriteString(lit.Val)
	context.WriteRune('"')
}

func (context *CCodeGeneratorContext) compileExpr(expr Expr) {
	switch ex := expr.(type) {
	case CodeBlock:
		context.compileCodeBlock(ex)
	case Call:
		context.compileCall(ex)
	case StrLit:
		context.compileStrLit(ex)
	default:
		panic(fmt.Sprintf("Not implemented %T", expr))
	}
}

func (context *CCodeGeneratorContext) compileCodeBlock(block CodeBlock) {
	context.WriteString("{")
	context.indentation += 1

	for _, expr := range block.Items {
		context.newlineAndIndent()
		context.compileExpr(expr)
	}

	context.indentation -= 1
	context.newlineAndIndent()
	context.WriteString("}")
	context.newlineAndIndent()

}

func (context *CCodeGeneratorContext) compileProcDef(procDef ProcDef) {
	context.compileTypeExpr(procDef.ResultType)
	// context.compileTypeExpr(procDef.ResultType)
	context.WriteString(" ")
	context.WriteString(procDef.Name)
	context.WriteString("(")
	for i, arg := range procDef.Args {
		if i != 0 {
			context.WriteString(", ")
		}
		context.compileTypeExpr(arg.Type)
		context.WriteString(" ")
		context.WriteString(arg.Name)
	}
	context.WriteString(")")

	body, ok := procDef.Body.(CodeBlock)
	// ensure code block for code generation
	if !ok {
		body.Items = []Expr{procDef.Body}
	}
	context.compileCodeBlock(body)

}

func compilePackageToC(pak PackageDef) string {
	for _, proc := range pak.ProcDefs {
		if proc.Name == "main" {
			builder := &CCodeGeneratorContext{pak: pak}
			builder.compileProcDef(proc)
			return builder.String()
		}
	}
	panic("no main entry point found")
}
