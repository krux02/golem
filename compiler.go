package main

import (
	"fmt"
	"strings"
)

type CCodeGeneratorContext struct {
	strings.Builder
	indentation int
	pak         TcPackageDef
}

func (context *CCodeGeneratorContext) newlineAndIndent() {
	context.WriteString("\n")
	for i := 0; i < context.indentation; i++ {
		context.WriteString("  ")
	}
}

func (context *CCodeGeneratorContext) compileTypeExpr(typ TypeHandle) {
	context.WriteString(typ.Name())
}

func (context *CCodeGeneratorContext) compileCall(call TcCall) {
	switch call.Sym.Name {
	case "+", "-", "*", "/":
		context.WriteString("(")
		for i, it := range call.Args {
			if i != 0 {
				context.WriteString(" ")
				context.WriteString(call.Sym.Name)
				context.WriteString(" ")
			}
			context.compileExpr(it)
		}
		context.WriteString(")")
	default:
		context.WriteString(call.Sym.Name)
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

func (context *CCodeGeneratorContext) compileSymbol(sym TcSymbol) {
	context.WriteString(sym.Name)
}

func (context *CCodeGeneratorContext) compileExpr(expr TcExpr) {
	switch ex := expr.(type) {
	case TcCodeBlock:
		context.compileCodeBlock(ex)
	case TcCall:
		context.compileCall(ex)
	case StrLit:
		context.compileStrLit(ex)
	case TcSymbol:
		context.compileSymbol(ex)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T", expr))
	}
}

func (context *CCodeGeneratorContext) compileCodeBlock(block TcCodeBlock) {
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

func (context *CCodeGeneratorContext) compileProcDef(procDef TcProcDef) {
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

	body, ok := procDef.Body.(TcCodeBlock)
	// ensure code block for code generation
	if !ok {
		body.Items = []TcExpr{procDef.Body}
	}
	context.compileCodeBlock(body)

}

func compilePackageToC(pak TcPackageDef) string {
	builder := &CCodeGeneratorContext{pak: pak}
	for _, proc := range pak.ProcDefs {
		builder.compileProcDef(proc)
	}
	return builder.String()
}
