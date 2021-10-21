package main

import (
	"fmt"
	"strings"
)

type CCodeGeneratorContext struct {
	strings.Builder
	Indentation int
	Pak         TcPackageDef
}

func (context *CCodeGeneratorContext) newlineAndIndent() {
	context.WriteString("\n")
	for i := 0; i < context.Indentation; i++ {
		context.WriteString("  ")
	}
}

func (context *CCodeGeneratorContext) compileTypeExpr(typ Type) {
	context.WriteString(typ.Name())
}

func (context *CCodeGeneratorContext) compileCall(call TcCall) {
	switch call.Sym.Name {
	case "+", "-", "*", "/", "<", ">", "==", "=", "-=", "+=":
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

func (context *CCodeGeneratorContext) compileCharLit(lit CharLit) {
	context.WriteRune('\'')
	switch lit.Rune {
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
		context.WriteRune(lit.Rune)
	}

	//context.WriteString(lit.Val)
	context.WriteRune('\'')
}

func (context *CCodeGeneratorContext) compileStrLit(lit StrLit) {
	context.WriteRune('"')
	for _, rune := range lit.Value {
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

func (context *CCodeGeneratorContext) compileIntLit(lit IntLit) {
	WriteIntLit(&context.Builder, lit.Value)
}

func (context *CCodeGeneratorContext) compileSymbol(sym TcSymbol) {
	if sym.Kind == SkLoopIterator {
		context.WriteString("*")
	}
	context.WriteString(sym.Name)
}

func (context *CCodeGeneratorContext) compileExpr(expr TcExpr) {
	context.compileExprWithPrefix(expr, "")
}

func (context *CCodeGeneratorContext) compileVariableDefStmt(stmt TcVariableDefStmt) {
	context.compileTypeExpr(stmt.Sym.Typ)
	context.WriteByte(' ')
	context.compileSymbol(stmt.Sym)
	context.WriteString(" = ")
	context.compileExpr(stmt.Value)
}

func (context *CCodeGeneratorContext) compileIfStmt(stmt TcIfStmt) {
	context.WriteString("if (")
	context.compileExpr(stmt.Condition)
	context.WriteString(") ")
	context.compileCodeBlock(stmt.Body)
}

func (context *CCodeGeneratorContext) compileForLoopStmt(stmt TcForLoopStmt) {
	// currently only iterating a cstring is possible, this code is
	// temporaray and written to work only for that (for now)

	/*
		for(const char *c = mystring; *c != '\0'; *c++) {
		  printf("char: %c\n", *c);
		}
	*/

	context.WriteString("for(const char ")
	context.compileSymbol(stmt.LoopSym)
	context.WriteString(" = ")
	// TODO this is still wrong, it doesn't work for CodeBlock expressions here
	context.compileExpr(stmt.Collection)
	context.WriteString("; ")
	context.compileSymbol(stmt.LoopSym)
	context.WriteString(" != '\\0'; ")
	context.compileSymbol(stmt.LoopSym)
	context.WriteString("++) ")
	context.compileCodeBlock(stmt.Body)
}

// lastExprPrefix is used to inject a return statement at each control
// flow end in procedures, as in C code
func (context *CCodeGeneratorContext) compileExprWithPrefix(expr TcExpr, lastExprPrefix string) {
	switch ex := expr.(type) {
	case TcCodeBlock:
		context.compileCodeBlockWithPrefix(ex, lastExprPrefix)
	case TcCall:
		context.WriteString(lastExprPrefix)
		context.compileCall(ex)
	case StrLit:
		context.WriteString(lastExprPrefix)
		context.compileStrLit(ex)
	case CharLit:
		context.WriteString(lastExprPrefix)
		context.compileCharLit(ex)
	case IntLit:
		context.WriteString(lastExprPrefix)
		context.compileIntLit(ex)
	case TcSymbol:
		context.WriteString(lastExprPrefix)
		context.compileSymbol(ex)
	case TcVariableDefStmt:
		if lastExprPrefix != "" {
			panic(fmt.Sprintf("internal error, lastExprPrefix not supported here", lastExprPrefix))
		}
		context.compileVariableDefStmt(ex)
	case TcReturnStmt:
		if lastExprPrefix == "" || lastExprPrefix == "return " {
			context.WriteString("return ")
			context.compileExpr(ex.Value)
			return
		}
		panic(fmt.Sprintf("internal error, cannot inject '%s' in return statement", lastExprPrefix))
	case TcIfStmt:
		if lastExprPrefix != "" {
			panic(fmt.Sprintf("internal error, cannot inject '%s' in return statement", lastExprPrefix))
		}
		// TODO this should be forwarded
		context.compileIfStmt(ex)
	case TcForLoopStmt:
		if lastExprPrefix != "" {
			panic(fmt.Sprintf("internal error, cannot inject '%s' in return statement", lastExprPrefix))
		}
		context.compileForLoopStmt(ex)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T", expr))
	}
}

func (context *CCodeGeneratorContext) compileCodeBlock(block TcCodeBlock) {
	context.compileCodeBlockWithPrefix(block, "")
}

func (context *CCodeGeneratorContext) compileCodeBlockWithPrefix(block TcCodeBlock, lastExprPrefix string) {
	context.WriteString("{")
	context.Indentation += 1

	N := len(block.Items)
	for i, expr := range block.Items {
		context.newlineAndIndent()
		if i == N-1 {
			context.compileExprWithPrefix(expr, lastExprPrefix)
		} else {
			context.compileExpr(expr)
		}
		context.WriteRune(';')
	}

	context.Indentation -= 1
	context.newlineAndIndent()
	context.WriteString("}")
}

func (context *CCodeGeneratorContext) compileProcDef(procDef TcProcDef) {
	context.newlineAndIndent()
	context.compileTypeExpr(procDef.ResultType)
	// context.compileTypeExpr(procDef.ResultType)
	context.WriteString(" ")
	context.WriteString(procDef.Name)
	context.WriteString("(")
	for i, arg := range procDef.Args {
		if i != 0 {
			context.WriteString(", ")
		}
		context.compileTypeExpr(arg.Typ)
		context.WriteString(" ")
		context.WriteString(arg.Name)
	}
	context.WriteString(")")

	body, ok := procDef.Body.(TcCodeBlock)
	// ensure code block for code generation
	if !ok {
		body.Items = []TcExpr{procDef.Body}
	}
	context.compileCodeBlockWithPrefix(body, "return ")
}

func (context *CCodeGeneratorContext) compileStructDef(structDef TcStructDef) {
	context.newlineAndIndent()
	context.WriteString("typedef struct ")
	context.WriteString(structDef.Name)
	context.WriteString(" {")
	context.Indentation += 1
	for _, field := range structDef.Fields {
		context.newlineAndIndent()
		context.compileTypeExpr(field.Type)
		context.WriteString(" ")
		context.WriteString(field.Name)
		context.WriteString(";")
	}
	context.Indentation -= 1
	context.newlineAndIndent()
	context.WriteString("} ")
	context.WriteString(structDef.Name)
	context.WriteString(";")
}

func compilePackageToC(pak TcPackageDef) string {
	builder := &CCodeGeneratorContext{Pak: pak}
	// TODO this sholud depend on the usage of `printf`
	builder.WriteString("\n#include <stdio.h>")
	// TODO this sholud depend on the usage of `string` as a type
	builder.newlineAndIndent()
	builder.WriteString("typedef char* string;")
	for _, typ := range pak.TypeDefs {
		builder.compileStructDef(typ)
	}
	for _, proc := range pak.ProcDefs {
		builder.compileProcDef(proc)
	}
	return builder.String()
}
