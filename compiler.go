package main

import (
	"fmt"
	"strings"
)

type CodeBuilder struct {
	strings.Builder
	Indentation int
}

type PackageGeneratorContext struct {
	includes    CodeBuilder
	typeDecl    CodeBuilder
	forwardDecl CodeBuilder
	functions   CodeBuilder
	Pak         TcPackageDef
}

func (context *CodeBuilder) newlineAndIndent() {
	context.WriteString("\n")
	for i := 0; i < context.Indentation; i++ {
		context.WriteString("  ")
	}
}

func (context *CodeBuilder) compileTypeExpr(typ Type) {
	switch typ := typ.(type) {
	case *BuiltinType:
		context.WriteString(typ.name)
	case ArrayType:
		// TODO this is wrong.
		context.compileTypeExpr(typ.Elem)
		context.WriteByte('[')
		WriteIntLit(&context.Builder, typ.Len)
		context.WriteByte(']')
	default:
		panic("not implemented")
	}
}

func (context *CodeBuilder) compileSymWithType(sym TcSymbol) {
	switch typ := sym.Type().(type) {
	case *BuiltinType:
		context.WriteString(typ.name)
		context.WriteString(" ")
		context.WriteString(sym.Name)
	case ArrayType:
		// TODO this is wrong for nested arrays
		context.compileTypeExpr(typ.Elem)
		context.WriteString(" ")
		context.WriteString(sym.Name)
		context.WriteByte('[')
		WriteIntLit(&context.Builder, typ.Len)
		context.WriteByte(']')
	default:
		panic("not implemented")
	}
}

func (context *CodeBuilder) compileCall(call TcCall) {
	if call.Sym.Impl.generateAsOperator {
		context.WriteString("(")
		for i, it := range call.Args {
			if i != 0 {
				context.WriteString(" ")
				context.WriteString(call.Sym.Impl.builtinName)
				context.WriteString(" ")
			}
			context.compileExpr(it)
		}
		context.WriteString(")")
	} else {
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

func (context *CodeBuilder) compileCharLit(lit CharLit) {
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

func (context *CodeBuilder) compileStrLit(lit StrLit) {
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

func (context *CodeBuilder) compileIntLit(lit IntLit) {
	WriteIntLit(&context.Builder, lit.Value)
}

func (context *CodeBuilder) compileSymbol(sym TcSymbol) {
	fmt.Printf("compile symbol %#v\n", sym)
	switch sym.Name {
	case "true":
		context.WriteString("1")
	case "false":
		context.WriteString("0")
	default:
		if sym.Kind == SkLoopIterator {
			context.WriteString("*")
		}
		context.WriteString(sym.Name)
	}
}

func (context *CodeBuilder) compileExpr(expr TcExpr) {
	context.compileExprWithPrefix(expr, false)
}

func (context *CodeBuilder) compileVariableDefStmt(stmt TcVariableDefStmt) {
	context.compileSymWithType(stmt.Sym)
	context.WriteString(" = ")
	context.compileExpr(stmt.Value)
}

func (context *CodeBuilder) compileIfStmt(stmt TcIfStmt) {
	context.WriteString("if (")
	context.compileExpr(stmt.Condition)
	context.WriteString(") ")
	context.compileExpr(stmt.Body)
}

func wrapInCodeBlock(expr TcExpr) TcCodeBlock {
	// I hope this function is temporary and can be removed at some point in the future
	if cb, ok := expr.(TcCodeBlock); ok {
		return cb
	}
	return TcCodeBlock{Items: []TcExpr{expr}}
}

func (context *CodeBuilder) compileIfElseStmt(stmt TcIfElseStmt, injectReturn bool) {
	context.WriteString("if (")
	context.compileExpr(stmt.Condition)
	context.WriteString(") ")
	context.compileCodeBlock(wrapInCodeBlock(stmt.Body), injectReturn)
	context.WriteString(" else ")
	context.compileCodeBlock(wrapInCodeBlock(stmt.Else), injectReturn)
}

func (context *CodeBuilder) compileForLoopStmt(stmt TcForLoopStmt) {
	// HACK: currently only iterating a cstring is possible, this code
	// is temporaray and written to work only for that (for now), type
	// arguments (e.g. for seq[int]) don't exist yet.
	/*
		for(const char *c = mystring; *c != '\0'; *c++) {
		  printf("char: %c\n", *c);
		}
	*/

	if stmt.Collection.Type() == TypeString {
		context.WriteString("for(const char ")
		context.compileSymbol(stmt.LoopSym)
		context.WriteString(" = ")
		context.compileExpr(stmt.Collection)
		context.WriteString("; ")
		context.compileSymbol(stmt.LoopSym)
		context.WriteString(" != '\\0'; ")
		context.compileSymbol(stmt.LoopSym)
		context.WriteString("++) ")
	} else if arrayType, ok := stmt.Collection.Type().(ArrayType); ok {
		context.WriteString("for(")
		context.compileTypeExpr(arrayType.Elem)
		context.WriteString(" const ")
		context.compileSymbol(stmt.LoopSym)
		context.WriteString(" = ")
		context.compileExpr(stmt.Collection)
		context.WriteString(", ")
		context.compileSymbol(stmt.LoopSym)
		context.WriteString("_END = ")
		context.WriteString(stmt.LoopSym.Name)
		context.WriteString(" + ")
		WriteIntLit(&context.Builder, arrayType.Len)
		context.WriteString("; ")
		context.WriteString(stmt.LoopSym.Name)
		context.WriteString(" != ")
		context.WriteString(stmt.LoopSym.Name)
		context.WriteString("_END; ++")
		context.WriteString(stmt.LoopSym.Name)
		context.WriteString(") ")
	} else {
		panic("not implemented")
	}
	context.compileCodeBlock(wrapInCodeBlock(stmt.Body), false)
}

func (context *CodeBuilder) compileArrayLit(lit TcArrayLit) {
	context.WriteString("{")
	for i, it := range lit.Items {
		if i != 0 {
			context.WriteString(", ")
		}
		context.compileExpr(it)
	}
	context.WriteString("}")

}

func (context *CodeBuilder) injectReturn(injectReturn bool) {
	if injectReturn {
		context.WriteString("return ")
	}
}

// lastExprPrefix is used to inject a return statement at each control
// flow end in procedures, as in C code
func (context *CodeBuilder) compileExprWithPrefix(expr TcExpr, injectReturn bool) {
	switch ex := expr.(type) {
	case TcCodeBlock:
		context.compileCodeBlock(ex, injectReturn)
	case TcCall:
		context.injectReturn(injectReturn)
		context.compileCall(ex)
	case StrLit:
		context.injectReturn(injectReturn)
		context.compileStrLit(ex)
	case CharLit:
		context.injectReturn(injectReturn)
		context.compileCharLit(ex)
	case IntLit:
		context.injectReturn(injectReturn)
		context.compileIntLit(ex)
	case TcArrayLit:
		context.injectReturn(injectReturn)
		context.compileArrayLit(ex)
	case TcSymbol:
		context.injectReturn(injectReturn)
		context.compileSymbol(ex)
	case TcVariableDefStmt:
		if injectReturn {
			panic(fmt.Sprintf("internal error, injectReturn not supported here", injectReturn))
		}
		context.compileVariableDefStmt(ex)
	case TcReturnStmt:
		// ignore the value of injectReturn here
		context.WriteString("return ")
		context.compileExpr(ex.Value)
	case TcIfStmt:
		if injectReturn {
			panic(fmt.Sprintf("internal error, injectReturn not supported here", injectReturn))
		}
		context.compileIfStmt(ex)
	case TcIfElseStmt:
		context.compileIfElseStmt(ex, injectReturn)
	case TcForLoopStmt:
		if injectReturn {
			panic(fmt.Sprintf("internal error, injectReturn not supported here", injectReturn))
		}
		context.compileForLoopStmt(ex)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T", expr))
	}
}

func (context *CodeBuilder) compileCodeBlock(block TcCodeBlock, injectReturn bool) {
	N := len(block.Items)
	isExpr := !injectReturn && block.Type() != TypeVoid
	if isExpr {
		context.WriteString("(")
	}
	context.WriteString("{")
	context.Indentation += 1
	for i, expr := range block.Items {
		context.newlineAndIndent()
		if i == N-1 {
			context.compileExprWithPrefix(expr, injectReturn)
		} else {
			context.compileExpr(expr)
		}
		context.WriteRune(';')
	}
	context.Indentation -= 1
	context.newlineAndIndent()
	context.WriteString("}")
	if isExpr {
		context.WriteString(")")
	}
}

func (context *CodeBuilder) compileProcDef(procDef TcProcDef, justforward bool) {
	fmt.Printf("%#v\n", procDef)
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

	if justforward {
		context.WriteByte(';')
	} else {
		body, ok := procDef.Body.(TcCodeBlock)
		// ensure code block for code generation
		if !ok {
			body.Items = []TcExpr{procDef.Body}
		}
		// instruct to inject "return" at the end of each control flow
		context.compileCodeBlock(body, procDef.ResultType != TypeVoid)
	}
}

func (context *CodeBuilder) compileStructDef(structDef TcStructDef) {
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

func (context *PackageGeneratorContext) compileProcDef(procDef TcProcDef) {
	context.forwardDecl.compileProcDef(procDef, true)
	context.functions.compileProcDef(procDef, false)
}

func compilePackageToC(pak TcPackageDef) string {
	context := &PackageGeneratorContext{Pak: pak}
	// TODO this sholud depend on the usage of `printf`
	context.includes.WriteString("#include <stdio.h>\n")
	// TODO this sholud depend on the usage of `string` as a type
	context.typeDecl.WriteString("typedef char* string;\n")
	context.typeDecl.WriteString("typedef unsigned char bool;\n")
	//for _, typ := range pak.TypeDefs {
	//context.typeDecl.compileStructDef(typ)
	//}
	//for _, proc := range pak.ProcDefs {
	//context.typeDecl.compileProcDef(proc)
	//}
	context.compileProcDef(pak.Main)

	final := &strings.Builder{}
	final.WriteString(context.includes.String())
	final.WriteString(context.typeDecl.String())
	final.WriteString(context.forwardDecl.String())
	final.WriteString(context.functions.String())
	return final.String()
}
