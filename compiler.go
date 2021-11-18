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

	// functions marked for code generation
	TodoList []*TcProcDef
}

func (builder *CodeBuilder) newlineAndIndent() {
	builder.WriteString("\n")
	for i := 0; i < builder.Indentation; i++ {
		builder.WriteString("  ")
	}
}

func (builder *CodeBuilder) compileTypeExpr(typ Type) {
	switch typ := typ.(type) {
	case *BuiltinType:
		builder.WriteString(typ.name)
	case ArrayType:
		// TODO this is wrong.
		builder.compileTypeExpr(typ.Elem)
		builder.WriteByte('[')
		WriteIntLit(&builder.Builder, typ.Len)
		builder.WriteByte(']')
	default:
		panic("not implemented")
	}
}

func (builder *CodeBuilder) compileSymWithType(sym TcSymbol) {
	switch typ := sym.Type().(type) {
	case *BuiltinType:
		builder.WriteString(typ.name)
		builder.WriteString(" ")
		builder.WriteString(sym.Name)
	case ArrayType:
		// TODO this is wrong for nested arrays
		builder.compileTypeExpr(typ.Elem)
		builder.WriteString(" ")
		builder.WriteString(sym.Name)
		builder.WriteByte('[')
		WriteIntLit(&builder.Builder, typ.Len)
		builder.WriteByte(']')
	default:
		panic("not implemented")
	}
}

func (builder *CodeBuilder) compileCall(context *PackageGeneratorContext, call TcCall) {
	context.markForGeneration(call.Sym.Impl)

	if call.Sym.Impl.generateAsOperator {
		builder.WriteString("(")
		for i, it := range call.Args {
			if i != 0 {
				builder.WriteString(" ")
				builder.WriteString(call.Sym.Impl.builtinName)
				builder.WriteString(" ")
			}
			builder.compileExpr(context, it)
		}
		builder.WriteString(")")
	} else {
		builder.WriteString(call.Sym.Name)
		builder.WriteString("(")
		for i, it := range call.Args {
			if i != 0 {
				builder.WriteString(", ")
			}
			builder.compileExpr(context, it)
		}
		builder.WriteString(")")
	}
}

func (builder *CodeBuilder) compileCharLit(lit CharLit) {
	builder.WriteRune('\'')
	switch lit.Rune {
	case '\a':
		builder.WriteString("\\a")
	case '\b':
		builder.WriteString("\\b")
	case '\f':
		builder.WriteString("\\f")
	case '\n':
		builder.WriteString("\\n")
	case '\r':
		builder.WriteString("\\r")
	case '\t':
		builder.WriteString("\\t")
	case '\v':
		builder.WriteString("\\v")
	case '\\':
		builder.WriteString("\\\\")
	case '\'':
		builder.WriteString("\\'")
	case '"':
		builder.WriteString("\\\"")
	default:
		builder.WriteRune(lit.Rune)
	}

	//builder.WriteString(lit.Val)
	builder.WriteRune('\'')
}

func (builder *CodeBuilder) compileStrLit(lit StrLit) {
	builder.WriteRune('"')
	for _, rune := range lit.Value {
		switch rune {
		case '\a':
			builder.WriteString("\\a")
		case '\b':
			builder.WriteString("\\b")
		case '\f':
			builder.WriteString("\\f")
		case '\n':
			builder.WriteString("\\n")
		case '\r':
			builder.WriteString("\\r")
		case '\t':
			builder.WriteString("\\t")
		case '\v':
			builder.WriteString("\\v")
		case '\\':
			builder.WriteString("\\\\")
		case '\'':
			builder.WriteString("\\'")
		case '"':
			builder.WriteString("\\\"")
		default:
			builder.WriteRune(rune)
		}
	}
	//builder.WriteString(lit.Val)
	builder.WriteRune('"')
}

func (builder *CodeBuilder) compileIntLit(lit IntLit) {
	WriteIntLit(&builder.Builder, lit.Value)
}

func (builder *CodeBuilder) compileSymbol(sym TcSymbol) {
	fmt.Printf("compile symbol %#v\n", sym)
	switch sym.Name {
	case "true":
		builder.WriteString("1")
	case "false":
		builder.WriteString("0")
	default:
		if sym.Kind == SkLoopIterator {
			builder.WriteString("*")
		}
		builder.WriteString(sym.Name)
	}
}

func (builder *CodeBuilder) compileExpr(context *PackageGeneratorContext, expr TcExpr) {
	builder.compileExprWithPrefix(context, expr, false)
}

func (builder *CodeBuilder) compileVariableDefStmt(context *PackageGeneratorContext, stmt TcVariableDefStmt) {
	builder.compileSymWithType(stmt.Sym)
	builder.WriteString(" = ")
	builder.compileExpr(context, stmt.Value)
}

func (builder *CodeBuilder) compileIfStmt(context *PackageGeneratorContext, stmt TcIfStmt) {
	builder.WriteString("if (")
	builder.compileExpr(context, stmt.Condition)
	builder.WriteString(") ")
	builder.compileExpr(context, stmt.Body)
}

func wrapInCodeBlock(context *PackageGeneratorContext, expr TcExpr) TcCodeBlock {
	// I hope this function is temporary and can be removed at some point in the future
	if cb, ok := expr.(TcCodeBlock); ok {
		return cb
	}
	return TcCodeBlock{Items: []TcExpr{expr}}
}

func (builder *CodeBuilder) compileIfElseStmt(context *PackageGeneratorContext, stmt TcIfElseStmt, injectReturn bool) {
	builder.WriteString("if (")
	builder.compileExpr(context, stmt.Condition)
	builder.WriteString(") ")
	builder.compileCodeBlock(context, wrapInCodeBlock(context, stmt.Body), injectReturn)
	builder.WriteString(" else ")
	builder.compileCodeBlock(context, wrapInCodeBlock(context, stmt.Else), injectReturn)
}

func (builder *CodeBuilder) compileForLoopStmt(context *PackageGeneratorContext, stmt TcForLoopStmt) {
	// HACK: currently only iterating a cstring is possible, this code
	// is temporaray and written to work only for that (for now), type
	// arguments (e.g. for seq[int]) don't exist yet.
	/*
		for(const char *c = mystring; *c != '\0'; *c++) {
		  printf("char: %c\n", *c);
		}
	*/

	if stmt.Collection.Type() == TypeString {
		builder.WriteString("for(const char ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.compileExpr(context, stmt.Collection)
		builder.WriteString("; ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString(" != '\\0'; ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString("++) ")
	} else if arrayType, ok := stmt.Collection.Type().(ArrayType); ok {
		builder.WriteString("for(")
		builder.compileTypeExpr(arrayType.Elem)
		builder.WriteString(" const ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.compileExpr(context, stmt.Collection)
		builder.WriteString(", ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString("_END = ")
		builder.WriteString(stmt.LoopSym.Name)
		builder.WriteString(" + ")
		WriteIntLit(&builder.Builder, arrayType.Len)
		builder.WriteString("; ")
		builder.WriteString(stmt.LoopSym.Name)
		builder.WriteString(" != ")
		builder.WriteString(stmt.LoopSym.Name)
		builder.WriteString("_END; ++")
		builder.WriteString(stmt.LoopSym.Name)
		builder.WriteString(") ")
	} else {
		panic("not implemented")
	}
	builder.compileCodeBlock(context, wrapInCodeBlock(context, stmt.Body), false)
}

func (builder *CodeBuilder) compileArrayLit(context *PackageGeneratorContext, lit TcArrayLit) {
	builder.WriteString("{")
	for i, it := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.compileExpr(context, it)
	}
	builder.WriteString("}")

}

func (builder *CodeBuilder) injectReturn(injectReturn bool) {
	if injectReturn {
		builder.WriteString("return ")
	}
}

// lastExprPrefix is used to inject a return statement at each control
// flow end in procedures, as in C code
func (builder *CodeBuilder) compileExprWithPrefix(context *PackageGeneratorContext, expr TcExpr, injectReturn bool) {
	switch ex := expr.(type) {
	case TcCodeBlock:
		builder.compileCodeBlock(context, ex, injectReturn)
	case TcCall:
		builder.injectReturn(injectReturn)
		builder.compileCall(context, ex)
	case StrLit:
		builder.injectReturn(injectReturn)
		builder.compileStrLit(ex)
	case CharLit:
		builder.injectReturn(injectReturn)
		builder.compileCharLit(ex)
	case IntLit:
		builder.injectReturn(injectReturn)
		builder.compileIntLit(ex)
	case TcArrayLit:
		builder.injectReturn(injectReturn)
		builder.compileArrayLit(context, ex)
	case TcSymbol:
		builder.injectReturn(injectReturn)
		builder.compileSymbol(ex)
	case TcVariableDefStmt:
		if injectReturn {
			panic(fmt.Sprintf("internal error, injectReturn not supported here", injectReturn))
		}
		builder.compileVariableDefStmt(context, ex)
	case TcReturnStmt:
		// ignore the value of injectReturn here
		builder.WriteString("return ")
		builder.compileExpr(context, ex.Value)
	case TcIfStmt:
		if injectReturn {
			panic(fmt.Sprintf("internal error, injectReturn not supported here", injectReturn))
		}
		builder.compileIfStmt(context, ex)
	case TcIfElseStmt:
		builder.compileIfElseStmt(context, ex, injectReturn)
	case TcForLoopStmt:
		if injectReturn {
			panic(fmt.Sprintf("internal error, injectReturn not supported here", injectReturn))
		}
		builder.compileForLoopStmt(context, ex)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T", expr))
	}
}

func (builder *CodeBuilder) compileCodeBlock(context *PackageGeneratorContext, block TcCodeBlock, injectReturn bool) {
	N := len(block.Items)
	isExpr := !injectReturn && block.Type() != TypeVoid
	if isExpr {
		builder.WriteString("(")
	}
	builder.WriteString("{")
	builder.Indentation += 1
	for i, expr := range block.Items {
		builder.newlineAndIndent()
		if i == N-1 {
			fmt.Println("debug context: ", AstFormat(expr))
			builder.compileExprWithPrefix(context, expr, injectReturn)
		} else {
			builder.compileExpr(context, expr)
		}
		builder.WriteRune(';')
	}
	builder.Indentation -= 1
	builder.newlineAndIndent()
	builder.WriteString("}")
	if isExpr {
		builder.WriteString(")")
	}
}

func (builder *CodeBuilder) compileStructDef(context *PackageGeneratorContext, structDef TcStructDef) {
	builder.newlineAndIndent()
	builder.WriteString("typedef struct ")
	builder.WriteString(structDef.Name)
	builder.WriteString(" {")
	builder.Indentation += 1
	for _, field := range structDef.Fields {
		builder.newlineAndIndent()
		builder.compileTypeExpr(field.Type)
		builder.WriteString(" ")
		builder.WriteString(field.Name)
		builder.WriteString(";")
	}
	builder.Indentation -= 1
	builder.newlineAndIndent()
	builder.WriteString("} ")
	builder.WriteString(structDef.Name)
	builder.WriteString(";")
}

func compileProcDef(context *PackageGeneratorContext, procDef *TcProcDef) {
	// reuse string here

	headBuilder := &CodeBuilder{}
	headBuilder.compileTypeExpr(procDef.ResultType)
	// headBuilder.compileTypeExpr(procDef.ResultType)
	headBuilder.WriteString(" ")
	headBuilder.WriteString(procDef.Name)
	headBuilder.WriteString("(")
	for i, arg := range procDef.Args {
		if i != 0 {
			headBuilder.WriteString(", ")
		}
		headBuilder.compileTypeExpr(arg.Typ)
		headBuilder.WriteString(" ")
		headBuilder.WriteString(arg.Name)
	}
	headBuilder.WriteString(")")

	builderStr := headBuilder.String()
	context.forwardDecl.WriteString(builderStr)
	context.forwardDecl.WriteString(";")

	context.functions.WriteString(builderStr)
	body, ok := procDef.Body.(TcCodeBlock)
	// ensure code block for code generation
	if !ok {
		body.Items = []TcExpr{procDef.Body}
	}
	// instruct to inject "return" at the end of each control flow
	context.functions.compileCodeBlock(context, body, procDef.ResultType != TypeVoid)
}

func (context *PackageGeneratorContext) markForGeneration(procDef *TcProcDef) {

	if procDef == nil {
		return // builtin procs don't have a procDef to point to
	}
	if procDef.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoList = append(context.TodoList, procDef)
	procDef.scheduledforgeneration = true
}

// might return nil when nothing to do
func (context *PackageGeneratorContext) popMarkedForGenerationProcDef() *TcProcDef {
	N := len(context.TodoList)
	if N > 0 {
		result := context.TodoList[N-1]
		context.TodoList = context.TodoList[:N-1]
		return result
	}
	return nil
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
	//
	context.markForGeneration(pak.Main)
	for procDef := context.popMarkedForGenerationProcDef(); procDef != nil; procDef = context.popMarkedForGenerationProcDef() {
		compileProcDef(context, procDef)
	}

	final := &strings.Builder{}
	final.WriteString(context.includes.String())
	final.WriteString(context.typeDecl.String())
	final.WriteString(context.forwardDecl.String())
	final.WriteString(context.functions.String())
	return final.String()
}
