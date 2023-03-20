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
	TodoListProc []*TcProcDef
	// types marked for code generation
	TodoListEnum   []*TcEnumDef
	TodoListStruct []*TcStructDef
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
		builder.WriteString(typ.internalName)
	case *ArrayType:
		// TODO this is wrong.
		builder.compileTypeExpr(typ.Elem)
		builder.WriteByte('[')
		WriteIntLit(&builder.Builder, typ.Len)
		builder.WriteByte(']')
	case *TcStructDef:
		builder.WriteString(typ.Name)
	case *TcEnumDef:
		builder.WriteString(typ.Name)
	default:
		panic("not implemented")
	}
}

func (builder *CodeBuilder) compileSymWithType(context *PackageGeneratorContext, sym TcSymbol) {
	// TODO rename this, it is a declaration, not just a symbol that has a type
	switch typ := sym.Type().(type) {
	case *BuiltinType:
		builder.WriteString(typ.internalName)
		builder.WriteString(" ")
		builder.WriteString(sym.source)
	case *ArrayType:
		// TODO this is wrong for nested arrays
		builder.compileTypeExpr(typ.Elem)
		builder.WriteString(" ")
		builder.WriteString(sym.source)
		builder.WriteByte('[')
		WriteIntLit(&builder.Builder, typ.Len)
		builder.WriteByte(']')
	case *TcStructDef:
		context.markStructTypeForGeneration(typ)
		// TODO this should be the mangled name
		builder.WriteString(typ.Name)
		builder.WriteString(" ")
		builder.WriteString(sym.source)
	case *TcEnumDef:
		context.markEnumTypeForGeneration(typ)
		// TODO this should be the mangled name
		builder.WriteString(typ.Name)
		builder.WriteString(" ")
		builder.WriteString(sym.source)
	default:
		panic(fmt.Errorf("not implemented %T", typ))
	}
}

func (builder *CodeBuilder) compileCall(context *PackageGeneratorContext, call TcCall) {
	context.markProcForGeneration(call.Sym.Impl)

	if call.Sym.Impl.generateAsOperator {
		builder.WriteString("(")
		for i, it := range call.Args {
			if i != 0 {
				builder.WriteString(" ")
				builder.WriteString(call.Sym.Impl.MangledName)
				builder.WriteString(" ")
			}
			builder.compileExpr(context, it)
		}
		builder.WriteString(")")
	} else {
		builder.WriteString(call.Sym.Impl.MangledName)
		builder.WriteString("(")
		builder.compileCommaSeparatedExprList(context, call.Args)
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

func (builder *CodeBuilder) compileFloatLit(lit FloatLit) {
	str := fmt.Sprintf("%f", lit.Value)
	builder.Builder.WriteString(str)
}

func (builder *CodeBuilder) compileSymbol(sym TcSymbol) {
	switch sym.source {
	case "true":
		builder.WriteString("1")
	case "false":
		builder.WriteString("0")
	default:
		switch sym.Kind {
		case SkLoopIterator:
			builder.WriteString("*")
		case SkEnum:
			typ := sym.Typ.(*TcEnumDef)
			builder.WriteString(typ.Name)
			builder.WriteRune('_')
		}
		builder.WriteString(sym.source)
	}
}

func (builder *CodeBuilder) compileExpr(context *PackageGeneratorContext, expr TcExpr) {
	builder.compileExprWithPrefix(context, expr)
}

func (builder *CodeBuilder) compileVariableDefStmt(context *PackageGeneratorContext, stmt TcVariableDefStmt) {
	builder.compileSymWithType(context, stmt.Sym)
	builder.WriteString(" = ")
	builder.compileExpr(context, stmt.Value)
}

func (builder *CodeBuilder) compileIfStmt(context *PackageGeneratorContext, stmt TcIfStmt) {
	builder.WriteString("if (")
	builder.compileExpr(context, stmt.Condition)
	builder.WriteString(") ")
	builder.compileExpr(context, stmt.Body)
}

// func wrapInCodeBlock(expr TcExpr) (result TcCodeBlock) {
// 	// I hope this function is temporary and can be removed at some point in the future
// 	if cb, ok := expr.(TcCodeBlock); ok {
// 		return cb
// 	}
// 	result = TcCodeBlock{Items: []TcExpr{expr}}
// 	result.source = expr.Source()
// 	return result
// }

func ExprHasValue(expr TcExpr) bool {
	typ := expr.Type()
	return typ != TypeNoReturn && typ != TypeVoid
}

// TODO: rename, it is not a Stmt anymore
func (builder *CodeBuilder) compileIfElseStmt(context *PackageGeneratorContext, stmt TcIfElseStmt) {
	if ExprHasValue(stmt) {
		builder.WriteString("(")
		builder.compileExpr(context, stmt.Condition)
		builder.WriteString(" ? ")
		builder.compileExpr(context, stmt.Body)
		builder.WriteString(" : ")
		builder.compileExpr(context, stmt.Else)
		builder.WriteString(")")
	} else {
		builder.WriteString("if (")
		builder.compileExpr(context, stmt.Condition)
		builder.WriteString(") ")
		builder.compileExpr(context, stmt.Body)
		builder.WriteString(" else ")
		builder.compileExpr(context, stmt.Else)
	}
}

func (builder *CodeBuilder) compileDotExpr(context *PackageGeneratorContext, dotExpr TcDotExpr) {
	builder.compileExpr(context, dotExpr.Lhs)
	builder.WriteString(".")
	builder.WriteString(dotExpr.Rhs.Name)
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
	} else if arrayType, ok := stmt.Collection.Type().(*ArrayType); ok {
		builder.WriteString("for(")
		builder.compileTypeExpr(arrayType.Elem)
		builder.WriteString(" const ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.compileExpr(context, stmt.Collection)
		builder.WriteString(", ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString("_END = ")
		builder.WriteString(stmt.LoopSym.source)
		builder.WriteString(" + ")
		WriteIntLit(&builder.Builder, arrayType.Len)
		builder.WriteString("; ")
		builder.WriteString(stmt.LoopSym.source)
		builder.WriteString(" != ")
		builder.WriteString(stmt.LoopSym.source)
		builder.WriteString("_END; ++")
		builder.WriteString(stmt.LoopSym.source)
		builder.WriteString(") ")
	} else {
		panic("not implemented")
	}
	builder.compileExpr(context, stmt.Body)
}

func (builder *CodeBuilder) compileCommaSeparatedExprList(context *PackageGeneratorContext, list []TcExpr) {
	for i, it := range list {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.compileExpr(context, it)
	}
}

func (builder *CodeBuilder) compileArrayLit(context *PackageGeneratorContext, lit TcArrayLit) {
	builder.WriteString("{")
	builder.compileCommaSeparatedExprList(context, lit.Items)
	builder.WriteString("}")

}

func (builder *CodeBuilder) compileStructLit(context *PackageGeneratorContext, lit TcStructLit) {
	builder.WriteString("(")
	builder.compileTypeExpr(lit.typ)
	builder.WriteString("){")
	builder.compileCommaSeparatedExprList(context, lit.Items)
	builder.WriteString("}")
}

// lastExprPrefix is not used anymore, rename to not use prefix anymore
// flow end in procedures, as in C code
func (builder *CodeBuilder) compileExprWithPrefix(context *PackageGeneratorContext, expr TcExpr) {
	switch ex := expr.(type) {
	case TcCodeBlock:
		builder.compileCodeBlock(context, ex)
	case TcCall:
		builder.compileCall(context, ex)
	case TcDotExpr:
		builder.compileDotExpr(context, ex)
	case StrLit:
		builder.compileStrLit(ex)
	case CharLit:
		builder.compileCharLit(ex)
	case IntLit:
		builder.compileIntLit(ex)
	case FloatLit:
		builder.compileFloatLit(ex)
	case TcArrayLit:
		builder.compileArrayLit(context, ex)
	case TcSymbol:
		builder.compileSymbol(ex)
	case TcVariableDefStmt:
		builder.compileVariableDefStmt(context, ex)
	case TcReturnStmt:
		// ignore the value of injectReturn here
		builder.WriteString("return ")
		builder.compileExpr(context, ex.Value)
	case TcIfStmt:
		builder.compileIfStmt(context, ex)
	case TcIfElseStmt:
		builder.compileIfElseStmt(context, ex)
	case TcForLoopStmt:
		builder.compileForLoopStmt(context, ex)
	case TcStructLit:
		builder.compileStructLit(context, ex)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T", expr))
	}
}

func (builder *CodeBuilder) compileCodeBlock(context *PackageGeneratorContext, block TcCodeBlock) {
	N := len(block.Items)
	isExpr := ExprHasValue(block)
	if isExpr {
		builder.WriteString("(")
	}
	builder.WriteString("{")
	builder.Indentation += 1
	for i, expr := range block.Items {
		builder.newlineAndIndent()
		if i == N-1 {
			builder.compileExprWithPrefix(context, expr)
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

func compileEnumDef(context *PackageGeneratorContext, enumDef *TcEnumDef) {
	builder := &context.typeDecl
	builder.WriteString("typedef enum ")
	builder.WriteString(enumDef.Name)
	builder.WriteString(" {")
	builder.Indentation += 1
	for _, sym := range enumDef.Values {
		builder.newlineAndIndent()
		builder.compileSymbol(sym)
		builder.WriteString(",")
	}
	builder.Indentation -= 1
	builder.newlineAndIndent()
	builder.WriteString("} ")
	builder.WriteString(enumDef.Name)
	builder.WriteString(";\n")
}

func compileStructDef(context *PackageGeneratorContext, structDef *TcStructDef) {
	builder := &context.typeDecl
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
	builder.WriteString(";\n")
}

func compileProcDef(context *PackageGeneratorContext, procDef *TcProcDef) {
	// reuse string here
	headBuilder := &CodeBuilder{}
	headBuilder.compileTypeExpr(procDef.ResultType)
	// headBuilder.compileTypeExpr(procDef.ResultType)
	headBuilder.WriteString(" ")
	headBuilder.WriteString(procDef.MangledName)
	headBuilder.WriteString("(")
	for i, arg := range procDef.Args {
		if i != 0 {
			headBuilder.WriteString(", ")
		}
		headBuilder.compileSymWithType(context, arg)
	}
	headBuilder.WriteString(")")

	builderStr := headBuilder.String()
	context.forwardDecl.WriteString(builderStr)
	context.forwardDecl.WriteString(";\n")

	context.functions.newlineAndIndent()
	context.functions.WriteString(builderStr)

	// NOTE: Body.Type() != ResultType
	// return statements are of type `NoReturn`, but then ResultType may not be `NoReturn`.//

	injectReturn := ExprHasValue(procDef.Body)
	// code generation needs to have a code block here, otherwise it is not valid C
	var body TcCodeBlock
	body.source = procDef.Body.Source()

	if injectReturn {
		body.Items = append(body.Items, TcReturnStmt{Value: procDef.Body})
	} else {
		codeBlockBody, ok := procDef.Body.(TcCodeBlock)
		if ok {
			body = codeBlockBody
		} else {
			body.Items = append(body.Items, codeBlockBody)
		}
	}

	context.functions.compileCodeBlock(context, body)
}

func (context *PackageGeneratorContext) markProcForGeneration(procDef *TcProcDef) {
	if procDef.Body == nil {
		return // builtin procs don't have a procDef to point to
	}
	if procDef.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoListProc = append(context.TodoListProc, procDef)
	procDef.scheduledforgeneration = true
}

func (context *PackageGeneratorContext) markStructTypeForGeneration(typeDef *TcStructDef) {
	if typeDef.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoListStruct = append(context.TodoListStruct, typeDef)
	typeDef.scheduledforgeneration = true
}

func (context *PackageGeneratorContext) markEnumTypeForGeneration(typeDef *TcEnumDef) {
	if typeDef.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoListEnum = append(context.TodoListEnum, typeDef)
	typeDef.scheduledforgeneration = true
}

// might return nil when nothing to do
func (context *PackageGeneratorContext) popMarkedForGenerationProcDef() (result *TcProcDef) {
	N := len(context.TodoListProc)
	if N > 0 {
		result = context.TodoListProc[N-1]
		context.TodoListProc = context.TodoListProc[:N-1]
	}
	return
}

func (context *PackageGeneratorContext) popMarkedForGenerationEnumDef() (result *TcEnumDef) {
	N := len(context.TodoListEnum)
	if N > 0 {
		result = context.TodoListEnum[N-1]
		context.TodoListEnum = context.TodoListEnum[:N-1]
	}
	return
}

func (context *PackageGeneratorContext) popMarkedForGenerationStructDef() (result *TcStructDef) {
	N := len(context.TodoListStruct)
	if N > 0 {
		result = context.TodoListStruct[N-1]
		context.TodoListStruct = context.TodoListStruct[:N-1]
	}
	return
}

func compilePackageToC(pak TcPackageDef) string {
	context := &PackageGeneratorContext{Pak: pak}
	context.includes.WriteString("#include <stdint.h>\n")
	// TODO this should depend on the usage of `printf`
	context.includes.WriteString("#include <stdio.h>\n")
	// TODO this should depend on the usage of `assert`
	context.includes.WriteString("#include <assert.h>\n")
	// TODO this sholud depend on the usage of `string` as a type
	context.typeDecl.WriteString("typedef char* string;\n")
	context.typeDecl.WriteString("typedef unsigned char bool;\n")
	context.markProcForGeneration(pak.Main)
	for procDef := context.popMarkedForGenerationProcDef(); procDef != nil; procDef = context.popMarkedForGenerationProcDef() {
		compileProcDef(context, procDef)
	}
	for typeDef := context.popMarkedForGenerationEnumDef(); typeDef != nil; typeDef = context.popMarkedForGenerationEnumDef() {
		compileEnumDef(context, typeDef)
	}
	for typeDef := context.popMarkedForGenerationStructDef(); typeDef != nil; typeDef = context.popMarkedForGenerationStructDef() {
		compileStructDef(context, typeDef)
	}
	// TODO this creates a type declaration for every usage. Should be done only once

	final := &strings.Builder{}
	final.WriteString("/* includes */\n")
	final.WriteString(context.includes.String())
	final.WriteString("/* typeDecl */\n")
	final.WriteString(context.typeDecl.String())
	final.WriteString("/* forwardDecl */\n")
	final.WriteString(context.forwardDecl.String())
	final.WriteString("/* functions */\n")
	final.WriteString(context.functions.String())
	return final.String()
}
