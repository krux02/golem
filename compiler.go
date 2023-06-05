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

func (builder *CodeBuilder) NewlineAndIndent() {
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
	case *EnumSetType:
		builder.WriteString("uint64_t")
	default:
		panic("not implemented")
	}
}

func (builder *CodeBuilder) compileSymDeclaration(context *PackageGeneratorContext, sym TcSymbol) {
	switch typ := sym.GetType().(type) {
	case *BuiltinType:
		builder.WriteString(typ.internalName)
		builder.WriteString(" ")
		builder.WriteString(sym.Source)
	case *ArrayType:
		// TODO this is wrong for nested arrays
		builder.compileTypeExpr(typ.Elem)
		builder.WriteString(" ")
		builder.WriteString(sym.Source)
		builder.WriteByte('[')
		WriteIntLit(&builder.Builder, typ.Len)
		builder.WriteByte(']')
	case *TcStructDef:
		context.markStructTypeForGeneration(typ)
		// TODO this should be the mangled name
		builder.WriteString(typ.Name)
		builder.WriteString(" ")
		builder.WriteString(sym.Source)
	case *TcEnumDef:
		context.markEnumTypeForGeneration(typ)
		// TODO this should be the mangled name
		builder.WriteString(typ.Name)
		builder.WriteString(" ")
		builder.WriteString(sym.Source)
	case *EnumSetType:
		context.markEnumTypeForGeneration(typ.Elem)
		builder.WriteString("uint64_t ")
		builder.WriteString(sym.Source)
	default:
		panic(fmt.Errorf("not implemented %T", typ))
	}
}

func (builder *CodeBuilder) compileCall(context *PackageGeneratorContext, call TcCall) {
	impl := call.Sym.Impl
	context.markProcForGeneration(impl)
	builder.WriteString(impl.Prefix)
	builder.compileSeparatedExprList(context, call.Args, impl.Infix)
	builder.WriteString(impl.Postfix)
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

func (builder *CodeBuilder) compileStrLit(value string) {
	builder.WriteRune('"')
	for _, rune := range value {
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
	switch sym.Source {
	case "true":
		builder.WriteString("1")
	case "false":
		builder.WriteString("0")
	default:
		switch sym.Kind {
		case SkLoopIterator:
			builder.WriteString("*")
		case SkEnum:
			typ := sym.Type.(*TcEnumDef)
			builder.WriteString(typ.Name)
			builder.WriteRune('_')
		}
		builder.WriteString(sym.Source)
	}
}

func (builder *CodeBuilder) compileExpr(context *PackageGeneratorContext, expr TcExpr) {
	builder.compileExprWithPrefix(context, expr)
}

func (builder *CodeBuilder) compileVariableDefStmt(context *PackageGeneratorContext, stmt TcVariableDefStmt) {
	builder.compileSymDeclaration(context, stmt.Sym)
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
	typ := expr.GetType()
	return typ != TypeNoReturn && typ != TypeVoid
}

func (builder *CodeBuilder) compileIfElseExpr(context *PackageGeneratorContext, stmt TcIfElseExpr) {
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

	if stmt.Collection.GetType() == TypeString {
		builder.WriteString("for(const char ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.compileExpr(context, stmt.Collection)
		builder.WriteString("; ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString(" != '\\0'; ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString("++) ")
	} else if arrayType, ok := stmt.Collection.GetType().(*ArrayType); ok {
		builder.WriteString("for(")
		builder.compileTypeExpr(arrayType.Elem)
		builder.WriteString(" const ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.compileExpr(context, stmt.Collection)
		builder.WriteString(", ")
		builder.compileSymbol(stmt.LoopSym)
		builder.WriteString("_END = ")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString(" + ")
		WriteIntLit(&builder.Builder, arrayType.Len)
		builder.WriteString("; ")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString(" != ")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString("_END; ++")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString(") ")
	} else {
		panic("not implemented")
	}
	builder.compileExpr(context, stmt.Body)
}

func (builder *CodeBuilder) compileSeparatedExprList(context *PackageGeneratorContext, list []TcExpr, separator string) {
	for i, it := range list {
		if i != 0 {
			builder.WriteString(separator)
		}
		builder.compileExpr(context, it)
	}
}

func (builder *CodeBuilder) compileArrayLit(context *PackageGeneratorContext, lit TcArrayLit) {
	builder.WriteString("{")
	builder.compileSeparatedExprList(context, lit.Items, ", ")
	builder.WriteString("}")

}

func (builder *CodeBuilder) compileStructLit(context *PackageGeneratorContext, lit TcStructLit) {
	builder.WriteString("(")
	builder.compileTypeExpr(lit.Type)
	builder.WriteString("){")
	builder.compileSeparatedExprList(context, lit.Items, ", ")
	builder.WriteString("}")
}

func (builder *CodeBuilder) compileEnumSetLit(context *PackageGeneratorContext, lit TcEnumSetLit) {
	builder.WriteString("((1 << ")
	builder.compileSeparatedExprList(context, lit.Items, ") | (1 << ")
	builder.WriteString("))")
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
		builder.compileStrLit(ex.Value)
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
	case TcIfElseExpr:
		builder.compileIfElseExpr(context, ex)
	case TcForLoopStmt:
		builder.compileForLoopStmt(context, ex)
	case TcStructLit:
		builder.compileStructLit(context, ex)
	case TcEnumSetLit:
		builder.compileEnumSetLit(context, ex)
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
		builder.NewlineAndIndent()
		if i == N-1 {
			builder.compileExprWithPrefix(context, expr)
		} else {
			builder.compileExpr(context, expr)
		}
		builder.WriteRune(';')
	}
	builder.Indentation -= 1
	builder.NewlineAndIndent()
	builder.WriteString("}")
	if isExpr {
		builder.WriteString(")")
	}
}

func compileEnumDef(context *PackageGeneratorContext, enumDef *TcEnumDef) {
	builder := &context.typeDecl
	builder.NewlineAndIndent()
	builder.WriteString("typedef enum ")
	builder.WriteString(enumDef.Name)
	builder.WriteString(" {")
	builder.Indentation += 1
	for _, sym := range enumDef.Values {
		builder.NewlineAndIndent()
		builder.compileSymbol(sym)
		builder.WriteString(",")
	}
	builder.Indentation -= 1
	builder.NewlineAndIndent()
	builder.WriteString("} ")
	builder.WriteString(enumDef.Name)
	builder.WriteString(";")
	builder.NewlineAndIndent()
	builder.WriteString("const string ")
	builder.WriteString(enumDef.Name)
	builder.WriteString("_names_array[] = {")
	for i, sym := range enumDef.Values {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.compileStrLit(sym.Source)
	}
	builder.WriteString("};")
}

func compileStructDef(context *PackageGeneratorContext, structDef *TcStructDef) {
	builder := &context.typeDecl
	builder.NewlineAndIndent()
	builder.WriteString("typedef struct ")
	builder.WriteString(structDef.Name)
	builder.WriteString(" {")
	builder.Indentation += 1
	for _, field := range structDef.Fields {
		builder.NewlineAndIndent()
		builder.compileTypeExpr(field.Type)
		builder.WriteString(" ")
		builder.WriteString(field.Name)
		builder.WriteString(";")
	}
	builder.Indentation -= 1
	builder.NewlineAndIndent()
	builder.WriteString("} ")
	builder.WriteString(structDef.Name)
	builder.WriteString(";")
}

func compileProcDef(context *PackageGeneratorContext, procDef *TcProcDef) {
	if len(procDef.GenericParams) != 0 {
		panic("generic instances cannot be used here")
	}
	// reuse string here
	headBuilder := &CodeBuilder{}
	headBuilder.NewlineAndIndent()
	headBuilder.compileTypeExpr(procDef.ResultType)
	// headBuilder.compileTypeExpr(procDef.ResultType)
	headBuilder.WriteString(" ")
	headBuilder.WriteString(procDef.Prefix)
	for i, arg := range procDef.Params {
		if i != 0 {
			headBuilder.WriteString(procDef.Infix)
		}
		headBuilder.compileSymDeclaration(context, arg)
	}
	headBuilder.WriteString(procDef.Postfix)

	builderStr := headBuilder.String()
	context.forwardDecl.WriteString(builderStr)
	context.forwardDecl.WriteString(";")

	context.functions.NewlineAndIndent()
	context.functions.WriteString(builderStr)

	// NOTE: Body.Type() != ResultType
	// return statements are of type `NoReturn`, but then ResultType may not be `NoReturn`.//

	injectReturn := ExprHasValue(procDef.Body)
	// code generation needs to have a code block here, otherwise it is not valid C
	var body TcCodeBlock
	body.Source = procDef.Body.GetSource()

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
	context.includes.NewlineAndIndent()
	context.includes.WriteString("#include <stdint.h>")
	// TODO this should depend on the usage of `printf`
	context.includes.NewlineAndIndent()
	context.includes.WriteString("#include <stdio.h>")
	// TODO this should depend on the usage of `assert`
	context.includes.NewlineAndIndent()
	context.includes.WriteString("#include <assert.h>")
	// TODO this sholud depend on the usage of `string` as a type
	context.typeDecl.NewlineAndIndent()
	context.typeDecl.WriteString("typedef char* string;")
	context.typeDecl.NewlineAndIndent()
	context.typeDecl.WriteString("typedef unsigned char bool;")
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
	final.WriteString("\n/* includes */")
	final.WriteString(context.includes.String())
	final.WriteString("\n/* typeDecl */")
	final.WriteString(context.typeDecl.String())
	final.WriteString("\n/* forwardDecl */")
	final.WriteString(context.forwardDecl.String())
	final.WriteString("\n/* functions */")
	final.WriteString(context.functions.String())
	return final.String()
}
