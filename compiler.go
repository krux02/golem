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
	TodoListArray  []*ArrayType
	TodoListEnum   []*EnumType
	TodoListStruct []*StructType
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
		builder.WriteString(typ.InternalName)
	case *StructType:
		builder.WriteString(typ.Impl.Name)
	case *EnumType:
		builder.WriteString(typ.Impl.Name)
	case *ArrayType:
		typ.ManglePrint(&builder.Builder)
	case *EnumSetType:
		builder.WriteString("uint64_t")
	default:
		panic("not implemented")
	}
}

func (builder *CodeBuilder) compileSymDeclaration(context *PackageGeneratorContext, sym TcSymbol) {
	switch typ := sym.GetType().(type) {
	case *BuiltinType:
		builder.WriteString(typ.InternalName)
		builder.WriteString(" ")
		builder.WriteString(sym.Source)
	case *ArrayType:
		context.markArrayTypeForGeneration(typ)
		typ.ManglePrint(&builder.Builder)
		builder.WriteString(" ")
		builder.WriteString(sym.Source)
	case *StructType:
		context.markStructTypeForGeneration(typ)
		// TODO this should be the mangled name
		builder.WriteString(typ.Impl.Name)
		builder.WriteString(" ")
		builder.WriteString(sym.Source)
	case *EnumType:
		context.markEnumTypeForGeneration(typ)
		// TODO this should be the mangled name
		builder.WriteString(typ.Impl.Name)
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
	builder.CompileSeparatedExprList(context, call.Args, impl.Infix)
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

func (builder *CodeBuilder) compileStrLit(value string, boxed bool) {
	if boxed {
		builder.WriteString(`(string){.len=`)
		WriteIntLit(&builder.Builder, int64(len(value)))
		builder.WriteString(`, .data="`)
	} else {
		builder.WriteString(`"`)
	}
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
	if boxed {
		builder.WriteString(`"}`)
	} else {
		builder.WriteString(`"`)
	}
}

func (builder *CodeBuilder) CompileIntLit(lit *IntLit) {
	WriteIntLit(&builder.Builder, lit.Value)
}

func (builder *CodeBuilder) CompileFloatLit(lit FloatLit) {
	str := fmt.Sprintf("%f", lit.Value)
	builder.Builder.WriteString(str)
}

func (builder *CodeBuilder) CompileSymbol(sym TcSymbol) {
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
			typ := sym.Type.(*EnumType)
			builder.WriteString(typ.Impl.Name)
			builder.WriteRune('_')
		}
		builder.WriteString(sym.Source)
	}
}

func (builder *CodeBuilder) CompileExpr(context *PackageGeneratorContext, expr TcExpr) {
	builder.CompileExprWithPrefix(context, expr)
}

func (builder *CodeBuilder) CompileVariableDefStmt(context *PackageGeneratorContext, stmt TcVariableDefStmt) {
	builder.compileSymDeclaration(context, stmt.Sym)
	builder.WriteString(" = ")
	builder.CompileExpr(context, stmt.Value)
}

func (builder *CodeBuilder) CompileIfStmt(context *PackageGeneratorContext, stmt TcIfStmt) {
	builder.WriteString("if (")
	builder.CompileExpr(context, stmt.Condition)
	builder.WriteString(") ")
	builder.CompileExpr(context, stmt.Body)
}

func ExprHasValue(expr TcExpr) bool {
	typ := expr.GetType()
	return typ != TypeNoReturn && typ != TypeVoid
}

func (builder *CodeBuilder) CompileIfElseExpr(context *PackageGeneratorContext, stmt TcIfElseExpr) {
	if ExprHasValue(stmt) {
		builder.WriteString("(")
		builder.CompileExpr(context, stmt.Condition)
		builder.WriteString(" ? ")
		builder.CompileExpr(context, stmt.Body)
		builder.WriteString(" : ")
		builder.CompileExpr(context, stmt.Else)
		builder.WriteString(")")
	} else {
		builder.WriteString("if (")
		builder.CompileExpr(context, stmt.Condition)
		builder.WriteString(") ")
		builder.CompileExpr(context, stmt.Body)
		builder.WriteString(" else ")
		builder.CompileExpr(context, stmt.Else)
	}
}

func (builder *CodeBuilder) CompileDotExpr(context *PackageGeneratorContext, dotExpr TcDotExpr) {
	builder.CompileExpr(context, dotExpr.Lhs)
	builder.WriteString(".")
	builder.WriteString(dotExpr.Rhs.Name)
}

func (builder *CodeBuilder) CompileForLoopStmt(context *PackageGeneratorContext, stmt TcForLoopStmt) {
	if stmt.Collection.GetType() == TypeString {
		// for(char const *it = collection.data, *it_END = collection.data + collection.len; it != it_END; ++it)
		builder.WriteString("for(char const")
		builder.CompileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.CompileExpr(context, stmt.Collection)
		builder.WriteString(".data, ")
		builder.CompileSymbol(stmt.LoopSym)
		builder.WriteString("_END = ")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString(" + ")
		builder.CompileExpr(context, stmt.Collection)
		builder.WriteString(".len; ")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString(" != ")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString("_END; ++")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString(") ")
	} else if arrayType, ok := stmt.Collection.GetType().(*ArrayType); ok {
		builder.WriteString("for(")
		builder.compileTypeExpr(arrayType.Elem)
		builder.WriteString(" const ")
		builder.CompileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.CompileExpr(context, stmt.Collection)
		builder.WriteString(".arr, ")
		builder.CompileSymbol(stmt.LoopSym)
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
	builder.CompileExpr(context, stmt.Body)
}

func (builder *CodeBuilder) CompileSeparatedExprList(context *PackageGeneratorContext, list []TcExpr, separator string) {
	for i, it := range list {
		if i != 0 {
			builder.WriteString(separator)
		}
		builder.CompileExpr(context, it)
	}
}

func (builder *CodeBuilder) CompileArrayLit(context *PackageGeneratorContext, lit TcArrayLit) {
	builder.WriteString("{")
	builder.CompileSeparatedExprList(context, lit.Items, ", ")
	builder.WriteString("}")

}

func (builder *CodeBuilder) CompileStructLit(context *PackageGeneratorContext, lit TcStructLit) {
	builder.WriteString("(")
	builder.compileTypeExpr(lit.Type)
	builder.WriteString("){")
	builder.CompileSeparatedExprList(context, lit.Items, ", ")
	builder.WriteString("}")
}

func (builder *CodeBuilder) CompileEnumSetLit(context *PackageGeneratorContext, lit TcEnumSetLit) {
	builder.WriteString("((1 << ")
	builder.CompileSeparatedExprList(context, lit.Items, ") | (1 << ")
	builder.WriteString("))")
}

// lastExprPrefix is not used anymore, rename to not use prefix anymore
// flow end in procedures, as in C code
func (builder *CodeBuilder) CompileExprWithPrefix(context *PackageGeneratorContext, expr TcExpr) {
	switch ex := expr.(type) {
	case TcCodeBlock:
		builder.CompileCodeBlock(context, ex)
	case TcCall:
		builder.compileCall(context, ex)
	case TcDotExpr:
		builder.CompileDotExpr(context, ex)
	case StrLit:
		builder.compileStrLit(ex.Value, true)
	case CStrLit:
		builder.compileStrLit(ex.Value, false)
	case CharLit:
		builder.compileCharLit(ex)
	case *IntLit:
		builder.CompileIntLit(ex)
	case FloatLit:
		builder.CompileFloatLit(ex)
	case TcArrayLit:
		builder.CompileArrayLit(context, ex)
	case TcSymbol:
		builder.CompileSymbol(ex)
	case TcVariableDefStmt:
		builder.CompileVariableDefStmt(context, ex)
	case TcReturnStmt:
		// ignore the value of injectReturn here
		builder.WriteString("return ")
		builder.CompileExpr(context, ex.Value)
	case TcIfStmt:
		builder.CompileIfStmt(context, ex)
	case TcIfElseExpr:
		builder.CompileIfElseExpr(context, ex)
	case TcForLoopStmt:
		builder.CompileForLoopStmt(context, ex)
	case TcStructLit:
		builder.CompileStructLit(context, ex)
	case TcEnumSetLit:
		builder.CompileEnumSetLit(context, ex)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T", expr))
	}
}

func (builder *CodeBuilder) CompileCodeBlock(context *PackageGeneratorContext, block TcCodeBlock) {
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
			builder.CompileExprWithPrefix(context, expr)
		} else {
			builder.CompileExpr(context, expr)
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

func compileArrayDef(context *PackageGeneratorContext, arrayType *ArrayType) {
	builder := &context.typeDecl
	builder.NewlineAndIndent()
	builder.WriteString("typedef struct {")
	builder.compileTypeExpr(arrayType.Elem)
	fmt.Fprintf(builder, " arr[%d];} ", arrayType.Len)
	arrayType.ManglePrint(&builder.Builder)
	builder.WriteString(";")
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
		builder.CompileSymbol(sym)
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
		builder.compileStrLit(sym.Source, true)
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
	if len(procDef.Signature.GenericParams) != 0 {
		panic("generic instances cannot be used here")
	}
	// reuse string here
	headBuilder := &CodeBuilder{}
	headBuilder.NewlineAndIndent()
	headBuilder.compileTypeExpr(procDef.Signature.ResultType)
	// headBuilder.compileTypeExpr(procDef.ResultType)
	headBuilder.WriteString(" ")
	headBuilder.WriteString(procDef.Prefix)
	for i, arg := range procDef.Signature.Params {
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

	context.functions.CompileCodeBlock(context, body)
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

func (context *PackageGeneratorContext) markArrayTypeForGeneration(typeDef *ArrayType) {
	if typeDef.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoListArray = append(context.TodoListArray, typeDef)
	typeDef.scheduledforgeneration = true
}

func (context *PackageGeneratorContext) markStructTypeForGeneration(typ *StructType) {
	if typ.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoListStruct = append(context.TodoListStruct, typ)
	typ.scheduledforgeneration = true
}

func (context *PackageGeneratorContext) markEnumTypeForGeneration(typ *EnumType) {
	if typ.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoListEnum = append(context.TodoListEnum, typ)
	typ.scheduledforgeneration = true
}

// might return nil when nothing to do
func (context *PackageGeneratorContext) popMarkedForGenerationProcDef() (result *TcProcDef) {
	N := len(context.TodoListProc)
	if N > 0 {
		result = context.TodoListProc[N-1]
		context.TodoListProc = context.TodoListProc[:N-1]
	}
	return result
}

func (context *PackageGeneratorContext) popMarkedForGenerationEnumType() (result *EnumType) {
	N := len(context.TodoListEnum)
	if N > 0 {
		result = context.TodoListEnum[N-1]
		context.TodoListEnum = context.TodoListEnum[:N-1]
	}
	return result
}

func (context *PackageGeneratorContext) popMarkedForGenerationStructDef() (result *StructType) {
	N := len(context.TodoListStruct)
	if N > 0 {
		result = context.TodoListStruct[N-1]
		context.TodoListStruct = context.TodoListStruct[:N-1]
	}
	return result
}

func (context *PackageGeneratorContext) popMarkedForGenerationArrayDef() (result *ArrayType) {
	N := len(context.TodoListArray)
	if N > 0 {
		result = context.TodoListArray[N-1]
		context.TodoListArray = context.TodoListArray[:N-1]
	}
	return result
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
	context.typeDecl.WriteString("typedef struct string {size_t len; char const* data;} string;")
	context.typeDecl.NewlineAndIndent()
	context.typeDecl.WriteString("typedef unsigned char bool;")
	context.markProcForGeneration(pak.Main)
	for procDef := context.popMarkedForGenerationProcDef(); procDef != nil; procDef = context.popMarkedForGenerationProcDef() {
		compileProcDef(context, procDef)
	}
	for typ := context.popMarkedForGenerationEnumType(); typ != nil; typ = context.popMarkedForGenerationEnumType() {
		compileEnumDef(context, typ.Impl)
	}
	for typ := context.popMarkedForGenerationStructDef(); typ != nil; typ = context.popMarkedForGenerationStructDef() {
		compileStructDef(context, typ.Impl)
	}

	// TODO, this invertion of order is total bullshit. It's just made because it makes one example compile better
	for i, N := 0, len(context.TodoListArray); i < (N / 2); i++ {
		j := N - 1 - i
		context.TodoListArray[i], context.TodoListArray[j] = context.TodoListArray[j], context.TodoListArray[i]
	}

	for typeDef := context.popMarkedForGenerationArrayDef(); typeDef != nil; typeDef = context.popMarkedForGenerationArrayDef() {
		compileArrayDef(context, typeDef)
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
