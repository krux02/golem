package main

import (
	"fmt"
	"math"
	"math/big"
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
	Pak         *TcPackageDef

	// functions marked for code generation
	TodoListProc []*TcProcDef
	// types marked for code generation
	TodoListTypes []Type
}

func (builder *CodeBuilder) NewlineAndIndent() {
	builder.WriteString("\n")
	for i := 0; i < builder.Indentation; i++ {
		builder.WriteString("  ")
	}
}

func (builder *CodeBuilder) compileTypeExpr(context *PackageGeneratorContext, typ Type) {
	markTypeForGeneration(context, typ)
	switch typ := typ.(type) {
	case *BuiltinType:
		builder.WriteString(typ.InternalName)
	case *BuiltinIntType:
		builder.WriteString(typ.InternalName)
	case *BuiltinFloatType:
		builder.WriteString(typ.InternalName)
	case *BuiltinStringType:
		builder.WriteString(typ.InternalName)
	case *ArrayType:
		typ.ManglePrint(&builder.Builder)
	case *StructType:
		// TODO this should be the mangled name (or importc name)
		builder.WriteString(typ.Impl.Name)
	case *EnumType:
		// TODO this should be the mangled name (or importc name)
		builder.WriteString(typ.Impl.Name)
	case *EnumSetType:
		builder.WriteString("uint64_t")
	case *PtrType:
		builder.compileTypeExpr(context, typ.Target)
		builder.WriteRune('*')
	default:
		panic(fmt.Errorf("not implemented %T", typ))
	}
}

func (builder *CodeBuilder) compileCall(context *PackageGeneratorContext, call TcCall) {
	switch impl := call.Sym.Signature.Impl.(type) {
	case *TcBuiltinProcDef:
		builder.WriteString(impl.Prefix)
		builder.CompileSeparatedExprList(context, call.Args, impl.Infix)
		builder.WriteString(impl.Postfix)
	case *TcBuiltinGenericProcDef:
		builder.WriteString(impl.Prefix)
		builder.CompileSeparatedExprList(context, call.Args, impl.Infix)
		builder.WriteString(impl.Postfix)
	case *TcProcDef:
		context.markProcForGeneration(impl)
		builder.WriteString(impl.MangledName)
		builder.WriteString("(")
		builder.CompileSeparatedExprList(context, call.Args, ", ")
		builder.WriteString(")")
	case *TcTemplateDef:
		panic(fmt.Errorf("internal error: templates must be resolved before code generation"))
	case *TcBuiltinMacroDef:
		panic(fmt.Errorf("internal error: macros must be resolved before code generation"))
	default:
		panic(fmt.Errorf("internal error: %T", impl))
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

func (builder *CodeBuilder) compileStrLit(value string, boxed bool) {
	if boxed {
		builder.WriteString(`(string){.len=`)
		WriteUIntLit(&builder.Builder, false, uint64(len(value)))
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

var BigMinInt64 = big.NewInt(math.MinInt64)

func (builder *CodeBuilder) CompileIntLit(lit TcIntLit) {
	// C doesn't allow -9223372036854775808 as a singed integer literal
	if lit.Value.Cmp(BigMinInt64) == 0 {
		builder.WriteString("-9223372036854775807 - 1")
	} else {
		WriteIntLit(&builder.Builder, lit.Value)
	}
}

func (builder *CodeBuilder) CompileFloatLit(lit TcFloatLit) {
	fmt.Fprintf(builder, "%f", lit.Value)
}

func (builder *CodeBuilder) CompileSymbol(sym TcSymbol) {
	switch sym.Kind {
	case SkLoopIterator, SkVarProcArg:
		builder.WriteString("*")
		builder.WriteString(sym.Source)
	case SkEnum:
		typ := sym.Type.(*EnumType)
		builder.WriteString(typ.Impl.Name)
		builder.WriteRune('_')
		builder.WriteString(sym.Source)
	case SkConst:
		// should be OK here to pass in nil as context. Only literals allowed here and they don't need a context argument
		builder.CompileExpr(nil, sym.Value)
	default:
		builder.WriteString(sym.Source)
	}
}

func (builder *CodeBuilder) CompileExpr(context *PackageGeneratorContext, expr TcExpr) {
	builder.CompileExprWithPrefix(context, expr)
}

func (builder *CodeBuilder) CompileVariableDefStmt(context *PackageGeneratorContext, stmt TcVariableDefStmt) {
	builder.compileTypeExpr(context, stmt.Sym.GetType())
	builder.WriteString(" ")
	builder.WriteString(stmt.Sym.Source)
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
	builder.WriteString("(")
	builder.CompileExpr(context, dotExpr.Lhs)
	builder.WriteString(").")
	builder.CompileExpr(context, dotExpr.Rhs)
}

func (builder *CodeBuilder) CompileForLoopStmt(context *PackageGeneratorContext, stmt TcForLoopStmt) {
	if stmt.Collection.GetType() == TypeStr {
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
		builder.compileTypeExpr(context, arrayType.Elem)
		builder.WriteString(" const ")
		builder.CompileSymbol(stmt.LoopSym)
		builder.WriteString(" = ")
		builder.CompileExpr(context, stmt.Collection)
		builder.WriteString(".arr, ")
		builder.CompileSymbol(stmt.LoopSym)
		builder.WriteString("_END = ")
		builder.WriteString(stmt.LoopSym.Source)
		builder.WriteString(" + ")
		WriteUIntLit(&builder.Builder, false, uint64(arrayType.Len))
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

func (builder *CodeBuilder) CompileWhileLoopStmt(context *PackageGeneratorContext, stmt TcWhileLoopStmt) {
	builder.WriteString("while(")
	builder.CompileExpr(context, stmt.Condition)
	builder.WriteString(") ")
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
	builder.compileTypeExpr(context, lit.Type)
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
	case TcStrLit:
		builder.compileStrLit(ex.Value, ex.Type == TypeStr)
	case CharLit:
		builder.compileCharLit(ex)
	case TcIntLit:
		builder.CompileIntLit(ex)
	case TcFloatLit:
		builder.CompileFloatLit(ex)
	case NilLit:
		builder.WriteString("NULL")
	case TcArrayLit:
		builder.CompileArrayLit(context, ex)
	case TcSymbol:
		builder.CompileSymbol(ex)
	case TcVariableDefStmt:
		builder.CompileVariableDefStmt(context, ex)
	case TcReturnExpr:
		// ignore the value of injectReturn here
		builder.WriteString("return ")
		builder.CompileExpr(context, ex.Value)
	case TcIfStmt:
		builder.CompileIfStmt(context, ex)
	case TcIfElseExpr:
		builder.CompileIfElseExpr(context, ex)
	case TcForLoopStmt:
		builder.CompileForLoopStmt(context, ex)
	case TcWhileLoopStmt:
		builder.CompileWhileLoopStmt(context, ex)
	case TcStructLit:
		builder.CompileStructLit(context, ex)
	case TcEnumSetLit:
		builder.CompileEnumSetLit(context, ex)
	case TcStructField:
		builder.WriteString(ex.Name)
	case TcTypeContext:
		builder.compileTypeExpr(context, ex.WrappedType)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T expr: %s", expr, AstFormat(expr)))
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
	builder.compileTypeExpr(context, arrayType.Elem)
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
		builder.compileTypeExpr(context, field.Type)
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
	headBuilder.compileTypeExpr(context, procDef.Signature.ResultType)
	headBuilder.WriteString(" ")
	headBuilder.WriteString(procDef.MangledName)
	headBuilder.WriteString("(")
	for i, arg := range procDef.Signature.Params {
		if i != 0 {
			headBuilder.WriteString(", ")
		}
		headBuilder.compileTypeExpr(context, arg.GetType())
		headBuilder.WriteString(" ")
		if arg.Kind == SkVarProcArg {
			headBuilder.WriteString("*")
		}
		headBuilder.WriteString(arg.Source)
	}
	headBuilder.WriteString(")")

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
		body.Items = append(body.Items, TcReturnExpr{Value: procDef.Body})
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
	if procDef.Importc {
		return // importc procs don't have an impl
	}
	if procDef.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	context.TodoListProc = append(context.TodoListProc, procDef)
	procDef.scheduledforgeneration = true
}

func markTypeForGeneration(context *PackageGeneratorContext, typ Type) {
	switch typ := typ.(type) {
	case *ArrayType:
		markArrayTypeForGeneration(context, typ)
	case *StructType:
		markStructTypeForGeneration(context, typ)
	case *EnumType:
		markEnumTypeForGeneration(context, typ)
	case *EnumSetType:
		markEnumTypeForGeneration(context, typ.Elem)
	case *PtrType:
		markTypeForGeneration(context, typ.Target)
	case *BuiltinType, *BuiltinIntType, *BuiltinFloatType, *BuiltinStringType:
		return
	default:
		panic(fmt.Errorf("internal error: %T", typ))
	}
}

func markArrayTypeForGeneration(context *PackageGeneratorContext, typ *ArrayType) {
	if typ.scheduledforgeneration {
		return // nothing to do when already scheduled
	}
	markTypeForGeneration(context, typ.Elem)
	typ.scheduledforgeneration = true
	context.TodoListTypes = append(context.TodoListTypes, typ)
	//fmt.Printf("marking type for generation: %s %s\n", AstFormat(typ), AstFormat(typ.Elem))
}

func markStructTypeForGeneration(context *PackageGeneratorContext, typ *StructType) {
	if typ.scheduledforgeneration || typ.Impl.Importc {
		return // nothing to do when already scheduled
	}
	for _, field := range typ.Impl.Fields {
		markTypeForGeneration(context, field.Type)
	}
	typ.scheduledforgeneration = true
	context.TodoListTypes = append(context.TodoListTypes, typ)
}

func markEnumTypeForGeneration(context *PackageGeneratorContext, typ *EnumType) {
	if typ.scheduledforgeneration || typ.Impl.Importc {
		return // nothing to do when already scheduled
	}
	typ.scheduledforgeneration = true
	context.TodoListTypes = append(context.TodoListTypes, typ)
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

func (context *PackageGeneratorContext) popMarkedForGenerationType() (result Type) {
	N := len(context.TodoListTypes)
	if N > 0 {
		result = context.TodoListTypes[N-1]
		context.TodoListTypes = context.TodoListTypes[:N-1]
	}
	return result
}

// TODO this needs a signature change. Compiling a Package will also trigger the
// compilation of its imported packages. This is not yet reflected in the API.
func compilePackageToC(program *ProgramContext, pak *TcPackageDef, mainPackage bool) string {
	context := &PackageGeneratorContext{Pak: pak}
	context.includes.NewlineAndIndent()
	context.includes.WriteString("#include <stdint.h>")
	context.includes.NewlineAndIndent()
	// TODO this should depend on the usage of `printf`
	context.includes.WriteString("#include <stdio.h>")
	context.includes.NewlineAndIndent()

	for _, emit := range pak.EmitStatements {
		context.includes.WriteString(emit.Value.Value)
		context.includes.NewlineAndIndent()
	}

	// TODO this should depend on the usage of `assert`
	context.includes.WriteString("#include <assert.h>")
	context.typeDecl.NewlineAndIndent()
	context.typeDecl.WriteString("typedef struct string {size_t len; char const* data;} string;")
	context.typeDecl.NewlineAndIndent()
	context.typeDecl.WriteString("typedef unsigned char bool;")
	context.typeDecl.NewlineAndIndent()
	context.typeDecl.WriteString("typedef float f32x4 __attribute__ ((vector_size(16), aligned(16)));")

	// program.Main
	if mainPackage {
		context.markProcForGeneration(program.Main)
	}

	for procDef := context.popMarkedForGenerationProcDef(); procDef != nil; procDef = context.popMarkedForGenerationProcDef() {
		compileProcDef(context, procDef)
	}

	for _, typ := range context.TodoListTypes {
		switch typ := typ.(type) {
		case *EnumType:
			compileEnumDef(context, typ.Impl)
		case *StructType:
			compileStructDef(context, typ.Impl)
		case *ArrayType:
			compileArrayDef(context, typ)
		default:
			panic(fmt.Errorf("internal error: this type should not be in the marked for generation queue: %T", typ))
		}
	}

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
