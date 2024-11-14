package main

import (
	"fmt"
	"math"
	"math/big"
	"strings"
	"unicode/utf8"
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

	// procs marked for code generation
	ScheduledForGenerationProcDef    map[*TcProcDef]bool
	ScheduledForGenerationStructType map[*StructType]bool
	ScheduledForGenerationEnumType   map[*EnumType]bool
	ScheduledForGenerationArrayType  map[*ArrayType]bool
	ScheduledForGenerationVectorType map[*SimdVectorType]bool
}

func NewPackageGeneratorContext(pak *TcPackageDef) *PackageGeneratorContext {
	return &PackageGeneratorContext{
		Pak:                              pak,
		ScheduledForGenerationProcDef:    make(map[*TcProcDef]bool),
		ScheduledForGenerationStructType: make(map[*StructType]bool),
		ScheduledForGenerationEnumType:   make(map[*EnumType]bool),
		ScheduledForGenerationArrayType:  make(map[*ArrayType]bool),
		ScheduledForGenerationVectorType: make(map[*SimdVectorType]bool),
	}
}

func (builder *CodeBuilder) NewlineAndIndent() {
	builder.WriteString("\n")
	for i := 0; i < builder.Indentation; i++ {
		builder.WriteString("  ")
	}
}

func (builder *CodeBuilder) CompileTypeExpr(context *PackageGeneratorContext, typ Type) {
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
	case *SimdVectorType:
		builder.WriteString(typ.Elem.GetName())
		builder.WriteString("x")
		WriteUIntLit(&builder.Builder, false, uint64(typ.Len))
	case *StructType:
		// TODO this should be the mangled name (or importc name)
		builder.WriteString(typ.Impl.Name)
	case *EnumType:
		// TODO this should be the mangled name (or importc name)
		builder.WriteString(typ.Impl.Name)
	case *EnumSetType:
		builder.WriteString("uint64_t")
	case *PtrType:
		builder.CompileTypeExpr(context, typ.Target)
		builder.WriteRune('*')
	default:
		panic(fmt.Errorf("not implemented %T", typ))
	}
}

func (builder *CodeBuilder) CompileReturn(context *PackageGeneratorContext, expr *TcReturnStmt) {
	builder.WriteString("return ")
	builder.CompileArg(context, expr.Value, false)
}

func (builder *CodeBuilder) CompileCall(context *PackageGeneratorContext, call *TcCall) {
	switch impl := call.Sym.Overloadable.(type) {
	case *TcBuiltinProcDef:
		builder.WriteString(impl.Prefix)
		builder.CompileSeparatedExprList(context, call.Args, impl.Infix)
		builder.WriteString(impl.Postfix)
	case *TcProcDef:
		context.markProcForGeneration(impl)
		builder.WriteString(impl.MangledName)
		builder.WriteString("(")
		if impl.Importc {
			builder.CompileSeparatedExprList(context, call.Args, ", ")
		} else {
			for i, it := range call.Args {
				if i != 0 {
					builder.WriteString(", ")
				}
				builder.CompileArg(context, it, SymIsImplicitPointer(impl.Signature.Params[i]))
			}
		}
		builder.WriteString(")")
	case *TcTemplateDef:
		panic(fmt.Errorf("internal error: templates must be resolved before code generation"))
	case *TcBuiltinMacroDef:
		panic(fmt.Errorf("internal error: macros must be resolved before code generation"))
	default:
		panic(fmt.Errorf("internal error: %T", impl))
	}
}

func (builder *CodeBuilder) compileStrLit(value string, boxed bool, quote rune) {
	if boxed {
		builder.WriteString(`(string){.len=`)
		WriteUIntLit(&builder.Builder, false, uint64(len(value)))
		builder.WriteString(`, .data=`)
	}
	builder.WriteRune(quote)
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
	builder.WriteRune(quote)
	if boxed {
		builder.WriteRune('}')
	}
}

var BigMinInt64 = big.NewInt(math.MinInt64)

func (builder *CodeBuilder) CompileIntLit(lit *TcIntLit) {
	// C doesn't allow -9223372036854775808 as a singed integer literal
	if lit.Value.Cmp(BigMinInt64) == 0 {
		builder.WriteString("-9223372036854775807 - 1")
	} else {
		WriteIntLit(&builder.Builder, lit.Value)
		if lit.Type == TypeUInt64 {
			builder.WriteString("u")
		}
	}
}

func (builder *CodeBuilder) CompileFloatLit(lit *TcFloatLit) {
	fmt.Fprintf(builder, "%f", lit.Value)
}

func (builder *CodeBuilder) CompileSymbol(sym *TcSymbol) {
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

func (builder *CodeBuilder) CompileVariableDefStmt(context *PackageGeneratorContext, stmt *TcVariableDefStmt) {
	builder.CompileTypeExpr(context, stmt.Sym.GetType())
	builder.WriteString(" ")
	builder.WriteString(stmt.Sym.Source)
	builder.WriteString(" = ")
	builder.CompileExpr(context, stmt.Value)
}

func (builder *CodeBuilder) CompileIfStmt(context *PackageGeneratorContext, stmt *TcIfStmt) {
	for i, it := range stmt.ConditionBodyPairs {
		if i == 0 {
			builder.WriteString("if (")
		} else {
			builder.WriteString("else if (")
		}
		builder.CompileExpr(context, it.Condition)
		builder.WriteString(") ")
		builder.CompileExpr(context, it.Body)
	}

}

func ExprHasValue(expr TcExpr) bool {
	typ := expr.GetType()
	return typ != TypeNoReturn && typ != TypeVoid
}

func (builder *CodeBuilder) CompileIfElseExpr(context *PackageGeneratorContext, stmt *TcIfElseExpr) {
	if ExprHasValue(stmt) {
		builder.WriteString("(")
		for _, it := range stmt.ConditionBodyPairs {
			builder.CompileExpr(context, it.Condition)
			builder.WriteString(" ? ")
			builder.CompileExpr(context, it.Body)
			builder.WriteString(" : ")
		}
		builder.CompileExpr(context, stmt.Else)
		builder.WriteString(")")
	} else {
		for _, it := range stmt.ConditionBodyPairs {
			builder.WriteString("if (")
			builder.CompileExpr(context, it.Condition)
			builder.WriteString(") ")
			builder.CompileExpr(context, it.Body)
			builder.WriteString(" else ")
		}
		builder.CompileExpr(context, stmt.Else)
	}
}

func (builder *CodeBuilder) CompileDotExpr(context *PackageGeneratorContext, dotExpr *TcDotExpr) {
	builder.WriteString("(")
	builder.CompileExpr(context, dotExpr.Lhs)
	if IsImplicitPointer(dotExpr.Lhs) {
		builder.WriteString(")->")
	} else {
		builder.WriteString(").")
	}
	builder.CompileExpr(context, dotExpr.Rhs)
}

func (builder *CodeBuilder) CompileForLoopStmt(context *PackageGeneratorContext, stmt *TcForLoopStmt) {
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
		builder.CompileTypeExpr(context, arrayType.Elem)
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

func (builder *CodeBuilder) CompileWhileLoopStmt(context *PackageGeneratorContext, stmt *TcWhileLoopStmt) {
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

func (builder *CodeBuilder) CompileArrayLit(context *PackageGeneratorContext, lit *TcArrayLit) {
	builder.WriteString("(")
	builder.CompileTypeExpr(context, lit.Type)
	builder.WriteString("){")
	builder.CompileSeparatedExprList(context, lit.Items, ", ")
	builder.WriteString("}")
}

func (builder *CodeBuilder) CompileStructLit(context *PackageGeneratorContext, lit *TcStructLit) {
	builder.WriteString("(")
	builder.CompileTypeExpr(context, lit.Type)
	builder.WriteString("){")
	builder.CompileSeparatedExprList(context, lit.Items, ", ")
	builder.WriteString("}")
}

func (builder *CodeBuilder) CompileEnumSetLit(context *PackageGeneratorContext, lit *TcEnumSetLit) {
	builder.WriteString("((1 << ")
	builder.CompileSeparatedExprList(context, lit.Items, ") | (1 << ")
	builder.WriteString("))")
}

func (builder *CodeBuilder) CompileArg(context *PackageGeneratorContext, arg TcExpr, requirePointer bool) {
	if symRef, isSymRef := arg.(*TcSymRef); isSymRef {
		isImplicitPointer := SymIsImplicitPointer(symRef.Sym)
		if isImplicitPointer == requirePointer {
		} else if isImplicitPointer && !requirePointer {
			builder.WriteString("*")
		} else if !isImplicitPointer && requirePointer {
			builder.WriteString("&")
		}
	}
	builder.CompileExpr(context, arg)
}

func (builder *CodeBuilder) CompileExpr(context *PackageGeneratorContext, expr TcExpr) {

	switch ex := expr.(type) {
	case *TcCodeBlock:
		builder.CompileCodeBlock(context, ex)
	case *TcCall:
		builder.CompileCall(context, ex)
	case *TcDotExpr:
		builder.CompileDotExpr(context, ex)
	case *TcStrLit:
		switch ex.Type {
		case TypeStr:
			builder.compileStrLit(ex.Value, true, '"')
		case TypeCStr:
			builder.compileStrLit(ex.Value, false, '"')
		case TypeChar: // TODO dead code, should be handled in IntLit
			rune, _ := utf8.DecodeRuneInString(ex.Value)
			if rune < 128 {
				builder.compileStrLit(ex.Value, false, '\'')
			} else {
				fmt.Fprintf(builder, "%d", rune)
			}
		default:
			panic("internal error")
		}
	case *TcIntLit:
		builder.CompileIntLit(ex)
	case *TcFloatLit:
		builder.CompileFloatLit(ex)
	case *NilLit:
		builder.WriteString("NULL")
	case *TcArrayLit:
		builder.CompileArrayLit(context, ex)
	case *TcSymbol:
		builder.CompileSymbol(ex)
	case *TcSymRef:
		builder.CompileSymbol(ex.Sym)
	case *TcVariableDefStmt:
		builder.CompileVariableDefStmt(context, ex)
	case *TcReturnStmt:
		builder.CompileReturn(context, ex)
	case *TcIfStmt:
		builder.CompileIfStmt(context, ex)
	case *TcIfElseExpr:
		builder.CompileIfElseExpr(context, ex)
	case *TcForLoopStmt:
		builder.CompileForLoopStmt(context, ex)
	case *TcWhileLoopStmt:
		builder.CompileWhileLoopStmt(context, ex)
	case *TcStructLit:
		builder.CompileStructLit(context, ex)
	case *TcEnumSetLit:
		builder.CompileEnumSetLit(context, ex)
	case *TcStructField:
		builder.WriteString(ex.Name)
	case *TcTypeContext:
		builder.CompileTypeExpr(context, ex.WrappedType)
	case *TcEmitExpr:
		builder.CompileEmitExpr(context, ex)
	case *TcCastExpr:
		builder.CompileCastExpr(context, ex)
	case *TcConvExpr:
		builder.CompileConvExpr(context, ex)
	case nil:
		panic(fmt.Sprintf("invalid Ast, expression is nil %T", expr))
	default:
		panic(fmt.Sprintf("Not implemented %T expr: %s", expr, AstFormat(expr)))
	}
}

func (builder *CodeBuilder) CompileEmitExpr(context *PackageGeneratorContext, emit *TcEmitExpr) {
	for _, pair := range emit.SourceSymPairs {
		builder.WriteString(pair.EmitSource)
		builder.CompileExpr(context, pair.Sym)
	}
	builder.WriteString(emit.EmitSource)
	builder.NewlineAndIndent()
}

func (builder *CodeBuilder) CompileCastExpr(context *PackageGeneratorContext, cast *TcCastExpr) {
	builder.WriteString("*((")
	builder.CompileTypeExpr(context, cast.Type)
	builder.WriteString("*)(&(")
	builder.CompileExpr(context, cast.Expr)
	builder.WriteString(")))")
}

func (builder *CodeBuilder) CompileConvExpr(context *PackageGeneratorContext, cast *TcConvExpr) {
	builder.WriteString("(")
	builder.CompileTypeExpr(context, cast.Type)
	builder.WriteString(")(")
	builder.CompileExpr(context, cast.Expr)
	builder.WriteString(")")
}

func (builder *CodeBuilder) CompileCodeBlock(context *PackageGeneratorContext, block *TcCodeBlock) {
	isExpr := ExprHasValue(block)
	if isExpr {
		builder.WriteString("(")
	}
	builder.WriteString("{")
	builder.Indentation += 1
	for _, expr := range block.Items {
		builder.NewlineAndIndent()
		builder.CompileExpr(context, expr)
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
	builder.CompileTypeExpr(context, arrayType.Elem)
	fmt.Fprintf(builder, " arr[%d];} ", arrayType.Len)
	arrayType.ManglePrint(&builder.Builder)
	builder.WriteString(";")
}

func compileSimdVectorDef(context *PackageGeneratorContext, arrayType *SimdVectorType) {
	builder := &context.typeDecl
	builder.NewlineAndIndent()
	builder.WriteString("typedef ")

	builder.CompileTypeExpr(context, arrayType.Elem)
	builder.WriteString(" ")
	builder.WriteString(arrayType.Elem.GetName())
	builder.WriteString("x")
	WriteUIntLit(&builder.Builder, false, uint64(arrayType.Len))

	builder.WriteString(" __attribute__ ((vector_size(")
	WriteUIntLit(&builder.Builder, false, uint64(arrayType.Len))
	builder.WriteString("*sizeof(")
	builder.CompileTypeExpr(context, arrayType.Elem)
	builder.WriteString(")), aligned(")
	WriteUIntLit(&builder.Builder, false, uint64(arrayType.Len))
	builder.WriteString("*sizeof(")
	builder.CompileTypeExpr(context, arrayType.Elem)
	builder.WriteString("))));")

	//typedef float f32x4 __attribute__ ((vector_size(16), aligned(16)));
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
		builder.compileStrLit(sym.Source, true, '"')
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
		builder.CompileTypeExpr(context, field.Type)
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

func IsImplicitPointer(expr TcExpr) bool {
	if sym, isSymRef := expr.(*TcSymRef); isSymRef {
		return SymIsImplicitPointer(sym.Sym)
	}
	return false
}

func SymIsImplicitPointer(sym *TcSymbol) bool {
	switch sym.Kind {
	case SkInvalid:
		panic("invalid sym kind")
	case SkConst:
		return false
	case SkLet:
		return false
	case SkVar:
		return false
	case SkProcArg:
		_, isStructType := sym.Type.(*StructType)
		return isStructType
	case SkVarProcArg:
		return true
	case SkLoopIterator:
		return false
	case SkVarLoopIterator:
		return true
	case SkEnum:
		return false
	}
	panic("internal error, switch not exhaustive")
}

func compileProcDef(context *PackageGeneratorContext, procDef *TcProcDef) {
	if len(procDef.Signature.GenericParams) != 0 {
		panic("generic instances cannot be used here")
	}

	//procDef = cgenprepass(procDef).(*TcProcDef)

	// reuse string here
	headBuilder := &CodeBuilder{}
	headBuilder.NewlineAndIndent()
	headBuilder.CompileTypeExpr(context, procDef.Signature.ResultType)
	headBuilder.WriteString(" ")
	headBuilder.WriteString(procDef.MangledName)
	headBuilder.WriteString("(")
	for i, arg := range procDef.Signature.Params {
		if i != 0 {
			headBuilder.WriteString(", ")
		}
		headBuilder.CompileTypeExpr(context, arg.GetType())
		headBuilder.WriteString(" ")

		if SymIsImplicitPointer(arg) {
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
	body := &TcCodeBlock{Source: procDef.Body.GetSource()}

	if injectReturn {
		body.Items = append(body.Items, &TcReturnStmt{Value: procDef.Body})
	} else {
		codeBlockBody, ok := procDef.Body.(*TcCodeBlock)
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

	if context.ScheduledForGenerationProcDef[procDef] {
		return // nothing to do when already scheduled
	}
	context.TodoListProc = append(context.TodoListProc, procDef)
	context.ScheduledForGenerationProcDef[procDef] = true
}

func markTypeForGeneration(context *PackageGeneratorContext, typ Type) {
	switch typ := typ.(type) {
	case *ArrayType:
		markArrayTypeForGeneration(context, typ)
	case *SimdVectorType:
		markSimdVectorTypeForGeneration(context, typ)
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
	if context.ScheduledForGenerationArrayType[typ] {
		return // nothing to do when already scheduled
	}
	markTypeForGeneration(context, typ.Elem)
	context.TodoListTypes = append(context.TodoListTypes, typ)
	context.ScheduledForGenerationArrayType[typ] = true
	//fmt.Printf("marking type for generation: %s %s\n", AstFormat(typ), AstFormat(typ.Elem))
}

func markSimdVectorTypeForGeneration(context *PackageGeneratorContext, typ *SimdVectorType) {
	if context.ScheduledForGenerationVectorType[typ] {
		return // nothing to do when already scheduled
	}
	markTypeForGeneration(context, typ.Elem)
	context.TodoListTypes = append(context.TodoListTypes, typ)
	context.ScheduledForGenerationVectorType[typ] = true
	//fmt.Printf("marking type for generation: %s %s\n", AstFormat(typ), AstFormat(typ.Elem))
}

func markStructTypeForGeneration(context *PackageGeneratorContext, typ *StructType) {
	if typ.Impl.Importc || context.ScheduledForGenerationStructType[typ] {
		return // nothing to do when already scheduled
	}
	for _, field := range typ.Impl.Fields {
		markTypeForGeneration(context, field.Type)
	}
	context.TodoListTypes = append(context.TodoListTypes, typ)
	context.ScheduledForGenerationStructType[typ] = true
}

func markEnumTypeForGeneration(context *PackageGeneratorContext, typ *EnumType) {
	if typ.Impl.Importc || context.ScheduledForGenerationEnumType[typ] {
		return // nothing to do when already scheduled
	}
	context.TodoListTypes = append(context.TodoListTypes, typ)
	context.ScheduledForGenerationEnumType[typ] = true
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
	context := NewPackageGeneratorContext(pak)

	context.includes.NewlineAndIndent()
	context.includes.WriteString("#include <stdint.h>")
	context.includes.NewlineAndIndent()
	// TODO this should depend on the usage of `printf`
	context.includes.WriteString("#include <stdio.h>")
	context.includes.NewlineAndIndent()

	for _, emit := range pak.EmitStatements {
		context.includes.CompileEmitExpr(context, emit)
	}

	// TODO this should depend on the usage of `assert`
	context.includes.WriteString(`
#include <assert.h>
typedef struct string {size_t len; char const* data;} string;
typedef unsigned char bool;
`)

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
		case *SimdVectorType:
			compileSimdVectorDef(context, typ)
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
