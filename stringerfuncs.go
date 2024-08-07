package main

import (
	"fmt"
	"slices"
	"strings"
)

/// TOKENS

type AstPrettyPrinter struct {
	strings.Builder
	LineIdx     int
	Indentation int
}

func (this TokenKind) String() string {
	return TokenKindNames[this]
}

func (this Token) String() string {
	return fmt.Sprintf("(%s %q)", this.kind, this.value)
}

/// AST NODES

func (builder *AstPrettyPrinter) NewlineAndIndent() {
	builder.WriteRune('\n')
	N := builder.Indentation
	for i := 0; i < N; i++ {
		builder.WriteString("  ")
	}
	builder.LineIdx += 1
}

type PrettyPrintable interface {
	PrettyPrint(builder *AstPrettyPrinter)
}

func (builder *AstPrettyPrinter) WriteNode(node PrettyPrintable) {
	if node == nil {
		builder.WriteString("<nil>")
	} else {
		node.PrettyPrint(builder)
	}
}

func AstFormat(node PrettyPrintable) string {
	if node == nil {
		return "<nil>" // this branch is a compiler bug. Crashing would be more helpful to fix it than this.
	}
	builder := &AstPrettyPrinter{}
	node.PrettyPrint(builder)
	result := builder.String()
	if len(result) == 0 {
		return fmt.Sprintf("%T", node)
	}
	return result
}

func (arg InvalidTokenExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("<invalid ")
	builder.WriteString(TokenKindNames[arg.token.kind])
	builder.WriteString(": ")
	builder.WriteString(arg.token.value)
	builder.WriteString(">")
}

func (ident Ident) PrettyPrint(builder *AstPrettyPrinter) {
	// the only exception where pretty Print may print the original
	// source.
	builder.WriteString(ident.Source)
}

func (call Call) PrettyPrint(builder *AstPrettyPrinter) {
	ident, isIdent := call.Callee.(Ident)
	isOperator := isIdent && slices.Contains([]string{".", ":"}, ident.Source)

	// maybe operator should be its own syntax token
	if !isOperator {
		builder.WriteNode(call.Callee)
	}
	builder.WriteString("(")
	for i, arg := range call.Args {
		if i != 0 {
			if isOperator {
				builder.WriteString(ident.Source)
			} else {
				builder.WriteString(", ")
			}
		}
		builder.WriteNode(arg)
	}
	builder.WriteString(")")
}

func (expr ColonExpr) PrettyPrint(builder *AstPrettyPrinter) {
	expr.Lhs.PrettyPrint(builder)
	builder.WriteString(": ")
	expr.Rhs.PrettyPrint(builder)
}

func (codeBlock CodeBlock) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("{")
	builder.Indentation++
	for _, item := range codeBlock.Items {
		builder.NewlineAndIndent()
		builder.WriteNode(item)
		builder.WriteString(";")
	}
	builder.Indentation--
	if len(codeBlock.Items) > 0 {
		builder.NewlineAndIndent()
	}
	builder.WriteString("}")
}

func (lit CharLit) PrettyPrint(builder *AstPrettyPrinter) {
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

func (lit StrLit) PrettyPrint(builder *AstPrettyPrinter) {
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

func (lit CStrLit) PrettyPrint(builder *AstPrettyPrinter) {
	// TODO, this isn't correct, it is indestinguishable from StrLit
	StrLit{Value: lit.Value}.PrettyPrint(builder)
}

func (list ExprList) PrettyPrint(builder *AstPrettyPrinter) {
	// TODO this is almost identical to `ArrayLit` maybe the node can me merged?
	builder.WriteRune('(')
	for i, expr := range list.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteNode(expr)
	}
	builder.WriteRune(')')
}

func (lit ArrayLit) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteNode(expr)
	}
	builder.WriteRune(']')
}

func (node TcErrorNode) PrettyPrint(builder *AstPrettyPrinter) {
	node.SourceNode.PrettyPrint(builder)
}

func (lit TcArrayLit) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteNode(expr)
	}
	builder.WriteRune(']')
}

func (lit TcStructLit) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteNode(expr)
	}
	builder.WriteString("]:")
	//builder.WriteNode(lit.typ)
	builder.WriteString(lit.Type.Impl.Name)
}

func (lit TcEnumSetLit) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteNode(expr)
	}
	builder.WriteString("]: set[")
	//builder.WriteNode(lit.typ)
	builder.WriteNode(lit.ElemType)
	builder.WriteRune(']')
}

func WriteIntLit(builder *strings.Builder, value int64) {
	// TODO split this up in "WriteNumber" that just prints the number, and "WriteCIntLit" that has the special case for the smallest number
	//fmt.Fprintf(builder, "%d", value)
	if value == 0 {
		builder.WriteByte('0')
		return
	}
	if value < 0 {
		builder.WriteRune('-')
		value = -value
	}
	if value >= 0 {
		const N = 32
		var buffer [N]byte
		i := 0
		for ; value > 0; i++ {
			buffer[i] = "0123456789"[value%10]
			value = value / 10
		}
		for ; i > 0; i-- {
			builder.WriteByte(buffer[i-1])
		}
	} else {
		// value = -value had no effect
		if value != -9223372036854775808 {
			panic("assert fail")
		}
		// C doesn't allow -9223372036854775808 as a singed integer literal
		builder.WriteString("9223372036854775807 - 1")
	}
}

func (lit IntLit) PrettyPrint(builder *AstPrettyPrinter) {
	WriteIntLit(&builder.Builder, lit.Value)
	if lit.Type != nil && lit.Type != (Type)(GetIntLitType(lit.Value)) {
		builder.WriteString(":")
		lit.Type.PrettyPrint(builder)
	}
}

func (lit FloatLit) PrettyPrint(builder *AstPrettyPrinter) {
	str := fmt.Sprintf("%f", lit.Value)
	builder.WriteString(str)
	if lit.Type != nil {
		builder.WriteString(":")
		lit.Type.PrettyPrint(builder)
	}
}

func (lit NilLit) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("nullptr")
}

// func (typeExpr TypeExpr) PrettyPrint(builder *AstPrettyPrinter) {
// 	builder.WriteString(typeExpr.Ident.Source)
// 	if len(typeExpr.ExprArgs) > 0 {
// 		builder.WriteString("(")
// 		for i, arg := range typeExpr.ExprArgs {
// 			if i != 0 {
// 				builder.WriteString(", ")
// 			}
// 			builder.WriteNode(arg)
// 		}
// 		builder.WriteString(")")
// 	}
// 	if len(typeExpr.TypeArgs) > 0 {
// 		builder.WriteString("[")
// 		for i, arg := range typeExpr.TypeArgs {
// 			if i != 0 {
// 				builder.WriteString(", ")
// 			}
// 			builder.WriteNode(arg)
// 		}
// 		builder.WriteString("]")
// 	}
// }

func (typeDef StructDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteNode(typeDef.Name)
	builder.WriteString(" = struct {")
	builder.Indentation += 1
	for _, field := range typeDef.Fields {
		builder.NewlineAndIndent()
		builder.WriteNode(field)
	}
	builder.Indentation -= 1
	builder.NewlineAndIndent()
	builder.WriteString("}")
}

func (typeDef EnumDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteNode(typeDef.Name)
	builder.WriteString(" = struct {")
	builder.Indentation += 1
	for _, field := range typeDef.Values {
		builder.NewlineAndIndent()
		builder.WriteNode(field)
	}
	builder.Indentation -= 1
	builder.NewlineAndIndent()
	builder.WriteString("}")
}

func (typeDef TraitDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("trait ")
	builder.WriteNode(typeDef.Name)
	builder.WriteString("(")
	for i, typ := range typeDef.DependentTypes {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteNode(typ)
	}
	builder.WriteString(") {")
	for _, sig := range typeDef.Signatures {
		builder.WriteNode(sig)
	}
	builder.WriteString("}")
}

func (typeDef *TcTraitDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("trait ")
	builder.WriteString(typeDef.Name)
	builder.WriteString("(")
	for i, typ := range typeDef.DependentTypes {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteNode(typ)
	}
	builder.WriteString(") {")
	for _, sig := range typeDef.Signatures {
		builder.WriteNode(sig)
	}
	builder.WriteString("}")
}

func (procDef *ProcDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc ")
	if procDef.Annotations.Value != "" {
		procDef.Annotations.PrettyPrint(builder)
		builder.WriteString(" ")
	}
	builder.WriteNode(procDef.Name)
	builder.WriteString("(")
	if len(procDef.Args) > 3 {
		for i, arg := range procDef.Args {
			if i != 0 {
				builder.WriteString(", ")
			}
			builder.NewlineAndIndent()
			builder.WriteNode(arg.Name)
			builder.WriteString(": ")
			builder.WriteNode(arg.Type)
		}
		builder.NewlineAndIndent()

	} else {
		for i, arg := range procDef.Args {
			if i != 0 {
				builder.WriteString(", ")
			}
			builder.WriteNode(arg.Name)
			builder.WriteString(": ")
			builder.WriteNode(arg.Type)
		}
	}
	builder.WriteString("): ")
	builder.WriteNode(procDef.ResultType)
	if procDef.Body != nil {
		builder.WriteString(" = ")
		builder.WriteNode(procDef.Body)
	}
}

func (returnExpr ReturnExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("return ")
	builder.WriteNode(returnExpr.Value)
}

func (varExpr VarExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("var ")
	builder.WriteNode(varExpr.Expr)
}

func (expr TypeContext) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteNode(expr.Expr)
}

func (stmt VariableDefStmt) PrettyPrint(builder *AstPrettyPrinter) {
	switch stmt.Kind {
	case SkLet:
		builder.WriteString("let ")
	case SkVar:
		builder.WriteString("var ")
	case SkConst:
		builder.WriteString("const ")
	default:
		panic("illegal or not implemented")
	}
	builder.WriteNode(stmt.Name)
	if stmt.TypeExpr != nil {
		builder.WriteString(": ")
		builder.WriteNode(stmt.TypeExpr)
	}
	if stmt.Value != nil {
		builder.WriteString(" = ")
		builder.WriteNode(stmt.Value)
	}
}

func (loopStmt ForLoopStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("for ")
	builder.WriteNode(loopStmt.LoopIdent)
	builder.WriteString(" in ")
	builder.WriteNode(loopStmt.Collection)
	builder.WriteString(" do ")
	builder.WriteNode(loopStmt.Body)
}

func (loopStmt TcForLoopStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("for ")
	builder.WriteString(loopStmt.LoopSym.Source)
	builder.WriteString(" in ")
	builder.WriteNode(loopStmt.Collection)
	builder.WriteString(" do ")
	builder.WriteNode(loopStmt.Body)
}

func (loopStmt WhileLoopStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("while ")
	builder.WriteNode(loopStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteNode(loopStmt.Body)
}

func (loopStmt TcWhileLoopStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("while ")
	builder.WriteNode(loopStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteNode(loopStmt.Body)
}

func (ifStmt IfExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteNode(ifStmt.Body)
}

func (ifStmt TcIfStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteNode(ifStmt.Body)
}

func (ifStmt IfElseExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteNode(ifStmt.Body)
	builder.WriteString(" else ")
	builder.WriteNode(ifStmt.Else)
}

func (ifStmt TcIfElseExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteNode(ifStmt.Body)
	builder.WriteString(" else ")
	builder.WriteNode(ifStmt.Else)
}

func (pak PackageDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("# file: ")
	builder.WriteString(pak.Name)
	for _, stmt := range pak.TopLevelStmts {
		builder.NewlineAndIndent()
		builder.WriteNode(stmt)
	}
}

func (breakstmt BreakStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("break")
}
func (continuestmt ContinueStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("continue")
}

func (emitstmt EmitStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("emit ")
	emitstmt.Value.PrettyPrint(builder)
}

func (staticExpr StaticExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("static ")
	staticExpr.Expr.PrettyPrint(builder)
}

func (importStmt ImportStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("import ")
	importStmt.Value.PrettyPrint(builder)
}

func (importStmt TcImportStmt) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("import ")
	importStmt.Value.PrettyPrint(builder)
}

// format type checked ast nodes
func (typ *BuiltinType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typ.Name)
}

func (typ *BuiltinIntType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typ.Name)
}

func (typ *BuiltinFloatType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typ.Name)
}

func (typ *UnspecifiedType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("?unspecified?")
}

func (typ *ErrorType) PrettyPrint(builder *AstPrettyPrinter) {
	//fmt.Fprintf(&builder.Builder,
	builder.WriteString("?err?")
}

func (typ *ArrayType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("array(")
	WriteIntLit(&builder.Builder, typ.Len)
	builder.WriteString(", ")
	builder.WriteNode(typ.Elem)
	builder.WriteString(")")
}

func (typ *EnumSetType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("set(")
	builder.WriteNode(typ.Elem)
	builder.WriteString(")")
}

func (typ *GenericTypeSymbol) PrettyPrint(builder *AstPrettyPrinter) {
	// builder.WriteString("(")
	builder.WriteString(typ.Name)
	// builder.WriteString(" : ")
	// typ.Constraint.PrettyPrint(builder)
	// builder.WriteString(")")
}

func (call TcCall) PrettyPrint(builder *AstPrettyPrinter) {
	if call.Braced {
		builder.WriteString(call.Sym.Source)
		builder.WriteString("(")
		for i, arg := range call.Args {
			if i != 0 {
				builder.WriteString(", ")
			}
			builder.WriteNode(arg)
		}
		builder.WriteString(")")
	} else {
		for i, arg := range call.Args {
			if i != 0 {
				builder.WriteString(" ")
				builder.WriteString(call.Sym.Source)
				builder.WriteString(" ")
			}
			builder.WriteNode(arg)
		}
	}
}

func (structDef *StructType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(structDef.Impl.Name)
}

func (structDef *TcStructDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	if structDef.Importc {
		builder.WriteString("\"importc\" ")
	}
	builder.WriteString(structDef.Name)
	builder.WriteString(" = struct {")
	builder.Indentation++
	for _, field := range structDef.Fields {
		builder.NewlineAndIndent()
		builder.WriteString(field.Name)
		builder.WriteString(": ")
		builder.WriteNode(field.Type)
	}
	builder.Indentation--
	builder.NewlineAndIndent()
	builder.WriteString("}")
}

func (typ *EnumType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typ.Impl.Name)
}

func (enumDef *TcEnumDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	if enumDef.Importc {
		builder.WriteString("\"importc\" ")
	}
	builder.WriteString(enumDef.Name)
	builder.WriteString(" = struct {")
	builder.Indentation++
	for _, field := range enumDef.Values {
		builder.NewlineAndIndent()
		builder.WriteString(field.Source)
	}
	builder.Indentation--
	builder.NewlineAndIndent()
	builder.WriteString("}")
}

func (codeBlock TcCodeBlock) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("{")
	builder.Indentation++
	for _, item := range codeBlock.Items {
		builder.NewlineAndIndent()
		builder.WriteNode(item)
		builder.WriteString(";")
	}
	builder.Indentation--
	if len(codeBlock.Items) > 0 {
		builder.NewlineAndIndent()
	}
	builder.WriteString("}")
}

func (sym TcSymbol) PrettyPrint(printer *AstPrettyPrinter) {
	printer.WriteString(sym.Source)
}

func (sym TcProcSymbol) PrettyPrint(printer *AstPrettyPrinter) {
	printer.WriteString(sym.Source)
}

func (stmt TcVariableDefStmt) PrettyPrint(builder *AstPrettyPrinter) {
	switch stmt.Sym.Kind {
	case SkLet:
		builder.WriteString("let ")
	case SkVar:
		builder.WriteString("var ")
	case SkConst:
		builder.WriteString("const ")
	default:
		panic("internal errer")
	}
	builder.WriteNode(stmt.Sym)
	builder.WriteString(": ")
	builder.WriteNode(stmt.Sym.Type)
	// builder.WriteString(stmt.Sym.Typ.Name())
	if stmt.Value != nil {
		builder.WriteString(" = ")
		builder.WriteNode(stmt.Value)
	}
}

func (returnExpr TcReturnExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("return ")
	builder.WriteNode(returnExpr.Value)
}

func (expr TcTypeContext) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteNode(expr.WrappedType)
}

func (expr TcDotExpr) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteNode(expr.Lhs)
	builder.WriteByte('.')
	builder.WriteNode(expr.Rhs)
}

func (expr TcStructField) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(expr.Name)
}

func (signature *Signature) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("(")
	if len(signature.Params) > 3 {
		builder.Indentation += 2
		builder.NewlineAndIndent()
		iLast := len(signature.Params) - 1
		for i, arg := range signature.Params {
			if len(arg.Source) > 0 {
				builder.WriteString(arg.Source)
			} else {
				builder.WriteString("_")
			}
			builder.WriteString(": ")
			builder.WriteNode(arg.GetType())
			if i == iLast {
				builder.Indentation -= 2
			}
			builder.NewlineAndIndent()
		}

	} else {
		for i, arg := range signature.Params {
			if i != 0 {
				builder.WriteString("; ")
			}
			if len(arg.Source) > 0 {
				builder.WriteString(arg.Source)
			} else {
				builder.WriteString("_")
			}
			builder.WriteString(": ")
			builder.WriteNode(arg.GetType())
		}
	}
	builder.WriteString("): ")
	builder.WriteNode(signature.ResultType)
}

func (procDef *TcProcDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc ")
	if procDef.Importc {
		builder.WriteString("\"importc\"")
	}
	builder.WriteString(procDef.Signature.Name)
	procDef.Signature.PrettyPrint(builder)
	if procDef.Body != nil {
		builder.WriteString(" = ")
		builder.WriteNode(procDef.Body)
	}
}

func (procDef *TcBuiltinProcDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc \"builtin\" ")
	builder.WriteString(procDef.Signature.Name)
	procDef.Signature.PrettyPrint(builder)
}

func (procDef *TcBuiltinGenericProcDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc \"builtin\" ")
	builder.WriteString(procDef.Signature.Name)
	procDef.Signature.PrettyPrint(builder)
}

func (procDef *TcTemplateDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("template ")
	builder.WriteString(procDef.Signature.Name)
	procDef.Signature.PrettyPrint(builder)
	builder.WriteString(" = ")
	builder.WriteNode(procDef.Body)
}

func (procDef *TcBuiltinMacroDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("macro \"builtin\"")
	builder.WriteString(procDef.Signature.Name)
	procDef.Signature.PrettyPrint(builder)
}

func (procDef *TcErrorProcDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("# this implemantation does not exist: ")
	builder.WriteString(procDef.Signature.Name)
}

func (pak TcPackageDef) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("# file: ")
	builder.WriteString(pak.Name)

	if len(pak.CFlags) > 0 {
		builder.WriteString("\n# Cflags:\n")
		for _, flag := range pak.CFlags {
			builder.WriteString("addCFlags(")
			StrLit{Value: flag}.PrettyPrint(builder)
			builder.WriteString(")")
		}
		builder.WriteString("\n")
	}

	if len(pak.Imports) > 0 {
		builder.WriteString("\n# imports:")
	}

	for _, importStmt := range pak.Imports {
		builder.NewlineAndIndent()
		builder.WriteNode(importStmt)
	}

	for _, emitStmt := range pak.EmitStatements {
		builder.NewlineAndIndent()
		builder.WriteNode(emitStmt)
	}
	for _, traitDef := range pak.TraitDefs {
		builder.NewlineAndIndent()
		builder.WriteNode(traitDef)
	}
	for _, enumDef := range pak.EnumDefs {
		builder.NewlineAndIndent()
		builder.WriteNode(enumDef)
	}
	for _, structDef := range pak.StructDefs {
		builder.NewlineAndIndent()
		builder.WriteNode(structDef)
	}
	for _, varDef := range pak.VarDefs {
		builder.NewlineAndIndent()
		builder.WriteNode(varDef)
	}
	for _, proc := range pak.ProcDefs {
		builder.NewlineAndIndent()
		builder.WriteNode(proc)
	}
}

func (section NamedDocSection) PrettyPrint(builder *AstPrettyPrinter) {
	builder.NewlineAndIndent()
	builder.WriteString("## ")
	builder.WriteString(section.Name)
	builder.WriteString(":")
	for _, line := range section.Lines {
		builder.NewlineAndIndent()
		builder.WriteString("##   ")
		builder.WriteString(line)
	}
}

func (self UniqueTypeConstraint) PrettyPrint(builder *AstPrettyPrinter) {
	self.Typ.PrettyPrint(builder)
}

func (self *TypeGroup) PrettyPrint(builder *AstPrettyPrinter) {
	if self.Name != "" {
		builder.WriteString(self.Name)
		return
	}
	for i, typ := range self.Items {
		if i != 0 {
			builder.WriteString(" | ")
		}
		typ.PrettyPrint(builder)
	}
}

func (self *TypeTrait) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(self.Impl.Name)
}

func (typ *OpenGenericType) PrettyPrint(builder *AstPrettyPrinter) {
	// don't print open symbols for now
	builder.WriteString("OpenGenericType{")
	typ.Type.PrettyPrint(builder)
	builder.WriteString("}")
}

func (typ *PtrType) PrettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("ptr(")
	typ.Target.PrettyPrint(builder)
	builder.WriteString(")")
}

func (lit *IntLitType) PrettyPrint(builder *AstPrettyPrinter) {
	WriteIntLit(&builder.Builder, lit.Value)
}

func (lit *FloatLitType) PrettyPrint(builder *AstPrettyPrinter) {
	fmt.Fprintf(builder, "%f", lit.Value)
}

func (doc DocComment) PrettyPrint(builder *AstPrettyPrinter) {
	for _, line := range doc.BaseDoc {
		builder.NewlineAndIndent()
		builder.WriteString("## ")
		builder.WriteString(line)
	}
	for _, section := range doc.NamedDocSections {
		section.PrettyPrint(builder)
	}
	// ensure that the following expression won't be commentified
	builder.NewlineAndIndent()
}
