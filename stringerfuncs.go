package main

import (
	"fmt"
	"strings"
)

/// TOKENS

type AstPrettyPrinter struct {
	strings.Builder
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
}

func (builder *AstPrettyPrinter) WriteAstNode(node AstNode) {
	node.prettyPrint(builder)
}

func AstFormat(node AstNode) string {
	builder := &AstPrettyPrinter{}
	builder.WriteAstNode(node)
	return builder.String()
}

func (ident Ident) prettyPrint(builder *AstPrettyPrinter) {
	// the only exception where pretty Print may print the original
	// source.
	builder.WriteString(ident.source)
}

func (call Call) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteAstNode(call.Callee)
	builder.WriteString("(")
	for i, arg := range call.Args {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteAstNode(arg)
	}
	builder.WriteString(")")
}

func (expr ColonExpr) prettyPrint(builder *AstPrettyPrinter) {
	expr.Lhs.prettyPrint(builder)
	builder.WriteString(": ")
	expr.Rhs.prettyPrint(builder)
}

func (codeBlock CodeBlock) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("{")
	builder.Indentation++
	for _, item := range codeBlock.Items {
		builder.NewlineAndIndent()
		builder.WriteAstNode(item)
		builder.WriteString(";")
	}
	builder.Indentation--
	if len(codeBlock.Items) > 0 {
		builder.NewlineAndIndent()
	}
	builder.WriteString("}")
}

func (lit CharLit) prettyPrint(builder *AstPrettyPrinter) {
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

func (lit StrLit) prettyPrint(builder *AstPrettyPrinter) {
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

func (lit ArrayLit) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteAstNode(expr)
	}
	builder.WriteRune(']')
}

func (lit TcArrayLit) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteAstNode(expr)
	}
	builder.WriteRune(']')
}

func (lit TcStructLit) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteAstNode(expr)
	}
	builder.WriteString("]:")
	//builder.WriteAstNode(lit.typ)
	builder.WriteString(lit.typ.Name)
}

func (lit TcEnumSetLit) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('[')
	for i, expr := range lit.Items {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteAstNode(expr)
	}
	builder.WriteString("]: set[")
	//builder.WriteAstNode(lit.typ)
	builder.WriteString(lit.ElemType.Name)
	builder.WriteRune(']')
}

func WriteIntLit(builder *strings.Builder, value int64) {
	if value == 0 {
		builder.WriteByte('0')
		return
	}
	if value < 0 {
		builder.WriteRune('-')
		value = -value
	}
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
}

func (lit IntLit) prettyPrint(builder *AstPrettyPrinter) {
	WriteIntLit(&builder.Builder, lit.Value)
	if lit.typ != nil {
		builder.WriteString(":")
		lit.typ.prettyPrint(builder)
	}
}

func (lit FloatLit) prettyPrint(builder *AstPrettyPrinter) {
	str := fmt.Sprintf("%f", lit.Value)
	builder.WriteString(str)
	if lit.typ != nil {
		builder.WriteString(":")
		lit.typ.prettyPrint(builder)
	}
}

func (typeExpr TypeExpr) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typeExpr.Ident.source)
	if len(typeExpr.ExprArgs) > 0 {
		builder.WriteString("(")
		for i, arg := range typeExpr.ExprArgs {
			if i != 0 {
				builder.WriteString(", ")
			}
			builder.WriteAstNode(arg)
		}
		builder.WriteString(")")
	}
	if len(typeExpr.TypeArgs) > 0 {
		builder.WriteString("[")
		for i, arg := range typeExpr.TypeArgs {
			if i != 0 {
				builder.WriteString(", ")
			}
			builder.WriteAstNode(arg)
		}
		builder.WriteString("]")
	}
}

func (typeDef TypeDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteAstNode(typeDef.Name)
	builder.WriteString(" = ")
	builder.WriteAstNode(typeDef.Kind)
	builder.WriteAstNode(typeDef.Body)
}

func (procDef ProcDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc ")
	builder.WriteAstNode(procDef.Name)
	builder.WriteString("(")
	if len(procDef.Args) > 3 {
		builder.Indentation += 2
		for _, arg := range procDef.Args {
			builder.NewlineAndIndent()
			builder.WriteAstNode(arg.Name)
			builder.WriteString(": ")
			builder.WriteAstNode(arg.Type)
		}
		builder.Indentation -= 2
		builder.NewlineAndIndent()

	} else {
		for i, arg := range procDef.Args {
			if i != 0 {
				builder.WriteString("; ")
			}
			builder.WriteAstNode(arg.Name)
			builder.WriteString(": ")
			builder.WriteAstNode(arg.Type)
		}
	}
	builder.WriteString("): ")
	builder.WriteAstNode(procDef.ResultType)
	builder.WriteString(" = ")
	builder.WriteAstNode(procDef.Body)
}

func (returnStmt ReturnStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("return ")
	builder.WriteAstNode(returnStmt.Value)
}

func (stmt VariableDefStmt) prettyPrint(builder *AstPrettyPrinter) {
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
	builder.WriteAstNode(stmt.Name)
	if stmt.TypeExpr.IsSet() {
		builder.WriteString(": ")
		builder.WriteAstNode(stmt.TypeExpr)
	}
	if stmt.Value != nil {
		builder.WriteString(" = ")
		builder.WriteAstNode(stmt.Value)
	}
}

func (loopStmt ForLoopStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("for ")
	builder.WriteAstNode(loopStmt.LoopIdent)
	builder.WriteString(" in ")
	builder.WriteAstNode(loopStmt.Collection)
	builder.WriteString(" do ")
	builder.WriteAstNode(loopStmt.Body)
}

func (loopStmt TcForLoopStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("for ")
	builder.WriteString(loopStmt.LoopSym.source)
	builder.WriteString(" in ")
	builder.WriteAstNode(loopStmt.Collection)
	builder.WriteString(" do ")
	builder.WriteAstNode(loopStmt.Body)
}

func (ifStmt IfExpr) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteAstNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteAstNode(ifStmt.Body)
}

func (ifStmt TcIfStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteAstNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteAstNode(ifStmt.Body)
}

func (ifStmt IfElseExpr) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteAstNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteAstNode(ifStmt.Body)
	builder.WriteString(" else ")
	builder.WriteAstNode(ifStmt.Else)
}

func (ifStmt TcIfElseExpr) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("if ")
	builder.WriteAstNode(ifStmt.Condition)
	builder.WriteString(" do ")
	builder.WriteAstNode(ifStmt.Body)
	builder.WriteString(" else ")
	builder.WriteAstNode(ifStmt.Else)
}

func (pak PackageDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("# file: ")
	builder.WriteString(pak.Name)
	for _, stmt := range pak.TopLevelStmts {
		builder.NewlineAndIndent()
		builder.WriteAstNode(stmt)
	}
}

func (breakstmt BreakStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("break")
}
func (continuestmt ContinueStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("continue")
}

// format type checked ast nodes

func (typ BuiltinType) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typ.name)
}

func (typ *ArrayType) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("array(")
	WriteIntLit(&builder.Builder, typ.Len)
	builder.WriteString(")[")
	builder.WriteAstNode(typ.Elem)
	builder.WriteString("]")
}

func (typ *EnumSetType) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("set[")
	builder.WriteAstNode(typ.Elem)
	builder.WriteString("]")
}

func (call TcCall) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(call.Sym.Name)
	builder.WriteString("(")
	for i, arg := range call.Args {
		if i != 0 {
			builder.WriteString(", ")
		}
		builder.WriteAstNode(arg)
	}
	builder.WriteString(")")
}

func (structDef *TcStructDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteString(structDef.Name)
	builder.WriteString(" = struct {")
	builder.Indentation++
	for _, field := range structDef.Fields {
		builder.NewlineAndIndent()
		builder.WriteString(field.Name)
		builder.WriteString(": ")
		builder.WriteAstNode(field.Type)
	}
	builder.Indentation--
	builder.NewlineAndIndent()
	builder.WriteString("}")
}

func (enumDef *TcEnumDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteString(enumDef.Name)
	builder.WriteString(" = struct {")
	builder.Indentation++
	for _, field := range enumDef.Values {
		builder.NewlineAndIndent()
		builder.WriteString(field.source)
	}
	builder.Indentation--
	builder.NewlineAndIndent()
	builder.WriteString("}")
}

func (codeBlock TcCodeBlock) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("{")
	builder.Indentation++
	for _, item := range codeBlock.Items {
		builder.NewlineAndIndent()
		builder.WriteAstNode(item)
		builder.WriteString(";")
	}
	builder.Indentation--
	if len(codeBlock.Items) > 0 {
		builder.NewlineAndIndent()
	}
	builder.WriteString("}")
}

func (sym TcSymbol) prettyPrint(printer *AstPrettyPrinter) {
	printer.WriteString(sym.source)
}

func (sym TcProcSymbol) prettyPrint(printer *AstPrettyPrinter) {
	printer.WriteString(sym.Name)
}

func (stmt TcVariableDefStmt) prettyPrint(builder *AstPrettyPrinter) {
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
	builder.WriteAstNode(stmt.Sym)
	builder.WriteString(": ")
	builder.WriteAstNode(stmt.Sym.Typ)
	// builder.WriteString(stmt.Sym.Typ.Name())
	if stmt.Value != nil {
		builder.WriteString(" = ")
		builder.WriteAstNode(stmt.Value)
	}
}

func (returnStmt TcReturnStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("return ")
	builder.WriteAstNode(returnStmt.Value)
}

func (expr TcDotExpr) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteAstNode(expr.Lhs)
	builder.WriteByte('.')
	builder.WriteString(expr.Rhs.Name)
}

func (procDef TcProcDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc ")
	builder.WriteString(procDef.Name)
	builder.WriteString("(")
	if len(procDef.Args) > 3 {
		builder.Indentation += 2
		builder.NewlineAndIndent()
		iLast := len(procDef.Args) - 1
		for i, arg := range procDef.Args {
			builder.WriteString(arg.source)
			builder.WriteString(": ")
			builder.WriteAstNode(arg.Type())
			if i == iLast {
				builder.Indentation -= 2
			}
			builder.NewlineAndIndent()
		}

	} else {
		for i, arg := range procDef.Args {
			if i != 0 {
				builder.WriteString("; ")
			}
			builder.WriteString(arg.source)
			builder.WriteString(": ")
			builder.WriteAstNode(arg.Type())
		}
	}
	builder.WriteString("): ")
	builder.WriteAstNode(procDef.ResultType)
	builder.WriteString(" = ")
	builder.WriteAstNode(procDef.Body)
}

func (pak TcPackageDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("# file: ")
	builder.WriteString(pak.Name)
	for _, typ := range pak.EnumDefs {
		builder.NewlineAndIndent()
		builder.WriteAstNode(typ)
	}
	for _, typ := range pak.StructDefs {
		builder.NewlineAndIndent()
		builder.WriteAstNode(typ)
	}
	for _, proc := range pak.ProcDefs {
		builder.NewlineAndIndent()
		builder.WriteAstNode(proc)
	}
}

func (section NamedDocSection) prettyPrint(builder *AstPrettyPrinter) {
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

func (doc DocComment) prettyPrint(builder *AstPrettyPrinter) {
	for _, line := range doc.BaseDoc {
		builder.NewlineAndIndent()
		builder.WriteString("## ")
		builder.WriteString(line)
	}
	for _, section := range doc.NamedDocSections {
		section.prettyPrint(builder)
	}
	// ensure that the following expression won't be commentified
	builder.NewlineAndIndent()
}
