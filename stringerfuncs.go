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

func (builder *AstPrettyPrinter) NewlineAndIndent() {
	builder.WriteRune('\n')
	N := builder.Indentation
	for i := 0; i < N; i++ {
		builder.WriteString("  ")
	}
}

func (this TokenKind) String() string {
	return TokenKindNames[this]
}

func (this Token) String() string {
	return fmt.Sprintf("(%s %q)", this.kind, this.value)
}

/// AST NODES

func (sym Symbol) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(sym.Name)
}

func (call Call) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(call.Sym.Name)
	builder.WriteString("(")
	for i, arg := range call.Args {
		if i != 0 {
			builder.WriteString(", ")
		}
		arg.prettyPrint(builder)
	}
	builder.WriteString(")")
}

func (codeBlock CodeBlock) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("{")
	builder.Indentation++
	for _, item := range codeBlock.Items {
		builder.NewlineAndIndent()
		item.prettyPrint(builder)
		builder.WriteString(";")
	}
	builder.Indentation--
	builder.NewlineAndIndent()
	builder.WriteString("}")
	builder.NewlineAndIndent()
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

func WriteIntLit(builder *strings.Builder, value int) {
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
}

func (typeExpr TypeExpr) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typeExpr.Ident)
}

func (structDef StructDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("type ")
	builder.WriteString(structDef.Name)
	builder.WriteString(" = struct {")
	builder.Indentation++
	builder.NewlineAndIndent()
	iLast := len(structDef.Fields) - 1
	for i, field := range structDef.Fields {
		builder.WriteString(field.Name)
		builder.WriteString(" : ")
		field.TypeExpr.prettyPrint(builder)
		if i == iLast {
			builder.Indentation--
		}
		builder.NewlineAndIndent()
	}
	builder.WriteString("}")
	builder.NewlineAndIndent()
}

func (procDef ProcDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc ")
	builder.WriteString(procDef.Name)
	builder.WriteString("(")
	if len(procDef.Args) > 3 {
		builder.Indentation += 2
		builder.NewlineAndIndent()
		iLast := len(procDef.Args) - 1
		for i, arg := range procDef.Args {
			builder.WriteString(arg.Name)
			builder.WriteString(": ")
			arg.Type.prettyPrint(builder)
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
			builder.WriteString(arg.Name)
			builder.WriteString(": ")
			arg.Type.prettyPrint(builder)
		}
	}
	builder.WriteString("): ")
	procDef.ResultType.prettyPrint(builder)
	builder.WriteString(" = ")
	procDef.Body.prettyPrint(builder)
}

func (returnStmt ReturnStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("return ")
	returnStmt.Value.prettyPrint(builder)
}

func (letStmt LetStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.NewlineAndIndent()
	builder.WriteString("let ")
	builder.WriteString(letStmt.Name)
	if letStmt.TypeExpr.IsSet() {
		builder.WriteString(":")
		letStmt.TypeExpr.prettyPrint(builder)
	}
	builder.WriteString(" = ")
	letStmt.Value.prettyPrint(builder)
}

func (sym Symbol) String() string {
	return sym.Name
}

func (codeBlock CodeBlock) String() string {
	builder := &AstPrettyPrinter{}
	codeBlock.prettyPrint(builder)
	result := builder.String()
	return result
}

func (call Call) String() string {
	builder := &AstPrettyPrinter{}
	call.prettyPrint(builder)
	return builder.String()
}

func (call TcCall) prettyPrint(*AstPrettyPrinter) {
	panic("not implemented")
}

func (call TcCodeBlock) prettyPrint(*AstPrettyPrinter) {
	panic("not implemented")
}

func (sym TcLetSymbol) prettyPrint(printer *AstPrettyPrinter) {
	printer.WriteString(sym.Name)
}

func (letStmt TcLetStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.NewlineAndIndent()
	builder.WriteString("let ")
	letStmt.Sym.prettyPrint(builder)
	builder.WriteString(":")
	builder.WriteString(letStmt.Sym.Typ.Name())
	builder.WriteString(" = ")
	letStmt.Value.prettyPrint(builder)
}

func (returnStmt TcReturnStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.NewlineAndIndent()
	builder.WriteString("return ")
	returnStmt.Value.prettyPrint(builder)
}

func (sym TcLetSymbol) String() string {
	builder := &AstPrettyPrinter{}
	sym.prettyPrint(builder)
	return builder.String()
}

func (codeBlock TcCodeBlock) String() string {
	builder := &AstPrettyPrinter{}
	codeBlock.prettyPrint(builder)
	return builder.String()
}

func (call TcCall) String() string {
	builder := &AstPrettyPrinter{}
	call.prettyPrint(builder)
	return builder.String()
}

func (lit StrLit) String() string {
	builder := &AstPrettyPrinter{}
	lit.prettyPrint(builder)
	return builder.String()
}

func (lit IntLit) String() string {
	builder := &AstPrettyPrinter{}
	lit.prettyPrint(builder)
	return builder.String()
}

func (structDef StructDef) String() string {
	builder := &AstPrettyPrinter{}
	structDef.prettyPrint(builder)
	return builder.String()
}

func (procDef ProcDef) String() string {
	builder := &AstPrettyPrinter{}
	procDef.prettyPrint(builder)
	return builder.String()
}

func (letStmt LetStmt) String() string {
	builder := &AstPrettyPrinter{}
	letStmt.prettyPrint(builder)
	return builder.String()
}

func (letStmt TcLetStmt) String() string {
	builder := &AstPrettyPrinter{}
	letStmt.prettyPrint(builder)
	return builder.String()
}

func (returnStmt ReturnStmt) String() string {
	builder := &AstPrettyPrinter{}
	returnStmt.prettyPrint(builder)
	return builder.String()
}

func (returnStmt TcReturnStmt) String() string {
	builder := &AstPrettyPrinter{}
	returnStmt.prettyPrint(builder)
	return builder.String()
}
