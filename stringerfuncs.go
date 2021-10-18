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
	builder.WriteString(ident.Name)
}

func (call Call) prettyPrint(builder *AstPrettyPrinter) {
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
	for _, field := range structDef.Fields {
		builder.NewlineAndIndent()
		builder.WriteString(field.Name)
		builder.WriteString(": ")
		builder.WriteAstNode(field.TypeExpr)
	}
	builder.Indentation--
	builder.NewlineAndIndent()
	builder.WriteString("}")
}

func (procDef ProcDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("proc ")
	builder.WriteString(procDef.Name)
	builder.WriteString("(")
	if len(procDef.Args) > 3 {
		builder.Indentation += 2
		for _, arg := range procDef.Args {
			builder.NewlineAndIndent()
			builder.WriteString(arg.Name)
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
			builder.WriteString(arg.Name)
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

func (letStmt LetStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("let ")
	builder.WriteString(letStmt.Name)
	if letStmt.TypeExpr.IsSet() {
		builder.WriteString(":")
		builder.WriteAstNode(letStmt.TypeExpr)
	}
	builder.WriteString(" = ")
	builder.WriteAstNode(letStmt.Value)
}

func (pak PackageDef) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(pak.Name)
	for _, typ := range pak.TypeDefs {
		builder.NewlineAndIndent()
		builder.WriteAstNode(typ)
	}
	for _, proc := range pak.ProcDefs {
		builder.NewlineAndIndent()
		builder.WriteAstNode(proc)
	}
}

// format type checked ast nodes

func (typ BuiltinType) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(typ.name)
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

func (structDef TcStructDef) prettyPrint(builder *AstPrettyPrinter) {
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

func (sym TcLetSymbol) prettyPrint(printer *AstPrettyPrinter) {
	printer.WriteString(sym.Name)
}

func (letStmt TcLetStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("let ")
	builder.WriteAstNode(letStmt.Sym)
	builder.WriteString(":")
	builder.WriteString(letStmt.Sym.Typ.Name())
	builder.WriteString(" = ")
	builder.WriteAstNode(letStmt.Value)
}

func (returnStmt TcReturnStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("return ")
	builder.WriteAstNode(returnStmt.Value)
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
			builder.WriteString(arg.Name)
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
			builder.WriteString(arg.Name)
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
	builder.WriteString(pak.Name)
	for _, typ := range pak.TypeDefs {
		builder.NewlineAndIndent()
		builder.WriteAstNode(typ)
	}
	for _, proc := range pak.ProcDefs {
		builder.NewlineAndIndent()
		builder.WriteAstNode(proc)
	}
}
