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
	builder.WriteString(sym.Value)
}

func (call Call) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString(call.Sym.Value)
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
	builder.NewlineAndIndent()
	iLast := len(codeBlock.Items) - 1
	for i, item := range codeBlock.Items {
		item.prettyPrint(builder)
		builder.WriteString(";")
		if i == iLast {
			builder.Indentation--
		}
		builder.NewlineAndIndent()
	}
	builder.WriteString("}")
	builder.NewlineAndIndent()
}

func (stmt ReturnStmt) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteString("return ")
	stmt.Expr.prettyPrint(builder)
	builder.WriteString(";\n")
}

func (lit StrLit) prettyPrint(builder *AstPrettyPrinter) {
	builder.WriteRune('"')
	for _, rune := range lit.Val {
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
		field.Type.prettyPrint(builder)
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

func (sym Symbol) String() string {
	return sym.Value
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

func (stmt ReturnStmt) String() string {
	builder := &AstPrettyPrinter{}
	stmt.prettyPrint(builder)
	return builder.String()
}

func (lit StrLit) String() string {
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
