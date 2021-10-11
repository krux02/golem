package main

// an overview all all nods that are allowed in an untyped AST emitted directly from the parser

type Expr interface {
	String() string
	prettyPrint(*AstPrettyPrinter)
	expression()
}

type TypeExpr struct {
	Ident string
}

type StructField struct {
	Name string
	Type TypeExpr
}

type StructDef struct {
	Name   string
	Fields []StructField
}

type ProcArgument struct {
	Name string
	Type TypeExpr
}

type CodeBlock struct {
	Items []Expr
}

type Symbol struct {
	Name              string
	OperatorPrecedence int
}

type StrLit struct {
	Val string
}

type Call struct {
	Sym  Symbol
	Args []Expr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type ProcDef struct {
	Name       string
	Args       []ProcArgument
	ResultType TypeExpr
	Body       Expr
}

type PackageDef struct {
	Name     string
	TypeDefs []StructDef
	ProcDefs []ProcDef
}

func (sym Symbol) expression() {}
func (block CodeBlock) expression() {}
func (lit StrLit) expression() {}
func (call Call) expression() {}
