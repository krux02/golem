package main

// an overview all all nods that are allowed in an untyped AST emitted directly from the parser

type AstNode interface {
	prettyPrint(*AstPrettyPrinter)
	// TODO add something here to access original source string.
}

type Expr interface {
	AstNode
	expression()
}

type TypeExpr struct {
	// this type is a placeholder, it is supposed to become more complex
	Ident string
}

type StructField struct {
	Name     string
	TypeExpr TypeExpr
}

type LetStmt struct {
	Name     string
	TypeExpr TypeExpr
	Value    Expr
}

type BreakStmt struct {
	Source string
}

type ContinueStmt struct {
	Source string
}

type ReturnStmt struct {
	Value Expr
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

type Ident struct {
	Name string
}

type StrLit struct {
	Value string
}

type IntLit struct {
	Value int
}

type FloatLit struct {
	Value float64
}

type ArrayLit struct {
	Items []Expr
}

type Call struct {
	Sym  Ident
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
	Globals  []LetStmt
	TypeDefs []StructDef
	ProcDefs []ProcDef
}

func (ident Ident) expression()               {}
func (block CodeBlock) expression()           {}
func (lit StrLit) expression()                {}
func (lit IntLit) expression()                {}
func (lit FloatLit) expression()              {}
func (lit ArrayLit) expression()              {}
func (call Call) expression()                 {}
func (letstmt LetStmt) expression()           {}
func (returnstmt ReturnStmt) expression()     {}
func (breakstmt BreakStmt) expression()       {}
func (continuestmt ContinueStmt) expression() {}
