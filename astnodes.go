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

type VarStmt struct {
	Name     string
	TypeExpr TypeExpr
	Value    Expr
}

type ConstStmt struct {
	Name     string
	TypeExpr TypeExpr
	Value    Expr
}

type ForLoopStmt struct {
	LoopIdent  Ident
	Collection Expr
	Body       CodeBlock
}

type IfStmt struct {
	Condition Expr
	Body      CodeBlock
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

type CharLit struct {
	Rune rune
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

func (ident Ident) expression()       {}
func (block CodeBlock) expression()   {}
func (lit StrLit) expression()        {}
func (lit IntLit) expression()        {}
func (lit FloatLit) expression()      {}
func (lit ArrayLit) expression()      {}
func (lit CharLit) expression()       {}
func (call Call) expression()         {}
func (stmt LetStmt) expression()      {}
func (stmt VarStmt) expression()      {}
func (stmt ConstStmt) expression()    {}
func (stmt ForLoopStmt) expression()  {}
func (stmt IfStmt) expression()       {}
func (stmt ReturnStmt) expression()   {}
func (stmt BreakStmt) expression()    {}
func (stmt ContinueStmt) expression() {}
