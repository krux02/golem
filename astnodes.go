package main

// an overview all all nods that are allowed in an untyped AST emitted directly from the parser

type AstNode interface {
	prettyPrint(*AstPrettyPrinter)
	Source() string
	// TODO add something here to access original source string.
}

type Expr interface {
	AstNode
	// get the substring of the original source string. This needs to be
	// a real substring to be convertible into line column
	// representation
	expression()
}

type AbstractAstNode struct {
	source string
}

func (astNode AbstractAstNode) Source() string {
	return astNode.source
}

type TypeExpr struct {
	AbstractAstNode
	// this type is a placeholder, it is supposed to become more complex
	Ident string
}

type StructField struct {
	AbstractAstNode
	Name     string
	TypeExpr TypeExpr
}

type VariableDefStmt struct {
	AbstractAstNode
	Kind     SymbolKind // only SkVar SkLet SkConst allowed
	Name     string
	TypeExpr TypeExpr
	Value    Expr
}

type ForLoopStmt struct {
	AbstractAstNode
	LoopIdent  Ident
	Collection Expr
	Body       Expr
}

type IfStmt struct {
	AbstractAstNode
	Condition Expr
	Body      Expr
}

type IfElseStmt struct {
	AbstractAstNode
	Condition Expr
	Body      Expr
	Else      Expr
}

type BreakStmt struct {
	AbstractAstNode
}

type ContinueStmt struct {
	AbstractAstNode
}

type ReturnStmt struct {
	AbstractAstNode
	Value Expr
}

type StructDef struct {
	AbstractAstNode
	Name   string
	Fields []StructField
}

type ProcArgument struct {
	AbstractAstNode
	Name string
	Type TypeExpr
}

type CodeBlock struct {
	AbstractAstNode
	Items []Expr
}

type Ident struct {
	AbstractAstNode
	Name string
}

type StrLit struct {
	AbstractAstNode
	Value string
}

type CharLit struct {
	AbstractAstNode
	Rune rune
}

type IntLit struct {
	AbstractAstNode
	Value int
}

type FloatLit struct {
	AbstractAstNode
	Value float64
}

type ArrayLit struct {
	AbstractAstNode
	Items []Expr
}

type Call struct {
	AbstractAstNode
	Callee Expr
	Args []Expr
	// other properties (TODO can this be removed?)
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type ProcDef struct {
	AbstractAstNode
	Name       string
	Args       []ProcArgument
	ResultType TypeExpr
	Body       Expr
}

type PackageDef struct {
	AbstractAstNode
	Name     string
	Globals  []VariableDefStmt
	TypeDefs []StructDef
	ProcDefs []ProcDef
}

func (ident Ident) expression()          {}
func (block CodeBlock) expression()      {}
func (lit StrLit) expression()           {}
func (lit IntLit) expression()           {}
func (lit FloatLit) expression()         {}
func (lit ArrayLit) expression()         {}
func (lit CharLit) expression()          {}
func (call Call) expression()            {}
func (stmt VariableDefStmt) expression() {}
func (stmt ForLoopStmt) expression()     {}
func (stmt IfStmt) expression()          {}
func (stmt IfElseStmt) expression()      {}
func (stmt ReturnStmt) expression()      {}
func (stmt BreakStmt) expression()       {}
func (stmt ContinueStmt) expression()    {}
