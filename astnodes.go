package main

// an overview all all nods that are allowed in an untyped AST emitted directly from the parser

type AstNode interface {
	prettyPrint(*AstPrettyPrinter)
	Source() string
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

// all lines from a documentation comment
type DocLines []string

type NamedDocSection struct {
	AbstractAstNode
	Name  string
	Lines DocLines
}

type DocComment struct {
	AbstractAstNode
	BaseDoc          DocLines
	NamedDocSections []NamedDocSection
}

// most simple AST node
type Ident struct {
	// the abstract source field is the identifier value
	AbstractAstNode
	Comment DocLines
}

type TypeExpr struct {
	AbstractAstNode
	// this type is a placeholder, it is supposed to become more complex
	Ident    Ident
	ExprArgs []Expr
	TypeArgs []TypeExpr
}

type StructField struct {
	AbstractAstNode
	Name     Ident
	TypeExpr TypeExpr
}

type VariableDefStmt struct {
	AbstractAstNode
	Kind     SymbolKind // only SkVar SkLet SkConst allowed
	Name     Ident
	TypeExpr TypeExpr
	Value    Expr
}

type ForLoopStmt struct {
	AbstractAstNode
	LoopIdent  Ident
	Collection Expr
	Body       Expr
}

type IfExpr struct {
	AbstractAstNode
	Condition Expr
	Body      Expr
}

type IfElseExpr struct {
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
	Name   Ident
	Fields []StructField
}

type ProcArgument struct {
	AbstractAstNode
	Name Ident
	Type TypeExpr
}

type CodeBlock struct {
	AbstractAstNode
	Items []Expr
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
	typ   *BuiltinType
	Value int64
}

type FloatLit struct {
	AbstractAstNode
	typ   *BuiltinType
	Value float64
}

type ArrayLit struct {
	AbstractAstNode
	Items []Expr
}

type Call struct {
	AbstractAstNode
	Callee Expr
	Args   []Expr
	// other properties (TODO can this be removed?)
	Braced bool // true for (a+b) +(a,b), false for a+b
	Prefix bool // true for -a ++x etc, false for everything else
}

type ColonExpr struct {
	AbstractAstNode
	Lhs Expr
	Rhs TypeExpr
}

type ProcDef struct {
	AbstractAstNode
	Name       Ident
	Args       []ProcArgument
	ResultType TypeExpr
	Body       Expr
}

type PackageDef struct {
	AbstractAstNode
	Name          string
	TopLevelStmts []Expr
	// Globals  []VariableDefStmt
	// TypeDefs []StructDef
	// ProcDefs []ProcDef
}

func (_ ProcDef) expression()         {}
func (_ StructDef) expression()       {}
func (_ DocComment) expression()      {}
func (_ Ident) expression()           {}
func (_ CodeBlock) expression()       {}
func (_ StrLit) expression()          {}
func (_ IntLit) expression()          {}
func (_ FloatLit) expression()        {}
func (_ ArrayLit) expression()        {}
func (_ CharLit) expression()         {}
func (_ Call) expression()            {}
func (_ ColonExpr) expression()       {}
func (_ VariableDefStmt) expression() {}
func (_ ForLoopStmt) expression()     {}
func (_ IfExpr) expression()          {}
func (_ IfElseExpr) expression()      {}
func (_ ReturnStmt) expression()      {}
func (_ BreakStmt) expression()       {}
func (_ ContinueStmt) expression()    {}
