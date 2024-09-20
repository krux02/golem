package main

import (
	"math/big"
)

// an overview all all nods that are allowed in an untyped AST emitted directly from the parser

type AstNode interface {
	PrettyPrint(*AstPrettyPrinter)
	// Recursive substitute symbols
	RecSubSyms(substitutions []TemplateSubstitution) Expr
	GetSource() string
}

type Expr = AstNode

// all lines from a documentation comment
type DocLines []string

type NamedDocSection struct {
	Source string
	Name   string
	Lines  DocLines
}

// the type of doc comment that precedes a statement.
type PrefixDocComment struct {
	Source           string
	BaseDoc          DocLines
	NamedDocSections []*NamedDocSection
}

// most simple AST node
type Ident struct {
	// the abstract source field is the identifier value
	Source  string
	Comment DocLines
}

type InvalidTokenExpr struct {
	Source string
	kind   TokenKind
}

// an expression tagget to be a type expression
type TypeExpr Expr

type VariableDefStmt struct {
	Source string
	Prefix Ident // var, let, const ...
	Expr   Expr  // everything else
}

type ForLoopStmt struct {
	Source     string
	LoopIdent  *Ident
	Collection Expr
	Body       Expr
}

type WhileLoopStmt struct {
	Source    string
	Condition Expr
	Body      Expr
}

type IfExpr struct {
	Source    string
	Condition Expr
	Body      Expr
}

type IfElseExpr struct {
	Source    string
	Condition Expr
	Body      Expr
	Else      Expr
}

type TypeContext struct {
	Source string
	Expr   TypeExpr
}

type ReturnExpr struct {
	Source string
	Value  Expr
}

type VarExpr struct {
	Source string
	Expr   Expr
}

type TypeDef struct {
	Source      string
	Expr        Expr
	Annotations *StrLit
}

type CodeBlock struct {
	Source string
	Items  []Expr
}

type StrLit struct {
	Source string
	Value  string
}

type IntLit struct {
	Source string
	Value  *big.Int
}

type FloatLit struct {
	Source string
	Value  *big.Float
}

type NilLit struct {
	Source string
	Type   Type
}

type ExprList struct {
	Source string
	Items  []Expr
}

type ArrayLit struct {
	Source string
	Items  []Expr
}

type Call struct {
	Source string
	Callee Expr
	Args   []Expr
	// `Braced` true for (a+b) +(a,b), false for a+b. This property is used to
	// check if applyOperatorPrecedence may rehang this ast.
	Braced bool
	Prefix bool // true for -a ++x etc, false for everything else
}

type BracketExpr struct {
	Source string
	Callee Expr
	Args   []Expr
}

type ProcDef struct {
	Source      string
	Expr        Expr
	Annotations *StrLit
	DocComment  *PrefixDocComment
}

type PackageDef struct {
	Source        string
	Name          string
	WorkDir       string
	TopLevelStmts []Expr
}

type BreakStmt struct {
	Source string
}

type ContinueStmt struct {
	Source string
}

type TraitDef struct {
	Source string
	Expr   Expr
}

type StaticExpr struct {
	Source string
	Expr   Expr
}

func (arg *InvalidTokenExpr) GetSource() string { return arg.Source }
func (arg *ExprList) GetSource() string         { return arg.Source }
func (arg *ProcDef) GetSource() string          { return arg.Source }
func (arg *TypeDef) GetSource() string          { return arg.Source }
func (arg *PrefixDocComment) GetSource() string { return arg.Source }
func (arg *NamedDocSection) GetSource() string  { return arg.Source }
func (arg *Ident) GetSource() string            { return arg.Source }
func (arg *CodeBlock) GetSource() string        { return arg.Source }
func (arg *StrLit) GetSource() string           { return arg.Source }
func (arg *IntLit) GetSource() string           { return arg.Source }
func (arg *FloatLit) GetSource() string         { return arg.Source }
func (arg *ArrayLit) GetSource() string         { return arg.Source }
func (arg *Call) GetSource() string             { return arg.Source }
func (arg *BracketExpr) GetSource() string      { return arg.Source }
func (arg *VariableDefStmt) GetSource() string  { return arg.Source }
func (arg *ForLoopStmt) GetSource() string      { return arg.Source }
func (arg *WhileLoopStmt) GetSource() string    { return arg.Source }
func (arg *IfExpr) GetSource() string           { return arg.Source }
func (arg *IfElseExpr) GetSource() string       { return arg.Source }
func (arg *ReturnExpr) GetSource() string       { return arg.Source }
func (arg *VarExpr) GetSource() string          { return arg.Source }
func (arg *BreakStmt) GetSource() string        { return arg.Source }
func (arg *ContinueStmt) GetSource() string     { return arg.Source }
func (arg *TypeContext) GetSource() string      { return arg.Source }
func (arg *NilLit) GetSource() string           { return arg.Source }
func (arg *StaticExpr) GetSource() string       { return arg.Source }
func (arg *TraitDef) GetSource() string         { return arg.Source }
func (arg *PackageDef) GetSource() string       { return arg.Source }
