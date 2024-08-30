package main

import "math/big"

// an overview all all nods that are allowed in an untyped AST emitted directly from the parser

type AstNode interface {
	PrettyPrint(*AstPrettyPrinter)
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

type DocComment struct {
	Source           string
	BaseDoc          DocLines
	NamedDocSections []NamedDocSection
}

// most simple AST node
type Ident struct {
	// the abstract source field is the identifier value
	Source  string
	Comment DocLines
}

type InvalidTokenExpr struct {
	token Token
}

// an expression tagget to be a type expression
type TypeExpr Expr

type StructField struct {
	Source   string
	Name     Ident
	TypeExpr TypeExpr
}

type VariableDefStmt struct {
	Source   string
	Kind     SymbolKind // only SkVar SkLet SkConst allowed
	Name     Ident
	TypeExpr TypeExpr // optional
	Value    Expr
}

type ForLoopStmt struct {
	Source     string
	LoopIdent  Ident
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

type BreakStmt struct {
	Source string
}

type ContinueStmt struct {
	Source string
}

type TypeContext struct { // type <Expr>
	Source string
	Expr   TypeExpr
}

type ReturnExpr struct { // return <Expr>
	Source string
	Value  Expr
}

type VarExpr struct {
	Source string
	Expr   Expr
}

type EnumDef struct { // type MyEnum enum <Expr>
	Source      string
	Name        Ident
	Values      []Ident
	Annotations StrLit
}

type StructDef struct {
	Source      string
	Name        Ident
	Fields      []ColonExpr
	Annotations StrLit
}

type TraitDef struct {
	Source         string
	Name           Ident
	DependentTypes []Ident
	Signatures     []*ProcDef
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
	// other properties (TODO can this be removed?)
	Braced bool // true for (a+b) +(a,b), false for a+b
	Prefix bool // true for -a ++x etc, false for everything else
}

type ColonExpr struct {
	Source string
	Lhs    Expr
	Rhs    TypeExpr
}

type ProcDef struct {
	Source      string
	Expr        Expr
	Annotations StrLit
	DocComment  DocComment
}

type PackageDef struct {
	Source        string
	Name          string
	WorkDir       string
	TopLevelStmts []Expr
}

type StaticExpr struct { // static <Expr>
	Source string
	Expr   Expr
}

type ImportStmt struct {
	Source string
	Value  StrLit
}

type EmitStmt struct {
	Source string
	Value  StrLit
}

func (arg InvalidTokenExpr) GetSource() string { return arg.token.value }
func (arg ExprList) GetSource() string         { return arg.Source }
func (arg ProcDef) GetSource() string          { return arg.Source }
func (arg StructDef) GetSource() string        { return arg.Source }
func (arg EnumDef) GetSource() string          { return arg.Source }
func (arg DocComment) GetSource() string       { return arg.Source }
func (arg NamedDocSection) GetSource() string  { return arg.Source }
func (arg Ident) GetSource() string            { return arg.Source }
func (arg CodeBlock) GetSource() string        { return arg.Source }
func (arg StrLit) GetSource() string           { return arg.Source }
func (arg IntLit) GetSource() string           { return arg.Source }
func (arg FloatLit) GetSource() string         { return arg.Source }
func (arg ArrayLit) GetSource() string         { return arg.Source }
func (arg Call) GetSource() string             { return arg.Source }
func (arg ColonExpr) GetSource() string        { return arg.Source }
func (arg VariableDefStmt) GetSource() string  { return arg.Source }
func (arg ForLoopStmt) GetSource() string      { return arg.Source }
func (arg WhileLoopStmt) GetSource() string    { return arg.Source }
func (arg IfExpr) GetSource() string           { return arg.Source }
func (arg IfElseExpr) GetSource() string       { return arg.Source }
func (arg ReturnExpr) GetSource() string       { return arg.Source }
func (arg VarExpr) GetSource() string          { return arg.Source }
func (arg BreakStmt) GetSource() string        { return arg.Source }
func (arg ContinueStmt) GetSource() string     { return arg.Source }
func (arg TypeContext) GetSource() string      { return arg.Source }
func (arg NilLit) GetSource() string           { return arg.Source }
func (arg EmitStmt) GetSource() string         { return arg.Source }
func (arg StaticExpr) GetSource() string       { return arg.Source }
func (arg ImportStmt) GetSource() string       { return arg.Source }
func (arg *TraitDef) GetSource() string        { return arg.Source }
func (arg PackageDef) GetSource() string       { return arg.Source }
