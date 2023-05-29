package main

// an overview all all nods that are allowed in an untyped AST emitted directly from the parser

type AstNode interface {
	prettyPrint(*AstPrettyPrinter)
	GetSource() string
}

type Expr interface {
	AstNode
	// get the substring of the original source string. This needs to be
	// a real substring to be convertible into line column
	// representation
	expression()
}

type AbstractAstNode struct {
	Source string
}

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

type TypeExpr struct {
	Source string
	// this type is a placeholder, it is supposed to become more complex
	Ident    Ident
	ExprArgs []Expr
	TypeArgs []TypeExpr
}

type StructField struct {
	Source   string
	Name     Ident
	TypeExpr TypeExpr
}

type VariableDefStmt struct {
	Source   string
	Kind     SymbolKind // only SkVar SkLet SkConst allowed
	Name     Ident
	TypeExpr TypeExpr
	Value    Expr
}

type ForLoopStmt struct {
	Source     string
	LoopIdent  Ident
	Collection Expr
	Body       Expr
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

type ReturnStmt struct {
	Source string
	Value  Expr
}

// type TypeDef struct {
// 	Source string
// 	Name   Ident
// 	Kind   Ident
// 	Body   CodeBlock
// }

type EnumDef struct {
	Source string
	Name   Ident
	Values []Ident
}

type StructDef struct {
	Source string
	Name   Ident
	Fields []ColonExpr
}

type ProcArgument struct {
	Source string
	Name   Ident
	Type   TypeExpr
}

type CodeBlock struct {
	Source string
	Items  []Expr
}

type StrLit struct {
	Source string
	Value  string
}

type CharLit struct {
	Source string
	Rune   rune
}

type IntLit struct {
	Source string
	typ    *BuiltinType
	Value  int64
}

type FloatLit struct {
	Source string
	typ    *BuiltinType
	Value  float64
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
	Source     string
	Name       Ident
	Args       []ProcArgument
	ResultType TypeExpr
	Body       Expr
}

type PackageDef struct {
	Source        string
	Name          string
	TopLevelStmts []Expr
}

func (_ ProcDef) expression()         {}
func (_ StructDef) expression()       {}
func (_ EnumDef) expression()         {}
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

func (arg ProcDef) GetSource() string         { return arg.Source }
func (arg StructDef) GetSource() string       { return arg.Source }
func (arg EnumDef) GetSource() string         { return arg.Source }
func (arg DocComment) GetSource() string      { return arg.Source }
func (arg NamedDocSection) GetSource() string { return arg.Source }
func (arg Ident) GetSource() string           { return arg.Source }
func (arg CodeBlock) GetSource() string       { return arg.Source }
func (arg StrLit) GetSource() string          { return arg.Source }
func (arg IntLit) GetSource() string          { return arg.Source }
func (arg FloatLit) GetSource() string        { return arg.Source }
func (arg ArrayLit) GetSource() string        { return arg.Source }
func (arg CharLit) GetSource() string         { return arg.Source }
func (arg Call) GetSource() string            { return arg.Source }
func (arg ColonExpr) GetSource() string       { return arg.Source }
func (arg VariableDefStmt) GetSource() string { return arg.Source }
func (arg ForLoopStmt) GetSource() string     { return arg.Source }
func (arg IfExpr) GetSource() string          { return arg.Source }
func (arg IfElseExpr) GetSource() string      { return arg.Source }
func (arg ReturnStmt) GetSource() string      { return arg.Source }
func (arg BreakStmt) GetSource() string       { return arg.Source }
func (arg ContinueStmt) GetSource() string    { return arg.Source }
func (arg TypeExpr) GetSource() string        { return arg.Source }

func (arg PackageDef) GetSource() string { return arg.Source }
