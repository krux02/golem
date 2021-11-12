package main

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	AstNode
	typenode()
}

type TcExpr interface {
	AstNode
	Expr
	Type() Type
}

type TcStructField struct {
	AbstractAstNode
	Name string
	Type Type
}

type TcStructDef struct {
	AbstractAstNode
	Name   string
	Fields []TcStructField
}

type TcCodeBlock struct {
	AbstractAstNode
	Items []TcExpr
}

// TODO unify TcProcSym with the othe symbol types

type TcProcSymbol struct {
	AbstractAstNode
	Name string
	Impl *TcProcDef
}

type SymbolKind int

const (
	SkLet SymbolKind = iota
	SkVar
	SkConst
	SkProcArg
	SkLoopIterator
)

type TcSymbol struct {
	AbstractAstNode
	Name string
	Kind SymbolKind
	Typ  Type
}

type TcForLoopStmt struct {
	AbstractAstNode
	LoopSym    TcSymbol
	Collection TcExpr
	Body       TcExpr
}

type TcIfStmt struct {
	AbstractAstNode
	Condition TcExpr
	Body      TcExpr
}

type TcIfElseStmt struct {
	AbstractAstNode
	Condition TcExpr
	Body      TcExpr
	Else      TcExpr
}

type TcCall struct {
	AbstractAstNode
	Sym  TcProcSymbol
	Args []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcVariableDefStmt struct {
	AbstractAstNode
	Sym   TcSymbol
	Value TcExpr
}

type TcReturnStmt struct {
	AbstractAstNode
	Value TcExpr
}

type TcProcDef struct {
	AbstractAstNode
	Name       string
	Args       []TcSymbol
	ResultType Type
	Body       TcExpr

	// This code here is supposed to be temporary code. TcProcDef should
	// be a tidy clean minimal definition. This code must be optimized
	// for reading. Do NOT bloat it with shit for lots of of exceptions
	// in the language.
	// Exception for printf. It has varargs that are at this point in
	// time no language feature yet.
	printfargs bool
	generateAsOperator bool
	builtinName string
}

type TcArrayLit struct {
	AbstractAstNode
	Items []TcExpr
}

type TcPackageDef struct {
	AbstractAstNode
	Name     string
	TypeDefs []TcStructDef
	ProcDefs []TcProcDef
}

func (sym TcSymbol) expression()           {}
func (stmt TcVariableDefStmt) expression() {}
func (stmt TcReturnStmt) expression()      {}
func (stmt TcForLoopStmt) expression()     {}
func (stmt TcIfStmt) expression()          {}
func (stmt TcIfElseStmt) expression()      {}
func (block TcCodeBlock) expression()      {}
func (call TcCall) expression()            {}
func (call TcArrayLit) expression()        {}
