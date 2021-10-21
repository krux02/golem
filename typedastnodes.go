package main

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	AstNode
	Name() string
}

type TcExpr interface {
	AstNode
	Expr
	Type() Type
}

type TcStructField struct {
	Name string
	Type Type
}

type TcStructDef struct {
	Name   string
	Fields []TcStructField
}

type TcCodeBlock struct {
	Items []TcExpr
}

// TODO unify TcProcSym with the othe symbol types

type TcProcSymbol struct {
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
	Name string
	Kind SymbolKind
	Typ  Type
}

type TcForLoopStmt struct {
	LoopSym    TcSymbol
	Collection TcExpr
	Body       TcCodeBlock
}

type TcIfStmt struct {
	Condition TcExpr
	Body      TcCodeBlock
}

type TcCall struct {
	Sym  TcProcSymbol
	Args []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcVariableDefStmt struct {
	Sym   TcSymbol
	Value TcExpr
}

type TcReturnStmt struct {
	Value TcExpr
}

type TcProcDef struct {
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
}

type TcPackageDef struct {
	Name     string
	TypeDefs []TcStructDef
	ProcDefs []TcProcDef
}

func (sym TcSymbol) expression()           {}
func (stmt TcVariableDefStmt) expression() {}
func (stmt TcReturnStmt) expression()      {}
func (stmt TcForLoopStmt) expression()     {}
func (stmt TcIfStmt) expression()          {}
func (block TcCodeBlock) expression()      {}
func (call TcCall) expression()            {}
