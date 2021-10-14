package main

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	Name() string
}

type TcExpr interface {
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

// TODO rename to TcProcSym and TcLetSym

type TcProcSymbol struct {
	Name string
	Impl *TcProcDef
}

type TcLetSymbol struct {
	Name string
	Typ  Type
}

type TcCall struct {
	Sym  TcProcSymbol
	Args []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcLetStmt struct {
	Sym   TcLetSymbol
	Value TcExpr
}

type TcReturnStmt struct {
	Value TcExpr
}

type TcProcDef struct {
	Name       string
	Args       []TcLetSymbol
	ResultType Type
	Body       TcExpr
}

type TcPackageDef struct {
	Name     string
	TypeDefs []TcStructDef
	ProcDefs []TcProcDef
}

func (sym TcLetSymbol) expression()   {}
func (stmt TcLetStmt) expression()    {}
func (stmt TcReturnStmt) expression() {}
func (block TcCodeBlock) expression() {}
func (call TcCall) expression()       {}
