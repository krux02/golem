package main

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type TcExpr interface {
	Expr
	Type() TypeHandle
}

type TcStructField struct {
	Name string
	Type TypeHandle
}

type TcVariableSym struct {
	Name string
	Type TypeHandle
}

type TcStructDef struct {
	Name   string
	Fields []TcStructField
}

type TcProcArgument struct {
	Name string
	Type TypeHandle
}

type TcCodeBlock struct {
	Items []TcExpr
}

type TcSymbol struct {
	Value              string
	OperatorPrecedence int
}

type TcStrLit struct {
	Val string
}

type TcCall struct {
	Sym  Symbol
	Args []Expr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcProcDef struct {
	Name       string
	Args       []TcProcArgument
	ResultType TypeHandle
	Body       Expr
}

type TcPackageDef struct {
	Name     string
	TypeDefs []TcStructDef
	ProcDefs []TcProcDef
}

func (sym TcSymbol) expression() {}
func (block TcCodeBlock) expression() {}
func (lit TcStrLit) expression() {}
func (call TcCall) expression() {}
